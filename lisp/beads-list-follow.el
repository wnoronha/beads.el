;;; beads-list-follow.el --- List detail follow, enrich, and history -*- lexical-binding: t; -*-

;;; Commentary:
;; List detail follow and visit history.
;; Full issue bodies come from detail's `bd show` (`beads-detail-refresh');
;; after apply, detail pushes fields into the list via
;; `beads-list-merge-from-show'. The list keeps stubs until then.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)

(declare-function beads-list--id-at-point "beads-list-render" ())
(declare-function beads-list-goto-id "beads-list-render" (id))
(declare-function beads-detail-display "beads-detail" (id &optional force select stub))
(declare-function beads-detail-goto-section "beads-detail" (title &optional force-comments))
(declare-function beads-detail--goto-heading "beads-detail-render" (title))
(declare-function beads-detail--after-heading-hop "beads-detail" ())
(declare-function beads-detail-shown-id "beads-detail" ())

(defvar beads-detail-buffer-name)
(defvar beads-list-buffer-name)
(defvar beads-list-focus-issue-fn)
(defvar beads-list--issues)

(defvar-local beads-list--history nil
  "Ring of recently visited issue ids (newest first).")
(defvar-local beads-list--history-pos 0
  "Index into `beads-list--history' for M-p/M-n.")

;; Buffer name lives in beads-layout.el (shared with window config).

(defcustom beads-auto-follow nil
  "When non-nil, moving in the list updates the detail buffer.
Default nil: open detail with RET (standalone switch, or paired
pane when `beads-layout' is set). Most useful with a paired layout
so the list keeps focus. Toggle via `beads-list-toggle-follow'.
Classic Gnus: (setq beads-layout \\='stacked beads-auto-follow t)."
  :type 'boolean
  :group 'beads)

(defcustom beads-follow-delay 0.12
  "Idle seconds before following list point into detail.
Debounces rapid scroll so each bd show is not fired per keystroke."
  :type 'number
  :group 'beads)
(defvar-local beads-list--follow-timer nil
  "Pending idle timer for detail follow.")
(defvar-local beads-list--merged-from-show (make-hash-table :test #'equal)
  "Issue ids whose list stubs were merged from a detail `bd show'.")

(defun beads-list-toggle-follow ()
  "Toggle Gnus-style auto-follow into the detail pane."
  (interactive)
  (setq beads-auto-follow (not beads-auto-follow))
  (force-mode-line-update t)
  (when beads-auto-follow
    (beads-list--schedule-follow))
  (message "follow %s" (if beads-auto-follow "on" "off")))
(defun beads-list--goto-history (delta)
  "Move DELTA steps in visit history and select that issue."
  (unless beads-list--history
    (user-error "No visit history yet"))
  (let* ((len (length beads-list--history))
         (pos (mod (+ beads-list--history-pos delta) len))
         (id (nth pos beads-list--history)))
    (setq beads-list--history-pos pos)
    (unless (beads-list-goto-id id)
      (user-error "Issue %s not visible (folded or filtered)" id))
    (beads-list-follow-now)
    (message "history %d/%d %s" (1+ pos) len id)))

(defun beads-list-history-back ()
  "Jump to previous visited issue."
  (interactive)
  (beads-list--goto-history 1))

(defun beads-list-history-forward ()
  "Jump to next visited issue."
  (interactive)
  (beads-list--goto-history -1))
(defun beads-list--merge-show-fields (base full)
  "Return BASE with design/acceptance/notes/description from FULL."
  (let ((out (copy-sequence base)))
    (dolist (key '(description design acceptance_criteria notes
                               comments comment_count
                               dependents dependent_count
                               dependencies dependency_count))
      (when (assq key full)
        (setq out (cons (cons key (alist-get key full))
                        (assq-delete-all key out)))))
    out))

(defun beads-list--replace-issue (id new-iss)
  "Replace issue ID in `beads-list--issues' and row text properties."
  (setq beads-list--issues
        (mapcar (lambda (iss)
                  (if (equal (beads-issue-id iss) id) new-iss iss))
                beads-list--issues))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (equal (get-text-property (point) 'beads-id) id)
        (let ((inhibit-read-only t)
              (end (next-single-property-change (point) 'beads-id nil (point-max))))
          (put-text-property (point) end 'beads-issue new-iss)))
      (forward-line 1))))

(defun beads-list-merge-from-show (id full)
  "Merge FULL `bd show' payload into the list stub for ID.
Public API: detail owns show fetches and calls this after apply.
No-op when the list buffer is missing or ID is not loaded."
  (when-let* ((lbuf (get-buffer beads-list-buffer-name)))
    (with-current-buffer lbuf
      (when (eq major-mode 'beads-list-mode)
        (let* ((cache (or beads-list--merged-from-show
                          (setq beads-list--merged-from-show
                                (make-hash-table :test #'equal))))
               (cur (cl-find id beads-list--issues
                             :key #'beads-issue-id :test #'equal))
               ;; Prefer alist with id; else unwrap one-element show list.
               (payload (cond
                         ((and (listp full) (assq 'id full)) full)
                         ((and (listp full) (listp (car full))
                               (assq 'id (car full)))
                          (car full))
                         (t full)))
               (merged (and cur payload
                            (beads-list--merge-show-fields cur payload))))
          (when merged
            (puthash id t cache)
            (beads-list--replace-issue id merged)
            (force-mode-line-update t)))))))

(defun beads-list--record-visit (id)
  "Push ID onto visit history (newest first)."
  (unless (equal id (car beads-list--history))
    (setq beads-list--history
          (cons id (cl-remove id beads-list--history :test #'equal)))
    (when (> (length beads-list--history) 30)
      (setq beads-list--history (cl-subseq beads-list--history 0 30)))
    (setq beads-list--history-pos 0)))

(defun beads-list--follow-now ()
  "Push the issue at point into the detail pane (no focus steal).
Internal; prefer `beads-list-follow-now' from other features.
Detail owns the subsequent `bd show'; list keeps the stub until
`beads-list-merge-from-show'."
  (setq beads-list--follow-timer nil)
  (when (and beads-auto-follow (eq major-mode 'beads-list-mode))
    (let* ((id (beads-list--id-at-point))
           (detail-id (progn (require 'beads-detail) (beads-detail-shown-id)))
           (stale (and id (not (equal id detail-id)))))
      (when stale
        (beads-list--record-visit id)
        (require 'beads-detail)
        (let ((stub (get-text-property (line-beginning-position) 'beads-issue)))
          (beads-detail-display id nil nil stub))))))

(defun beads-list-follow-now ()
  "Push the issue at point into the detail pane (no focus steal).
Public API for other features (detail siblings, etc.)."
  (beads-list--follow-now))

(defun beads-list-focus-issue (id)
  "Move list point to ID and follow into the detail pane.
Public API used by detail (via `beads-list-focus-issue-fn').
Return non-nil if ID was found and focused.
Always updates detail (even when `beads-auto-follow' is nil), so
sibling navigation from the detail pane keeps working."
  (let ((list-buf (get-buffer beads-list-buffer-name)))
    (unless list-buf
      (user-error "No list buffer"))
    (with-current-buffer list-buf
      (unless (beads-list-goto-id id)
        (user-error "Issue %s not visible (folded or filtered)" id))
      (let ((beads-auto-follow t))
        (beads-list-follow-now))
      t)))

(defun beads-list--schedule-follow ()
  "Debounce detail follow after list motion (Gnus/mu4e style)."
  (when beads-auto-follow
    (when beads-list--follow-timer
      (cancel-timer beads-list--follow-timer))
    (setq beads-list--follow-timer
          (run-with-timer beads-follow-delay nil
                          (let ((buf (current-buffer)))
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (beads-list-follow-now)))))))))

(defun beads-list--follow-after-command ()
  "post-command-hook: follow after scroll / motion commands."
  (when (and beads-auto-follow
             (eq major-mode 'beads-list-mode)
             (not (minibufferp))
             (memq this-command
                   '(beads-list-next-line
                     beads-list-prev-line
                     next-line previous-line
                     scroll-up-command scroll-down-command
                     mwheel-scroll pixel-scroll-precision
                     beginning-of-buffer end-of-buffer
                     beads-list-toggle-fold
                     beads-list-collapse beads-list-expand
                     beads-list-collapse-all beads-list-expand-all)))
    (beads-list--schedule-follow)))

(defun beads-list-goto-detail-section (heading)
  "Show issue detail and jump to outline HEADING.
Opens the detail pane even when `beads-auto-follow' is off."
  (let ((id (beads-list--id-at-point)))
    (unless id (user-error "No issue at point"))
    (let ((beads-auto-follow t))
      (beads-list-follow-now))
    (when-let* ((db (get-buffer beads-detail-buffer-name)))
      (with-current-buffer db
        (require 'beads-detail)
        (if (beads-detail--goto-heading heading)
            (progn
              (when (get-buffer-window db t)
                (set-window-point (get-buffer-window db t) (point)))
              (beads-detail--after-heading-hop))
          (message "No %s section" heading))))))

(defun beads-list-goto-acceptance ()
  "Follow issue and jump to Acceptance."
  (interactive)
  (beads-list-goto-detail-section "Acceptance"))

(defun beads-list-goto-design ()
  "Follow issue and jump to Design."
  (interactive)
  (beads-list-goto-detail-section "Design"))

(defun beads-list-goto-description ()
  "Follow issue and jump to Description."
  (interactive)
  (beads-list-goto-detail-section "Description"))

(defun beads-list-goto-links ()
  "Follow issue and jump to the first link group."
  (interactive)
  (let ((id (beads-list--id-at-point)))
    (unless id (user-error "No issue at point"))
    (let ((beads-auto-follow t))
      (beads-list-follow-now))
    (when-let* ((db (get-buffer beads-detail-buffer-name)))
      (with-current-buffer db
        (require 'beads-detail)
        (when (or (beads-detail--goto-heading "Parent")
                  (beads-detail--goto-heading "Depends on")
                  (beads-detail--goto-heading "Children")
                  (beads-detail--goto-heading "Blocks")
                  (beads-detail--goto-heading "Related"))
          (when (get-buffer-window db t)
            (set-window-point (get-buffer-window db t) (point)))
          (beads-detail--after-heading-hop))))))

(defun beads-list-goto-comments ()
  "Follow issue and jump to Comments."
  (interactive)
  (beads-list-goto-detail-section "Comments"))

(defun beads-list-open-detail ()
  "Open detail for the issue at point and select it."
  (interactive)
  (let ((id (beads-list--id-at-point)))
    (unless id (user-error "No issue at point"))
    (beads-list--record-visit id)
    (require 'beads-detail)
    (beads-detail-display id t t)))

(setq beads-list-focus-issue-fn #'beads-list-focus-issue)

(provide 'beads-list-follow)
;;; beads-list-follow.el ends here
