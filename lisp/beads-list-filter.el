;;; beads-list-filter.el --- List filters, search, and presets -*- lexical-binding: t; -*-

;;; Commentary:
;; List filters, search, and presets.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)
(require 'beads-tree)
(require 'beads-presets)

(declare-function beads-list-redraw "beads-list-render" (&optional announce))
(declare-function beads-list--id-at-point "beads-list-render" ())
(declare-function beads-list-refresh "beads-list-data" (&optional seed-fold))

(defvar beads-list--issues)
(defvar beads-list--kind)

(defvar-local beads-list--type-filter nil
  "When non-nil, only show this issue_type string.")
(defvar-local beads-list--label-filter nil
  "When non-nil, only show issues with this label.")
(defvar-local beads-list--priority-max nil
  "When non-nil integer, only show issues with priority <= this.")
(defvar-local beads-list--todos-only nil
  "When non-nil, only show issues with markdown task checkboxes.")
(defvar-local beads-list--preset 'all
  "Active list preset symbol.")
(defvar-local beads-list--query ""
  "Client-side title/id substring filter.")
(defvar-local beads-list--narrow-id nil
  "When non-nil, only show this issue and its descendants.")
(defvar-local beads-list--status-filter nil
  "When non-nil, only show this status string.")
(defvar-local beads-list--hide-closed-roots nil
  "When non-nil, drop closed root issues from the list.")
(defun beads-list--subtree-ids (issues root-id)
  "Return ROOT-ID plus all descendant ids within ISSUES."
  (let* ((built (beads-tree-build issues))
         (kids (cdr built))
         (ids (list root-id)))
    (cl-labels
        ((walk (id)
           (dolist (ch (gethash id kids))
             (let ((cid (beads-issue-id ch)))
               (push cid ids)
               (walk cid)))))
      (walk root-id)
      ids)))
(defun beads-list--apply-filters (issues query)
  "Apply list filter pipeline to ISSUES.
Stages: narrow subtree, hide-closed roots, status/type/label/priority,
todos-only, then QUERY (id/title/description; prefix ! inverts)."
  (let ((items issues))
    (when beads-list--narrow-id
      (let ((keep (beads-list--subtree-ids items beads-list--narrow-id)))
        (setq items
              (cl-remove-if-not
               (lambda (iss) (member (beads-issue-id iss) keep))
               items))))
    (when beads-list--hide-closed-roots
      (let* ((built (beads-tree-build items))
             (by-id (car built))
             (kids (cdr built))
             (drop (make-hash-table :test #'equal)))
        (cl-labels
            ((mark (id)
               (puthash id t drop)
               (dolist (ch (gethash id kids))
                 (mark (beads-issue-id ch)))))
          (maphash
           (lambda (id iss)
             (let ((pid (beads-issue-parent iss)))
               (when (and (beads-issue-closed-p iss)
                          (or (null pid) (string-empty-p pid)
                              (not (gethash pid by-id))))
                 (mark id))))
           by-id))
        (setq items
              (cl-remove-if
               (lambda (iss) (gethash (beads-issue-id iss) drop))
               items))))
    (when (and beads-list--status-filter
               (not (string-empty-p beads-list--status-filter)))
      (setq items
            (cl-remove-if-not
             (lambda (iss)
               (string= (beads-issue-status iss) beads-list--status-filter))
             items)))
    (when (and beads-list--type-filter
               (not (string-empty-p beads-list--type-filter)))
      (setq items
            (cl-remove-if-not
             (lambda (iss)
               (string= (beads-issue-type iss) beads-list--type-filter))
             items)))
    (when (and beads-list--label-filter
               (not (string-empty-p beads-list--label-filter)))
      (setq items
            (cl-remove-if-not
             (lambda (iss)
               (member beads-list--label-filter (beads-issue-labels iss)))
             items)))
    (when (numberp beads-list--priority-max)
      (setq items
            (cl-remove-if-not
             (lambda (iss)
               (<= (beads-issue-priority iss) beads-list--priority-max))
             items)))
    (when beads-list--todos-only
      (setq items
            (cl-remove-if-not
             (lambda (iss)
               (let ((src (concat (or (beads-issue-description iss) "")
                                  "\n"
                                  (or (beads-issue-acceptance iss) ""))))
                 (string-match-p "\\[[ xX]\\]" src)))
             items)))
    (if (or (null query) (string-empty-p query))
        items
      (let* ((invert (string-prefix-p "!" query))
             (q (downcase (if invert (substring query 1) query)))
             (q (string-trim q)))
        (if (string-empty-p q)
            items
          (funcall (if invert #'cl-remove-if #'cl-remove-if-not)
                   (lambda (iss)
                     (or (string-match-p (regexp-quote q)
                                         (downcase (beads-issue-id iss)))
                         (string-match-p (regexp-quote q)
                                         (downcase (beads-issue-title iss)))
                         (string-match-p (regexp-quote q)
                                         (downcase (or (beads-issue-description iss) "")))))
                   items))))))
(defun beads-list-set-preset (preset)
  "Switch list PRESET and refresh."
  (interactive)
  (unless (eq beads-list--kind 'list)
    (setq beads-list--kind 'list))
  (setq beads-list--preset preset)
  (beads-list-refresh t))

(defun beads-list-choose-preset ()
  "Pick a list preset via completing-read."
  (interactive)
  (let* ((choices
          (mapcar (lambda (cell)
                    (cons (beads-preset-name (car cell)) (car cell)))
                  beads-presets))
         (name (completing-read "Preset: " (mapcar #'car choices) nil t
                                nil nil
                                (beads-preset-name beads-list--preset)))
         (preset (cdr (assoc name choices))))
    (when preset
      (beads-list-set-preset preset))))

(defun beads-list-preset-key ()
  "Apply preset from last key (0-5 / a).
Works for bare digits and under the `C-c C-p' prefix."
  (interactive)
  (let* ((key (this-command-keys-vector))
         (ch (and (> (length key) 0) (aref key (1- (length key)))))
         (s (and ch (char-to-string ch)))
         (preset (and s (beads-preset-from-key s))))
    (unless preset
      (user-error "Unknown preset key"))
    (beads-list-set-preset preset)))

(defun beads-list-search ()
  "Prompt for a client-side search substring.
Special: `todos' toggles the todos-only checkbox filter."
  (interactive)
  (let ((raw (read-string "Search: " beads-list--query)))
    (if (member (downcase (string-trim raw)) '("todos" ":todos" "todo"))
        (progn
          (setq beads-list--query "")
          (beads-list-toggle-todos-only))
      (setq beads-list--query raw)
      (beads-list-redraw t))))

(defun beads-list-clear-search ()
  "Clear search, type/status filters, and subtree narrow."
  (interactive)
  (setq beads-list--query ""
        beads-list--type-filter nil
        beads-list--label-filter nil
        beads-list--priority-max nil
        beads-list--status-filter nil
        beads-list--todos-only nil
        beads-list--narrow-id nil
        beads-list--hide-closed-roots nil)
  (beads-list-redraw))

(defun beads-list-toggle-hide-closed-roots ()
  "Toggle hiding of closed root issues (and their trees)."
  (interactive)
  (setq beads-list--hide-closed-roots (not beads-list--hide-closed-roots))
  (beads-list-redraw t)
  (message "hide closed roots %s"
           (if beads-list--hide-closed-roots "on" "off")))

(defun beads-list-cycle-status-filter ()
  "Cycle status filter: all → open → in_progress → blocked → closed → all."
  (interactive)
  (setq beads-list--status-filter
        (pcase beads-list--status-filter
          ("open" "in_progress")
          ("in_progress" "blocked")
          ("blocked" "closed")
          ("closed" nil)
          (_ "open")))
  (beads-list-redraw t)
  (message "status: %s" (or beads-list--status-filter "all")))

(defun beads-list-toggle-todos-only ()
  "Toggle filter to issues with markdown `[ ]` / `[x]` task checkboxes."
  (interactive)
  (setq beads-list--todos-only (not beads-list--todos-only))
  (beads-list-redraw t)
  (message "todos-only %s" (if beads-list--todos-only "on" "off")))

(defun beads-list-filter-type ()
  "Filter list to an issue type (empty clears)."
  (interactive)
  (let* ((types (cl-delete-duplicates
                 (mapcar #'beads-issue-type beads-list--issues)
                 :test #'string=))
         (choice (completing-read "Type (empty=all): " types nil nil
                                  beads-list--type-filter)))
    (setq beads-list--type-filter
          (if (string-empty-p choice) nil choice))
    (beads-list-redraw t)))

(defun beads-list-filter-label ()
  "Filter list to a label (empty clears)."
  (interactive)
  (let* ((labels (cl-delete-duplicates
                  (apply #'append
                         (mapcar #'beads-issue-labels beads-list--issues))
                  :test #'string=))
         (choice (completing-read "Label (empty=all): " labels nil nil
                                  beads-list--label-filter)))
    (setq beads-list--label-filter
          (if (string-empty-p choice) nil choice))
    (beads-list-redraw t)))

(defun beads-list-filter-priority ()
  "Keep issues with priority <= N (empty clears)."
  (interactive)
  (let ((s (read-string "Max priority 0-4 (empty=all): "
                        (if beads-list--priority-max
                            (number-to-string beads-list--priority-max)
                          ""))))
    (setq beads-list--priority-max
          (if (string-empty-p (string-trim s))
              nil
            (string-to-number s)))
    (beads-list-redraw t)))
(defun beads-list-narrow-subtree (arg)
  "Narrow list to issue at point and descendants.
With prefix ARG, or when already narrowed to that id, widen."
  (interactive "P")
  (let ((id (beads-list--id-at-point)))
    (cond
     (arg
      (setq beads-list--narrow-id nil)
      (beads-list-redraw t)
      (message "widen"))
     ((and id (equal id beads-list--narrow-id))
      (setq beads-list--narrow-id nil)
      (beads-list-redraw t)
      (message "widen"))
     (id
      (setq beads-list--narrow-id id)
      (beads-list-redraw t)
      (message "narrow %s" id))
     (t (user-error "No issue at point")))))

(provide 'beads-list-filter)
;;; beads-list-filter.el ends here
