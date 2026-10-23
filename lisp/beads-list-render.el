;;; beads-list-render.el --- List row rebuild, render, fold, and motion -*- lexical-binding: t; -*-

;;; Commentary:
;; List row rebuild, render, fold, and motion.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)
(require 'beads-coltree)
(require 'beads-tree)
(require 'beads-presets)

(declare-function beads-list--apply-filters "beads-list-filter" (issues query))
(declare-function beads-list--header "beads-list-header" ())
(declare-function beads-list--schedule-follow "beads-list-follow" ())

(defvar beads-list--issues)
(defvar beads-list--query)
(defvar beads-list--kind)
(defvar beads-list--hide-closed-roots)
(defvar beads-list--preset)
(defvar beads-tree-sort)

(defvar-local beads-list--rows nil
  "Flattened rows: (issue depth has-kids collapsed).")
(defvar-local beads-list--collapsed nil
  "Hash-table of collapsed parent ids. Owned by list-render;
seeded by list-data on load / mode init.")
(defvar-local beads-list--size-timer nil
  "Idle timer for window-size re-render.")
(defvar-local beads-list--last-width nil
  "Last observed window width for size-change debounce.")
(defun beads-list--live-under (root-id)
  "Count open/in_progress/blocked descendants of ROOT-ID in current issues."
  (let* ((built (beads-tree-build beads-list--issues))
         (kids (cdr built))
         (n 0))
    (cl-labels
        ((walk (id)
           (dolist (ch (gethash id kids))
             (when (member (beads-issue-status ch)
                           '("open" "in_progress" "blocked"))
               (setq n (1+ n)))
             (walk (beads-issue-id ch)))))
      (walk root-id)
      n)))
(defun beads-list--id-at-point ()
  "Issue id on the current line."
  (get-text-property (line-beginning-position) 'beads-id))

(defun beads-list--rebuild-rows (&optional announce)
  "Recompute `beads-list--rows' from issues + collapsed + filters.
When ANNOUNCE, echo visible vs total counts.
Prefer `beads-list-redraw' from other modules."
  (let* ((filtered (beads-list--apply-filters beads-list--issues beads-list--query))
         (rows (if (eq beads-list--kind 'list)
                   (beads-tree-flatten filtered beads-list--collapsed)
                 (mapcar (lambda (iss) (list iss 0 nil nil)) filtered))))
    (setq beads-list--rows rows)
    (when announce
      (message "%d shown · %d loaded%s"
               (length rows) (length beads-list--issues)
               (if beads-list--hide-closed-roots " · hide:closed" "")))))

(defun beads-list--render ()
  "Redraw the list buffer from `beads-list--rows'.
Prefer `beads-list-redraw' from other modules."
  (let ((inhibit-read-only t)
        (pos (point))
        (id (beads-list--id-at-point)))
    (erase-buffer)
    (setq header-line-format '(:eval (beads-list--header)))
    (if (null beads-list--rows)
        (insert
         (propertize
          (pcase beads-list--kind
            ('ready "  (ready queue empty — nothing unblocked)\n")
            ('blocked "  (no blocked issues)\n")
            ('human "  (no human-needed issues)\n")
            (_ "  (no issues — try u clear, f all, X off, or preset 0)\n"))
          'face 'shadow))
      (let ((prev-depth nil))
        (dolist (row beads-list--rows)
          (cl-destructuring-bind (iss depth has-kids collapsed &optional last-p) row
            ;; Magit-style breathing room before each new root.
            (when (and (eq beads-list--kind 'list)
                       (eql depth 0)
                       prev-depth
                       (> prev-depth 0))
              (insert "\n"))
            (let* ((line (beads-coltree-format-issue iss depth collapsed has-kids last-p))
                   (start (point))
                   (props (list 'beads-id (beads-issue-id iss)
                                'beads-issue iss
                                'beads-has-kids has-kids
                                'beads-collapsed collapsed
                                'beads-depth depth
                                'mouse-face 'highlight
                                'help-echo "mouse-1: select and follow")))
              (insert line)
              (when (and collapsed has-kids)
                (let ((live-n (beads-list--live-under (beads-issue-id iss))))
                  (when (> live-n 0)
                    (insert (propertize (format " ·%d live" live-n)
                                        'face 'warning)))))
              (insert "\n")
              (when (and beads-list--query
                         (not (string-empty-p beads-list--query)))
                (let ((q (regexp-quote beads-list--query))
                      (case-fold-search t))
                  (save-excursion
                    (goto-char start)
                    (while (re-search-forward q (line-end-position) t)
                      (add-face-text-property (match-beginning 0) (match-end 0)
                                              'match t)))))
              (add-text-properties start (point) props))
            (setq prev-depth depth)))))
    (goto-char (point-min))
    (when id
      (unless (beads-list--goto-id id)
        (goto-char (min pos (point-max)))))
    (setq mode-name
          (format "Beads[%s]" (beads-preset-name beads-list--preset)))))

(defun beads-list-redraw (&optional announce)
  "Rebuild filtered rows and redraw the list buffer.
Public facade for filter/data/fold callers. When ANNOUNCE, echo
visible vs loaded counts."
  (beads-list--rebuild-rows announce)
  (beads-list--render))

(defun beads-list--goto-id (id)
  "Move point to the title row for ID.
Internal; prefer `beads-list-goto-id' from other features."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (when (equal (get-text-property (point) 'beads-id) id)
          (setq found (point)))
        (forward-line 1)))
    (when found
      (goto-char found)
      t)))

(defun beads-list-goto-id (id)
  "Move point to the title row for ID.
Public API for other features (detail siblings, etc.).
Return non-nil if ID was found."
  (beads-list--goto-id id))
(defun beads-list--on-primary-row-p ()
  "Non-nil when point is on an issue title row."
  (get-text-property (line-beginning-position) 'beads-id))
(defun beads-list-recenter ()
  "Recenter point in the list window."
  (interactive)
  (recenter-top-bottom)
  (redisplay t))
(defun beads-list-toggle-fold ()
  "Collapse or expand the parent at point."
  (interactive)
  (let* ((id (beads-list--id-at-point))
         (has (get-text-property (line-beginning-position) 'beads-has-kids)))
    (unless (and id has)
      (user-error "Nothing to fold"))
    (if (gethash id beads-list--collapsed)
        (remhash id beads-list--collapsed)
      (puthash id t beads-list--collapsed))
    (beads-list-redraw)
    (beads-list--goto-id id)))

(defun beads-list-collapse ()
  "Collapse parent at point (or no-op)."
  (interactive)
  (let ((id (beads-list--id-at-point))
        (has (get-text-property (line-beginning-position) 'beads-has-kids)))
    (when (and id has)
      (puthash id t beads-list--collapsed)
      (beads-list-redraw)
      (beads-list--goto-id id))))

(defun beads-list-expand ()
  "Expand parent at point."
  (interactive)
  (let ((id (beads-list--id-at-point)))
    (when id
      (remhash id beads-list--collapsed)
      (beads-list-redraw)
      (beads-list--goto-id id))))

(defun beads-list-collapse-all ()
  "Collapse every parent."
  (interactive)
  (let ((built (beads-tree-build beads-list--issues)))
    (maphash (lambda (pid children)
               (when children (puthash pid t beads-list--collapsed)))
             (cdr built)))
  (beads-list-redraw))

(defun beads-list-expand-all ()
  "Expand every parent."
  (interactive)
  (clrhash beads-list--collapsed)
  (beads-list-redraw))

(defun beads-list-fold-idle ()
  "Collapse branches with no live work (open/in_progress/blocked)."
  (interactive)
  (setq beads-list--collapsed
        (beads-tree-collapse-idle beads-list--issues))
  (beads-list-redraw t)
  (message "Folded idle branches"))

(defun beads-list-cycle-sort ()
  "Cycle tree sort: updated → priority → id → updated."
  (interactive)
  (setq beads-tree-sort
        (pcase beads-tree-sort
          ('updated 'priority)
          ('priority 'id)
          (_ 'updated)))
  (beads-list-redraw)
  (message "sort: %s" beads-tree-sort))
(defun beads-list--move-primary (dir)
  "Move DIR lines among primary issue rows and schedule detail follow."
  (forward-line dir)
  (while (and (not (if (< dir 0) (bobp) (eobp)))
              (not (beads-list--on-primary-row-p)))
    (forward-line dir))
  (when (eobp) (forward-line -1))
  (when (bobp) (goto-char (point-min)))
  (while (and (not (bobp)) (not (beads-list--on-primary-row-p)))
    (forward-line -1))
  (beads-list--schedule-follow))

(defun beads-list-mouse-select (event)
  "Select the issue under mouse EVENT and follow into detail."
  (interactive "e")
  (mouse-set-point event)
  (when-let* ((id (beads-list--id-at-point)))
    (beads-list--goto-id id)
    (beads-list--schedule-follow)))

(defun beads-list-next-line ()
  "Move to next issue title row and follow."
  (interactive)
  (beads-list--move-primary 1))

(defun beads-list-prev-line ()
  "Move to previous issue title row and follow."
  (interactive)
  (beads-list--move-primary -1))
(defun beads-list--on-window-size (frame)
  "Debounced re-render when a beads list window on FRAME changes width."
  (dolist (win (window-list frame 'no-mini))
    (with-current-buffer (window-buffer win)
      (when (eq major-mode 'beads-list-mode)
        (let ((w (window-width win)))
          (unless (equal w beads-list--last-width)
            (setq beads-list--last-width w)
            (when beads-list--size-timer
              (cancel-timer beads-list--size-timer))
            (setq beads-list--size-timer
                  (run-with-idle-timer
                   0.2 nil
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq beads-list--size-timer nil)
                         ;; Width-only: rows unchanged, reflow titles.
                         (beads-list--render))))
                   (current-buffer)))))))))

(provide 'beads-list-render)
;;; beads-list-render.el ends here
