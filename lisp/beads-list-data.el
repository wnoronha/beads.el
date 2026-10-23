;;; beads-list-data.el --- List refresh, apply-loaded, and queue kind -*- lexical-binding: t; -*-

;;; Commentary:
;; List refresh, apply-loaded, and queue kind.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-cli)
(require 'beads-issue)
(require 'beads-tree)
(require 'beads-presets)

(declare-function beads-list-redraw "beads-list-render" (&optional announce))
(declare-function beads-list--id-at-point "beads-list-render" ())
(declare-function beads-list--schedule-follow "beads-list-follow" ())
(declare-function beads-list-goto-id "beads-list-render" (id))
(declare-function beads-tree-seed-collapsed "beads-tree" (issues))

(defvar beads-list--preset)
(defvar beads-list--query)
(defvar beads-list--collapsed)
(defvar beads-list--rows)
(defvar beads-list--merged-from-show)
(defvar beads-actor)
(defvar user-login-name)

(defvar-local beads-list--issues nil
  "Raw issue alists for the current list.")
(defvar-local beads-list--kind 'list
  "Buffer kind: list, ready, blocked, or human.")
(defvar-local beads-list--loading nil
  "Non-nil while an async refresh is in flight.")
(defvar-local beads-list--load-gen 0
  "Increments on each refresh; stale async callbacks ignore mismatches.")
(defun beads-list--actor ()
  "Actor string for mine preset."
  (or beads-actor user-login-name))
(defun beads-list--apply-loaded (issues &optional seed-fold)
  "Install ISSUES into the buffer and redraw.
When SEED-FOLD, reset collapsed from closed parents."
  (setq beads-list--issues issues
        beads-list--loading nil
        beads-list--merged-from-show (make-hash-table :test #'equal))
  (when (or seed-fold (null beads-list--collapsed))
    (setq beads-list--collapsed (beads-tree-seed-collapsed issues)))
  (beads-list-redraw)
  ;; Land on first live issue when present (Gnus opens first unread).
  (unless (beads-list--id-at-point)
    (goto-char (point-min)))
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let ((iss (get-text-property (point) 'beads-issue)))
          (when (and iss
                     (member (beads-issue-status iss)
                             '("open" "in_progress" "blocked")))
            (setq found (point))))
        (forward-line 1)))
    (when found (goto-char found)))
  (beads-list--schedule-follow)
  (cond
   ((and (memq beads-list--kind '(ready blocked human))
         (null beads-list--rows))
    (message "%s empty — press l for list" beads-list--kind))
   (t (message "Loaded %d issues" (length issues)))))

(defun beads-list--fresh-load-p (gen kind)
  "Non-nil when GEN/KIND still match this buffer's current refresh."
  (and (eql gen beads-list--load-gen)
       (eq kind beads-list--kind)))

(defun beads-list-refresh (&optional seed-fold)
  "Reload issues for this buffer from bd.
With prefix arg SEED-FOLD, reset collapse state from closed parents.
Plain `g' keeps the user's fold state."
  (interactive "P")
  (setq beads-list--loading t
        beads-list--load-gen (1+ (or beads-list--load-gen 0)))
  (force-mode-line-update)
  (let ((buf (current-buffer))
        (gen beads-list--load-gen)
        (kind beads-list--kind))
    (cl-flet ((done (ok data apply-fn)
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (cond
                     ((not (beads-list--fresh-load-p gen kind))
                      ;; Stale response after kind/reload change — ignore.
                      nil)
                     (ok (funcall apply-fn data))
                     (t
                      (setq beads-list--loading nil)
                      (message "beads %s: %s" kind data)))))))
      (pcase kind
        ('ready
         (beads-cli-call-async
          (lambda (ok data)
            (done ok data
                  (lambda (d) (beads-list--apply-loaded (or d '()) nil))))
          "ready" "--exclude-type" "epic,gate" "--limit" "50"))
        ('human
         (beads-cli-call-async
          (lambda (ok data)
            (done ok data
                  (lambda (d) (beads-list--apply-loaded (or d '()) nil))))
          "human" "list"))
        ('blocked
         (beads-cli-call-async
          (lambda (ok data)
            (done ok data
                  (lambda (d)
                    (let ((items (cl-remove-if-not
                                  (lambda (i)
                                    (string= (beads-issue-status i) "blocked"))
                                  (or d '()))))
                      (beads-list--apply-loaded items nil)))))
          "list" "--status" "blocked"))
        (_
         (let* ((filt (beads-preset-filter beads-list--preset (beads-list--actor)))
                (status (car filt))
                (assignee (cdr filt))
                (args (list "list" "--status" status)))
           (when assignee
             (setq args (append args (list "--assignee" assignee))))
           (apply #'beads-cli-call-async
                  (lambda (ok data)
                    (done ok data
                          (lambda (d)
                            (beads-list--apply-loaded (or d '()) seed-fold))))
                  args)))))))
(defun beads-list--bump-comment-count (id)
  "Increment local comment_count for ID and redraw without bd list."
  (setq beads-list--issues
        (mapcar
         (lambda (iss)
           (if (not (equal (beads-issue-id iss) id))
               iss
             (let* ((n (beads-issue-comment-count iss))
                    (n (if (numberp n) n 0)))
               (cons (cons 'comment_count (1+ n))
                     (assq-delete-all 'comment_count (copy-sequence iss))))))
         beads-list--issues))
  (when beads-list--merged-from-show
    (remhash id beads-list--merged-from-show))
  (beads-list-redraw)
  (beads-list-goto-id id))
(defun beads-list-show-ready ()
  "Switch this buffer to the ready queue."
  (interactive)
  (setq beads-list--kind 'ready
        beads-list--preset 'all
        beads-list--query "")
  (beads-list-refresh))

(defun beads-list-show-blocked ()
  "Switch this buffer to the blocked queue."
  (interactive)
  (setq beads-list--kind 'blocked
        beads-list--query "")
  (beads-list-refresh))

(defun beads-list-show-human ()
  "Switch this buffer to the human queue."
  (interactive)
  (setq beads-list--kind 'human
        beads-list--query "")
  (beads-list-refresh))

(defun beads-list-show-list ()
  "Return to the threaded list (current preset)."
  (interactive)
  (setq beads-list--kind 'list)
  (beads-list-refresh))

(provide 'beads-list-data)
;;; beads-list-data.el ends here
