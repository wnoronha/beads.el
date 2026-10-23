;;; beads-tree.el --- Parent/child forest for list view -*- lexical-binding: t; -*-

;;; Commentary:
;; Build a parent→child forest from issue summaries and flatten it for
;; display, honoring a collapsed parent-id set (BTU fold semantics).

;;; Code:

(require 'beads-issue)
(require 'cl-lib)

(defun beads-tree--status-rank (status)
  "Closed sorts after open-like statuses."
  (if (string= status "closed") 1 0))

(defvar beads-tree-sort 'updated
  "Sibling/root secondary sort: `updated', `priority', or `id'.")

(defun beads-tree--less-updated (a b)
  "Newer updated_at first; tie-break id ASC."
  (let ((ua (beads-issue-updated-at a))
        (ub (beads-issue-updated-at b)))
    (cond
     ((string< ub ua) t)
     ((string< ua ub) nil)
     (t (string< (beads-issue-id a) (beads-issue-id b))))))

(defun beads-tree--less-priority (a b)
  "Lower priority number first; tie-break id ASC."
  (let ((pa (beads-issue-priority a))
        (pb (beads-issue-priority b)))
    (cond
     ((/= pa pb) (< pa pb))
     (t (string< (beads-issue-id a) (beads-issue-id b))))))

(defun beads-tree--less-id (a b)
  "Id ASC."
  (string< (beads-issue-id a) (beads-issue-id b)))

(defun beads-tree--less-secondary (a b)
  "Compare by `beads-tree-sort'."
  (pcase beads-tree-sort
    ('priority (beads-tree--less-priority a b))
    ('id (beads-tree--less-id a b))
    (_ (beads-tree--less-updated a b))))

(defun beads-tree--less-root (a b)
  "Order roots: non-closed first, then `beads-tree-sort'."
  (let ((ra (beads-tree--status-rank (beads-issue-status a)))
        (rb (beads-tree--status-rank (beads-issue-status b))))
    (cond
     ((/= ra rb) (< ra rb))
     (t (beads-tree--less-secondary a b)))))

(defun beads-tree--less-child (a b)
  "Order siblings by `beads-tree-sort'."
  (beads-tree--less-secondary a b))

(defun beads-tree-build (issues)
  "Return (by-id . children-hash) from ISSUES list.
CHILDREN-HASH maps parent-id → list of child issues.
Roots are issues whose parent is empty or missing from BY-ID."
  (let ((by-id (make-hash-table :test #'equal))
        (kids (make-hash-table :test #'equal)))
    (dolist (iss issues)
      (puthash (beads-issue-id iss) iss by-id))
    (dolist (iss issues)
      (let ((pid (beads-issue-parent iss)))
        (when (and pid (not (string-empty-p pid)) (gethash pid by-id))
          (puthash pid
                   (cons iss (gethash pid kids))
                   kids))))
    (maphash (lambda (pid children)
               (puthash pid (sort (copy-sequence children) #'beads-tree--less-child) kids))
             kids)
    (cons by-id kids)))

(defun beads-tree-roots (by-id &optional _kids)
  "Return root issues from BY-ID, sorted.
Optional _KIDS is ignored; kept for call-site symmetry with `beads-tree-build'."
  (let (roots)
    (maphash
     (lambda (_id iss)
       (let ((pid (beads-issue-parent iss)))
         (unless (and pid (not (string-empty-p pid)) (gethash pid by-id))
           (push iss roots))))
     by-id)
    (sort roots #'beads-tree--less-root)))

(defun beads-tree-flatten (issues collapsed)
  "Flatten ISSUES into display rows.
COLLAPSED is a hash-table of parent ids that hide their children.
Each row is (issue depth has-kids collapsed last-sibling)."
  (let* ((built (beads-tree-build issues))
         (by-id (car built))
         (kids (cdr built))
         (rows '()))
    (cl-labels
        ((walk (iss depth last-p)
           (let* ((id (beads-issue-id iss))
                  (children (gethash id kids))
                  (fold-p (and children (gethash id collapsed)))
                  (n (length children))
                  (i 0))
             (push (list iss depth (and children t) fold-p last-p) rows)
             (unless fold-p
               (dolist (ch children)
                 (walk ch (1+ depth) (= i (1- n)))
                 (setq i (1+ i)))))))
      (let* ((roots (beads-tree-roots by-id kids))
             (n (length roots))
             (i 0))
        (dolist (root roots)
          (walk root 0 (= i (1- n)))
          (setq i (1+ i)))))
    (nreverse rows)))

(defun beads-tree--live-p (issue)
  "Non-nil when ISSUE is open-like work."
  (member (beads-issue-status issue)
          '("open" "in_progress" "blocked")))

(defun beads-tree-seed-collapsed (issues)
  "Default collapsed set: closed parents, but keep live work visible.
Parents of open/in_progress/blocked issues stay expanded so active
work is not hidden under a folded closed epic."
  (let* ((tbl (make-hash-table :test #'equal))
         (built (beads-tree-build issues))
         (by-id (car built))
         (kids (cdr built)))
    (maphash
     (lambda (pid children)
       (when children
         (let ((parent (gethash pid by-id)))
           (when (and parent (beads-issue-closed-p parent))
             (puthash pid t tbl)))))
     kids)
    ;; Reveal ancestors of live issues.
    (maphash
     (lambda (_id iss)
       (when (beads-tree--live-p iss)
         (let ((pid (beads-issue-parent iss)))
           (while (and pid (not (string-empty-p pid)))
             (remhash pid tbl)
             (let ((parent (gethash pid by-id)))
               (setq pid (and parent (beads-issue-parent parent))))))))
     by-id)
    tbl))

(defun beads-tree-collapse-idle (issues)
  "Collapsed set for parents with no live descendants in their subtree.
Keeps paths to open/in_progress/blocked work expanded; folds done branches."
  (let* ((tbl (make-hash-table :test #'equal))
         (built (beads-tree-build issues))
         (by-id (car built))
         (kids (cdr built))
         (memo (make-hash-table :test #'equal)))
    (cl-labels
        ((live-under-p (iss)
           (let* ((id (beads-issue-id iss))
                  (cached (gethash id memo 'unset)))
             (if (not (eq cached 'unset))
                 cached
               (let* ((self (beads-tree--live-p iss))
                      (children (gethash id kids))
                      (any (or self
                               (cl-some #'live-under-p children))))
                 (puthash id any memo)
                 any)))))
      (maphash
       (lambda (pid children)
         (when children
           (let ((parent (gethash pid by-id)))
             (when (and parent (not (live-under-p parent)))
               (puthash pid t tbl)))))
       kids))
    tbl))

(provide 'beads-tree)
;;; beads-tree.el ends here
