;;; beads-rel.el --- Relation predicates and progress helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared hierarchy / blocking / closed predicates and checkbox / rel
;; progress counts. Used by list header chips and detail link grouping.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)

(defun beads-rel-hierarchy-p (rel)
  "Non-nil when REL is parent-child (or typeless bd-show dependent)."
  (let ((typ (beads-issue-rel-type rel)))
    (or (string= typ "parent-child")
        (string-empty-p typ))))

(defun beads-rel-blocking-p (rel)
  "Non-nil when REL is a blocks/waits-for edge (not parent-child)."
  (member (beads-issue-rel-type rel)
          '("blocks" "conditional-blocks" "waits-for")))

(defun beads-rel-relates-p (rel)
  "Non-nil when REL is a relates-to edge."
  (member (beads-issue-rel-type rel)
          '("relates-to" "relates_to" "related")))

(defun beads-rel-partition-for-show (issue)
  "Partition ISSUE links like `bd show'.
Groups: Parent, Depends on, Children, Blocks, Related.
Return an alist of those keys to rel lists. Related is id-deduped."
  (let* ((deps (beads-issue-dependencies issue))
         (dents (beads-issue-dependents issue))
         (parent (cl-remove-if-not #'beads-rel-hierarchy-p deps))
         (children (cl-remove-if-not #'beads-rel-hierarchy-p dents))
         (depends-on (cl-remove-if-not #'beads-rel-blocking-p deps))
         (blocks (cl-remove-if-not #'beads-rel-blocking-p dents))
         (related-raw (append
                       (cl-remove-if-not #'beads-rel-relates-p deps)
                       (cl-remove-if-not #'beads-rel-relates-p dents)))
         (related (cl-delete-duplicates
                   related-raw
                   :test (lambda (a b)
                           (equal (beads-issue-rel-id a)
                                  (beads-issue-rel-id b))))))
    (list (cons 'parent parent)
          (cons 'depends-on depends-on)
          (cons 'children children)
          (cons 'blocks blocks)
          (cons 'related related))))

(defun beads-rel-closed-p (rel &optional issues)
  "Non-nil when REL points at a closed issue.
When REL has no status, look up the target id in ISSUES (list of
issue alists)."
  (let ((st (or (alist-get 'status rel) "")))
    (cond
     ((string= st "closed") t)
     ((not (string-empty-p st)) nil)
     (t
      (let* ((id (beads-issue-rel-id rel))
             (iss (and id issues
                       (cl-find id issues
                                :key #'beads-issue-id :test #'equal))))
        (and iss (beads-issue-closed-p iss)))))))

(defun beads-rel-progress (rels &optional issues)
  "Return (done . total) for RELS using closed status, or nil if empty.
ISSUES is passed to `beads-rel-closed-p' for status fallback."
  (let ((total (length rels)))
    (when (> total 0)
      (cons (cl-count-if (lambda (r) (beads-rel-closed-p r issues)) rels)
            total))))

(defun beads-rel-checkbox-progress-in (text)
  "Return (done . total) for markdown checkboxes in TEXT, or nil."
  (when (and text (not (string-empty-p text)))
    (let ((done 0)
          (open 0))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-match-p "\\[[xX]\\]\\|✓" line) (setq done (1+ done)))
         ((string-match-p "\\[ \\]\\|○" line) (setq open (1+ open)))))
      (let ((total (+ done open)))
        (when (> total 0)
          (cons done total))))))

(defun beads-rel-acceptance-progress (iss)
  "Return (done . total) for acceptance-criteria checkboxes only."
  (when iss
    (beads-rel-checkbox-progress-in (beads-issue-acceptance iss))))

(defun beads-rel-parent-progress (iss &optional issues)
  "Return (done . total) for parent(s) of ISS, or nil.
ISSUES supplies status when building a synthetic parent rel."
  (when iss
    (let* ((pid (beads-issue-parent iss))
           (from-deps (cl-remove-if-not #'beads-rel-hierarchy-p
                                        (beads-issue-dependencies iss)))
           (rels
            (cond
             (from-deps from-deps)
             ((and pid (not (string-empty-p pid)))
              (let* ((p (and issues
                             (cl-find pid issues
                                      :key #'beads-issue-id :test #'equal)))
                     (st (if p (beads-issue-status p) "")))
                (list (list (cons 'id pid) (cons 'status st)))))
             (t nil))))
      (beads-rel-progress rels issues))))

(defun beads-rel-child-progress (id &optional issues)
  "Return (done . total) for direct children of ID in ISSUES."
  (when (and id issues)
    (let* ((kids (cl-remove-if-not
                  (lambda (iss) (equal (beads-issue-parent iss) id))
                  issues))
           (total (length kids)))
      (when (> total 0)
        (cons (cl-count-if #'beads-issue-closed-p kids) total)))))

(provide 'beads-rel)
;;; beads-rel.el ends here
