;;; beads-issue.el --- Issue alist accessors -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers over bd JSON alists. Keys are symbols from json-parse-string.
;; Columnar list line formatting lives in `beads-coltree'.

;;; Code:

(defun beads-issue-get (issue key &optional default)
  "Return KEY from ISSUE alist, or DEFAULT."
  (let ((v (alist-get key issue nil nil #'eq)))
    (if (eq v nil) default v)))

(defun beads-issue-json-true-p (value)
  "Non-nil when VALUE is a JSON true from `beads-cli--parse'.
Accepts t and the rare string \"true\" from odd payloads."
  (or (eq value t)
      (and (stringp value) (string= (downcase value) "true"))))

(defun beads-issue-pinned-p (issue)
  "Non-nil when ISSUE is pinned."
  (beads-issue-json-true-p (beads-issue-get issue 'pinned)))

(defun beads-issue-id (issue)
  "Issue id."
  (beads-issue-get issue 'id ""))

(defun beads-issue-title (issue)
  "Issue title."
  (beads-issue-get issue 'title ""))

(defun beads-issue-status (issue)
  "Issue status string."
  (beads-issue-get issue 'status ""))

(defun beads-issue-priority (issue)
  "Issue priority as integer."
  (or (beads-issue-get issue 'priority) 0))

(defun beads-issue-type (issue)
  "Issue type string."
  (beads-issue-get issue 'issue_type ""))

(defun beads-issue-assignee (issue)
  "Assignee string."
  (or (beads-issue-get issue 'assignee) ""))

(defun beads-issue-owner (issue)
  "Owner string (often user@host)."
  (or (beads-issue-get issue 'owner) ""))

(defun beads-issue-assignee-handle (issue)
  "Login-style handle for ISSUE (e.g. alice), never a display name.
Prefer `owner' local-part (user@host); else bare assignee without spaces."
  (let* ((owner (beads-issue-owner issue))
         (who (beads-issue-assignee issue)))
    (cond
     ((and owner (string-match "\\`\\([^@[:space:]]+\\)@" owner))
      (match-string 1 owner))
     ((and who (string-match "\\`\\([^@[:space:]]+\\)@" who))
      (match-string 1 who))
     ((and who (not (string-match-p "[[:space:]]" who))
           (not (string-empty-p who)))
      who)
     (t nil))))

(defun beads-issue-parent (issue)
  "Parent id or empty string."
  (or (beads-issue-get issue 'parent) ""))

(defun beads-issue-updated-at (issue)
  "Updated-at timestamp string."
  (or (beads-issue-get issue 'updated_at) ""))

(defun beads-issue-description (issue)
  "Description text."
  (or (beads-issue-get issue 'description) ""))

(defun beads-issue-design (issue)
  "Design text."
  (or (beads-issue-get issue 'design) ""))

(defun beads-issue-acceptance (issue)
  "Acceptance criteria text."
  (or (beads-issue-get issue 'acceptance_criteria) ""))

(defun beads-issue-notes (issue)
  "Notes text."
  (or (beads-issue-get issue 'notes) ""))

(defun beads-issue-labels (issue)
  "Label list."
  (or (beads-issue-get issue 'labels) '()))

(defun beads-issue-dependencies (issue)
  "Dependencies list (outgoing links)."
  (or (beads-issue-get issue 'dependencies) '()))

(defun beads-issue-dependents (issue)
  "Dependents list (incoming links)."
  (or (beads-issue-get issue 'dependents) '()))

(defun beads-issue-comments (issue)
  "Embedded comments list."
  (or (beads-issue-get issue 'comments) '()))

(defun beads-issue-comment-count (issue)
  "Comment count integer."
  (or (beads-issue-get issue 'comment_count) 0))

(defun beads-issue-closed-p (issue)
  "Non-nil when ISSUE is closed."
  (string= (beads-issue-status issue) "closed"))

(defun beads-issue-external-ref (issue)
  "External reference URL/id when present."
  (let ((v (beads-issue-get issue 'external_ref)))
    (cond
     ((stringp v) v)
     ((and (listp v) (stringp (car v))) (car v))
     (t ""))))

(defun beads-issue-due-at (issue)
  "Due timestamp string, or empty."
  (or (beads-issue-get issue 'due_at) ""))

(defun beads-issue-created-at (issue)
  "Created-at timestamp string."
  (or (beads-issue-get issue 'created_at) ""))

(defun beads-issue-started-at (issue)
  "Started-at timestamp string."
  (or (beads-issue-get issue 'started_at) ""))

(defun beads-issue-created-by (issue)
  "Creator string."
  (or (beads-issue-get issue 'created_by) ""))

(defun beads-issue-estimated-minutes (issue)
  "Estimate in minutes, or nil."
  (beads-issue-get issue 'estimated_minutes))

(defun beads-issue-spec-id (issue)
  "Spec document id, or empty."
  (or (beads-issue-get issue 'spec_id) ""))

(defun beads-issue-metadata (issue)
  "Custom metadata alist (or nil)."
  (beads-issue-get issue 'metadata))

(defun beads-issue-rel-id (rel)
  "Target id from a dependency/dependent alist REL."
  (or (alist-get 'depends_on_id rel)
      (alist-get 'issue_id rel)
      (alist-get 'id rel)
      ""))

(defun beads-issue-rel-type (rel)
  "Dependency type from REL."
  (or (alist-get 'type rel)
      (alist-get 'dependency_type rel)
      ""))

(defun beads-issue-rel-title (rel)
  "Title from REL when present."
  (or (alist-get 'title rel) ""))

(provide 'beads-issue)
;;; beads-issue.el ends here
