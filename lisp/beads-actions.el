;;; beads-actions.el --- Mutating bd actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Claim, comment, create, close, status, title, reparent — write paths
;; shared by list/detail.

;;; Code:

(require 'beads-cli)

(defun beads-action-claim (id &optional on-done)
  "Claim ID. Call ON-DONE with (ok err) when finished."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Claim issue: "))
         nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (if on-done
      (beads-cli-call-async
       (lambda (ok data)
         (funcall on-done ok (unless ok data)))
       "update" id "--claim")
    (beads-cli-claim id)
    (message "Claimed %s" id)))

(defvar-local beads-action--comment-id nil
  "Issue id for the active comment compose buffer.")
(defvar-local beads-action--comment-on-done nil
  "Callback (ok err) for the active comment compose buffer.")

(defun beads-action--post-comment (id text &optional on-done)
  "Post TEXT on ID via `bd comment --file' (supports multiline).
Call ON-DONE with (ok err) when finished."
  (let ((tmp (make-temp-file "beads-comment-" nil ".md")))
    (with-temp-file tmp
      (insert text))
    (if on-done
        (beads-cli-call-async
         (lambda (ok data)
           (ignore-errors (delete-file tmp))
           (funcall on-done ok (unless ok data)))
         "comment" id "--file" tmp)
      (unwind-protect
          (progn
            (beads-cli-call "comment" id "--file" tmp)
            (message "Commented on %s" id))
        (ignore-errors (delete-file tmp))))))

(defun beads-action--dismiss-compose (buf)
  "Kill comment compose BUF and remove its split window.
`pop-to-buffer' opens below the detail pane; a bare `kill-buffer'
leaves that window behind (often showing `*beads-detail*' twice)."
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil t))
      (when (and (window-live-p win)
                 (not (one-window-p t)))
        (delete-window win)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun beads-action-comment-submit ()
  "Submit the comment compose buffer (bound to C-c C-c)."
  (interactive)
  (let ((id beads-action--comment-id)
        (on-done beads-action--comment-on-done)
        (body (string-trim-right (buffer-string)))
        (buf (current-buffer)))
    (when (string-empty-p (string-trim body))
      (user-error "Empty comment"))
    (unless id (user-error "No issue id"))
    (beads-action--post-comment
     id body
     (lambda (ok err)
       (beads-action--dismiss-compose buf)
       (when on-done
         (funcall on-done ok err))
       (unless on-done
         (if ok (message "Commented on %s" id)
           (message "Comment failed: %s" err)))))))

(defun beads-action-comment-cancel ()
  "Cancel the comment compose buffer (bound to C-c C-k)."
  (interactive)
  (let ((on-done beads-action--comment-on-done)
        (buf (current-buffer)))
    (beads-action--dismiss-compose buf)
    (when on-done
      (funcall on-done nil "canceled"))
    (message "Comment canceled")))

(defvar beads-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-action-comment-submit)
    (define-key map (kbd "C-c C-k") #'beads-action-comment-cancel)
    map)
  "Keymap for `beads-comment-mode'.")

(define-derived-mode beads-comment-mode text-mode "Beads-Comment"
  "Compose a Beads comment. C-c C-c submits; C-c C-k cancels."
  (setq header-line-format
        (list " Comment on "
              '(:eval (or beads-action--comment-id "?"))
              " — C-c C-c submit · C-c C-k cancel")))

(defun beads-action-comment (id &optional text on-done)
  "Add a comment on ID.
When TEXT is non-nil, post it immediately.
Otherwise open a multiline compose buffer (C-c C-c to submit).
Call ON-DONE with (ok err) when finished (err may be \"canceled\")."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Comment on: "))
         nil nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (if (and text (not (string-empty-p (string-trim text))))
      (beads-action--post-comment id text on-done)
    (let ((buf (get-buffer-create (format "*beads-comment:%s*" id))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer))
        (beads-comment-mode)
        (setq beads-action--comment-id id
              beads-action--comment-on-done on-done
              default-directory (beads-cli--workdir)))
      (pop-to-buffer buf
                     '((display-buffer-below-selected)
                       (window-height . 10)))
      (message "Edit comment; C-c C-c to submit"))))

(defvar beads--id-at-point-fn nil
  "Buffer-local function returning issue id at point, or nil.")

(defun beads--id-at-point ()
  "Return issue id at point via `beads--id-at-point-fn'."
  (when (functionp beads--id-at-point-fn)
    (funcall beads--id-at-point-fn)))

(defvar beads-list-buffer-name)
(defvar beads-detail-buffer-name)
(defvar beads-detail--comments-fetched-id)
(declare-function beads-list-refresh "beads-list-data" (&optional seed-fold))
(declare-function beads-list--bump-comment-count "beads-list-data" (id))
(declare-function beads-detail-current-id "beads-detail" ())
(declare-function beads-detail-refresh "beads-detail" ())

(defun beads--refresh-after-mutation (id &optional switch-comments)
  "Refresh list/detail after a write on ID.
When SWITCH-COMMENTS, bump list comment count and jump to Comments."
  (when-let* ((lbuf (get-buffer beads-list-buffer-name)))
    (with-current-buffer lbuf
      (when (eq major-mode 'beads-list-mode)
        (when switch-comments
          (beads-list--bump-comment-count id))
        (beads-list-refresh))))
  (when-let* ((db (get-buffer beads-detail-buffer-name)))
    (with-current-buffer db
      (when (equal (beads-detail-current-id) id)
        (when switch-comments
          (setq beads-detail--comments-fetched-id nil))
        (beads-detail-refresh)))))

(defun beads-claim ()
  "Claim the issue at point (list or detail)."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (beads-action-claim
     id
     (lambda (ok err)
       (if ok
           (progn
             (message "Claimed %s" id)
             (beads--refresh-after-mutation id))
         (message "Claim failed: %s" err))))))

(defun beads-comment ()
  "Comment on the issue at point (list or detail)."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (beads-action-comment
     id nil
     (lambda (ok err)
       (cond
        ((equal err "canceled") nil)
        (ok
         (message "Commented on %s" id)
         (beads--refresh-after-mutation id t))
        (t (message "Comment failed: %s" err)))))))

(defconst beads-action-status-choices
  '("open" "in_progress" "closed" "deferred" "blocked")
  "Common status values for `beads-action-set-status'.")

(defun beads-action-create (title &optional description on-done)
  "Create an issue with TITLE and optional DESCRIPTION.
Call ON-DONE with (ok data-or-err) when finished (payload on success)."
  (interactive
   (list (read-string "Title: ")
         (let ((d (read-string "Description (empty to skip): ")))
           (and (not (string-empty-p d)) d))
         nil))
  (unless (and title (not (string-empty-p (string-trim title))))
    (user-error "Empty title"))
  (let ((args (list "create" "--title" title)))
    (when (and description (not (string-empty-p description)))
      (setq args (append args (list "--description" description))))
    (if on-done
        (apply #'beads-cli-call-async
               (lambda (ok data) (funcall on-done ok data))
               args)
      (let ((payload (apply #'beads-cli-call args)))
        (message "Created %s"
                 (or (and (listp payload) (alist-get 'id payload)) title))
        payload))))

(defun beads-action-close (id &optional reason on-done)
  "Close ID after `yes-or-no-p' confirmation. Optional REASON.
When REASON is nil, prompt. Pass \"\" to skip without prompting.
Call ON-DONE with (ok err) when finished."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Close issue: "))
         nil nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (unless (yes-or-no-p (format "Close %s? " id))
    (user-error "Canceled"))
  (when (eq reason nil)
    (let ((r (read-string "Reason (empty to skip): ")))
      (setq reason (or r ""))))
  (let ((args (append (list "close" id)
                      (and reason (not (string-empty-p reason))
                           (list "--reason" reason)))))
    (if on-done
        (apply #'beads-cli-call-async
               (lambda (ok data)
                 (funcall on-done ok (unless ok data)))
               args)
      (apply #'beads-cli-call args)
      (message "Closed %s" id))))

(defun beads-action-set-status (id status &optional on-done)
  "Set STATUS on ID. Call ON-DONE with (ok err) when finished."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Issue: "))
         (completing-read "Status: " beads-action-status-choices nil t)
         nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (unless (and status (not (string-empty-p status)))
    (user-error "No status"))
  (if on-done
      (beads-cli-call-async
       (lambda (ok data)
         (funcall on-done ok (unless ok data)))
       "update" id "--status" status)
    (beads-cli-set-status id status)
    (message "Set %s status to %s" id status)))

(defun beads-action-set-title (id title &optional on-done)
  "Set TITLE on ID. Call ON-DONE with (ok err) when finished."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Issue: "))
         (read-string "New title: ")
         nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (unless (and title (not (string-empty-p (string-trim title))))
    (user-error "Empty title"))
  (if on-done
      (beads-cli-call-async
       (lambda (ok data)
         (funcall on-done ok (unless ok data)))
       "update" id "--title" title)
    (beads-cli-set-title id title)
    (message "Retitled %s" id)))

(defun beads-action-reparent (id parent &optional on-done)
  "Reparent ID under PARENT (empty string clears). Confirm first.
When PARENT is nil, prompt. Call ON-DONE with (ok err) when finished."
  (interactive
   (list (or (beads--id-at-point)
             (read-string "Issue: "))
         nil nil))
  (unless (and id (not (string-empty-p id)))
    (user-error "No issue id"))
  (when (eq parent nil)
    (setq parent (read-string "Parent id (empty to clear): ")))
  (unless (yes-or-no-p
           (if (string-empty-p parent)
               (format "Clear parent of %s? " id)
             (format "Reparent %s under %s? " id parent)))
    (user-error "Canceled"))
  (if on-done
      (beads-cli-call-async
       (lambda (ok data)
         (funcall on-done ok (unless ok data)))
       "update" id "--parent" parent)
    (beads-cli-reparent id parent)
    (message (if (string-empty-p parent)
                 "Cleared parent of %s"
               "Reparented %s")
             id)))

(defun beads-create ()
  "Create a new issue and refresh the list."
  (interactive)
  (let ((title (read-string "Title: "))
        (desc (let ((d (read-string "Description (empty to skip): ")))
                (and (not (string-empty-p d)) d))))
    (beads-action-create
     title desc
     (lambda (ok data)
       (if ok
           (let ((new-id (and (listp data) (alist-get 'id data))))
             (message "Created %s" (or new-id title))
             (beads--refresh-after-mutation (or new-id "")))
         (message "Create failed: %s" data))))))

(defun beads-close ()
  "Close the issue at point after confirmation."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (beads-action-close
     id nil
     (lambda (ok err)
       (if ok
           (progn
             (message "Closed %s" id)
             (beads--refresh-after-mutation id))
         (message "Close failed: %s" err))))))

(defun beads-set-status ()
  "Set status of the issue at point."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (let ((status (completing-read "Status: " beads-action-status-choices
                                   nil t)))
      (beads-action-set-status
       id status
       (lambda (ok err)
         (if ok
             (progn
               (message "Set %s status to %s" id status)
               (beads--refresh-after-mutation id))
           (message "Status failed: %s" err)))))))

(defun beads-set-title ()
  "Edit title of the issue at point."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (let ((title (read-string "New title: ")))
      (beads-action-set-title
       id title
       (lambda (ok err)
         (if ok
             (progn
               (message "Retitled %s" id)
               (beads--refresh-after-mutation id))
           (message "Retitle failed: %s" err)))))))

(defun beads-reparent ()
  "Reparent the issue at point after confirmation."
  (interactive)
  (let ((id (beads--id-at-point)))
    (unless id (user-error "No issue at point"))
    (beads-action-reparent
     id nil
     (lambda (ok err)
       (if ok
           (progn
             (message "Reparented %s" id)
             (beads--refresh-after-mutation id))
         (message "Reparent failed: %s" err))))))

(provide 'beads-actions)
;;; beads-actions.el ends here
