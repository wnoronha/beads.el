;;; beads-yank.el --- Yank helpers for list (and shared preview source) -*- lexical-binding: t; -*-

;;; Commentary:
;; Yank helpers for list (and shared preview source).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)
(require 'beads-md)
(require 'beads-faces)

(require 'beads-cli)
(declare-function beads-list--id-at-point "beads-list-render" ())
(declare-function beads-list-merge-from-show "beads-list-follow" (id full))

(defun beads-list--latest-comment-text (iss)
  "Plain text of the newest comment on ISS, or nil."
  (let ((comments (beads-issue-comments iss)))
    (when comments
      (let* ((c (car (last comments)))
             (author (or (alist-get 'author c) "?"))
             (text (string-trim (or (alist-get 'text c) ""))))
        (unless (string-empty-p text)
          (format "> %s — %s" author
                  (replace-regexp-in-string "\n" " " text)))))))

(defun beads-list--preview-source (iss)
  "Markdown body for yank / view-md / checkbox helpers (not list under-row)."
  (let* ((desc (string-trim-right (or (beads-issue-description iss) "")))
         (desc (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" "" desc))
         (design (string-trim-right (or (beads-issue-design iss) "")))
         (accept (string-trim-right (or (beads-issue-acceptance iss) "")))
         (parts nil))
    (unless (string-empty-p (string-trim desc))
      (push desc parts))
    (unless (string-empty-p (string-trim design))
      (push design parts))
    (when (and (not (string-empty-p (string-trim accept)))
               (string-match-p "\\[[ xX]\\]\\|^[-*+]\\s-" accept))
      (push (concat "### Acceptance\n" accept) parts))
    (when (> (or (beads-issue-comment-count iss) 0) 0)
      (when-let* ((cmt (beads-list--latest-comment-text iss)))
        (push cmt parts)))
    (string-join (nreverse parts) "\n\n")))
(defvar beads--id-at-point-fn)
(defvar beads-detail--issue)

(defun beads--issue-at-point ()
  "Return issue alist at point (list row or detail buffer), or nil."
  (or (get-text-property (line-beginning-position) 'beads-issue)
      (bound-and-true-p beads-detail--issue)))

(defun beads-yank-id ()
  "Copy issue id at point to the kill ring (list or detail)."
  (interactive)
  (let ((id (or (and (functionp beads--id-at-point-fn)
                     (funcall beads--id-at-point-fn))
                (let ((iss (beads--issue-at-point)))
                  (and iss (beads-issue-id iss))))))
    (unless id (user-error "No issue at point"))
    (kill-new id)
    (message "Copied %s" id)))

(defun beads-yank-title ()
  "Copy issue title at point to the kill ring (list or detail)."
  (interactive)
  (let ((iss (beads--issue-at-point)))
    (unless iss (user-error "No issue at point"))
    (let ((title (beads-issue-title iss)))
      (kill-new title)
      (message "Copied title"))))

(defun beads-list-yank-org ()
  "Copy an Org-mode link for the issue at point."
  (interactive)
  (let ((iss (get-text-property (line-beginning-position) 'beads-issue)))
    (unless iss (user-error "No issue at point"))
    (let* ((id (beads-issue-id iss))
           (title (beads-issue-title iss))
           (link (format "[[btu:%s][%s]]" id title)))
      (kill-new link)
      (message "Copied %s" link))))

(defun beads-list-yank-markdown ()
  "Copy a markdown link `[title](btu:id)` for the issue at point."
  (interactive)
  (let ((iss (get-text-property (line-beginning-position) 'beads-issue)))
    (unless iss (user-error "No issue at point"))
    (let* ((id (beads-issue-id iss))
           (title (beads-issue-title iss))
           (link (format "[%s](btu:%s)" title id)))
      (kill-new link)
      (message "Copied %s" link))))

(defun beads-list-yank-description ()
  "Copy issue description at point."
  (interactive)
  (let ((iss (get-text-property (line-beginning-position) 'beads-issue)))
    (unless iss (user-error "No issue at point"))
    (let ((desc (beads-issue-description iss)))
      (when (string-empty-p desc)
        (user-error "No description"))
      (kill-new desc)
      (message "Copied description (%d chars)" (length desc)))))

(defun beads-list-yank-preview-rendered ()
  "Copy the rendered markdown preview for the issue at point."
  (interactive)
  (let ((iss (get-text-property (line-beginning-position) 'beads-issue)))
    (unless iss (user-error "No issue at point"))
    (let* ((beads-md-prefer-markdown-mode nil)
           (src (beads-list--preview-source iss))
           (snip (beads-md-snippet src 12 120))
           (plain (substring-no-properties snip)))
      (when (string-empty-p (string-trim plain))
        (user-error "No preview text"))
      (kill-new plain)
      (message "Copied rendered preview (%d chars)" (length plain)))))

(defun beads-list-view-markdown ()
  "Show full rendered markdown for the issue at point in a side buffer.
Works from list or detail."
  (interactive)
  (let ((iss (beads--issue-at-point)))
    (unless iss (user-error "No issue at point"))
    ;; One-shot show when list stub lacks bodies (detail owns ongoing enrich).
    (when (and (eq major-mode 'beads-list-mode)
               (string-empty-p (string-trim (or (beads-issue-description iss) "")))
               (string-empty-p (string-trim (or (beads-issue-design iss) ""))))
      (when-let* ((full (ignore-errors (beads-cli-show (beads-issue-id iss)))))
        (beads-list-merge-from-show (beads-issue-id iss) full)
        (setq iss (or (get-text-property (line-beginning-position) 'beads-issue)
                      iss))))
    (let* ((id (beads-issue-id iss))
           (src (beads-list--preview-source iss))
           (beads-md-prefer-markdown-mode nil)
           (rendered (beads-md-string
                      (if (string-empty-p (string-trim src))
                          (or (beads-issue-description iss) "")
                        src)))
           (buf (get-buffer-create "*beads-md*")))
      (when (or (string-empty-p (string-trim (substring-no-properties rendered)))
                (string= (substring-no-properties rendered) "(none)"))
        (user-error "No markdown body"))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "%s\n" id) 'face 'beads-face-id)
                  (propertize (beads-issue-title iss) 'face 'bold)
                  "\n"
                  (propertize (make-string 40 ?─) 'face 'shadow)
                  "\n\n"
                  rendered)
          (goto-char (point-min))
          (view-mode 1)
          (setq header-line-format
                (format " beads md · %s · q to quit" id))))
      (display-buffer buf '(display-buffer-below-selected
                            (window-height . 0.35)))
      (message "markdown view %s" id))))

(defun beads-list-browse-external ()
  "Open `external_ref' in a browser when present.
Works from list or detail."
  (interactive)
  (let ((iss (beads--issue-at-point)))
    (unless iss (user-error "No issue at point"))
    (let ((ref (beads-issue-external-ref iss)))
      (when (or (null ref) (string-empty-p ref))
        (user-error "No external_ref on %s" (beads-issue-id iss)))
      (browse-url ref))))

(provide 'beads-yank)
;;; beads-yank.el ends here
