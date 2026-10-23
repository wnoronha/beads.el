;;; beads-md.el --- Lightweight markdown fontification -*- lexical-binding: t; -*-

;;; Commentary:
;; Zero-dependency markdown faces for detail bodies (and short list
;; snippets). Uses markdown-mode when available; otherwise applies a
;; small built-in ruleset good enough for issue descriptions.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defcustom beads-md-prefer-markdown-mode t
  "When non-nil and markdown-mode is installed, use it for font-lock."
  :type 'boolean
  :group 'beads)

(declare-function markdown-mode "markdown-mode" ())
(declare-function gfm-view-mode "markdown-mode" ())

(defface beads-md-header
  '((t :inherit font-lock-keyword-face :weight bold
       :underline nil :box nil :background unspecified :extend nil))
  "Markdown ATX headers (flat; avoid TTY highlight backgrounds)."
  :group 'beads-faces)

(defface beads-md-code
  '((t :inherit font-lock-constant-face))
  "Inline `code`."
  :group 'beads-faces)

(defface beads-md-bold
  '((t :weight bold))
  "Markdown bold."
  :group 'beads-faces)

(defface beads-md-italic
  '((t :slant italic))
  "Markdown italic."
  :group 'beads-faces)

(defface beads-md-list
  '((t :inherit font-lock-builtin-face))
  "List markers."
  :group 'beads-faces)

(defface beads-md-checkbox
  '((t :inherit font-lock-warning-face))
  "Task list checkboxes."
  :group 'beads-faces)

(defface beads-md-link
  '((t :inherit link))
  "Markdown links."
  :group 'beads-faces)

(defface beads-md-blockquote
  '((t :inherit font-lock-comment-face :slant italic))
  "Markdown blockquote lines."
  :group 'beads-faces)

(defface beads-md-strike
  '((t :inherit shadow :strike-through t))
  "Markdown strikethrough."
  :group 'beads-faces)

(defface beads-md-hr
  '((t :inherit shadow))
  "Markdown horizontal rules."
  :group 'beads-faces)

(defcustom beads-md-hide-markup t
  "When non-nil, strip common markdown markers for a rendered look."
  :type 'boolean
  :group 'beads)

(defcustom beads-md-issue-id-regexp
  "\\b\\([a-z][a-z0-9]*-[0-9]+\\(?:\\.[0-9]+\\)*\\)\\b"
  "Regexp matching beads issue ids (group 1 = id)."
  :type 'regexp
  :group 'beads)

(defun beads-md--apply-builtin (beg end)
  "Fontify (and optionally unwrap) region BEG..END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      ;; Apply faces first on source form, then strip markers.
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+\\)\\s-+\\(.*\\)$" nil t)
        (put-text-property (match-beginning 2) (match-end 2) 'face 'beads-md-header)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'shadow))
      (goto-char (point-min))
      (while (re-search-forward "^\\(>\\)\\s-?\\(.*\\)$" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'beads-md-blockquote)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'shadow))
      (goto-char (point-min))
      (while (re-search-forward "^```\\([^\n]*\\)\n\\(\\(?:.*\n\\)*?\\)```" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'beads-md-code)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'shadow))
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-code)
        (put-text-property (match-beginning 0) (match-beginning 1) 'face 'shadow)
        (put-text-property (match-end 1) (match-end 0) 'face 'shadow))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\*\\([^*]+\\)\\*\\*\\*" nil t)
        (add-face-text-property (match-beginning 1) (match-end 1) 'beads-md-bold)
        (add-face-text-property (match-beginning 1) (match-end 1) 'beads-md-italic))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-bold))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:^\\|[^*]\\)\\*\\([^*\n]+\\)\\*" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-italic))
      (goto-char (point-min))
      (while (re-search-forward "_\\([^_\n]+\\)_" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-italic))
      (goto-char (point-min))
      (while (re-search-forward "~~\\([^~\n]+\\)~~" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-strike))
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(-\\{3,\\}\\|\\*\\{3,\\}\\)\\s-*$" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'beads-md-hr))
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*|.+|\\s-*$" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'beads-md-code)
        (save-excursion
          (goto-char (match-beginning 0))
          (while (re-search-forward "|" (line-end-position) t)
            (put-text-property (match-beginning 0) (match-end 0)
                               'face 'beads-md-list))))
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\([-*+]\\|[0-9]+[.)]\\)\\s-+\\(\\[[ xX]\\]\\)?" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-list)
        (when (match-beginning 2)
          (put-text-property (match-beginning 2) (match-end 2) 'face 'beads-md-checkbox)))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'beads-md-link))
      (when beads-md-hide-markup
        (let ((inhibit-modification-hooks t))
          (goto-char (point-min))
          (while (re-search-forward "^#+\\s-+" nil t)
            (replace-match "" t t))
          (goto-char (point-min))
          (while (re-search-forward "^>\\s-?" nil t)
            (replace-match (propertize "› " 'face 'beads-md-blockquote) t t))
          (goto-char (point-min))
          (while (re-search-forward "\\*\\*\\*\\([^*]+\\)\\*\\*\\*" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face '(beads-md-bold beads-md-italic)))))
          (goto-char (point-min))
          (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face 'beads-md-bold))))
          (goto-char (point-min))
          (while (re-search-forward "\\*\\([^*\n]+\\)\\*" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face 'beads-md-italic))))
          (goto-char (point-min))
          (while (re-search-forward "_\\([^_\n]+\\)_" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face 'beads-md-italic))))
          (goto-char (point-min))
          (while (re-search-forward "~~\\([^~\n]+\\)~~" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face 'beads-md-strike))))
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*\\(-\\{3,\\}\\|\\*\\{3,\\}\\)\\s-*$" nil t)
            (replace-match (propertize "────" 'face 'beads-md-hr) t t))
          (goto-char (point-min))
          (while (re-search-forward
                  "^```\\([^\n]*\\)\n\\(\\(?:.*\n\\)*?\\)```" nil t)
            (let ((lang (string-trim (or (match-string 1) "")))
                  (txt (match-string 2))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (unless (string-empty-p lang)
                (insert (propertize (concat "‹" lang "›\n") 'face 'shadow)))
              (insert (propertize txt 'face 'beads-md-code))))
          (goto-char (point-min))
          (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
            (let ((txt (match-string 1))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize txt 'face 'beads-md-code))))
          (goto-char (point-min))
          (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
            (let ((label (match-string 1))
                  (url (match-string 2))
                  (m0 (match-beginning 0))
                  (m1 (match-end 0)))
              (delete-region m0 m1)
              (goto-char m0)
              (insert (propertize label 'face 'beads-md-link))
              (when (string-match-p "\\`https?://" url)
                (make-text-button m0 (point)
                                  'action (lambda (_) (browse-url url))
                                  'follow-link t
                                  'face 'beads-md-link
                                  'help-echo (format "Browse %s" url)))))
          ;; Task list boxes → status-like glyphs
          (goto-char (point-min))
          (while (re-search-forward "\\[ \\]" nil t)
            (replace-match (propertize "○" 'face 'beads-md-checkbox) t t))
          (goto-char (point-min))
          (while (re-search-forward "\\[[xX]\\]" nil t)
            (replace-match (propertize "✓" 'face 'beads-face-status-closed) t t))
          ;; Bullet markers → quieter glyphs (keep indent)
          (goto-char (point-min))
          (while (re-search-forward "^\\(\\s-*\\)\\([-*+]\\)\\(\\s-+\\)" nil t)
            (replace-match
             (concat (match-string 1)
                     (propertize "•" 'face 'beads-md-list)
                     (match-string 3))
             t t)))))))

(defun beads-md-buttonize-ids (beg end &optional action)
  "Add buttons on issue ids in region BEG..END.
ACTION is a function of one id string (defaults to message)."
  (let ((act (or action (lambda (id) (message "Issue %s" id))))
        (inhibit-read-only t))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward beads-md-issue-id-regexp end t)
        (let ((id (match-string-no-properties 1))
              (m0 (match-beginning 1))
              (m1 (match-end 1)))
          (unless (button-at m0)
            (make-text-button
             m0 m1
             'action (lambda (_) (funcall act id))
             'follow-link t
             'face 'beads-md-link
             'mouse-face 'highlight
             'help-echo (format "Open %s" id))))))))

(defun beads-md-buttonize-urls (beg end)
  "Add browse-url buttons on http(s) URLs in BEG..END."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "https?://[[:alnum:]./?#&=%~_+-]+" end t)
        (let ((url (match-string-no-properties 0))
              (m0 (match-beginning 0))
              (m1 (match-end 0)))
          (unless (button-at m0)
            (make-text-button
             m0 m1
             'action (lambda (_) (browse-url url))
             'follow-link t
             'face 'beads-md-link
             'mouse-face 'highlight
             'help-echo (format "Browse %s" url))))))))

(defun beads-md--via-markdown-mode (text)
  "Fontify TEXT using markdown-mode; return propertized string or nil."
  (when (and beads-md-prefer-markdown-mode
             (require 'markdown-mode nil t))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks
        (if (fboundp 'gfm-view-mode)
            (gfm-view-mode)
          (markdown-mode)))
      (when (boundp 'markdown-hide-markup)
        (setq-local markdown-hide-markup t))
      (font-lock-ensure (point-min) (point-max))
      (buffer-string))))

(defun beads-md-string (text)
  "Return TEXT with markdown faces applied."
  (cond
   ((or (null text) (string-empty-p text)) "(none)")
   ((beads-md--via-markdown-mode text))
   (t
    (with-temp-buffer
      (insert text)
      (beads-md--apply-builtin (point-min) (point-max))
      (buffer-string)))))

(defun beads-md--wrap-line (line width)
  "Word-wrap LINE to WIDTH; return list of lines (preserve leading indent)."
  (let* ((line (string-trim-right line))
         (indent (and (string-match "\\`\\([ \t]*\\)" line)
                      (match-string 1 line)))
         (indent (or indent ""))
         (body (substring line (length indent)))
         (pad (length indent))
         (avail (max 8 (- width pad)))
         (out nil)
         (rest body))
    (if (<= (string-width line) width)
        (list line)
      (while (and rest (not (string-empty-p rest)))
        (if (<= (string-width rest) avail)
            (progn
              (push (concat indent rest) out)
              (setq rest nil))
          (let* ((chunk (truncate-string-to-width rest avail nil nil ""))
                 (break (or (cl-position ?\s chunk :from-end t)
                            (cl-position ?\t chunk :from-end t)
                            (length chunk)))
                 (piece (string-trim-right (substring rest 0 break)))
                 (next (string-trim-left (substring rest break))))
            (when (string-empty-p piece)
              (setq piece (truncate-string-to-width rest avail nil nil "…")
                    next ""))
            (push (concat indent piece) out)
            (setq rest next)
            (setq indent (concat indent "  ")
                  pad (length indent)
                  avail (max 8 (- width pad))))))
      (nreverse out))))

(defun beads-md-snippet (text &optional max-lines max-width)
  "Return a short markdown-rendered multi-line snippet from TEXT.
MAX-LINES defaults to 2; MAX-WIDTH defaults to 88.
Preserves leading indentation on list/todo lines (trim right only).
Long lines wrap on word boundaries when possible."
  (let* ((src (string-trim-right (or text "")))
         (src (replace-regexp-in-string "<!--\\(?:.\\|\n\\)*?-->" "" src))
         ;; Drop YAML frontmatter so list previews start on real body.
         (src (replace-regexp-in-string
               "\\`---[\t ]*\n\\(?:.*\n\\)*?---[\t ]*\n?" "" src))
         ;; Drop leading blank lines only; keep first-line indent.
         (src (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" "" src))
         (src (string-trim-right src))
         (n (or max-lines 2))
         (w (or max-width 88)))
    (if (string-empty-p src)
        ""
      (let* ((rendered (beads-md-string src))
             (raw (split-string rendered "\n" t))
             (wrapped (cl-mapcan (lambda (line) (beads-md--wrap-line line w))
                                 raw))
             (taken (cl-subseq wrapped 0 (min n (length wrapped))))
             (out (mapcar
                   (lambda (line)
                     (if (<= (string-width line) w)
                         line
                       (truncate-string-to-width line w nil nil "…")))
                   taken)))
        (string-join out "\n")))))

(provide 'beads-md)
;;; beads-md.el ends here
