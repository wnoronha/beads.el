;;; beads-detail.el --- Issue detail major mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Detail major mode: single-page issue view, claim/comment/yank, siblings.
;; Rendering lives in beads-detail-render.el; shared keys in beads-keymap.el.

;;; Code:

(require 'beads-cli)
(require 'beads-issue)
(require 'beads-actions)
(require 'beads-layout)
(require 'beads-md)
(require 'beads-yank)
(require 'beads-detail-render)
(require 'beads-keymap)
(require 'outline)
(require 'cl-lib)

(defvar beads-list-focus-issue-fn nil
  "Function (ID) to focus ID in the list and follow into detail.
Set by list-follow load; detail siblings use this instead of requiring list.")

(defun beads-detail-current-id ()
  "Return the issue id shown in the current detail buffer, or nil."
  beads-detail--id)

(defun beads-detail-shown-id ()
  "Return the issue id in the detail buffer, or nil if none."
  (when-let* ((db (get-buffer beads-detail-buffer-name)))
    (buffer-local-value 'beads-detail--id db)))

(defun beads-detail-goto-section (title &optional force-comments)
  "Jump to outline section TITLE in the detail buffer.
If FORCE-COMMENTS, clear the comments fetch cache and refresh.
Recenter like TAB hops (`beads-detail-heading-window-line')."
  (when force-comments
    (setq beads-detail--comments-fetched-id nil)
    (beads-detail-refresh))
  (unless (beads-detail--goto-heading title)
    (user-error "No section %s" title))
  (beads-detail--after-heading-hop))

(defun beads-detail-refresh ()
  "Reload current issue from bd.
Always targets the body detail buffer (safe from pinned meta)."
  (interactive)
  (let* ((buf (beads-detail--prepare-buffer))
         (id (buffer-local-value 'beads-detail--id buf)))
    (unless id
      (user-error "No issue"))
    (with-current-buffer buf
      (setq beads-detail--loading t)
      (force-mode-line-update))
    (beads-cli-call-async
     (lambda (ok data)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           ;; Drop stale responses (Gnus keeps one current article).
           (when (equal beads-detail--id id)
             (if ok
                 (let ((issue (beads-cli--unwrap-show data)))
                   (if issue
                       (beads-detail--apply issue)
                     (setq beads-detail--loading nil)
                     (message "beads show: empty payload")))
               (setq beads-detail--loading nil)
               (message "beads show: %s" data))))))
     "show" id "--include-comments" "--include-dependents")))

(defun beads-detail--ensure-outline ()
  "Signal if outline minor mode is not active."
  (unless (bound-and-true-p outline-minor-mode)
    (user-error "No outline sections")))

(defcustom beads-detail-heading-window-line 1
  "Window line (0-based) for the hopped outline heading.
After TAB/S-TAB or section jumps, the heading is recentered here so
it always appears in the same place.  Default 1 = second visible
line (near-top): read the section body downward.  Use nil to leave
point wherever Emacs scrolls; use a mid-window integer (e.g. height/2)
if you prefer center."
  :type '(choice (const :tag "No recenter" nil)
                 (integer :tag "Window line (0 = top)"))
  :group 'beads)

(defun beads-detail--message-heading ()
  "Echo the section key for the heading at point."
  (when (outline-on-heading-p)
    (beginning-of-line)
    (message "%s"
             (beads-detail--section-key
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))))

(defun beads-detail--recenter-heading ()
  "Put heading at point on `beads-detail-heading-window-line'.
Uses the detail window even when another window is selected (e.g. M-:)."
  (when (integerp beads-detail-heading-window-line)
    (when-let* ((win (get-buffer-window (current-buffer) 0)))
      (with-selected-window win
        (recenter beads-detail-heading-window-line)))))

(defun beads-detail--after-heading-hop ()
  "Recenter hopped heading and echo its name."
  (beginning-of-line)
  (beads-detail--recenter-heading)
  (beads-detail--message-heading))

(defun beads-detail-next-heading ()
  "Jump to the next section heading (wraps to the first).
Bound to TAB — hop, do not fold.  Heading is pinned near the top
of the window (`beads-detail-heading-window-line')."
  (interactive)
  (beads-detail--ensure-outline)
  (let ((start (point)))
    (condition-case nil
        (outline-next-visible-heading 1)
      (error (goto-char (point-min))
             (outline-next-visible-heading 1)))
    (when (or (not (outline-on-heading-p)) (= (point) start))
      (goto-char (point-min))
      (outline-next-visible-heading 1)))
  (beads-detail--after-heading-hop))

(defun beads-detail-prev-heading ()
  "Jump to the previous section heading (wraps to the last).
Bound to S-TAB — hop, do not fold.  Heading is pinned near the top
of the window (`beads-detail-heading-window-line')."
  (interactive)
  (beads-detail--ensure-outline)
  (let ((start (point)))
    (condition-case nil
        (outline-previous-visible-heading 1)
      (error (goto-char (point-max))
             (outline-previous-visible-heading 1)))
    (when (or (not (outline-on-heading-p)) (= (point) start))
      (goto-char (point-max))
      (outline-previous-visible-heading 1)))
  (beads-detail--after-heading-hop))

(defun beads-detail-toggle-fold ()
  "Fold or unfold the outline section at point.
Prefer TAB for hopping; use this (z) when you mean to collapse."
  (interactive)
  (beads-detail--ensure-outline)
  (unless (outline-on-heading-p)
    (outline-previous-heading))
  (unless (outline-on-heading-p)
    (user-error "No section heading"))
  (let ((key (beads-detail--section-key
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))))
    (outline-toggle-children)
    (beads-detail--capture-folds)
    (if (beads-detail--heading-collapsed-p)
        (progn
          (setq beads-detail--expanded-empty-sections
                (delete key beads-detail--expanded-empty-sections))
          (message "Folded %s" key))
      (save-excursion
        (forward-line 1)
        (when (looking-at-p
               (concat (regexp-quote (beads-detail--pad)) "(none)"))
          (cl-pushnew key beads-detail--expanded-empty-sections
                      :test #'equal)))
      (message "Unfolded %s" key))))

(defun beads-detail--id-at-point ()
  "Return issue id at point, or the buffer's current issue id."
  (or (get-text-property (point) 'beads-id)
      beads-detail--id))

(defun beads-detail-follow-link ()
  "Follow issue id at point (button, property, or symbol)."
  (interactive)
  (cond
   ((button-at (point))
    (push-button))
   ((get-text-property (point) 'beads-id)
    (beads-detail-show (get-text-property (point) 'beads-id)))
   ((let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((tok (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (when (string-match beads-md-issue-id-regexp tok)
            (beads-detail-show (match-string 1 tok))
            t))))
    nil)
   (t (user-error "No issue id at point"))))

(defun beads-detail-yank-section-rendered (getter label)
  "Copy section from GETTER with markdown unwrapped; LABEL for messages."
  (unless beads-detail--issue (user-error "No issue"))
  (let* ((raw (funcall getter beads-detail--issue))
         (rendered (substring-no-properties (beads-md-string raw))))
    (when (or (string-empty-p (string-trim (or raw "")))
              (string= rendered "(none)"))
      (user-error "No %s" label))
    (kill-new rendered)
    (message "Copied rendered %s (%d chars)" label (length rendered))))

(defun beads-detail-yank-description-rendered ()
  "Copy description with markdown markers unwrapped (plain rendered text)."
  (interactive)
  (beads-detail-yank-section-rendered #'beads-issue-description "description"))

(defun beads-detail-yank-notes-rendered ()
  "Copy notes with markdown markers unwrapped."
  (interactive)
  (beads-detail-yank-section-rendered #'beads-issue-notes "notes"))

(defun beads-detail-yank-design-rendered ()
  "Copy design with markdown markers unwrapped."
  (interactive)
  (beads-detail-yank-section-rendered #'beads-issue-design "design"))

(defun beads-detail-yank-at-heading ()
  "Yank rendered section for the outline heading at or above point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at outline-regexp)
      (outline-previous-heading))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (cond
       ((string-match-p "Description" line)
        (beads-detail-yank-description-rendered))
       ((string-match-p "Design" line)
        (beads-detail-yank-design-rendered))
       ((string-match-p "Notes" line)
        (beads-detail-yank-notes-rendered))
       ((string-match-p "Acceptance" line)
        (beads-detail-yank-section-rendered #'beads-issue-acceptance
                                            "acceptance"))
       (t (user-error "No yankable section at point"))))))

(defun beads-detail--visible-sibling-ids ()
  "Sibling ids of the current issue as shown in the list buffer."
  (let* ((list-buf (get-buffer beads-list-buffer-name))
         (pid (and beads-detail--issue
                   (beads-issue-parent beads-detail--issue)))
         (pid (if (and pid (not (string-empty-p pid))) pid nil))
         ids)
    (when list-buf
      (with-current-buffer list-buf
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (get-text-property (point) 'beads-issue)
              (let* ((iss (get-text-property (point) 'beads-issue))
                     (p (beads-issue-parent iss))
                     (p (if (and p (not (string-empty-p p))) p nil)))
                (when (equal p pid)
                  (push (beads-issue-id iss) ids))))
            (forward-line 1))))
      (nreverse ids))))

(defun beads-detail-sibling (dir)
  "Move DIR (±1) among visible siblings and follow in the list."
  (let* ((ids (beads-detail--visible-sibling-ids))
         (cur beads-detail--id)
         (idx (cl-position cur ids :test #'equal)))
    (unless (and ids idx)
      (user-error "No siblings visible in list"))
    (unless (functionp beads-list-focus-issue-fn)
      (user-error "List focus not available"))
    (let ((next (nth (mod (+ idx dir) (length ids)) ids)))
      (funcall beads-list-focus-issue-fn next)
      (message "sibling %s" next))))

(defun beads-detail-next-sibling ()
  "Show next sibling issue (same parent) from the list."
  (interactive)
  (beads-detail-sibling 1))

(defun beads-detail-prev-sibling ()
  "Show previous sibling issue (same parent) from the list."
  (interactive)
  (beads-detail-sibling -1))

(defun beads-detail-quit ()
  "Leave detail without ending the Beads session.
Standalone (`beads-layout' nil): Emacs `quit-window' (same as
`special-mode' q / typically restores the list after RET).
Paired layouts: tear down the detail pane via `beads-layout-quit-detail'.
`C-x k' also works to kill/bury this buffer."
  (interactive)
  (if (beads-layout--paired-p)
      (beads-layout-quit-detail)
    (quit-window)))

(define-derived-mode beads-detail-mode special-mode "Beads-Detail"
  "Major mode for a single Beads issue (single scrollable page).

Layout and follow
  Standalone default: RET from the list switches to this buffer.
  With `beads-layout' `stacked'/`side-by-side', shown beside or
  below the list.  List motion refreshes this buffer when
  `beads-auto-follow' is on (paired layouts keep list focus).

Chrome
  No custom header-line.  Mode-line is stock Emacs.  Content is one
  page: MOCK 10 meta, Description/Design/Acceptance/Notes, link
  groups, Comments.  Outline headings are `* Title' (TAB hops).

Leaving
  q uses Emacs `quit-window' (standalone) or tears down a paired
  detail pane — it does not end the Beads session.  `C-x k' also
  leaves this buffer.  Quit the session from the list with q there.

Navigation
  \\[beads-detail-next-heading] / \\[beads-detail-prev-heading]
  hop sections (TAB / S-TAB).  \\[beads-detail-toggle-fold] (z)
  folds.  \\[beads-detail-next-sibling] / \\[beads-detail-prev-sibling]
  siblings (n/p).  RET / M-RET follow an issue id or link.

Keys track `beads-keymap-style'.  Use C-h m or ?
(`describe-mode') for this help and the live map below.

Status glyphs match the list (`beads-status-legend' / `beads-faces').

\\{beads-detail-mode-map}"
  (setq buffer-read-only t
        truncate-lines nil
        header-line-format nil
        beads--id-at-point-fn #'beads-detail--id-at-point)
  ;; Classic "* Title" outline; optional ▸ fold hint + item count.
  (setq-local outline-regexp (beads-detail-outline-regexp)
              outline-level (lambda () 1))
  ;; Flat headings: set before outline-minor-mode so it never paints TTY bg.
  (setq-local outline-minor-mode-highlight nil)
  (when (boundp 'outline-minor-mode-use-buttons)
    (setq-local outline-minor-mode-use-buttons nil))
  (outline-minor-mode 1)
  ;; Soft wrap like mu4e/gnus article buffers (no fringe \\ continuations).
  (visual-line-mode 1)
  (setq-local word-wrap t
              scroll-margin 2
              scroll-conservatively 101
              imenu-create-index-function #'beads-detail--imenu-index)
  (when (boundp 'imenu-auto-rescan)
    (setq-local imenu-auto-rescan t)))

(defun beads-detail--imenu-index ()
  "Imenu index of outline section headings in the detail buffer."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (beads-detail-outline-regexp) nil t)
        (let ((key (beads-detail--section-key
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
          (push (cons key (copy-marker (line-beginning-position)))
                entries)))
      (nreverse entries))))

(defun beads-detail--prepare-buffer ()
  "Return the detail buffer in `beads-detail-mode'."
  (let ((buf (get-buffer-create beads-detail-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-detail-mode)
        (beads-detail-mode))
      (setq default-directory (beads-cli--workdir)))
    buf))

(defun beads-detail-display (id &optional force select stub)
  "Display issue ID in the detail window (Gnus summary→article).
Skip reload when ID is already shown unless FORCE.
Keep list focus unless SELECT is non-nil.
STUB, when an issue alist from the list, paints the page immediately
while `bd show' loads."
  (let* ((buf (beads-detail--prepare-buffer))
         (same (with-current-buffer buf
                 (and (not force)
                      (equal beads-detail--id id)
                      beads-detail--issue
                      (not beads-detail--loading)))))
    (unless same
      (with-current-buffer buf
        (setq beads-detail--id id
              beads-detail--issue stub
              beads-detail--link-rows-override nil
              beads-detail--collapsed-sections nil
              beads-detail--expanded-empty-sections nil
              beads-detail--comments-fetched-id nil
              beads-detail--loading t)
        (if stub
            (beads-detail--render)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize (format "Loading %s…\n" id) 'face 'shadow))))
        (force-mode-line-update)
        (beads-detail-refresh)))
    (beads-layout-display-detail buf select)
    buf))

;;;###autoload
(defun beads-detail-show (id)
  "Show detail for issue ID and select the detail window."
  (interactive "sIssue id: ")
  (beads-detail-display id t t))

(provide 'beads-detail)
;;; beads-detail.el ends here
