;;; beads-detail-render.el --- Detail buffer rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Single-page detail rendering: meta, prose sections, links, comments.
;; Outline headings use classic "* Title" so TAB/S-TAB hop works.
;; Major mode and interactive commands live in beads-detail.el.

;;; Code:

(require 'beads-cli)
(require 'beads-issue)
(require 'beads-coltree)
(require 'beads-rel)
(require 'beads-faces)
(require 'beads-md)
(require 'outline)
(require 'cl-lib)

(declare-function beads-detail-show "beads-detail" (id))
(declare-function beads-detail-display "beads-detail" (id &optional force select stub))
(declare-function beads-list-merge-from-show "beads-list-follow" (id full))
(defvar beads-list-buffer-name)

(defcustom beads-detail-markdown t
  "When non-nil, fontify Details bodies via `beads-md-string'."
  :type 'boolean
  :group 'beads)

(declare-function beads-status-glyph "beads-faces" (status))
(declare-function beads-md-buttonize-ids "beads-md" (beg end &optional action))
(declare-function beads-md-buttonize-urls "beads-md" (beg end))

;; Buffer name lives in beads-layout.el (shared with window config).

(defvar-local beads-detail--id nil)
(defvar-local beads-detail--issue nil)
(defvar-local beads-detail--loading nil)
(defvar-local beads-detail--comments-fetched-id nil
  "Issue id whose comments were loaded via `bd comments', or nil.")

(defconst beads-detail--heading-names
  (concat "Description\\|Design\\|Acceptance\\|Notes\\|"
          "Parent\\|Depends on\\|Children\\|Blocks\\|Related\\|Comments")
  "Alternation of outline heading titles (link groups match `bd show').")

(defun beads-detail-outline-regexp ()
  "Regexp for detail outline headings.
Classic outline `* Title' (optional ▸ fold hint, optional item count).
The leading star keeps body markdown lines like bare Acceptance off the map."
  (concat "^\\(?:▸\\s-+\\)?\\*\\s-+"
          "\\(?:" beads-detail--heading-names "\\)"
          "\\(?:\\s-+([0-9]+)\\)?\\s-*$"))

(defun beads-detail--insert-outline-heading (title &optional count)
  "Insert outline heading TITLE; optional item COUNT like Children (3)."
  (insert (propertize "* " 'face 'shadow 'beads-outline-heading t))
  (insert (propertize title 'face 'beads-face-section
                      'beads-outline-heading t))
  (when (and (integerp count) (> count 0))
    (insert (propertize (format " (%d)" count) 'face 'shadow)))
  (insert "\n"))

(defcustom beads-detail-body-indent 2
  "Spaces of indent for content under outline headings.
Applies to Description/Design/Acceptance/Notes bodies, link rows,
and comments so the page shares one left margin."
  :type 'integer
  :group 'beads)

(defcustom beads-detail-hide-empty-sections t
  "When non-nil, skip Detail sections whose body is empty."
  :type 'boolean
  :group 'beads)

(defcustom beads-detail-max-link-rows 40
  "Max rows to show per link group before truncating with a more marker."
  :type 'integer
  :group 'beads)

(defun beads-detail--pad ()
  "Return the shared body indent string."
  (make-string (max 0 beads-detail-body-indent) ?\s))

(defun beads-detail--indent-text (text)
  "Prefix every line of TEXT with `beads-detail-body-indent' spaces."
  (let ((pad (beads-detail--pad))
        (text (or text "")))
    (if (string-empty-p text)
        text
      (mapconcat (lambda (line) (concat pad line))
                 (split-string text "\n")
                 "\n"))))

(defun beads-detail--markdownify (text)
  "Return TEXT with markdown faces when `beads-detail-markdown' is set.
Uses the builtin renderer (not markdown-mode) so hide-markup unwraps
markers consistently across Description, Design, Notes, and Comments."
  (if beads-detail-markdown
      ;; Prefer builtin path: markdown-mode often leaves ** visible.
      (let ((beads-md-prefer-markdown-mode nil)
            (beads-md-hide-markup t))
        (beads-md-string text))
    (if (and text (not (string-empty-p text))) text "(none)")))

(defun beads-detail--apply-wrap-prefix (beg end &optional pad)
  "Set `wrap-prefix' on BEG..END so visual-line soft wraps keep PAD.
Without this, continuation lines flush left and break the margin."
  (let ((pad (or pad (beads-detail--pad))))
    (when (and (> end beg) (not (string-empty-p pad)))
      (add-text-properties beg end (list 'wrap-prefix pad)))))

(defun beads-detail--insert-padded-body (text &optional checkboxes)
  "Insert markdownified TEXT with shared body indent.
When CHECKBOXES, make ○/✓ glyphs locally toggleable.
Soft wraps keep the indent via `wrap-prefix' (visual-line-mode)."
  (let* ((rendered (beads-detail--markdownify text))
         (pad (beads-detail--pad))
         (padded (beads-detail--indent-text rendered))
         (start (point)))
    (insert padded "\n")
    (beads-detail--clamp-table-lines start (point))
    (beads-md-buttonize-ids start (point) #'beads-detail--open-id)
    (beads-md-buttonize-urls start (point))
    (when checkboxes
      (beads-detail--buttonize-checkboxes start (point)))
    (add-text-properties start (point) '(outline-level 2))
    (beads-detail--apply-wrap-prefix start (point) pad)))

(defvar-local beads-detail--link-rows-override nil
  "Buffer-local override for `beads-detail-max-link-rows' (nil = use custom).")

(defvar-local beads-detail--collapsed-sections nil
  "Section keys currently folded (persist across re-renders).")

(defvar-local beads-detail--expanded-empty-sections nil
  "Empty Links groups the user explicitly unfolded (skip auto-collapse).")

(defun beads-detail--section-key (line)
  "Canonical key for outline heading LINE (strip ▸ / * / counts)."
  (setq line (replace-regexp-in-string "\\`▸\\s-+" "" line))
  (setq line (replace-regexp-in-string "\\`\\*\\s-+" "" line))
  (cond
   ((string-match-p "\\`Depends on" line) "Depends on")
   ((string-match-p "\\`Parent" line) "Parent")
   ((string-match-p "\\`Children" line) "Children")
   ((string-match-p "\\`Blocks" line) "Blocks")
   ((string-match-p "\\`Related" line) "Related")
   ((string-match-p "\\`Comments" line) "Comments")
   ((string-match-p "\\`Description" line) "Description")
   ((string-match-p "\\`Design" line) "Design")
   ((string-match-p "\\`Acceptance" line) "Acceptance")
   ((string-match-p "\\`Notes" line) "Notes")
   (t (string-trim line))))

(defun beads-detail--heading-collapsed-p ()
  "Non-nil when point is on a heading whose body is hidden."
  (let ((end (line-end-position)))
    (and (< end (point-max))
         (eq (get-char-property (1+ end) 'invisible) 'outline))))

(defun beads-detail--capture-folds ()
  "Remember which outline sections are collapsed.
Update keys present in the buffer; keep keys for headings not shown
(e.g. Links folds while Details is displayed)."
  (when (bound-and-true-p outline-minor-mode)
    (let (present keys)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at outline-regexp)
            (let ((key (beads-detail--section-key
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
              (push key present)
              (when (beads-detail--heading-collapsed-p)
                (push key keys))))
          (forward-line 1)))
      (let ((keep (cl-remove-if
                   (lambda (k) (member k present))
                   beads-detail--collapsed-sections)))
        (setq beads-detail--collapsed-sections
              (delete-dups (append keys keep)))))))

(defun beads-detail--restore-folds ()
  "Re-collapse sections listed in `beads-detail--collapsed-sections'.
Prefix collapsed headings with a dim ▸ fold hint."
  (when (bound-and-true-p outline-minor-mode)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at outline-regexp)
            (let ((key (beads-detail--section-key
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
              (when (member key beads-detail--collapsed-sections)
                (outline-hide-subtree)
                (beginning-of-line)
                (unless (looking-at "▸")
                  (insert (propertize "▸ " 'face 'beads-face-fold
                                      'beads-fold-hint t))))))
          (forward-line 1))))))

(defun beads-detail--seed-empty-link-folds ()
  "Mark empty link groups collapsed unless user expanded them.
Skip headings that already show a positive item count like `Children (12)'."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at outline-regexp)
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (key (beads-detail--section-key line)))
          (when (member key '("Parent" "Depends on" "Children" "Blocks" "Related"))
            (unless (string-match-p "(\\([1-9][0-9]*\\))" line)
              (save-excursion
                (forward-line 1)
                (when (and (looking-at-p
                            (concat (regexp-quote (beads-detail--pad))
                                    "(none)\\s-*$"))
                           (not (member key beads-detail--expanded-empty-sections)))
                  (cl-pushnew key beads-detail--collapsed-sections
                              :test #'equal)))))))
      (forward-line 1))))

(defun beads-detail--open-id (id)
  "Open issue ID in this detail pane."
  (beads-detail-show id))

(defun beads-detail--expand-more-links (_button)
  "Raise link-row cap and re-render links in place."
  (setq beads-detail--link-rows-override
        (max 10000 (1+ (or beads-detail--link-rows-override
                           beads-detail-max-link-rows
                           40))))
  (if beads-detail--issue
      (beads-detail--render)
    (when beads-detail--id
      (beads-detail-display beads-detail--id t nil)))
  (message "showing up to %d link rows" beads-detail--link-rows-override))

(defun beads-detail--toggle-checkbox-at (pos)
  "Locally flip ○/✓ checkbox glyph at POS (visual only)."
  (let ((inhibit-read-only t)
        (ch (char-after pos)))
    (cond
     ((eq ch ?○)
      (goto-char pos)
      (delete-char 1)
      (insert (propertize "✓" 'face 'beads-face-status-closed)))
     ((eq ch ?✓)
      (goto-char pos)
      (delete-char 1)
      (insert (propertize "○" 'face 'beads-md-checkbox))))
    (when-let* ((lb (get-buffer beads-list-buffer-name)))
      (with-current-buffer lb
        (force-mode-line-update t)))))

(defun beads-detail--buttonize-checkboxes (beg end)
  "Make ○/✓ glyphs in BEG..END clickable for local toggle."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[○✓]" end t)
      (let ((p (1- (point))))
        (unless (button-at p)
          (make-text-button
           p (1+ p)
           'action (lambda (_) (beads-detail--toggle-checkbox-at p))
           'follow-link t
           'help-echo "Toggle checkbox (local only)"
           'face (get-text-property p 'face)))))))

(defun beads-detail--clamp-table-lines (beg end)
  "Truncate markdown table rows in BEG..END to the window width."
  (let* ((win (get-buffer-window (current-buffer) t))
         (w (max 24 (if win (- (window-width win) 2) 72)))
         (inhibit-read-only t))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when (looking-at-p "^\\s-*|.+|\\s-*$")
          (let* ((line (buffer-substring (point) (line-end-position)))
                 (plain (substring-no-properties line)))
            (when (> (string-width plain) w)
              (delete-region (point) (line-end-position))
              (insert (truncate-string-to-width line w nil nil "…")))))
        (forward-line 1)))))

(defun beads-detail--insert-section (title body &optional checkboxes)
  "Insert outline SECTION titled TITLE with padded BODY text.
When CHECKBOXES, make ○/✓ glyphs locally toggleable."
  (when (or (not beads-detail-hide-empty-sections)
            (and body (not (string-empty-p (string-trim body)))))
    (beads-detail--insert-outline-heading title)
    (beads-detail--insert-padded-body body checkboxes)
    (insert "\n")))

(defun beads-detail--format-estimate (minutes)
  "Humanize MINUTES estimate."
  (cond
   ((null minutes) nil)
   ((< minutes 60) (format "%dm" minutes))
   ((zerop (% minutes 60)) (format "%dh" (/ minutes 60)))
   (t (format "%dh %dm" (/ minutes 60) (% minutes 60)))))

(defun beads-detail--format-day (raw)
  "Calendar day for ISO RAW, or RAW if unparsed."
  (let ((parsed (beads-detail--parse-time raw)))
    (if parsed
        (format-time-string "%Y-%m-%d" parsed)
      (let ((s (string-trim (or raw ""))))
        (if (string-empty-p s) nil s)))))

(defun beads-detail--day-key (raw)
  "Return YYYY-MM-DD for RAW, or nil."
  (when-let* ((day (beads-detail--format-day raw)))
    (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" day)
         (substring day 0 10))))

(defun beads-detail--due-face (raw)
  "Face for due date RAW: error if overdue, warning if within 7 days."
  (when-let* ((due (beads-detail--parse-time raw)))
    (let* ((now (current-time))
           (secs (float-time (time-subtract due now)))
           (days (/ secs 86400.0)))
      (cond
       ((< days 0) 'error)
       ((<= days 7) 'warning)
       (t nil)))))

(defun beads-detail--meta-people (issue)
  "Distinct people for ISSUE chrome: assignee, then author, then owner."
  (let (seen out)
    (dolist (name (list (beads-issue-assignee issue)
                        (beads-issue-created-by issue)
                        (beads-issue-owner issue)))
      (let ((n (string-trim (or name ""))))
        (when (and (not (string-empty-p n))
                   (not (member n seen)))
          (push n seen)
          (push n out))))
    (nreverse out)))

(defun beads-detail--insert-meta-attr (text &optional face)
  "Insert one indented unlabeled attr line TEXT.
Optional FACE applies to the whole line body."
  (when (and text (not (string-empty-p (string-trim text))))
    (let* ((pad (beads-detail--pad))
           (beg (point)))
      (insert pad)
      (let ((body-beg (point)))
        (insert (if face (propertize text 'face face) text) "\n")
        (beads-detail--apply-wrap-prefix beg (point) pad)
        (beads-md-buttonize-ids body-beg (1- (point)) #'beads-detail--open-id)
        (beads-md-buttonize-urls body-beg (1- (point)))))))

(defun beads-detail--render-meta (issue)
  "Insert ISSUE chrome: breadcrumb, title, state, then quiet attrs.
MOCK 10 layout — color only on status / hot priority / type / due alerts."
  (let* ((id (beads-issue-id issue))
         (parent (string-trim (or (beads-issue-parent issue) "")))
         (st (beads-issue-status issue))
         (pri (beads-issue-priority issue))
         (typ (beads-issue-type issue))
         (st-face (beads-face-for-status st))
         (crumb-beg (point)))
    ;; Hero: parent › id
    (when (not (string-empty-p parent))
      (insert (propertize parent 'face 'shadow)
              (propertize " › " 'face 'shadow)))
    (insert (propertize id 'face 'beads-face-id) "\n")
    (beads-md-buttonize-ids crumb-beg (1- (point)) #'beads-detail--open-id)
    ;; Title (weight, not hue)
    (insert (propertize (or (beads-issue-title issue) "")
                        'face (if (beads-issue-closed-p issue)
                                  'beads-face-title-closed
                                'beads-face-title))
            "\n")
    ;; State cluster
    (insert (propertize (format "%s %s" (beads-status-glyph st) st)
                        'face st-face)
            (propertize " · " 'face 'shadow)
            (propertize (format "P%d" pri)
                        'face (beads-face-for-priority pri))
            (propertize " · " 'face 'shadow)
            (propertize (or typ "")
                        'face (beads-face-for-type typ))
            "\n\n")
    ;; Attrs (unlabeled strips)
    (when-let* ((people (beads-detail--meta-people issue)))
      (beads-detail--insert-meta-attr (mapconcat #'identity people " · ")))
    (when-let* ((labels (beads-issue-labels issue)))
      (beads-detail--insert-meta-attr
       (mapconcat #'identity labels " · ")
       'shadow))
    ;; Planning: due (alert face) · estimate · created · updated?
    (let* ((pad (beads-detail--pad))
           (due-raw (beads-issue-due-at issue))
           (due-day (beads-detail--format-day due-raw))
           (due-face (beads-detail--due-face due-raw))
           (est (beads-detail--format-estimate
                 (beads-issue-estimated-minutes issue)))
           (created (beads-detail--format-day (beads-issue-created-at issue)))
           (updated (beads-detail--format-day (beads-issue-updated-at issue)))
           (created-key (beads-detail--day-key (beads-issue-created-at issue)))
           (updated-key (beads-detail--day-key (beads-issue-updated-at issue)))
           (sep (propertize " · " 'face 'shadow))
           (bits nil))
      (when (and updated-key created-key
                 (not (string= updated-key created-key)))
        (push (propertize (format "updated %s" updated) 'face 'shadow) bits))
      (when created
        (push (propertize (format "created %s" created) 'face 'shadow) bits))
      (when est
        (push (propertize est 'face 'shadow) bits))
      (when due-day
        (push (propertize (format "due %s" due-day)
                          'face (or due-face 'shadow))
              bits))
      (when bits
        (let ((beg (point)))
          (insert pad)
          (let ((first t))
            (dolist (bit bits)
              (unless first (insert sep))
              (insert bit)
              (setq first nil)))
          (insert "\n")
          (beads-detail--apply-wrap-prefix beg (point) pad))))
    (let* ((ext (string-trim (or (beads-issue-external-ref issue) "")))
           (spec (string-trim (or (beads-issue-spec-id issue) "")))
           (refs (delq nil (list (and (not (string-empty-p ext)) ext)
                                 (and (not (string-empty-p spec)) spec)))))
      (when refs
        (beads-detail--insert-meta-attr
         (mapconcat #'identity refs " · ")
         'shadow)))
    ;; Blank line before outline `* Description'.
    (insert "\n")))

(defun beads-detail--render-sections (issue)
  "Insert outline sections, links, and comments for ISSUE (no meta)."
  (beads-detail--insert-section "Description"
                                (beads-issue-description issue))
  (beads-detail--insert-section "Design" (beads-issue-design issue))
  (beads-detail--insert-section "Acceptance"
                                (beads-issue-acceptance issue)
                                t)
  (beads-detail--insert-section "Notes" (beads-issue-notes issue))
  (beads-detail--render-links issue)
  (beads-detail--render-comments issue))

(defun beads-detail--render-body (issue)
  "Render full single-page body for ISSUE (meta + sections)."
  (beads-detail--render-meta issue)
  (beads-detail--render-sections issue))

(defun beads-detail--open-link-button (button)
  "Follow BUTTON to its `beads-id'."
  (beads-detail--open-id (button-get button 'beads-id)))

(define-button-type 'beads-detail-issue-link
  'action #'beads-detail--open-link-button
  'follow-link t
  'face 'beads-md-link
  'mouse-face 'highlight
  'help-echo "Open issue (mouse-1 / RET)")

(defun beads-detail--render-link-group (title rels direction)
  "Insert TITLE then RELS with DIRECTION annotation.
Each issue id is a `beads-detail-issue-link' button (mouse-1 / RET).
Rows use `beads-detail-body-indent'."
  (beads-detail--insert-outline-heading title (length rels))
  (let ((pad (beads-detail--pad)))
    (if (null rels)
        (insert (propertize (concat pad "(none)\n") 'face 'shadow))
      (let* ((maxn (or beads-detail--link-rows-override
                       beads-detail-max-link-rows
                       40))
             (shown (if (> (length rels) maxn)
                        (cl-subseq rels 0 maxn)
                      rels))
             (hidden (max 0 (- (length rels) (length shown)))))
        (dolist (r shown)
          (let* ((rid (beads-issue-rel-id r))
                 (typ (beads-issue-rel-type r))
                 (ttl (beads-issue-rel-title r))
                 (row0 (point))
                 (arrow (pcase direction
                          ("depends" "→")
                          ("blocks" "←")
                          ("related" "↔")
                          ("parent" "↑")
                          ("child" "↳")
                          (_ "·"))))
            (insert pad
                    (propertize arrow 'face 'beads-face-fold)
                    " "
                    (propertize (beads-coltree-pad typ 12) 'face 'beads-face-type)
                    " ")
            (let ((id0 (point)))
              (insert rid)
              (make-text-button id0 (point)
                                'type 'beads-detail-issue-link
                                'beads-id rid
                                'help-echo (format "Open %s (mouse-1 / RET)" rid)))
            (unless (string-empty-p ttl)
              (insert "  ")
              (let ((t0 (point)))
                (insert ttl)
                (make-text-button t0 (point)
                                  'type 'beads-detail-issue-link
                                  'face 'beads-face-title
                                  'beads-id rid
                                  'help-echo (format "Open %s (mouse-1 / RET)" rid))))
            (insert "\n")
            (add-text-properties row0 (1- (point))
                                 (list 'beads-id rid 'beads-link t))
            (beads-detail--apply-wrap-prefix row0 (1- (point)) pad)))
        (when (> hidden 0)
          (let ((m0 (point)))
            (insert (format "%s… +%d more\n" pad hidden))
            (make-text-button m0 (1- (point))
                              'action #'beads-detail--expand-more-links
                              'follow-link t
                              'face 'shadow
                              'mouse-face 'highlight
                              'help-echo "Show more links (mouse-1 / RET)")
            (beads-detail--apply-wrap-prefix m0 (1- (point)) pad))))))
  (insert "\n"))

(defun beads-detail--render-links (issue)
  "Render link groups for ISSUE in `bd show' order.
Parent, Depends on, Children, Blocks, Related — omit empty groups."
  (let ((parts (beads-rel-partition-for-show issue)))
    (dolist (spec '(("Parent" parent "parent")
                    ("Depends on" depends-on "depends")
                    ("Children" children "child")
                    ("Blocks" blocks "blocks")
                    ("Related" related "related")))
      (let* ((title (nth 0 spec))
             (key (nth 1 spec))
             (dir (nth 2 spec))
             (rels (alist-get key parts)))
        (when rels
          (beads-detail--render-link-group title rels dir))))))

(defun beads-detail--parse-time (raw)
  "Parse ISO-ish timestamp RAW to Emacs time, or nil."
  (when (and raw (not (string-empty-p raw)))
    (or (ignore-errors
          (when (fboundp 'iso8601-parse)
            (encode-time (iso8601-parse raw))))
        (ignore-errors
          (encode-time
           (parse-time-string
            (replace-regexp-in-string
             "Z\\'" "" (replace-regexp-in-string "T" " " raw))))))))

(defun beads-detail--comment-time (raw)
  "Compact local time string for comment timestamp RAW."
  (let ((parsed (beads-detail--parse-time raw)))
    (if (not parsed)
        (or raw "?")
      (format-time-string "%Y-%m-%d %H:%M" parsed))))

(defun beads-detail--merge-comments (comments)
  "Install COMMENTS list into `beads-detail--issue' and redraw."
  (when beads-detail--issue
    (let* ((n (length comments))
           (iss (cons (cons 'comments comments)
                      (assq-delete-all
                       'comments
                       (assq-delete-all
                        'comment_count
                        (copy-sequence beads-detail--issue))))))
      (setq iss (cons (cons 'comment_count n) iss)
            beads-detail--issue iss)
      (beads-detail--render))))

(defun beads-detail--fetch-comments (&optional force)
  "Load comments for the current issue via `bd comments'.
Skip when already fetched for this id unless FORCE.
Also skip when `bd show` already embedded a non-empty comments list."
  (let* ((id beads-detail--id)
         (iss beads-detail--issue)
         (buf (current-buffer))
         (embedded (and iss (beads-issue-comments iss))))
    (when (and id iss
               (or force
                   (and (not (equal beads-detail--comments-fetched-id id))
                        (null embedded))))
      (beads-cli-call-async
       (lambda (ok data)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (equal beads-detail--id id)
               (setq beads-detail--comments-fetched-id id)
               (if ok
                   (beads-detail--merge-comments
                    (if (listp data) data '()))
                 (message "beads comments: %s" data))))))
       "comments" id))))

(defun beads-detail--render-comments (issue)
  "Render Comments outline section for ISSUE.
Gnus/mu4e style: author · time header, markdown body, blank line
between comments — no box-drawing rules."
  (let* ((comments (beads-issue-comments issue))
         (count (or (beads-issue-comment-count issue)
                    (length comments)
                    0))
         (loading (and (null comments)
                       (> count 0)
                       (not (equal beads-detail--comments-fetched-id
                                   (beads-issue-id issue)))))
         (pad (beads-detail--pad))
         (n 0)
         (total (length comments)))
    (beads-detail--insert-outline-heading "Comments" (and (> count 0) count))
    (cond
     (loading
      (insert (propertize (concat pad "Loading comments…\n\n") 'face 'shadow))
      (beads-detail--fetch-comments))
     ((null comments)
      (insert (propertize (concat pad "(none)\n") 'face 'shadow))
      (insert (propertize (concat pad "Press c to add a comment.\n\n")
                          'face 'shadow)))
     (t
      (dolist (c comments)
        (setq n (1+ n))
        (let ((author (or (alist-get 'author c) "?"))
              (at (beads-detail--comment-time
                   (or (alist-get 'created_at c) "")))
              (text (or (alist-get 'text c) "")))
          ;; From-line style (Gnus article / mu4e headers).
          (insert (propertize (concat pad author) 'face 'beads-face-id)
                  (propertize (format "  %s" at) 'face 'shadow)
                  "\n")
          (beads-detail--insert-padded-body text)
          (unless (= n total)
            (insert "\n"))))
      (insert "\n")))))

(defun beads-detail--goto-heading (title)
  "Move point to outline heading TITLE when present."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^\\(?:▸\\s-+\\)?\\*\\s-+" (regexp-quote title) "\\b")
         nil t)
    (beginning-of-line)
    t))

(defun beads-detail--render ()
  "Redraw detail buffer as a single scrollable page."
  (let ((inhibit-read-only t)
        (issue beads-detail--issue))
    (beads-detail--capture-folds)
    (erase-buffer)
    (setq header-line-format nil)
    (force-mode-line-update t)
    (if (null issue)
        (insert (if beads-detail--loading "Loading…" "(no issue)"))
      (progn
        (beads-detail--render-body issue)
        (goto-char (point-min))
        (beads-detail--seed-empty-link-folds)
        (beads-detail--restore-folds)
        (beads-detail--goto-heading "Description")))))

(defun beads-detail--apply (issue)
  "Install ISSUE and redraw.
Pushes show fields into the list stub via `beads-list-merge-from-show'
so list/detail share one `bd show' owner (detail)."
  (let ((prev beads-detail--id)
        (new (beads-issue-id issue)))
    (when (and prev (not (equal prev new)))
      (setq beads-detail--link-rows-override nil
            beads-detail--collapsed-sections nil
            beads-detail--expanded-empty-sections nil)))
  (setq beads-detail--issue issue
        beads-detail--id (beads-issue-id issue)
        beads-detail--loading nil)
  ;; `bd show --include-comments` embeds bodies (even if empty) — treat as fetched.
  (when (assq 'comments issue)
    (setq beads-detail--comments-fetched-id (beads-issue-id issue)))
  (beads-list-merge-from-show (beads-issue-id issue) issue)
  ;; No pulse on apply — follow redraws often and yellow overlays flash/stick.
  (beads-detail--render))


(provide 'beads-detail-render)
;;; beads-detail-render.el ends here
