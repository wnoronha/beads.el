;;; beads-coltree.el --- Columnar tree-list chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared indent + fixed-column row formatting for Beads list-style UIs
;; (main list, ready/blocked/human queues, and similar future buffers).
;;
;; When to use which tree UI
;; -------------------------
;; * beads-coltree (this module) — Hierarchical rows that keep fixed
;;   columns (id / priority / type / title) aligned across depths.
;;   Hand-rolled box-drawing chrome plus fold glyphs (`beads-tree-guide-style`).
;;   Preferred for every Beads “issue table” view.
;; * tree-widget — Built-in hierarchy widget. Fine for browsers without
;;   column alignment; do not use for Beads list/queue UIs (columns drop).
;; * outline-mode — Section folds inside a buffer (detail Details/Links
;;   headings). Not a multi-column issue list.
;;
;; This path does not require `tree-widget` or any ELPA hierarchy package.

;;; Code:

(require 'beads-faces)
(require 'beads-issue)

(defcustom beads-tree-guide-style 'mid
  "Branch chrome width for list tree guides.
`short' — 2 cols/level (`├ `/`└ `/`│ `; fold merge `├▾')
`mid'   — 4 cols/level (`├── `/`└── `/`│   `; merge `├─▾ ')
`long'  — 6 cols/level (`├──── `/`└──── `/`│     `; merge `├───▾ ')
Not Emacs `tree-widget`; our columnar text tree. Reloading the list
(`g') applies a changed value."
  :type '(choice (const :tag "Short (2 cols)" short)
                 (const :tag "Mid (4 cols)" mid)
                 (const :tag "Long (6 cols)" long))
  :group 'beads)

(defcustom beads-issue-title-max nil
  "Max display width for titles, or nil to fill the window.
Defined with coltree (list column layout); name kept for existing configs."
  :type '(choice (const :tag "Fill window" nil) integer)
  :group 'beads)

(defun beads-coltree-pad (string width)
  "Pad or truncate STRING to WIDTH display columns."
  (truncate-string-to-width (or string "") width nil ?\s t))

(defun beads-coltree-propertize (string face)
  "Return STRING with FACE applied."
  (propertize string 'face face))

(defun beads-coltree--guide-parts (style)
  "Return (STEM ROOT-OPEN ROOT-CLOSED ROOT-EMPTY TIP TIP-LAST
PARENT PARENT-LAST PARENT-COLLAPSED PARENT-LAST-COLLAPSED) for STYLE."
  (pcase (or style 'mid)
    ('short
     '("│ " "▾ " "▸ " "  " "├ " "└ " "├▾" "└▾" "├▸" "└▸"))
    ('long
     '("│     " "▾     " "▸     " "      "
       "├──── " "└──── " "├───▾ " "└───▾ " "├───▸ " "└───▸ "))
    (_
     '("│   " "▾   " "▸   " "    "
       "├── " "└── " "├─▾ " "└─▾ " "├─▸ " "└─▸ "))))

(defun beads-coltree-indent (depth &optional last-p collapsed-p has-kids-p)
  "Return leading tree/fold chrome for DEPTH.
Width per level follows `beads-tree-guide-style'. Depth 0 is fold only;
depth>0 uses stems plus a tip. When HAS-KIDS-P the tip merges fold so
status/id stay column-aligned with leaves at the same depth."
  (let* ((face 'beads-face-fold)
         (parts (beads-coltree--guide-parts beads-tree-guide-style))
         (stem-u (nth 0 parts))
         (root-open (nth 1 parts))
         (root-closed (nth 2 parts))
         (root-empty (nth 3 parts))
         (tip (nth 4 parts))
         (tip-last (nth 5 parts))
         (par (nth 6 parts))
         (par-last (nth 7 parts))
         (par-c (nth 8 parts))
         (par-last-c (nth 9 parts)))
    (cond
     ((or (null depth) (<= depth 0))
      (cond
       ((not has-kids-p) root-empty)
       (collapsed-p (propertize root-closed 'face face))
       (t (propertize root-open 'face face))))
     (t
      (let* ((stem (when (> depth 1)
                     (propertize (apply #'concat (make-list (1- depth) stem-u))
                                 'face face)))
             (node (cond
                    ((not has-kids-p) (if last-p tip-last tip))
                    (collapsed-p (if last-p par-last-c par-c))
                    (t (if last-p par-last par)))))
        (concat stem (propertize node 'face face)))))))

(defun beads-coltree-cell (string width &optional face)
  "Fixed-width cell: pad STRING to WIDTH; apply FACE when non-nil."
  (let ((padded (beads-coltree-pad string width)))
    (if face (beads-coltree-propertize padded face) padded)))

(defun beads-coltree--normalize-cell (cell)
  "Turn CELL into a display string.
CELL is a string, or a list (STRING WIDTH &optional FACE)."
  (cond
   ((stringp cell) cell)
   ((and (consp cell) (stringp (car cell)))
    (beads-coltree-cell (nth 0 cell) (nth 1 cell) (nth 2 cell)))
   (t (error "Invalid coltree cell: %S" cell))))

(defun beads-coltree-format-row (indent cells &optional trailing)
  "Build one columnar tree row.
INDENT is tree/fold chrome from `beads-coltree-indent'.
CELLS is a list of strings or (STRING WIDTH &optional FACE) specs.
TRAILING is an optional suffix string (comments, assignee, …)."
  (let* ((parts (mapcar #'beads-coltree--normalize-cell cells))
         (body (mapconcat #'identity parts " ")))
    (concat (or indent "") body (or trailing ""))))

(defun beads-coltree-format-issue (issue &optional depth collapsed-p has-kids-p last-p)
  "One-line display for ISSUE at DEPTH with faces.
Layout: tree/fold (`beads-tree-guide-style' cols × depth) · status · id ·
P# · type · title · comments · @handle. See `beads-coltree-indent'.
No relative-age column."
  (let* ((indent (beads-coltree-indent depth last-p collapsed-p has-kids-p))
         (pri-n (beads-issue-priority issue))
         (st (beads-issue-status issue))
         (typ (beads-issue-type issue))
         (closed (beads-issue-closed-p issue))
         (glyph (beads-coltree-propertize
                 (beads-status-glyph st)
                 (beads-face-for-status st)))
         (glyph (if (beads-issue-pinned-p issue)
                    (beads-coltree-propertize
                     (beads-status-glyph "pinned")
                     (beads-face-for-status "pinned"))
                  glyph))
         (cc (beads-issue-comment-count issue))
         (comments (when (and (numberp cc) (> cc 0))
                     (beads-coltree-propertize
                      (format " ·%d" cc) 'shadow)))
         (handle (beads-issue-assignee-handle issue))
         ;; Only surface handle on active claims (avoids @me noise).
         (assignee
          (when (and handle (string= st "in_progress"))
            (beads-coltree-propertize (format " @%s" handle) 'shadow)))
         (cells
          (list glyph
                (list (beads-issue-id issue) 14 'beads-face-id)
                (list (format "P%d" pri-n) 4 (beads-face-for-priority pri-n))
                (list typ 8 (beads-face-for-type typ))))
         (prefix (beads-coltree-format-row indent cells " "))
         (suffix (concat (or comments "") (or assignee "")))
         (win-w (or (and (get-buffer-window (current-buffer) t)
                         (window-width (get-buffer-window (current-buffer) t)))
                    100))
         (title-max (or beads-issue-title-max
                        (max 12 (- win-w (string-width prefix)
                                   (string-width suffix) 1))))
         (title (beads-coltree-propertize
                 (truncate-string-to-width
                  (beads-issue-title issue) title-max nil nil "…")
                 (if closed 'beads-face-title-closed 'beads-face-title))))
    (concat prefix title suffix)))

(provide 'beads-coltree)
;;; beads-coltree.el ends here
