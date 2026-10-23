;;; beads-list-header.el --- List header-line chips and progress -*- lexical-binding: t; -*-

;;; Commentary:
;; List header-line chips and progress. Chip construction is declarative
;; via `beads-list--header-chip' / filter chip specs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'beads-issue)
(require 'beads-rel)
(require 'beads-presets)
(require 'beads-faces)

(declare-function beads-list-cycle-status-filter "beads-list-filter" ())
(declare-function beads-list-filter-type "beads-list-filter" ())
(declare-function beads-list-filter-label "beads-list-filter" ())
(declare-function beads-list-filter-priority "beads-list-filter" ())
(declare-function beads-list-cycle-sort "beads-list-render" ())
(declare-function beads-list-toggle-follow "beads-list-follow" ())
(declare-function beads-list-toggle-hide-closed-roots "beads-list-filter" ())
(declare-function beads-list-toggle-todos-only "beads-list-filter" ())
(declare-function beads-list-choose-preset "beads-list-filter" ())

(defvar beads-list--kind)
(defvar beads-list--preset)
(defvar beads-list--query)
(defvar beads-list--loading)
(defvar beads-list--status-filter)
(defvar beads-list--type-filter)
(defvar beads-list--label-filter)
(defvar beads-list--priority-max)
(defvar beads-list--narrow-id)
(defvar beads-auto-follow)
(defvar beads-list--hide-closed-roots)
(defvar beads-list--todos-only)
(defvar beads-list--rows)
(defvar beads-list--collapsed)
(defvar beads-list--issues)
(defvar beads-tree-sort)

(defun beads-list--header-chip (text help command &optional face)
  "Propertize TEXT as a clickable header chip running COMMAND.
HELP is the mouse help-echo; FACE defaults to `link'."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda () (interactive) (call-interactively command)))
    (propertize text
                'face (or face 'link)
                'mouse-face 'highlight
                'help-echo help
                'keymap map)))

(defun beads-list--count-chip ()
  "Visible-row count / live / folded chip for the header-line.
Help-echo shows `beads-status-legend' (glyph meanings without *beads-help*)."
  (let* ((n (length beads-list--rows))
         (live (cl-count-if
                (lambda (row)
                  (member (beads-issue-status (car row))
                          '("open" "in_progress" "blocked")))
                beads-list--rows))
         (fc (and beads-list--collapsed
                  (hash-table-count beads-list--collapsed)))
         (fold (if (and fc (> fc 0)) (format " · %d folded" fc) ""))
         (text (format " · %d (%d live)%s" n live fold)))
    (propertize text 'face 'shadow 'help-echo beads-status-legend)))

(defun beads-list--filter-chips ()
  "Build status/type/label/priority header chips from active filters."
  (mapconcat
   #'identity
   (delq nil
         (list
          (when beads-list--status-filter
            (beads-list--header-chip
             (format " · status:%s" beads-list--status-filter)
             "mouse-1: cycle status filter"
             #'beads-list-cycle-status-filter))
          (when beads-list--type-filter
            (beads-list--header-chip
             (format " · type:%s" beads-list--type-filter)
             "mouse-1: change type filter"
             #'beads-list-filter-type))
          (when beads-list--label-filter
            (beads-list--header-chip
             (format " · label:%s" beads-list--label-filter)
             "mouse-1: change label filter"
             #'beads-list-filter-label))
          (when (numberp beads-list--priority-max)
            (beads-list--header-chip
             (format " · P<=%d" beads-list--priority-max)
             "mouse-1: change priority filter"
             #'beads-list-filter-priority))))
   ""))

(defun beads-list--header ()
  "Header-line text: view context + counts (sole custom list chrome).
Emphasize when this window is selected (Magit-style focus cue).
Mode-line stays stock Emacs."
  (let* ((kind (symbol-name beads-list--kind))
         (preset (when (eq beads-list--kind 'list)
                   (beads-preset-name beads-list--preset)))
         (q beads-list--query)
         (load (if beads-list--loading " …" ""))
         (focused (eq (selected-window) (get-buffer-window (current-buffer))))
         (mark (if focused "▸" " "))
         (type-chip (beads-list--filter-chips))
         (sort-chip
          (beads-list--header-chip
           (format " · sort:%s" beads-tree-sort)
           "mouse-1: cycle sort"
           #'beads-list-cycle-sort))
         (narrow-chip (when beads-list--narrow-id
                        (format " · narrow:%s" beads-list--narrow-id)))
         (follow-chip
          (if beads-auto-follow
              ""
            (beads-list--header-chip
             " · follow:off"
             "mouse-1: enable auto-follow"
             #'beads-list-toggle-follow)))
         (hide-chip
          (if beads-list--hide-closed-roots
              (beads-list--header-chip
               " · hide:closed"
               "mouse-1: show closed roots"
               #'beads-list-toggle-hide-closed-roots)
            ""))
         (todo-chip
          (concat
           (when beads-list--todos-only
             (beads-list--header-chip
              " · todos"
              "mouse-1: clear todos filter"
              #'beads-list-toggle-todos-only))
           (beads-list--selected-context-chip)))
         (preset-chip
          (when preset
            (beads-list--header-chip
             preset "mouse-1: choose preset" #'beads-list-choose-preset)))
         (counts (beads-list--count-chip))
         (base (cond
                ((and q (not (string-empty-p q)))
                 (format " %s beads:%s · search %s%s%s%s%s%s%s%s%s "
                         mark kind q type-chip sort-chip
                         (or narrow-chip "") follow-chip hide-chip
                         todo-chip counts load))
                (preset-chip
                 (concat (format " %s beads:%s · " mark kind)
                         preset-chip
                         (format "%s%s%s%s%s%s%s%s "
                                 type-chip sort-chip
                                 (or narrow-chip "") follow-chip hide-chip
                                 todo-chip counts load)))
                (t
                 (format " %s beads:%s%s%s%s%s%s%s%s%s "
                         mark kind type-chip sort-chip
                         (or narrow-chip "") follow-chip hide-chip
                         todo-chip counts load))))
         (text base))
    ;; Keep inner faces; only tint the focus marker.
    (when focused
      (setq text (concat (propertize " " 'face 'mode-line) (substring text 1))))
    text))
(defun beads-list--progress-part (glyph done total help)
  "Format one `GLYPH DONE/TOTAL' header chip, or nil."
  (when (and (numberp total) (> total 0))
    (propertize (format " %s %d/%d" glyph done total)
                'face 'shadow
                'help-echo help)))

(defun beads-list--selected-context-chip ()
  "Selected-issue context chips.
Order: ↑ parent · ↓ child · ← blocks · → blocked by · ✓ acceptance.
Example: ` ↑ 1/1 ↓ 0/5 ← 2/3 → 1/1 ✓ 0/2'."
  (let* ((iss (get-text-property (line-beginning-position) 'beads-issue))
         (id (and iss (beads-issue-id iss)))
         (parts nil))
    (when iss
      (when-let* ((p (beads-rel-parent-progress iss beads-list--issues)))
        (push (beads-list--progress-part
               "↑" (car p) (cdr p)
               "Parent closed/total")
              parts))
      (when-let* ((p (beads-rel-child-progress id beads-list--issues)))
        (push (beads-list--progress-part
               "↓" (car p) (cdr p)
               "Children closed/total")
              parts))
      (when-let* ((p (beads-rel-progress
                      (cl-remove-if-not #'beads-rel-blocking-p
                                        (beads-issue-dependents iss))
                      beads-list--issues)))
        (push (beads-list--progress-part
               "←" (car p) (cdr p)
               "Blocks closed/total (issues this one blocks)")
              parts))
      (when-let* ((p (beads-rel-progress
                      (cl-remove-if-not #'beads-rel-blocking-p
                                        (beads-issue-dependencies iss))
                      beads-list--issues)))
        (push (beads-list--progress-part
               "→" (car p) (cdr p)
               "Blocked by closed/total (depends-on)")
              parts))
      (when-let* ((p (beads-rel-acceptance-progress iss)))
        (push (beads-list--progress-part
               "✓" (car p) (cdr p)
               "Acceptance criteria checkboxes done/total")
              parts)))
    (let ((s (mapconcat #'identity (nreverse (delq nil parts)) "")))
      (if (string-empty-p s) "" (concat " ·" s)))))

(provide 'beads-list-header)
;;; beads-list-header.el ends here
