;;; beads-faces.el --- Faces for beads list and detail -*- lexical-binding: t; -*-

;;; Commentary:
;; Faces for the threaded list (and shared detail meta). Tuned for dark
;; terminals; inherit standard faces so themes still apply.
;;
;; Status glyph legend (canonical: `beads-status-legend'; also in the
;; list mode docstring, README, and list mode-line help-echo — not a
;; separate *beads-help* buffer):
;;   ○ open · ◐ in_progress · ● blocked · ✓ closed · ❄ deferred
;;   ◈ pinned · ◇ hooked · ◌ custom · ⊘ gated

;;; Code:

(require 'subr-x)

(defgroup beads-faces nil
  "Faces for beads.el.

Status glyphs (see `beads-status-legend'):
○ open · ◐ in_progress · ● blocked · ✓ closed · ❄ deferred ·
◈ pinned · ◇ hooked · ◌ custom · ⊘ gated."
  :group 'beads)

(defface beads-face-id
  '((t :inherit font-lock-constant-face))
  "Face for issue ids."
  :group 'beads-faces)

(defface beads-face-priority-high
  '((t :inherit error :weight bold))
  "Face for P0/P1."
  :group 'beads-faces)

(defface beads-face-priority
  '((t :inherit font-lock-keyword-face))
  "Face for normal priority."
  :group 'beads-faces)

(defface beads-face-status-open
  '((t :inherit success))
  "Face for open status."
  :group 'beads-faces)

(defface beads-face-status-progress
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for in_progress status."
  :group 'beads-faces)

(defface beads-face-status-blocked
  '((t :inherit warning :weight bold))
  "Face for blocked status."
  :group 'beads-faces)

(defface beads-face-status-closed
  '((t :inherit shadow))
  "Face for closed status."
  :group 'beads-faces)

(defface beads-face-status-deferred
  '((t :inherit font-lock-comment-face))
  "Face for deferred status."
  :group 'beads-faces)

(defface beads-face-status-pinned
  '((t :inherit font-lock-warning-face))
  "Face for pinned status."
  :group 'beads-faces)

(defface beads-face-status-hooked
  '((t :inherit font-lock-builtin-face))
  "Face for hooked status."
  :group 'beads-faces)

(defface beads-face-status-gated
  '((t :inherit warning :weight bold))
  "Face for gated status."
  :group 'beads-faces)

(defface beads-face-status-custom
  '((t :inherit shadow))
  "Face for unknown/custom status."
  :group 'beads-faces)

(defface beads-face-type-epic
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for epic type."
  :group 'beads-faces)

(defface beads-face-type-bug
  '((t :inherit error))
  "Face for bug type."
  :group 'beads-faces)

(defface beads-face-type-feature
  '((t :inherit font-lock-type-face))
  "Face for feature type."
  :group 'beads-faces)

(defface beads-face-type-task
  '((t :inherit font-lock-variable-name-face))
  "Face for task type."
  :group 'beads-faces)

(defface beads-face-type-chore
  '((t :inherit font-lock-comment-face))
  "Face for chore type."
  :group 'beads-faces)

(defface beads-face-type-gate
  '((t :inherit font-lock-warning-face))
  "Face for gate type."
  :group 'beads-faces)

(defface beads-face-type-spike
  '((t :inherit font-lock-preprocessor-face))
  "Face for spike type."
  :group 'beads-faces)

(defface beads-face-type-story
  '((t :inherit font-lock-doc-face))
  "Face for story type."
  :group 'beads-faces)

(defface beads-face-type
  '((t :inherit font-lock-type-face))
  "Face for other issue types."
  :group 'beads-faces)

(defface beads-face-title
  '((t :inherit default :weight semi-bold))
  "Face for live issue titles."
  :group 'beads-faces)

(defface beads-face-title-closed
  '((t :inherit shadow :weight light :slant normal))
  "Face for closed issue titles (dimmer than live)."
  :group 'beads-faces)
(defface beads-face-fold
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for ▸/▾ fold markers."
  :group 'beads-faces)

(defface beads-face-tab
  ;; Flat against header-line — do not inherit mode-line-inactive
  ;; (its background reads as a second "selected" tab).
  '((t :inherit header-line :box nil :weight normal :slant normal))
  "Face for inactive detail tabs (tab-line style)."
  :group 'beads-faces)

(defface beads-face-tab-current
  ;; Explicit contrast vs inactive; avoid mode-line box bleed onto neighbors.
  '((((class color) (min-colors 88) (background light))
     :inherit header-line :weight bold :box nil
     :foreground "black" :background "gray75")
    (((class color) (min-colors 88) (background dark))
     :inherit header-line :weight bold :box nil
     :foreground "white" :background "gray35")
    (t :inherit header-line :weight bold :inverse-video t :box nil))
  "Face for the selected detail tab (Magit/tab-line emphasis)."
  :group 'beads-faces)

(defface beads-face-tab-id
  '((t :inherit beads-face-id :weight bold))
  "Face for the issue id in the detail tab bar."
  :group 'beads-faces)

(defface beads-face-section
  '((t :inherit font-lock-keyword-face :weight bold
       :underline nil :box nil :background unspecified :extend nil))
  "Face for detail section headings (flat; no outline highlight bleed)."
  :group 'beads-faces)

(defface beads-face-list-hl
  '((((class color) (min-colors 88) (background light))
     :inherit hl-line :extend t)
    (((class color) (min-colors 88) (background dark))
     :inherit hl-line :extend t)
    (t :inherit highlight :extend t))
  "Face for the selected list row (hl-line)."
  :group 'beads-faces)

(defun beads-face-for-status (status)
  "Return face symbol for STATUS string."
  (pcase (downcase (or status ""))
    ("open" 'beads-face-status-open)
    ("in_progress" 'beads-face-status-progress)
    ("blocked" 'beads-face-status-blocked)
    ("closed" 'beads-face-status-closed)
    ("deferred" 'beads-face-status-deferred)
    ("pinned" 'beads-face-status-pinned)
    ("hooked" 'beads-face-status-hooked)
    ("gated" 'beads-face-status-gated)
    ("" 'beads-face-status-open)
    (_ 'beads-face-status-custom)))

(defconst beads-status-legend
  "○ open  ◐ in_progress  ● blocked  ✓ closed  ❄ deferred  ◈ pinned  ◇ hooked  ◌ custom  ⊘ gated"
  "Legend matching BTU / beads status glyphs.")

(defun beads-status-glyph (status)
  "Return BTU/beads status glyph for STATUS."
  (pcase (downcase (string-trim (or status "")))
    ("open" "○")
    ("in_progress" "◐")
    ("blocked" "●")
    ("closed" "✓")
    ("deferred" "❄")
    ("pinned" "◈")
    ("hooked" "◇")
    ("gated" "⊘")
    ("" "○")
    (_ "◌")))

(defun beads-face-for-type (type)
  "Return face symbol for TYPE string."
  (pcase (downcase (or type ""))
    ("epic" 'beads-face-type-epic)
    ("bug" 'beads-face-type-bug)
    ("feature" 'beads-face-type-feature)
    ("task" 'beads-face-type-task)
    ("chore" 'beads-face-type-chore)
    ("gate" 'beads-face-type-gate)
    ("spike" 'beads-face-type-spike)
    ("story" 'beads-face-type-story)
    ("decision" 'beads-face-type-spike)
    ("milestone" 'beads-face-type-story)
    (_ 'beads-face-type)))

(defun beads-face-for-priority (pri)
  "Return face symbol for priority integer PRI."
  (if (<= (or pri 99) 1)
      'beads-face-priority-high
    'beads-face-priority))

(provide 'beads-faces)
;;; beads-faces.el ends here
