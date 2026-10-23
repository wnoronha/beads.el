;;; beads-keymap.el --- Shared and mode keymaps for beads -*- lexical-binding: t; -*-

;;; Commentary:
;; `beads-mode-map' is the shared parent for list and detail.
;; Default style `emacs' follows ibuffer/Magit/Gnus idioms; `btu'
;; restores the prior single-letter BTU-parity map.

;;; Code:

(declare-function beads-claim "beads-actions" ())
(declare-function beads-comment "beads-actions" ())
(declare-function beads-create "beads-actions" ())
(declare-function beads-close "beads-actions" ())
(declare-function beads-set-status "beads-actions" ())
(declare-function beads-set-title "beads-actions" ())
(declare-function beads-reparent "beads-actions" ())
(declare-function beads-yank-id "beads-yank" ())
(declare-function beads-yank-title "beads-yank" ())

(declare-function beads-list-next-line "beads-list-render" ())
(declare-function beads-list-prev-line "beads-list-render" ())
(declare-function beads-list-mouse-select "beads-list-render" ())
(declare-function beads-list-refresh "beads-list-data" (&optional seed-fold))
(declare-function beads-list-open-detail "beads-list-follow" ())
(declare-function beads-list-toggle-fold "beads-list-render" ())
(declare-function beads-list-collapse "beads-list-render" ())
(declare-function beads-list-show-list "beads-list-data" ())
(declare-function beads-list-expand "beads-list-render" ())
(declare-function beads-list-fold-idle "beads-list-render" ())
(declare-function beads-list-cycle-sort "beads-list-render" ())
(declare-function beads-list-narrow-subtree "beads-list-filter" (&optional arg))
(declare-function beads-list-collapse-all "beads-list-render" ())
(declare-function beads-list-expand-all "beads-list-render" ())
(declare-function beads-list-search "beads-list-filter" ())
(declare-function beads-list-clear-search "beads-list-filter" ())
(declare-function beads-list-yank-org "beads-yank" ())
(declare-function beads-list-yank-markdown "beads-yank" ())
(declare-function beads-list-yank-description "beads-yank" ())
(declare-function beads-list-yank-preview-rendered "beads-yank" ())
(declare-function beads-list-view-markdown "beads-yank" ())
(declare-function beads-list-browse-external "beads-yank" ())
(declare-function beads-list-recenter "beads-list-render" ())
(declare-function beads-list-filter-type "beads-list-filter" ())
(declare-function beads-list-filter-label "beads-list-filter" ())
(declare-function beads-list-filter-priority "beads-list-filter" ())
(declare-function beads-list-cycle-status-filter "beads-list-filter" ())
(declare-function beads-list-toggle-todos-only "beads-list-filter" ())
(declare-function beads-list-toggle-follow "beads-list-follow" ())
(declare-function beads-list-toggle-hide-closed-roots "beads-list-filter" ())
(declare-function beads-list-balance-windows "beads-list" ())
(declare-function beads-list-zoom-list "beads-list" ())
(declare-function beads-list-zoom-detail "beads-list" ())
(declare-function beads-list-unzoom "beads-list" ())
(declare-function beads-list-history-back "beads-list-follow" ())
(declare-function beads-list-history-forward "beads-list-follow" ())
(declare-function beads-list-show-ready "beads-list-data" ())
(declare-function beads-list-show-blocked "beads-list-data" ())
(declare-function beads-list-show-human "beads-list-data" ())
(declare-function beads-list-goto-description "beads-list-follow" ())
(declare-function beads-list-goto-links "beads-list-follow" ())
(declare-function beads-list-goto-comments "beads-list-follow" ())
(declare-function beads-list-goto-acceptance "beads-list-follow" ())
(declare-function beads-list-goto-design "beads-list-follow" ())
(declare-function beads-list-goto-description "beads-list-follow" ())
(declare-function beads-list-preset-key "beads-list-filter" ())

(declare-function beads-list-quit "beads-list" ())
(declare-function beads-detail-quit "beads-detail" ())
(declare-function beads-detail-refresh "beads-detail" ())
(declare-function beads-detail-next-heading "beads-detail" ())
(declare-function beads-detail-prev-heading "beads-detail" ())
(declare-function beads-detail-toggle-fold "beads-detail" ())
(declare-function beads-detail-goto-section "beads-detail" (title &optional force-comments))
(declare-function beads-detail--goto-heading "beads-detail-render" (title))
(declare-function beads-detail--after-heading-hop "beads-detail" ())
(declare-function beads-detail-follow-link "beads-detail" ())
(declare-function beads-detail-yank-description-rendered "beads-detail" ())
(declare-function beads-detail-yank-notes-rendered "beads-detail" ())
(declare-function beads-detail-yank-design-rendered "beads-detail" ())
(declare-function beads-detail-yank-at-heading "beads-detail" ())
(declare-function beads-detail-next-sibling "beads-detail" ())
(declare-function beads-detail-prev-sibling "beads-detail" ())

(defgroup beads-keymap nil
  "Keymaps for Beads list and detail modes."
  :group 'beads
  :prefix "beads-keymap-")

(defcustom beads-keymap-style 'emacs
  "Keymap style for Beads list and detail.
`emacs' (default): ibuffer/Magit/Gnus feel — bare n/p, RET, TAB, /,
g, q, ?, 1–3; most commands under C-c prefixes.
`btu': previous single-letter BTU/TUI parity map."
  :type '(choice (const :tag "Emacs (ibuffer/Magit/Gnus)" emacs)
                 (const :tag "BTU single-letter" btu))
  :group 'beads-keymap
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'beads-keymap-rebuild)
           (beads-keymap-rebuild))))

(defun beads-keymap--goto-description ()
  "Jump to Description section from list or detail."
  (interactive)
  (if (eq major-mode 'beads-list-mode)
      (beads-list-goto-description)
    (beads-detail-goto-section "Description")))

(defun beads-keymap--goto-links ()
  "Jump to first Links section from list or detail."
  (interactive)
  (if (eq major-mode 'beads-list-mode)
      (beads-list-goto-links)
    (unless (or (beads-detail--goto-heading "Parent")
                (beads-detail--goto-heading "Depends on")
                (beads-detail--goto-heading "Children")
                (beads-detail--goto-heading "Blocks")
                (beads-detail--goto-heading "Related"))
      (user-error "No links section"))
    (beads-detail--after-heading-hop)))

(defun beads-keymap--goto-comments ()
  "Jump to Comments section from list or detail."
  (interactive)
  (if (eq major-mode 'beads-list-mode)
      (beads-list-goto-comments)
    (beads-detail-goto-section "Comments")))

(defun beads-keymap--yank-description ()
  "Yank description (rendered in detail, raw on list)."
  (interactive)
  (call-interactively
   (if (eq major-mode 'beads-detail-mode)
       #'beads-detail-yank-description-rendered
     #'beads-list-yank-description)))

(defun beads-keymap--make-yank-map ()
  "Return the shared `C-c C-y' yank prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'beads-yank-id)
    (define-key map (kbd "t") #'beads-yank-title)
    (define-key map (kbd "o") #'beads-list-yank-org)
    (define-key map (kbd "m") #'beads-list-yank-markdown)
    (define-key map (kbd "d") #'beads-keymap--yank-description)
    (define-key map (kbd "w") #'beads-list-yank-preview-rendered)
    (define-key map (kbd "n") #'beads-detail-yank-notes-rendered)
    (define-key map (kbd "g") #'beads-detail-yank-design-rendered)
    (define-key map (kbd "h") #'beads-detail-yank-at-heading)
    map))

(defun beads-keymap--make-filter-map ()
  "Return the list `C-c C-f' filter prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'beads-list-filter-type)
    (define-key map (kbd "l") #'beads-list-filter-label)
    (define-key map (kbd "p") #'beads-list-filter-priority)
    (define-key map (kbd "s") #'beads-list-cycle-status-filter)
    (define-key map (kbd "%") #'beads-list-toggle-todos-only)
    (define-key map (kbd "f") #'beads-list-toggle-follow)
    (define-key map (kbd "x") #'beads-list-toggle-hide-closed-roots)
    (define-key map (kbd "u") #'beads-list-clear-search)
    (define-key map (kbd "S") #'beads-list-cycle-sort)
    (define-key map (kbd "z") #'beads-list-fold-idle)
    map))

(defun beads-keymap--make-preset-map ()
  "Return the list `C-c C-p' preset prefix map."
  (let ((map (make-sparse-keymap)))
    (dolist (k '("0" "1" "2" "3" "4" "5" "a"))
      (define-key map (kbd k) #'beads-list-preset-key))
    map))

(defun beads-keymap--make-queue-map ()
  "Return the list `C-c C-w' queue prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'beads-list-show-ready)
    (define-key map (kbd "b") #'beads-list-show-blocked)
    (define-key map (kbd "h") #'beads-list-show-human)
    (define-key map (kbd "l") #'beads-list-show-list)
    map))

(defun beads-keymap--make-history-map ()
  "Return the list `C-c C-h' history prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'beads-list-history-back)
    (define-key map (kbd "n") #'beads-list-history-forward)
    map))

(defun beads-keymap--make-jump-map ()
  "Return the list `C-c C-j' section-jump prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'beads-list-goto-acceptance)
    (define-key map (kbd "d") #'beads-list-goto-design)
    (define-key map (kbd "e") #'beads-list-goto-description)
    map))

(defun beads-keymap--make-parent (style)
  "Build shared parent keymap for STYLE."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; `q' is pane-specific (Gnus): detail → list; list → session exit.
    ;; `?' matches C-h m (describe-mode); custom *beads-help* retired.
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "c") #'beads-comment)
    (define-key map (kbd "i") #'imenu)
    (define-key map (kbd "e") #'beads-list-browse-external)
    (define-key map (kbd "v") #'beads-list-view-markdown)
    ;; 1/2/3 jump to page sections (no tabs).
    (define-key map (kbd "1") #'beads-keymap--goto-description)
    (define-key map (kbd "2") #'beads-keymap--goto-links)
    (define-key map (kbd "3") #'beads-keymap--goto-comments)
    (pcase style
      ('emacs
       ;; Write keys on parent. Avoid C-c C-w / C-c C-n: list uses those
       ;; for queue prefix and narrow-subtree.
       (define-key map (kbd "C-c C-a") #'beads-claim)
       (define-key map (kbd "C-c C-o") #'beads-create)
       (define-key map (kbd "C-c C-d") #'beads-close)
       (define-key map (kbd "C-c C-s") #'beads-set-status)
       (define-key map (kbd "C-c C-t") #'beads-set-title)
       (define-key map (kbd "C-c C-r") #'beads-reparent)
       (define-key map (kbd "C-c C-y") (beads-keymap--make-yank-map)))
      ('btu
       (define-key map (kbd "C") #'beads-claim)
       (define-key map (kbd "+") #'beads-create)
       (define-key map (kbd "x") #'beads-close)
       (define-key map (kbd "s") #'beads-set-status)
       (define-key map (kbd "t") #'beads-set-title)
       (define-key map (kbd "R") #'beads-reparent)
       (define-key map (kbd "y") #'beads-yank-id)
       (define-key map (kbd "Y") #'beads-yank-title)))
    map))

(defun beads-keymap--make-list (style parent)
  "Build list keymap for STYLE with PARENT."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    (define-key map (kbd "q") #'beads-list-quit)
    (define-key map (kbd "n") #'beads-list-next-line)
    (define-key map (kbd "p") #'beads-list-prev-line)
    (define-key map (kbd "<down>") #'beads-list-next-line)
    (define-key map (kbd "<up>") #'beads-list-prev-line)
    (define-key map [mouse-1] #'beads-list-mouse-select)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "RET") #'beads-list-open-detail)
    (define-key map (kbd "TAB") #'beads-list-toggle-fold)
    (define-key map (kbd "z") #'beads-list-collapse-all)
    (define-key map (kbd "Z") #'beads-list-expand-all)
    (define-key map (kbd "/") #'beads-list-search)
    (define-key map (kbd "C-l") #'beads-list-recenter)
    (define-key map (kbd "=") #'beads-list-balance-windows)
    (define-key map (kbd "C-=") #'beads-list-zoom-list)
    (define-key map (kbd "C--") #'beads-list-zoom-detail)
    (define-key map (kbd "C-0") #'beads-list-unzoom)
    (pcase style
      ('emacs
       (define-key map (kbd "-") #'beads-list-collapse)
       (define-key map (kbd "+") #'beads-list-expand)
       (define-key map (kbd "<left>") #'beads-list-collapse)
       (define-key map (kbd "<right>") #'beads-list-expand)
       (define-key map (kbd "C-c C-f") (beads-keymap--make-filter-map))
       (define-key map (kbd "C-c C-p") (beads-keymap--make-preset-map))
       (define-key map (kbd "C-c C-w") (beads-keymap--make-queue-map))
       (define-key map (kbd "C-c C-h") (beads-keymap--make-history-map))
       (define-key map (kbd "C-c C-n") #'beads-list-narrow-subtree)
       (define-key map (kbd "C-c C-j") (beads-keymap--make-jump-map)))
      ('btu
       (define-key map (kbd "j") #'beads-list-next-line)
       (define-key map (kbd "k") #'beads-list-prev-line)
       (define-key map (kbd "o") #'beads-list-open-detail)
       (define-key map (kbd "h") #'beads-list-collapse)
       (define-key map (kbd "<left>") #'beads-list-collapse)
       (define-key map (kbd "l") #'beads-list-show-list)
       (define-key map (kbd "<right>") #'beads-list-expand)
       (define-key map (kbd "H") #'beads-list-fold-idle)
       (define-key map (kbd "S") #'beads-list-cycle-sort)
       (define-key map (kbd "N") #'beads-list-narrow-subtree)
       (define-key map (kbd "u") #'beads-list-clear-search)
       (define-key map (kbd "O") #'beads-list-yank-org)
       (define-key map (kbd "m") #'beads-list-yank-markdown)
       (define-key map (kbd "D") #'beads-list-yank-description)
       (define-key map (kbd "W") #'beads-list-yank-preview-rendered)
       (define-key map (kbd "T") #'beads-list-filter-type)
       (define-key map (kbd "L") #'beads-list-filter-label)
       (define-key map (kbd "P") #'beads-list-filter-priority)
       (define-key map (kbd "f") #'beads-list-cycle-status-filter)
       (define-key map (kbd "%") #'beads-list-toggle-todos-only)
       (define-key map (kbd "F") #'beads-list-toggle-follow)
       (define-key map (kbd "X") #'beads-list-toggle-hide-closed-roots)
       (define-key map (kbd "M-p") #'beads-list-history-back)
       (define-key map (kbd "M-n") #'beads-list-history-forward)
       (define-key map (kbd "r") #'beads-list-show-ready)
       (define-key map (kbd "b") #'beads-list-show-blocked)
       (define-key map (kbd "w") #'beads-list-show-human)
       (define-key map (kbd "M-1") #'beads-list-goto-description)
       (define-key map (kbd "M-2") #'beads-list-goto-links)
       (define-key map (kbd "M-3") #'beads-list-goto-comments)
       (define-key map (kbd "A") #'beads-list-goto-acceptance)
       (define-key map (kbd "B") #'beads-list-goto-design)
       (define-key map (kbd "E") #'beads-list-goto-description)
       (define-key map (kbd "0") #'beads-list-preset-key)
       (define-key map (kbd "1") #'beads-list-preset-key)
       (define-key map (kbd "2") #'beads-list-preset-key)
       (define-key map (kbd "3") #'beads-list-preset-key)
       (define-key map (kbd "4") #'beads-list-preset-key)
       (define-key map (kbd "5") #'beads-list-preset-key)
       (define-key map (kbd "a") #'beads-list-preset-key)))
    map))

(defun beads-keymap--make-detail (style parent)
  "Build detail keymap for STYLE with PARENT."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    ;; q → beads-detail-quit → quit-window (standalone) / layout tear-down (paired)
    (define-key map (kbd "q") #'beads-detail-quit)
    (define-key map (kbd "g") #'beads-detail-refresh)
    ;; TAB hops headings (Info-like); fold is z — not the accidental key.
    (define-key map (kbd "TAB") #'beads-detail-next-heading)
    (define-key map (kbd "<backtab>") #'beads-detail-prev-heading)
    (define-key map (kbd "z") #'beads-detail-toggle-fold)
    (define-key map (kbd "RET") #'beads-detail-follow-link)
    (define-key map (kbd "M-RET") #'beads-detail-follow-link)
    (pcase style
      ('emacs
       (define-key map (kbd "n") #'beads-detail-next-sibling)
       (define-key map (kbd "p") #'beads-detail-prev-sibling)
       (define-key map (kbd "C-n") #'next-line)
       (define-key map (kbd "C-p") #'previous-line))
      ('btu
       (define-key map (kbd "o") #'beads-detail-follow-link)
       (define-key map (kbd "W") #'beads-detail-yank-description-rendered)
       (define-key map (kbd "N") #'beads-detail-yank-notes-rendered)
       (define-key map (kbd "G") #'beads-detail-yank-design-rendered)
       (define-key map (kbd "M-w") #'beads-detail-yank-at-heading)
       (define-key map (kbd "n") #'next-line)
       (define-key map (kbd "p") #'previous-line)
       (define-key map (kbd "j") #'next-line)
       (define-key map (kbd "k") #'previous-line)
       (define-key map (kbd "M-n") #'beads-detail-next-sibling)
       (define-key map (kbd "M-p") #'beads-detail-prev-sibling)))
    map))

(defvar beads-mode-map nil
  "Parent keymap shared by Beads list and detail modes.
Rebuilt by `beads-keymap-rebuild' from `beads-keymap-style'.
Mode help (`describe-mode' / C-h m) documents pane-specific
keys via `\\{beads-list-mode-map}' and `\\{beads-detail-mode-map}'.")
(defvar beads-list-mode-map nil
  "Keymap for `beads-list-mode'.
Included in that mode's docstring via `\\{beads-list-mode-map}'.")
(defvar beads-detail-mode-map nil
  "Keymap for `beads-detail-mode'.
Included in that mode's docstring via `\\{beads-detail-mode-map}'.")

(defun beads-keymap-rebuild (&optional style)
  "Rebuild Beads keymaps for STYLE (default `beads-keymap-style')."
  (let* ((style (or style beads-keymap-style))
         (parent (beads-keymap--make-parent style))
         (list-map (beads-keymap--make-list style parent))
         (detail-map (beads-keymap--make-detail style parent)))
    (setq beads-mode-map parent
          beads-list-mode-map list-map
          beads-detail-mode-map detail-map)
    (setq-default beads-list-mode-map list-map
                  beads-detail-mode-map detail-map)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond
         ((eq major-mode 'beads-list-mode)
          (use-local-map beads-list-mode-map))
         ((eq major-mode 'beads-detail-mode)
          (use-local-map beads-detail-mode-map)))))
    style))

(beads-keymap-rebuild)

(provide 'beads-keymap)
;;; beads-keymap.el ends here
