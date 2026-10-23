;;; beads-list.el --- Threaded issue list mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Major mode coordinator for the Beads list: loads filter/header/data/
;; follow/render/yank/keymap modules and defines mode entry points.
;;
;; Buffer-local contract (list ↔ detail ↔ follow)
;;   List (`beads-list-mode'): issues/kind/loading in `beads-list-data';
;;   rows/collapsed in `beads-list-render'; filters in `beads-list-filter';
;;   follow timer/history/merge cache in `beads-list-follow'.
;;   Refresh: `beads-list-refresh' → `beads-list--apply-loaded' →
;;   `beads-list-redraw'. Redraw-only: `beads-list-redraw'.
;;   Detail: `beads-detail--id'/`--issue'; refresh `beads-detail-refresh'
;;   (`bd show'). Follow: `beads-list-follow-now' → `beads-detail-display';
;;   after show, `beads-list-merge-from-show' enriches list stubs.
;;   Writes (claim/comment/…) only via `beads-actions' (see docs/architecture.md).

;;; Code:

(require 'beads-cli)
(require 'beads-issue)
(require 'beads-tree)
(require 'beads-presets)
(require 'beads-actions)
(require 'beads-layout)
(require 'beads-md)
(require 'beads-faces)
(require 'hl-line)
(require 'cl-lib)
(require 'subr-x)

(require 'beads-yank)
(require 'beads-list-filter)
(require 'beads-list-header)
(require 'beads-list-render)
(require 'beads-list-follow)
(require 'beads-list-data)
(require 'beads-keymap)

(defcustom beads-tty-hide-menu-bar nil
  "When non-nil, hide the menu bar on TTY when opening the Beads list.
Off by default: `menu-bar-mode' is global, so enabling this affects
the whole Emacs session, not only the Beads buffers."
  :type 'boolean
  :group 'beads)

(defvar beads-detail-buffer-name)

(defun beads-list-zoom-list ()
  "Maximize the list window."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer (current-buffer)))

(defun beads-list-zoom-detail ()
  "Maximize the detail window."
  (interactive)
  (let ((dbuf (get-buffer beads-detail-buffer-name)))
    (unless dbuf (user-error "No detail buffer"))
    (delete-other-windows)
    (switch-to-buffer dbuf)))

(defun beads-list-unzoom ()
  "Restore stacked/side-by-side list+detail layout."
  (interactive)
  (require 'beads-layout)
  (beads-layout-ensure t)
  (when-let* ((win (get-buffer-window beads-list-buffer-name t)))
    (select-window win)))

(defun beads-list-balance-windows ()
  "Balance list/detail windows."
  (interactive)
  (balance-windows)
  (message "windows balanced"))

(defun beads-list-quit ()
  "Exit the Beads session: bury list and detail panes.
Gnus summary `q`: leave the summary. Detail `q` only returns here."
  (interactive)
  (beads-layout-quit-session))

(define-derived-mode beads-list-mode special-mode "Beads"
  "Major mode for browsing Beads issues.

Layout and follow
  Default is standalone screens (`beads-layout' nil): full-frame
  list only.  RET opens the issue in the detail buffer
  (`switch-to-buffer'); detail q returns to the list.  Set
  `beads-layout' to `stacked' or `side-by-side' for an always-on
  paired split.  With `beads-auto-follow' non-nil, moving point
  updates detail without stealing focus (off by default; most
  useful with a paired layout).  Toggle follow:
  \\[beads-list-toggle-follow] (emacs style C-c C-f f; btu style F).
  Classic Gnus: (setq beads-layout \\='stacked beads-auto-follow t).

Leaving
  q (`beads-list-quit') ends the session: buries list and detail.
  In the detail buffer, q only returns here.

Chrome
  Header-line holds view context (preset, filters, sort, counts).
  Mode-line is stock Emacs.  Detail uses its own header for tabs.

Keys track `beads-keymap-style' (`emacs' or `btu').  Use C-h m
or ? (`describe-mode') for this help and the live map below.

Status glyphs (list column): ○ open · ◐ in_progress · ● blocked ·
✓ closed · ❄ deferred · ◈ pinned · ◇ hooked · ◌ custom · ⊘ gated.
Same legend: `beads-status-legend' in `beads-faces' (Customize group
doc), and hover the header count chip (help-echo).  Not a separate
help buffer.

\\{beads-list-mode-map}"
  (setq buffer-read-only t
        truncate-lines t
        beads--id-at-point-fn #'beads-list--id-at-point)
  (setq-local scroll-margin 2
              scroll-conservatively 101)
  ;; Stock mode-line; list chrome lives in header-line only.
  (setq-local beads-list--collapsed (make-hash-table :test #'equal))
  (setq-local hl-line-face 'beads-face-list-hl
              ;; Full visual line so fold column is covered too.
              hl-line-range-function nil)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'beads-list--follow-after-command nil t)
  (add-hook 'window-size-change-functions #'beads-list--on-window-size)
  (setq-local imenu-create-index-function #'beads-list--imenu-index)
  (setq-local beads-list--size-timer nil
              beads-list--last-width nil))

(defun beads-list--imenu-index ()
  "Imenu index of visible issue ids."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id (get-text-property (point) 'beads-id))
                    (iss (get-text-property (point) 'beads-issue)))
          (push (cons (format "%s %s" id (beads-issue-title iss))
                      (copy-marker (point)))
                entries))
        (forward-line 1)))
    (nreverse entries)))

;;;###autoload
(defun beads-list (&optional directory)
  "Open the Beads list. Optional DIRECTORY sets `beads-directory'."
  (interactive)
  (when directory
    (setq beads-directory (expand-file-name directory)))
  ;; Opt-in TTY chrome (menu-bar-mode is global — off by default).
  (when (and beads-tty-hide-menu-bar
             (not (display-graphic-p))
             (fboundp 'menu-bar-mode))
    (menu-bar-mode -1))
  (let ((buf (get-buffer-create beads-list-buffer-name)))
    (with-current-buffer buf
      ;; Always re-enter so keymap / follow hook pick up reloads.
      (beads-list-mode)
      (setq beads-list--kind 'list
            default-directory (beads-cli--workdir)))
    ;; Standalone: show list full-frame. Paired: ensure stacked/side split.
    (if (beads-layout--paired-p)
        (beads-layout-ensure t)
      (switch-to-buffer buf)
      (delete-other-windows))
    (with-current-buffer buf
      (beads-list-refresh t))
    (when-let* ((win (get-buffer-window buf t)))
      (select-window win))))

(provide 'beads-list)
;;; beads-list.el ends here
