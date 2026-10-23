;;; beads-layout.el --- List / detail window placement -*- lexical-binding: t; -*-

;;; Commentary:
;; Default screens are standalone: `M-x beads' shows only the list;
;; RET opens detail in its own buffer (switch-to-buffer). Shared CLI /
;; issue / actions / md stay common.
;;
;; Opt-in paired panes via `beads-layout':
;;   stacked / side-by-side — always-on Gnus/BTU splits
;; Pair with `beads-auto-follow' for motion→detail without focus steal.

;;; Code:

(require 'subr-x)

(defcustom beads-layout nil
  "How to place the list and detail screens.
nil            — standalone (default): list alone; RET switches to
                 the detail buffer. No forced split.
\\=`stacked'      — list above, detail below (Gnus / many mail clients).
\\=`side-by-side' — list left, detail right (BTU columns).

Re-enable classic Gnus split + follow with:
  (setq beads-layout \\='stacked beads-auto-follow t)"
  :type '(choice (const :tag "standalone buffers (default)" nil)
                 (const stacked)
                 (const side-by-side))
  :group 'beads)

(defcustom beads-list-window-size 0.45
  "Fraction of the frame kept for the list pane when layout is paired (0–1)."
  :type 'float
  :group 'beads)

(defvar beads-list-buffer-name "*beads*"
  "Name of the Beads list buffer.")
(defvar beads-detail-buffer-name "*beads-detail*"
  "Name of the Beads detail buffer.")

(defun beads-layout--paired-p ()
  "Non-nil when `beads-layout' requests a list|detail split."
  (memq beads-layout '(stacked side-by-side)))

(defun beads-layout--list-buffer ()
  "Return the list buffer, creating it if needed."
  (get-buffer-create beads-list-buffer-name))

(defun beads-layout--detail-buffer ()
  "Return the detail buffer, creating it if needed."
  (get-buffer-create beads-detail-buffer-name))

(defun beads-layout--stacked-p (list-win detail-win)
  "Non-nil when LIST-WIN and DETAIL-WIN are one above the other."
  (= (window-left-column list-win) (window-left-column detail-win)))

(defun beads-layout--side-by-side-p (list-win detail-win)
  "Non-nil when LIST-WIN and DETAIL-WIN share a top line (columns)."
  (= (window-top-line list-win) (window-top-line detail-win)))

(defun beads-layout--wrong-p (list-win detail-win)
  "Non-nil when visible windows do not match a paired `beads-layout'."
  (cond
   ((eq beads-layout 'stacked)
    (or (not (beads-layout--stacked-p list-win detail-win))
        ;; Detail must be below list.
        (<= (window-top-line detail-win) (window-top-line list-win))))
   ((eq beads-layout 'side-by-side)
    (or (not (beads-layout--side-by-side-p list-win detail-win))
        ;; Detail must be right of list.
        (<= (window-left-column detail-win) (window-left-column list-win))))
   (t nil)))

(defun beads-layout-ensure (&optional force)
  "Ensure list|detail windows when `beads-layout' is paired.
No-op for standalone (nil). Point stays in the list window.
When FORCE, rebuild even if both buffers are already visible."
  (when (beads-layout--paired-p)
    (let* ((list-buf (beads-layout--list-buffer))
           (detail-buf (beads-layout--detail-buffer))
           (list-win (get-buffer-window list-buf t))
           (detail-win (get-buffer-window detail-buf t)))
      (when (or force
                (not list-win)
                (not detail-win)
                (and list-win detail-win
                     (beads-layout--wrong-p list-win detail-win)))
        (delete-other-windows)
        (switch-to-buffer list-buf)
        (let* ((size beads-list-window-size)
               (detail
                (if (eq beads-layout 'stacked)
                    ;; List on top, detail below (Gnus article).
                    (split-window-below
                     (max 1 (floor (* (window-total-height) size))))
                  ;; List left, detail right (BTU columns).
                  (split-window-right
                   (max 1 (floor (* (window-total-width) size)))))))
          (set-window-buffer detail detail-buf)
          (select-window (get-buffer-window list-buf)))))))

(defun beads-layout-display-detail (buf &optional select)
  "Show detail BUF according to `beads-layout'.
Standalone (nil): with SELECT, `switch-to-buffer' BUF (RET). Without
SELECT (auto-follow), update an existing detail window if any; do not
split or steal focus.
Paired layouts: ensure the split; SELECT focuses detail, else list."
  (cond
   ((not (beads-layout--paired-p))
    (if select
        (switch-to-buffer buf)
      (when-let* ((win (get-buffer-window buf t)))
        (set-window-buffer win buf))))
   (t
    (beads-layout-ensure)
    (let ((win (get-buffer-window buf t)))
      (unless win
        (beads-layout-ensure t)
        (setq win (get-buffer-window buf t)))
      (when win
        (set-window-buffer win buf)
        (if select
            (select-window win)
          (when-let* ((lw (get-buffer-window (beads-layout--list-buffer) t)))
            (select-window lw))))))))

(defun beads-layout-quit-detail ()
  "Leave the detail screen and return to the list.
Does not exit the Beads session.
Standalone: switch back to the list buffer in the current window.
Paired: delete the detail window so the list expands, or show list
when detail was the only visible Beads window."
  (let* ((list-buf (beads-layout--list-buffer))
         (detail-buf (beads-layout--detail-buffer))
         (list-win (get-buffer-window list-buf t))
         (detail-win (get-buffer-window detail-buf t)))
    (cond
     ((and list-win detail-win (not (eq list-win detail-win)))
      (select-window list-win)
      (when (window-live-p detail-win)
        (delete-window detail-win)))
     (detail-win
      (set-window-buffer detail-win list-buf)
      (select-window detail-win))
     (list-win
      (select-window list-win))
     (t
      (switch-to-buffer list-buf)))))

(defun beads-layout--non-beads-buffer (beads-bufs)
  "Return a buffer not in BEADS-BUFS, or a fresh *scratch*."
  (or (catch 'found
        (dolist (b (buffer-list))
          (unless (memq b beads-bufs)
            (throw 'found b))))
      (get-buffer-create "*scratch*")))

(defun beads-layout-quit-session ()
  "Bury list and detail windows (session exit).
Deletes panes showing Beads buffers. Avoids `quit-window' on the detail
pane, which would restore the list into that window and leave a dual
list layout."
  (let* ((list-buf (get-buffer beads-list-buffer-name))
         (detail-buf (get-buffer beads-detail-buffer-name))
         (beads-bufs (delq nil (list detail-buf list-buf)))
         (wins (delete-dups
                (apply #'append
                       (mapcar (lambda (b)
                                 (get-buffer-window-list b nil t))
                               beads-bufs))))
         (fallback (beads-layout--non-beads-buffer beads-bufs))
         (has-other
          (catch 'yes
            (dolist (w (window-list nil 0))
              (unless (memq (window-buffer w) beads-bufs)
                (throw 'yes t)))
            nil)))
    (dolist (b beads-bufs)
      (when (buffer-live-p b)
        (bury-buffer b)))
    (cond
     ((null wins)
      (when (memq (current-buffer) beads-bufs)
        (switch-to-buffer fallback)))
     (has-other
      ;; Keep non-beads windows; drop beads panes.
      (dolist (win wins)
        (when (window-live-p win)
          (if (one-window-p)
              (set-window-buffer win fallback)
            (delete-window win)))))
     (t
      ;; Frame is only beads panes: collapse to one + show fallback.
      (let ((keep (car wins)))
        (dolist (win (cdr wins))
          (when (and (window-live-p win) (not (one-window-p)))
            (delete-window win)))
        (when (window-live-p keep)
          (set-window-buffer keep fallback)
          (select-window keep)))))))

(provide 'beads-layout)
;;; beads-layout.el ends here
