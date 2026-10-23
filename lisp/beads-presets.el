;;; beads-presets.el --- List filter presets (BTU 0-5) -*- lexical-binding: t; -*-

;;; Commentary:
;; Named list filters matching BTU presets.

;;; Code:

(require 'cl-lib)

(defconst beads-presets
  '((all         . ((status . "open,in_progress,blocked,closed") (assignee . nil)))
    (open        . ((status . "open") (assignee . nil)))
    (in_progress . ((status . "in_progress") (assignee . nil)))
    (deferred    . ((status . "deferred") (assignee . nil)))
    (closed      . ((status . "closed") (assignee . nil)))
    (mine        . ((status . "open,in_progress,blocked") (assignee . t))))
  "Alist of preset symbol → filter plist.
Assignee t means use the current actor.")

(defun beads-preset-name (preset)
  "Display name for PRESET symbol."
  (symbol-name preset))

(defun beads-preset-from-key (key)
  "Map KEY string to a preset symbol, or nil."
  (pcase key
    ("0" 'all)
    ("1" 'open)
    ("2" 'in_progress)
    ("3" 'deferred)
    ("4" 'closed)
    ((or "5" "a") 'mine)
    (_ nil)))

(defun beads-preset-filter (preset actor)
  "Return (STATUS . ASSIGNEE) for PRESET and ACTOR string."
  (let* ((spec (alist-get preset beads-presets))
         (status (alist-get 'status spec))
         (assignee-flag (alist-get 'assignee spec))
         (assignee (when assignee-flag
                     (or actor user-login-name))))
    (cons status assignee)))

(provide 'beads-presets)
;;; beads-presets.el ends here
