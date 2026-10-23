;;; beads.el --- Emacs UI for Beads (bd) -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: beads.el contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, project, issues
;; URL: https://github.com/wnoronha/beads.el
;; License: MIT

;;; Commentary:
;;
;; Emacs UI for the Beads issue tracker (`bd`).
;;
;; Open a Beads project, then run M-x beads. The list buffer shows
;; issues. RET opens a detail buffer for one issue. Detail q returns
;; to the list. List q ends the session.
;;
;; Requirements: Emacs 29.1+, `bd` on PATH (see `beads-bd-min-version').
;; Install and keys: see README.md.

;;; Code:

(defconst beads-version "0.1.0")

(defun beads--ensure-load-path ()
  "Ensure this file's directory is on `load-path'."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (when dir
      (add-to-list 'load-path dir))))

(beads--ensure-load-path)

(require 'beads-cli)
(require 'beads-faces)
(require 'beads-coltree)
(require 'beads-md)
(require 'beads-layout)
(require 'beads-list)
(require 'beads-detail)

;;;###autoload
(defun beads (&optional directory)
  "Open Beads list for DIRECTORY (or `beads-directory' / default-directory)."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Beads project: " nil nil t))))
  (beads-list directory))

;;;###autoload
(defun beads-here ()
  "Open Beads for `default-directory'."
  (interactive)
  (let ((beads-directory default-directory))
    (beads-list)))

;;;###autoload
(defun beads-show (id)
  "Show issue ID in the detail buffer."
  (interactive "sIssue id: ")
  (beads-detail-show id))

(provide 'beads)
;;; beads.el ends here
