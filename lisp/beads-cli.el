;;; beads-cli.el --- Run bd with JSON I/O -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrapper around the bd CLI. Sync and async helpers only.
;; Callers own parsing and UI.
;;
;; First real use probes `bd --version' via `beads-cli-ensure-bd'.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup beads nil
  "Emacs front end for Beads (bd)."
  :group 'tools
  :prefix "beads-")

(defcustom beads-bd-executable "bd"
  "Path to the bd executable."
  :type 'string
  :group 'beads)

(defcustom beads-directory nil
  "Working directory for bd (-C). Nil means `default-directory'."
  :type '(choice (const :tag "Default directory" nil) directory)
  :group 'beads)

(defcustom beads-actor nil
  "Actor name passed as --actor. Nil uses bd defaults."
  :type '(choice (const :tag "bd default" nil) string)
  :group 'beads)

(defcustom beads-bd-min-version "0.1.0"
  "Minimum required bd version string, or nil to skip the version check."
  :type '(choice (const :tag "Disable check" nil) string)
  :group 'beads)

(defvar beads-cli-debug nil
  "When non-nil, log bd invocations to *Messages*.")

(defvar beads-cli--bd-version nil
  "Cached bd version string from the last successful probe, or nil.")

;; bd CLI contract (subcommands/flags this package relies on):
;;   global     --json --no-color -C DIR [--actor NAME]
;;   list       [--status STATUS] [--assignee ACTOR]
;;   ready      [--exclude-type TYPES] [--limit N]
;;   human list
;;   show ID    --include-comments --include-dependents
;;   create     TITLE | --title TITLE [-d/--description] [-p/--priority] [--parent]
;;   close ID   [-r/--reason]
;;   update ID  --claim | -s/--status | --title | --parent
;;   comment ID --file PATH
;;   comments ID
;;   version / --version   (probe only; not via beads-cli-call)
(defconst beads-cli-bd-contract
  '(("list" "--status" "--assignee")
    ("ready" "--exclude-type" "--limit")
    ("human" "list")
    ("show" "--include-comments" "--include-dependents")
    ("create" "--title" "--description" "--priority" "--parent")
    ("close" "--reason")
    ("update" "--claim" "--status" "--title" "--parent")
    ("comment" "--file")
    ("comments")
    ("version"))
  "bd subcommands and flags beads.el relies on.")

(defun beads-cli--workdir ()
  "Resolve the directory bd should run in."
  (expand-file-name (or beads-directory default-directory)))

(defun beads-cli--base-args ()
  "Global flags shared by every bd invocation."
  (let ((args (list "--json" "--no-color"
                    "-C" (beads-cli--workdir))))
    (when (and beads-actor (not (string-empty-p beads-actor)))
      (setq args (append args (list "--actor" beads-actor))))
    args))

(defun beads-cli--parse (raw)
  "Parse RAW JSON string from bd. Return Lisp data or signal.
JSON false becomes `:json-false' (not nil) so boolean fields stay
distinguishable; JSON true becomes t. Use `beads-issue-json-true-p'."
  (when (and raw (not (string-empty-p (string-trim raw))))
    (json-parse-string raw
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object :json-false)))

(defun beads-cli--parse-version (raw)
  "Extract a dotted version string from bd version output RAW.
Accepts text like \"bd version 1.3.1 (abc)\" or JSON with a version key.
Return nil when no version can be found."
  (when (and raw (not (string-empty-p (string-trim raw))))
    (or (let ((parsed (ignore-errors
                        (json-parse-string raw :object-type 'alist))))
          (and (listp parsed)
               (let ((v (alist-get 'version parsed)))
                 (and (stringp v)
                      (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)+\\'" v)
                      v))))
        (and (string-match "\\([0-9]+\\(?:\\.[0-9]+\\)+\\)" raw)
             (match-string 1 raw)))))

(defun beads-cli--probe-version (program)
  "Run PROGRAM --version (fallback: version). Return stdout or nil."
  (with-temp-buffer
    (let ((exit (call-process program nil t nil "--version")))
      (unless (zerop exit)
        (erase-buffer)
        (setq exit (call-process program nil t nil "version")))
      (and (zerop exit) (buffer-string)))))

(defun beads-cli-ensure-bd ()
  "Ensure bd exists and meets `beads-bd-min-version'.
On first check (when a minimum is set), probe `bd --version' and cache
the result in `beads-cli--bd-version'. Signal `user-error' if bd is
missing, unparseable, or older than the minimum. When
`beads-bd-min-version' is nil, only verify the executable exists.
Return the cached version string, or t when the version check is off."
  (let ((program (executable-find beads-bd-executable)))
    (unless program
      (user-error "bd not found; install bd or set beads-bd-executable"))
    (if (null beads-bd-min-version)
        t
      (unless beads-cli--bd-version
        (let* ((raw (beads-cli--probe-version program))
               (ver (beads-cli--parse-version raw)))
          (unless ver
            (user-error "Could not parse bd version from: %s"
                        (let ((s (string-trim (or raw ""))))
                          (if (string-empty-p s) "(empty)" s))))
          (setq beads-cli--bd-version ver)))
      (when (version-list-< (version-to-list beads-cli--bd-version)
                            (version-to-list beads-bd-min-version))
        (user-error
         "bd %s is older than required beads-bd-min-version %s; upgrade bd"
         beads-cli--bd-version beads-bd-min-version))
      beads-cli--bd-version)))

(defun beads-cli--error-message (stderr exit)
  "Build a short error string from STDERR and EXIT code."
  (let* ((s (string-trim (or stderr "")))
         (parsed (ignore-errors (beads-cli--parse s)))
         (msg (and (listp parsed) (alist-get 'error parsed))))
    (cond
     ((and msg (stringp msg)) msg)
     ((not (string-empty-p s)) s)
     (t (format "bd exited %s" exit)))))

(defun beads-cli-call (&rest args)
  "Run bd with ARGS (after global flags). Return parsed JSON or nil.
Signal `user-error' on non-zero exit."
  (beads-cli-ensure-bd)
  (let* ((program (executable-find beads-bd-executable))
         (full (append (beads-cli--base-args) args))
         (buf (generate-new-buffer " *beads-cli*"))
         (err-file (make-temp-file "beads-cli-err-"))
         exit)
    (unless program
      (user-error "bd not found (set beads-bd-executable)"))
    (when beads-cli-debug
      (message "beads-cli: %s %s" program (mapconcat #'identity full " ")))
    (unwind-protect
        (progn
          (setq exit (apply #'call-process program nil (list buf err-file) nil full))
          (let* ((stdout (with-current-buffer buf (buffer-string)))
                 (stderr (with-temp-buffer
                           (insert-file-contents err-file)
                           (buffer-string))))
            (unless (zerop exit)
              (user-error "%s" (beads-cli--error-message
                               (if (string-empty-p stderr) stdout stderr)
                               exit)))
            (beads-cli--parse stdout)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (file-exists-p err-file) (delete-file err-file)))))

(cl-defun beads-cli-call-async (callback &rest args)
  "Run bd ARGS asynchronously. On finish call CALLBACK with (ok data-or-err).
OK is t on success; DATA-OR-ERR is parsed JSON or an error string."
  (beads-cli-ensure-bd)
  (let* ((program (executable-find beads-bd-executable))
         (full (append (beads-cli--base-args) args))
         (stdout (generate-new-buffer " *beads-cli-out*"))
         (stderr (generate-new-buffer " *beads-cli-err*")))
    (unless program
      (funcall callback nil "bd not found (set beads-bd-executable)")
      (cl-return-from beads-cli-call-async nil))
    (when beads-cli-debug
      (message "beads-cli async: %s %s" program (mapconcat #'identity full " ")))
    (make-process
     :name "beads-cli"
     :command (cons program full)
     :buffer stdout
     :stderr stderr
     :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let* ((exit (process-exit-status proc))
                (out (with-current-buffer stdout (buffer-string)))
                (err (with-current-buffer stderr (buffer-string)))
                (ok (zerop exit)))
           (unwind-protect
               (if ok
                   (condition-case e
                       (funcall callback t (beads-cli--parse out))
                     (error (funcall callback nil (error-message-string e))))
                 (funcall callback nil
                          (beads-cli--error-message
                           (if (string-empty-p err) out err) exit)))
             (when (buffer-live-p stdout) (kill-buffer stdout))
             (when (buffer-live-p stderr) (kill-buffer stderr)))))))
    t))

;;;###autoload
(defun beads-cli-list (&optional status assignee)
  "List issues. STATUS is a comma-separated string or nil."
  (let ((args (list "list")))
    (when status
      (setq args (append args (list "--status" status))))
    (when (and assignee (not (string-empty-p assignee)))
      (setq args (append args (list "--assignee" assignee))))
    (or (apply #'beads-cli-call args) '())))

(defun beads-cli--unwrap-show (payload)
  "Normalize `bd show' PAYLOAD to a single issue alist."
  (cond
   ((and (listp payload) (assq 'id payload)) payload)
   ((and (listp payload) (listp (car payload)) (assq 'id (car payload)))
    (car payload))
   ((listp payload) payload)
   (t nil)))

;;;###autoload
(defun beads-cli-show (id)
  "Show issue ID with comments and dependents."
  (beads-cli--unwrap-show
   (beads-cli-call "show" id
                   "--include-comments"
                   "--include-dependents")))

;;;###autoload
(defun beads-cli-claim (id)
  "Claim issue ID (update --claim)."
  (beads-cli-call "update" id "--claim")
  t)

;;;###autoload
(defun beads-cli-comment (id text)
  "Add TEXT as a comment on ID via `bd comment --file' (multiline-safe)."
  (let ((tmp (make-temp-file "beads-comment-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert text))
          (beads-cli-call "comment" id "--file" tmp)
          t)
      (ignore-errors (delete-file tmp)))))

(defun beads-cli-update (id &rest flags)
  "Run `bd update' on ID with FLAGS. Return parsed JSON."
  (apply #'beads-cli-call "update" id flags))

;;;###autoload
(defun beads-cli-create (title &optional description priority parent)
  "Create an issue with TITLE. Optional DESCRIPTION, PRIORITY, PARENT.
Return parsed JSON from bd."
  (let ((args (list "create" "--title" title)))
    (when (and description (not (string-empty-p description)))
      (setq args (append args (list "--description" description))))
    (when priority
      (setq args (append args (list "--priority"
                                    (if (stringp priority)
                                        priority
                                      (number-to-string priority))))))
    (when (and parent (not (string-empty-p parent)))
      (setq args (append args (list "--parent" parent))))
    (apply #'beads-cli-call args)))

;;;###autoload
(defun beads-cli-close (id &optional reason)
  "Close issue ID. Optional REASON is passed as --reason."
  (if (and reason (not (string-empty-p reason)))
      (beads-cli-call "close" id "--reason" reason)
    (beads-cli-call "close" id)))

;;;###autoload
(defun beads-cli-set-status (id status)
  "Set STATUS on issue ID via `bd update --status'."
  (beads-cli-update id "--status" status))

;;;###autoload
(defun beads-cli-set-title (id title)
  "Set TITLE on issue ID via `bd update --title'."
  (beads-cli-update id "--title" title))

;;;###autoload
(defun beads-cli-reparent (id parent-or-empty)
  "Set parent of ID to PARENT-OR-EMPTY (empty string clears)."
  (beads-cli-update id "--parent" (or parent-or-empty "")))

(provide 'beads-cli)
;;; beads-cli.el ends here
