;;; beads-ert.el --- Offline ERT for beads.el -*- lexical-binding: t; -*-
;;
;; Sole offline test suite. Run: make ert / make verify
;; No live bd project required.

(setq debug-on-error t load-prefer-newer t)

(let* ((root (expand-file-name ".." (file-name-directory load-file-name)))
       (lisp (expand-file-name "lisp" root)))
  (add-to-list 'load-path lisp))

(require 'ert)
(require 'cl-lib)
(require 'beads)
(require 'beads-autoloads)
(require 'beads-cli)
(require 'beads-issue)
(require 'beads-faces)
(require 'beads-coltree)
(require 'beads-keymap)
(require 'beads-list)
(require 'beads-list-data)
(require 'beads-list-filter)
(require 'beads-list-follow)
(require 'beads-list-render)
(require 'beads-rel)
(require 'beads-md)
(require 'beads-detail)
(require 'beads-detail-render)
(require 'beads-actions)
(require 'beads-layout)
(require 'outline)

;;; Package / load

(ert-deftest beads-ert-modules-provide ()
  "Core modules load."
  (dolist (feat '(beads beads-list-data beads-list-filter beads-list-header
                  beads-list-follow beads-list-render beads-coltree beads-rel
                  beads-keymap beads-detail-render beads-md beads-actions))
    (should (featurep feat))))

(ert-deftest beads-ert-autoloads-bound ()
  "Public entry points are defined."
  (should (fboundp 'beads))
  (should (fboundp 'beads-here))
  (should (fboundp 'beads-show)))

;;; List

(ert-deftest beads-ert-defaults-standalone ()
  "Standalone defaults: no forced split, no auto-follow."
  (should (null beads-layout))
  (should (null beads-auto-follow)))

(ert-deftest beads-ert-tty-hide-menu-bar-default-off ()
  "TTY menu-bar hide is opt-in (no global side effect by default)."
  (should (boundp 'beads-tty-hide-menu-bar))
  (should (null beads-tty-hide-menu-bar)))

(ert-deftest beads-ert-keymap-emacs-style ()
  "Default emacs keymap style and help binding."
  (beads-keymap-rebuild 'emacs)
  (should (eq beads-keymap-style 'emacs))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-a")) #'beads-claim))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-o")) #'beads-create))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-d")) #'beads-close))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-s")) #'beads-set-status))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-t")) #'beads-set-title))
  (should (eq (lookup-key beads-mode-map (kbd "C-c C-r")) #'beads-reparent))
  (should (keymapp (lookup-key beads-list-mode-map (kbd "C-c C-f"))))
  (should (keymapp (lookup-key beads-list-mode-map (kbd "C-c C-w"))))
  (should (eq (lookup-key beads-list-mode-map (kbd "C-c C-n"))
              #'beads-list-narrow-subtree))
  (should (eq (lookup-key beads-list-mode-map (kbd "n")) #'beads-list-next-line))
  (should (null (lookup-key beads-list-mode-map (kbd "o"))))
  (should (eq (lookup-key beads-mode-map (kbd "?")) #'describe-mode))
  (should (eq (lookup-key beads-list-mode-map (kbd "?")) #'describe-mode))
  (should (eq (lookup-key beads-detail-mode-map (kbd "?")) #'describe-mode)))

(ert-deftest beads-ert-coltree-glyphs ()
  "Closed and pinned glyphs appear on formatted rows."
  (let* ((closed '((id . "c1") (title . "Done") (status . "closed")
                   (priority . 2) (issue_type . "task") (parent . "")))
         (pinned '((id . "p1") (title . "Pin") (status . "open")
                   (priority . 2) (issue_type . "task") (parent . "")
                   (pinned . t)))
         (cline (beads-coltree-format-issue closed 0 nil nil nil))
         (pline (beads-coltree-format-issue pinned 0 nil nil nil)))
    (should (string-search (beads-status-glyph "closed") cline))
    (should (string-search (beads-status-glyph "pinned") pline))))

(ert-deftest beads-ert-coltree-indent-mid ()
  "Default mid tree guide is 4-col ├── ."
  (let ((beads-tree-guide-style 'mid))
    (should (string-prefix-p "├── " (beads-coltree-indent 1 nil nil nil)))
    (should (not (featurep 'tree-widget)))))

(ert-deftest beads-ert-filter-pipeline ()
  "Filter pipeline: status, type, query, todos-only."
  (let* ((beads-list-buffer-name "*beads-ert-filt*")
         (buf (get-buffer-create beads-list-buffer-name))
         (issues
          (list
           '((id . "a") (title . "Alpha task") (status . "open")
             (priority . 1) (issue_type . "task") (parent . "")
             (description . "") (labels . ("ui")))
           '((id . "b") (title . "Beta bug") (status . "blocked")
             (priority . 0) (issue_type . "bug") (parent . "")
             (description . "has [ ] todo") (labels . ("ui")))
           '((id . "c") (title . "Closed") (status . "closed")
             (priority . 3) (issue_type . "task") (parent . "")
             (description . "") (labels . ())))))
    (unwind-protect
        (with-current-buffer buf
          (beads-list-mode)
          (setq beads-list--issues issues
                beads-list--query ""
                beads-list--status-filter nil
                beads-list--type-filter nil
                beads-list--label-filter nil
                beads-list--priority-max nil
                beads-list--todos-only nil
                beads-list--narrow-id nil
                beads-list--hide-closed-roots nil)
          (should (= 3 (length (beads-list--apply-filters issues ""))))
          (setq beads-list--status-filter "open")
          (should (= 1 (length (beads-list--apply-filters issues ""))))
          (setq beads-list--status-filter nil
                beads-list--type-filter "bug")
          (should (= 1 (length (beads-list--apply-filters issues ""))))
          (setq beads-list--type-filter nil
                beads-list--query "beta")
          (should (= 1 (length (beads-list--apply-filters issues "beta"))))
          (setq beads-list--query ""
                beads-list--todos-only t)
          (should (= 1 (length (beads-list--apply-filters issues ""))))
          (should (fboundp 'beads-list-redraw)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-ert-list-refresh-mocked ()
  "Mocked async list refresh paints rows."
  (let* ((beads-list-buffer-name "*beads-ert-list*")
         (beads-detail-buffer-name "*beads-ert-detail*")
         (beads-auto-follow nil)
         (mock-issues
          (list
           '((id . "ert-1") (title . "Mock issue") (status . "open")
             (priority . 2) (issue_type . "task") (parent . "")
             (updated_at . "2026-01-01T00:00:00Z"))))
         (async-calls nil)
         (buf (get-buffer-create beads-list-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-cli-call-async)
                   (lambda (callback &rest args)
                     (push args async-calls)
                     (funcall callback t mock-issues)
                     t)))
          (with-current-buffer buf
            (beads-list-mode)
            (setq beads-list--kind 'list
                  beads-list--preset 'all
                  beads-list--query "")
            (beads-list-refresh t)
            (should async-calls)
            (should (member "list" (car (reverse async-calls))))
            (should (null beads-list--loading))
            (should (= (length beads-list--issues) 1))
            (should (> (length beads-list--rows) 0))
            (goto-char (point-min))
            (should (equal (beads-list--id-at-point) "ert-1"))
            (should (string-search "Mock issue"
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when-let* ((db (get-buffer beads-detail-buffer-name)))
        (kill-buffer db)))))

(ert-deftest beads-ert-list-refresh-stale-ignored ()
  "Stale async callbacks do not clobber a newer refresh."
  (let* ((beads-list-buffer-name "*beads-ert-stale*")
         (beads-auto-follow nil)
         (pending nil)
         (buf (get-buffer-create beads-list-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-cli-call-async)
                   (lambda (callback &rest _args)
                     (push callback pending)
                     t)))
          (with-current-buffer buf
            (beads-list-mode)
            (setq beads-list--kind 'list
                  beads-list--preset 'all)
            (beads-list-refresh)
            (should (= (length pending) 1))
            (beads-list-refresh)
            (should (= (length pending) 2))
            (funcall (car (reverse pending)) t
                     (list '((id . "stale") (title . "Stale")
                             (status . "open") (priority . 1)
                             (issue_type . "task") (parent . "")
                             (updated_at . ""))))
            (should (null beads-list--issues))
            (funcall (car pending) t
                     (list '((id . "fresh") (title . "Fresh")
                             (status . "open") (priority . 1)
                             (issue_type . "task") (parent . "")
                             (updated_at . ""))))
            (should (= (length beads-list--issues) 1))
            (should (equal (beads-issue-id (car beads-list--issues)) "fresh"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest beads-ert-list-merge-from-show ()
  "Detail show payload merges into list stub."
  (let* ((beads-list-buffer-name "*beads-ert-merge*")
         (buf (get-buffer-create beads-list-buffer-name))
         (stub '((id . "m1") (title . "Stub") (status . "open")
                 (priority . 1) (issue_type . "task") (parent . "")
                 (description . "")))
         (full '((id . "m1") (title . "Stub") (status . "open")
                 (priority . 1) (issue_type . "task") (parent . "")
                 (description . "full body")
                 (design . "design text")
                 (acceptance_criteria . "- [ ] a"))))
    (unwind-protect
        (with-current-buffer buf
          (beads-list-mode)
          (setq beads-list--issues (list stub)
                beads-list--merged-from-show (make-hash-table :test #'equal))
          (beads-list-merge-from-show "m1" full)
          (let ((merged (car beads-list--issues)))
            (should (string= "full body" (beads-issue-description merged)))
            (should (string= "design text" (beads-issue-design merged)))
            (should (gethash "m1" beads-list--merged-from-show))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Detail MOCK 10

(defun beads-ert--sample-issue (&rest extra)
  "Minimal issue alist; EXTRA alist pairs override/add."
  (append
   extra
   '((id . "t-1")
     (title . "Sample")
     (status . "open")
     (priority . 2)
     (issue_type . "task")
     (parent . "t-parent")
     (assignee . "Ada")
     (created_by . "Ada")
     (owner . "ops")
     (labels . ("ux" "sample"))
     (acceptance_criteria . "- [ ] a\n- [x] b\n- [ ] c")
     (description . "Hello body\n\nAcceptance criteria live below.\n")
     (design . "Design notes")
     (notes . "More notes")
     (dependencies . nil)
     (dependents . nil)
     (comments . nil)
     (comment_count . 0))))

(ert-deftest beads-ert-meta-breadcrumb-title-state ()
  "MOCK 10 hero: breadcrumb, title, state cluster."
  (with-temp-buffer
    (beads-detail--render-meta (beads-ert--sample-issue))
    (goto-char (point-min))
    (should (looking-at "t-parent › t-1"))
    (forward-line 1)
    (should (looking-at "Sample"))
    (forward-line 1)
    (should (looking-at "○ open · P2 · task"))))

(ert-deftest beads-ert-meta-people-dedupe ()
  "People strip drops duplicate assignee/author."
  (should (equal (beads-detail--meta-people (beads-ert--sample-issue))
                 '("Ada" "ops")))
  (should (equal (beads-detail--meta-people
                  (beads-ert--sample-issue
                   '(assignee . "Ada")
                   '(created_by . "Bea")
                   '(owner . "Ada")))
                 '("Ada" "Bea"))))

(ert-deftest beads-ert-meta-no-acceptance-chrome ()
  "Acceptance progress is not in meta chrome."
  (with-temp-buffer
    (beads-detail--render-body (beads-ert--sample-issue))
    (goto-char (point-min))
    (should (not (re-search-forward "Acceptance: 1/3" nil t)))
    (goto-char (point-min))
    (should (re-search-forward "^\\* Acceptance" nil t))))

(ert-deftest beads-ert-body-outline-sections ()
  "Single page has outline sections including Comments."
  (with-temp-buffer
    (setq-local outline-regexp (beads-detail-outline-regexp))
    (let ((inhibit-read-only t))
      (beads-detail--render-body (beads-ert--sample-issue)))
    (goto-char (point-min))
    (should (re-search-forward "^\\* Description" nil t))
    (should (re-search-forward "^\\* Design" nil t))
    (should (re-search-forward "^\\* Acceptance" nil t))
    (should (re-search-forward "^\\* Notes" nil t))
    (should (re-search-forward "^\\* Comments" nil t))))

(ert-deftest beads-ert-links-bd-show-groups ()
  "Link partition matches bd show group keys."
  (let* ((iss
          (beads-ert--sample-issue
           `(dependencies
             . (((id . "dep-1") (dependency_type . "blocks")
                 (title . "Dep"))
                ((id . "rel-1") (dependency_type . "relates-to")
                 (title . "Rel"))
                ((id . "par") (dependency_type . "parent-child")
                 (title . "Par"))))
           `(dependents
             . (((id . "child-1") (dependency_type . "parent-child")
                 (title . "Kid"))
                ((id . "blk-1") (dependency_type . "blocks")
                 (title . "Blocked"))))))
         (parts (beads-rel-partition-for-show iss)))
    (should (alist-get 'parent parts))
    (should (alist-get 'depends-on parts))
    (should (alist-get 'children parts))
    (should (alist-get 'blocks parts))
    (should (alist-get 'related parts))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-detail--render-links iss))
      (let ((text (buffer-string)))
        (should (string-match-p "\\* Parent" text))
        (should (string-match-p "\\* Depends on" text))
        (should (string-match-p "\\* Children" text))
        (should (string-match-p "\\* Blocks" text))
        (should (string-match-p "\\* Related" text))
        (should (not (string-match-p "Depended on by" text)))))))

(ert-deftest beads-ert-outline-regexp-skips-md ()
  "Outline map is * Title headings, not bare markdown words."
  (let ((re (beads-detail-outline-regexp)))
    (should (string-match-p re "* Description"))
    (should (string-match-p re "* Acceptance"))
    (should (not (string-match-p re "Acceptance criteria live below.")))
    (should (not (string-match-p re "## Acceptance")))))

(ert-deftest beads-ert-wrap-prefix-on-body ()
  "Indented detail body sets wrap-prefix for soft-wrap."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (beads-detail-body-indent 2))
      (beads-detail--insert-padded-body "line one\nline two"))
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'wrap-prefix) "  "))))

(ert-deftest beads-ert-links-truncate-more ()
  "Link groups over max rows show a +N more button."
  (let* ((many (cl-loop for i from 1 to 45
                        collect (list (cons 'id (format "kid-%d" i))
                                      (cons 'dependency_type "parent-child")
                                      (cons 'title (format "Child %d" i)))))
         (beads-detail-max-link-rows 40))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (beads-detail--render-link-group "Children" many "child"))
      (goto-char (point-min))
      (should (re-search-forward "\\+5 more" nil t))
      (should (button-at (match-beginning 0))))))

(ert-deftest beads-ert-empty-link-fold-seed ()
  "Empty link groups are seeded collapsed."
  (let ((beads-detail--collapsed-sections nil)
        (beads-detail--expanded-empty-sections nil))
    (with-temp-buffer
      (setq-local outline-regexp (beads-detail-outline-regexp))
      (outline-minor-mode 1)
      (let ((inhibit-read-only t))
        (beads-detail--insert-outline-heading "Depends on")
        (insert "  (none)\n\n")
        (beads-detail--insert-outline-heading "Children" 2)
        (insert "  kid\n"))
      (beads-detail--seed-empty-link-folds)
      (should (member "Depends on" beads-detail--collapsed-sections)))))

;;; Comments / CLI

(ert-deftest beads-ert-cli-parse-version ()
  "Version parse helper extracts dotted versions from text or JSON."
  (should (equal (beads-cli--parse-version "bd version 1.3.1 (c1c4b642a)")
                 "1.3.1"))
  (should (equal (beads-cli--parse-version
                  "{\"version\":\"2.0.0\",\"build\":\"x\"}")
                 "2.0.0"))
  (should (equal (beads-cli--parse-version "1.0") "1.0"))
  (should (null (beads-cli--parse-version "")))
  (should (null (beads-cli--parse-version nil)))
  (should (null (beads-cli--parse-version "no version here"))))

(ert-deftest beads-ert-cli-ensure-bd-missing ()
  "ensure-bd signals user-error when executable-find fails."
  (let ((beads-cli--bd-version nil)
        (beads-bd-min-version "0.1.0"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (beads-cli-ensure-bd) :type 'user-error))))

(ert-deftest beads-ert-cli-call-invokes-ensure ()
  "beads-cli-call invokes beads-cli-ensure-bd before running bd."
  (let ((ensured nil))
    (cl-letf (((symbol-function 'beads-cli-ensure-bd)
               (lambda () (setq ensured t)))
              ((symbol-function 'executable-find)
               (lambda (_) "/bin/echo"))
              ((symbol-function 'call-process)
               (lambda (&rest _) 0)))
      (beads-cli-call "list")
      (should ensured))))

(ert-deftest beads-ert-comment-uses-file ()
  "bd comment posts via --file."
  (let ((seen nil))
    (cl-letf (((symbol-function 'beads-cli-call)
               (lambda (&rest args)
                 (setq seen args)
                 nil)))
      (beads-cli-comment "ert-1" "hello\nworld")
      (should (equal (car seen) "comment"))
      (should (equal (nth 1 seen) "ert-1"))
      (should (equal (nth 2 seen) "--file"))
      (should (stringp (nth 3 seen))))))

(ert-deftest beads-ert-cli-create-argv ()
  "beads-cli-create builds create --title argv."
  (let ((seen nil))
    (cl-letf (((symbol-function 'beads-cli-call)
               (lambda (&rest args)
                 (setq seen args)
                 '((id . "new-1")))))
      (beads-cli-create "Hello" "body" 2 "par-1")
      (should (equal seen
                     '("create" "--title" "Hello"
                       "--description" "body"
                       "--priority" "2"
                       "--parent" "par-1"))))))

(ert-deftest beads-ert-cli-close-argv ()
  "beads-cli-close builds close [--reason] argv."
  (let ((seen nil))
    (cl-letf (((symbol-function 'beads-cli-call)
               (lambda (&rest args)
                 (setq seen args)
                 nil)))
      (beads-cli-close "ert-1" "done")
      (should (equal seen '("close" "ert-1" "--reason" "done")))
      (beads-cli-close "ert-2")
      (should (equal seen '("close" "ert-2"))))))

(ert-deftest beads-ert-cli-update-status-title-parent ()
  "Status/title/reparent go through update flags."
  (let ((seen nil))
    (cl-letf (((symbol-function 'beads-cli-call)
               (lambda (&rest args)
                 (setq seen args)
                 nil)))
      (beads-cli-set-status "ert-1" "deferred")
      (should (equal seen '("update" "ert-1" "--status" "deferred")))
      (beads-cli-set-title "ert-1" "New")
      (should (equal seen '("update" "ert-1" "--title" "New")))
      (beads-cli-reparent "ert-1" "")
      (should (equal seen '("update" "ert-1" "--parent" "")))
      (beads-cli-reparent "ert-1" "par-9")
      (should (equal seen '("update" "ert-1" "--parent" "par-9"))))))

(ert-deftest beads-ert-action-close-confirms ()
  "Close requires yes-or-no-p; cancel skips CLI."
  (let ((seen nil)
        (asked nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt) (setq asked prompt) nil))
              ((symbol-function 'beads-cli-call-async)
               (lambda (_cb &rest args) (setq seen args) t))
              ((symbol-function 'beads-cli-call)
               (lambda (&rest args) (setq seen args) nil)))
      (should-error (beads-action-close "ert-1" "" nil) :type 'user-error)
      (should (string-match-p "ert-1" asked))
      (should (null seen)))))

(ert-deftest beads-ert-action-close-async-argv ()
  "Confirmed close posts close argv async."
  (let ((seen nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'beads-cli-call-async)
               (lambda (cb &rest args)
                 (setq seen args)
                 (funcall cb t nil)
                 t)))
      (beads-action-close "ert-1" "shipped"
                         (lambda (ok err) (should ok) (should (null err))))
      (should (equal seen '("close" "ert-1" "--reason" "shipped"))))))

(ert-deftest beads-ert-action-create-async-argv ()
  "Create with on-done uses async create --title."
  (let ((seen nil) (payload nil))
    (cl-letf (((symbol-function 'beads-cli-call-async)
               (lambda (cb &rest args)
                 (setq seen args)
                 (funcall cb t '((id . "new-1")))
                 t)))
      (beads-action-create "T" "D"
                          (lambda (ok data)
                            (should ok)
                            (setq payload data)))
      (should (equal seen '("create" "--title" "T" "--description" "D")))
      (should (equal (alist-get 'id payload) "new-1")))))

(ert-deftest beads-ert-action-status-title-async-argv ()
  "Set-status and set-title async argv shapes."
  (let ((seen nil))
    (cl-letf (((symbol-function 'beads-cli-call-async)
               (lambda (cb &rest args)
                 (setq seen args)
                 (funcall cb t nil)
                 t)))
      (beads-action-set-status "ert-1" "blocked" (lambda (ok _) (should ok)))
      (should (equal seen '("update" "ert-1" "--status" "blocked")))
      (beads-action-set-title "ert-1" "Hi" (lambda (ok _) (should ok)))
      (should (equal seen '("update" "ert-1" "--title" "Hi"))))))

(ert-deftest beads-ert-action-reparent-confirms-and-argv ()
  "Reparent confirms then passes --parent (including empty)."
  (let ((seen nil) (asked nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt) (setq asked prompt) t))
              ((symbol-function 'beads-cli-call-async)
               (lambda (cb &rest args)
                 (setq seen args)
                 (funcall cb t nil)
                 t)))
      (beads-action-reparent "ert-1" "" (lambda (ok _) (should ok)))
      (should (string-match-p "Clear parent" asked))
      (should (equal seen '("update" "ert-1" "--parent" "")))
      (beads-action-reparent "ert-1" "par-2" (lambda (ok _) (should ok)))
      (should (string-match-p "par-2" asked))
      (should (equal seen '("update" "ert-1" "--parent" "par-2"))))))

(ert-deftest beads-ert-bd-contract-write-flags ()
  "Contract lists create/close/update write flags."
  (should (equal (assoc "create" beads-cli-bd-contract)
                 '("create" "--title" "--description" "--priority" "--parent")))
  (should (equal (assoc "close" beads-cli-bd-contract)
                 '("close" "--reason")))
  (should (member "--status" (cdr (assoc "update" beads-cli-bd-contract))))
  (should (member "--title" (cdr (assoc "update" beads-cli-bd-contract))))
  (should (member "--parent" (cdr (assoc "update" beads-cli-bd-contract)))))

(ert-deftest beads-ert-comment-dismiss-closes-split ()
  "Comment compose dismiss deletes its window."
  (let* ((detail (get-buffer-create "*beads-ert-detail-dismiss*"))
         (compose (get-buffer-create "*beads-comment:ert-x*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer detail)
          (split-window-below)
          (other-window 1)
          (switch-to-buffer compose)
          (should (= 2 (length (window-list))))
          (beads-action--dismiss-compose compose)
          (should (= 1 (length (window-list))))
          (should (not (buffer-live-p compose)))
          (should (eq (window-buffer (selected-window)) detail)))
      (when (buffer-live-p compose) (kill-buffer compose))
      (when (buffer-live-p detail) (kill-buffer detail))
      (delete-other-windows))))

;;; Markdown

(ert-deftest beads-ert-md-unwrap-faces ()
  "Markup unwrap keeps bold/code/strike faces."
  (let ((beads-md-prefer-markdown-mode nil)
        (beads-md-hide-markup t))
    (dolist (spec '(("xx ~~gone~~ yy" "gone" beads-md-strike)
                    ("xx **loud** yy" "loud" beads-md-bold)
                    ("xx `code` yy" "code" beads-md-code)))
      (let* ((s (beads-md-string (nth 0 spec)))
             (i (string-match (nth 1 spec) s))
             (want (nth 2 spec))
             (f (and i (get-text-property i 'face s))))
        (should i)
        (should (or (eq f want) (and (listp f) (memq want f))))))))

(ert-deftest beads-ert-md-blockquote-hr-fence ()
  "Blockquote, HR, and fence markers render cleanly."
  (let ((beads-md-prefer-markdown-mode nil)
        (beads-md-hide-markup t))
    (should (string-match-p "›" (substring-no-properties
                                 (beads-md-string "> quoted"))))
    (should (string-match-p "────"
                            (beads-md-snippet "before\n---\nafter" 5 40)))
    (let ((fence (beads-md-string "before\n```\ncode\nblock\n```\nafter\n")))
      (should (not (string-search "```" fence)))
      (should (string-search "code" fence)))))

(ert-deftest beads-ert-md-frontmatter-strip ()
  "YAML frontmatter is stripped from snippets."
  (let ((snip (beads-md-snippet "---\ntitle: x\n---\nReal body here" 2 80)))
    (should (not (string-match-p "title:" snip)))
    (should (string-match-p "Real body" snip))))

;;; Data / quit

(ert-deftest beads-ert-cli-error-message ()
  "Error text prefers JSON error field, else stderr/stdout, else exit."
  (should (equal (beads-cli--error-message "{\"error\":\"nope\"}" 1) "nope"))
  ;; Callers pass stdout when stderr is empty; function sees one string.
  (should (equal (beads-cli--error-message "stderr text" 2) "stderr text"))
  (should (equal (beads-cli--error-message "{\"ok\":false}" 4) "{\"ok\":false}"))
  (should (equal (beads-cli--error-message "" 7) "bd exited 7"))
  (should (equal (beads-cli--error-message nil 3) "bd exited 3")))

(ert-deftest beads-ert-json-booleans-pinned ()
  "JSON true/false map to t / :json-false; pinned-p works."
  (should (eq (beads-cli--parse "false") :json-false))
  (should (eq (beads-cli--parse "true") t))
  (let* ((raw "{\"id\":\"p1\",\"pinned\":true,\"hooked\":false,\"title\":\"T\"}")
         (iss (beads-cli--parse raw)))
    (should (eq (alist-get 'pinned iss) t))
    (should (eq (alist-get 'hooked iss) :json-false))
    (should (beads-issue-pinned-p iss))
    (should (beads-issue-json-true-p t))
    (should (not (beads-issue-json-true-p :json-false)))
    (should (not (beads-issue-json-true-p nil)))))

(ert-deftest beads-ert-detail-quit-to-list ()
  "Paired layout: detail q returns to list and closes detail window."
  (let* ((beads-list-buffer-name "*beads-ert-quit-list*")
         (beads-detail-buffer-name "*beads-ert-quit-detail*")
         (list-buf (get-buffer-create beads-list-buffer-name))
         (detail-buf (get-buffer-create beads-detail-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer list-buf (beads-list-mode))
          (with-current-buffer detail-buf (beads-detail-mode))
          (delete-other-windows)
          (switch-to-buffer list-buf)
          (let ((beads-layout 'stacked))
            (beads-layout-ensure t))
          (select-window (get-buffer-window detail-buf t))
          (beads-detail-quit)
          (should (eq (current-buffer) list-buf))
          (should (null (get-buffer-window detail-buf t)))
          (should (get-buffer-window list-buf t)))
      (when (buffer-live-p list-buf) (kill-buffer list-buf))
      (when (buffer-live-p detail-buf) (kill-buffer detail-buf))
      (delete-other-windows))))

(ert-run-tests-batch-and-exit "^beads-ert-")
