;;; agent-shell-worktree-tests.el --- Worktree creation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for worktree creation and data directory placement.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
;; For `agent-shell-tests--with-remote-default-directory'.
(require 'agent-shell-mock-remote)

(defun agent-shell-worktree-test--git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY, returning its output."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "Git failed: %s" (buffer-string)))
      (buffer-string))))

(defun agent-shell-worktree-test--repository (directory)
  "Create a Git repository with an initial commit in DIRECTORY."
  (make-directory directory t)
  (agent-shell-worktree-test--git directory "init" "-q")
  (agent-shell-worktree-test--git
   directory "-c" "user.name=Worktree Test" "-c" "user.email=worktree@example.com"
   "commit" "--allow-empty" "-qm" "Initial commit"))

(ert-deftest agent-shell-worktree-creation-from-main-and-linked-test ()
  "Create sibling worktrees from either working tree, keeping local data local."
  (let* ((directory (make-temp-file "agent-shell-worktree test " t))
         (main (expand-file-name "main" directory))
         (linked (expand-file-name "linked" directory))
         (agent-shell-cwd-function nil)
         (agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
    (unwind-protect
        (progn
          (agent-shell-worktree-test--repository main)
          (agent-shell-worktree-test--git main "worktree" "add" "-qb" "linked" linked)
          (dolist (start (list main linked))
            (let* ((default-directory (file-name-as-directory start))
                   (name (if (equal start main) "from-main" "from-linked"))
                   (expected (expand-file-name (concat ".agent-shell/worktrees/" name) main))
                   (started-directory nil))
              (ert-info ((format "Starting from %s" start))
                (cl-letf (((symbol-function 'agent-shell-worktree--generate-name)
                           (lambda () name))
                          ((symbol-function 'read-directory-name)
                           (lambda (_prompt directory &rest _) directory))
                          ((symbol-function 'agent-shell)
                           (lambda (&rest _) (setq started-directory default-directory))))
                  (agent-shell-new-worktree-shell))
                (should (file-directory-p expected))
                (should (file-equal-p started-directory expected))
                (should (file-equal-p
                         (string-trim (agent-shell-worktree-test--git expected "rev-parse" "--show-toplevel"))
                         expected))
                (should (file-equal-p
                         (agent-shell--dot-subdir "screenshots")
                         (expand-file-name ".agent-shell/screenshots" start)))
                (should (file-equal-p
                         (agent-shell--dot-subdir "transcripts")
                         (expand-file-name ".agent-shell/transcripts" start)))))))
      (when (file-in-directory-p directory temporary-file-directory)
        (delete-directory directory t)))))

(ert-deftest agent-shell-worktree-custom-data-directory-test ()
  "Honor a custom data directory when creating a worktree."
  (let* ((directory (make-temp-file "agent-shell-worktree-custom " t))
         (main (expand-file-name "main" directory))
         (custom (expand-file-name "custom-data" directory))
         (expected (expand-file-name "worktrees/custom" custom))
         (started-directory nil)
         (agent-shell-dot-subdir-function
          (lambda (subdir) (expand-file-name subdir custom))))
    (unwind-protect
        (progn
          (agent-shell-worktree-test--repository main)
          (let ((default-directory (file-name-as-directory main)))
            (cl-letf (((symbol-function 'agent-shell-worktree--generate-name) (lambda () "custom"))
                      ((symbol-function 'read-directory-name)
                       (lambda (_prompt directory &rest _) directory))
                      ((symbol-function 'agent-shell)
                       (lambda (&rest _) (setq started-directory default-directory))))
              (agent-shell-new-worktree-shell)))
          (should (file-equal-p started-directory expected))
          (should (file-directory-p expected)))
      (when (file-in-directory-p directory temporary-file-directory)
        (delete-directory directory t)))))

(ert-deftest agent-shell-worktree-bare-parent-keeps-current-root-test ()
  "Keep the current root when there is no main working tree."
  (let* ((directory (make-temp-file "agent-shell-worktree-bare " t))
         (main (expand-file-name "main" directory))
         (bare (expand-file-name "bare.git" directory))
         (linked (expand-file-name "linked" directory)))
    (unwind-protect
        (progn
          (agent-shell-worktree-test--repository main)
          (agent-shell-worktree-test--git directory "clone" "--bare" "-q" main bare)
          (agent-shell-worktree-test--git bare "worktree" "add" "--detach" "-q" linked "HEAD")
          (let ((default-directory (file-name-as-directory linked)))
            (should (file-equal-p (agent-shell-worktree--main-worktree-root) linked))))
      (when (file-in-directory-p directory temporary-file-directory)
        (delete-directory directory t)))))

(ert-deftest agent-shell-worktree-old-git-falls-back-to-current-root-test ()
  "Fall back to the current root when `git worktree list -z' fails."
  (let* ((directory (make-temp-file "agent-shell-worktree-old-git " t))
         (main (expand-file-name "main" directory))
         (linked (expand-file-name "linked" directory))
         (process-file (symbol-function 'process-file)))
    (unwind-protect
        (progn
          (agent-shell-worktree-test--repository main)
          (agent-shell-worktree-test--git main "worktree" "add" "-qb" "linked" linked)
          (let ((default-directory (file-name-as-directory linked)))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (program infile buffer display &rest args)
                         (if (member "-z" args)
                             129
                           (apply process-file program infile buffer display args)))))
              (should (file-equal-p (agent-shell-worktree--main-worktree-root) linked)))))
      (when (file-in-directory-p directory temporary-file-directory)
        (delete-directory directory t)))))

(ert-deftest agent-shell-worktree--main-worktree-root-local-test ()
  "Test the root of a local repository is the directory it lives in."
  (let* ((directory (make-temp-file "agent-shell-worktree-root-local " t))
         (repo (file-truename (expand-file-name "repo" directory))))
    (unwind-protect
        (progn
          (agent-shell-worktree-test--repository repo)
          (let ((default-directory (file-name-as-directory repo)))
            (should (file-equal-p (agent-shell-worktree--main-worktree-root) repo))))
      (delete-directory directory t))))

(ert-deftest agent-shell-worktree--main-worktree-root-remote-test ()
  "Test the root of a remote repository names the host it lives on.

git runs on that host and prints a path in its own terms, which isn't a
file name this machine can open."
  (agent-shell-tests--with-remote-default-directory
    (let* ((directory (make-temp-file "agent-shell-worktree-root-remote " t))
           (repo (file-truename (expand-file-name "repo" directory)))
           (remote-repo (file-name-as-directory
                         (concat (file-remote-p default-directory) repo))))
      (unwind-protect
          (progn
            (agent-shell-worktree-test--repository repo)
            (let* ((default-directory remote-repo)
                   (root (agent-shell-worktree--main-worktree-root)))
              (should (equal (file-remote-p root) (file-remote-p remote-repo)))
              (should (file-directory-p (expand-file-name ".git" root)))))
        (delete-directory directory t)))))

(ert-deftest agent-shell-new-worktree-shell-creates-remote-worktree-test ()
  "Test a worktree for a remote repository is created on that host.

The path handed to git named the host as well as the directory, which
git on that host can't resolve."
  (agent-shell-tests--with-remote-default-directory
    (let* ((directory (make-temp-file "agent-shell-worktree-remote " t))
           (repo (file-truename (expand-file-name "repo" directory)))
           (remote-repo (file-name-as-directory
                         (concat (file-remote-p default-directory) repo)))
           (worktree-path (concat remote-repo "worktree"))
           (started-in nil))
      (unwind-protect
          (progn
            (agent-shell-worktree-test--repository repo)
            (let ((default-directory remote-repo))
              (cl-letf (((symbol-function 'read-directory-name)
                         (lambda (&rest _args) worktree-path))
                        ((symbol-function 'agent-shell)
                         (lambda (&rest _args) (setq started-in default-directory))))
                (agent-shell-new-worktree-shell))
              (should (file-directory-p worktree-path))
              (should (file-exists-p (expand-file-name ".git" worktree-path)))
              (should (equal started-in worktree-path))))
        (delete-directory directory t)))))

(ert-deftest agent-shell-new-worktree-shell-refuses-a-path-on-another-host-test ()
  "Test a worktree path edited onto another host is refused before git runs.

git runs on the repository's host and creates the worktree there, so a
path typed for this machine (say `~/wt/foo') made a worktree on the
remote host while the check for it looked here and reported failure."
  (agent-shell-tests--with-remote-default-directory
    (let* ((directory (make-temp-file "agent-shell-worktree-other-host " t))
           (repo (file-truename (expand-file-name "repo" directory)))
           (remote-repo (file-name-as-directory
                         (concat (file-remote-p default-directory) repo)))
           (local-path (expand-file-name "worktree" repo))
           (started nil))
      (unwind-protect
          (progn
            (agent-shell-worktree-test--repository repo)
            (let ((default-directory remote-repo))
              (cl-letf (((symbol-function 'read-directory-name)
                         (lambda (&rest _args) local-path))
                        ((symbol-function 'agent-shell)
                         (lambda (&rest _args) (setq started t))))
                (should (string-match-p
                         "same host"
                         (cadr (should-error (agent-shell-new-worktree-shell)
                                             :type 'user-error)))))
              (should-not started)
              (should-not (file-exists-p local-path))))
        (delete-directory directory t)))))

(provide 'agent-shell-worktree-tests)
;;; agent-shell-worktree-tests.el ends here
