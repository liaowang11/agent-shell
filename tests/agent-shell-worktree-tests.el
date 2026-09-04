;;; agent-shell-worktree-tests.el --- Tests for agent-shell-worktree -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-worktree)
;; For `agent-shell-tests--with-remote-default-directory'.
(require 'agent-shell-tests)

;;; Code:

(defun agent-shell-worktree-tests--make-repo ()
  "Create a git repository holding one commit and return its directory."
  (let ((directory (file-truename (make-temp-file "agent-shell-worktree-repo" t))))
    (let ((default-directory directory))
      (dolist (command '("git init -q -b main ."
                         "git config user.email tests@example.com"
                         "git config user.name Tests"
                         "git commit -q --allow-empty -m first"))
        (shell-command-to-string command)))
    directory))

(ert-deftest agent-shell-worktree--git-repo-root-local-test ()
  "Test the root of a local repository is the directory it lives in."
  (let ((repo (agent-shell-worktree-tests--make-repo)))
    (unwind-protect
        (let ((default-directory repo))
          (should (equal (agent-shell-worktree--git-repo-root)
                         (directory-file-name repo))))
      (delete-directory repo t))))

(ert-deftest agent-shell-worktree--git-repo-root-remote-test ()
  "Test the root of a remote repository names the host it lives on.

git runs on that host and prints a path in its own terms, which isn't a
file name this machine can open."
  (agent-shell-tests--with-remote-default-directory
    (let* ((repo (agent-shell-worktree-tests--make-repo))
           (remote-repo (file-name-as-directory
                         (concat (file-remote-p default-directory) repo))))
      (unwind-protect
          (let* ((default-directory remote-repo)
                 (root (agent-shell-worktree--git-repo-root)))
            (should (equal (file-remote-p root) (file-remote-p remote-repo)))
            (should (file-directory-p (expand-file-name ".git" root))))
        (delete-directory repo t)))))

(ert-deftest agent-shell-new-worktree-shell-creates-remote-worktree-test ()
  "Test a worktree for a remote repository is created on that host.

The path handed to git named the host as well as the directory, which
git on that host can't resolve."
  (agent-shell-tests--with-remote-default-directory
    (let* ((repo (agent-shell-worktree-tests--make-repo))
           (remote-repo (file-name-as-directory
                         (concat (file-remote-p default-directory) repo)))
           (worktree-path (concat remote-repo "worktree"))
           (started-in nil))
      (unwind-protect
          (let ((default-directory remote-repo))
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _args) worktree-path))
                      ((symbol-function 'agent-shell)
                       (lambda (&rest _args) (setq started-in default-directory))))
              (agent-shell-new-worktree-shell))
            (should (file-directory-p worktree-path))
            (should (file-exists-p (expand-file-name ".git" worktree-path)))
            (should (equal started-in worktree-path)))
        (delete-directory repo t)))))

(provide 'agent-shell-worktree-tests)
;;; agent-shell-worktree-tests.el ends here
