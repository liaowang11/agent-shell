;;; agent-shell-mock-remote.el --- A remote host for tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A TRAMP method reaching the local host through remote syntax, so tests
;; that must cross a connection get a real one (a shell spawned with `sh')
;; without a network or a second machine.  Emacs's own tramp-tests.el
;; defines the same method for the same reason.

;;; Code:

(require 'tramp)

(defun agent-shell-tests--register-mock-method ()
  "Register the mock TRAMP method, unless it's already registered."
  (unless (assoc "mock" tramp-methods)
    (add-to-list 'tramp-methods
                 '("mock"
                   (tramp-login-program "sh")
                   (tramp-login-args (("-i")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))
                   (tramp-connection-timeout 10)))))

(defmacro agent-shell-tests--with-remote-default-directory (&rest body)
  "Run BODY with `default-directory' on the mock remote connection.

Drops the connection afterwards so each test starts from the same
state."
  (declare (indent 0) (debug t))
  `(progn
     (agent-shell-tests--register-mock-method)
     (let ((default-directory (format "/mock:%s:%s" (system-name) temporary-file-directory))
           (tramp-verbose 0)
           (tramp-allow-unsafe-temporary-files t))
       (unwind-protect
           (progn ,@body)
         (ignore-errors
           (tramp-cleanup-connection
            (tramp-dissect-file-name default-directory) nil 'keep-password))))))

(provide 'agent-shell-mock-remote)
;;; agent-shell-mock-remote.el ends here
