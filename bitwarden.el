;;; bitwarden.el --- Bitwarden command wrapper -*- lexical-binding: t -*-

;; Copyright (C) 2018  Sean Farley

;; Author: Sean Farley
;; URL: https://github.com/seanfarley/emacs-bitwarden
;; Version: 0.1.0
;; Created: 2018-09-04
;; Package-Requires: ((emacs "24.4"))
;; Keywords: extensions processes bw bitwarden

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package wraps the bitwarden command-line program.

;;; Code:

(require 'json)

(defcustom bitwarden-bw-executable (executable-find "bw")
  "The bw cli executable used by Bitwarden."
  :group 'bitwarden
  :type 'string)

(defcustom bitwarden-data-file
  (expand-file-name "~/Library/Application Support/Bitwarden CLI/data.json")
  "The bw cli executable used by Bitwarden."
  :group 'bitwarden
  :type 'string)

(defcustom bitwarden-user nil
  "Bitwarden user e-mail."
  :group 'bitwarden
  :type 'string)

(defun bitwarden-logged-in-p ()
  "Check if `bitwarden-user' is logged in.
Returns nil if not logged in."
  (let* ((json-object-type 'hash-table)
         (json-key-type 'string)
         (json (json-read-file bitwarden-data-file)))
    (gethash "__PROTECTED__key" json)))

(defun bitwarden-unlocked-p ()
  "Check if we have already set the 'BW_SESSION' environment variable."
  (and (bitwarden-logged-in-p) (getenv "BW_SESSION")))

(defun bitwarden--raw-runcmd (cmd &rest args)
  "Run bw command CMD with ARGS.
Returns a list with the first element being the exit code and the
second element being the output."
  (with-temp-buffer
    (list (apply 'call-process
                 bitwarden-bw-executable
                 nil (current-buffer) nil
                 (cons cmd args))
          (replace-regexp-in-string "\n$" ""
                                    (buffer-string)))))

(defun bitwarden-runcmd (cmd &rest args)
  "Run bw command CMD with ARGS.
This is a wrapper for `bitwarden--raw-runcmd' that also checks
for common errors."
  (if (bitwarden-logged-in-p)
      (if (bitwarden-unlocked-p)
          (let* ((ret (apply #'bitwarden--raw-runcmd cmd args))
                 (exit-code (nth 0 ret))
                 (output (nth 1 ret)))
            (if (eq exit-code 0)
                output
              (cond ((string-match "^More than one result was found." output)
                     (message "Bitwarden: more than one result found"))
                    (t
                     (message "Bitwarden: unknown error: %s" output)))))
        (message "Bitwarden: vault is locked"))
    (message "Bitwarden: you are not logged in")))

(defun bitwarden--login-proc-filter (proc string)
  "Interacts with PROC by sending line-by-line STRING."
  ;; read username if not defined
  (when (string-match "^? Email address:" string)
    (let ((user (read-string "Bitwarden email:")))
      ;; if we are here then the user forgot to fill in this field so let's do
      ;; that now
      (setq bitwarden-user user)
      (process-send-string proc (concat bitwarden-user "\n"))))

  ;; read master password
  (when (string-match "^? Master password:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden master password:") "\n")))

  ;; check for bad password
  (when (string-match "^Username or password is incorrect" string)
    (message "Bitwarden: incorrect master password"))

  ;; if trying to unlock, check if logged in
  (when (string-match "^You are not logged in" string)
    (message "Bitwarden: cannot unlock: not logged in"))

  ;; read the 2fa code
  (when (string-match "^? Two-step login code:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden two-step login code:") "\n")))

  ;; check for bad code
  (when (string-match "^Login failed" string)
    (message "Bitwarden: incorrect two-step code"))

  ;; success! now save the BW_SESSION into the environment so spawned processes
  ;; inherit it
  (when (string-match "^\\(You are logged in\\|Your vault is now unlocked\\)"
                      string)
    ;; set the session env variable so spawned processes inherit
    (string-match "export BW_SESSION=\"\\(.*\\)\"" string)
    (setenv "BW_SESSION" (match-string 1 string))
    (message
     (concat "Bitwarden: successfully logged in as " bitwarden-user))))

(defun bitwarden--raw-unlock (cmd)
  "Raw CMD to either unlock a vault or login.

The only difference between unlock and login is just the name of
the command and whether to pass the user."
  (when (get-process "bitwarden")
    (delete-process "bitwarden"))
  (let ((process (start-process-shell-command
                  "bitwarden"
                  nil                   ; don't use a buffer
                  (concat bitwarden-bw-executable " " cmd))))
    (set-process-filter process #'bitwarden--login-proc-filter)))

(defun bitwarden-unlock ()
  "Unlock bitwarden vault.
It is not sufficient to check the env variable for BW_SESSION
since that could be set yet could be expired or incorrect."
  (interactive "M")
  (bitwarden--raw-unlock "unlock"))

(defun bitwarden-login ()
  "Prompts user for password if not logged in."
  (interactive "M")
  (when (bitwarden-logged-in-p)
    (error "Bitwarden: already logged in"))
  (bitwarden--raw-unlock (concat "login " bitwarden-user)))

(defun bitwarden-lock ()
  "Lock the bw vault.  Does not ask for confirmation."
  (interactive)
  (when (bitwarden-unlocked-p)
    (setenv "BW_SESSION" nil)))

;;;###autoload
(defun bitwarden-logout ()
  "Log out bw.  Does not ask for confirmation."
  (interactive)
  (when (bitwarden-logged-in-p)
    (bitwarden-runcmd "logout")
    (bitwarden-lock)))

(provide 'bitwarden)

;;; bitwarden.el ends here
