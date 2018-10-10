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

(defvar bitwarden--2fa nil
  "Bitwarden private, global variable.
This variable determines if we already asked for master password
and the two-step verification was incorrect. Reported
https://github.com/dani-garcia/bitwarden_rs/issues/215")

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

  ;; this is an incorrect return from the server (bitwarden_rs), reported here:
  ;; https://github.com/dani-garcia/bitwarden_rs/issues/215
  (when (string-match "^The model state is invalid" string)
    (if bitwarden--2fa
        (message "Bitwarden: incorrect two-step code")
    (message "Bitwarden: incorrect master password"))
    (setq bitwarden--2fa nil))

  ;; read the 2fa code
  (when (string-match "^? Two-step login code:" string)
    (setq bitwarden--2fa t)
    (process-send-string
     proc (concat (read-passwd "Bitwarden two-step login code:") "\n")))

  ;; success! now save the BW_SESSION into the environment so spawned processes
  ;; inherit it
  (when (string-match "^You are logged in!" string)
    (setq bitwarden--2fa nil)
    ;; set the session env variable so spawned processes inherit
    (string-match "export BW_SESSION=\"\\(.*\\)\"" string)
    (setenv "BW_SESSION" (match-string 1 string))
    (message
     (concat "Bitwarden: successfully logged in as " bitwarden-user))))

(defun bitwarden-login ()
  "Prompts user for password if not logged in."
  (interactive "M")
  (when (bitwarden-logged-in-p)
    (error "Bitwarden: already logged in"))
  (when (get-process "bitwarden")
    (delete-process "bitwarden"))
  (setq bitwarden--2fa nil)
  (let ((process (start-process-shell-command
                  "bitwarden"
                  nil                   ; don't use a buffer
                  (concat bitwarden-bw-executable " login " bitwarden-user))))
    (set-process-filter process #'bitwarden--login-proc-filter)))

;;;###autoload
(defun bitwarden-logout ()
  "Log out bw.  Does not ask for confirmation."
  (interactive)
  (when (bitwarden-logged-in-p)
    (setenv "BW_SESSION" nil)
    (bitwarden-runcmd "logout")))

(provide 'bitwarden)

;;; bitwarden.el ends here
