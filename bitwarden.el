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

;=============================== custom variables ==============================

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

(defcustom bitwarden-automatic-unlock nil
  "Optional function to be called to attempt to unlock the vault.

Set this to a lamdba that will evaluate to a password. For
example, this can be the :secret plist from
`auth-source-search'."
  :group 'bitwarden
  :type 'function)

(defconst bitwarden--err-logged-in "you are not logged in")
(defconst bitwarden--err-multiple  "more than one result found")
(defconst bitwarden--err-locked    "vault is locked")

;===================================== util ====================================

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
                     bitwarden--err-multiple)
                    (t nil))))
        bitwarden--err-locked)
    bitwarden--err-logged-in))

(defun bitwarden--login-proc-filter (proc string print-message)
  "Interacts with PROC by sending line-by-line STRING.

If PRINT-MESSAGE is set then messages are printed to minibuffer."
  ;; read username if not defined
  (when (string-match "^? Email address:" string)
    (let ((user (read-string "Bitwarden email: ")))
      ;; if we are here then the user forgot to fill in this field so let's do
      ;; that now
      (setq bitwarden-user user)
      (process-send-string proc (concat bitwarden-user "\n"))))

  ;; read master password
  (when (string-match "^? Master password:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden master password: ") "\n")))

  ;; check for bad password
  (when (string-match "^Username or password is incorrect" string)
    (bitwarden--message "incorrect master password" nil print-message))

  ;; if trying to unlock, check if logged in
  (when (string-match "^You are not logged in" string)
    (bitwarden--message "cannot unlock: not logged in" nil print-message))

  ;; read the 2fa code
  (when (string-match "^? Two-step login code:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden two-step login code: ") "\n")))

  ;; check for bad code
  (when (string-match "^Login failed" string)
    (bitwarden--message "incorrect two-step code" nil print-message))

  ;; check for already logged in
  (when (string-match "^You are already logged in" string)
    (string-match "You are already logged in as \\(.*\\)\\." string)
    (bitwarden--message
     "already logged in as %s" (match-string 1 string) print-message))

  ;; success! now save the BW_SESSION into the environment so spawned processes
  ;; inherit it
  (when (string-match "^\\(You are logged in\\|Your vault is now unlocked\\)"
                      string)
    ;; set the session env variable so spawned processes inherit
    (string-match "export BW_SESSION=\"\\(.*\\)\"" string)
    (setenv "BW_SESSION" (match-string 1 string))
    (bitwarden--message
     "successfully logged in as %s" bitwarden-user print-message)))

(defun bitwarden--raw-unlock (cmd print-message)
  "Raw CMD to either unlock a vault or login.

The only difference between unlock and login is just the name of
the command and whether to pass the user.

If PRINT-MESSAGE is set then messages are printed to minibuffer."
  (when (get-process "bitwarden")
    (delete-process "bitwarden"))
  (let ((process (start-process-shell-command
                  "bitwarden"
                  nil                   ; don't use a buffer
                  (concat bitwarden-bw-executable " " cmd))))
    (set-process-filter process (lambda (proc string)
                                  (bitwarden--login-proc-filter
                                   proc string print-message)))
    ;; suppress output to the minibuffer when running this programatically
    nil))

;================================= interactive =================================

(defun bitwarden-unlock (&optional print-message)
  "Unlock bitwarden vault.

It is not sufficient to check the env variable for BW_SESSION
since that could be set yet could be expired or incorrect.

If run interactively PRINT-MESSAGE gets set and messages are
printed to minibuffer."
  (interactive "p")
  (let ((pass (when bitwarden-automatic-unlock
                (concat " " (funcall bitwarden-automatic-unlock)))))
    (bitwarden--raw-unlock (concat "unlock " pass) print-message)))

(defun bitwarden-login (&optional print-message)
  "Prompts user for password if not logged in.

If run interactively PRINT-MESSAGE gets set and messages are
printed to minibuffer."
  (interactive "p")
  (unless bitwarden-user
    (setq bitwarden-user (read-string "Bitwarden email: ")))

  (let ((pass (when bitwarden-automatic-unlock
                (concat " " (funcall bitwarden-automatic-unlock)))))
    (bitwarden--raw-unlock (concat "login " bitwarden-user pass) print-message)))

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

(defun bitwarden--message (msg args &optional print-message)
  "Print MSG using `message' and `format' with ARGS if non-nil.

PRINT-MESSAGE is an optional parameter to control whether this
method should print at all. If nil then nothing will be printed
at all.

This method will prepend 'Bitwarden: ' before each MSG as a
convenience. Also, return a value of nil so that no strings
are mistaken as a password (e.g. accidentally interpreting
'Bitwarden: error' as the password when in fact, it was an error
message but happens to be last on the method stack)."
  (when print-message
    (let ((msg (if args (format msg args) msg)))
      (message (concat "Bitwarden: " msg))))
  nil)

(defun bitwarden--handle-message (msg &optional print-message)
  "Handle return MSG of `bitwarden--auto-cmd'.

Since `bitwarden--auto-cmd' returns a list of (err-code message),
this function exists to handle that. Printing the error message
is entirely dependent on PRINT-MESSAGE (see below for more info
on PRINT-MESSAGE).

If the error code is 0, then print the password based on
PRINT-MESSAGE or just return it.

If the error code is non-zero, then print the message based on
PRINT-MESSAGE and return nil.

PRINT-MESSAGE is an optional parameter to control whether this
method should print at all. If nil then nothing will be printed
at all but password will be returned (e.g. when run
non-interactively)."
  (let* ((err (nth 0 msg))
         (pass (nth 1 msg)))
    (cond
     ((eq err 0)
      (if print-message
          (message "%s" pass)
        pass))
     (t
      (bitwarden--message "%s" pass print-message)
      nil))))

(defun bitwarden--auto-cmd (cmd &optional recursive-pass)
  "Run Bitwarden CMD and attempt to auto unlock.

If RECURSIVE-PASS is set, then treat this call as a second
attempt after trying to auto-unlock.

Returns a tuple of the error code and the error message or
password if successful."
  (let* ((res (or recursive-pass (apply 'bitwarden-runcmd cmd))))
    (cond
     ((string-match bitwarden--err-locked res)
      ;; try to unlock automatically, if possible
      (if (not bitwarden-automatic-unlock)
          (list 1 (format "error: %s" res))

        ;; only attempt a retry once; to prevent infinite recursion
        (when (not recursive-pass)
          ;; because I don't understand how emacs is asyncronous here nor
          ;; how to tell it to wait until the process is done, we do so here
          ;; manually
          (bitwarden-unlock)
          (while (get-process "bitwarden")
            (sleep-for 0.1))
          (bitwarden--auto-cmd cmd (apply 'bitwarden-runcmd cmd)))))
     ((or (string-match bitwarden--err-logged-in res)
          (string-match bitwarden--err-multiple res))
      (list 2 (format "error: %s" res)))
     (t (list 0 res)))))

;;;###autoload
(defun bitwarden-getpass (account &optional print-message)
  "Get password associated with ACCOUNT.

If run interactively PRINT-MESSAGE gets set and password is
printed to minibuffer."
  (interactive "MBitwarden account name: \np")
  (bitwarden--handle-message
   (bitwarden--auto-cmd (list "get" "password" account))
   print-message))

;;;###autoload
(defun bitwarden-search (&optional search-str)
  "Search for vault for items containing SEARCH-STR.

If run interactively PRINT-MESSAGE gets set and password is
printed to minibuffer.

Returns a vector of hashtables of the results."
  (let* ((args (and search-str (list "--search" search-str)))
         (ret (bitwarden--auto-cmd (append (list "list" "items") args)))
         (result (bitwarden--handle-message ret)))
    (when result
      (let* ((json-object-type 'hash-table)
             (json-key-type 'string)
             (json (json-read-from-string result)))
           json))))
         (result (bitwarden--handle-message ret t)))
    (when result
      (let* ((json-object-type 'hash-table)
             (json-key-type 'string)
             (json (json-read-from-string result)))
           json))))


(provide 'bitwarden)

;;; bitwarden.el ends here
