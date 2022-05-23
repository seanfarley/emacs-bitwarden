;;; bitwarden.el --- Bitwarden command wrapper -*- lexical-binding: t -*-

;; Copyright (C) 2018  Sean Farley

;; Author: Sean Farley
;; URL: https://github.com/seanfarley/emacs-bitwarden
;; Version: 0.1.3
;; Created: 2018-09-04
;; Package-Requires: ((emacs "25.1"))
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

(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tree-widget)

;=============================== custom variables ==============================

(defgroup bitwarden nil
  "Bitwarden functions and settings."
  :group 'external
  :tag "bitwarden"
  :prefix "bitwarden-")

(defcustom bitwarden-bw-executable (executable-find "bw")
  "The bw cli executable used by Bitwarden."
  :group 'bitwarden
  :type 'string)

(defcustom bitwarden-data-file
  (expand-file-name "Bitwarden CLI/data.json"
                    (cond
                     ((getenv "BITWARDENCLI_APPDATA_DIR")
                      (getenv "BITWARDENCLI_APPDATA_DIR"))
                     ((eq system-type 'darwin)
                      "~/Library/Application Support")
                     ((eq system-type 'windows-nt)
                      (getenv "APPDATA"))
                     ((getenv "XDG_CONFIG_HOME")
                      (getenv "XDG_CONFIG_HOME"))
                     (t
                      "~/.config")))
  "The bw data file used by Bitwarden."
  :group 'bitwarden
  :type 'string)

(defcustom bitwarden-user nil
  "Bitwarden user e-mail."
  :group 'bitwarden
  :type 'string)

(defcustom bitwarden-automatic-unlock nil
  "Optional function to be called to attempt to unlock the vault.

Set this to a function that will evaluate to a password. For
example, this can be the :secret plist from
`auth-source-search'."
  :group 'bitwarden
  :type 'function)

(defcustom bitwarden-api-secret-key nil
  "Optional function to be called to return API secret key.

Set this to a function that will evaluate to a string (the API secret key)."
  :group 'bitwarden
  :type 'function)

(defcustom bitwarden-api-client-id nil
  "Optional function to be called to return the API client id..

Set this to a function that will evaluate to a string (the API client id)."
  :group 'bitwarden
  :type 'function)


(defconst bitwarden--err-logged-in "you are not logged in")
(defconst bitwarden--err-multiple  "more than one result found")
(defconst bitwarden--err-locked    "vault is locked")

;===================================== util ====================================

(defun bitwarden-logged-in-p ()
  "Check if `bitwarden-user' is logged in.
Returns nil if not logged in."
  (let* ((ret (apply #'bitwarden--raw-runcmd "login" '("--check")))
         (exit-code (nth 0 ret)))
    (eq exit-code 0)))

(defun bitwarden-unlocked-p ()
  "Check if `bitwarden-user' is loged in.
Returns nil if not unlocked."
  (let* ((ret (apply #'bitwarden--raw-runcmd "unlock" '("--check")))
         (exit-code (nth 0 ret)))
    (eq exit-code 0)))

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
  (make-process :name "bitwarden"
                :buffer nil
                :connection-type 'pipe
                :command (append (list bitwarden-bw-executable)
                                 cmd)
                :filter (lambda (proc string)
                          (bitwarden--login-proc-filter
                           proc string print-message)))

  ;; suppress output to the minibuffer when running this programatically
  nil)

;================================= interactive =================================

(defun bitwarden-unlock (&optional print-message)
  "Unlock bitwarden vault.

It is not sufficient to check the env variable for BW_SESSION
since that could be set yet could be expired or incorrect.

If run interactively PRINT-MESSAGE gets set and messages are
printed to minibuffer."
  (interactive "p")
  (let ((pass (if bitwarden-automatic-unlock
                  (funcall bitwarden-automatic-unlock)
                "")))
    (bitwarden--raw-unlock (list "unlock" pass) print-message)))

;;;###autoload
(defun bitwarden-login (&optional print-message)
  "Prompt user for password if not logged in.

If run interactively PRINT-MESSAGE gets set and messages are
printed to minibuffer."
  (interactive "p")
  (if (and bitwarden-api-client-id bitwarden-api-secret-key)
      (progn
        (setenv "BW_CLIENTID" (funcall bitwarden-api-client-id))
        (setenv "BW_CLIENTSECRET" (funcall bitwarden-api-secret-key))
        (bitwarden--raw-unlock (list "login") print-message))
    (unless bitwarden-user
      (setq bitwarden-user (read-string "Bitwarden email: ")))
    (let ((pass (when bitwarden-automatic-unlock
                  (funcall bitwarden-automatic-unlock))))
      (bitwarden--raw-unlock (list "login" bitwarden-user pass) print-message))))

(defun bitwarden-lock ()
  "Lock the bw vault.  Does not ask for confirmation."
  (interactive)
  (when (bitwarden-unlocked-p)
    (setenv "BW_SESSION" nil)))

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

Returns a vector of hashtables of the results."
  (let* ((args (and search-str (list "--search" search-str)))
         (ret (bitwarden--auto-cmd (append (list "list" "items") args)))
         (result (bitwarden--handle-message ret)))
    (when result
      (let* ((json-object-type 'hash-table)
             (json-key-type 'string)
             (json (json-read-from-string result)))
           json))))

(defun bitwarden-search-filter-username (accounts &optional username)
  "Filter results of `bitwarden-search' ACCOUNTS by USERNAME.

ACCOUNTS can be the results of `bitwarden-search' or a string to
search which will call `bitwarden-search' as a convenience."
  (let* ((accounts (if (vectorp accounts)
                       accounts (bitwarden-search accounts)))
         ;; filter out matches that are not logins
         (accounts (seq-filter (lambda (elt) (gethash "login" elt)) accounts)))
    (if (and (stringp username) (not (string= username "")))
        (seq-filter (lambda (elt)
                      (when-let ((login (gethash "login" elt)))
                        (string= (gethash "username" login) username)))
                    accounts)
      accounts)))

(defun bitwarden-folders ()
  "List bitwarden folders."
  (let* ((ret (bitwarden--auto-cmd (list "list" "folders")))
         (result (bitwarden--handle-message ret)))
    (when result
      (let* ((json-object-type 'hash-table)
             (json-key-type 'string)
             (json (json-read-from-string result)))
        json))))

(defun bitwarden-sync ()
  "Sync local store with server."
  (interactive)
  (let ((res (bitwarden--auto-cmd (list "sync"))))
    (message (nth 1 res))))

;================================= auth-source =================================

(defun bitwarden-auth-source-search (&rest spec)
  "Search Bitwarden according to SPEC.
See `auth-source-search' for a description of the plist SPEC."
  (let* ((host (plist-get spec :host))
         (max (plist-get spec :max))
         (user (plist-get spec :user))
         (res (mapcar #'bitwarden-auth-source--build-result
                      (bitwarden-search-filter-username host user))))
    (seq-take res max)))

(defun bitwarden-auth-source--build-result (elt)
  "Build a auth-source result for ELT.

This is meant to be used by `mapcar' for the results from
`bitwarden-search-filter-username'."
  (let* ((host (gethash "name" elt))
         (login (gethash "login" elt)) ;; always present since
                                       ;; `bitwarden-search-filter-username'
                                       ;; tests for it
         (user (gethash "username" login))
         (pass (gethash "password" login)))
    `(:host ,host
      :user ,user
      :secret (lambda () ,pass))))

(defvar bitwarden-auth-source-backend
  (auth-source-backend :type 'bitwarden
                       :source "." ;; not used
                       :search-function #'bitwarden-auth-source-search)
  "Auth-source backend variable for Bitwarden.")

(defun bitwarden-auth-source-backend-parse (entry)
  "Create auth-source backend from ENTRY."
  (when (eq entry 'bitwarden)
    (auth-source-backend-parse-parameters entry bitwarden-auth-source-backend)))

;; advice to add custom auth-source function
(if (boundp 'auth-source-backend-parser-functions)
    (add-hook 'auth-source-backend-parser-functions
              #'bitwarden-auth-source-backend-parse)
  (advice-add 'auth-source-backend-parse
              :before-until #'bitwarden-auth-source-backend-parse))

;;;###autoload
(defun bitwarden-auth-source-enable ()
  "Enable Bitwarden auth-source by adding it to `auth-sources'."
  (interactive)
  (add-to-list 'auth-sources 'bitwarden)
  (auth-source-forget-all-cached)
  (message "Bitwarden: auth-source enabled"))

;================================= widget utils ================================

(defun bitwarden-list-next ()
  "Move to the next item."
  (interactive)
  (forward-line)
  (beginning-of-line)
  (widget-forward 1))

(defun bitwarden-list-prev ()
  "Move to the previous item."
  (interactive)
  (widget-backward 2)
  (beginning-of-line)
  (widget-forward 1))

;; bitwarden-list-dialog-mode
(defvar bitwarden-list-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'bitwarden-list-next)
    (define-key map "p" 'bitwarden-list-prev)
    (define-key map "q" 'bitwarden-list-cancel-dialog)
    map)
  "Keymap used in recentf dialogs.")

(define-derived-mode bitwarden-list-dialog-mode nil "bitwarden-list-dialog"
  "Major mode of recentf dialogs.

\\{bitwarden-list-dialog-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defsubst bitwarden-list-all-get-item-at-pos ()
  "Get hashtable from widget at current pos in dialog widget."
  (let ((widget (get-char-property (point) 'button)))
    (widget-value widget)))

(defsubst bitwarden-list-all-make-spaces (spaces)
  "Create a string with SPACES number of whitespaces."
  (mapconcat 'identity (make-list spaces " ") ""))

(defsubst bitwarden-pad-to-width (item width)
  "Create a string with ITEM padded to WIDTH."
  (if (= (length item) width)
      item
    (if (>= (length item) width)
        (concat (substring item 0 (- width 1)) "…")
      (concat item (bitwarden-list-all-make-spaces (- width (length item)))))))

;================================ widget actions ===============================

;; Dialog settings and actions
(defun bitwarden-list-cancel-dialog (&rest _ignore)
  "Cancel the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer))
  (bitwarden--message "dialog canceled" nil t))

(defun bitwarden-list-all-kill-ring-save (&optional widget-item)
  "Bitwarden `kill-ring-save', insert password to kill ring.

If WIDGET-ITEM is not supplied then look for the widget at the
current point."
  (interactive)
  (let* ((item (or widget-item
                   (bitwarden-list-all-get-item-at-pos)))
         (type (gethash "type" item))
         (login (gethash "login" item)))
    (if (not (eq type 1))
        (bitwarden--message "error: not a login item" nil t)
      (kill-new (gethash "password" login))
      (message "Password added to kill ring"))))

(defun bitwarden-list-all-item-action (widget &rest _ignore)
  "Do action to element associated with WIDGET's value.
IGNORE other arguments."
  (bitwarden-list-all-kill-ring-save (widget-value widget))
  (kill-buffer (current-buffer)))

;=================================== widgets ===================================

(defmacro bitwarden-list-dialog (name &rest forms)
  "Show a dialog buffer with NAME, setup with FORMS."
  (declare (indent 1) (debug t))
  `(with-current-buffer (get-buffer-create ,name)
     ;; Cleanup buffer
     (let ((inhibit-read-only t)
           (ol (overlay-lists)))
       (mapc 'delete-overlay (car ol))
       (mapc 'delete-overlay (cdr ol))
       (erase-buffer))
     (bitwarden-list-dialog-mode)
     ,@forms
     (widget-setup)
     (switch-to-buffer (current-buffer))))

(defsubst bitwarden-list-all-make-element (item)
  "Create a new cons list from ITEM."
  (let* ((folder-id (gethash "folderId" item))
         (login-item (gethash "login" item)))
    (cons folder-id
          (list (cons (concat
                       (bitwarden-pad-to-width (gethash "name" item) 40)
                       (bitwarden-pad-to-width
                        (if login-item (gethash "username" login-item) "")
                        32)
                       (format-time-string
                        "%Y-%m-%d %T"
                        (date-to-time (bitwarden-pad-to-width
                                       (gethash "revisionDate" item) 24))))
                      item)))))

(defun bitwarden-list-all-tree (key val)
  "Return a `tree-widget' of folders.

Creates a widget with text KEY and items VAL."
  ;; Represent a sub-menu with a tree widget
  `(tree-widget
    :open t
    :match ignore
    :node (item :tag ,key
                :sample-face bold
                :format "%{%t%}\n")
    ,@(mapcar 'bitwarden-list-all-item val)))

(defun bitwarden-list-all-item (pass-element)
  "Return a widget to display PASS-ELEMENT in a dialog buffer."

  ;; Represent a single file with a link widget
  `(link :tag ,(car pass-element)
         :button-prefix ""
         :button-suffix ""
         :button-face default
         :format "%[%t\n%]"
         :help-echo ,(concat "Viewing item " (gethash "id" (cdr pass-element)))
         :action bitwarden-list-all-item-action
         ,(cdr pass-element)))

(defun bitwarden-list-all-items (items)
  "Return a list of widgets to display ITEMS in a dialog buffer."
  (let* ((folders (mapcar (lambda (e)
                            (cons
                             (gethash "id" e)
                             (gethash "name" e)))
                          (bitwarden-folders)))
         (hash (make-hash-table :test 'equal)))

  ;; create hash table where the keys are the folders and each value is a list
  ;; of the password items
  (dolist (x (mapcar 'bitwarden-list-all-make-element items))
    (let* ((folder-id (car x))
           (key (cdr (assoc folder-id folders)))
           (val (cdr x))
           (klist (gethash key hash)))
      (puthash key (append klist val) hash)))

  (mapcar (lambda (key)
            (bitwarden-list-all-tree key (gethash key hash)))
          (sort (hash-table-keys hash) #'string<))))

;;;###autoload
(defun bitwarden-list-all ()
  "Show a dialog, listing all entries associated with `bitwarden-user'.
If optional argument GROUP is given, only entries in GROUP will be listed."
  (interactive)
  (if (bitwarden-unlocked-p)
      (bitwarden-list-dialog "*bitwarden-list*"

        ;; Use a L&F that looks like the recentf menu.
        (tree-widget-set-theme "folder")

        (apply 'widget-create
               `(group
                 :indent 0
                 :format "%v\n"
                 ,@(bitwarden-list-all-items
                    (bitwarden-search))))

        (widget-create
         'push-button
         :notify 'bitwarden-list-cancel-dialog
         "Cancel")
        (goto-char (point-min)))
    (bitwarden--message "vault not unlocked!" nil t)))

(provide 'bitwarden)

;;; bitwarden.el ends here
