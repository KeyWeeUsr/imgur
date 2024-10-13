;;; imgur.el --- Imgur client -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, imgur, client
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/KeyWeeUsr/imgur

;; This program is free software; you can redistribute it and/or modify
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

;; TBD

;;; Code:

(require 'subr-x)

(defgroup imgur nil
  "Imgur client configuration."
  :group 'external
  :group 'communication)

(defcustom imgur-default-session-name
  "default"
  "Name used as an alist key for `imgur-creds'."
  :group 'imgur
  :type 'string)

(defcustom imgur-log-prefix
  "imgur"
  "Log message prefix."
  :group 'imgur
  :type 'string)

;; TODO: why is "image" not in docs yet used in cURL and behaves like "file"?
(defcustom imgur-allowed-types
  '(file url base64 raw image)
  "Allowed types for upload."
  :group 'imgur
  :type '(repeat symbol))

(defvar imgur-creds nil
  "Credentials for all sessions in nested alists.")

(defvar imgur-procs nil
  "Process references for all sessions in alists.")

;; private/helper funcs
(defun imgur--server-create-sentinel (session cb)
  "Sentinel for auth listener.
Argument SESSION Session to access creds in `imgur-creds'.
Argument CB Callback."
  `(lambda (proc state)
     (when (alist-get (intern ,session) imgur-creds)
       (message "%s: (%s) Credentials obtained, closing server"
                imgur-log-prefix ,session)
       (condition-case nil
           (delete-process proc)
         (error t))
       (condition-case nil
           (progn
             (delete-process (alist-get (intern ,session) imgur-procs))
             (setf (alist-get (intern ,session) imgur-procs) nil))
         (error t))
       (when ,cb
         (funcall ,cb)))))

(defun imgur--parse-creds (raw-data)
  "Parse credentials from URL in RAW-DATA."
  (url-parse-query-string
   (base64-decode-string
    (substring
     (cadr (split-string (car (split-string raw-data "\r\n")) " ")) 1))))

(defun imgur--generate-body ()
  "Generate auth server response which redirects to itself.
Necessary to pluck creds from fragment section and insert to path
for extraction."
  (string-join
   `("<!DOCTYPE html>" "<script>"
     ,(string-join
       `("var r=new XMLHttpRequest()"
         ,(string-join '("r.open('POST'," "`http://${location.host}/"
                         "${btoa(location.hash.slice(1))}`)"))
         "r.send()" "</script>") ";")
     "You can close the window now.")))

(defun imgur--server-handler (session base client-id client-secret)
  "Sentinel for server requests.
Argument SESSION Session to access creds in `imgur-creds'.
Argument BASE URL base for API calls.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret."
  `(lambda (proc data)
     (unwind-protect
         (when (string= (substring data 0 4) "POST")
           (setf
            (alist-get 'base (alist-get (intern ,session) imgur-creds)) ,base)
           (setf (alist-get 'client-id
                            (alist-get (intern ,session) imgur-creds))
                 ,client-id)
           (setf (alist-get 'client-secret
                            (alist-get (intern ,session) imgur-creds))
                 ,client-secret)
           (dolist (item (imgur--parse-creds data))
             (setf (alist-get (intern (car item))
                              (alist-get (intern ,session) imgur-creds))
                   (cadr item))))
       (process-send-string
        proc (string-join `("HTTP/1.1 200 OK" "Connection: close"
                            ""  ;; end headers
                            ,(imgur--generate-body)) "\r\n")))
     (condition-case nil (delete-process proc) (error t))))

;; public funcs
(defun imgur-reset (prefix)
  "Reset all modifications and state to default.
Argument PREFIX Force cleaning, drop references if can't clean."
  (interactive "P")
  (dolist (session imgur-procs)
    (condition-case nil
        (progn
          (delete-process (alist-get (car session) imgur-procs))
          (setf (alist-get (car session) imgur-procs) nil))
      (error
       (message "%s: Failed to close process: %s"
                imgur-log-prefix (alist-get (car session) imgur-procs))
       (when prefix
         (message "%s: Dropping process reference: %s"
                  imgur-log-prefix (alist-get (car session) imgur-procs))
         (setf (alist-get (car session) imgur-procs) nil))))
    (unless (alist-get (car session) imgur-procs)
      (setq imgur-procs (delete session imgur-procs))))

  (dolist (session imgur-creds)
    (setf (alist-get (car session) imgur-creds) nil)
    (unless (alist-get (car session) imgur-creds)
      (setq imgur-creds (delete session imgur-creds)))))

(defun imgur-authorize (base client-id client-secret &rest args)
  "Authorize the client against BASE for SESSION.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret.

Optional argument ARGS allows specifying these keys:
* :success - (function/nil) called on successful run
* :fail - (function/nil) called on failed run
* :session - (string/`imgur-default-session-name') session name"
  (let ((success (plist-get args :success))
        (fail (plist-get args :fail))
        (session (or (plist-get args :session) imgur-default-session-name)))

    (unless (or (functionp success) (null success))
      (error "Optional :success must be a funcall-able"))
    (unless (or (functionp fail) (null fail))
      (error "Optional :fail must be a funcall-able"))

    (when (or (null base) (eq 0 (length base)))
      (let ((err (format "Bad base (%s)" base)))
        (when fail (funcall fail err))
        (user-error err)))

    (when (or (null client-id) (eq 0 (length client-id)))
      (let ((err (format "Bad client-id (%s)" client-id)))
        (when fail (funcall fail err))
        (user-error err)))

    (when (or (null client-secret) (eq 0 (length client-secret)))
      (let ((err (format "Bad client-secret (%s)" client-secret)))
        (when fail (funcall fail err))
        (user-error err)))

    (if (alist-get (intern session) imgur-creds)
        (when success (funcall success))

      (condition-case nil
          (delete-process (get-process (format "imgur-authorize-server")))
        (error nil))

      (unless noninteractive
        (read-string "Opening authorization website (press enter)..."))

      ;; TODO: allow multi-session
      ;; a) single port + route
      ;; b) multi-port + session-port/session-url pairing for redir URLs
      (setf (alist-get (intern session) imgur-procs)
            (make-network-process
             :name (format "imgur-authorize-server")
             :server t
             :host "127.0.0.1"
             :service 61626
             :family 'ipv4
             :sentinel (imgur--server-create-sentinel session success)
             :filter (imgur--server-handler
                      session base client-id client-secret)))

      (unless (alist-get (intern session) imgur-procs)
        (error "Failed creating server"))

      (browse-url
       (format "%s/oauth2/authorize?client_id=%s&response_type=%s"
               base (url-hexify-string client-id) "token")))))

(defun imgur-authorize-interactive-with-session
    (base client-id client-secret session)
  "Authorize the client against BASE for SESSION.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret."
  (interactive  "sURL base: \nsClient ID: \nsClient secret: \nsSession: ")
  (imgur-authorize base client-id client-secret :session session))

(defun imgur-authorize-interactive (base client-id client-secret)
  "Authorize the client against BASE using `imgur-default-session-name'.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret."
  (interactive  "sURL base: \nsClient ID: \nsClient secret: ")
  (apply #'imgur-authorize-interactive-with-session
         `(,base ,client-id ,client-secret ,imgur-default-session-name)))

(defun imgur-upload
    (base client-id client-secret type file title description &rest args)
  "Upload resource to Imgur.
Argument BASE URL base for API calls.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret.
Argument TYPE Resource type from `imgur-allowed-types'.
Argument FILE Path to resource.
Argument TITLE Title for resource on Imgur.
Argument DESCRIPTION Description for resource on Imgur.

Optional argument ARGS allows specifying these keys:
* :success - (function/nil) called on successful run
* :fail - (function/nil) called on failed run
* :session - (string/`imgur-default-session-name') session name"
  (let ((success (plist-get args :success))
        (fail (plist-get args :fail))
        (session (or (plist-get args :session) imgur-default-session-name)))
    (ignore file title description session)

    (unless (or (functionp success) (null success))
      (error "Optional :success must be a funcall-able"))
    (unless (or (functionp fail) (null fail))
      (error "Optional :fail must be a funcall-able"))

    (when (or (null base) (eq 0 (length base)))
      (let ((err (format "Bad base (%s)" base)))
        (when fail (funcall fail err))
        (user-error err)))

    (when (or (null client-id) (eq 0 (length client-id)))
      (let ((err (format "Bad client-id (%s)" client-id)))
        (when fail (funcall fail err))
        (user-error err)))

    (when (or (null client-secret) (eq 0 (length client-secret)))
      (let ((err (format "Bad client-secret (%s)" client-secret)))
        (when fail (funcall fail err))
        (user-error err)))

    (when (or (null type) (not (symbolp type)))
      (let ((err (format "Bad type (%s)" type)))
        (when fail (funcall fail err))
        (user-error err)))

    (unless (member type imgur-allowed-types)
      (user-error "Unsupported type '%s' specified (%S)"
                  type imgur-allowed-types))

    (let* ((url-request-method "POST")
           (url-show-status nil)
           (boundary (make-temp-name "boundary-"))
           (url-request-extra-headers
            `(("Authorization" . ,(format
                                   "Client-ID %s"
                                   (encode-coding-string client-id 'utf-8)))
              ("Content-Type" . ,(format "multipart/form-data; boundary=%s"
                                         boundary)))))
      (ignore url-request-method url-show-status url-request-extra-headers)
      (url-retrieve
       (format "%s/3/image" base)
       (lambda (status)
         (ignore status)
         (message "raw: >%s<" (with-current-buffer (current-buffer)
                                (buffer-string))))))))

(defun imgur-upload-interactive-with-session
    (type file title description session)
  "Upload resource TYPE to Imgur using passed SESSION.
Argument TYPE Resource type from `imgur-allowed-types'.
Argument FILE Path to resource.
Argument TITLE Title for resource on Imgur.
Argument DESCRIPTION Description for resource on Imgur."
  (interactive "SType: \nfFile: \nsTitle: \nsDescription: \nsSession: ")

  (if (not (alist-get (intern session) imgur-creds))
      (progn
        (message "%s: (%s) Credentials missing, obtaining"
                 imgur-log-prefix session)
        (call-interactively #'imgur-authorize-interactive))
    (let ((creds (alist-get (intern session) imgur-creds)))
      (imgur-upload
       (alist-get 'base creds)
       (alist-get 'client-id creds)
       (alist-get 'client-secret creds)
       type file title description :session session))))

(defun imgur-upload-interactive (type file title description)
  "Upload resource TYPE to Imgur using the default session.
Argument TYPE Resource type from `imgur-allowed-types'.
Argument FILE Path to resource.
Argument TITLE Title for resource on Imgur.
Argument DESCRIPTION Description for resource on Imgur."
  (interactive "SType: \nfFile: \nsTitle: \nsDescription: ")

  (apply #'imgur-upload-interactive-with-session
         `(,type ,file ,title ,description ,imgur-default-session-name)))

(defun imgur-upload-image-interactive-with-session
    (file title description session)
  "Upload image to Imgur using passed SESSION.
Argument TYPE Resource type from `imgur-allowed-types'.
Argument FILE Path to resource.
Argument TITLE Title for resource on Imgur.
Argument DESCRIPTION Description for resource on Imgur."
  (interactive "fFile: \nsTitle: \nsDescription: \nsSession: ")

  (apply #'imgur-upload-interactive-with-session
         `(image ,file ,title ,description ,session)))

(defun imgur-upload-image-interactive (file title description)
  "Upload image to Imgur using the default session.
Argument TYPE Resource type from `imgur-allowed-types'.
Argument FILE Path to resource.
Argument TITLE Title for resource on Imgur.
Argument DESCRIPTION Description for resource on Imgur."
  (interactive "fFile: \nsTitle: \nsDescription: ")

  (apply #'imgur-upload-image-interactive-with-session
         `(,file ,title ,description ,imgur-default-session-name)))

(provide 'imgur)
;;; imgur.el ends here
