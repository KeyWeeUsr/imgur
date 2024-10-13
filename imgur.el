;;; imgur.el --- Imgur client -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, imgur, client
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
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

(defgroup imgur nil
  "Imgur client configuration."
  :group 'external
  :group 'communication)

(defcustom imgur-default-session-name
  "default"
  "Name used as an alist key for `imgur-creds'."
  :group 'imgur
  :type 'string)

(defvar imgur-creds nil
  "Credentials for all sessions in nested alists.")

;; public funcs
(defun imgur-authorize-interactive-with-session
    (base client-id client-secret session)
  "Authorize the client against BASE for SESSION.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret."
  (interactive  "sURL base: \nsClient ID: \nsClient secret: \nsSession: "))

(defun imgur-authorize-interactive (base client-id client-secret)
  "Authorize the client against BASE using `imgur-default-session-name'.
Argument CLIENT-ID Imgur application client ID.
Argument CLIENT-SECRET Imgur application client secret."
  (interactive  "sURL base: \nsClient ID: \nsClient secret: ")
  (apply #'imgur-authorize-interactive-with-session
         `(,base ,client-id ,client-secret ,imgur-default-session-name)))

(provide 'imgur)
;;; imgur.el ends here
