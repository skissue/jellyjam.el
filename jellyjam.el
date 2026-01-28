;;; jellyjam.el --- Jellyfin music player -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/skissue/gxy
;; Keywords: multimedia


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Jellyfin music client and player utilizing mpv.

;;; Code:

(require 'plz)

(defconst jellyjam--client-name "jellyjam"
  "Client name sent to Jellyfin server.")

(defconst jellyjam--client-version "0.0.1"
  "Client version sent to Jellyfin server.")

(defconst jellyjam--device-name "Emacs"
  "Device name sent to Jellyfin server.")

(defvar jellyjam--sessions nil
  "List of known Jellyfin sessions.
Each session is a plist with :server, :user-id, :access-token,
:username.")

(defvar jellyjam--active-session nil
  "Plist containing the currently active session info.")

(defun jellyjam--auth-header (&optional token)
  "Generate the X-Emby-Authorization header value.
If TOKEN is provided, include it in the header."
  ;; NOTE plz.el currently cannot handle quotes in headers, so do not quote
  ;; these values.
  (format "MediaBrowser Client=%s, Device=%s, DeviceId=%s, Version=%s%s"
          jellyjam--client-name
          jellyjam--device-name
          (system-name)
          jellyjam--client-version
          (if token (format ", Token=%s" token) "")))

(defun jellyjam-authenticate (server user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
Save the session in `jellyjam--sessions'."
  (interactive
   (list (read-string "Jellyfin server URL: ")
         (read-string "Username: ")
         (read-passwd "Password: ")))
  (let ((url (concat (string-remove-suffix "/" server) "/Users/AuthenticateByName")))
    (plz 'post url
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(jellyjam--auth-header)))
      :body (json-serialize `(:Username ,user :Pw ,pass))
      :as #'json-parse-buffer
      :then (lambda (response)
              (let* ((access-token (gethash "AccessToken" response))
                     (user-data (gethash "User" response))
                     (user-id (gethash "Id" user-data))
                     (session `(:server ,(string-remove-suffix "/" server)
                                        :user-id ,user-id
                                        :access-token ,access-token
                                        :username ,user)))
                (push session jellyjam--sessions)
                (setq jellyjam--active-session session)
                (message "Authenticated as %s on %s" user server)))
      :else (lambda (err)
              (message "Authentication failed: %S" err)))))

(provide 'jellyjam)

;;; jellyjam.el ends here
