;;; jellyjam.el --- Jellyfin music player -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
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

(defgroup jellyjam nil
  "Jellyfin music client and player."
  :group 'multimedia
  :prefix "jellyjam-")

(defcustom jellyjam-thumbnail-size '(64 . 64)
  "Thumbnail size (width . height)."
  :type '(cons integer integer))

(defcustom jellyjam-max-items-per-page 100
  "Maximum items to list per page."
  :type 'integer)

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

(defmacro jellyjam--get (endpoint &rest then)
  "Make authenticated GET request to ENDPOINT, evaluating THEN on success.
ENDPOINT is relative to the server URL. THEN is evaluated with the
response data bound to `response'."
  (declare (indent defun))
  `(progn
     (unless jellyjam--active-session
       (error "No active Jellyfin session"))
     (let ((server (plist-get jellyjam--active-session :server))
           (token (plist-get jellyjam--active-session :access-token)))
       (plz 'get (concat server ,endpoint)
         :headers `(("Authorization" . ,(jellyjam--auth-header token)))
         :as #'json-parse-buffer
         :then (lambda (response) ,@then)
         :else (lambda (err) (message "Request failed: %S" err))))))

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

(defun jellyjam--image-url (id)
  "Return the image URL for item ID."
  (format "%s/Items/%s/Images/Primary?maxWidth=%d&maxHeight=%d"
          (plist-get jellyjam--active-session :server)
          id
          (car jellyjam-thumbnail-size) (cdr jellyjam-thumbnail-size)))

(defun jellyjam--retrieve-thumbnail (queue id buffer)
  "Retrieve thumbnail for item ID and display in BUFFER using QUEUE."
  (plz-queue queue 'get (jellyjam--image-url id)
    :as 'binary
    :then (lambda (data)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (with-silent-modifications
                  (save-excursion
                    (goto-char (point-min))
                    (when (search-forward (format "[[%s]]" id) nil t)
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert-image (create-image data nil :data))))))))
    :else (lambda (err)
            (message "Error fetching thumbnail for %s: %S" id err))))

(defun jellyjam--format-duration (ticks)
  "Format TICKS (100-nanosecond units) as human-readable duration."
  (if (or (null ticks) (zerop ticks))
      ""
    (let* ((total-seconds (/ ticks 10000000))
           (hours (/ total-seconds 3600))
           (minutes (/ (mod total-seconds 3600) 60))
           (seconds (mod total-seconds 60)))
      (if (> hours 0)
          (format "%d:%02d:%02d" hours minutes seconds)
        (format "%d:%02d" minutes seconds)))))

(defvar-local jellyjam--current-page 1
  "Current page number in collection buffer.")

(defvar-local jellyjam--collection-command nil
  "Command providing the collections for the current buffer.")

(defun jellyjam-collections-next-page ()
  "Go to next page of collections."
  (interactive)
  (funcall jellyjam--collection-command (1+ jellyjam--current-page)))

(defun jellyjam-collections-prev-page ()
  "Go to previous page of collections."
  (interactive)
  (if (> jellyjam--current-page 1)
      (funcall jellyjam--collection-command (1- jellyjam--current-page))
    (user-error "No previous page")))

(define-derived-mode jellyjam-collections-mode tabulated-list-mode "Jellyjam Playlists"
  "Major mode for displaying Jellyfin collections.
Used for albums and playlists."
  (setq tabulated-list-format
        (vector (list " " (ceiling (/ (float (car jellyjam-thumbnail-size))
                                      (frame-char-width)))
                      nil)
                '("Name" 40 t)
                '("Items" 8 t)
                '("Duration" 10 t)))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (local-set-key (kbd "N") #'jellyjam-collections-next-page)
  (local-set-key (kbd "P") #'jellyjam-collections-prev-page))

(defun jellyjam--format-playlist-entry (playlist)
  "Format PLAYLIST hash-table as a tabulated-list entry."
  (let ((id (gethash "Id" playlist))
        (name (or (gethash "Name" playlist) "Untitled"))
        (count (or (gethash "ChildCount" playlist) 0))
        (ticks (gethash "RunTimeTicks" playlist)))
    (list id (vector (format "[[%s]]" id)
                     name
                     (number-to-string count)
                     (jellyjam--format-duration ticks)))))

(defun jellyjam-playlists (&optional page)
  "List available playlists on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page))
         (buf (get-buffer-create "*Jellyjam Playlists*")))
    (jellyjam--get
      (format "/Items?includeItemTypes=Playlist&Recursive=true&startIndex=%d&limit=%d"
              start-index jellyjam-max-items-per-page)
      (let ((items (gethash "Items" response))
            (queue (make-plz-queue :limit 4)))
        (with-current-buffer buf
          (jellyjam-collections-mode)
          (setq jellyjam--collection-command #'jellyjam-playlists
                jellyjam--current-page page
                tabulated-list-entries
                (mapcar #'jellyjam--format-playlist-entry items))
          (tabulated-list-print t)
          (plz-run
           (dolist (entry tabulated-list-entries queue)
             (jellyjam--retrieve-thumbnail queue (car entry) buf))))
        (switch-to-buffer buf)))))

(provide 'jellyjam)

;;; jellyjam.el ends here
