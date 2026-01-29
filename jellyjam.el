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

(require 'jellyjam-api)
(require 'jellyjam-view)
(require 'jellyjam-playback)

(defgroup jellyjam nil
  "Jellyfin music client and player."
  :group 'multimedia
  :prefix "jellyjam-")

(defun jellyjam-playlist-tracks (playlist-id &optional page)
  "List tracks in playlist PLAYLIST-ID on PAGE."
  (let ((page (or page 1))
        (start-index (* (1- (or page 1)) jellyjam-max-items-per-page)))
    (jellyjam--get
      (format "/Playlists/%s/Items?startIndex=%d&limit=%d"
              playlist-id start-index jellyjam-max-items-per-page)
      (jellyjam--tracks
       (gethash "Items" response)
       page
       (lambda (p) (jellyjam-playlist-tracks playlist-id p))
       #'jellyjam-play-track))))

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
          (jellyjam-items-mode)
          (setq jellyjam--items-command #'jellyjam-playlists
                jellyjam--open-command #'jellyjam-playlist-tracks
                jellyjam--current-page page
                tabulated-list-format (vector (jellyjam--image-column-spec)
                                              '("Name" 40 t)
                                              '("Items" 8 t)
                                              '("Duration" 10 t))
                tabulated-list-entries
                (mapcar #'jellyjam--format-playlist-entry items))
          (tabulated-list-init-header)
          (tabulated-list-print t)
          (plz-run
           (dolist (entry tabulated-list-entries queue)
             (jellyjam--retrieve-thumbnail queue (car entry) buf))))
        (switch-to-buffer buf)))))

(defun jellyjam-album-tracks (album-id &optional page)
  "List tracks in album ALBUM-ID on PAGE."
  (let ((page (or page 1))
        (start-index (* (1- (or page 1)) jellyjam-max-items-per-page)))
    (jellyjam--get
      (format "/Items?parentId=%s&includeItemTypes=Audio&startIndex=%d&limit=%d"
              album-id start-index jellyjam-max-items-per-page)
      (jellyjam--tracks
       (gethash "Items" response)
       page
       (lambda (p) (jellyjam-album-tracks album-id p))
       #'jellyjam-play-track))))

(defun jellyjam-albums (&optional page)
  "List available albums on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page))
         (buf (get-buffer-create "*Jellyjam Albums*")))
    (jellyjam--get
      (format "/Items?includeItemTypes=MusicAlbum&Recursive=true&startIndex=%d&limit=%d"
              start-index jellyjam-max-items-per-page)
      (let ((items (gethash "Items" response))
            (queue (make-plz-queue :limit 4)))
        (with-current-buffer buf
          (jellyjam-items-mode)
          (setq jellyjam--items-command #'jellyjam-albums
                jellyjam--open-command #'jellyjam-album-tracks
                jellyjam--current-page page
                tabulated-list-format (vector (jellyjam--image-column-spec)
                                              '("Name" 30 t)
                                              '("Artist" 20 t)
                                              '("Duration" 10 t))
                tabulated-list-entries
                (mapcar #'jellyjam--format-album-entry items))
          (tabulated-list-init-header)
          (tabulated-list-print t)
          (plz-run
           (dolist (entry tabulated-list-entries queue)
             (jellyjam--retrieve-thumbnail queue (car entry) buf))))
        (switch-to-buffer buf)))))

(defun jellyjam-tracks (&optional page)
  "List available tracks on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page)))
    (jellyjam--get
      (format "/Items?includeItemTypes=Audio&Recursive=true&startIndex=%d&limit=%d"
              start-index jellyjam-max-items-per-page)
      (jellyjam--tracks (gethash "Items" response) page #'jellyjam-tracks
                        #'jellyjam-play-track))))

(provide 'jellyjam)

;;; jellyjam.el ends here
