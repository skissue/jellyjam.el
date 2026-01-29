;;; jellyjam.el --- Jellyfin music player -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (plz "0.9.1"))
;; Homepage: https://github.com/skissue/jellyjam.el
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
        (format "/Playlists/%s/Items" playlist-id)
        `(:startIndex ,start-index :limit ,jellyjam-max-items-per-page)
      (jellyjam--display-items
       :items (gethash "Items" response)
       :buffer-name "*Jellyjam Tracks*"
       :fields '(name artists album duration)
       :page page
       :pagination-cmd (lambda (p) (jellyjam-playlist-tracks playlist-id p))
       :open-cmd #'jellyjam-play-track))))

(defun jellyjam-playlists (&optional page)
  "List available playlists on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page)))
    (jellyjam--get
        "/Items"
        `(:includeItemTypes "Playlist" :Recursive t
                            :startIndex ,start-index :limit ,jellyjam-max-items-per-page)
      (jellyjam--display-items
       :items (gethash "Items" response)
       :buffer-name "*Jellyjam Playlists*"
       :fields '(name count duration)
       :page page
       :pagination-cmd #'jellyjam-playlists
       :open-cmd #'jellyjam-playlist-tracks))))

(defun jellyjam-album-tracks (album-id &optional page)
  "List tracks in album ALBUM-ID on PAGE."
  (let ((page (or page 1))
        (start-index (* (1- (or page 1)) jellyjam-max-items-per-page)))
    (jellyjam--get
        "/Items"
        `(:parentId ,album-id :includeItemTypes "Audio"
                    :startIndex ,start-index :limit ,jellyjam-max-items-per-page)
      (jellyjam--display-items
       :items (gethash "Items" response)
       :buffer-name "*Jellyjam Tracks*"
       :fields '(name artists album duration)
       :page page
       :pagination-cmd (lambda (p) (jellyjam-album-tracks album-id p))
       :open-cmd #'jellyjam-play-track))))

(defun jellyjam-albums (&optional page)
  "List available albums on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page)))
    (jellyjam--get
        "/Items"
        `(:includeItemTypes "MusicAlbum" :Recursive t
                            :startIndex ,start-index :limit ,jellyjam-max-items-per-page)
      (jellyjam--display-items
       :items (gethash "Items" response)
       :buffer-name "*Jellyjam Albums*"
       :fields '(name artist duration)
       :page page
       :pagination-cmd #'jellyjam-albums
       :open-cmd #'jellyjam-album-tracks))))

(defun jellyjam-tracks (&optional page)
  "List available tracks on PAGE."
  (interactive)
  (let* ((page (or page 1))
         (start-index (* (1- page) jellyjam-max-items-per-page)))
    (jellyjam--get
        "/Items"
        `(:includeItemTypes "Audio" :Recursive t
                            :startIndex ,start-index :limit ,jellyjam-max-items-per-page)
      (jellyjam--display-items
       :items (gethash "Items" response)
       :buffer-name "*Jellyjam Tracks*"
       :fields '(name artists album duration)
       :page page
       :pagination-cmd #'jellyjam-tracks
       :open-cmd #'jellyjam-play-track))))

(provide 'jellyjam)

;;; jellyjam.el ends here
