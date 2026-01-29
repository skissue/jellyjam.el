;;; jellyjam-view.el --- Item listing views -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated list views for Jellyfin items.

;;; Code:

(require 'plz)
(require 'jellyjam-api)

(defcustom jellyjam-thumbnail-size '(64 . 64)
  "Thumbnail size (width . height)."
  :type '(cons integer integer)
  :group 'jellyjam)

(defcustom jellyjam-max-items-per-page 100
  "Maximum items to list per page."
  :type 'integer
  :group 'jellyjam)

(defvar-local jellyjam--current-page 1
  "Current page number in collection buffer.")

(defvar-local jellyjam--items-command nil
  "Command providing the items for the current buffer.")

(defvar-local jellyjam--open-command nil
  "Command to open the item at point.")

(defun jellyjam--image-column-spec ()
  "Specification for the image column for `tabulated-list-format'."
  (list " "
        (ceiling (/ (float (car jellyjam-thumbnail-size))
                    (frame-char-width)))
        nil))

(defun jellyjam--retrieve-thumbnail (queue id buffer)
  "Retrieve thumbnail for item ID and display in BUFFER using QUEUE."
  (plz-queue queue 'get (jellyjam--image-url
                         id
                         (car jellyjam-thumbnail-size)
                         (cdr jellyjam-thumbnail-size))
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

(defun jellyjam-items-next-page ()
  "Go to next page of items."
  (interactive)
  (funcall jellyjam--items-command (1+ jellyjam--current-page)))

(defun jellyjam-items-prev-page ()
  "Go to previous page items collections."
  (interactive)
  (if (> jellyjam--current-page 1)
      (funcall jellyjam--items-command (1- jellyjam--current-page))
    (user-error "No previous page")))

(defun jellyjam-items-open ()
  "Open the item at point."
  (interactive)
  (if jellyjam--open-command
      (funcall jellyjam--open-command (tabulated-list-get-id))
    (user-error "No open command for this buffer")))

(define-derived-mode jellyjam-items-mode tabulated-list-mode "Jellyjam"
  "Major mode for displaying Jellyfin item lists."
  (setq tabulated-list-padding 2)
  (local-set-key (kbd "N") #'jellyjam-items-next-page)
  (local-set-key (kbd "P") #'jellyjam-items-prev-page)
  (local-set-key (kbd "RET") #'jellyjam-items-open))

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

(defun jellyjam--format-album-entry (album)
  "Format ALBUM hash-table as a tabulated-list entry."
  (let ((id (gethash "Id" album))
        (name (or (gethash "Name" album) "Untitled"))
        (artist (or (gethash "AlbumArtist" album) "Unknown"))
        (ticks (gethash "RunTimeTicks" album)))
    (list id (vector (format "[[%s]]" id)
                     name
                     artist
                     (jellyjam--format-duration ticks)))))

(defun jellyjam--format-track-entry (track)
  "Format TRACK hash-table as a tabulated-list entry."
  (let ((id (gethash "Id" track))
        (name (or (gethash "Name" track) "Untitled"))
        ;; ???
        (artist (or (gethash "Artists" track) ["Unknown"]))
        (album (or (gethash "Album" track) "Unknown"))
        (ticks (gethash "RunTimeTicks" track)))
    (list id (vector (format "[[%s]]" id)
                     name
                     (aref artist 0)
                     album
                     (jellyjam--format-duration ticks)))))

(defun jellyjam--tracks (tracks page pagination-command open-command)
  "List TRACKS for PAGE.
PAGINATION-COMMAND is used to navigate pages.
OPEN-COMMAND is called when opening a track."
  (interactive)
  (let* ((buf (get-buffer-create "*Jellyjam Tracks*"))
         (queue (make-plz-queue :limit 4)))
    (with-current-buffer buf
      (jellyjam-items-mode)
      (setq jellyjam--items-command pagination-command
            jellyjam--open-command open-command
            jellyjam--current-page page
            tabulated-list-format (vector (jellyjam--image-column-spec)
                                          '("Name" 30 t)
                                          '("Artist" 20 t)
                                          '("Album" 20 t)
                                          '("Duration" 10 t))
            tabulated-list-entries
            (mapcar #'jellyjam--format-track-entry tracks))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (plz-run
       (dolist (entry tabulated-list-entries queue)
         (jellyjam--retrieve-thumbnail queue (car entry) buf))))
    (switch-to-buffer buf)))

(provide 'jellyjam-view)

;;; jellyjam-view.el ends here
