;;; jellyjam-view.el --- Item listing views -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated list views for Jellyfin items.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'jellyjam-api)
(require 'jellyjam-playback)

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

(defvar-local jellyjam--open-function nil
  "Function to open the item at point.")

(defvar-local jellyjam--queue-function nil
  "Function to queue the item at point.")

(defvar-local jellyjam--play-function nil
  "Function to play the item at point, overriding the current queue.")

(defun jellyjam--image-column-spec ()
  "Specification for the image column for `tabulated-list-format'."
  (list " "
        (ceiling (/ (float (car jellyjam-thumbnail-size))
                    (frame-char-width)))
        nil))

(defun jellyjam--insert-thumbnail (buffer id data)
  "Insert thumbnail DATA for ID into BUFFER at placeholder."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (when (search-forward (format "[[%s]]" id) nil t)
            (delete-region (match-beginning 0) (match-end 0))
            (insert-image (create-image data nil :data))))))))

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
  (if jellyjam--open-function
      (funcall jellyjam--open-function (tabulated-list-get-id))
    (user-error "No open function for this buffer")))

(defun jellyjam-items-queue ()
  "Queue the item at point."
  (interactive)
  (if jellyjam--queue-function
      (funcall jellyjam--queue-function (tabulated-list-get-id))
    (user-error "No queue function for this buffer")))

(defun jellyjam-items-play ()
  "Play the item at point, overriding the current queue."
  (interactive)
  (if jellyjam--play-function
      (funcall jellyjam--play-function (tabulated-list-get-id))
    (user-error "No play function for this buffer")))

(define-derived-mode jellyjam-items-mode tabulated-list-mode "Jellyjam"
  "Major mode for displaying Jellyfin item lists."
  (setq tabulated-list-padding 2)
  (local-set-key (kbd "N") #'jellyjam-items-next-page)
  (local-set-key (kbd "P") #'jellyjam-items-prev-page)
  (local-set-key (kbd "RET") #'jellyjam-items-open)
  (local-set-key (kbd "a") #'jellyjam-items-queue)
  (local-set-key (kbd "C-<return>") #'jellyjam-items-play))

(defun jellyjam--field-spec (field)
  "Return (COLUMN-SPEC . EXTRACTOR) for FIELD symbol."
  (pcase field
    ('name '(("Name" 30 t) .
             (lambda (item) (or (gethash "Name" item) "Untitled"))))
    ('artist '(("Artist" 20 t) .
               (lambda (item) (or (gethash "AlbumArtist" item) "Unknown"))))
    ('artists '(("Artist" 20 t) .
                (lambda (item)
                  (let ((artists (gethash "Artists" item)))
                    (if (and artists (> (length artists) 0))
                        (aref artists 0)
                      "Unknown")))))
    ('album '(("Album" 20 t) .
              (lambda (item) (or (gethash "Album" item) "Unknown"))))
    ('duration '(("Duration" 10 t) .
                 (lambda (item) (jellyjam--format-duration
                            (gethash "RunTimeTicks" item)))))
    ('count '(("Items" 8 t) .
              (lambda (item)
                (number-to-string (or (gethash "ChildCount" item) 0)))))
    (_ (error "Unknown field: %s" field))))

(cl-defun jellyjam--display-items (&key items buffer-name fields page
                                        pagination-cmd open-cmd queue-cmd play-cmd)
  "Display ITEMS in BUFFER-NAME with FIELDS on PAGE.
PAGINATION-CMD navigates pages, OPEN-CMD opens items, QUEUE-CMD queues items."
  (let* ((field-specs (mapcar #'jellyjam--field-spec fields))
         (columns (vconcat (list (jellyjam--image-column-spec))
                           (mapcar #'car field-specs)))
         (extractors (mapcar #'cdr field-specs))
         (format-entry
          (lambda (item)
            (let ((id (gethash "Id" item)))
              (list id (vconcat (list (format "[[%s]]" id))
                                (mapcar (lambda (fn) (funcall fn item))
                                        extractors))))))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (jellyjam-items-mode)
      (setq jellyjam--items-command pagination-cmd
            jellyjam--open-function open-cmd
            jellyjam--queue-function queue-cmd
            jellyjam--play-function play-cmd
            jellyjam--current-page page
            tabulated-list-format columns
            tabulated-list-entries (mapcar format-entry items))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (jellyjam--fetch-thumbnails
       (mapcar #'car tabulated-list-entries)
       (car jellyjam-thumbnail-size)
       (cdr jellyjam-thumbnail-size)
       (lambda (id data) (jellyjam--insert-thumbnail buf id data))
       (lambda (id err) (message "Error fetching thumbnail for %s: %S" id err))))
    (switch-to-buffer buf)))

(defmacro jellyjam-define-view (name docstring &rest args)
  "Define a paginated view command NAME with DOCSTRING.
ARGS is a plist with:
  :params       - plist of extra query params (evaluated)
  :buffer-name  - buffer name string
  :fields       - list of field symbols
  :open-cmd     - command to open items
  :queue-cmd    - command to queue items
  :play-cmd     - command to play items (clears queue first)"
  (declare (indent 2))
  (map-let (:params :buffer-name :fields :open-cmd :queue-cmd :play-cmd) args
    `(defun ,name (&optional page parent-id)
       ,docstring
       (interactive)
       (let* ((page (or page 1))
              (start-index (* (1- page) jellyjam-max-items-per-page))
              (params (append ,params
                              (when parent-id `(:parentId ,parent-id))
                              `(:startIndex ,start-index
                                            :limit ,jellyjam-max-items-per-page))))
         (jellyjam--get "/Items" params
           (jellyjam--display-items
            :items (gethash "Items" response)
            :buffer-name ,buffer-name
            :fields ',fields
            :page page
            :pagination-cmd (lambda (p) (,name p parent-id))
            :open-cmd ,open-cmd
            :queue-cmd ,queue-cmd
            :play-cmd ,play-cmd))))))

(jellyjam-define-view jellyjam-playlists
    "List available playlists."
  :params '(:includeItemTypes "Playlist" :Recursive t)
  :buffer-name "*Jellyjam Playlists*"
  :fields (name count duration)
  :open-cmd (lambda (id) (jellyjam-tracks nil id))
  :queue-cmd #'jellyjam-queue-collection
  :play-cmd #'jellyjam-play-collection)

(jellyjam-define-view jellyjam-albums
    "List available albums."
  :params '(:includeItemTypes "MusicAlbum" :Recursive t)
  :buffer-name "*Jellyjam Albums*"
  :fields (name artist duration)
  :open-cmd (lambda (id) (jellyjam-tracks nil id))
  :queue-cmd #'jellyjam-queue-collection
  :play-cmd #'jellyjam-play-collection)

(jellyjam-define-view jellyjam-tracks
    "List tracks, optionally filtered by PARENT-ID."
  :params '(:includeItemTypes "Audio" :Recursive t)
  :buffer-name "*Jellyjam Tracks*"
  :fields (name artists album duration)
  :open-cmd #'jellyjam-play-track
  :queue-cmd #'jellyjam-queue-track
  :play-cmd #'jellyjam-play-track)

(provide 'jellyjam-view)

;;; jellyjam-view.el ends here
