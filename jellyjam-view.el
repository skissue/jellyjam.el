;;; jellyjam-view.el --- Item listing views -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated list views for Jellyfin items.

;;; Code:

(require 'cl-lib)
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
                                        pagination-cmd open-cmd)
  "Display ITEMS in BUFFER-NAME with FIELDS on PAGE.
PAGINATION-CMD navigates pages, OPEN-CMD opens items."
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
         (buf (get-buffer-create buffer-name))
         (queue (make-plz-queue :limit 4)))
    (with-current-buffer buf
      (jellyjam-items-mode)
      (setq jellyjam--items-command pagination-cmd
            jellyjam--open-command open-cmd
            jellyjam--current-page page
            tabulated-list-format columns
            tabulated-list-entries (mapcar format-entry items))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (plz-run
       (dolist (entry tabulated-list-entries queue)
         (jellyjam--retrieve-thumbnail queue (car entry) buf))))
    (switch-to-buffer buf)))

(provide 'jellyjam-view)

;;; jellyjam-view.el ends here
