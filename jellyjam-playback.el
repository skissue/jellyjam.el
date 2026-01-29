;;; jellyjam-playback.el --- mpv playback control -*- lexical-binding: t -*-

;;; Commentary:

;; mpv-based audio playback for Jellyfin tracks.

;;; Code:

(require 'jellyjam-api)

(defcustom jellyjam-default-volume 100
  "Default volume passed to mpv (0-100)."
  :type 'integer
  :group 'jellyjam)

(defcustom jellyjam-volume-step 5
  "Default volume adjustment step for volume up/down commands."
  :type 'integer
  :group 'jellyjam)

(defconst jellyjam--mpv-socket "/tmp/jellyjam-mpv.sock"
  "Path to mpv IPC socket.")

(defvar jellyjam--mpv-process nil
  "Current mpv process for audio playback.")

(defun jellyjam--mpv-command (url)
  "Format and return command to start mpv with URL."
  (list "mpv" "--no-video"
        (format "--input-ipc-server=%s" jellyjam--mpv-socket)
        (format "--volume=%d" jellyjam-default-volume)
        url))

(defun jellyjam-play-track (id)
  "Play track ID with mpv."
  (when (and jellyjam--mpv-process
             (process-live-p jellyjam--mpv-process))
    (kill-process jellyjam--mpv-process))
  (when (file-exists-p jellyjam--mpv-socket)
    (delete-file jellyjam--mpv-socket))
  (let ((url (jellyjam--audio-url id))
        (buf (get-buffer-create "*Jellyjam mpv*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Playing: %s\nURL: %s\n\n" id url)))
    (setq jellyjam--mpv-process
          (make-process :name "jellyjam-mpv"
                        :buffer buf
                        :command (jellyjam--mpv-command url)
                        :sentinel (lambda (proc event)
                                    (message "mpv: %s" (string-trim event)))))
    (message "Playing track...")))

(defun jellyjam--mpv-send (command)
  "Send COMMAND list to mpv via IPC socket."
  (unless (file-exists-p jellyjam--mpv-socket)
    (user-error "mpv is not running"))
  (let ((json (concat (json-serialize `(:command ,command)) "\n")))
    (with-temp-buffer
      (let ((proc (make-network-process
                   :name "jellyjam-mpv-ipc"
                   :buffer (current-buffer)
                   :family 'local
                   :service jellyjam--mpv-socket)))
        (process-send-string proc json)
        (accept-process-output proc 0.1)
        (delete-process proc)
        (goto-char (point-min))
        (ignore-errors (json-parse-buffer))))))

(defun jellyjam-volume-set (volume)
  "Set mpv volume to VOLUME (0-100)."
  (interactive "nVolume: ")
  (jellyjam--mpv-send `["set_property" "volume" ,volume])
  (message "Volume: %d" volume))

(defun jellyjam-volume-up ()
  "Increase mpv volume."
  (interactive)
  (let* ((response (jellyjam--mpv-send '["get_property" "volume"]))
         (current (gethash "data" response)))
    (when current
      (jellyjam-volume-set (min 100 (+ current jellyjam-volume-step))))))

(defun jellyjam-volume-down ()
  "Decrease mpv volume."
  (interactive)
  (let* ((response (jellyjam--mpv-send '["get_property" "volume"]))
         (current (gethash "data" response)))
    (when current
      (jellyjam-volume-set (max 0 (- current jellyjam-volume-step))))))

(provide 'jellyjam-playback)

;;; jellyjam-playback.el ends here
