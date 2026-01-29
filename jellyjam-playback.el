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

(defun jellyjam--mpv-command ()
  "Format and return command to start mpv process."
  (list "mpv" "--no-video" "--idle"
        (format "--input-ipc-server=%s" jellyjam--mpv-socket)
        (format "--volume=%d" jellyjam-default-volume)))

(defun jellyjam--ensure-mpv ()
  "Ensure mpv is running.
Return non-nil if there was an error."
  (unless (and jellyjam--mpv-process
               (process-live-p jellyjam--mpv-process))
    (when (file-exists-p jellyjam--mpv-socket)
      (delete-file jellyjam--mpv-socket))
    (setq jellyjam--mpv-process
          (make-process :name "jellyjam-mpv"
                        :buffer (get-buffer-create "*Jellyjam mpv*")
                        :command (jellyjam--mpv-command)
                        :sentinel (lambda (_proc event)
                                    (message "mpv: %s" (string-trim event)))))
    (let ((tries 50))
      (while (and (> tries 0)
                  (not (file-exists-p jellyjam--mpv-socket)))
        (sleep-for 0.05)
        (cl-decf tries))
      (unless (file-exists-p jellyjam--mpv-socket)
        (error "Failed to start mpv")))))

(defun jellyjam-play-track (id)
  "Play track ID with mpv."
  (let ((url (jellyjam--audio-url id)))
    (jellyjam--mpv-send "loadfile" url "replace")
    (message "Playing track...")))

(defun jellyjam--mpv-send (&rest args)
  "Send ARGS to mpv via IPC socket."
  (jellyjam--ensure-mpv)
  (let ((json (concat (json-serialize `(:command ,(apply #'vector args))) "\n")))
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

(defun jellyjam-stop ()
  "Stop Jellyjam playback."
  (interactive)
  (jellyjam--mpv-send "stop"))

(defun jellyjam-pause ()
  "Toggle Jellyjam pause state."
  (interactive)
  (jellyjam--mpv-send "cycle" "pause"))

(defun jellyjam-volume-set (volume)
  "Set Jellyjam volume to VOLUME (0-100)."
  (interactive "nVolume: ")
  (jellyjam--mpv-send "set_property" "volume" volume)
  (message "Volume: %d" volume))

(defun jellyjam-volume-up ()
  "Increase Jellyjam volume."
  (interactive)
  (let* ((response (jellyjam--mpv-send "get_property" "volume"))
         (current (gethash "data" response)))
    (when current
      (jellyjam-volume-set (min 100 (+ current jellyjam-volume-step))))))

(defun jellyjam-volume-down ()
  "Decrease Jellyjam volume."
  (interactive)
  (let* ((response (jellyjam--mpv-send "get_property" "volume"))
         (current (gethash "data" response)))
    (when current
      (jellyjam-volume-set (max 0 (- current jellyjam-volume-step))))))

(provide 'jellyjam-playback)

;;; jellyjam-playback.el ends here
