;;; jellyjam-playback.el --- mpv playback control -*- lexical-binding: t -*-

;;; Commentary:

;; mpv-based audio playback for Jellyfin tracks.

;;; Code:

(require 'jellyjam-api)
(require 'jellyjam-mpv)

(defvar jellyjam--queue nil
  "List of track IDs in playback order.")

(defvar jellyjam--queue-pos nil
  "Current position in queue (0-indexed), or nil if empty.")

(defvar jellyjam--queue-observer-registered nil
  "Non-nil if playlist-playing-pos observer has been registered.")

(defun jellyjam--queue-position-changed (pos)
  "Handle playlist position change to POS from mpv."
  (setq jellyjam--queue-pos pos))

(defun jellyjam--ensure-queue-observer ()
  "Ensure the queue position observer is registered."
  (unless jellyjam--queue-observer-registered
    (jellyjam-add-observer "playlist-playing-pos" #'jellyjam--queue-position-changed)
    (setq jellyjam--queue-observer-registered t)))

(defun jellyjam-current-track ()
  "Return the ID of the currently playing track, or nil."
  (when (and jellyjam--queue jellyjam--queue-pos)
    (nth jellyjam--queue-pos jellyjam--queue)))

(defun jellyjam-queue-length ()
  "Return the number of tracks in the queue."
  (length jellyjam--queue))

(defcustom jellyjam-default-volume 100
  "Default volume passed to mpv (0-100)."
  :type 'integer
  :group 'jellyjam)

(defcustom jellyjam-volume-step 5
  "Default volume adjustment step for volume up/down commands."
  :type 'integer
  :group 'jellyjam)

(defun jellyjam-play-track (id &optional silent)
  "Play track ID with mpv.
Show a message notifying the user unless SILENT is non-nil."
  (jellyjam--ensure-queue-observer)
  (setq jellyjam--queue (list id))
  (setq jellyjam--queue-pos 0)
  (jellyjam--mpv-send "loadfile" (jellyjam--audio-url id) "replace")
  (jellyjam--mpv-send "set_property" "pause" :false)
  (unless silent
    (message "Playing track")))

(defun jellyjam-queue-track (id &optional silent)
  "Add track ID to mpv playlist.
Show a message notifying the user unless SILENT is non-nil."
  (setq jellyjam--queue (append jellyjam--queue (list id)))
  (jellyjam--mpv-send "loadfile" (jellyjam--audio-url id) "append")
  (unless silent
    (message "Queued track")))

(defun jellyjam-queue-tracks (ids &optional silent)
  "Queue multiple track IDS.
Show a message notifying the user unless SILENT is non-nil."
  (dolist (id ids)
    (jellyjam-queue-track id :silent))
  (unless silent
    (message "Queued %d tracks" (length ids))))

(defun jellyjam-clear-queue (&optional silent)
  "Clear the Jellyjam playback queue.
Show a message notifying the user unless SILENT is non-nil."
  (interactive)
  (setq jellyjam--queue nil)
  (setq jellyjam--queue-pos nil)
  (jellyjam--mpv-send "playlist-clear")
  ;; `playlist-clear' does not remove the current file.
  (jellyjam--mpv-send "playlist-remove" "current")
  (unless silent
    (message "Cleared queue")))

(defun jellyjam-play-collection (parent-id)
  "Clear queue and play all tracks under PARENT-ID."
  (jellyjam--get-child-items parent-id
      (jellyjam-clear-queue :silent)
    (jellyjam-play-track (car ids) :silent)
    (jellyjam-queue-tracks (cdr ids) :silent)
    (message "Playing %d tracks" (length ids))))

(defun jellyjam-queue-collection (parent-id)
  "Add all tracks under PARENT-ID to the queue."
  (jellyjam--get-child-items parent-id
      (jellyjam-queue-tracks ids)))

(defun jellyjam-stop ()
  "Stop Jellyjam playback."
  (interactive)
  (setq jellyjam--queue nil)
  (setq jellyjam--queue-pos nil)
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
