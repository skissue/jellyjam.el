;;; jellyjam-mpv.el --- mpv integration -*- lexical-binding: t -*-

;;; Commentary:

;; mpv process management, IPC, and event system.

;;; Code:

(require 'cl-lib)

(defconst jellyjam--mpv-socket "/tmp/jellyjam-mpv.sock"
  "Path to mpv IPC socket.")

(defvar jellyjam--mpv-process nil
  "Current mpv process for audio playback.")

(defvar jellyjam--property-observers nil
  "Alist mapping observer IDs to property names ((ID . property) ...).")

(defvar jellyjam--observer-counter 0
  "Counter for generating unique observer IDs.")

(defvar jellyjam--ipc-process nil
  "Persistent IPC connection to mpv.")

(defvar jellyjam--ipc-buffer ""
  "Buffer for accumulating incomplete IPC output.")

(defvar jellyjam--pending-response nil
  "Storage for synchronous command responses.")

(defun jellyjam--ipc-filter (_proc output)
  "Process OUTPUT from mpv IPC, parsing JSON lines and dispatching events."
  (setq jellyjam--ipc-buffer (concat jellyjam--ipc-buffer output))
  (let ((lines (split-string jellyjam--ipc-buffer "\n" t)))
    (if (string-suffix-p "\n" output)
        (setq jellyjam--ipc-buffer "")
      (setq jellyjam--ipc-buffer (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (when-let* ((json (ignore-errors (json-parse-string line))))
        (if (gethash "event" json)
            (jellyjam--dispatch-event json)
          (setq jellyjam--pending-response json))))))

(defun jellyjam--dispatch-event (event)
  "Dispatch EVENT to appropriate handlers via hooks."
  (let ((event-name (gethash "event" event)))
    (if (string= event-name "property-change")
        (when-let* ((id (gethash "id" event))
                    (entry (assq id jellyjam--property-observers))
                    (property (cdr entry))
                    (hook-sym (intern (format "jellyjam-property-%s-functions" property))))
          (when (boundp hook-sym)
            (run-hook-with-args hook-sym (gethash "data" event))))
      (let ((hook-sym (intern (format "jellyjam-event-%s-functions" event-name))))
        (when (boundp hook-sym)
          (run-hook-with-args hook-sym event))))))

(defun jellyjam--reregister-observers ()
  "Re-register all property observers after reconnect."
  (dolist (entry jellyjam--property-observers)
    (jellyjam--mpv-send "observe_property" (car entry) (cdr entry))))

(defun jellyjam--mpv-command ()
  "Format and return command to start mpv process."
  (list "mpv" "--no-video" "--idle"
        "--quiet" "--msg-color=no" "--term-osd=no"
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

(defun jellyjam--mpv-send (&rest args)
  "Send ARGS to mpv via persistent IPC connection.
Returns the parsed JSON response synchronously."
  (jellyjam--ensure-ipc)
  (setq jellyjam--pending-response nil)
  (let ((json (concat (json-serialize `(:command ,(apply #'vector args))) "\n")))
    (process-send-string jellyjam--ipc-process json)
    (let ((tries 100))
      (while (and (> tries 0) (null jellyjam--pending-response))
        (accept-process-output jellyjam--ipc-process 0.01)
        (cl-decf tries)))
    jellyjam--pending-response))

(defun jellyjam--ensure-ipc ()
  "Ensure persistent IPC connection to mpv exists."
  (unless (and jellyjam--ipc-process
               (process-live-p jellyjam--ipc-process))
    (jellyjam--ensure-mpv)
    (setq jellyjam--ipc-buffer "")
    (setq jellyjam--ipc-process
          (make-network-process
           :name "jellyjam-ipc"
           :buffer nil
           :family 'local
           :service jellyjam--mpv-socket
           :filter #'jellyjam--ipc-filter
           :sentinel (lambda (_proc _event)
                       (setq jellyjam--ipc-process nil))))
    (jellyjam--reregister-observers)))

(defun jellyjam-add-observer (property fn)
  "Observe PROPERTY changes, calling FN with the new value.
FN receives the new property value as its single argument.
If PROPERTY is already being observed, FN is simply added to the hook."
  (jellyjam--ensure-ipc)
  (let ((hook-sym (intern (format "jellyjam-property-%s-functions" property))))
    (unless (rassoc property jellyjam--property-observers)
      (let ((id (cl-incf jellyjam--observer-counter)))
        (push (cons id property) jellyjam--property-observers)
        (jellyjam--mpv-send "observe_property" id property)))
    (add-hook hook-sym fn)))

(defun jellyjam-kill ()
  "Kill the mpv process."
  (interactive)
  (when (and jellyjam--mpv-process
             (process-live-p jellyjam--mpv-process))
    (delete-process jellyjam--mpv-process)
    (setq jellyjam--mpv-process nil)
    (when (file-exists-p jellyjam--mpv-socket)
      (delete-file jellyjam--mpv-socket))
    (message "Killed mpv process")))

(provide 'jellyjam-mpv)

;;; jellyjam-mpv.el ends here
