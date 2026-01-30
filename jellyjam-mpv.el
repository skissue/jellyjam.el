;;; jellyjam-mpv.el --- mpv integration -*- lexical-binding: t -*-

;;; Commentary:

;; mpv process management, IPC, and event system.

;;; Code:

(require 'cl-lib)

(defconst jellyjam--mpv-socket "/tmp/jellyjam-mpv.sock"
  "Path to mpv IPC socket.")

(defvar jellyjam--mpv-process nil
  "Current mpv process for audio playback.")

(defvar jellyjam--event-handlers (make-hash-table :test 'equal)
  "Hash table mapping event names to lists of handler functions.")

(defvar jellyjam--property-handlers (make-hash-table :test 'eql)
  "Hash table mapping observer IDs to (property-name . handler) cons cells.")

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
  "Dispatch EVENT to appropriate handlers."
  (let ((event-name (gethash "event" event)))
    (if (string= event-name "property-change")
        (when-let* ((id (gethash "id" event))
                    (entry (gethash id jellyjam--property-handlers))
                    (handler (cdr entry)))
          (funcall handler event))
      (dolist (handler (gethash event-name jellyjam--event-handlers))
        (funcall handler event)))))

(defun jellyjam--reregister-observers ()
  "Re-register all property observers after reconnect."
  (maphash (lambda (id entry)
             (jellyjam--mpv-send "observe_property" id (car entry)))
           jellyjam--property-handlers))

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

(defmacro jellyjam-on (event args &rest body)
  "Register a handler for EVENT, binding ARGS from event JSON data.
ARGS should be symbols corresponding to keys in the event JSON. BODY is
evaluated for each instance of EVENT with ARGS bound."
  (declare (indent 2))
  (let ((event-sym (gensym "event")))
    `(puthash ,event
              (cons (lambda (,event-sym)
                      (let ,(mapcar (lambda (arg)
                                      `(,arg (gethash ,(symbol-name arg) ,event-sym)))
                                    args)
                        ,@body))
                    (gethash ,event jellyjam--event-handlers))
              jellyjam--event-handlers)))

(defmacro jellyjam-observe (property &rest body)
  "Observe PROPERTY changes, binding `value' to new data.
Returns the observer ID. BODY is evaluated for every change of PROPERTY
with the new value bound to `value'."
  (declare (indent 1))
  (let ((id-sym (gensym "id"))
        (event-sym (gensym "event")))
    `(progn
       (jellyjam--ensure-ipc)
       (let ((,id-sym (cl-incf jellyjam--observer-counter)))
         (puthash ,id-sym
                  (cons ,property
                        (lambda (,event-sym)
                          (let ((value (gethash "data" ,event-sym)))
                            ,@body)))
                  jellyjam--property-handlers)
         (jellyjam--mpv-send "observe_property" ,id-sym ,property)
         ,id-sym))))

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
