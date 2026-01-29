;;; jellyjam-api.el --- Jellyfin API interaction -*- lexical-binding: t -*-

;;; Commentary:

;; API interaction and authentication for Jellyfin.

;;; Code:

(require 'plz)

(defconst jellyjam--client-name "jellyjam"
  "Client name sent to Jellyfin server.")

(defconst jellyjam--client-version "0.0.1"
  "Client version sent to Jellyfin server.")

(defconst jellyjam--device-name "Emacs"
  "Device name sent to Jellyfin server.")

(defvar jellyjam--sessions nil
  "List of known Jellyfin sessions.
Each session is a plist with :server, :user-id, :access-token,
:username.")

(defvar jellyjam--active-session nil
  "Plist containing the currently active session info.")

(defun jellyjam--auth-header (&optional token)
  "Generate the X-Emby-Authorization header value.
If TOKEN is provided, include it in the header."
  ;; NOTE plz.el currently cannot handle quotes in headers, so do not quote
  ;; these values.
  (format "MediaBrowser Client=%s, Device=%s, DeviceId=%s, Version=%s%s"
          jellyjam--client-name
          jellyjam--device-name
          (system-name)
          jellyjam--client-version
          (if token (format ", Token=%s" token) "")))

(defun jellyjam--plist-to-query-string (plist)
  "Convert PLIST to a URL query string."
  (let (pairs)
    (map-do
     (lambda (key value)
       (push (format "%s=%s"
                     (url-hexify-string (substring (symbol-name key) 1))
                     (url-hexify-string
                      (pcase value
                        ('nil "false")
                        ('t "true")
                        (_ (format "%s" value)))))
             pairs))
     plist)
    (when pairs
      (concat "?" (string-join pairs "&")))))

(defmacro jellyjam--get (endpoint params &rest then)
  "Make authenticated GET request to ENDPOINT, evaluating THEN on success.
ENDPOINT is relative to the server URL. PARAMS is a plist of query
parameters. Pass nil for no parameters. THEN is evaluated with the
response data bound to `response'."
  (declare (indent 2))
  `(progn
     (unless jellyjam--active-session
       (error "No active Jellyfin session"))
     (let* ((server (plist-get jellyjam--active-session :server))
            (token (plist-get jellyjam--active-session :access-token))
            (query-string (jellyjam--plist-to-query-string ,params))
            (url (concat server ,endpoint query-string)))
       (plz 'get url
         :headers `(("Authorization" . ,(jellyjam--auth-header token)))
         :as #'json-parse-buffer
         :then (lambda (response) ,@then)
         :else (lambda (err) (message "Request failed: %S" err))))))

(defun jellyjam-authenticate (server user pass)
  "Authenticate with Jellyfin SERVER using USER and PASS.
Save the session in `jellyjam--sessions'."
  (interactive
   (list (read-string "Jellyfin server URL: ")
         (read-string "Username: ")
         (read-passwd "Password: ")))
  (let ((url (concat (string-remove-suffix "/" server) "/Users/AuthenticateByName")))
    (plz 'post url
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(jellyjam--auth-header)))
      :body (json-serialize `(:Username ,user :Pw ,pass))
      :as #'json-parse-buffer
      :then (lambda (response)
              (let* ((access-token (gethash "AccessToken" response))
                     (user-data (gethash "User" response))
                     (user-id (gethash "Id" user-data))
                     (session `(:server ,(string-remove-suffix "/" server)
                                        :user-id ,user-id
                                        :access-token ,access-token
                                        :username ,user)))
                (push session jellyjam--sessions)
                (setq jellyjam--active-session session)
                (message "Authenticated as %s on %s" user server)))
      :else (lambda (err)
              (message "Authentication failed: %S" err)))))

(defun jellyjam--audio-url (id)
  "Return the audio stream URL for item ID."
  (format "%s/Audio/%s/universal?ApiKey=%s&Container=opus,webm|opus,ts|mp3,mp3,flac,webma,webm|webma,wav,ogg&TranscodingContainer=ts&TranscodingProtocol=hls&AudioCodec=opus"
          (plist-get jellyjam--active-session :server)
          id
          (plist-get jellyjam--active-session :access-token)))

(defun jellyjam--image-url (id width height)
  "Return the image URL for item ID with dimensions WIDTH x HEIGHT."
  (format "%s/Items/%s/Images/Primary?maxWidth=%d&maxHeight=%d"
          (plist-get jellyjam--active-session :server)
          id
          width height))

(provide 'jellyjam-api)

;;; jellyjam-api.el ends here
