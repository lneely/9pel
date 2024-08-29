(defvar 9p-server-socket-name "/tmp/emacs-9p-server.sock"
  "The name of the socket file used by the 9P server.")

(defvar 9p-server-process nil
  "The process object of the running 9P server.")

(defun 9p-start-server (&optional socket-name)
  "Start the 9P server on SOCKET-NAME.
If SOCKET-NAME is not provided, use the default value."
  (interactive)
  (when 9p-server-process
    (error "9P server is already running. Stop it first with 9p-stop-server"))
  (setq 9p-server-socket-name (or socket-name 9p-server-socket-name))
  (unless 9p-server-socket-name
    (setq 9p-server-socket-name "/tmp/emacs-9p-server.sock"))
  (when (file-exists-p 9p-server-socket-name)
    (delete-file 9p-server-socket-name))
  (setq 9p-server-process
        (make-network-process
         :name "9P Server"
         :server t
         :family 'local
         :service 9p-server-socket-name
         :filter #'9p-handle-message
         :sentinel #'9p-server-sentinel))
  (9p-log "9P server started on Unix domain socket: %s" 9p-server-socket-name))

(defun 9p-stop-server ()
  "Stop the 9P server and remove the socket file."
  (interactive)
  (when 9p-server-process
    (delete-process 9p-server-process)
    (setq 9p-server-process nil)
    (when (file-exists-p 9p-server-socket-name)
      (delete-file 9p-server-socket-name)
      (9p-log "Socket file removed: %s" 9p-server-socket-name))
    (9p-log "9P server stopped")))

(defun 9p-server-sentinel (proc event)
  "Handle server process status changes."
  (9p-log "9P server process %s" event))

(defun 9p-handle-message (proc string)
  "Process incoming data from the 9P client."
  (condition-case err
      (progn
        (9p-log "Received 9P message: %d bytes" (length string))
        (9p-log "Raw message: %s" (9p-hex-dump string))

        ;; verify required headers present
        (when (< (length string) 9)
          (error "Message too short"))

        ;; unpack 9p message
        (let* ((size (9p-gbit32-le string 0))
               (type (9p-gbit8 string 4))
               (tag (9p-ensure-32bit (9p-gbit32-le string 5))))

        ;; handle incoming 9p messages by type
        (cond
          (t (error "Unsupported message type")))))
    (error
     (9p-log "Error in 9p-handle-message: %s" err)
     (9p-send-error proc (9p-ensure-16bit #xFFFF) (error-message-string err)))))


(defun 9p-send-error (proc tag error-message)
  "Send an error response to the client."
  (let* ((error-string (encode-coding-string error-message 'utf-8))
         (message-size (+ 4 1 2 2 (length error-string)))
         (response (make-string message-size 0)))
    (9p-pbit32-le response 0 message-size)
    (aset response 4 (9p-message-type 'Rerror))
    (9p-pbit16-le response 5 (logand tag #xFFFF)) 
    (9p-pbit16-le response 7 (length error-string))
    (dotimes (i (length error-string))
      (aset response (+ 9 i) (aref error-string i)))
    (9p-log "Sending Rerror - size: %d, tag: %04X, message: %s"
            message-size (logand tag #xFFFF) error-message)
    (9p-log "Error response hex dump: %s" (9p-hex-dump response))
    (condition-case err
        (process-send-string proc response)
      (error
       (9p-log "Error sending response: %s" err)))
    (9p-log "Rerror sent successfully")))

(defun 9p-valid-tag-p (tag)
  (and (integerp tag) (>= tag 0) (< tag #x100000000)))

(defun 9p-normalize-tag (tag)
  "Normalize the tag to ensure it's a 32-bit value."
  (logand tag #xFFFFFFFF))
