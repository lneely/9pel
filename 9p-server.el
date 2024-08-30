(load-file "./pack.el")
(load-file "./helpers.el")
(load-file "./log.el")
(load-file "./msg.el")
(load-file "./handle.el")

;; 9p-server-socket-name defines the filesystem path to create the
;; unix socket for 9p communication.
(defvar 9p-server-socket-name "/tmp/emacs-9p-server.sock"
  "The name of the socket file used by the 9P server.")

;; 9p-server-process is the process object for the running 9p server.
(defvar 9p-server-process nil
  "The process object of the running 9P server.")

;; 9p-start-server starts the 9p server on a given socket.
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

;; 9p-stop-server stops the running 9p server and removes the socket file.
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

;; 9p-server-sentinel handles status changes.
(defun 9p-server-sentinel (proc event)
  "Handle server process status changes."
  (9p-log "9P server process %s" event))

;; 9p-handle-message handles incoming messages from
;; 9p-server-socket.
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
        (let* ((size (9p-gbit32 string 0))
               (type (9p-gbit8 string 4))
               (tag (9p-ensure-32bit (9p-gbit32 string 5))))

          ;; handle incoming 9p messages by type
          (cond
           ((= type (9p-message-type 'Tversion))
            (9p-log "Got Tversion message")
            (9p-handle-Tversion proc (substring string 4)))
           ((= type (9p-message-type 'Tattach))
            (9p-log "Got Tattach message")
            (9p-handle-Tattach proc (substring string 4)))  ; Pass the buffer starting from the type byte
           (t (error "Unsupported message type")))))
    (error
     (9p-log "Error in 9p-handle-message: %s" err)
     (9p-send-Rerror proc (9p-ensure-16bit #xFFFF) (error-message-string err)))))

;; 9p-restart-server restarts the 9p server.
(defun 9p-restart-server ()
  "Restart the 9P server."
  (interactive)
  (when 9p-server-process
    (9p-stop-server))
  (9p-start-server))

;; Bind the reload-and-restart function to a key (dev convenience for now)
(global-set-key (kbd "C-c 9") '9p-restart-server)
