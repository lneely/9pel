;; 9P-NOTAG is a dummy tag value for use in special situations.
(defconst 9P-NOTAG #xFFFF
  "Dummy tag value, equivalent to (ushort)~0U in C.")

;; 9P-NOFID is a dummy fid value.
(defconst 9P-NOFID #xFFFFFFFF
  "Dummy fid value, equivalent to (u32int)~0U in C.")

;; 9P-MESSAGE-TYPES enumerates the T- and R-messages defined in the
;; protocol.
(defconst 9P-MESSAGE-TYPES
  '((Tversion . 100) (Rversion . 101) (Tauth . 102) (Rauth . 103)
    (Tattach . 104) (Rattach . 105) (Terror . 106) (Rerror . 107)
    (Tflush . 108) (Rflush . 109) (Twalk . 110) (Rwalk . 111)
    (Topen . 112) (Ropen . 113) (Tcreate . 114) (Rcreate . 115)
    (Tread . 116) (Rread . 117) (Twrite . 118) (Rwrite . 119)
    (Tclunk . 120) (Rclunk . 121) (Tremove . 122) (Rremove . 123)
    (Tstat . 124) (Rstat . 125) (Twstat . 126) (Rwstat . 127)
    (Tmax . 128) (Topenfd . 98) (Ropenfd . 99))
  "Enumeration of 9P message types.")

;; PBIT8 packs 1 byte into a buffer at the given offset with the given
;; value.
(defun 9p-pbit8 (string offset value)
  "Put an 8-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF)))

;; GBIT8 unpacks 1 byte from a buffer at the given offset.
(defun 9p-gbit8 (string offset)
  "Get an 8-bit unsigned integer from STRING at OFFSET."
  (aref string offset))

;; PBIT16 packs 2 bytes into a buffer at the given offset with the given
;; value.
(defun 9p-pbit16 (string offset value)
  "Put a 16-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF)))

;; GBIT16 unpacks 2 bytes from a buffer at the given offset.
(defun 9p-gbit16 (string offset)
  "Get a 16-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)))

;; PBIT32 packs 4 bytes into a buffer at the given offset with the given
;; value.
(defun 9p-pbit32 (string offset value)
  "Put a 32-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF))
  (aset string (+ offset 2) (logand (ash value -16) #xFF))
  (aset string (+ offset 3) (logand (ash value -24) #xFF)))

;; GBIT32 unpacks 4 bytes from a buffer at the given offset.
(defun 9p-gbit32 (string offset)
  "Get a 32-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)
          (ash (aref string (+ offset 2)) 16)
          (ash (aref string (+ offset 3)) 24)))

;; PBIT64 packs 8 bytes into a buffer at the given offset with the given
;; value.
(defun 9p-pbit64 (string offset value)
  "Put a 64-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (dotimes (i 8)
    (aset string (+ offset i) (logand (ash value (* -8 i)) #xFF))))

;; GBIT64 unpacks 4 bytes from a buffer at the given offset.
(defun 9p-gbit64 (string offset)
  "Get a 64-bit unsigned integer from STRING at OFFSET in little-endian order."
  (let ((result 0))
    (dotimes (i 8)
      (setq result (logior result (ash (aref string (+ offset i)) (* 8 i)))))
    result))

;; 9p-gstring unpacks a string of a given length out of a buffer located
;; at a given offset.
(defun 9p-gstring (buffer offset length)
  "Get a string from BUFFER starting at OFFSET with LENGTH bytes."
  (let ((end (+ offset length)))
    ;; Check if the end index goes beyond the buffer length
    (if (> end (length buffer))
        (error "Buffer overflow: attempt to read beyond buffer end")
      ;; Extract and decode the substring from buffer
      (decode-coding-string (substring buffer offset end) 'utf-8))))

;; 9p-pstring packs a string into a buffer at a given offset.
(defun 9p-pstring (buffer offset string)
  "Put STRING into BUFFER starting at OFFSET without prefixing its length.
Returns the number of bytes written."
  (let* ((encoded-string (encode-coding-string string 'utf-8))
         (string-length (length encoded-string)))
    (if (> (+ offset string-length) (length buffer))
        (error "Buffer overflow: not enough space in buffer to write string")
      (progn
        (dotimes (i string-length)
          (aset buffer (+ offset i) (aref encoded-string i)))
        string-length))))

;; 9p-message-type is a helper function that returns an integer
;; constant for a given message type name, e.g., 'Rerror returns 107.
(defun 9p-message-type (type)
  (cdr (assq type 9P-MESSAGE-TYPES)))

;; 9p-log is a helper function that writes log messages to the *9P
;; Server Log* buffer.
(defun 9p-log (format-string &rest args)
  "Log a message with a timestamp."
  (let ((log-buffer (get-buffer-create "*9P Server Log*")))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (apply #'format format-string args))))))

;; 9p-hex-dump is a helper function that formats raw data into a
;; readable hexadecimal string representation.
(defun 9p-hex-dump (string)
  "Return a hex dump of STRING."
  (mapconcat (lambda (c) (format "%02x" c)) string " "))

;; 9p-server-socket-name defines the filesystem path to create the
;; unix socket for 9p communication.
;; 
;; TODO: Create socket in user's xdg runtime directory instead of
;; /tmp. Otherwise, this will not work on multi-user systems.
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
(defun 9p-handle-message (proc buffer)
  "Process incoming data from the 9P client."
  (let ((unibyte-buffer (string-as-unibyte buffer)))
    (condition-case err
        (progn
          (9p-log "Received 9P message: %d bytes" (length unibyte-buffer))
          (9p-log "Raw message: %s" (9p-hex-dump unibyte-buffer))
          (when (< (length unibyte-buffer) 9)
            (error "Message too short: %d bytes" (length unibyte-buffer)))
          (let* ((size (9p-gbit32 unibyte-buffer 0))
                 (type (9p-gbit8 unibyte-buffer 4))
                 (tag (9p-gbit16 unibyte-buffer 5)))
            (cond
             ((= type (9p-message-type 'Tversion))
              (9p-recv-Tversion proc unibyte-buffer))
             ((= type (9p-message-type 'Tauth))
              (9p-recv-Tauth proc unibyte-buffer))
             ((= type (9p-message-type 'Tattach))
              (9p-recv-Tattach proc unibyte-buffer))
             ((= type (9p-message-type 'Tclunk))
              (9p-recv-Tclunk proc unibyte-buffer))
             (t (error "Unsupported message type: %d" type))))
          (9p-log "Message handling completed successfully"))
      (error
       (9p-log "Error in 9p-handle-message: %s" (error-message-string err))
       (9p-send-Rerror proc tag (error-message-string err))))))

;; 9p-restart-server restarts the 9p server.
(defun 9p-restart-server ()
  "Restart the 9P server."
  (interactive)
  (when 9p-server-process
    (9p-stop-server))
  (9p-start-server))

;; Bind the reload-and-restart function to a key (dev convenience for now)
(global-set-key (kbd "C-c 9") '9p-restart-server)

;; 9p-recv-Tversion is invoked when a Tversion message is read from
;; the socket. A Tversion expects an Rversion response.
(defun 9p-recv-Tversion (proc buffer)
  "Handle a received Tversion message from the client."
  (let* ((tag (9p-gbit16 buffer 5))
         (msize (9p-gbit32 buffer 7))
         (version-size (9p-gbit16 buffer 11))
         (version-data (substring buffer 13 (+ 13 version-size))))

    ;; debug: print info about the message
    (9p-log "Got Tversion message")
    (9p-log "\tMsize: %d" msize)
    (9p-log "\tTag: %d" tag)
    (9p-log "\tVersion-Size: %d" version-size)
    (9p-log "\tVersion-Data: %s" version-data)
    (9p-send-Rversion proc tag msize version-data)))

;; 9p-send-Rversion generates a Rversion response and writes it to the
;; socket.
(defun 9p-send-Rversion (proc tag msize version)
  "Send an Rversion message via PROC with TAG, MSIZE, and VERSION."
  (let* ((version-length (length (encode-coding-string version 'utf-8)))
         (total-length (+ 4 1 2 4 2 version-length))
         (buffer (make-string total-length 0)))

    (9p-pbit32 buffer 0 total-length)  
    (9p-pbit8 buffer 4 (9p-message-type 'Rversion))
    (9p-pbit16 buffer 5 tag)  
    (9p-pbit32 buffer 7 msize)
    (9p-pbit16 buffer 11 (length version))
    (9p-pstring buffer 13 version) 

    (9p-log "Sending Rversion message: %s" (9p-hex-dump buffer))
    (process-send-string proc buffer)))

;; 9p-recv-Tauth is invoked when a Tauth message is received on the
;; socket. It responds with NOFID to indicate to the client that no
;; authentication is required.
(defun 9p-recv-Tauth (proc buffer)
  "Handle a received Tauth message from the client."
  (9p-log "Got Tauth message")
  (let* ((tag (9p-gbit16 buffer 5)))
    (9p-send-Rauth proc tag)))

;; 9p-send-Rauth responds to a Tauth message with Rauth. The Rauth
;; returns NOFID, indicating to the client that no authentication is
;; required. Consequently it does not provide unames or anames in the
;; message.
;;
;; (Future: For now, 9pel uses a domain socket to communicate. There
;; is no reason this could not be extended to support TCP. However,
;; some basic protections should probably be in place for running on a
;; network.)
(defun 9p-send-Rauth (proc tag)
  "Respond with Rauth message."
  (let* ((total-length (+ 4 1 2 13))
         (buffer (make-string total-length 0)))

    (9p-pbit32 buffer 0 total-length)
    (9p-pbit8 buffer 4 (9p-message-type 'Rauth))
    (9p-pbit16 buffer 5 tag)

    ;; empty qid 
    (9p-pbit8 buffer 7 0)
    (9p-pbit32 buffer 8 0)
    (9p-pbit64 buffer 12 0)

    (9p-log "Sending Rauth message: %s" (9p-hex-dump buffer))
    (process-send-string proc buffer)))

;; TODO: implement
(defun 9p-recv-Tattach (proc buffer)
  (let* ((tag (9p-gbit16 buffer 5)))
    (9p-send-Rerror proc tag "Got Tattach")))

;; TODO: implement
(defun 9p-send-Rattach ())

;; TODO: implement
(defun 9p-recv-Tclunk (proc buffer)
  (let* ((tag (9p-gbit16 buffer 5)))
    (9p-send-Rerror proc tag "Got Tclunk")))

;; TODO: implement
(defun 9p-send-Rclunk (proc buffer))
    
;; 9p-send-Rerror sends an Rerror message back to the client given a
;; server process and a tag (the tag is set by the client).
(defun 9p-send-Rerror (proc tag error-message)
  "Send an error response to the client."
  (let* ((error-string (encode-coding-string error-message 'utf-8))
         (message-size (+ 4 1 2 2 (length error-string)))
         (response (make-string message-size 0))
         (unibyte-response (string-as-unibyte response)))
    (9p-pbit32 unibyte-response 0 message-size)
    (aset unibyte-response 4 (9p-message-type 'Rerror))
    (9p-pbit16 unibyte-response 5 (logand tag #xFFFF)) 
    (9p-pbit16 unibyte-response 7 (length error-string))
    (dotimes (i (length error-string))
      (aset unibyte-response (+ 9 i) (aref error-string i)))
    (9p-log "Sending Rerror - size: %d, tag: %04X, message: %s"
            message-size (logand tag #xFFFF) error-message)
    (9p-log "Error response hex dump: %s" (9p-hex-dump unibyte-response))
    (condition-case err
        (process-send-string proc unibyte-response)
      (error
       (9p-log "Error sending response: %s" (error-message-string err))))
    (9p-log "Rerror sent successfully")))
