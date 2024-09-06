;; 9P-NOTAG is a dummy tag value for use in special situations.
(defconst 9P-NOTAG #xFFFF)

;; 9P-NOFID is a dummy fid value.
(defconst 9P-NOFID #xFFFFFFFF)

;; 9P-QTDIR identifies that a quantum identifier is for a directory.
(defconst 9P-QTDIR #x80)

;; 9P-QTFILE identifies that a quantum identifier is for a regular
;; file.
(defconst 9P-QTFILE #x00)

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

;; 9p-next-fid tracks the next fid to generate, and is incremented
;; whenever 9p-fid-generate is called.
(defvar 9p-next-fid 0
  "The next FID to be assigned.")

;; 9p-root-namespace defines the virtual root of the filesystem we are
;; serving.
(defvar 9p-root-namespace '("/b/"))

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

;; 9p-active-fids contains all active fids and maps them to their
;; corresonding paths in our vfs.
(defvar 9p-active-fids (make-hash-table :test 'equal)
  "Hash table to store FID to path mappings.")

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

;; 9p-fid-generate returns a new unique fid, and increments the
;; 9p-next-fid variable. It wraps to zero when we reach the uint32
;; maximum value.
(defun 9p-fid-generate ()
  "Generate a new, unique FID in a thread-safe manner."
  (mutex-lock 9p-fid-lock)
  (unwind-protect
      (let ((fid 9p-next-fid))
        (setq 9p-next-fid (logand (1+ 9p-next-fid) #xFFFFFFFE))
        (if (= fid #xFFFFFFFF)
            (9p-fid-generate)  ; Recursively get the next FID if we hit the reserved value
          fid))
    (mutex-unlock 9p-fid-lock)))

;; 9p-fid-set-path associates a given fid to a given path in the vfs
;; and adds it to our active fids.
(defun 9p-fid-set-path (fid path)
  "Associate a path with a FID."
  (puthash fid path 9p-active-fids))

;; 9p-fid-get-path retrieves the fid for a given path in the vfs from
;; our active fids.
(defun 9p-fid-get-path (fid)
  "Get the path associated with a FID."
  (gethash fid 9p-active-fids))

;; 9p-fid-remove removes an fid entry from the table.
(defun 9p-fid-remove (fid)
  "Remove a FID from the table."
  (remhash fid 9p-active-fids))

;; 9p-in-namespace checks that a given path is in the allowed
;; namespace for our 9p server.
(defun 9p-in-namespace-p (path)
  "Check if the given path is within the allowed namespace."
  (or (string= path "/")
      (cl-some (lambda (allowed-path)
                 (string-prefix-p allowed-path path))
               9p-root-namespace)))

;; 9p-rewrite-path rewrites a path sent by the 9p client so that it is
;; always in the allowed namespace.
(defun 9p-rewrite-path (path)
  "Rewrite the path to ensure it stays within the allowed namespace."
  (if (9p-in-namespace-p path)
      path
    (concat "/b" path)))

;; 9p-message-type is a helper function that returns an integer
;; constant for a given message type name, e.g., 'Rerror returns 107.
(defun 9p-message-type (type)
  (cdr (assq type 9P-MESSAGE-TYPES)))

;; TODO: 9p-handle-wstat is a helper function for Twstat 
;; implement this)
(defun 9p-handle-wstat (fid stat)
  "Handle wstat operation for the given FID and stat structure."
  (let ((path (9p-get-path fid)))
    (if (9p-in-namespace-p path)
        (let ((real-path (9p-rewrite-path path)))
          ;; Implement the actual wstat operation here
          ;; This might include changing file permissions, timestamps, etc.
          ;; Be careful to only allow changes that make sense for your virtual filesystem
          (error "Wstat operation not fully implemented"))
      (error "Path not in allowed namespace"))))

;; 9p-handle-walk walks the given path in the allowed namespace.
(defun 9p-handle-walk (fid newfid names)
  "Handle the 9P walk operation."
  (let ((path (9p-get-path fid)))
    (dolist (name names)
      (setq path (expand-file-name name path))
      (setq path (9p-rewrite-path path))
      (unless (file-exists-p path)
        (error "File not found")))
    (9p-set-path newfid path)
    (length names)))

(defun 9p-handle-read (fid offset count)
  "Handle the 9P read operation."
  (let ((path (9p-get-path fid)))
    (if (string= path "/")
        (let ((root-contents (mapconcat #'identity (9p-list-root) "\n")))
          (substring root-contents offset (min (length root-contents) (+ offset count))))
      (with-temp-buffer
        (insert-file-contents (9p-rewrite-path path))
        (buffer-substring (+ (point-min) offset)
                          (min (+ (point-min) offset count) (point-max)))))))

(defun 9p-handle-stat (fid)
  "Handle the 9P stat operation."
  (let ((path (9p-get-path fid)))
    (if (string= path "/")
        (list :type 'directory
              :name "/"
              :length 0
              :mode #o040755)
      (let ((attrs (file-attributes (9p-rewrite-path path))))
        (list :type (if (eq (car attrs) t) 'directory 'file)
              :name (file-name-nondirectory path)
              :length (nth 7 attrs)
              :mode (file-modes (9p-rewrite-path path)))))))



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

;; 9p-qid is a helper function that generates a quantum identifier for
;; a given file path. The QID is an 8-bit integer, followed by a
;; 32-bit integer, followed by a 64-bit integer.
(defun 9p-qid (path)
  "Generate a QID for the given path."
  (let* ((attrs (file-attributes path))
         (type (if (eq (car attrs) t) 9P-QTDIR 9P-QTFILE))
         (version (float-time (nth 5 attrs)))  ; Use mtime as version
         (path-id (sxhash path)))
    (list type version path-id)))

;; 9p-start-server starts the 9p server on a given socket.
(defun 9p-start-server (&optional socket-name)
  "Start the 9P server on SOCKET-NAME.
If SOCKET-NAME is not provided, use the default value.
Sets the global `9p-server-process` and returns the server process object."
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
  (9p-log "9P server started on Unix domain socket: %s" 9p-server-socket-name)
  9p-server-process)

;; 9p-stop-server stops the running 9p server and removes the socket file.
(defun 9p-stop-server (&optional process)
  "Stop the 9P server and remove the socket file.
If PROCESS is provided, stop that specific server process.
Otherwise, stop the server associated with `9p-server-process`."
  (interactive)
  (let ((server-process (or process 9p-server-process)))
    (when server-process
      (delete-process server-process)
      (when (eq server-process 9p-server-process)
        (setq 9p-server-process nil))
      (when (file-exists-p 9p-server-socket-name)
        (delete-file 9p-server-socket-name)
        (9p-log "Socket file removed: %s" 9p-server-socket-name))
      (9p-log "9P server stopped"))))

;; 9p-server-sentinel handles status changes.
(defun 9p-server-sentinel (proc event)
  "Handle server process status changes."
  (9p-log "9P server process %s" event))

;; 9p-handle-message handles incoming messages from
;; 9p-server-socket.
(defun 9p-handle-message (proc buffer)
  "Process incoming data from the 9P client."
  (message "In 9p server message handler")
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
	    (message "Message type: %d" type)
            (cond
             ((= type (9p-message-type 'Tversion))
              (9p-recv-Tversion proc unibyte-buffer))
             ((= type (9p-message-type 'Tauth))
              (9p-recv-Tauth proc unibyte-buffer))
             ((= type (9p-message-type 'Tattach))
              (9p-recv-Tattach proc unibyte-buffer))
             ((= type (9p-message-type 'Twalk))
              (9p-recv-Twalk proc unibyte-buffer))
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
  (message "9P server received Tversion")
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
;;
;; TODO: Rversion can simply respond with "9P2000" as the version. We
;; will still use the msize we got from Tversion.
;;
;; Refer: https://9fans.github.io/plan9port/man/man4/9pserve.html
;; See '-M' flag.
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

    (9p-log "Preparing to send Rversion message:")
    (9p-log "  Total length: %d" total-length)
    (9p-log "  Tag: %d" tag)
    (9p-log "  Msize: %d" msize)
    (9p-log "  Version: %s" version)
    (9p-log "  Message content: %s" (9p-hex-dump buffer))

    (condition-case err
        (progn
          (process-send-string proc buffer)
          (9p-log "Rversion message sent successfully"))
      (error
       (9p-log "Error sending Rversion message: %s" (error-message-string err))))))

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
;; Refer: https://9fans.github.io/plan9port/man/man4/9pserve.html
;; See '-n' flag for expected behavior.
(defun 9p-send-Rauth (proc tag)
  "Respond with error message; no authentication."
  (9p-send-Rerror proc tag "No authentication needed"))


;; 9p-recv-Tattach is invoked when a Tattach message is read from the
;; socket.
(defun 9p-recv-Tattach (proc buffer)
  (let* ((tag (9p-gbit16 buffer 5))
         (fid (9p-gbit32 buffer 7))
         (afid (9p-gbit32 buffer 11))
         (uname-length (9p-gbit16 buffer 15))
         (uname-data (9p-gstring buffer 17 uname-length))
         (aname-length (9p-gbit16 buffer (+ 17 uname-length)))
         (aname-data (9p-gstring buffer (+ 19 uname-length) aname-length)))

    (9p-send-Rattach proc tag fid afid uname-data aname-data)))

;; TODO: implement
(defun 9p-send-Rattach (proc tag fid afid uname aname)
  (let* ((total-length (+ 4 1 2 1 4 8))
         (buffer (make-string total-length 0)))
    (9p-pbit32 buffer 0 total-length)
    (9p-pbit8 buffer 4 (9p-message-type 'Rattach))
    (9p-pbit16 buffer 5 tag)

    ;; TODO: qid
    (let ((qid (9p-qid "/")))
      (9p-log "QID: Type %s, Version: %d, Path: %d"
              (nth 0 qid)  
              (nth 1 qid)  
              (nth 2 qid))
      (9p-pbit8 buffer 7 (nth 0 qid))
      (9p-pbit32 buffer 8 (nth 1 qid))
      (9p-pbit64 buffer 12 (nth 2 qid)))

    (9p-log "Sending Rattach message: %s" (9p-hex-dump buffer))
    (process-send-string proc buffer)))

(defun 9p-recv-Twalk (tag fid newfid nwname &rest wnames)
  "Handle 9P Twalk message."
  (condition-case err
      (let ((nwqids (9p-handle-walk fid newfid wnames)))
        (9p-send-Rwalk tag nwqids))
    (error
     (9p-send-Rerror tag (error-message-string err)))))

(defun 9p-recv-Tread (proc tag fid offset count)
  "Handle 9P Tread message."
  (condition-case err
      (let* ((data (9p-handle-read fid offset count))
             (count (length data)))
        (9p-send-Rread tag count data))
    (error
     (9p-send-Rerror tag (error-message-string err)))))

(defun 9p-recv-Tstat (proc tag fid)
  "Handle 9P Tstat message."
  (condition-case err
      (let ((stat (9p-handle-stat fid)))
        (9p-send-Rstat tag stat))
    (error
     (9p-send-Rerror tag (error-message-string err)))))

(defun 9p-recv-Twstat (proc tag fid stat)
  "Handle 9P Twstat message."
  (condition-case err
      (progn
        (9p-handle-wstat fid stat)
        (9p-send-Rwstat tag))
    (error
     (9p-send-Rerror tag (error-message-string err)))))

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

(provide '9p)

