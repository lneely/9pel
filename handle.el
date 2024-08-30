;; handle.el provides message handlers for each type of 9P2000
;; message.

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
;; (Future: For multi-user systems, it would be worthwhile to
;; implement basic protection such that only the user running the
;; process may send and receive messages on the socket.)
(defun 9p-send-Rauth (proc tag)
  "Respond with Rauth message."
  (let* ((total-length (+ 4 1 2 4))
         (buffer (make-string total-length 0)))

    (9p-pbit32 buffer 0 total-length)
    (9p-pbit8 buffer 4 (9p-message-type 'Rauth))
    (9p-pbit16 buffer 5 tag)
    (9p-pbit16 buffer 7 9P-NOFID)

    (9p-log "Sending Rauth message: %s" (9p-hex-dump buffer))
    (process-send-string proc buffer)))
    
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
