;; handle.el provides message handlers for each type of 9P2000
;; message.

(defun 9p-recv-Tversion (proc buffer)
  "Handle a received Tversion message from the client."
  (9p-log "Got Tversion message")
  (9p-send-Rerror proc (9p-ensure-16bit #xFFFF) "Got Tversion message"))

(defun 9p-send-Rversion (proc tag error-message)
  "Send a Rversion response to the client."
  (9p-log "Sending Rversion response")
  )

(defun 9p-recv-Tattach (proc buffer)
  "Handle a received Tattach message from the client."
  (9p-log "Got Tattach message")
  (9p-send-Rerror proc (9p-ensure-16bit #xFFFF) "Got Tattach message"))

(defun 9p-send-Rattach (proc tag error-message)
  "Send a Rattach respons to the client."
    (9p-log "Sending Rattach response"))

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
