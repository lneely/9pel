;; handle.el provides message handlers for each type of 9P2000
;; message.

(defun 9p-handle-Tversion (proc buffer)
  "Handle a Tversion message from the 9P protocol.
PROC is the server process, BUFFER contains the raw message data."
  (let ((tag (9p-gbit16 buffer 1)))  ; Assuming tag starts at offset 1
    (9p-log "Received Tversion message with tag: %04X" tag)
    (9p-send-Rerror proc tag "Tversion received.")
    (9p-log "Sent Rerror in response to Tversion")))

(defun 9p-handle-Tattach (proc buffer)
  "Handle a Tattach message from the 9P protocol.
PROC is the server process, BUFFER contains the raw message data."
  (let ((tag (9p-gbit16 buffer)))
    (9p-log "Received Tattach message with tag: %04X" tag)
    (9p-send-Rerror proc tag "Tattach received by server.")
    (9p-log "Sent Rerror in response to Tattach")))

