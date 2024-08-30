;; helpers.el contains helper functions.

;; 9p-ensure-32bit guarantees that the value is a 32-bit integer.
(defun 9p-ensure-32bit (value)
  "Ensure VALUE is a 32-bit integer."
  (logand value #xFFFFFFFF))

;; 9p-ensure-16bit guarantees that the value is a 16-bit integer.
(defun 9p-ensure-16bit (value)
  "Ensure VALUE is a 16-bit integer."
  (logand value #xFFFF))

;; 9p-send-Rerror sends an Rerror message back to the client given a
;; server process and a tag (the tag is set by the client).
(defun 9p-send-Rerror (proc tag error-message)
  "Send an error response to the client."
  (let* ((error-string (encode-coding-string error-message 'utf-8))
         (message-size (+ 4 1 2 2 (length error-string)))
         (response (make-string message-size 0)))
    (9p-pbit32 response 0 message-size)
    (aset response 4 (9p-message-type 'Rerror))
    (9p-pbit16 response 5 (logand tag #xFFFF)) 
    (9p-pbit16 response 7 (length error-string))
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

;; a variable-length field in 9p consists of a uint16 representing the
;; size of the field, followed by a UTF-8 string containing the data.
(defun 9p-read-variable-length-field (buffer)
  "Read a variable-length field from BUFFER.
Returns a list (SIZE STRING), where SIZE is the field size in bytes,
and STRING is the decoded UTF-8 string."
  (let* ((field-size (9p-gbit16 buffer))
         (raw-bytes (cl-loop repeat field-size
                             collect (9p-gbit8 buffer)))
         (utf8-string (decode-coding-string 
                       (apply #'unibyte-string raw-bytes) 
                       'utf-8 t)))
    (list field-size utf8-string)))
