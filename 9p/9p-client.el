;; 9p-client provides client-side functionality to facilitate testing.

(provide '9p-client)

(defun 9p-send-Tversion (proc msize version tag)
  "Send a Tversion message to the server."
  (let* ((version-bytes (string-to-unibyte version))
         (version-len (length version-bytes))
         (msg-size (+ 4 1 2 4 2 version-len))
         (buffer (make-string msg-size 0)))
    (9p-pbit32 buffer 0 msg-size)
    (9p-pbit8 buffer 4 (9p-message-type 'Tversion))
    (9p-pbit16 buffer 5 tag)
    (9p-pbit32 buffer 7 msize)
    (9p-pbit16 buffer 11 version-len)
    (setf (substring buffer 13) version-bytes)
    (message "Attempting to send Tversion: size=%d, tag=%d, msize=%d, version=%s" 
             msg-size tag msize version)
    (condition-case err
        (process-send-string proc buffer)
      (error (message "Error in process-send-string: %s" (error-message-string err))))
    (message "Tversion sent to %s" (process-name proc))
    (condition-case err
        (message "Process status: %s" (process-status proc))
      (error (message "Error getting process status: %s" (error-message-string err))))
    (condition-case err
        (message "Process send queue size: %d" (process-send-queue-size proc))
      (error (message "Error getting send queue size: %s" (error-message-string err))))
    (sleep-for 0.1)  ; Wait for 100ms
    (condition-case err
        (message "Process status after delay: %s" (process-status proc))
      (error (message "Error getting process status after delay: %s" (error-message-string err))))
    (condition-case err
        (message "Process send queue size after delay: %d" (process-send-queue-size proc))
      (error (message "Error getting send queue size after delay: %s" (error-message-string err))))
    (message "Tversion send attempt completed")))

(defun 9p-recv-Rversion (buffer)
  "Parse a received Rversion message from the server."
  (let* ((size (9p-gbit32 buffer 0))
         (type (9p-gbit8 buffer 4))
         (tag (9p-gbit16 buffer 5))
         (msize (9p-gbit32 buffer 7))
         (version-len (9p-gbit16 buffer 11))
         (version (9p-gstring buffer 13 version-len)))

    (if (= type (9p-message-type 'Rversion))
        (list :type 'Rversion :size size :tag tag :msize msize :version version)
      ;; Unknown message type
      (list :type 'unknown :size size :tag tag))))

(defun 9p-send-Tauth (proc afid uname aname tag)
  "Send a Tauth message to the server."
  (let* ((uname-bytes (string-to-unibyte uname))
         (uname-len (length uname-bytes))
         (aname-bytes (string-to-unibyte aname))
         (aname-len (length aname-bytes))
         (msg-size (+ 4 1 2 4 2 uname-len 2 aname-len))
         (buffer (make-string msg-size 0 t)))
    
    ;; Construct the message
    (9p-pbit32 buffer 0 msg-size)          
    (9p-pbit8 buffer 4 (9p-message-type 'Tauth))  
    (9p-pbit16 buffer 5 tag)           
    (9p-pbit32 buffer 7 afid)          
    (9p-pbit16 buffer 11 uname-len)    
    (dotimes (i uname-len)
      (aset buffer (+ 13 i) (aref uname-bytes i)))
    (9p-pbit16 buffer (+ 13 uname-len) aname-len) 
    (dotimes (i aname-len)
      (aset buffer (+ 15 uname-len i) (aref aname-bytes i)))
    (process-send-string proc buffer)))

(defun 9p-recv-Rauth (buffer)
  "Parse a received Rauth message from the server."
  (let* ((size (9p-gbit32 buffer 0))
         (type (9p-gbit8 buffer 4))
         (tag (9p-gbit16 buffer 5)))
    
    (if (= type (9p-message-type 'Rerror))
        ;; Parse Rerror
        (let* ((error-len (9p-gbit16 buffer 7))
               (error-msg (9p-gstring buffer 9 error-len)))
          (list :type 'Rerror :size size :tag tag :error error-msg))
      
      ;; Parse Rauth
      (if (= type (9p-message-type 'Rauth))
          (let ((aqid (substring buffer 7 20)))
            (list :type 'Rauth :size size :tag tag :aqid aqid))
        
        ;; Unknown message type
        (list :type 'unknown :size size :tag tag)))))

