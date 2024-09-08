;;; 9p-client.el -- provides client-side functionality to facilitate
;;; testing.

;;; Commentary:
;;;

;;; Code:

(add-to-list 'load-path ".")
(require '9p-util)

;;; Process management:

(defvar 9p-client-process nil
  "The 9P client process.")

(defun 9p-start-client ()
  "Start the 9P client connecting to optional SOCKET-NAME.
Sets the global 9p-client-process."
	(let* ((socket-path (9p-get-socket-path)))
				 (unless (file-exists-p socket-path)
					 (error "9P server socket does not exist: %s" socket-path))
				 (setq 9p-client-process
							 (make-network-process
								:name "9P Client"
								:family 'local
								:service socket-path
								:filter #'9p-client-filter))
				 (9p-log "9P client connected to Unix domain socket: %s" socket-path))
    9p-client-process)

	(defun 9p-stop-client ()
		"Stop the 9P client process."
		(when 9p-client-process
			(delete-process 9p-client-process)
			(setq 9p-client-process nil)
			(9p-log "9P client stopped")))


;;; Message handling;

	(defun 9p-handle-message (process output)
		"Read OUTPUT for the 9P client PROCESS."
		(let ((current-data (or (process-get process :received-data) "")))
			(process-put process :received-data (concat current-data output))))

	(defun 9p-read-from-socket (process)
		"Read data from the socket associated with PROCESS.
Returns the data as a string, or nil if no data is available."
		(let ((data (process-get process :received-data)))
			(when data
				(process-put process :received-data nil))
			data))

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
			(process-send-string proc buffer)))


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
		"Parse incoming Rauth stored in BUFFER."
		(let* ((size (9p-gbit32 buffer 0))
					 (type (9p-gbit8 buffer 4))
					 (tag (9p-gbit16 buffer 5)))
			
			(if (= type (9p-message-type 'Rerror))
					(let* ((error-len (9p-gbit16 buffer 7))
								 (error-msg (9p-gstring buffer 9 error-len)))
						(list :type 'Rerror :size size :tag tag :error error-msg))
				
				(if (= type (9p-message-type 'Rauth))
						(let ((aqid (substring buffer 7 20)))
							(list :type 'Rauth :size size :tag tag :aqid aqid))
					
					;; Unknown message type
					(list :type 'unknown :size size :tag tag)))))

	(provide '9p-client)

;;; 9p-client.el ends here
