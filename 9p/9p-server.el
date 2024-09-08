;;; 9p-server.el -- implements 9p server

;;; Commentary:
;;;

;;; Code:

(add-to-list 'load-path ".")
(require '9p-util)

;;; Constants:

(defconst 9P-NOTAG #xFFFF)
(defconst 9P-NOFID #xFFFFFFFF)
(defconst 9P-QTDIR #x80)
(defconst 9P-QTFILE #x00)

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

;;; Helper functions:

(defun 9p-message-type (type)
	"Return the integer value for TYPE."
  (cdr (assq type 9P-MESSAGE-TYPES)))

;;; Quantum identifier management:

(defun 9p-qid (path)
  "Generate a QID for the given PATH."
  (let* ((attrs (file-attributes path))
         (type (if (eq (car attrs) t) 9P-QTDIR 9P-QTFILE))
         (version (float-time (nth 5 attrs)))
         (path-id (sxhash path)))
    (list type version path-id)))

;;; Process management:

(defvar 9p-socket-name (9p-get-socket-path)
  "The name of the socket file used by the 9P server.")

(defvar 9p-server-process nil
  "The process object of the running 9P server.")

(defun 9p-start-server (&optional socket-name)
  "Start the 9P server on SOCKET-NAME.
If SOCKET-NAME is not provided, use the default value.
Sets the global `9p-server-process` and returns the server process object."
  (interactive)
  (when 9p-server-process
    (error "9P server is already running. Stop it first with 9p-stop-server"))
  (setq 9p-socket-name (or socket-name 9p-socket-name))
  (unless 9p-socket-name
    (setq 9p-socket-name "/tmp/emacs-9p-server.sock"))
  (when (file-exists-p 9p-socket-name)
    (delete-file 9p-socket-name))
  (setq 9p-server-process
        (make-network-process
         :name "9P Server"
         :server t
         :family 'local
         :service 9p-socket-name
         :filter #'9p-handle-message
         :sentinel #'9p-server-sentinel))
  (9p-log "9P server started on Unix domain socket: %s" 9p-socket-name)
  9p-server-process)


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
      (when (file-exists-p 9p-socket-name)
        (delete-file 9p-socket-name)
        (9p-log "Socket file removed: %s" 9p-socket-name))
      (9p-log "9P server stopped"))))

(defun 9p-restart-server ()
  "Restart the 9P server."
  (interactive)
  (when 9p-server-process
    (9p-stop-server))
  (9p-start-server))


;;; Fid management:

(defvar 9p-fid-lock (make-mutex "9p-fid-lock")
  "Mutex for thread-safe FID generation in the 9P server.")

(defvar 9p-next-fid 0
  "The next FID to be assigned.")

(defvar 9p-active-fids (make-hash-table :test 'equal)
  "Hash table to store FID to path mappings.")

(defun 9p-fid-generate ()
  "Generate a new, unique FID in a thread-safe manner."
  (mutex-lock 9p-fid-lock)
  (unwind-protect
      (let ((fid 9p-next-fid))
        (setq 9p-next-fid (logand (1+ 9p-next-fid) #xFFFFFFFE))
        (if (= fid #xFFFFFFFF)
            (9p-fid-generate)
          fid))
    (mutex-unlock 9p-fid-lock)))

(defun 9p-fid-set-path (fid path)
  "Associate a PATH with a FID."
  (puthash fid path 9p-active-fids))

(defun 9p-fid-get-path (fid)
  "Get the path associated with a FID."
  (gethash fid 9p-active-fids))

(defun 9p-fid-remove (fid)
  "Remove a FID from the table."
  (remhash fid 9p-active-fids))


;;; Message Handling:

(defun 9p-server-sentinel (proc event)
  "Log events related to the 9P server process PROC.
State changes are indicated as EVENT."
  (9p-log "9P server event %s" event))

(defun 9p-handle-message (proc buffer)
	"Handle incoming message on PROC stored in BUFFER."
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
             ((= type (9p-message-type 'Twalk))
              (9p-recv-Twalk proc unibyte-buffer))
             ((= type (9p-message-type 'Tclunk))
              (9p-recv-Tclunk proc unibyte-buffer))
             (t (error "Unsupported message type: %d" type)))))
      (error
       (9p-log "Error in 9p-handle-message: %s"
							 (error-message-string err))
       (9p-send-Rerror proc tag (error-message-string err))))))


(defun 9p-recv-Tversion (proc buffer)
	"Handle inbound Tversion message on PROC stored in BUFFER."
  (let* ((tag (9p-gbit16 buffer 5))
         (msize (9p-gbit32 buffer 7))
         (version-size (9p-gbit16 buffer 11))
         (version-data (substring buffer 13 (+ 13 version-size))))
    (9p-send-Rversion proc tag msize version-data)))

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

    (condition-case err
				(progn (process-send-string proc buffer)
							 (error	(9p-log "Error sending Rversion message: %s"
															(error-message-string err)))))))

(defun 9p-recv-Tauth (proc buffer)
	"Handle inbound Tauth message on PROC stored in BUFFER."
  (9p-log "Got Tauth message")
  (let* ((tag (9p-gbit16 buffer 5)))
    (9p-send-Rerror proc tag "No authentication needed")))

(defun 9p-recv-Tattach (proc buffer)
	"Handle inbound Tattach message on PROC stored in BUFFER."
  (let* ((tag (9p-gbit16 buffer 5))
         (fid (9p-gbit32 buffer 7))
         (afid (9p-gbit32 buffer 11))
         (uname-length (9p-gbit16 buffer 15))
         (uname-data (9p-gstring buffer 17 uname-length))
         (aname-length (9p-gbit16 buffer (+ 17 uname-length)))
         (aname-data (9p-gstring buffer (+ 19 uname-length) aname-length)))

    (9p-send-Rattach proc tag fid afid uname-data aname-data)))

(defun 9p-send-Rattach (proc tag fid afid uname aname)
	"Construct Rattach with TAG, FID, AFID, UNAME, and ANAME.
Send via the server process PROC."
  (let* ((total-length (+ 4 1 2 1 4 8))
         (buffer (make-string total-length 0)))
    (9p-pbit32 buffer 0 total-length)
    (9p-pbit8 buffer 4 (9p-message-type 'Rattach))
    (9p-pbit16 buffer 5 tag)

    ;; TODO: qid for vfs root
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

;; TODO:
(defun 9p-recv-Twalk (proc buffer)
	"Handle inbound Twalk message on PROC stored in BUFFER.")

;; TODO:
(defun 9p-recv-Tread (proc buffer)
	"Handle inbound Tread message on PROC stored in BUFFER.")

;; TODO:
(defun 9p-recv-Tstat (proc buffer)
	"Handle inbound Tstat message on PROC stored in BUFFER.")

;; TODO:
(defun 9p-recv-Twstat (proc buffer)
	"Handle inbound Tstat message on PROC stored in BUFFER.")

;; TODO:
(defun 9p-recv-Tclunk (proc buffer)
	"Handle inbound Tclunk message on PROC stored in BUFFER."
  (let* ((tag (9p-gbit16 buffer 5)))
    (9p-send-Rerror proc tag "Got Tclunk")))

(defun 9p-send-Rerror (proc tag error-message)
  "Construct Rerror message with given TAG and ERROR-MESSAGE.
Send message using server process PROC."
  (let* ((error-string (string-as-unibyte error-message))
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

;;; Virtual Filesystem Integration:

(defvar 9p-root-namespace "/b/"
	"The 9P server root namespace points to a vfs location.")

(defun 9p-in-namespace-p (path)
  "Check if the given PATH is within the allowed namespace."
  (or (string= path "/")
      (cl-some (lambda (allowed-path)
                 (string-prefix-p allowed-path path))
               9p-root-namespace)))

(defun 9p-rewrite-path (path)
  "Rewrite the PATH to ensure it stays within the allowed namespace."
  (if (9p-in-namespace-p path)
      path
    (concat "/b" path)))


(provide '9p-server)

;;; 9p-server.el ends here
