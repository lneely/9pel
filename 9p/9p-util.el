;;; 9p-util.el -- provides utility functions for 9p-client and
;;; 9p-server.

;;; Commentary:

;;; Code:

;;; Constants:

;;; Message type helpers:

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

(defun 9p-message-type (type)
	"Return the integer value for TYPE."
  (cdr (assq type 9P-MESSAGE-TYPES)))

;;; Bit Packing:

(defun 9p-pbit8 (string offset value)
  "Put an 8-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF)))

(defun 9p-gbit8 (string offset)
  "Get an 8-bit unsigned integer from STRING at OFFSET."
  (aref string offset))

(defun 9p-pbit16 (string offset value)
  "Put a 16-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF)))

(defun 9p-gbit16 (string offset)
  "Get a 16-bit unsigned integer from STRING at OFFSET."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)))

(defun 9p-pbit32 (string offset value)
  "Put a 32-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF))
  (aset string (+ offset 2) (logand (ash value -16) #xFF))
  (aset string (+ offset 3) (logand (ash value -24) #xFF)))

(defun 9p-gbit32 (string offset)
  "Get a 32-bit unsigned integer from STRING at OFFSET."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)
          (ash (aref string (+ offset 2)) 16)
          (ash (aref string (+ offset 3)) 24)))

(defun 9p-pbit64 (string offset value)
  "Put a 64-bit unsigned integer VALUE into STRING at OFFSET."
  (dotimes (i 8)
    (aset string (+ offset i) (logand (ash value (* -8 i)) #xFF))))

(defun 9p-gbit64 (string offset)
  "Get a 64-bit unsigned integer from STRING at OFFSET."
  (let ((result 0))
    (dotimes (i 8)
      (setq result (logior result (ash (aref string (+ offset i)) (* 8 i)))))
    result))

(defun 9p-gstring (buffer offset length)
  "Get a string from BUFFER starting at OFFSET with LENGTH bytes."
  (let ((end (+ offset length)))
    (if (> end (length buffer))
        (error "Buffer overflow: attempt to read beyond buffer end")
      (decode-coding-string (substring buffer offset end) 'utf-8))))

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


;;; Logging:

(defun 9p-log (message &rest fmt-args)
  "Log formatted MESSAGE to a log buffer.
BUFFER-NAME, if provided, specifies the name of the log buffer.
If BUFFER-NAME is not provided, it defaults to \"*9P Log*\".
FMT-ARGS are the optional format arguments for MESSAGE."
  (let* ((log-buffer-name "*9P Log*")
         (log-buffer (get-buffer-create log-buffer-name)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (if fmt-args
                          (apply #'format message fmt-args)
                        message))))))

;;; Socket Management:

(defun 9p-get-socket-path ()
  "Get the path for the 9P server socket using XDG_RUNTIME_DIR."
  (let ((xdg-runtime-dir (getenv "XDG_RUNTIME_DIR")))
    (if xdg-runtime-dir
        (expand-file-name "emacs-9p-server.sock" xdg-runtime-dir)
      (expand-file-name "emacs-9p-server.sock" temporary-file-directory))))

(defun 9p-hex-dump (string)
  "Return a hex dump of STRING."
  (mapconcat (lambda (c) (format "%02x" c)) string " "))

(provide '9p-util)

;;; 9p-util.el ends here.
