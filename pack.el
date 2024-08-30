;; pack.el is concerned with packing and unpacking bits.

(defun 9p-pbit8 (string offset value)
  "Put an 8-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF)))

(defun 9p-gbit8 (string offset)
  "Get an 8-bit unsigned integer from STRING at OFFSET."
  (aref string offset))

(defun 9p-pbit16 (string offset value)
  "Put a 16-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF)))

(defun 9p-gbit16 (string offset)
  "Get a 16-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)))

(defun 9p-pbit32 (string offset value)
  "Put a 32-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF))
  (aset string (+ offset 2) (logand (ash value -16) #xFF))
  (aset string (+ offset 3) (logand (ash value -24) #xFF)))

(defun 9p-gbit32 (string offset)
  "Get a 32-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (aref string (1+ offset)) 8)
          (ash (aref string (+ offset 2)) 16)
          (ash (aref string (+ offset 3)) 24)))

(defun 9p-pbit64 (string offset value)
  "Put a 64-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (dotimes (i 8)
    (aset string (+ offset i) (logand (ash value (* -8 i)) #xFF))))

(defun 9p-gbit64 (string offset)
  "Get a 64-bit unsigned integer from STRING at OFFSET in little-endian order."
  (let ((result 0))
    (dotimes (i 8)
      (setq result (logior result (ash (aref string (+ offset i)) (* 8 i)))))
    result))

(defun 9p-gstring (buffer offset length)
  "Get a string from BUFFER starting at OFFSET with LENGTH bytes."
  (let ((end (+ offset length)))
    ;; Check if the end index goes beyond the buffer length
    (if (> end (length buffer))
        (error "Buffer overflow: attempt to read beyond buffer end")
      ;; Extract and decode the substring from buffer
      (decode-coding-string (substring buffer offset end) 'utf-8))))

(defun 9p-pstring (buffer offset string)
  "Put STRING into BUFFER starting at OFFSET without prefixing its length.
Returns the number of bytes written."
  (let* ((encoded-string (encode-coding-string string 'utf-8))
         (string-length (length encoded-string)))
    ;; Check if buffer can accommodate the string data
    (if (> (+ offset string-length) (length buffer))
        (error "Buffer overflow: not enough space in buffer to write string")
      (progn
        ;; Write the string data into the buffer
        (dotimes (i string-length)
          (aset buffer (+ offset i) (aref encoded-string i)))
        ;; Return the number of bytes written
        string-length))))
