;; pack.el is concerned with packing and unpacking bits.

(defun 9p-pbit8 (string offset value)
  "Put an 8-bit unsigned integer VALUE into STRING at OFFSET."
  (aset string offset (logand value #xFF)))

(defun 9p-gbit8 (string offset)
  "Get an 8-bit unsigned integer from STRING at OFFSET."
  (logand (aref string offset) #xFF))

(defun 9p-pbit16 (string offset value)
  "Put a 16-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF)))

(defun 9p-gbit16 (string offset)
  "Get a 16-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (logand (aref string (1+ offset)) #xFF) 8)))

(defun 9p-pbit32 (string offset value)
  "Put a 32-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (aset string offset (logand value #xFF))
  (aset string (1+ offset) (logand (ash value -8) #xFF))
  (aset string (+ offset 2) (logand (ash value -16) #xFF))
  (aset string (+ offset 3) (logand (ash value -24) #xFF)))

(defun 9p-gbit32 (string offset)
  "Get a 32-bit unsigned integer from STRING at OFFSET in little-endian order."
  (logior (aref string offset)
          (ash (logand (aref string (1+ offset)) #xFF) 8)
          (ash (logand (aref string (+ offset 2)) #xFF) 16)
          (ash (logand (aref string (+ offset 3)) #xFF) 24)))

(defun 9p-pbit64 (string offset value)
  "Put a 64-bit unsigned integer VALUE into STRING at OFFSET in little-endian order."
  (dotimes (i 8)
    (aset string (+ offset i) (logand (ash value (* -8 i)) #xFF))))

(defun 9p-gbit64 (string offset)
  "Get a 64-bit unsigned integer from STRING at OFFSET in little-endian order."
  (let ((result 0))
    (dotimes (i 8)
      (setq result (logior result (ash (logand (aref string (+ offset i)) #xFF) (* 8 i)))))
    result))

