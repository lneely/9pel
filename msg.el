(defconst 9P-NOTAG #xFFFF
  "Dummy tag value, equivalent to (ushort)~0U in C.")

(defconst 9P-NOFID #xFFFFFFFF
  "Dummy fid value, equivalent to (u32int)~0U in C.")

(defconst 9P-MESSAGE-TYPES
  '((Tversion . 100)
    (Rversion . 101)
    (Tauth . 102)
    (Rauth . 103)
    (Tattach . 104)
    (Rattach . 105)
    (Terror . 106)  ; illegal
    (Rerror . 107)
    (Tflush . 108)
    (Rflush . 109)
    (Twalk . 110)
    (Rwalk . 111)
    (Topen . 112)
    (Ropen . 113)
    (Tcreate . 114)
    (Rcreate . 115)
    (Tread . 116)
    (Rread . 117)
    (Twrite . 118)
    (Rwrite . 119)
    (Tclunk . 120)
    (Rclunk . 121)
    (Tremove . 122)
    (Rremove . 123)
    (Tstat . 124)
    (Rstat . 125)
    (Twstat . 126)
    (Rwstat . 127)
    (Tmax . 128)
    (Topenfd . 98)
    (Ropenfd . 99))
  "Enumeration of 9P message types.")

(defun 9p-message-type (type)
  (cdr (assq type 9P-MESSAGE-TYPES)))

(defun 9p-message-type-symbol (value)
  (car (rassq value 9P-MESSAGE-TYPES)))

(defun 9p-read-string (buffer)
  "Read a 9P string from BUFFER.
A 9P string consists of a 2-byte size followed by that many bytes of UTF-8 text."
  (let* ((string-size (9p-gbit16 buffer))
         (utf8-bytes (cl-loop for i from 0 below string-size
                              collect (9p-uint8 buffer))))
    (decode-coding-string (apply #'unibyte-string utf8-bytes) 'utf-8)))