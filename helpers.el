;; helpers.el contains helper functions.

;; 9p-ensure-32bit guarantees that the value is a 32-bit integer.
(defun 9p-ensure-32bit (value)
  "Ensure VALUE is a 32-bit integer."
  (logand value #xFFFFFFFF))

;; 9p-ensure-16bit guarantees that the value is a 16-bit integer.
(defun 9p-ensure-16bit (value)
  "Ensure VALUE is a 16-bit integer."
  (logand value #xFFFF))

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
