(defun 9p-log (format-string &rest args)
  "Log a message with a timestamp."
  (let ((log-buffer (get-buffer-create "*9P Server Log*")))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (apply #'format format-string args))))))

(defun 9p-hex-dump (string)
  "Return a hex dump of STRING."
  (mapconcat (lambda (c) (format "%02x" c)) string " "))

(defun 9p-format-tag (tag)
  "Format TAG for logging."
  (if (= tag #xFFFF)
      "NOTAG"
    (format "%d" tag)))
