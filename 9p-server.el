;; Bind the reload-and-restart function to a key
(global-set-key (kbd "C-c 9") '9p-reload-and-restart)

(load-file "./data.el")
(load-file "./log.el")
(load-file "./srv.el")

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

(defun 9p-restart-server ()
  "Restart the 9P server."
  (interactive)
  (when 9p-server-process
    (delete-process 9p-server-process))
  (9p-start-server))

(defun 9p-reload-and-restart ()
  "Reload the current buffer and restart the 9P server."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    (load-file (buffer-file-name)))
  (when 9p-server-process
    (9p-stop-server))
  (9p-start-server)
  (if 9p-server-socket-name
      (message "9P server reloaded and restarted on socket: %s" 9p-server-socket-name)
    (message "9P server reloaded and restarted.")))
