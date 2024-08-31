;; rpc.el -- defines the RPC functionality exposed by the vfs

(defun vfs-rpc-list-buffers ()
  "List all current buffers as if they are files in a directory."
  (mapcar #'buffer-name (buffer-list)))

(defun vfs-rpc-open-buffer (buffer-name)
  "Open a buffer as if it were a file."
  (interactive
   (list (completing-read "Open buffer: " (vfs-rpc-list-buffers))))
  (switch-to-buffer buffer-name))

(defun vfs-rpc-read-buffer (buffer-name)
  "Read the contents of the buffer as if it were a file."
  (with-current-buffer buffer-name
    (buffer-string)))

(provide 'vfs-rpc)
