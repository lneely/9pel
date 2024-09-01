;;; vfs-ops-b.el --- Operation handlers for /b directory in VFS

;; This file contains the operation handlers for the /b directory in
;; the Virtual File System.

(require 'vfs)
(require 'vfs-rpc)

(defun vfs-op-b-directory-files (directory &optional full match nosort count)
  "List 'files' (buffer names) in the virtual directory /b."
  (let* ((buffer-names (vfs-rpc-list-buffers))
         (result (if full
                     (mapcar (lambda (name) (concat "/b/" name)) buffer-names)
                   buffer-names)))
    (when match
      (setq result (cl-remove-if-not
                    (lambda (name) (string-match-p match name))
                    result)))
    (if count
        (seq-take result count)
      result)))

(defun vfs-op-b-insert-directory (dir &optional switches wildcard full-directory-p)
  "Simulate inserting directory contents for directories registered in vfs-fileop-registry."
  (when (member dir (map-keys vfs-fileop-registry))
    (mapc (lambda (buffer-name)
            (insert (format "%-19s 0 Emacs Buffer\n" buffer-name)))
          (buffer-list))
    t))  ; Return t to indicate the operation was handled

(defun vfs-op-b-file-directory-p (filename)
  "Check if the path in /b/ is a directory."
  (string= filename "/b/"))

(defun vfs-op-b-file-attributes (filename &optional id-format)
  "Get file attributes for paths in /b/."
  (let* ((is-root (string= filename "/b/"))
         (buffer-name (unless is-root (substring filename 3)))  ; Remove "/b/" prefix
         (buffer (unless is-root (get-buffer buffer-name)))
         (size (if buffer (buffer-size buffer) 0))
         (mtime (current-time))
         (modes (if is-root #o040755 #o0100644)))  ; drwxr-xr-x for dir, -rw-r--r-- for files
    (list (if is-root t nil)  ; Type (t for directory, nil for regular file)
          1    ; Link count
          0    ; UID
          0    ; GID
          mtime  ; Access time
          mtime  ; Modification time
          mtime  ; Status change time
          size   ; Size
          modes  ; Modes
          nil    ; No extra ACL info
          (file-system-info filename)  ; Use the built-in file-system-info
          nil    ; inode number (nil as it's virtual)
          1)))   ; Device number

(defun vfs-op-b-file-system-info (filename)
  "Return file system info for the virtual /b/ directory."
  (let* ((total-ram (memory-info))  ; Get total RAM in bytes
         (free-ram (memory-info t)) ; Get free RAM in bytes
         (used-ram (- total-ram free-ram))
         ;; Reserve some space to ensure we don't report completely full
         (reserved-space (* 1024 1024 10))  ; Reserve 10 MB
         (avail-ram (max 0 (- free-ram reserved-space))))
    (list total-ram  ; Total space
          avail-ram  ; Free space
          avail-ram  ; Available space
          8192)))    ; Optimal transfer block size (arbitrary, using common 8K)


(provide 'vfs-ops-b)

;;; vfs-ops-b.el ends here

