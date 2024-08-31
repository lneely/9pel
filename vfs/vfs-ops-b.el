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

(provide 'vfs-ops-b)

;;; vfs-ops-b.el ends here
