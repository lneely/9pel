;;; vfs.el --- Virtual File System for Emacs

;; This file implements a Virtual File System for Emacs.

(defvar vfs-fileop-registry nil
  "Map of virtual directory paths to their handler functions.")

(defun vfs-valid-path (path)
  "Check if PATH is valid according to virtual filesystem rules, optionally starting with `^`."
  (and (stringp path) 
       (not (string-empty-p path))
       (string-prefix-p "/" path)
       (string-suffix-p "/" path)  
       (not (string-match-p " " (string-trim path)))
       (string= path (downcase path))))

(defun vfs-path-p (path)
  "Check if PATH is part of the virtual filesystem using regex matching. 
   Matching is explicitly case-sensitive."
  (let ((case-fold-search nil))
    (and (vfs-valid-path path)
         (cl-loop for (regex . _) in vfs-fileop-registry
                  for key-is-directory = (string-suffix-p "/" regex)
                  for path-is-directory = (string-suffix-p "/" path)
                  when (and key-is-directory path-is-directory
                            (string-match-p regex path))
                  return t
                  finally return nil))))

(defun vfs-register-directory (path)
  "Register a handler for a specific virtual directory path, ensuring idempotence and validity."
  (if (not (and (string-prefix-p "/" path)
                (string-suffix-p "/" path)
                (not (string-match-p "//" path))))
      (error "Invalid path: %s. Path must start and end with '/' and not contain '//'." path)
    (let* ((existing-entry (assoc path vfs-fileop-registry))
           (pattern-exists-in-handler
            (seq-some (lambda (pair)
                        (and (string= (car pair) path)
                             (eq (cdr pair) 'vfs-file-name-handler)))
                      file-name-handler-alist)))
      (unless (and existing-entry pattern-exists-in-handler)
        ;; Add to registry if not registered
        (unless existing-entry
          (let ((operations (make-hash-table :test 'equal)))
            (push (cons path operations) vfs-fileop-registry)))
        ;; Add to handler list if not already there
        (unless pattern-exists-in-handler
          (push (cons path 'vfs-file-name-handler)
                file-name-handler-alist))))))

(defun vfs-register-fileop (path operation func)
  "Register a new operation handler for a specific directory."
  (let ((dir-entry (assoc path vfs-fileop-registry)))
    (if dir-entry
        (puthash operation func (cdr dir-entry))
      (error "Directory %s not found in vfs-fileop-registry" path))))

(defun vfs-file-name-handler (operation &rest args)
  "Handle file operations for the virtual file system."
  (if (eq inhibit-file-name-operation operation)
      (let ((inhibit-file-name-handlers
             (cons 'vfs-file-name-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args))
    (let* ((file-name (if (stringp (car args)) (car args) ""))
           (matched-dir nil))
      (catch 'found
        (dolist (dir vfs-fileop-registry)
          (when (string-prefix-p (car dir) file-name)
            (setq matched-dir dir)
            (throw 'found t))))
      (if matched-dir
          (let ((handler (gethash operation (cdr matched-dir))))
            (if handler
                (apply handler args)
              (let ((inhibit-file-name-handlers
                     (cons 'vfs-file-name-handler
                           (and (eq inhibit-file-name-operation operation)
                                inhibit-file-name-handlers)))
                    (inhibit-file-name-operation operation))
                (apply operation args))))
        (let ((inhibit-file-name-handlers
               (cons 'vfs-file-name-handler
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation operation))
          (apply operation args))))))

(defun vfs-find-matching-dir (file-name)
  "Find the matching directory for a given file name."
  (catch 'found
    (dolist (dir vfs-fileop-registry)
      (when (string-prefix-p (car dir) file-name)
        (throw 'found dir)))
    nil))

(defun vfs-file-directory-p (path)
  "Check if PATH is a directory in the vfs using regex matching."
  (message "Checking if %s is a directory" path)
  (cl-some (lambda (dir)
             (and (string-suffix-p "/" dir)
                  (string-match dir path)))
           (map-keys vfs-fileop-registry)))


(provide 'vfs)

;; end vfs.el
