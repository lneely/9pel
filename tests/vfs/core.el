;;; core.el -- tests for core vfs functionality.

;;; Commentary:
;;;
;;; Integration tests to verify the core functionality of vfs.

;;; Code:

(add-to-list 'load-path "../../vfs/")

(require 'ert)
(require 'vfs)

(ert-deftest test-vfs-path-p ()
  (let ((vfs-fileop-registry nil))
    (vfs-register-directory "/b/")
    (vfs-register-directory "/test/")
    (vfs-register-fileop "/b/" 'directory-files 'vfs-op-b-directory-files)
    (vfs-register-fileop "/b/" 'insert-directory 'vfs-op-b-insert-directory)


    ;; Now perform the test assertions
    (should (vfs-path-p "/b/"))         ;; Directory registered
    (should (vfs-path-p "/test/"))
    (should (vfs-path-p "/b/subdir1/")) ;; Subdirectory
    (should-not (vfs-path-p "/b"))      ;; Dirs require trailing slash
    (should-not (vfs-path-p "/bogus"))  ;; Not registered path
    (should-not (vfs-path-p ""))        ;; Edge case: empty string
    (should-not (vfs-path-p nil))       ;; Edge case: nil
    (should-not (vfs-path-p " /b/ "))   ;; Edge case: extra space
    (should-not (vfs-path-p "/B/"))     ;; Edge case: case-sensitivity
    ))

(ert-deftest test-vfs-register-directory ()
  "Test the modified vfs-register-directory function."
  (let ((vfs-fileop-registry nil)
        (file-name-handler-alist nil))
    
    ;; Test valid directory registration
    (vfs-register-directory "/home/user/")
    (should (assoc "/home/user/" vfs-fileop-registry))
    (should (assoc "/home/user/" file-name-handler-alist))
    
    ;; Test idempotence (registering the same directory twice)
    (vfs-register-directory "/home/user/")
    (should (= 1 (length (cl-remove-if-not
                          (lambda (pair) (equal (car pair) "/home/user/"))
                          file-name-handler-alist))))
    
    ;; Test registering multiple directories
    (vfs-register-directory "/tmp/")
    (vfs-register-directory "/var/log/")
    (should (= 3 (length vfs-fileop-registry)))
    (should (= 3 (length (cl-remove-if-not
                          (lambda (pair) (eq (cdr pair) 'vfs-file-name-handler))
                          file-name-handler-alist))))
    
    ;; Test invalid paths
    (should-error (vfs-register-directory "/invalid") :type 'error)
    (should-error (vfs-register-directory "invalid/") :type 'error)
    (should-error (vfs-register-directory "/invalid//path/") :type 'error)
    (should-error (vfs-register-directory "^/home/") :type 'error)
    
    ;; Test that invalid registrations didn't affect the registry
    (should (= 3 (length vfs-fileop-registry)))
    (should (= 3 (length (cl-remove-if-not
                          (lambda (pair) (eq (cdr pair) 'vfs-file-name-handler))
                          file-name-handler-alist))))
    
    ;; Test exact path matching
    (should (assoc "/home/user/" vfs-fileop-registry))
    (should-not (assoc "/home/" vfs-fileop-registry))
    
    ;; Test file-name-handler-alist entries
    (should (assoc "/home/user/" file-name-handler-alist))
    (should (assoc "/tmp/" file-name-handler-alist))
    (should (assoc "/var/log/" file-name-handler-alist))
    
    ;; Test that all registered handlers point to vfs-file-name-handler
    (should (cl-every (lambda (path)
                        (eq (cdr (assoc path file-name-handler-alist))
                            'vfs-file-name-handler))
                      '("/home/user/" "/tmp/" "/var/log/")))))

(ert-deftest test-vfs-register-fileop-registration ()
  "Test that `vfs-register-fileop` correctly adds a new operation."
  (let ((vfs-fileop-registry (list (cons "/test/path" (make-hash-table :test 'equal)))))
    (vfs-register-fileop "/test/path" 'directory-files #'identity)
    (let ((operations (cdr (assoc "/test/path" vfs-fileop-registry))))
      (should (equal (gethash 'directory-files operations) #'identity)))))

(ert-deftest test-vfs-register-fileop-nonexistent ()
  "Test that `vfs-register-fileop` errors out for a nonexistent directory."
  (let ((vfs-fileop-registry nil))
    (should-error (vfs-register-fileop "/nonexistent/path" 'any-op #'any-function))))

(ert-deftest test-vfs-find-matching-dir ()
  "Test that `vfs-find-matching-dir` finds the correct directory based on file name prefixes."
  (let ((vfs-fileop-registry nil))  ;; Start with an empty registry
    ;; Register some directories
    (vfs-register-directory "/test/")
    (vfs-register-directory "/test/subdir/")
    (vfs-register-directory "/another/dir/")
    
    ;; Test finding the matching directory
    (should (equal (car (vfs-find-matching-dir "/test/file.txt")) "/test/"))
    (should (equal (car (vfs-find-matching-dir "/test/subdir/file.txt")) "/test/subdir/"))
    (should (equal (car (vfs-find-matching-dir "/another/dir/file.txt")) "/another/dir/"))

    ;; Test a file name that does not match any registered directories
    (should-not (vfs-find-matching-dir "/unrelated/file.txt"))))

(provide 'core)

;;; core.el ends here



