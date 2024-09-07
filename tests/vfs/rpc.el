;;; rpc.el -- tests for vfs rpc calls.

;;; Commentary:
;;;
;;; Integration tests to verify the rpc operations in vfs function as
;;; expected.

;;; Code:

(add-to-list 'load-path "../../vfs")

(require 'vfs-rpc)

;; ensure that vfs-rpc-list-buffers returns the expected list of
;; buffers from emacs.
(ert-deftest test-vfs-rpc-list-buffers ()
  "Test the vfs-rpc-list-buffers function."
  (let ((test-buffers
         '("test-buffer-1" "test-buffer-2" "test-buffer-3"))
        (original-buffers (buffer-list)))
    (unwind-protect
        (progn
          (dolist (buf-name test-buffers)
            (get-buffer-create buf-name))

          (let ((result (vfs-rpc-list-buffers)))
            (dolist (buf-name test-buffers)
              (should (member buf-name result)))

            ;; only strings
            (should (cl-every #'stringp result))

            ;; unique
            (should (equal (length result) (length (cl-remove-duplicates result :test #'string=))))

            ;; result length >= test buffers length
            (should (>= (length result) (length test-buffers)))

            ;; result matches actual buffer list
            (should (equal (sort result #'string<)
                           (sort (mapcar #'buffer-name (buffer-list)) #'string<)))))

      ;; cleanup
      (dolist (buf-name test-buffers)
        (when (get-buffer buf-name)
          (kill-buffer buf-name))))))


(ert-deftest test-vfs-rpc-open-buffer ()
  "Integration test for vfs-rpc-open-buffer function."
  ;; Set up test environment
  (let ((test-buffer-1 (generate-new-buffer "test-buffer-1"))
        (test-buffer-2 (generate-new-buffer "test-buffer-2"))
        (original-buffer (current-buffer)))
    (unwind-protect
        (progn
          ;; Mock vfs-rpc-list-buffers to return our test buffers
          (cl-letf (((symbol-function 'vfs-rpc-list-buffers)
                     (lambda () (list "test-buffer-1" "test-buffer-2"))))
            
            ;; Test opening the first buffer
            (vfs-rpc-open-buffer "test-buffer-1")
            (should (equal (buffer-name (current-buffer)) "test-buffer-1"))
            
            ;; Test opening the second buffer
            (vfs-rpc-open-buffer "test-buffer-2")
            (should (equal (buffer-name (current-buffer)) "test-buffer-2"))
            
            ;; Test with non-existent buffer name
            (let ((non-existent-buffer-name "non-existent-buffer"))
              (vfs-rpc-open-buffer non-existent-buffer-name)
              ;; Check if the function creates a new buffer or signals an error
              (should (or (get-buffer non-existent-buffer-name)
                          (equal (buffer-name (current-buffer)) non-existent-buffer-name)))
              (when (get-buffer non-existent-buffer-name)
                (kill-buffer non-existent-buffer-name)))
            
            ;; Test interactive call (simulated)
            (let ((called-args nil))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest args)
                           (setq called-args args)
                           "test-buffer-1")))
                (call-interactively 'vfs-rpc-open-buffer)
                (should (equal (buffer-name (current-buffer)) "test-buffer-1"))
                (should (equal (car called-args) "Open buffer: "))
                (should (equal (cadr called-args) '("test-buffer-1" "test-buffer-2")))))))
      
      ;; Clean up
      (kill-buffer test-buffer-1)
      (kill-buffer test-buffer-2)
      (switch-to-buffer original-buffer))))

(require 'ert)

(ert-deftest test-vfs-rpc-read-buffer ()
  "Integration test for vfs-rpc-read-buffer function."
  (let ((test-buffer-name "test-vfs-rpc-buffer")
        (test-content "Hello, this is a test content."))
    (unwind-protect
        (progn
          ;; Create a temporary buffer and write content to it
          (with-current-buffer (get-buffer-create test-buffer-name)
            (erase-buffer)
            (insert test-content))

          ;; Test reading the buffer content
          (should (equal (vfs-rpc-read-buffer test-buffer-name) test-content))

          ;; Test with empty buffer
          (with-current-buffer test-buffer-name
            (erase-buffer))
          (should (equal (vfs-rpc-read-buffer test-buffer-name) ""))

          ;; Test with non-existent buffer
          (should-error (vfs-rpc-read-buffer "non-existent-buffer")
                        :type 'error))

      ;; Clean up
      (when (get-buffer test-buffer-name)
        (kill-buffer test-buffer-name)))))

;;; rpc.el ends here
