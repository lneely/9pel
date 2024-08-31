(add-to-list 'load-path "..")

;; =====
;; integration tests: operations on /b/
;; =====

(require 'vfs-ops-b)

(ert-deftest test-vfs-b-directory-files ()
  "Integration test for /b/ virtual directory using directory-files."
  (let ((test-buffers '("test-b-buffer-1" "test-b-buffer-2" "test-b-buffer-3"))
        (original-buffers (buffer-list)))
    (unwind-protect
        (progn
          ;; get list of current buffers
          (dolist (buf-name test-buffers)
            (get-buffer-create buf-name))

          (vfs-register-directory "/b/")
          (vfs-register-fileop "/b/"
                               'directory-files
                               'vfs-op-b-directory-files)

          ;; run directory-files and test the output; it should match
          ;; the current buffer list.
          (let* ((result (directory-files "/b/"))
                 (current-buffers (mapcar #'buffer-name (buffer-list))))
            (should (equal (sort result #'string<)
                           (sort current-buffers #'string<)))

            ;; the list should be complete
            (dolist (buf-name test-buffers)
              (should (member buf-name result)))

            ;; full path test
            (let ((full-result (directory-files "/b/" t)))
              (should (equal (sort (mapcar (lambda (name) (concat "/b/" name)) current-buffers) #'string<)
                             (sort full-result #'string<))))

            ;; match test
            (let ((match-result (directory-files "/b/" nil "test-b-buffer-[12]")))
              (should (equal (sort match-result #'string<)
                             '("test-b-buffer-1" "test-b-buffer-2"))))

            ;; count test
            (let ((count-result (directory-files "/b/" nil nil nil 2)))
              (should (= (length count-result) 2)))))

      ;; mop up
      (dolist (buf-name test-buffers)
        (when (get-buffer buf-name)
          (kill-buffer buf-name))))))

(ert-run-tests-interactively t)
