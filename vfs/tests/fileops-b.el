(add-to-list 'load-path "..")

;; =====
;; integration tests: operations on /b/
;; =====

(require 'vfs-ops-b)

(ert-deftest test-vfs-b-directory-files ()
  "Integration test for /b/ virtual directory using directory-files."
  (let ((original-buffers (buffer-list)))
    (unwind-protect
        (progn
          (vfs-register-directory "/b/")
          (vfs-register-fileop "/b/"
                               'directory-files
                               'vfs-op-b-directory-files)

          ;; run directory-files and test the output; it should match
          ;; the current buffer list.
          (let* ((result (directory-files "/b/"))
                 (current-buffer-names (mapcar #'buffer-name (buffer-list))))
            (should (equal (sort result #'string<)
                           (sort current-buffer-names #'string<)))

            ;; full path test
            (let ((full-result (directory-files "/b/" t)))
              (should (equal (sort (mapcar (lambda (name) (concat "/b/" name)) current-buffer-names) #'string<)
                             (sort full-result #'string<))))

            ;; match test (assuming there are at least two buffers with names containing numbers)
            (let* ((numbered-buffers (cl-remove-if-not (lambda (name) (string-match-p "[0-9]" name)) current-buffer-names))
                   (match-pattern (if (>= (length numbered-buffers) 2)
                                      (concat (regexp-quote (substring (car numbered-buffers) 0 1)) ".*[0-9]")
                                    ".*")) ; fallback pattern if not enough numbered buffers
                   (match-result (directory-files "/b/" nil match-pattern)))
              (should (>= (length match-result) 2))
              (should (cl-every (lambda (name) (string-match-p match-pattern name)) match-result)))

            ;; count test
            (let ((count-result (directory-files "/b/" nil nil nil 2)))
              (should (= (length count-result) 2)))))
      )))

(ert-run-tests-interactively t)
