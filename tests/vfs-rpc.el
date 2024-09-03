;; ;; =====
;; ;; integration tests: rpc
;; ;; =====


;; (add-to-list 'load-path "../vfs")
;; (require 'vfs-rpc)

;; (ert-deftest test-vfs-rpc-list-buffers ()
;;   "Test the vfs-rpc-list-buffers function."
;;   (let ((test-buffers
;;          '("test-buffer-1" "test-buffer-2" "test-buffer-3"))
;;         (original-buffers (buffer-list)))
;;     (unwind-protect
;;         (progn
;;           (dolist (buf-name test-buffers)
;;             (get-buffer-create buf-name))

;;           (let ((result (vfs-rpc-list-buffers)))
;;             (dolist (buf-name test-buffers)
;;               (should (member buf-name result)))

;;             ;; only strings
;;             (should (cl-every #'stringp result))

;;             ;; unique
;;             (should (equal (length result) (length (cl-remove-duplicates result :test #'string=))))

;;             ;; result length >= test buffers length
;;             (should (>= (length result) (length test-buffers)))

;;             ;; result matches actual buffer list
;;             (should (equal (sort result #'string<)
;;                            (sort (mapcar #'buffer-name (buffer-list)) #'string<)))))

;;       ;; cleanup
;;       (dolist (buf-name test-buffers)
;;         (when (get-buffer buf-name)
;;           (kill-buffer buf-name))))))



