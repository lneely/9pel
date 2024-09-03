;; 9p-msg tests 9p messaging

;; (add-to-list 'load-path "..")
;; (require 'ert)
;; (require '9p)
;; (require '9p-client)

;; (ert-deftest test-9p-srv-Tversion ()
;;   "Test the 9P server Tversion/Rversion exchange."
;;   (let ((9p-received-rversion nil)
;;         (client-process nil))

;;     ;; Check if the server is running, start it if not
;;     (unless (and (boundp '9p-server-process)
;;                  9p-server-process
;;                  (process-live-p 9p-server-process))
;;       (should (progn (9p-start-server) t))
;;       (setq server-was-started t))


;;     ;; Create client process
;;     (should
;;      (setq client-process
;;            (make-network-process 
;;             :name "test-client"
;;             :family 'local
;;             :service 9p-server-socket-name
;;             :sentinel (lambda (proc event)
;;                         (message "Client process event: %s" event))
;;             :filter (lambda (proc string)
;;                       (message "Received data from server: %S" string)
;;                       (setq 9p-received-rversion t)))))

;;     ;; Check client connection
;;     (should (eq (process-status client-process) 'open))

;;     ;; Send Tversion
;;     (should (9p-send-Tversion client-process 8192 "9P2000" 128))

;;     ;; Wait for Rversion with timeout
;;     (should
;;      (with-timeout (2 (error "Timeout waiting for Rversion"))
;;        (while (not 9p-received-rversion)
;;          (accept-process-output client-process 0.1))
;;        t))

;;     ;; Check final process status
;;     (should (eq (process-status client-process) 'open))

;;     ;; Clean up
;;     (when (process-live-p client-process)
;;       (delete-process client-process))))



(add-to-list 'load-path "../9p")
(require 'ert)
(require '9p)
(require '9p-client)

(defun test-9p-tversion-integration ()
  "Integration test for 9p-send-Tversion and 9p-recv-Tversion using client and server processes."
  (let* ((test-msize 8192)
         (test-version "9P2000")
         (test-tag 42)
         (timeout 1.0)  ; 1 second timeout
         (start-time (current-time))
         (response-received nil))

    ;; Start the 9P server
    (9p-start-server)

    (unwind-protect
        (progn
          ;; Ensure the server process was created successfully
          (should (and (boundp '9p-server-process) 
                       (processp 9p-server-process)))

          ;; Start the 9P client
          (setq 9p-client-process (9p-start-client))

          ;; Ensure the client process was created successfully
          (should (processp 9p-client-process))

          ;; Send Tversion using the client process
          (9p-send-Tversion 9p-client-process test-msize test-version test-tag)

          ;; Wait for and process the response
          (while (and (not response-received)
                      (< (float-time (time-subtract (current-time) start-time)) timeout))
            (accept-process-output 9p-client-process 0.1)
            (let ((buffer (9p-read-from-socket 9p-client-process)))
              (when buffer
                (9p-log "Read data from server: %s" buffer)
                (let* ((size (9p-gbit32 buffer 0))
                       (type (9p-gbit8 buffer 4))
                       (tag (9p-gbit16 buffer 5))
                       (msize (9p-gbit32 buffer 7))
                       (version-len (9p-gbit16 buffer 11))
                       (version (9p-gstring buffer 13 version-len)))
                  (should (= type (9p-message-type 'Rversion)))
                  (should (= tag 42))
                  (should (= msize 8192))
                  (should (= version-len 6))
                  (should (string= version "9P2000"))
                  (setq response-received t))))))

      ;; Clean up resources in all cases
      (9p-stop-client)
      (9p-stop-server))))

