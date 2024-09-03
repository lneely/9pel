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

