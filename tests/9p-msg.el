(add-to-list 'load-path "../9p")

(require 'ert)
(require '9p)
(require '9p-client)

;; mock server-client interaction to test transmit / respond for
;; version.
(defun test-9p-tversion-integration ()
  "Integration test for 9p-send-Tversion and 9p-recv-Tversion using client and server processes."
  (let* ((test-msize 8192)
         (test-version "9P2000")
         (test-tag 42)
         (timeout 1.0)  ; 1 second timeout
         (start-time (current-time))
         (response-received nil))

    (9p-start-server)
    (unwind-protect
        (progn
          (should (and (boundp '9p-server-process) 
                       (processp 9p-server-process)))
          (setq 9p-client-process (9p-start-client))
          (should (processp 9p-client-process))
          (9p-send-Tversion 9p-client-process test-msize test-version test-tag)
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
      (9p-stop-client)
      (9p-stop-server))))


