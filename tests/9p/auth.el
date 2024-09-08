;;; auth.el -- 9p auth message exchange tests

;;; Commentary:
;;;
;;; Integration tests to verify 9p auth message exchange functions
;;; properly.

;;; Code:


(add-to-list 'load-path "../../9p")

(require 'ert)
(require '9p-server)
(require '9p-client)

;; mock server-client interaction to test transmit / respond for
;; auth.
(defun test-9p-auth-exchange ()
	"Test auth message exchange."
  (let* ((test-afid 9P-NOFID)
				 (test-aname "")
				 (test-uname "testuser")
         (test-tag 43)
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
					(9p-send-Tauth 9p-client-process test-afid
												 test-uname test-aname test-tag)
          (while (and (not response-received)
                      (< (float-time (time-subtract (current-time) start-time)) timeout))
            (accept-process-output 9p-client-process timeout)
            (let ((buffer (9p-read-from-socket 9p-client-process)))
              (when buffer
                (9p-log "Read data from server: %s" buffer)
								;; expect a Rerror
                (let* ((size (9p-gbit32 buffer 0))
                       (type (9p-gbit8 buffer 4))
                       (tag (9p-gbit16 buffer 5))
											 (err-len (9p-gbit16 buffer 7))
											 (err (9p-gstring buffer 9 err-len)))

                  (should (= type (9p-message-type 'Rerror)))
									(should (= tag test-tag))
									(should (string= err "No authentication needed"))
									
                  (setq response-received t)))))))
      (9p-stop-client)
      (9p-stop-server)))

;;; auth.el ends here
