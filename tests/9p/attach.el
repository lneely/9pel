;;; attach.el -- 9p attach message exchange tests

;;; Commentary:
;;;
;;; Integration tests to verify 9p attach message exchange functions
;;; properly.

;;; Code:

(add-to-list 'load-path "../../9p")

(require 'ert)
(require '9p-server)
(require '9p-client)

(defun test-9p-attach-exchange ()
	"Test attach message exchange."

	(let ((test-fid 9P-NOFID)
				(test-afid 9P-NOFID)
				(test-uname "testuser")
				(test-aname "")
				(test-tag 44))

		(9p-start-server)
		(unwind-protect
				(progn
					(should (and (boundp '9p-server-process)
                       (processp 9p-server-process)))
          (setq 9p-client-process (9p-start-client))
          (should (processp 9p-client-process))
					(9p-send-Tattach 9p-client-process test-tag
													 test-fid test-afid test-uname
													 test-aname)

					(let* ((size (9p-gbit32 buffer 0))
								 (type (9p-gbit8 buffer 4))
								 (tag (9p-gbit16 buffer 5))

								 (should (= type (9p-message-type 'Rattach)))))))))


(provide 'attach)
;;; attach.el ends here
