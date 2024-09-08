;;; core.el --- Core functionality for 9P server implementation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file tests core functionality for the 9P server implementation.

;;; Code:

(require '9p-util)
(require '9p-server)
(require 'vfs)
(require 'vfs-ops-b)

(defun test-9p-qid ()
	"Test 9p qid generation for a path in the vfs."
	(let* ((vfs-fileop-registry nil)
				 (test-dirs '("/b/"))
				 (test-files (buffer-list)))

		(vfs-register-directory "/b/")
		(vfs-register-fileop "/b/" 'directory-files 'vfs-op-b-directory-files)
		(vfs-register-fileop "/b/" 'file-attributes 'vfs-op-b-file-attributes)
		(should (vfs-path-p "/b/"))

		(dolist (test-path test-dirs)
			(setq qid (9p-qid test-path))
			(should (= (nth 0 qid) 9P-QTDIR))
			(should (nth 1 qid))
			(should (= (nth 2 qid) (sxhash test-path))))

		(dolist (test-path test-files)
			(setq qid (9p-qid test-path))
			(should (= (nth 0 qid) 9P-QTFILE))
			(should (nth 1 qid))
			(should (= (nth 2 qid) (sxhash test-path))))))

(provide 'core)

;;; core.el ends here
