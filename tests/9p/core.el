;;; core.el --- Core functionality for 9P server implementation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file tests core functionality for the 9P server implementation.

;;; Code:

(add-to-list 'load-path "../../9p")
(add-to-list 'load-path "../../vfs")

(require 'ert)
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

(defun test-9p-in-namespace-p ()
	"Test path validation."
	(should (9p-in-namespace-p 9p-root-namespace))

	;; rootfs is not in namespace
	(should-not (9p-in-namespace-p "/"))
	(should-not (9p-in-namespace-p "/home/"))

	;; rewritten paths are in namespace
	(should (9p-in-namespace-p (9p-rewrite-path "/")))
	(should (9p-in-namespace-p (9p-rewrite-path "/home/"))))
														 
														 

(defun test-9p-rewrite-path ()
	"Test path rewrite."
	(should (string= (9p-rewrite-path "/") 9p-root-namespace))
	(should (string= (9p-rewrite-path "/etc/")
									 (concat 9p-root-namespace "etc/"))))

(provide 'core)

;;; core.el ends here
