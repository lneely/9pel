;;; rpc.el -- defines the RPC functionality exposed by the vfs

;;; Commentary:
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; This file contains the operation handlers for the /b directory in
;;; the Virtual File System.

;;; Code:

(defun vfs-rpc-list-buffers ()
  "List all current buffers as if they are files in a directory."
  (mapcar #'buffer-name (buffer-list)))

(defun vfs-rpc-open-buffer (buffer-name)
  "Open a buffer as if it were a file."
  (interactive
   (list (completing-read "Open buffer: " (vfs-rpc-list-buffers))))
  (switch-to-buffer buffer-name))

(defun vfs-rpc-read-buffer (buffer-name)
  "Read the contents of the buffer as if it were a file."
  (with-current-buffer buffer-name
    (buffer-string)))

(provide 'vfs-rpc)

;;; vfs-rpc.el ends here.
