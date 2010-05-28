;;; Copyright (C) 2008-2010  Kouhei Sutou <kou@cozmixng.org>
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

(require 'cl)

(defvar flavor
  (cond ((featurep 'xemacs) "xemacs")
        ((boundp 'MULE) "mule")
        (t "emacs")))

(defvar prefix
  (expand-file-name ".." invocation-directory))

(defvar lisp-dir
  (concat prefix "/share/" flavor "/site-lisp/run-test"))

(defvar this-file
  (car (last current-load-list)))

(defvar target-dir
  (expand-file-name "lib" (file-name-directory this-file)))

(defun ensure-directory (directory)
  (unless (file-exists-p directory)
    (message "making directory: %s" directory)
    (make-directory directory t)))

(defun install-file (file directory)
  (let ((dest (expand-file-name (file-name-nondirectory file) directory)))
    (message "installing file: %s -> %s" file dest)
    (copy-file file dest t)))

(defun install-files (src-dir dest-dir)
  (ensure-directory dest-dir)
  (mapcar (lambda (entry)
            (let ((absolete-path (expand-file-name entry src-dir)))
              (cond ((equal "." (substring entry 0 1)) nil)
                    ((file-directory-p absolete-path)
                     (install-files absolete-path
                                    (expand-file-name entry dest-dir)))
                    (t
                     (install-file absolete-path dest-dir)))))
          (directory-files src-dir)))

(defun install ()
  (install-files target-dir lisp-dir))

(defun delete-directory-recursive (directory)
  (mapcar (lambda (entry)
            (let ((absolete-path (expand-file-name entry directory)))
              (cond ((equal "." (substring entry 0 1)) nil)
                    ((file-directory-p absolete-path)
                     (delete-directory-recursive absolete-path))
                    (t
                     (message "deleting file: %s" absolete-path)
                     (delete-file absolete-path)))))
          (directory-files directory))
  (message "deleting directory: %s" directory)
  (delete-directory directory))

(defun uninstall ()
  (if (file-exists-p lisp-dir)
      (delete-directory-recursive lisp-dir)))
