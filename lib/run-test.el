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
(require 'compile)
(require 'ansi-color)

(defconst run-test-version "0.0.2"
  "Version numbers of this version of run-test.el.")

(defvar run-test-suffixes '("" ".sh" ".scm" ".rb" ".py")
  "List of test file suffix.")

(defvar run-test-file-names '("test/run-test" "tests/run-test"
                              "test/runner" "run-test")
  "List of invoked file name by run-test.")

(defvar run-test-verbose-level-table '((0 . "--verbose=silent")
                                       (1 . "")
                                       (2 . "--verbose=progress")
                                       (3 . "--verbose=normal")
                                       (4 . "--verbose=verbose"))
  "passed argumets to run-test-file-names for set verbose level.")

(defconst run-test-error-regexp-alist-alist
  `((ruby-test-unit-failure
     "^test_.+(.+) \\[\\(\\(.+\\):\\([0-9]+\\)\\)\\]:$" 2 3 nil nil 1)
;;     (ruby-test-unit
;;      "^ +\\[?\\(\\(.+\\.rb\\):\\([0-9]+\\)\\(?::in `[^']+'\\)?\\)"
;;      2 3 nil nil 1)
    ,@compilation-error-regexp-alist-alist)
  "alist of values for `run-test-error-regexp-alist'.")

(defvar run-test-error-regexp-alist
  (mapcar 'car run-test-error-regexp-alist-alist)
  "alist that specifies how to match errors in compiler output.")

(defvar run-test-mode-line-color-change-time 5
  "time to show test result as mode line color.")

(defvar run-test-last-output-in-progress nil)
(defvar run-test-output-status nil)
(defvar run-test-original-mode-line-color nil)
(defvar run-test-restoring-original-mode-line-color 0)

(defvar run-test-finish-functions nil
  "functions to call when a run-test process finishes.
each function is called with two arguments: the run-test buffer,
and a string describing how the process finished.")

(defvar run-test-command-history nil
  "history of run-test commands.")

(define-compilation-mode run-test-mode "run-test" "run-test-mode"
  (set (make-local-variable 'run-test-last-output-in-progress) t)
  (set (make-local-variable 'run-test-output-status) 'success)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-process-setup-function)
       'run-test-setup-buffer))

(defun flatten (lst)
  (cond ((null lst) '())
        ((listp (car lst))
         (append (flatten (car lst))
                 (flatten (cdr lst))))
        (t (cons (car lst) (flatten (cdr lst))))))

(defun string-strip (string)
  (replace-regexp-in-string "\\(^\s*\\|\s*$\\)"
                            ""
                            string))

(defun run-test-get-verbose-level-arg (num)
  (let ((elem (assoc num run-test-verbose-level-table)))
    (if elem
        (concat " " (string-strip (cdr elem)))
      "")))

(defun run-test-executable-file-p (file)
  (and (file-executable-p test-file)
       (not (file-directory-p test-file))))

(defun run-test-find-run-test-files-in-directory (directory filenames)
  (let ((tests
	 (mapcar
	  (lambda (filename)
            (do ((test-file (concat directory filename)
                            (concat "../" test-file))
                 (rest-dir filename (and (string-match "\/\(.*\)" rest-dir)
                                         (match-string 1))))
                ((or (run-test-executable-file-p test-file)
                     (null rest-dir))
                 (if (run-test-executable-file-p test-file)
                     (cons filename test-file)
                   nil))))
          filenames))
	(compact
	 (lambda (list)
	   (cond ((null list) nil)
		 ((car list)
		  (setcdr list (funcall compact (cdr list)))
		  list)
		 ((funcall compact (cdr list)))))))
    (funcall compact tests)))

(defun run-test-find-run-test-files (directory filenames)
  (if (string= "/" (expand-file-name directory))
      nil
    (append (run-test-find-run-test-files-in-directory directory filenames)
            (run-test-find-run-test-files (concat directory "../") filenames))))

(defun run-test-find-test-files ()
  (let ((filenames (mapcar (lambda (filename)
                             (mapcar (lambda (suffix)
                                       (concat filename suffix))
                                     run-test-suffixes))
                           run-test-file-names)))
    (run-test-find-run-test-files "./" (flatten filenames))))

(defun run-test-command (test-file-infos verbose-arg)
  (cond ((null test-file-infos) nil)
        (t
         (let ((test-file-info (car test-file-infos)))
           (let* ((run-test-file (car test-file-info))
                  (test-file (cdr test-file-info)))
	     (list
	      (concat (concat "./"
			      (file-name-directory run-test-file))
		      (file-name-nondirectory test-file)
		      verbose-arg)
	      (expand-file-name
	       (car (split-string test-file run-test-file)))))))))

(defun run-test-read-command (&optional arg)
  (let* ((default
	   (run-test-command (run-test-find-test-files)
			     (run-test-get-verbose-level-arg
			      (prefix-numeric-value arg))))
	 (command (car default))
	 (directory (cadr default)))
    (when arg
      (setq command (read-string "Run test command: " command
                                 'run-test-command-history)
	    directory (read-directory-name "In directory: " directory)))
    (list command directory)))

(defun run-test-setup-buffer ()
  (make-local-variable 'compilation-filter-hook)
  (add-hook 'compilation-filter-hook
            '(lambda ()
               (let ((current (point)))
                 (ansi-color-apply-on-region (- current (string-bytes string))
                                             current)
                 (run-test-update-mode-line string)))))

(defun run-test-guess-status (string)
  (let ((case-fold-search nil)
        (target-string (if (string-match "\n" string)
                           (substring string 0 (match-beginning 0))
                         string)))
    (cond ((string-match "e" target-string) 'error)
          ((string-match "f" target-string) 'failure)
          ((string-match "p" target-string) 'pending)
          ((string-match "o" target-string) 'omission)
          ((string-match "n" target-string) 'notification)
          ((string-match "\\." target-string) 'success)
          (t nil))))

(defvar run-test-statuses '((error "yellow")
                            (failure "red")
                            (pending "magenta")
                            (omission "blue")
                            (notification "cyan")
                            (success "green")))

(defun run-test-compare-status (status1 status2)
  (let ((max-position (length run-test-statuses))
        (status-names (mapcar 'car run-test-statuses)))
    (<= (or (position status1 status-names) max-position)
        (or (position status2 status-names) max-position))))

(defun run-test-status-color (status)
  (let ((status-info (find-if (lambda (status-info)
                                (eq (car status-info) status))
                              run-test-statuses)))
    (and status-info (cadr status-info))))

(defun run-test-remove-start-message (string)
  (while (and (string-match "^\\(loaded suite\\|started\\| *\n\\)" string)
              (zerop (match-beginning 0)))
    (if (string-match "\n" string)
        (setq string (substring string (match-end 0)))
      (setq string "")))
  string)

(defun run-test-update-mode-line (string)
  (when run-test-last-output-in-progress
    (let* ((string (run-test-remove-start-message string))
           (status (run-test-guess-status string)))
      (if status
          (setq run-test-output-status
                (car (sort (list run-test-output-status status)
                           'run-test-compare-status))))
      (if (and (not (string-equal "" string))
               (or (not status) (string-match "\n" string)))
          (setq run-test-last-output-in-progress nil)))
    (unless run-test-original-mode-line-color
      (setq run-test-original-mode-line-color (face-background 'mode-line)))
    (let ((mode-line-background-color
           (run-test-status-color run-test-output-status)))
      (if mode-line-background-color
          (set-face-background 'mode-line mode-line-background-color)))))

(defun run-test-restore-mode-line-color (cur-buffer msg)
  (when (eq major-mode 'run-test-mode)
    (when run-test-original-mode-line-color
      (setq run-test-restoring-original-mode-line-color
            (+ 1 run-test-restoring-original-mode-line-color))
      (add-timeout run-test-mode-line-color-change-time
                   (lambda (color)
                     (setq run-test-restoring-original-mode-line-color
                           (- run-test-restoring-original-mode-line-color 1))
                     (when (zerop run-test-restoring-original-mode-line-color)
                       (set-face-background 'mode-line color)
                       (setq run-test-original-mode-line-color nil)))
                 run-test-original-mode-line-color))))

(add-hook 'run-test-finish-functions 'run-test-restore-mode-line-color)

(defun run-test-in-new-frame (&optional arg)
  (interactive "P")
  (let ((run-test-buffer-name "*run-test*"))
    (if (member run-test-buffer-name
                (mapcar 'buffer-name (buffer-list)))
        (kill-buffer run-test-buffer-name)))
  (let ((current-frame (car (frame-list)))
        (target-directory (cadr (split-string (pwd))))
        (frame (make-frame)))
    (select-frame frame)
    (cd target-directory)
    (if (null (run-test arg))
        (delete-frame frame)
      (delete-window)
      (other-frame -1)
      (select-frame current-frame))))

(defmacro run-test-command-start (directory &rest command)
  `(let ((name-of-mode "run-test")
	 (default-directory ,directory))
     (save-excursion
       (save-some-buffers)
       ,@command)))

(defun run-test (command directory)
  (interactive (run-test-read-command current-prefix-arg))
  (run-test-command-start directory
    (compilation-start command 'run-test-mode)))

(defun run-test-in-mini-buffer (command directory)
  (interactive (run-test-read-command current-prefix-arg))
  (run-test-command-start directory
    (shell-command command)))

(provide 'run-test)
