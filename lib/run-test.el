(require 'cl)
(require 'compile)
(require 'ansi-color)

(defconst run-test-version "0.0.2"
  "Version numbers of this version of run-test.el.")

(defvar run-test-suffixes '("" ".scm" ".rb" ".py" ".sh")
  "List of test file suffix.")

(defvar run-test-file-names '("test/run-test" "tests/run-test"
                              "test/runner" "run-test")
  "List of invoked file name by run-test.")

(defvar run-test-verbose-level-table '((0 . "-vs")
                                       (1 . "")
                                       (2 . "-vp")
                                       (3 . "-vn")
                                       (4 . "-vv"))
  "Passed argumets to run-test-file-names for set verbose level.")

(defconst run-test-error-regexp-alist-alist
  `((ruby-test-unit-failure
     "^test_.+(.+) \\[\\(\\(.+\\):\\([0-9]+\\)\\)\\]:$" 2 3 nil nil 1)
;;     (ruby-test-unit
;;      "^ +\\[?\\(\\(.+\\.rb\\):\\([0-9]+\\)\\(?::in `[^']+'\\)?\\)"
;;      2 3 nil nil 1)
    ,@compilation-error-regexp-alist-alist)
  "Alist of values for `run-test-error-regexp-alist'.")

(defvar run-test-error-regexp-alist
  (mapcar 'car run-test-error-regexp-alist-alist)
  "Alist that specifies how to match errors in compiler output.")

(defvar run-test-mode-line-color-change-time 5
  "Time to show test result as mode line color.")

(defvar run-test-last-output-in-progress nil)
(defvar run-test-output-status nil)
(defvar run-test-original-mode-line-color nil)
(defvar run-test-restoring-original-mode-line-color 0)

(defvar run-test-finish-functions nil
  "Functions to call when a run-test process finishes.
Each function is called with two arguments: the run-test buffer,
and a string describing how the process finished.")

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
        (string-strip (concat " " (cdr elem)))
      "")))

(defun run-test-executable-file-p (file)
  (and (file-executable-p test-file)
       (not (file-directory-p test-file))))

(defun run-test-find-run-test-files-in-directory (directory filenames)
  (mapcar (lambda (filename)
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

(defun run-test-if-find (test-file-infos verbose-arg runner)
  (cond ((null test-file-infos) nil)
        ((car test-file-infos)
         (let ((test-file-info (car test-file-infos)))
           (let* ((run-test-file (car test-file-info))
                  (test-file (cdr test-file-info))
                  (name-of-mode "run-test")
                  (default-directory
                    (expand-file-name
                     (car (split-string test-file run-test-file)))))
             (save-excursion
               (save-some-buffers)
               (funcall runner
                        (concat (concat "./"
                                        (file-name-directory run-test-file))
                                (file-name-nondirectory test-file)
                                verbose-arg)))
             t)))
        (t (run-test-if-find (cdr test-file-infos) verbose-arg runner))))

(defun run-test-setup-buffer ()
  (make-local-variable 'compilation-filter-hook)
  (add-hook 'compilation-filter-hook
            '(lambda ()
               (let ((current (point)))
                 (ansi-color-apply-on-region (- current (string-bytes string))
                                             current)
                 (run-test-update-mode-line string)))))

(defun run-test (&optional arg)
  (interactive "P")
  (run-test-if-find
   (run-test-find-test-files)
   (run-test-get-verbose-level-arg (prefix-numeric-value arg))
   (lambda (command)
     (compilation-start command 'run-test-mode))))

(defun run-test-guess-status (string)
  (let ((case-fold-search nil)
        (target-string (if (string-match "\n" string)
                           (substring string 0 (match-beginning 0))
                         string)))
    (cond ((string-match "E" target-string) 'error)
          ((string-match "F" target-string) 'failure)
          ((string-match "P" target-string) 'pending)
          ((string-match "O" target-string) 'omission)
          ((string-match "N" target-string) 'notification)
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
  (while (and (string-match "^\\(Loaded suite\\|Started\\| *\n\\)" string)
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

(defun run-test-in-mini-buffer (&optional arg)
  (interactive "P")
  (run-test-if-find (find-test-files)
                    (run-test-get-verbose-level-arg (prefix-numeric-value arg))
                    (lambda (command)
                      (shell-command command))))

(provide 'run-test)
