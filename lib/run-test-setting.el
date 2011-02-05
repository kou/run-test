;;; Copyright (C) 2008-2011  Kouhei Sutou <kou@cozmixng.org>
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

;; autoload
(autoload 'run-test "run-test" nil t)
(autoload 'run-test-in-new-frame "run-test" nil t)
(autoload 'run-test-in-mini-buffer "run-test" nil t)

;; key bindings
(define-key global-map "\C-c\C-t" 'run-test)
(define-key global-map "\C-cT" 'run-test-in-new-frame)
(define-key global-map "\C-[\M-\C-t" 'run-test-in-mini-buffer)

(provide 'run-test-setting)
