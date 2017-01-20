;;; c++-test ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 17 January 2017

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'c-test) ;; setup macros
  (defvar boost-test-abbrev-table))
(require 'nvp-test)
(autoload 'yas-expand "yasnippet")

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 (defmacro with-c++-vars (&rest body)
   (declare (indent defun))
   `(nvp-with-project (:test-re ".*test.*\.cpp" :root "test")
      ,@body))

 (defmacro setup-c++-test-buffer ()
   `(progn
      (setq-local local-abbrev-table boost-test-abbrev-table)
      (nvp-with-local-bindings
        ("C-c C-c" . c++-test-run-unit-test)))))

;; init new test dir / unit test file
(defun c++-test-init (&optional dir)
  (with-c-test-file "test.cpp" dir
    (setup-c++-test-buffer)
    (insert "boost_init")
    (pop-to-buffer (current-buffer))
    (call-interactively 'yas-expand)))

;; -------------------------------------------------------------------
;;; Commands 

(defun c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(c-test-fns 'c++
  ;; flags
  "-std=c++14 -O3 -s -o"
  ;; link
  "-lboost_unit_test_framework"
  ;; the rest inits a new test case when jumping with prefix
  (goto-char (point-max))
  (insert "\nbatc")
  (call-interactively 'yas-expand))

;;;###autoload(autoload 'c++-test-jump-to-test "c++-test")
;;;###autoload(autoload 'c++-test-run-unit-tests "c++-test")

(provide 'c++-test)
;;; c++-test.el ends here
