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
  (require 'c-tools)
  (require 'c-test) ;; setup macros
  (defvar boost-test-abbrev-table))
(require 'nvp-test)
(autoload 'yas-expand "yasnippet")

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 (defmacro with-c++-vars (&rest body)
   (declare (indent defun))
   `(nvp-with-project (:test-re ".*test.*\.cpp"
                       :root '("test" "tests" ".git" ".projectile"))
      ,@body))

 (defmacro setup-c++-test-buffer ()
   `(progn
      (setq-local local-abbrev-table boost-test-abbrev-table)
      (nvp-with-local-bindings
        ("C-c C-c" . c++-test-run-unit-test)))))

;; -------------------------------------------------------------------
;;; Setup Test

(defun c++-test-init ()
  (insert "boost_init")
  (call-interactively 'yas-expand))

;; if new expand template for new test
(defun c++-test-buffer (&optional new)
  (setup-c++-test-buffer)
  (when (or current-prefix-arg new)
    (goto-char (point-max))
    (insert "\nbatc")
    (call-interactively 'yas-expand)))

;;;###autoload(autoload 'nvp-project-c++-boost-setup "c++-test")
(nvp-define-project c++-boost
  :test-fmt "test_%s"
  :test-init-function 'c++-test-init
  :test-buffer-function 'c++-test-buffer
  :test-run-unit-function 'c++-test-run-unit-test)

;; -------------------------------------------------------------------
;;; Commands 

(defun c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(c-test-runner-fn c++-test-run-unit-test 'c++
  ;; flags
  "-std=c++14 -O3 -s"
  ;; link
  "-lboost_unit_test_framework")

(provide 'c++-test)
;;; c++-test.el ends here
