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
  (require 'cl-lib))
(require 'nvp-test)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 (defmacro with-c++-vars (&rest body)
   (declare (indent defun))
   `(nvp-with-project (:test-re ".*test.*\.cpp" :root "test")
      ,@body)))

;; compile and run boost.test in current buffer
(defun c++-test-run-unit-test ()
  (let* ((out (concat (nvp-bfn) (nvp-with-gnu/w32 ".out" ".exe")))
         (compile-command
         (concat (nvp-program "g++") " -std=c++11 -O3 -s -o " out
                 " " (file-name-nondirectory buffer-file-name)
                 " -lboost_unit_test_framework "
                 "; ./" out "; rm " out))
         (compilation-read-command nil))
    (call-interactively 'compile)))

;; -------------------------------------------------------------------
;;; Commands 

;; run boost.test
;;;###autoload
(defun c++-test-run-unit-tests ()
  "Run unit tests in test directory, if more than one prompts for test file."
  (interactive)
  (with-c++-vars
    (nvp-with-test nil (c++-test-run-unit-test))))

;;;###autoload
(defun c++-test-jump-to-test ()
  "Jump to tests in test directory."
  (interactive)
  (with-c++-vars
    (nvp-with-test nil
      (setq-local local-abbrev-table boost-abbrev-table)
      (pop-to-buffer (current-buffer)))))

(provide 'c++-test)
;;; c++-test.el ends here
