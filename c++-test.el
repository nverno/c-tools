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
(autoload 'yas-expand "yasnippet")

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 (defmacro with-c++-vars (&rest body)
   (declare (indent defun))
   `(nvp-with-project (:test-re ".*test.*\.cpp" :root "test")
      ,@body))

 (defmacro setup-test-buffer ()
   `(progn
      (setq-local local-abbrev-table boost-test-abbrev-table)
      (nvp-with-local-bindings
        ("C-c C-c" . c++-test-run-unit-test)))))

;; init new test dir / unit test file
(defun c++-test-init (&optional dir)
  (let* ((test-dir (or dir (expand-file-name "test" default-directory)))
         (test-file (expand-file-name "test.cpp" test-dir)))
    (unless (file-exists-p test-file)
      (make-directory test-dir 'parents)
      (with-current-buffer (find-file-other-window test-file)
        (setup-test-buffer)
        (insert "boost_init")
        (pop-to-buffer (current-buffer))
        (call-interactively 'yas-expand)))))

;; -------------------------------------------------------------------
;;; Commands 

;; compile and run boost.test in current buffer. Removes executable after
;; running unless KEEP is non-nil
(defun c++-test-run-unit-test (&optional keep)
  (interactive "P")
  (let* ((out (concat (nvp-bfn) (nvp-with-gnu/w32 ".out" ".exe")))
         (compile-command
         (concat (nvp-program "g++") " -std=c++14 -O3 -s -o " out
                 " " (file-name-nondirectory buffer-file-name)
                 " -lboost_unit_test_framework "
                 "; ./" out (and (not keep) (concat "; rm " out))))
         (compilation-read-command nil))
    (call-interactively 'compile)))

;; run boost.test
;;;###autoload
(defun c++-test-run-unit-tests (arg)
  "Run unit tests in test directory, if more than one prompts for test file.
With prefix don't remove compiled test."
  (interactive "P")
  (with-c++-vars
    (nvp-with-test nil (c++-test-run-unit-test arg))))

;;;###autoload
(defun c++-test-jump-to-test (arg)
  "Jump to tests in test directory, activates boost abbrev table in test buffer.
With prefix, expands snippet for new auto test."
  (interactive "P")
  (with-c++-vars
    (nvp-with-test (c++-test-init)
      (setup-test-buffer)
      (pop-to-buffer (current-buffer))
      (when arg
        (goto-char (point-max))
        (insert "\nbatc")
        (call-interactively 'yas-expand)))))

(defun c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(provide 'c++-test)
;;; c++-test.el ends here
