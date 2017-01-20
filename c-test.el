;;; c-test ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 20 January 2017

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
  (defvar check-abbrev-table)
  (defvar cunit-abbrev-table))
(require 'nvp-test)
(autoload 'yas-expand "yasnippet")

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  ;; setup mode specific dynamic variables
  (defmacro with-c-vars (&rest body)
    (declare (indent defun))
    `(nvp-with-project (:test-re ".*\\(?:test\\|check\\).*\.c"
                                 :root '("test" "tests" ".git" ".projectile"))
       ,@body))

  ;; locally set keys in test buffers to run tests
  (defmacro setup-c-test-buffer (&optional cunit)
    `(progn
       (setq-local local-abbrev-table ,(if cunit 'cunit-abbrev-table
                                         'check-abbrev-table))
       (setq-local nvp-abbrev-local-table ,(if cunit "cunit" "check"))
       (nvp-with-local-bindings
         ("C-c C-c" . c-test-run-unit-test))))

  ;; do BODY in test file, creating new test directory/file if necessary
  (defmacro with-c-test-file (filename dirname &rest body)
    (declare (indent defun))
    `(let* ((test-dir (or ,dirname (expand-file-name "test" default-directory)))
            (test-file (expand-file-name ,filename test-dir)))
       (unless (file-exists-p test-file)
         (make-directory test-dir 'parents)
         (with-current-buffer (find-file-other-window test-file)
           ,@body))))

  ;; generate function to run unit tests
  (defmacro c-test-runner-fn (name &optional c++ flags libs)
    (declare (indent defun))
    (let ((fn (nvp-string-or-symbol name)))
      `(defun ,fn (&optional keep)
         (interactive "P")
         (let* ((out (concat (nvp-bfn) (nvp-with-gnu/w32 ".out" ".exe")))
                (compile-command
                 (concat
                  (nvp-concat
                   (nvp-program ,(if c++ "g++" "gcc")) " " ,flags " ")
                  out " " (file-name-nondirectory buffer-file-name) " "
                  (nvp-concat ,libs "; ./")
                  out (and (not keep) (concat "; rm " out))))
                (compilation-read-command nil))
           (call-interactively 'compile)))))

  ;; generate functions to run unit tests/jump to tests.
  ;; BODY runs when jumping to test with prefix (eg. init a template)
  (defmacro c-test-fns (&optional c++ flags libs &rest body)
    (declare (indent defun))
    (let* ((prefix (if c++ "c++-" "c-"))
           (run1-fn (intern (concat prefix "test-run-unit-test")))
           (run-fn (intern (concat prefix "test-run-unit-tests")))
           (jump-fn (intern (concat prefix "test-jump-to-test")))
           (init-fn (intern (concat prefix "test-init")))
           (with-vars (intern (concat "with-" prefix "vars")))
           (setup (intern (concat "setup-" prefix "test-buffer"))))
     `(progn
        (c-test-runner-fn ,run1-fn ,c++ ,flags ,libs)

        (defun ,run-fn (arg)
          "Run unit tests in test directory. If more than one test, prompts for
test file. With prefix, doesn't remove compiled test after running."
          (interactive "P")
          (,with-vars (nvp-with-test nil (funcall ',run1-fn arg))))

        (defun ,jump-fn (arg)
          "Jump to tests in test directory, activating test abbrev table in 
test buffer. With prefix, init template for new test."
          (interactive "P")
          (,with-vars
           (nvp-with-test (funcall ',init-fn)
             (,setup)
             (pop-to-buffer (current-buffer))
             (when arg
               ,@body))))))))

;; init new test dir / unit test file
(defun c-test-init (&optional cunit dir)
  (with-c-test-file "test.c" dir
    (setup-c-test-buffer)
    (insert (if cunit "cunit_init" "check_init"))
    (pop-to-buffer (current-buffer))
    (call-interactively 'yas-expand)))

;; -------------------------------------------------------------------
;;; Commands

(c-test-fns nil
  ;; flags
  "-std=c11 -O2 -s -o"
  ;; link
  nil
  ;; init new test case
  (insert "In progress\n"))

;;;###autoload(autoload 'c-test-jump-to-test "c-test")
;;;###autoload(autoload 'c-test-run-unit-tests "c-test")

(defun c-test-help-online ()
  (interactive)
  (browse-url "https://libcheck.github.io/check/index.html"))

(provide 'c-test)
;;; c-test.el ends here
