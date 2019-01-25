;;; c-test.el --- C tests -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Last modified: <2019-01-24 22:38:11>
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
  (defvar cunit-abbrev-table)
  (defvar unity-abbrev-table)
  (defvar nvp-abbrev-local-table))
(require 'nvp-test)
(require 'c-tools)
(autoload 'yas-expand "yasnippet")
(autoload 'clang-complete-create-or-update "clang-complete")

;;; TODO:
;; - Generate test abbrevs from macros in header files

;; function to run unit test from test buffer
(defvar c-test-runner 'c-test-default-runner)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  ;; locally set keys in test buffers to run tests
  (defmacro setup-c-test-buffer (type)
    `(progn
       (setq-local local-abbrev-table
                   (symbol-value (intern (concat ,type "-abbrev-table"))))
       (setq-local nvp-abbrev-local-table ,type)
       (nvp-use-local-bindings
         ("C-c C-c" . c-test-run-unit-test))))

  ;; generate function to run unit tests
  ;; Runs tests in current buffer or FILE if non-nil
  (defmacro c-test-runner-fn (name &optional c++ flags libs)
    (declare (indent defun))
    (let ((fn (nvp-string-or-symbol name)))
      `(progn
         ;; (,'declare-function ,fn "")
         (defun ,fn (save &optional file post-compile)
          "Run tests in current buffer or FILE. Don't throw away executable if KEEP
is non-nil."
          (interactive "P")
          (let* ((default-directory (if file (file-name-directory file)
                                      default-directory))
                 (out (c-tools-out-file file))
                 (compile-command
                  (concat
                   (nvp-concat
                    (nvp-program ,(if c++ "g++" "gcc")) " " ,flags " ")
                   " -o " out " " (or file buffer-file-name)
                   (nvp-concat " " ,libs ";")
                   (or save post-compile
                       (concat "./" (file-name-nondirectory out)
                               "; rm " out))))
                 (compilation-read-command nil))
            (call-interactively 'compile)))))))

;; assume first path will be root, eg ~/.local/include:etc
(defun c-local-include-path (path)
  (expand-file-name
   path
   (car (split-string (or (getenv "C_INCLUDE_PATH")
                          (getenv "CPATH")) path-separator t " "))))

;; -------------------------------------------------------------------
;;; Setup Tests

(eval-when-compile
  (defvar yas-selected-text))
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-lookup-snippet "yasnippet")

;; init new test file
(defun c-test-init (type &optional source-file)
  (c-tools-setenv type)
  ;; call after setting test environment to get paths to unit testing
  ;; framework included
  (clang-complete-create-or-update nil 'c-mode '(("-D" . "TEST")))
  (and (fboundp 'irony-cdb-autosetup-compile-options)
       (irony-cdb-autosetup-compile-options))
  (yas-expand-snippet
   (yas-lookup-snippet (concat type "_init") 'c-mode)
   nil nil
   `((include-file ,(file-relative-name source-file)))))

;; called when opening a test buffer
(defun c-test-buffer (type &optional runner)
  (setq-local c-test-runner runner)
  (setup-c-test-buffer type))

;;;###autoload(autoload 'nvp-project-c-unity-setup "c-test")
(nvp-define-project c-unity
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'c-test-init "unity")
  :test-buffer-function (apply-partially 'c-test-buffer
                                         "unity" 'c-test-run-unity-test)
  :test-run-unit-function 'c-test-run-unity-test)

(nvp-define-project c-check
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'c-test-init "check")
  :test-buffer-function (apply-partially 'c-test-buffer "check")
  :test-run-unit-function 'c-test-default-runner)

(nvp-define-project c-cunit
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'c-test-init "cunit")
  :test-buffer-function (apply-partially 'c-test-buffer "cunit")
  :test-run-unit-function 'c-test-default-runner)

;; -------------------------------------------------------------------
;;; Commands

(defun c-test-run-unit-test (arg)
  (interactive "P")
  (funcall-interactively c-test-runner arg))

;; commands to run unit test
(c-test-runner-fn c-test-default-runner nil
  "-Werror -Wall -std=c11 -O2 -s -DTEST -I.")

(c-test-runner-fn c-test-run-unity-test nil
  (concat
   "-Werror -Wall -std=c11 -O2 -s -DTEST -I. -I"
   (c-local-include-path "unity/src") " "
   (c-local-include-path "unity/src/unity.c")))

;; compile associated unit test and run valgrind on it
(defun c-test-run-valgrind (file)
  (interactive
   (list (if (nvp-test-file-p)
             (buffer-file-name)
           (or (ignore-errors
                 (nvp-test-find-matching-test
                  (buffer-file-name) (nvp-test-dir 'local)))
               (buffer-file-name)))))
  (funcall-interactively
   nvp-test-run-unit-function current-prefix-arg
   file (concat "valgrind " (c-tools-out-file file))))

(defun c-test-help-online ()
  (interactive)
  (browse-url "https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityAssertionsCheatSheetSuitableforPrintingandPossiblyFraming.pdf")
  ;; (browse-url "https://libcheck.github.io/check/index.html")
  )

(provide 'c-test)
;;; c-test.el ends here
