;;; c++-tools ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 11 November 2016

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
  (require 'nvp-macro))
(require 'c-tools)

;; -------------------------------------------------------------------
;;; Commands 

;;; Compile
;; pretty much same as c-tools-compile
(nvp-make-or-compile-fn c++-tools-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -O2 -g -std=c++14"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "g++")
                  flags out (nvp-with-gnu/w32 ".out" ".exe") file)))
    (nvp-compile-basic)))

(defun c++-tools-compile-and-run (keep)
  (interactive "P")
  (funcall-interactively 'c-tools-compile-and-run keep
                         (nvp-program "g++") "-std=c++14 -O3 -s"))

;; -------------------------------------------------------------------
;;; Font-lock

(defface font-lock-doxygen-face
  '((nil (:foreground "SaddleBrown" :background "#f7f7f7")))
  "Special face to highlight doxygen tags such as <tt>...</tt>
and <code>...</code>."
  :group 'font-lock-highlighting-faces)

;; toggle font-locking for doxygen
(defvar-local c++-tools--add-font t)
(defun c++-tools-doxygen ()
  (interactive)
  (if (setq c++-tools--add-font (not c++-tools--add-font))
      (font-lock-refresh-defaults)
    (font-lock-add-keywords
     ;; 'c++-mode
     nil
     '(("\\(<\\(?:code\\|tt\\)>\"?\\)\\([^<]*?\\)\\(\"?</\\(?:code\\|tt\\)>\\)"
        (0 (prog1 ()
             (let* ((expr (match-string-no-properties 2))
                    (expr-len (length expr)))
               (if (eq 1 expr-len)
                   (compose-region (match-beginning 0)
                                   (match-end 0)
                                   (aref expr 0))
                 (compose-region (match-beginning 1)
                                 (1+ (match-end 1))
                                 (aref expr 0))
                 (compose-region (1- (match-beginning 3))
                                 (match-end 3)
                                 (aref expr (1- expr-len)))))))
        (0 'font-lock-doxygen-face t))))
    (font-lock-flush)
    (font-lock-ensure)))

(provide 'c++-tools)
;;; c++-tools.el ends here
