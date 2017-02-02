;;; c-help ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created:  2 February 2017

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
(require 'semantic/analyze)

;; sources determined by source file paths
(defvar c-help-sources
  (let ((uri "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s"))
    (cl-loop for p in semantic-c-dependency-system-include-path
       collect (cons p uri))))

;; semantic tag at point
(defsubst c-help-tag-at (point)
  (car (reverse (oref (semantic-analyze-current-context point) prefix))))

;; lookup info in man or online for thing at point
;;;###autoload
(defun c-help-at-point (point &optional online)
  (interactive "d")
  (let* ((tag (c-help-tag-at point))
         (file (and (semantic-tag-p tag)
                    (semantic-tag-file-name tag)))
         (ref (when (stringp file)
                (cl-some (lambda (src)
                           (and (string-prefix-p (car src) file)
                                src))
                         c-help-sources))))
    (if (not ref)
        (message "No documentation source found for %S" tag)
      (if (or online current-prefix-arg)
          (browse-url (format (cdr ref) (semantic-tag-name tag)))
        (man (semantic-tag-name tag))))))

(provide 'c-help)
;;; c-help.el ends here
