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

(defvar c-help-sources
  '(("/usr/local/")))

;; semantic tag at point
(defsubst c-help-tag-at (point)
  (car (reverse (oref (semantic-analyze-current-context point) prefix))))

;; get info on thing at point
(defun c-help-at-point (point)
  (interactive "d")
  (let ((tag (c-help-tag-at point))
        file type)
    (when tag
      (setq type (semantic-tag-class tag))
      (setq file (semantic-tag-file-name tag))
      )))

(defun c-help-type-at (point)
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix)))
	 (lastname (pop pf))
	 (tag (if (semantic-tag-p lastname) lastname (caar pf)))
	 (names (append
		 (when (semantic-tag-p tag)
		   (save-excursion
		     (when (semantic-tag-with-position-p tag)
		       (set-buffer (semantic-tag-buffer tag))
		       (semantic-go-to-tag tag)
		       (mapcar 'semantic-tag-name
                               (semantic-analyze-scope-nested-tags (point) nil)))))
		 (list (if (semantic-tag-p lastname) (semantic-tag-name lastname)
                         lastname)))))
    (concat (mapconcat 'concat names "::"))))

(provide 'c-help)
;;; c-help.el ends here
