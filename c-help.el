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
(defvar c-help-online-sources
  (let ((uri
         "http://pubs.opengroup.org/onlinepubs/9699919799/functions/%s.html"
         ;; "http://en.cppreference.com/mwiki/index.php?title=Special:Search&search=%s"
         ))
    (cl-loop for p in semantic-c-dependency-system-include-path
       collect (cons p uri))))

;; if filename prefix is member of car, apply cadr to (format cddr)
;; use man 2 for system call type stuff, otherwise man 3
(defvar c-help-local-sources
  `((("/usr/include/unistd" "/usr/include/fcntl"
      "sys/time" "sys/wait" "sys/resource"
      "/usr/include/signal") . (man "2 %s"))
    (,semantic-c-dependency-system-include-path . (man "3 %s"))))

(eval-when-compile
  (defmacro c-help-find-source (type file)
    "Find help location for TYPE as determined by FILE."
    (pcase type
      (`'online
       `(cl-some (lambda (src)
                   (and (string-prefix-p (car src) ,file)
                        src))
                 c-help-online-sources))
      (_
       `(cdr-safe
         (cl-find-if
          (lambda (entry)
            (cl-some (lambda (e) (string-match-p e ,file)) (car entry)))
          c-help-local-sources))))))

;; semantic tag at point
(defsubst c-help-tag-at (point)
  (car (reverse (oref (semantic-analyze-current-context point) prefix))))

;; TODO: if 'man 2' doesn't work, try 'man 3', eg. execvp in unistd
;;      - how to hook into Man to know if there was a problem?
;;        it creates the buffer no matter what, and runs async
;; (defun c-help-Man-cooked-hook ()
;;   (and (eq 0 (buffer-size))
;;        (throw 'no-dice nil)))
;; (setq Man-cooked-hook 'c-help-Man-cooked-hook)
(defun c-help-get-man-help (cmd)
  (let ((buf (man cmd)))
    (sit-for 0.1)                      ;FIXME
    (unless (buffer-live-p buf)
      (let ((num (substring cmd 0 1)))
        (man (concat
              (pcase num
                (`"1" "2")
                (`"2" "3")
                (`"3" "2"))
              (substring cmd 1)))))))

;; Lookup info in man or online for thing at point
;;;###autoload
(defun c-help-at-point (point &optional online)
  (interactive "d")
  (let* ((tag (c-help-tag-at point))
         (file (and (semantic-tag-p tag)
                    (semantic-tag-file-name tag))))
    (if (or online current-prefix-arg)
        (let ((ref (and (stringp file)
                        (c-help-find-source 'online file))))
          (if (not ref)
              (message "No documentation source found for %S" tag)
            (browse-url (format (cdr ref) (semantic-tag-name tag)))))
      (let ((action (and (stringp file)
                         (c-help-find-source 'local file)))
            (tag-name (or (and (semantic-tag-p tag)
                               (semantic-tag-name tag))
                          tag)))
        (when action
          (pcase (car action)
            ('man (c-help-get-man-help (format (cadr action) tag-name)))
            (_ (apply (car action)
                      (format (cadr action) tag-name)
                      (cddr action)))))))))

;; TODO: index and search
(defun c-help-std ()
  (interactive)
  (browse-url "http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf"))

(provide 'c-help)
;;; c-help.el ends here
