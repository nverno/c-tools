;;; c-debug ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-debug
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
  (require 'nvp-macro)
  (defvar c-mode-map)
  (defvar c++-mode-map))
(require 'nvp-indicate)
(require 'gud)
(eval-and-compile
  (require 'hydra))

;;; FIXME: gud-mode seems to clobber kill-buffer-hooks,
;;         so shell history isn't being saved/read properly
(declare-function hippie-expand-shell-setup "hippie-expand-shell")
(declare-function nvp-comint-setup-history "nvp-comint")
;;;###autoload
(defun c-debug-shell-setup ()
  (require 'nvp-comint)
  (hippie-expand-shell-setup 'comint-input-ring
                             'comint-line-beginning-position)
  (nvp-comint-setup-history ".gdb_history"))

;;;###autoload
(defun c-debug-gud-switch ()
  (interactive)
  (if gud-comint-buffer
      (pop-to-buffer gud-comint-buffer)
    (user-error "No gud buffer")))

;;; Help menu

(nvp-bindings "c-mode" 'cc-mode
  ("<f2> d g" . c-debug-gud-hydra/body))
(nvp-bindings "c++-mode" 'cc-mode
  ("<f2> d g" . c-debug-gud-hydra/body))

;; compiler doesnt understande these functions
(with-no-warnings
  (defhydra c-debug-gud-hydra (:color amaranth
                               :pre nvp-indicate-hydra-pre
                               :post nvp-indicate-hydra-post)
    ;; vi
    ("h" backward-char nil)
    ("j" next-line nil)
    ("k" previous-line nil)
    ("l" forward-char nil)
    ;; gud
    ("m" gud-many-windows "many-windows mode")
    ("t" gud-tbreak "tbreak")
    ("b" gud-break "break")
    ("d" gud-remove "remove")
    ;; ("D" )
    ("J" gud-jump "jump")
    ("p" gud-print "print")
    ("m" gud-until "move")
    ("n" gud-next "next")
    ("c" gud-cont "cont")
    ("o" gud-finish "out")
    ("r" gud-run "run")
    ("q" nil "quit")))
(hydra-set-property 'c-debug-gud-hydra :verbosity 1)

(provide 'c-debug)
;;; c-debug.el ends here
