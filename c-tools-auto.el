;;; c-tools-auto --- 

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
(require 'nvp-indicate)
(require 'gud)

;;;###autoload
(defun c-tools-gud-switch ()
  (interactive)
  (if gud-comint-buffer
      (pop-to-buffer gud-comint-buffer)
    (user-error "No gud buffer")))

;;; Help menu

(when (require 'hydra nil t)
 
  (nvp-bindings "c-mode" 'cc-mode
    ("<f2> d g" . c-tools-gud-hydra/body))
  (nvp-bindings "c++-mode" 'cc-mode
    ("<f2> d g" . c-tools-gud-hydra/body))
  
  ;; compiler doesnt understande these functions
  (with-no-warnings
    (defhydra c-tools-gud-hydra (:color amaranth
                                        :pre nvp-indicate-hydra-pre
                                        :post nvp-indicate-hydra-post)
      ;; vi
      ("h" backward-char)
      ("j" next-line)
      ("k" previous-line)
      ("l" forward-char)
      ;; gud
      ("t" gud-tbreak "tbreak")
      ("b" gud-break "break")
      ("d" gud-remove "nbr")
      ("p" gud-print "print" :color blue)
      ("m" gud-until "move")
      ("n" gud-next "next")
      ("c" gud-cont "cont")
      ("o" gud-finish "out")
      ("r" gud-run "run")
      ("q" nil "quit"))
    (hydra-set-property 'c-tools-gud-hydra :verbosity 1)))

(provide 'c-tools-auto)
;;; c-tools-auto.el ends here
