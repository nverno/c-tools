;;; c-tools --- ... -*- lexical-binding: t; -*-

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

;; [![Build Status](https://travis-ci.org/nverno/c-tools.svg?branch=master)](https://travis-ci.org/nverno/c-tools)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (defvar yas-snippet-dirs)
  (defvar gud-comint-buffer)
  (defvar irony-server-install-prefix)
  (defvar nvp-c-include-dirs)
  (defvar nvp-c++-include-dirs)
  (defvar nvp-clang-c-include-dirs)
  (defvar nvp-clang-c++-include-dirs))
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-ext-sudo-command "nvp-ext")
(autoload 'nvp-compile-basic "nvp-compile")
(autoload 'nvp-compile-cmake "nvp-compile")

(defvar c-tools--dir nil)
(when load-file-name
  (setq c-tools--dir (file-name-directory load-file-name)))

;; ------------------------------------------------------------
;;; Install

;; make includes.el and install dependencies or dont with NODEPS
;; force includes.el refresh with ARG
(defun c-tools-install (arg &optional includes irony)
  (interactive "P")
  (let ((arg arg))
    (cond
     (includes 
      ;; write sys include paths to c-tools-include.el
      (nvp-with-process-log 
        (c-tools-install-includes arg) nil
        (load (expand-file-name "c-tools-include" c-tools--dir))
        (c-tools-install arg nil 'irony)))
     (irony
      ;; install irony server
      ;; do depends first, also only returns process object on windows
      ;; currently
      (if (not (require 'irony nil t))
          (nvp-log "Error: `irony' not installed")
        (nvp-with-gnu/w32
            (c-tools-install-irony)
          (unless (file-exists-p irony-server-install-prefix)
            (c-tools-install-irony irony-server-install-prefix)))))
     (t
      ;; install dependencies, then recall to install rest
      (nvp-with-gnu/w32
          (nvp-with-process-log
            (call-interactively 'c-tools-install-deps) :pop-on-error
            (c-tools-install arg 'includes))
        ;; FIXME: msys / cygwin install cmake/clang
        (c-tools-install arg 'includes))))))

(nvp-with-gnu
  ;; install dependencies as sudo and return process object
  (defun c-tools-install-deps (password)
    (interactive
     (list (read-passwd "Password: ")))
    (nvp-ext-sudo-command
     password (expand-file-name "tools/install.sh" c-tools--dir))))

;;; Cache system include paths
;; regen includes after 5 days or force with ARG
(defun c-tools-install-includes (&optional arg)
  (let ((includes (expand-file-name "tools/includes.el" c-tools--dir)))
    (when (or (not (file-exists-p includes))
              (or arg (nvp-file-older-than-days includes 5)))
      (start-process "bash" "*nvp-install*" "bash"
                     (expand-file-name "tools/includes.sh"
                                       c-tools--dir)
                     "make_sys_includes"))))

;;; Install irony server

(nvp-with-gnu
  (defun c-tools-install-irony (&optional check-deps)
    (if check-deps
        (nvp-with-process-log
          (call-interactively 'c-tools-install-deps) :pop-on-error
          (c-tools-install-irony nil))
      (if (not (require 'irony nil t))
          (nvp-log "Error: `irony' not installed")
        (call-interactively 'irony-install-server)))))

(nvp-with-w32
  ;; Install irony server using MSYS compilers. Return process object
  (defun c-tools-install-irony (&optional irony-prefix irony-dir build-cmd)
    (let* ((irony-dir (or irony-dir
                          (expand-file-name "server"
                                            (file-name-directory
                                             (locate-library "irony")))))
           (build-dir (make-temp-file "_build" t))
           (irony-prefix (or irony-prefix
                             (expand-file-name ".emacs.d/cache/irony" "~")))
           (args (mapconcat 'identity
                            `(,irony-dir
                              "-G \"MSYS Makefiles\""
                              "-DCMAKE_CXX_COMPILER=g++.exe"
                              "-DCMAKE_C_COMPILER=gcc.exe"
                              ,(concat "-DCMAKE_INSTALL_PREFIX=" irony-prefix))
                            " "))
           (build-cmd
            "cmake --build . --use-stderr --config Release --target install")
           (default-directory build-dir))
      (start-process-shell-command
       "cmake" "*nvp-install*" (format "cmake %s && %s" args build-cmd)))))

;; ------------------------------------------------------------
;;; Interative

(nvp-newline c-tools-newline-dwim nil
  :pairs (("{" "}"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start "* ")

;;; Generate clang complete files
;; https://github.com/Rip-Rip/clang_complete/wiki
;; discusses making pre-compiled headers for clang_complete
(defvar-local c-tools-local-include-paths nil)
(defun c-tools-clang-complete (arg &optional paths)
  (interactive "P")
  (unless (bound-and-true-p c-tools-clang-c-include-dirs)
    (when (not (require 'c-tools-include "c-tools-include" t))
      (nvp-log "c-tools-include not found, running c-tools install.")
      (c-tools-install nil)))
  (cl-flet ((includes (lst) (mapcar #'(lambda (x) (concat "-I" x)) lst)))
    (let ((default
            (mapconcat
             'identity
             `("-DDEBUG"
               ,@(includes (or paths
                               (bound-and-true-p c-tools-local-include-paths)))
               ,@(includes (if (eq major-mode 'c-mode)
                               (bound-and-true-p
                                c-tools-clang-c-include-dirs)
                             (bound-and-true-p
                              c-tools-clang-c++-include-dirs))))
             "\n"))
          (file (expand-file-name ".clang_complete" default-directory)))
      (when arg
        (setq file (expand-file-name
                    ".clang_complete"
                    (read-directory-name
                     "Directory to make .clang_complete: " file))))
      (with-temp-file file
        (insert default)))))

;;; Compile

;; run make / cmake if there are corresponding makefiles,
;; otherwise prompt / use default
(nvp-make-or-compile-fn c-tools-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -O2 -g -std=c11"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "gcc")
                  flags out (nvp-with-gnu/w32 ".out" ".exe") file)))
    (nvp-compile-basic)))

;; ------------------------------------------------------------
;;; Toggle / insert

;; add header guard
(defun c-tools-add-guard ()
  (interactive)
  (let ((guard (concat
                (upcase (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))
                "_H")))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at-p (format "#ifndef %s" guard))
        (insert (format "#ifndef %s\n#define %s\n\n" guard guard))
        (goto-char (point-max))
        (insert (format "\n#endif /* %s */" guard))))))

;; ------------------------------------------------------------
;;; Setup

(eval-after-load 'yasnippet
  '(let ((dir (expand-file-name "snippets" c-tools--dir))
         (dirs (or (and (consp yas-snippet-dirs) yas-snippet-dirs)
                   (cons yas-snippet-dirs ()))))
     (unless (member dir dirs)
       (setq yas-snippet-dirs (delq nil (cons dir dirs))))
     (yas-load-directory dir)))

;; ------------------------------------------------------------
(declare-function yas-load-directory "yasnippet")

(provide 'c-tools)
;;; c-tools.el ends here
