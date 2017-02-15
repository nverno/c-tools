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
  (defvar c/R-abbrev-table)
  (defvar gud-comint-buffer)
  (defvar irony-server-install-prefix)
  (defvar nvp-abbrev-local-table))
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-ext-sudo-command "nvp-ext")
(autoload 'nvp-compile-basic "nvp-compile")
(autoload 'nvp-compile-cmake "nvp-compile")
(autoload 'string-trim-right "subr-x")

;;; TODO:
;; - Move env setup ffi - R, lisp - to separate file.
;;   Should do env., snippets, abbrevs, etc all together.

(nvp-package-dir c-tools--dir)
(nvp-package-load-snippets c-tools--dir)

(defvar-local c-tools-local-include-paths nil)
(setq-default c-tools-local-include-paths '("." ".." "../include"))

;; -------------------------------------------------------------------
;;; Util

;; split string STR on commas, but only when not between <..>
;; eg., "std::vector<std::pair<int,int>> i, int j" =>
;;      ("std::vector<std::pair<int,int>> i" "int j")
(defun c-tools-split-string (str &optional delim)
  (when (not (zerop (length str)))
    (let ((delim (or delim ?\,))
          (bcount 0)                     ; current opening brace count
          (prev 0)                       ; substring starting location
          (trim-p t)                     ; non-nil if skipping beginning blanks
          res)                           ; list of resulting strings
      (cl-loop for c across str
         for i from 0 upto (length str)
         do (pcase c
              (`?  (and trim-p (cl-incf prev)))
              (`?< (cl-incf bcount))
              (`?> (cl-decf bcount))
              ((pred (equal delim))
               (when (zerop bcount)
                 (push (substring str prev i) res)
                 (setf prev (1+ i))
                 (setf trim-p t)))
              (_ (setf trim-p nil))))
      (push (string-trim-right (substring str prev)) res)
      (nreverse res))))

(defsubst c-tools-out-file (&optional file)
  (concat (file-name-sans-extension (or file (buffer-file-name)))
          (nvp-with-gnu/w32 ".out" ".exe")))

;; pull out functions signatures from current buffer using ctags
(defun c-tools-function-signatures (&optional file)
  (let ((sigs (process-lines (nvp-program "ctags") "-x" "--c-kinds=fp"
                             (or file buffer-file-name))))
    (mapcar
     (lambda (s)
       (replace-regexp-in-string
        "[ \t;{]*$" ""
        (cadr (split-string s (or file buffer-file-name) t " "))))
     sigs)))

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
  (defun c-tools-install-deps ()
    (interactive)
    (nvp-ext-sudo-command
     nil
     (expand-file-name "tools/install.sh" c-tools--dir))))

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

;; -------------------------------------------------------------------
;;; Environment

(autoload 'asdf-where "asdf")

;; set environment stuff for macro expanding
;; could also set local `c-macro-preprocessor'?
(defun c-tools-setenv (type)
  (interactive
   (list (ido-completing-read "Set include path for: " '("unity" "R"))))
  (let ((env (getenv "C_INCLUDE_PATH")))
    (pcase type
      (`"unity"
       ;; add path to unity source to macroexpand all the shittles
       (unless (string-match-p "unity" env)
         (setenv "C_INCLUDE_PATH"
                 (concat env ":" (expand-file-name
                                  ".local/include/unity/src" (getenv "HOME"))))))
      (`"R"
       (unless (string-match-p "/R/include" env)
         (setenv "C_INCLUDE_PATH"
                 (concat
                  env ":"
                  (expand-file-name "lib/R/include" (asdf-where "R" 'current)))))
       (setq-local local-abbrev-table c/R-abbrev-table)
       (setq-local nvp-abbrev-local-table "c/R"))
      (_ ()))))

;; ------------------------------------------------------------
;;; Commands

(nvp-newline c-tools-newline-dwim nil
  :pairs (("{" "}"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start "* ")

(defun c-tools-newline-x ()
  (interactive)
  (end-of-line)
  (delete-horizontal-space)
  (unless (eq (char-before) ?\;)
    (insert ";"))
  (newline-and-indent))

;;;-- Marking --

(defun c-tools-mark-defun ()
  (interactive)
  (nvp-mark-defun
   ;; mark function on first invoke
   (c-mark-function)
   ;; successively extend to next functions
   (c-beginning-of-defun -1)
   (point)))

;;; Compile

;; run make / cmake if there are corresponding makefiles,
;; otherwise prompt / use default
(nvp-make-or-compile-fn c-tools-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -Werror -O2 -g -std=c11"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "gcc")
                  flags out (nvp-with-gnu/w32 ".out" ".exe") file)))
    (setq-local compile-command compile-command)
    (call-interactively 'nvp-compile-basic)))

;; compile current file and run it with output to compilation buffer
(defun c-tools-compile-and-run (keep &optional compiler flags post-action)
  (interactive "P")
  (let* ((out (concat (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))
                      (nvp-with-gnu/w32 ".out" ".exe")))
         (command
          (concat (or compiler (nvp-program "gcc")) " "
                  (or flags "-s -O3") " "
                  buffer-file-name " -o " out "; "
                  (or (and (eq post-action 'no-run) "")
                      post-action (concat "./" out))
                  (unless keep (concat "; rm " out)))))
    (setq-local compile-command command)
    (call-interactively 'nvp-compile-basic)))

;; watch error output with TEST
(defun c-tools-compile-watch (arg)
  (interactive "P")
  (let ((file (if arg (read-from-minibuffer "Output file: " "out.txt")
                "out.txt"))
        (out (file-name-nondirectory (c-tools-out-file))))
    (c-tools-compile-and-run
     nil nil "-O3 -DTEST -std=c11"
     (concat "./" out " 2> " file
             "& gnome-terminal -x watch tail -n10 " file))))

(defun c-tools-compile-debug ()
  (interactive)
  (c-tools-compile-and-run 'keep nil "-Wall -Werror -ggdb3 -DDEBUG" 'no-run)
  (call-interactively 'gdb))

;; -------------------------------------------------------------------
;;; Headers

(eval-when-compile
  (defvar yas-selected-text)
  (defvar yas-wrap-around-region))
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-lookup-snippet "yasnippet")

;;; Create/update header file with function signatures
(defun c-tools-create-or-update-header (and-go)
  (interactive (list t))
  (let ((header (concat (file-name-sans-extension buffer-file-name) ".h"))
        (sigs (c-tools-function-signatures))
        (yas-wrap-around-region nil)
        (init t))
    (when (file-exists-p header)
      (setq init nil)
      (setq sigs
            ;; remove any signatures that are already found in the header file
            (cl-set-difference
             sigs
             (c-tools-function-signatures header)
             :test 'string=)))
    (when sigs
      (with-current-buffer (find-file header)
        (setq sigs (concat "\n" (mapconcat 'identity sigs ";\n") ";"))
        (if init
            (let ((yas-selected-text sigs))
              (yas-expand-snippet
               (yas-lookup-snippet "header" 'cc-mode)))
          ;; search forward past #define HEADER_H
          (goto-char (point-min))
          (re-search-forward
           (concat
            "#define[ \t]+" (regexp-quote
                             (concat (upcase (file-name-nondirectory
                                              (file-name-sans-extension header)))
                                     "_H")))
           nil 'move)
          (insert sigs))))
    (if and-go
        (find-file header))))

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

;; -------------------------------------------------------------------
;;; Doxygen

(defun c-tools-toggle-doxygen ()
  (interactive)
  (save-excursion
    (when (re-search-forward "\\(?://\\|/\\*+\\)" nil 'move)
      (if (and (string= (match-string 0) "/**") (eq (char-after) ?<))
          (progn (delete-char -1)
                 (delete-char 1))
        (delete-char -1)
        (insert "**<")
        (end-of-line)
        (unless (looking-back "\\*/\\s-*" (line-beginning-position))
          (delete-horizontal-space)
          (insert " */"))))))

;; align comment start / end for doxygen region
(eval-when-compile
  (defvar align-to-tab-stop))
(defun c-tools-align-doxygen (beg end)
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beg end "\\(\\s-*\\)/\\*\\*")
    (align-regexp beg end "\\(\\s-*\\)\\*/")))

;; -------------------------------------------------------------------
;;; Yas

(eval-when-compile
  (defvar yas-text))

;; get variable name from declaration, either with type or not
;; eg., i = 1 or int i = 1 => `i'
(defsubst c-yas-var (text)
  (if (< (length text) 1)
      ""
   (let* ((str (car (split-string text "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; convert functions args to doxygen params
(defsubst c-yas-args-docstring (text)
  (let ((args (c-tools-split-string text)))
    (and args
         (mapconcat 'identity
                    (mapcar (lambda (s) (concat "\n * @param " s)) args) ""))))

;; -------------------------------------------------------------------
(declare-function c-beginning-of-defun "cc-cmds")
(declare-function c-mark-function "cc-cmds")

(provide 'c-tools)
;;; c-tools.el ends here
