;;; kotlin-mode.el --- Major mode for kotlin -*- lexical-binding: t; -*-

;; Copyright © 2015 Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Version: 2.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode for Kotlin programming language.

;;; Code:

(require 'comint)
(require 'rx)
(require 'cc-cmds)
(require 'cl-lib)
(require 'eieio)

(require 'kotlin-mode-lexer)
(require 'kotlin-mode-indent)

(defgroup kotlin nil
  "A Kotlin major mode."
  :group 'languages)

(defcustom kotlin-command "kotlinc"
  "The Kotlin command used for evaluating code."
  :type 'string
  :group 'kotlin)

(defcustom kotlin-args-repl '()
  "The arguments to pass to `kotlin-command' to start a REPL."
  :type 'list
  :group 'kotlin)

(defcustom kotlin-repl-buffer "*KotlinREPL*"
  "The name of the KotlinREPL buffer."
  :type 'string
  :group 'kotlin)

;; REPL

(defun kotlin-do-and-repl-focus (f &rest args)
  "Call function F with ARGS and pop to the REPL buffer."
  (apply f args)
  (pop-to-buffer kotlin-repl-buffer))

(defun kotlin-send-region (start end)
  "Send current region to Kotlin interpreter.

START and END define region within current buffer."
  (interactive "r")
  (comint-send-region kotlin-repl-buffer start end)
  (comint-send-string kotlin-repl-buffer "\n"))

(defun kotlin-send-region-and-focus (start end)
  "Send current region to Kotlin interpreter and switch to it.

START and END define region within current buffer."
  (interactive "r")
  (kotlin-do-and-repl-focus 'kotlin-send-region start end))

(defun kotlin-send-buffer ()
  "Send whole buffer to Kotlin interpreter."
  (interactive)
  (kotlin-send-region (point-min) (point-max)))

(defun kotlin-send-buffer-and-focus ()
  "Send whole buffer to Kotlin interpreter and switch to it."
  (interactive)
  (kotlin-do-and-repl-focus 'kotlin-send-buffer))

(defmacro kotlin-mode--save-mark-and-excursion (&rest body)
  "Polyfill of `save-mark-and-excursion' for <25.1.

For argument BODY, see `save-mark-and-excursion'."
  (declare (indent 0) (debug t))
  (let ((save-mark-and-excursion (if (fboundp 'save-mark-and-excursion)
                                     #'save-mark-and-excursion
                                   #'save-excursion)))
    (cons save-mark-and-excursion body)))

(defun kotlin-send-block ()
  "Send block to Kotlin interpreter."
  (interactive)
  (kotlin-mode--save-mark-and-excursion
    (mark-paragraph)
    (kotlin-send-region (region-beginning) (region-end))))

(defun kotlin-send-block-and-focus ()
  "Send block to Kotlin interpreter and switch to it."
  (interactive)
  (kotlin-do-and-repl-focus 'kotlin-send-block))

(defun kotlin-send-line ()
  "Send current line to Kotlin interpreter."
  (interactive)
  (kotlin-send-region
   (line-beginning-position)
   (line-end-position)))

(defun kotlin-send-line-and-focus ()
  "Send current line to Kotlin interpreter and switch to it."
  (interactive)
  (kotlin-do-and-repl-focus 'kotlin-send-line))

(defun kotlin-repl ()
  "Launch a Kotlin REPL using `kotlin-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc kotlin-repl-buffer)
    (set-buffer
     (apply 'make-comint "KotlinREPL"
            "env"
            nil
            "NODE_NO_READLINE=1"
            kotlin-command
            kotlin-args-repl))

    (set (make-local-variable 'comint-preoutput-filter-functions)
         (list (lambda (string)
                 (replace-regexp-in-string "\x1b\\[.[GJK]" "" string)))))

  (pop-to-buffer kotlin-repl-buffer))

;; Keymap

(defvar kotlin-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-z") 'kotlin-repl)
    (define-key map (kbd "C-c C-n") 'kotlin-send-line)
    (define-key map (kbd "C-c C-r") 'kotlin-send-region)
    (define-key map (kbd "C-c C-c") 'kotlin-send-block)
    (define-key map (kbd "C-c C-b") 'kotlin-send-buffer)
    (define-key map [remap indent-new-comment-line]
      #'kotlin-mode--indent-new-comment-line)
    map)
  "Keymap for `kotlin-mode'.")

;;; Font Lock

(defconst kotlin-mode--package-keywords
  '("package" "import"))

(defconst kotlin-mode--type-decl-keywords
  '("class" "interface" "typealias" "object"))

(defconst kotlin-mode--fun-decl-keywords
  '("fun"))

(defconst kotlin-mode--val-decl-keywords
  '("val" "var"))

(defconst kotlin-mode--statement-keywords
  '(;; Branching
    "if" "else"
    ;; Exceptions
    "try" "catch" "finally" "throw"
    ;; Loops
    "while" "for" "do" "continue" "break"
    ;; Miscellaneous
    "when" "is" "in" "as" "return"))

(defconst kotlin-mode--context-variables-keywords
  '("field" "it" "this" "super"))

(defconst kotlin-mode--generic-type-parameter-keywords
  '("where"))

(defvar kotlin-mode--misc-keywords
  (append kotlin-mode--package-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords
          kotlin-mode--generic-type-parameter-keywords)
  "Keywords used in Kotlin language.")

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--variance-modifier-keywords
  '("in" "out"))

(defconst kotlin-mode--reification-modifier-keywords
  '("reified"))

(defconst kotlin-mode--property-keywords
  '("by" "get" "set"))

(defconst kotlin-mode--initializer-keywords
  '("init" "constructor"))

(defconst kotlin-mode--annotation-use-site-target-keywords
  '("delegate" "field" "file" "get" "param" "property" "receiver" "set"
    "setparam"))

(defconst kotlin-mode--type-keywords
  '("dynamic"))

(defvar kotlin-mode--font-lock-keywords
  `(;; Keywords
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--misc-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Package names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--package-keywords) (+ space)
             (group (+ (any word ?.))))
       t)
     1 font-lock-string-face)

    ;; Types
    (,(rx-to-string
       `(and bow upper (group (* (or word "<" ">" "." "?" "!" "*"))))
       t)
     0 font-lock-type-face)

    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--type-keywords) eow)
       t)
     0 font-lock-type-face)

    ;; Classes/Enums
    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--type-decl-keywords) eow (+ space)
             (group (+ word)) eow)
       t)
     1 font-lock-type-face)

    ;; Constants
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--constants-keywords)) eow)
       t)
     0 font-lock-constant-face)

    ;; Value bindings
    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--val-decl-keywords) eow
             (+ space)
             (group (+ (or word (syntax symbol)))) (* space)  (\? ":"))
       t)
     1 font-lock-variable-name-face t)

    ;; Function names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--fun-decl-keywords)
             (+ space) bow (group (+ (any alnum word))) eow)
       t)
     1 font-lock-function-name-face)

    ;; Modifiers
    ;; Modifier is valid identifier being used as variable
    ;; TODO: Highlight only modifiers in the front of class/fun
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--modifier-keywords
                            ,@kotlin-mode--companion-modifier-keywords
                            ,@kotlin-mode--variance-modifier-keywords
                            ,@kotlin-mode--reification-modifier-keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Properties
    ;; by/get/set are valid identifiers being used as variable
    ;; TODO: Highlight only within the property declaration statement
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--property-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Constructor/Initializer blocks
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--initializer-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Annotation use-site targets
    (,(rx-to-string
       `(and "@"
             (group (or ,@kotlin-mode--annotation-use-site-target-keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Labels
    (,(rx-to-string
       `(and bow (group (+ word)) "@")
       t)
     1 font-lock-constant-face)

    ;; String interpolation
    (kotlin-mode--match-interpolation 0 font-lock-variable-name-face t))
  "Default highlighting expression for `kotlin-mode'.")

(defun kotlin-mode--match-interpolation (limit)
  "Find template expression before LIMIT.

Template expressions must be propertized by `kotlin-mode--syntax-propertize'.
If a template expression is found, move to that point, set `match-data',
and return non-nil.  Return nil otherwise."
  (let ((pos (next-single-char-property-change
              (point)
              'kotlin-property--interpolation
              nil
              limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'kotlin-property--interpolation)))
        (if value
            (progn (set-match-data value)
                   t)
          (kotlin-mode--match-interpolation limit))))))


;; the Kotlin mode

;;;###autoload
(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq font-lock-defaults '((kotlin-mode--font-lock-keywords) nil nil))

  (setq-local parse-sexp-lookup-properties t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'kotlin-mode--syntax-propertize-extend-region
            nil t)
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize)

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-padding 1)
  (setq-local comment-start-skip
              (rx (seq (zero-or-more (syntax whitespace))
                       (or
                        ;; Single-line comment
                        (seq "/" (one-or-more "/"))
                        ;; Multi-line comment
                        (seq "/" (one-or-more "*"))
                        ;; Middle of multi-line-comment
                        (seq (one-or-more "*") " "))
                       (zero-or-more (syntax whitespace)))))
  (setq-local adaptive-fill-regexp
              (rx (seq (zero-or-more (syntax whitespace))
                       (or
                        ;; Single-line comment
                        (seq "/" (one-or-more "/"))
                        ;; Middle of multi-line-comment
                        (seq (one-or-more "*") " "))
                       (zero-or-more (syntax whitespace)))))
  (setq-local fill-indent-according-to-mode t)
  (setq-local comment-multi-line t)

  (setq-local indent-line-function 'kotlin-mode--indent-line)

  (setq-local electric-indent-chars
              (append "{}()[]:;,." electric-indent-chars))

  (add-hook 'post-self-insert-hook #'kotlin-mode--post-self-insert nil t)

  (setq-local kotlin-mode--anchor-overlay
              (make-overlay (point-min) (point-min) nil t))

  (delete-overlay kotlin-mode--anchor-overlay)

  :group 'kotlin
  :syntax-table kotlin-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode) t)

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
