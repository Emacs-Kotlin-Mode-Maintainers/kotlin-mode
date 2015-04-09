;;; kotlin-mode.el --- Major mode for kotlin
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Keywords: languages

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

;;

;;; Code:

(require 'rx)

(defcustom kotlin-mode-hook nil
  "Hook run after entering `kotlin-mode'."
  :type 'hook
  :group 'kotlin
  )


(defvar kotlin-mode-map (make-sparse-keymap)
  "Keymap used by `kotlin-mode'."
  )


(defvar kotlin-mode-syntax-table
  (let ((st (make-syntax-table)))

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)

    ;; `_' as being a valid part of a word
    (modify-syntax-entry ?_ "w" st)

    ;; b-style comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st))


;;; Font Lock

(defconst kotlin-mode--misc-keywords
  '("package" "import"))

(defconst kotlin-mode--type-decl-keywords
  '("class" "trait" "typealias"))

(defconst kotlin-mode--fun-decl-keywords
  '("fun"))

(defconst kotlin-mode--val-decl-keywords
  '("val" "var"))

(defconst kotlin-mode--statement-keywords
  '("if" "else" "try" "while" "do"
    "throw" "return" "continue" "break" "when" "is" "in"))

(defconst kotlin-mode--context-variables-keywords
  '("this" "super"))

(defvar kotlin-mode--keywords
  (append kotlin-mode--misc-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords)
  "Keywords used in Kotlin language."
  )

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--modifier-keywords
  '("open" "private" "protected" "public"
    "override" "abstract" "final"))

(defconst kotlin-mode--property-keywords
  '("by" "get" "set"))

(defconst kotlin-mode--initializer-keywords
  '("init" "constructor")
  )

(defvar kotlin-mode-font-lock-keywords
  `(
    ;; Keywords
    (,(rx-to-string
     `(and bow (group (or ,@kotlin-mode--keywords)) eow)
     t)
     1 font-lock-keyword-face)

    ;; Types
    (,(rx-to-string
      `(and (* space) ":" (* space) (group (+ word)))
      t)
     0 font-lock-type-face)

    ;; Classes/Enums
    (,(rx-to-string
      `(and bow "class" eow (+ space)
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
             (group (+ word)) (* space)  (\? ":"))
       t)
     1 font-lock-variable-name-face t)

    ;; Function names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--fun-decl-keywords)
             (+ space) bow (group (+ (any alnum word))) eow)
       t)
     1 font-lock-function-name-face)

    ;; Access modifier
    ;; Access modifier is valid identifier being used as variable
    ;; TODO: Highlight only modifiers in the front of class/fun
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--modifier-keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; try-catch-finally
    ;; `catch' and `finally' are valid identifier being used as variable
    (,(rx-to-string
       `(and bow (group "catch") eow
             (* space) (*? anything) "{" )
       t)
     1 font-lock-keyword-face)
    (,(rx-to-string
       `(and bow (group "finally") eow
             (*? (or space ?\n)) "{")
       t)
     1 font-lock-keyword-face)

    ;; Properties
    ;; by/get/set are valid identifiers being used as variable
    ;; TODO: Highlight keywords in the property declaration statement
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--property-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Constructor/Initializer blocks
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--initializer-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; Package names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--misc-keywords) (+ space)
             (group (+ (any word ?.))))
       t)
     1 font-lock-string-face)

    ;; String interpolation
    (kotlin-mode--match-interpolation 0 font-lock-variable-name-face t)
    )
  "Default highlighting expression for `kotlin-mode'"
  )

(defun kotlin-mode--syntax-propertize-interpolation ()
  (let* ((pos (match-beginning 0))
         (context (save-excursion
                    (save-match-data (syntax-ppss pos)))))
    (when (nth 3 context)
      (put-text-property pos
                         (1+ pos)
                         'kotlin-property--interpolation
                         (match-data))))
  )

(defun kotlin-mode--syntax-propertize-function (start end)
  (let ((case-fold-search))
    (goto-char start)
    (remove-text-properties start end '(kotlin-property--interpolation))
    (funcall
     (syntax-propertize-rules
      ((let ((identifier '(any alnum " !%&()*+-./:<>?[]^_|~")))
         (rx-to-string
          `(or (group "${" (* ,identifier) "}")
               (group "$" (+ ,identifier)))))
       (0 (ignore (kotlin-mode--syntax-propertize-interpolation)))))
     start end))
  )

(defun kotlin-mode--match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point)
                                               'kotlin-property--interpolation
                                               nil
                                               limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'kotlin-property--interpolation)))
        (if value
            (progn (set-match-data value)
                   t)
          (kotlin-mode--match-interpolation limit)))))
  )


(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq-local font-lock-defaults '((kotlin-mode-font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize-function)

  :group 'kotlin
  :syntax-table kotlin-mode-syntax-table
  )

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
