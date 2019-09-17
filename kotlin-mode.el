;;; kotlin-mode.el --- Major mode for kotlin -*- lexical-binding: t; -*-

;; Copyright © 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

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

(require 'comint)
(require 'rx)
(require 'cc-cmds)
(require 'cl)
(require 'eieio)

(defgroup kotlin nil
  "A Kotlin major mode."
  :group 'languages)

(defcustom kotlin-tab-width tab-width
  "The tab width to use for indentation."
  :type 'integer
  :group 'kotlin-mode
  :safe 'integerp)

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

(defun kotlin-do-and-repl-focus (f &rest args)
  (apply f args)
  (pop-to-buffer kotlin-repl-buffer))

(defun kotlin-send-region (start end)
  "Send current region to Kotlin interpreter."
  (interactive "r")
  (comint-send-region kotlin-repl-buffer start end)
  (comint-send-string kotlin-repl-buffer "\n"))

(defun kotlin-send-region-and-focus (start end)
  "Send current region to Kotlin interpreter and switch to it."
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

(defun kotlin-send-block ()
  (interactive)
  (let* ((p (point)))
    (mark-paragraph)
    (kotlin-send-region (region-beginning) (region-end))
    (goto-char p)))

(defun kotlin-send-block-and-focus ()
  "Send block to Kotlin interpreter and switch to it."
  (interactive)
  (kotlin-do-and-repl-focus 'kotlin-send-block))

(defun kotlin-send-line ()
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
         (cons (lambda (string)
                 (replace-regexp-in-string "\x1b\\[.[GJK]" "" string)) nil)))

  (pop-to-buffer kotlin-repl-buffer))

(defvar kotlin-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-z") 'kotlin-repl)
    (define-key map (kbd "C-c C-n") 'kotlin-send-line)
    (define-key map (kbd "C-c C-r") 'kotlin-send-region)
    (define-key map (kbd "C-c C-c") 'kotlin-send-block)
    (define-key map (kbd "C-c C-b") 'kotlin-send-buffer)
    map)
  "Keymap for kotlin-mode")

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
  '("nested" "inner" "data" "class" "interface" "trait" "typealias" "enum" "object"))

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

(defvar kotlin-mode--keywords
  (append kotlin-mode--misc-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords)
  "Keywords used in Kotlin language.")

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--modifier-keywords
  '("open" "private" "protected" "public" "lateinit"
    "override" "abstract" "final" "companion"
    "annotation" "internal" "const" "in" "out")) ;; "in" "out"

(defconst kotlin-mode--property-keywords
  '("by" "get" "set")) ;; "by" "get" "set"

(defconst kotlin-mode--initializer-keywords
  '("init" "constructor"))

(defvar kotlin-mode--font-lock-keywords
  `(;; Keywords
    (,(rx-to-string
     `(and bow (group (or ,@kotlin-mode--keywords)) eow)
     t)
     1 font-lock-keyword-face)

    ;; Package names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--misc-keywords) (+ space)
             (group (+ (any word ?.))))
       t)
     1 font-lock-string-face)

    ;; Types
    (,(rx-to-string
      `(and bow upper (group (* (or word "<" ">" "." "?" "!" "*"))))
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

    ;; String interpolation
    (kotlin-mode--match-interpolation 0 font-lock-variable-name-face t))
  "Default highlighting expression for `kotlin-mode'")

(defun kotlin-mode--new-font-lock-keywords ()
  '(
    ("package\\|import" . font-lock-keyword-face)
    ))

(defun kotlin-mode--syntax-propertize-interpolation ()
  (let* ((pos (match-beginning 0))
         (context (save-excursion
                    (save-match-data (syntax-ppss pos)))))
    (when (nth 3 context)
      (put-text-property pos
                         (1+ pos)
                         'kotlin-property--interpolation
                         (match-data)))))

(defun kotlin-mode--syntax-propertize-function (start end)
  (let ((case-fold-search))
    (goto-char start)
    (remove-text-properties start end '(kotlin-property--interpolation))
    (funcall
     (syntax-propertize-rules
      ((let ((identifier '(or
                           (and alpha (* alnum))
                           (and "`" (+ (not (any "`\n"))) "`"))))
         (rx-to-string
          `(or (group "${" ,identifier "}")
               (group "$" ,identifier))))
       (0 (ignore (kotlin-mode--syntax-propertize-interpolation)))))
     start end)))

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
          (kotlin-mode--match-interpolation limit))))))

(defun kotlin-mode--prev-line ()
  "Moves up to the nearest non-empty line"
  (if (not (bobp))
      (progn
        (forward-line -1)
        (while (and (looking-at "^[ \t]*$") (not (bobp)))
          (forward-line -1)))))

(defun kotlin-mode--line-begins (pattern)
  "Return whether the current line begins with the given pattern"
  (save-excursion
    (beginning-of-line)
    (looking-at (format "^[ \t]*%s" pattern))))

(defun kotlin-mode--line-ends (pattern)
  "Return whether the current line ends with the given pattern"
  (save-excursion
    (beginning-of-line)
    (looking-at (format ".*%s[ \t]*$" pattern))))

(defun kotlin-mode--line-contains (pattern)
  "Return whether the current line contains the given pattern"
  (save-excursion
    (beginning-of-line)
    (looking-at (format ".*%s.*" pattern))))

(defun kotlin-mode--line-continuation()
  "Return whether this line continues a statement in the previous line"
  (or
   (kotlin-mode--line-begins "\\([\.=:]\\|->\\|[sg]et\\b\\)")
   (save-excursion
     (kotlin-mode--prev-line)
     (kotlin-mode--line-ends "\\([=:]\\|->\\)"))))

(defun kotlin-mode--base-indentation ()
  "Return the indentation level of the current line based on brackets only,
   i.e. ignoring 'continuation' indentation."
  (cond ((kotlin-mode--line-continuation)
         (- (current-indentation) kotlin-tab-width))
        ((kotlin-mode--in-comment-block)
         (- (current-indentation) 1))
        (t
         (current-indentation))))

(defclass kotlin-mode--bracket-counter ()
  ((count :initarg :count
          :initform 0
          :type integer
          :documentation "The net bracket count (+1 for open, -1 for close).")
   (indent :initarg :indent
           :initform 0
           :type integer
           :documentation "The indentation based on bracket layout.")
   (finished :initarg :finished
             :initform nil
             :type boolean
             :documentation "Whether the counting has finished.")
   (use-base :initarg :use-base
             :initform t
             :type boolean
             :documentation "Whether to factor out extra indentations."))
  "A class for counting brackets to find the appropriate bracket-based indent.
   The logic here involves keeping track of the net-bracket-count,
   defined as the number of open-brackets minus the number of close-brackets.
   We scroll backwards until the net-bracket-count is zero, and this point
   determines the desired indentation level for the current line.")

(cl-defmethod count-to-line-start ((counter kotlin-mode--bracket-counter))
  "Count the brackets on the current line, starting from the cursor
   position, and working backward, incrementing the count
   +1 for open-brackets, -1 for close-brackets.
   Return as soon as the overall count exceeds zero."
  (save-excursion
    (while (and (<= (oref counter count) 0) (not (bolp)))
      (backward-char)
      (cond ((looking-at "\\s\(")
             (oset counter count (+ (oref counter count) 1)))
            ((looking-at "\\s\)")
             (oset counter count (- (oref counter count) 1)))))
    (cond
     ;; If the net-bracket-count is zero, use this indentation
     ((= (oref counter count) 0)
      (oset counter finished t)
      (if (oref counter use-base)
          (add-indent counter (kotlin-mode--base-indentation))
        (add-indent counter (current-indentation))))
     ;; If we've now counted more open-brackets than close-brackets,
     ;; use the indentation of the content immediately following the
     ;; final open-bracket.
     ((> (oref counter count) 0)
      (oset counter finished t)
      (forward-char)
      (skip-syntax-forward "(")
      (skip-syntax-forward "-")
      (let (position)
        (setq position (point))
        (add-indent counter (- position (re-search-backward "^"))))))))

(cl-defmethod count-leading-close-brackets
    ((counter kotlin-mode--bracket-counter))
  "Count any close-bracket at the start of the current line."
  (if (looking-at "\\s\)")
      (oset counter use-base nil))
  (subtract-count counter (skip-syntax-forward ")")))

(cl-defmethod count-trailing-open-brackets
    ((counter kotlin-mode--bracket-counter))
  "If the bracket count is at zero, and there are open-brackets at the end
   of the line, do not count them, but add a single indentation level."
  (if (= (oref counter count) 0)
      (cond ((not (= (skip-syntax-backward "(") 0))
             (add-indent counter kotlin-tab-width)
             (oset counter use-base nil)))))

(cl-defmethod add-count ((counter kotlin-mode--bracket-counter) val)
  (oset counter count (+ (oref counter count) val)))

(cl-defmethod subtract-count ((counter kotlin-mode--bracket-counter) val)
  (oset counter count (- (oref counter count) val)))

(cl-defmethod add-indent ((counter kotlin-mode--bracket-counter) val)
  (oset counter indent (+ (oref counter indent) val)))

(cl-defmethod finished ((counter kotlin-mode--bracket-counter))
  (oref counter finished))


(defun kotlin-mode--in-comment-block ()
  "Return whether the cursor is within a standard comment block structure
   of the following format:
   /**
    * Description here
    */ "
  (save-excursion
    (let ((in-comment-block nil)
          (keep-going (not (kotlin-mode--line-ends "\\*\\*/"))))
      (while keep-going
        (kotlin-mode--prev-line)
        (cond
         ((bobp)
          (setq keep-going nil))
         ((kotlin-mode--line-contains "\\*/")
          (setq keep-going nil))
         ((kotlin-mode--line-begins "/\\*")
          (setq keep-going nil)
          (setq in-comment-block t))))
      in-comment-block)))

(defun kotlin-mode--indent-line ()
  "Indent current line as kotlin code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (kotlin-mode--beginning-of-buffer-indent)
    (let ((cur-indent 0))
      ;; Find bracket-based indentation first
      (let ((bracket-counter (kotlin-mode--bracket-counter)))
        (save-excursion
          (skip-syntax-forward "-")
          (count-leading-close-brackets bracket-counter))
        (save-excursion
          (progn (kotlin-mode--prev-line) (end-of-line))
          (count-trailing-open-brackets bracket-counter)
          (count-to-line-start bracket-counter)
          (while (and (not (finished bracket-counter)) (not (bobp)))
            (progn (kotlin-mode--prev-line) (end-of-line))
            (count-to-line-start bracket-counter)
            )
          (incf cur-indent (oref bracket-counter indent))))

      (cond ((kotlin-mode--line-continuation)
             ;; Add extra indentation if the line continues the previous one
             (incf cur-indent kotlin-tab-width))
            ((kotlin-mode--in-comment-block)
             ;; Add one space of extra indentation if inside a comment block
             (incf cur-indent)))

      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


(defun kotlin-mode--beginning-of-buffer-indent ()
  (indent-line-to 0))

;;;###autoload
(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq font-lock-defaults '((kotlin-mode--font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize-function)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'kotlin-mode--indent-line)
  (setq-local adaptive-fill-regexp comment-start-skip)

  :group 'kotlin
  :syntax-table kotlin-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode) t)

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
