;;; kotlin-mode.el --- Major mode for kotlin -*- lexical-binding: t; -*-

;; Copyright © 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Version: 2.0.0
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
(require 'cl-lib)
(require 'eieio)

(require 'kotlin-mode-lexer)

(defgroup kotlin nil
  "A Kotlin major mode."
  :group 'languages)

(defcustom kotlin-tab-width 4
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
  "Send block to Kotlin interpreter."
  (interactive)
  (save-mark-and-excursion
    (mark-paragraph)
    (kotlin-send-region (region-beginning) (region-end))))

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
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?` "\"" st)

    ;; `_' and `@' as being a valid part of a symbol
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?@ "_" st)

    ;; b-style comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23n" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\r "> b" st)
    st))

(defconst kotlin-mode--closing-brackets '(?} ?\) ?\]))

;;; Font Lock

(defconst kotlin-mode--misc-keywords
  '("package" "import"))

(defconst kotlin-mode--type-decl-keywords
  '("sealed" "inner" "data" "class" "interface" "trait" "typealias" "enum" "object"))

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

(defvar kotlin-mode--keywords
  (append kotlin-mode--misc-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords
          kotlin-mode--generic-type-parameter-keywords)
  "Keywords used in Kotlin language.")

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--modifier-keywords
  '("open" "private" "protected" "public" "lateinit"
    "override" "abstract" "final" "companion"
    "annotation" "internal" "const" "in" "out"
    "actual" "expect" "crossinline" "inline" "noinline" "external"
    "infix" "operator" "reified" "suspend" "tailrec" "vararg"))

(defconst kotlin-mode--property-keywords
  '("by" "get" "set")) ;; "by" "get" "set"

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
  "Default highlighting expression for `kotlin-mode'")

(defun kotlin-mode--match-interpolation (limit)
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

(defun kotlin-mode--prev-line ()
  "Moves up to the nearest non-empty line"
  (if (not (bobp))
      (progn
        (forward-line -1)
        (while (and (looking-at "^[ \t]*$") (not (bobp)))
          (forward-line -1)))))

(defun kotlin-mode--prev-line-begins (pattern)
  "Return whether the previous line begins with the given pattern"
  (save-excursion
    (kotlin-mode--prev-line)
    (looking-at (format "^[ \t]*%s" pattern))))

(defun kotlin-mode--prev-line-ends (pattern)
  "Return whether the previous line ends with the given pattern"
  (save-excursion
    (kotlin-mode--prev-line)
    (looking-at (format ".*%s[ \t]*$" pattern))))

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
   (and (kotlin-mode--prev-line-begins "\\(if\\|for\\|while\\)[ \t]+(")
        (kotlin-mode--prev-line-ends ")[[:space:]]*\\(\/\/.*\\|\\/\\*.*\\)?"))
   (and (kotlin-mode--prev-line-begins "else[ \t]*")
        (not (kotlin-mode--prev-line-begins "else [ \t]*->"))
        (not (kotlin-mode--prev-line-ends "{.*")))
   (or
    (kotlin-mode--line-begins "\\([.=:]\\|->\\|\\(\\(private\\|public\\|protected\\|internal\\)[ \t]*\\)?[sg]et\\b\\)"))))

(defun kotlin-mode--in-comment-block ()
  "Return whether the cursor is within a standard comment block structure
   of the following format:
   /**
    * Description here
    */"
  (save-excursion
    (let ((in-comment-block nil)
          (keep-going (and
                       (not (kotlin-mode--line-begins "\\*\\*+/"))
                       (not (kotlin-mode--line-begins "/\\*"))
                       (nth 4 (syntax-ppss)))))
      (while keep-going
        (kotlin-mode--prev-line)
        (cond
         ((kotlin-mode--line-begins "/\\*")
          (setq keep-going nil)
          (setq in-comment-block t))
         ((bobp)
          (setq keep-going nil))
         ((kotlin-mode--line-contains "\\*/")
          (setq keep-going nil))))
      in-comment-block)))

(defun kotlin-mode--first-line-p ()
  "Determine if point is on the first line."
  (save-excursion
    (beginning-of-line)
    (bobp)
    )
  )

(defun kotlin-mode--line-closes-block-p ()
  "Return whether or not the start of the line closes its containing block."
  (save-excursion
    (back-to-indentation)
    (memq (following-char) kotlin-mode--closing-brackets)
    ))

(defun kotlin-mode--get-opening-char-indentation (parser-state-index)
  "Determine the indentation of the line that starts the current block.

Caller must pass in PARSER-STATE-INDEX, which refers to the index
of the list returned by `syntax-ppss'.

If it does not exist, will return nil."
  (save-excursion
    (back-to-indentation)
    (let ((opening-pos (nth parser-state-index (syntax-ppss))))
      (when opening-pos
        (goto-char opening-pos)
        (current-indentation)))
    )
  )

(defun kotlin-mode--indent-for-continuation ()
  "Return the expected indentation for a continuation."
  (kotlin-mode--prev-line)
  (if (kotlin-mode--line-continuation)
      (kotlin-mode--indent-for-continuation)
    (+ kotlin-tab-width (current-indentation)))
  )

(defun kotlin-mode--indent-for-code ()
  "Return the level that this line of code should be indented to."
  (let ((indent-opening-block (kotlin-mode--get-opening-char-indentation 1)))
    (cond
     ((kotlin-mode--line-continuation) (save-excursion (kotlin-mode--indent-for-continuation)))
     ((booleanp indent-opening-block) 0)
     ((kotlin-mode--line-closes-block-p) indent-opening-block)
     (t (+ indent-opening-block kotlin-tab-width)))
    ))

(defun kotlin-mode--indent-for-comment ()
  "Return the level that this line of comment should be indented to."
  (let ((opening-indentation (kotlin-mode--get-opening-char-indentation 8)))
    (if opening-indentation
        (1+ opening-indentation)
      0)
    ))

(defun kotlin-mode--indent-line ()
  "Indent the current line of Kotlin code."
  (interactive)
  (let ((follow-indentation-p
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (beginning-of-line)
      (if (bobp) ; 1.)
          (progn
            (kotlin-mode--beginning-of-buffer-indent))
        (let ((not-indented t) cur-indent)
          (cond ((looking-at "^[ \t]*\\.") ; line starts with .
                 (save-excursion
                   (kotlin-mode--prev-line)
                   (cond ((looking-at "^[ \t]*\\.")
                          (setq cur-indent (current-indentation)))

                         (t
                          (setq cur-indent (+ (current-indentation) (* 2 kotlin-tab-width)))))
                   (if (< cur-indent 0)
                       (setq cur-indent 0))))

                ((looking-at "^[ \t]*}") ; line starts with }
                 (save-excursion
                   (kotlin-mode--prev-line)
                   (while (and (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*\\.")) (not (bobp)))
                     (kotlin-mode--prev-line))
                   (cond ((or (looking-at ".*{[ \t]*$") (looking-at ".*{.*->[ \t]*$"))
                          (setq cur-indent (current-indentation)))
                         (t
                          (setq cur-indent (- (current-indentation) kotlin-tab-width)))))
                 (if (< cur-indent 0)
                     (setq cur-indent 0)))

                ((looking-at "^[ \t]*)") ; line starts with )
                 (save-excursion
                   (kotlin-mode--prev-line)
                   (setq cur-indent (- (current-indentation) kotlin-tab-width)))
                 (if (< cur-indent 0)
                     (setq cur-indent 0)))

                (t
                 (save-excursion
                   (while not-indented
                     (kotlin-mode--prev-line)
                     (cond ((looking-at ".*{[ \t]*$") ; line ends with {
                            (setq cur-indent (+ (current-indentation) kotlin-tab-width))
                            (setq not-indented nil))

                           ((looking-at "^[ \t]*}") ; line starts with }
                            (setq cur-indent (current-indentation))
                            (setq not-indented nil))

                           ((looking-at ".*{.*->[ \t]*$") ; line ends with ->
                            (setq cur-indent (+ (current-indentation) kotlin-tab-width))
                            (setq not-indented nil))

                           ((looking-at ".*([ \t]*$") ; line ends with (
                            (setq cur-indent (+ (current-indentation) kotlin-tab-width))
                            (setq not-indented nil))

                           ((looking-at "^[ \t]*).*$") ; line starts with )
                            (setq cur-indent (current-indentation))
                            (setq not-indented nil))

                           ((bobp) ; 5.)
                            (setq not-indented nil)))))))
          (if cur-indent
              (indent-line-to cur-indent)
            (indent-line-to 0)))))

    (when follow-indentation-p
      (back-to-indentation))))


(defun kotlin-mode--beginning-of-buffer-indent ()
  (indent-line-to 0))

;;;###autoload
(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq font-lock-defaults '((kotlin-mode--font-lock-keywords) nil nil))
  (setq-local parse-sexp-lookup-properties t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'kotlin-mode--syntax-propertize-extend-region
            nil t)
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize)
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
