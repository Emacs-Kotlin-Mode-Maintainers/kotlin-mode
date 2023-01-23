;;; kotlin-mode-indent.el --- Major mode for kotlin, indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2019 taku0

;; Authors: taku0 (http://github.com/taku0)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Routines for indentation

;;; Code:

(require 'rx)
(require 'cl-lib)

(require 'kotlin-mode-lexer)

;;; Customizations

(defcustom kotlin-tab-width 4
  "Amount of indentation for block contents.

Example:

class Foo {
    func foo() {} // offset of this line
}"
  :type 'integer
  :group 'kotlin
  :safe 'integerp)

(defcustom kotlin-mode-parenthesized-expression-offset 4
  "Amount of indentation inside parentheses and square brackets.

Example:

foo(
    1 // offset of this line
)"
  :type 'integer
  :group 'kotlin
  :safe 'integerp)

(defcustom kotlin-mode-multiline-statement-offset 4
  "Amount of indentation for continuations of expressions.

Example:

val x = 1 +
    2 // offset of this line"
  :type 'integer
  :group 'kotlin
  :safe 'integerp)

(defcustom kotlin-mode-prepend-asterisk-to-comment-line t
  "Automatically insert a asterisk to each comment line if non-nil.

Example: if the enter key is pressed when the point is after A below,

/*
 * A
 */

an asterisk is inserted to the newline:

/*
 * A
 *
 */"
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-insert-space-after-asterisk-in-comment t
  "Automatically insert a space after asterisk in comment if non-nil.

Example: if an asterisk is inserted before A below,

/*
A
 */

a space is inserted after asterisk:

/*
 * A
 */"
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-auto-close-multiline-comment t
  "If non-nil, `indent-new-comment-line' automatically close multiline comment.

Example: when the enter key is pressed after unclosed comment below,

/**

a closing delimiter is inserted automatically:

/**
 * // cursor is here
 */"
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-fix-comment-close t
  "Fix \"* /\" in incomplete multiline comment to \"*/\" if non-nil.

Example:

/*
 *
 * // when a slash is inserted here

/*
 *
 */ // it become like this


/*
 *
 * / // rather than like this."
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-break-line-before-comment-close t
  "If non-nil, break line before the closing delimiter of multiline comments.

Example: if line break is inserted before A below,

/** A */

it becomes like this:

/**
 * A
 */

rather than like this:

/**
 * A */"
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-indent-nonempty-line-in-multiline-string nil
  "If non-nil, indent nonempty line in multiline string.

`indent-according-to-mode' is no-op otherwise."
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

(defcustom kotlin-mode-highlight-anchor nil
  "Highlight anchor point for indentation if non-nil.

Intended for debugging."
  :type 'boolean
  :group 'kotlin
  :safe 'booleanp)

;;; Constants and variables

(defconst kotlin-mode--statement-parent-tokens
  '(\( \[ { anonymous-function-parameter-arrow when-expression-arrow
    bare-else \(\)-before-control-structure-body \; implicit-\;)
  "Parent tokens for statements.

Parent tokens are tokens before the beginning of statements.")

(defconst kotlin-mode--expression-parent-tokens
  (append kotlin-mode--statement-parent-tokens
          '(\, "where" string-chunk-before-template-expression))
  "Parent tokens for expressions.

Parent tokens are tokens before the beginning of expressions.")

(defvar-local kotlin-mode--anchor-overlay nil)
(defvar-local kotlin-mode--anchor-overlay-timer nil)

;;; Indentation struct

(defclass kotlin-mode--indentation ()
  ((position :initarg :position
             :type number
             :accessor kotlin-mode--indentation-position
             :documentation "the position of the anchor point, such as
the start of the previous line or the start of the class declaration.")
   (offset :initarg :offset
           :type number
           :initform 0
           :accessor kotlin-mode--indentation-offset
           :documentation "the offset from the anchor point.  For
example, when indenting the first line of a class body, its anchor
point is the start of the class declaration and its offset is
`kotlin-tab-width'."))
  "Indentation.")

;;; Indentation logic

(defun kotlin-mode--indent-line ()
  "Indent the current line."
  (let* ((indentation (save-excursion (kotlin-mode--calculate-indent)))
         (indentation-column
          (save-excursion
            (goto-char (kotlin-mode--indentation-position indentation))
            (+ (current-column) (kotlin-mode--indentation-offset indentation))))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The point is on the left margin.
        ;; Move the point to the new leftmost non-comment char.
        (indent-line-to indentation-column)
      ;; Keep current relative position from leftmost non-comment char.
      (save-excursion (indent-line-to indentation-column)))
    (when kotlin-mode-highlight-anchor
      (kotlin-mode-highlight-anchor indentation))))

(defun kotlin-mode--calculate-indent ()
  "Return the indentation of the current line."
  (back-to-indentation)

  (let ((parser-state (syntax-ppss)))
    (cond
     ((nth 4 parser-state)
      ;; If the 4th element of `(syntax-ppss)' is non-nil, the point is on
      ;; the 2nd or following lines of a multiline comment, because:
      ;;
      ;; - The 4th element of `(syntax-ppss)' is nil on the comment starter.
      ;; - We have called `back-to-indentation`.
      (kotlin-mode--calculate-indent-of-multiline-comment))

     ((eq (nth 3 parser-state) t)
      (kotlin-mode--calculate-indent-of-multiline-string))

     ((looking-at "//")
      (kotlin-mode--calculate-indent-of-single-line-comment))

     (t
      (kotlin-mode--calculate-indent-of-code)))))

(defun kotlin-mode--calculate-indent-of-multiline-comment ()
  "Return the indentation of the current line inside a multiline comment."
  (back-to-indentation)
  (let ((comment-beginning-position (nth 8 (syntax-ppss)))
        (starts-with-asterisk (eq (char-after) ?*)))
    (forward-line -1)
    (back-to-indentation)
    (cond
     ;; 2nd line of the comment
     ((<= (point) comment-beginning-position)
      ;; The point was on the 2nd line of the comment.
      (goto-char comment-beginning-position)
      (forward-char)
      ;; If there are extra characters or spaces after asterisks, align with
      ;; the first non-space character or end of line.  Otherwise, align with
      ;; the first asterisk.
      (when (and
             (looking-at
              (rx (seq (zero-or-more "*") (one-or-more (not (any "\n*"))))))
             (not (and kotlin-mode-prepend-asterisk-to-comment-line
                       starts-with-asterisk)))
        (skip-chars-forward "*")
        (skip-syntax-forward " "))
      (make-instance 'kotlin-mode--indentation :position (point) :offset 0))

     ;; The point was on the 3rd or following lines of the comment.

     ;; Before closing delimiter
     ((= (save-excursion
           ;; Back to the original line.
           (forward-line)
           (back-to-indentation)
           (point))
         (save-excursion
           ;; Goto just before the closing delimiter.
           (goto-char comment-beginning-position)
           (if (forward-comment 1)
               (progn
                 ;; slash
                 (backward-char)
                 (skip-chars-backward "*")
                 (point))
             -1)))
      ;; Before the closing delimiter.  Align with the first asterisk of the
      ;; opening delimiter.
      ;;
      ;; TODO: If there are multiple asterisks on the closing
      ;; delimiter, and the middle lines have no leading asterisks,
      ;; align with the slash of the opening delimiter
      ;;
      ;; Example:
      ;;
      ;; /*********
      ;;    aaa
      ;; **********/
      (goto-char comment-beginning-position)
      (forward-char)
      (make-instance 'kotlin-mode--indentation :position (point) :offset 0))

     ;; Otherwise, align with a non-empty preceding line.

     ;; The previous line is empty
     ((and (bolp) (eolp))
      ;; Seek a non-empty-line.
      (while (and (bolp) (eolp) (not (bobp)))
        (forward-line -1))
      (forward-line)
      (kotlin-mode--calculate-indent-of-multiline-comment))

     ;; The previous line is not empty
     (t
      ;; Align to this line.
      (make-instance 'kotlin-mode--indentation :position (point) :offset 0)))))

(defun kotlin-mode--calculate-indent-of-multiline-string ()
  "Return the indentation of the current line inside a multiline string."
  (back-to-indentation)
  (let ((string-beginning-position
         (save-excursion (kotlin-mode--beginning-of-string))))
    (if (looking-at "\"\"\"")
        ;; Indenting the closing delimiter.  Align with the start of containing
        ;; expression with extra indentation.
        ;;
        ;; Example:
        ;;
        ;; val someString = """
        ;;     Hello.
        ;;     """
        ;;
        ;; When aligning to opening delimiter, align without offset:
        ;; foo("""
        ;;     aaa
        ;;     """)
        ;;
        ;; Possible alternative indentation:
        ;; val someString = """
        ;;     Hello.
        ;; """
        (progn
          (goto-char string-beginning-position)
          (let ((indentation
                 (kotlin-mode--calculate-indent-of-expression
                  kotlin-mode-multiline-statement-offset)))
            (if (= (kotlin-mode--indentation-position indentation)
                   string-beginning-position)
                (make-instance 'kotlin-mode--indentation
                               :position string-beginning-position
                               :offset 0)
              indentation)))
      ;; Other than the closing delimiter.
      (if (and (not (eolp))
               (not kotlin-mode-indent-nonempty-line-in-multiline-string))
          ;; The user prefers to keep indentations inside multiline string.
          (make-instance 'kotlin-mode--indentation :position (point) :offset 0)
        ;; The user prefers to indent lines inside multiline string.
        (beginning-of-line)
        (backward-char)
        (kotlin-mode--goto-non-template-expression-bol)
        (back-to-indentation)
        (cond
         ;; 2nd line of string
         ((<= (point) string-beginning-position)
          ;; The point was on the 2nd line of the string.  Align
          ;; with the start of containing expression with extra
          ;; indentation.
          (goto-char string-beginning-position)
          (kotlin-mode--calculate-indent-of-expression
           kotlin-mode-multiline-statement-offset))

         ;; The point was on the 3rd or following lines of the string.
         ;; Align with a non-empty preceding line.

         ;; The previous line is empty
         ((and (bolp) (eolp))
          ;; Seek a non-empty-line.
          (while (and (bolp) (eolp) (not (bobp)))
            (forward-line -1))
          (forward-line)
          (kotlin-mode--calculate-indent-of-multiline-string))

         ;; The previous line is not empty
         (t
          ;; Align to this line.
          (make-instance 'kotlin-mode--indentation
                         :position (point)
                         :offset 0)))))))

(defun kotlin-mode--goto-non-template-expression-bol ()
  "Back to the beginning of line that is not inside a template expression."
  (let ((chunk-beginning-position (nth 8 (syntax-ppss)))
        (matching-bracket t))
    (while (and matching-bracket
                (< (line-beginning-position) chunk-beginning-position))
      (setq matching-bracket
            (get-text-property
             chunk-beginning-position 'kotlin-property--matching-bracket))
      (when matching-bracket
        (goto-char matching-bracket)
        (setq chunk-beginning-position (nth 8 (syntax-ppss)))))
    (beginning-of-line)))

(defun kotlin-mode--calculate-indent-of-single-line-comment ()
  "Return the indentation of the current line inside a single-line comment."
  (cond
   ;; When the comment is beginning of the buffer, indent to the column 0.
   ((save-excursion
      (beginning-of-line)
      (bobp))
    (make-instance 'kotlin-mode--indentation :position (point-min) :offset 0))

   ;; If the previous line is also a single-line comment, align with it.
   ((save-excursion
      (forward-line -1)
      (skip-syntax-forward " ")
      (looking-at "//"))
    (forward-line -1)
    (skip-syntax-forward " ")
    (make-instance 'kotlin-mode--indentation :position (point) :offset 0))

   ;; Otherwise, indent like a expression.
   (t
    (kotlin-mode--calculate-indent-of-code))))

(defun kotlin-mode--calculate-indent-of-code ()
  "Return the indentation of the current line outside comments or string."
  (back-to-indentation)
  (let* ((previous-token (save-excursion (kotlin-mode--backward-token)))
         (previous-type (kotlin-mode--token-type previous-token))
         (previous-text (kotlin-mode--token-text previous-token))
         (next-token (save-excursion (kotlin-mode--forward-token)))
         (next-type (kotlin-mode--token-type next-token))
         (next-text (kotlin-mode--token-text next-token))
         (next-is-on-current-line
          (<= (kotlin-mode--token-start next-token) (line-end-position))))
    (cond
     ;; Beginning of the buffer
     ((eq previous-type 'outside-of-buffer)
      (make-instance 'kotlin-mode--indentation :position (point-min) :offset 0))

     ;; Before } on the current line.
     ;; Align with the head of the statement.
     ((and next-is-on-current-line (eq next-type '}))
      (goto-char (kotlin-mode--token-end next-token))
      (backward-list)
      (kotlin-mode--calculate-indent-after-open-curly-brace 0))

     ;; Before ) or ] on the current line.
     ;; Align with the head of the expression.
     ((and next-is-on-current-line
           (memq next-type '(\) \)-before-control-structure-body \])))
      (goto-char (kotlin-mode--token-end next-token))
      (backward-list)
      (kotlin-mode--calculate-indent-of-expression 0))

     ;; Before close angle bracket for generics (>) on the current line.
     ((and next-is-on-current-line
           (equal next-text ">")
           (save-excursion
             (goto-char (kotlin-mode--token-end next-token))
             (eq (kotlin-mode--token-type (kotlin-mode--backward-token-or-list))
                 '<>)))
      (goto-char (kotlin-mode--token-end next-token))
      (kotlin-mode--backward-token-or-list)
      (kotlin-mode--calculate-indent-of-expression 0))

     ;; Before end of a template expression on the current line.
     ((and next-is-on-current-line
           (eq next-type 'string-chunk-after-template-expression))
      (goto-char (get-text-property
                  (kotlin-mode--token-start next-token)
                  'kotlin-property--matching-bracket))
      ;; Skip "${"
      (forward-char 2)
      (kotlin-mode--backward-string-chunk)
      (kotlin-mode--calculate-indent-after-beginning-of-template-expression
       0))

     ;; Before { on the current line
     ((and next-is-on-current-line (eq next-type '{))
      (kotlin-mode--calculate-indent-after-open-curly-brace 0))

     ;; Before "else" on the current line
     ((and next-is-on-current-line (equal next-text "else"))
      (kotlin-mode--calculate-indent-of-else))

     ;; Before "where" on the current line
     ((and next-is-on-current-line (equal next-text "where"))
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       kotlin-mode-multiline-statement-offset))

     ;; Before "catch" or "finally" on the current line
     ((and next-is-on-current-line (member next-text '("catch" "finally")))
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       0
       '()
       '("catch" "finally")
       0))

     ;; Before "=" on the current line
     ;;
     ;; fun <T> foo(): T
     ;;     where
     ;;         T: A,
     ;;         T: B
     ;;     = bar()
     ((and next-is-on-current-line (equal next-text "="))
      (kotlin-mode--find-parent-and-align-with-next
       (remove '\, (remove "where" kotlin-mode--expression-parent-tokens))
       kotlin-mode-multiline-statement-offset))

     ;; Before "," on the current line
     ((and next-is-on-current-line (eq next-type '\,))
      (kotlin-mode--calculate-indent-of-prefix-comma))

     ;; After ","
     ((eq previous-type '\,)
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-after-comma))

     ;; After "catch".  Align with "try".
     ((equal previous-text "catch")
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       kotlin-mode-multiline-statement-offset))

     ;; After {
     ((eq previous-type '{)
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-after-open-curly-brace
       kotlin-tab-width))

     ;; After ( or [
     ((memq previous-type '(\( \[))
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-of-expression
       kotlin-mode-parenthesized-expression-offset
       kotlin-mode-parenthesized-expression-offset))

     ;; After open angle bracket for generics (<)
     ((and (equal previous-text "<")
           (save-excursion
             (goto-char (kotlin-mode--token-start previous-token))
             (eq (kotlin-mode--token-type (kotlin-mode--forward-token-or-list))
                 '<>)))
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-of-expression
       kotlin-mode-parenthesized-expression-offset
       kotlin-mode-parenthesized-expression-offset))

     ;; After beginning of a template expression
     ((eq previous-type 'string-chunk-before-template-expression)
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-after-beginning-of-template-expression
       kotlin-mode-parenthesized-expression-offset))

     ;; After ; or implicit-\;
     ((memq previous-type '(\; implicit-\;))
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--calculate-indent-after-semicolon))

     ;; After "->" of lambda parameters
     ((eq previous-type 'anonymous-function-parameter-arrow)
      (goto-char (cdr (kotlin-mode--find-containing-brackets
                       (kotlin-mode--token-start previous-token))))
      (kotlin-mode--calculate-indent-after-open-curly-brace
       kotlin-tab-width))

     ;; Before "->" of lambda parameters on the current line
     ((and next-is-on-current-line
           (eq next-type 'anonymous-function-parameter-arrow))
      (if (eq (kotlin-mode--token-type (kotlin-mode--backward-token-or-list))
              '{)
          (kotlin-mode--calculate-indent-after-open-curly-brace
           kotlin-tab-width)
        (goto-char (cdr (kotlin-mode--find-containing-brackets
                         (kotlin-mode--token-start next-token))))
        (kotlin-mode--align-with-next-token (kotlin-mode--forward-token))))

     ;; After "->" of when expression
     ((eq previous-type 'when-expression-arrow)
      (goto-char (kotlin-mode--token-start previous-token))
      (kotlin-mode--find-parent-and-align-with-next
       (cl-remove-if
        (lambda (e)
          (memq e '(when-expression-arrow
                    bare-else \(\)-before-control-structure-body)))
        kotlin-mode--statement-parent-tokens)
       kotlin-tab-width))

     ;; Before "->" of when expression on the current line
     ((and next-is-on-current-line (eq next-type 'when-expression-arrow))
      (if (equal (kotlin-mode--token-text previous-token) "else")
          (kotlin-mode--align-with-token
           previous-token
           kotlin-tab-width)
        (kotlin-mode--find-parent-and-align-with-next
         (cl-remove-if
          (lambda (e)
            (memq e '(when-expression-arrow
                      bare-else \(\)-before-control-structure-body)))
          kotlin-mode--statement-parent-tokens)
         kotlin-tab-width)))

     ;; After "where"
     ;;
     ;; class Foo<T>
     ;;     where
     ;;         T: A // align with "where" with offset
     ;;
     ;; class Foo<T> where
     ;;     T: A // align with "class" with offset
     ((equal previous-text "where")
      (goto-char (kotlin-mode--token-start previous-token))
      (if (kotlin-mode--bol-other-than-comments-p)
          (kotlin-mode--align-with-token
           previous-token
           kotlin-mode-multiline-statement-offset)
        (kotlin-mode--find-parent-and-align-with-next
         kotlin-mode--statement-parent-tokens
         kotlin-mode-multiline-statement-offset)))

     ;; After if, when, for, or while
     ((member previous-text '("if" "when" "for" "while"))
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       kotlin-mode-multiline-statement-offset))

     ;; After do
     ((equal previous-text "do")
      (kotlin-mode--align-with-token
       previous-token
       (if (equal next-text "while") 0 kotlin-tab-width)))

     ;; After else
     ((equal previous-text "else")
      (kotlin-mode--align-with-token
       previous-token
       kotlin-tab-width))

     ;; After "if ()", "while ()", or "for ()"
     ((eq previous-type '\)-before-control-structure-body)
      (kotlin-mode--backward-token-or-list)
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       kotlin-tab-width
       '()
       '("else")
       kotlin-tab-width))

     ;; Before ; on the current line
     ((and next-is-on-current-line (eq next-type '\;))
      (kotlin-mode--find-parent-and-align-with-next
       (remove '\; (remove 'implicit-\; kotlin-mode--statement-parent-tokens))
       0
       '(\; implicit-\;)))

     ;; Before "in" on the current line
     ((and next-is-on-current-line (equal next-text "in"))
      (kotlin-mode--calculate-indent-before-in))

     ;; After "in"
     ;;
     ;; Examples:
     ;; for (
     ;;     x
     ;;     in
     ;;     xs
     ;; ) {}
     ;;
     ;; for (x
     ;;      in
     ;;      xs) {[]}
     ;;
     ;; when (x) {
     ;;     in
     ;;         xs -> y in
     ;;                   ys
     ;;     in
     ;;         zs -> 1
     ;; }
     ;;
     ;; Foo<
     ;;     in
     ;;         X,
     ;;     @AAA in
     ;;         X
     ;; >
     ;;
     ;; val x = 1 in
     ;;     array
     ((equal previous-text "in")
      (let* ((type-and-parent
              (save-excursion
                (goto-char (kotlin-mode--token-start previous-token))
                (kotlin-mode--find-parent-of-in)))
             (type (car type-and-parent)))
        (if (or (memq type '(for <>))
                (save-excursion
                  (goto-char (kotlin-mode--token-start previous-token))
                  (not (kotlin-mode--bol-other-than-comments-p))))
            ;; for-statement and type parameters, or "in" is not at
            ;; the beginning of the line.
            (progn
              ;; Indent like a expression
              (goto-char (kotlin-mode--token-start previous-token))
              (kotlin-mode--calculate-indent-of-expression
               kotlin-mode-multiline-statement-offset))
          ;; After "in" at the beginning of the line.
          ;;
          ;; Example:
          ;;
          ;; when (x) {
          ;;     in
          ;;         xs -> 1
          ;;     in
          ;;         ys -> 1
          ;; }
          ;;
          ;; Pretend a semicolon exists before "in"
          (goto-char (kotlin-mode--token-start previous-token))
          (kotlin-mode--align-with-next-token
           (kotlin-mode--backward-token)
           kotlin-mode-multiline-statement-offset))))

     ;; Before "while" on the current line
     ((and next-is-on-current-line (equal next-text "while"))
      (let ((do-token (save-excursion (kotlin-mode--find-do-for-while))))
        (if do-token
            (kotlin-mode--align-with-token do-token)
          (kotlin-mode--calculate-indent-after-semicolon))))

     ;; Inside annotation
     ((or (and (eq previous-type 'annotation)
               (or
                ;; @file
                ;;     : // ← here
                (eq next-type ':)
                ;; @file:A
                ;;     .B // ← here
                ;; @file:A
                ;;     <A> // ← here
                (member next-text '("." "<"))))
          ;; @file:
          ;;     [ // ← here
          (and (eq previous-type ':)
               (eq next-type '\[)
               (save-excursion
                 (goto-char (kotlin-mode--token-start previous-token))
                 (eq (kotlin-mode--token-type (kotlin-mode--backward-token))
                     'annotation)))
          ;; @file:
          ;;     A // ← here
          ;; @file:A
          ;;     .
          ;;     B // ← here
          ;; @file:A
          ;;     <
          ;;         B
          ;;     > // ← here
          (and (or (eq next-type 'atom) (equal next-text ">"))
               (eq (kotlin-mode--token-type
                    (save-excursion
                      (kotlin-mode--extend-annotation-token-backward
                       next-token)))
                   'annotation)))
      (let ((start-of-annotation
             (cond
              ((eq previous-type 'annotation)
               (kotlin-mode--token-start previous-token))

              ((eq previous-type ':)
               (save-excursion
                 (goto-char (kotlin-mode--token-start previous-token))
                 (kotlin-mode--token-start (kotlin-mode--backward-token))))

              (t (kotlin-mode--token-start
                  (save-excursion (kotlin-mode--extend-annotation-token-backward
                                   next-token))))))
            (start-of-previous-line
             (save-excursion
               (kotlin-mode--backward-token-or-list t)
               (kotlin-mode--goto-non-comment-bol-with-same-nesting-level t)
               (point))))
        (goto-char (max start-of-annotation start-of-previous-line))
        (kotlin-mode--align-with-current-line
         (if (< start-of-annotation start-of-previous-line)
             ;; 3rd or following lines of the annotation.
             ;; Align with previous line without offset.
             0
           ;; 2nd line of the annotation.
           ;; Align with the start of the annotation with offset.
           kotlin-mode-multiline-statement-offset))))

     ;; After annotations or labels
     ((memq previous-type '(annotation label))
      (goto-char (kotlin-mode--token-start previous-token))
      ;; Align with preceding annotation or label at the beginning of
      ;; a line, or indent like an expression if the annotations are in
      ;; middle of a line.
      (let ((token previous-token))
        (while (and (memq (kotlin-mode--token-type token) '(annotation label))
                    (not (kotlin-mode--bol-other-than-comments-p)))
          (setq token (kotlin-mode--backward-token)))
        (unless (memq (kotlin-mode--token-type token) '(annotation label))
          ;; The annotations are in middle of a line.
          (goto-char (kotlin-mode--token-end token))))
      (if (kotlin-mode--bol-other-than-comments-p)
          ;; The annotations are at the beginning of a line.
          (kotlin-mode--align-with-current-line)
        ;; The annotations are in middle of a line.
        (kotlin-mode--align-with-next-token
         (kotlin-mode--find-parent-of-expression))))

     ;; Otherwise, it is continuation of the previous line
     (t
      (goto-char (kotlin-mode--token-end previous-token))
      (kotlin-mode--backward-token-or-list)
      (kotlin-mode--calculate-indent-of-expression
       kotlin-mode-multiline-statement-offset)))))

(defun kotlin-mode--find-parent-and-align-with-next
    (parents
     &optional
     offset
     stop-at-eol-token-types
     stop-at-bol-token-types
     bol-offset)
  "Find the parent and return indentation based on it.

A parent is, for example, the open bracket of the containing block or
semicolon of the preceding statement.

PARENTS is a list of token types that precedes the expression or the statement.
OFFSET is the offset.  If it is omitted, assumed to be 0.
See `kotlin-mode--backward-sexps-until' for the details of
STOP-AT-EOL-TOKEN-TYPES and STOP-AT-BOL-TOKEN-TYPES.
If scanning stops at STOP-AT-EOL-TOKEN-TYPES, align with the next token with
BOL-OFFSET.
If scanning stops at STOP-AT-BOL-TOKEN-TYPES, align with that token with
BOL-OFFSET.
If STOP-AT-BOL-TOKEN-TYPES or STOP-AT-BOL-TOKEN-TYPES is the symbol
`any', it matches all tokens.
The point is assumed to be on the previous line."
  (save-excursion
    (let* ((parent (kotlin-mode--backward-sexps-until
                    parents
                    stop-at-eol-token-types
                    stop-at-bol-token-types))
           (parent-end (kotlin-mode--token-end parent))
           (stopped-at-parent
            (or (memq (kotlin-mode--token-type parent) parents)
                (member (kotlin-mode--token-text parent) parents)
                (eq (kotlin-mode--token-type parent) 'outside-of-buffer)))
           (stopped-at-eol
            (and
             (not stopped-at-parent)
             stop-at-eol-token-types
             (or
              (eq stop-at-eol-token-types 'any)
              (memq (kotlin-mode--token-type parent)
                    stop-at-eol-token-types)
              (member (kotlin-mode--token-text parent)
                      stop-at-eol-token-types)))))
      (if stopped-at-parent
          (kotlin-mode--align-with-next-token parent offset)
        (when stopped-at-eol
          (goto-char parent-end)
          (forward-comment (point-max)))
        (kotlin-mode--align-with-current-line bol-offset)))))

(defun kotlin-mode--calculate-indent-of-expression
    (&optional
     offset
     bol-offset)
  "Return indentation of the current expression.

the point is assumed to be on the previous line.

OFFSET is the offset.  If it is omitted, assumed to be 0.
If scanning stops at eol, align with the next token with BOL-OFFSET."
  (save-excursion
    (let* ((pos (point))
           (parent-of-previous-line
            (save-excursion
              (kotlin-mode--goto-non-comment-bol-with-same-nesting-level)
              (kotlin-mode--backward-token)))
           (parent-of-expression (kotlin-mode--find-parent-of-expression)))
      ;; Skip annotations
      (goto-char (kotlin-mode--token-end parent-of-expression))
      (kotlin-mode--forward-annotations)
      (kotlin-mode--goto-non-comment-bol-with-same-nesting-level)
      (when (or (< (point) (kotlin-mode--token-end parent-of-expression))
                (< pos (point)))
        (goto-char (kotlin-mode--token-end parent-of-expression)))
      (setq parent-of-expression (kotlin-mode--backward-token))
      (if (<= (kotlin-mode--token-start parent-of-previous-line)
              (kotlin-mode--token-start parent-of-expression))
          ;; let x =
          ;;   1 + // indenting here
          ;;   2 +
          ;;   3
          ;;
          ;; Aligns with the parent of the expression with offset.
          (kotlin-mode--align-with-next-token parent-of-expression offset)
        ;; let x =
        ;;   1 +
        ;;   2 + // indenting here
        ;;   3   // or here
        ;;
        ;; Aligns with the previous line.
        (kotlin-mode--align-with-next-token parent-of-previous-line
                                            bol-offset)))))

(defun kotlin-mode--forward-annotations ()
  "Skip forward comments, whitespaces, labels, and annotations."
  (let (token)
    (while (progn
             (forward-comment (point-max))
             (setq token (kotlin-mode--forward-token))
             (memq (kotlin-mode--token-type token) '(annotation label))))
    (goto-char (kotlin-mode--token-start token))))

(defun kotlin-mode--backward-annotations ()
  "Skip backward comments, whitespaces, labels, and annotations."
  (let (token)
    (while (progn
             (forward-comment (- (point)))
             (setq token (kotlin-mode--backward-token))
             (memq (kotlin-mode--token-type token) '(annotation label))))
    (goto-char (kotlin-mode--token-end token))))

(defun kotlin-mode--calculate-indent-after-open-curly-brace (offset)
  "Return indentation after open curly braces.

Assuming the point is on the open brace.
OFFSET is the offset of the contents.
This function is also used for close curly braces."
  ;; If the statement is multiline expression, aligns with the start of
  ;; the line on which the open brace is:
  ;;
  ;; foo()
  ;;   .then { x ->
  ;;       foo()
  ;;       foo()
  ;;   }
  ;;   .then { x ->
  ;;       foo()
  ;;       foo()
  ;;   }
  ;;
  ;; rather than
  ;;
  ;; foo()
  ;;   .then { x ->
  ;;     foo()
  ;;     foo()
  ;; }
  ;;   .then { x ->
  ;;     foo()
  ;;     foo()
  ;; }
  ;;
  ;; Otherwise, aligns with the start of the whole statement:
  ;;
  ;; class Foo:
  ;;     Bar {
  ;;     fun foo() {}
  ;; }
  ;;
  ;; rather than
  ;;
  ;; class Foo:
  ;;     Bar {
  ;;         fun foo() {}
  ;;     }
  ;;
  ;; POSSIBLE IMPROVEMENT: those behavior should be configurable.
  ;;
  ;; FIXME: curly braces after binary operator is a part of a
  ;; multiline expression:
  ;;
  ;; class Foo:
  ;;     Bar by xs someInfixOperator { x ->
  ;;       // this is not the body of the class.
  ;;     } {
  ;;   // The body of the class.
  ;; }
  (let* ((brace-type-and-keyword-token (kotlin-mode--curly-brace-type))
         (brace-type (car brace-type-and-keyword-token))
         (keyword-token (cdr brace-type-and-keyword-token)))
    (cond
     ((memq brace-type
            '(object-literal
              lambda-literal
              if-expression
              try-expression
              catch-block
              finally-block
              when-expression))
      (goto-char (kotlin-mode--token-start keyword-token))
      (kotlin-mode--backward-annotations)
      (unless (eq brace-type 'lambda-literal)
        (forward-comment (point-max)))
      (kotlin-mode--calculate-indent-of-expression offset offset))

     ((eq brace-type 'else-block)
      (goto-char (kotlin-mode--token-start keyword-token))
      (kotlin-mode--calculate-indent-of-else offset))

     ((eq brace-type 'when-entry)
      (goto-char (kotlin-mode--token-start keyword-token))
      (if (equal (kotlin-mode--token-text
                  (save-excursion (kotlin-mode--backward-token)))
                 "else")
          (kotlin-mode--align-with-token (kotlin-mode--backward-token) offset)
        (kotlin-mode--find-parent-and-align-with-next
         (remove 'when-expression-arrow kotlin-mode--statement-parent-tokens)
         offset)))

     ((memq brace-type '(getter setter))
      (goto-char (kotlin-mode--token-start keyword-token))
      (kotlin-mode--try-backward-modifiers)
      (kotlin-mode--align-with-next-token (kotlin-mode--backward-token) offset))

     (t
      (goto-char (kotlin-mode--token-start keyword-token))
      (kotlin-mode--find-parent-and-align-with-next
       kotlin-mode--statement-parent-tokens
       offset)))))

(defun kotlin-mode--curly-brace-type ()
  "Return information about curly brace.

Return a cons (TYPE . KEYWORD-TOKEN) where TYPE is a symbol, and KEYWORD-TOKEN
is the keyword token:
- TYPE: class-declaration, KEYWORD-TOKEN: \"class\"
- TYPE: interface-declaration, KEYWORD-TOKEN: \"interface\"
- TYPE: object-declaration, KEYWORD-TOKEN: \"object\"
- TYPE: enum-entry, KEYWORD-TOKEN: identifier
- TYPE: object-literal, KEYWORD-TOKEN: \"object\"
- TYPE: anonymous-initializer, KEYWORD-TOKEN: \"init\"
- TYPE: function-declaration, KEYWORD-TOKEN: \"fun\"
- TYPE: getter, KEYWORD-TOKEN: \"get\"
- TYPE: setter, KEYWORD-TOKEN: \"set\"
- TYPE: secondary-constructor, KEYWORD-TOKEN: \"constructor\"
- TYPE: for-statement, KEYWORD-TOKEN: \"for\"
- TYPE: while-statement, KEYWORD-TOKEN: \"while\"
- TYPE: do-while-statement, KEYWORD-TOKEN: \"do\"
- TYPE: if-expression, KEYWORD-TOKEN: \"if\"
- TYPE: else-block, KEYWORD-TOKEN: \"else\"
- TYPE: when-entry, KEYWORD-TOKEN: \"->\"
- TYPE: try-expression, KEYWORD-TOKEN: \"try\"
- TYPE: catch-block, KEYWORD-TOKEN: \"catch\"
- TYPE: finally-block, KEYWORD-TOKEN: \"finally\"
- TYPE: lambda-literal, KEYWORD-TOKEN: \"{\"
- TYPE: when-expression, KEYWORD-TOKEN: \"when\"

Assuming the point is just before the open curly brace."
  ;; Other braces may appear between the keyword and the open brace:
  ;;
  ;; class Foo: Bar by if (aaa) {bbb} else {ccc} {
  ;; }
  ;;
  ;; So we maintain stack of braces and match them with keywords.
  (save-excursion
    (let* ((pos (point))
           (open-brace-points (list pos))
           (containing-brackets (kotlin-mode--find-containing-brackets (point)))
           previous-token
           previous-type
           previous-text
           brace-type
           result)
      ;; Special handling for enumEntry.
      (goto-char (cdr containing-brackets))
      (when (eq (car containing-brackets) '{)
        (let ((token (kotlin-mode--backward-sexps-until
                      (append kotlin-mode--statement-parent-tokens
                              '("enum")))))
          (when (equal (kotlin-mode--token-text token) "enum")
            (goto-char pos)
            (when (eq (kotlin-mode--token-type
                       (kotlin-mode--backward-sexps-until
                        kotlin-mode--statement-parent-tokens))
                      '{)
              ;; This must be a enumEntry.
              (goto-char pos)
              (setq previous-token (kotlin-mode--backward-token-or-list))
              (when (eq (kotlin-mode--token-type previous-token) '\(\))
                (setq previous-token (kotlin-mode--backward-token-or-list)))
              (setq result (cons 'enum-entry previous-token))))))
      (unless result
        ;;  Other cases
        (goto-char pos)
        (while open-brace-points
          (setq previous-token (kotlin-mode--backward-token-or-list))
          (setq previous-type (kotlin-mode--token-type previous-token))
          (setq previous-text (kotlin-mode--token-text previous-token))

          (cond
           ;; whenEntry.
           ;; Stop immediately.
           ((eq previous-type 'when-expression-arrow)
            (setq result (cons 'when-entry previous-token))
            (setq open-brace-points nil))

           ;; Hit beginning of the statement without seeing keyword.
           ;; Assume it is a lambda expression and stop the loop.
           ((or (eq previous-type 'outside-of-buffer)
                (memq previous-type kotlin-mode--statement-parent-tokens)
                (member previous-text kotlin-mode--statement-parent-tokens))
            (setq result (cons 'lambda-literal
                               (progn
                                 (goto-char pos)
                                 (kotlin-mode--forward-token))))
            (setq open-brace-points nil))

           ;; Found another braces.
           ;; Push it to the stack and continue the loop.
           ((eq previous-type '{})
            (push (point) open-brace-points))

           ;; Simple cases.
           ;; Those cannot be a part of an expression.
           ;; Stop immediately.
           ((member previous-text
                    '("class" "interface" "init" "fun" "get" "set"
                      "for" "while" "do"))
            (setq brace-type (assoc-default
                              previous-text
                              '(("class" . class-declaration)
                                ("interface" . interface-declaration)
                                ("init" . anonymous-initializer)
                                ;; FIXME handle anonymous function
                                ("fun" . function-declaration)
                                ("get" . getter)
                                ("set" . setter)
                                ("for" . for-statement)
                                ("while" . while-statement)
                                ("do" . do-while-statement))))
            (setq result (cons brace-type previous-token))
            (setq open-brace-points nil))

           ;; Semi-simple cases.
           ;; Those can be a part of an expression and the braces are mandatory.
           ((member previous-text '("try" "catch" "finally" "when"))
            (pop open-brace-points)
            (when (null open-brace-points)
              (setq brace-type (assoc-default
                                previous-text
                                '(("try" . try-expression)
                                  ("catch" . catch-block)
                                  ("finally" . finally-block)
                                  ("when" . when-expression))))
              (setq result (cons brace-type previous-token))))

           ;; If and else.
           ;; Those can be a part of an expression and the braces are optional.
           ((member previous-text '("if" "else"))
            (save-excursion
              (goto-char (kotlin-mode--token-end previous-token))
              (when (equal previous-text "if")
                (kotlin-mode--forward-token-or-list))

              (forward-comment (point-max))
              (when (= (point) (car open-brace-points))
                (pop open-brace-points)
                (when (null open-brace-points)
                  (setq brace-type
                        (if (equal previous-text "if")
                            'if-expression
                          'else-block))
                  (setq result (cons brace-type previous-token))))))

           ;; Object declaration or object literal.
           ((equal previous-text "object")
            (setq brace-type
                  (cond
                   ((save-excursion
                      (equal (kotlin-mode--token-text
                              (kotlin-mode--backward-token))
                             "companion"))
                    'object-declaration)
                   ((save-excursion
                      (goto-char (kotlin-mode--token-end previous-token))
                      (eq (kotlin-mode--token-type (kotlin-mode--forward-token))
                          'atom))
                    'object-declaration)
                   (t 'object-literal)))
            (pop open-brace-points)
            (when (or (null open-brace-points)
                      (eq brace-type 'object-declaration))
              (setq result (cons brace-type previous-token))
              (setq open-brace-points nil)))

           ;; Primary constructor or secondary constructor.
           ((equal previous-text "constructor")
            (let ((parent-token
                   (kotlin-mode--backward-sexps-until
                    (append kotlin-mode--statement-parent-tokens '("class")))))
              (if (equal (kotlin-mode--token-text parent-token) "class")
                  ;; primary constructor
                  (setq result (cons 'class-declaration parent-token))
                ;; secondary constructor
                (setq result (cons 'secondary-constructor previous-token)))
              (setq open-brace-points nil))))))
      result)))

(defun kotlin-mode--calculate-indent-of-else (&optional offset)
  "Return indentation for \"else\" token with OFFSET."
  ;; Align it with "if" token.
  ;; Since if-else can be nested, we keep nesting level to find matching "if".
  (let ((nesting-level 1)
        previous-token)
    (while (< 0 nesting-level)
      (setq previous-token (kotlin-mode--backward-token-or-list))
      (cond
       ((equal (kotlin-mode--token-text previous-token) "else")
        (setq nesting-level (1+ nesting-level)))

       ((equal (kotlin-mode--token-text previous-token) "if")
        (if (save-excursion
              ;; "else if" on the same line
              (and
               (equal (kotlin-mode--token-text (kotlin-mode--backward-token))
                      "else")
               (= (progn (kotlin-mode--goto-non-comment-bol)
                         (point))
                  (progn (goto-char (kotlin-mode--token-start previous-token))
                         (kotlin-mode--goto-non-comment-bol)
                         (point)))))
            ;; Skip "else-if" without changing nesting-level.
            (kotlin-mode--backward-token)
          (setq nesting-level (1- nesting-level))))

       ((memq (kotlin-mode--token-type previous-token)
              '({ \( outside-of-buffer))
        ;; Unmatched if-else
        (setq nesting-level 0)))))
  (kotlin-mode--forward-token)
  (kotlin-mode--calculate-indent-of-expression (or offset 0) (or offset 0)))

(defun kotlin-mode--calculate-indent-of-prefix-comma ()
  "Return indentation for prefix comma.

Example:

foo( 1
   , 2
   , 3
)

class Foo: A
         , B
         , C

class D<A, B, C>
    where A: AAA
        , B: BBB
        , C: CCC

This is also known as Utrecht-style in the Haskell community."
  (let ((parent (kotlin-mode--find-parent-of-list-element t)))
    (if (eq (kotlin-mode--token-type parent) '\,)
        ;; The comma was the 2nd or the following commas.
        ;; Align with the previous comma.
        (kotlin-mode--align-with-current-line)
      ;; The comma was the 1st comma.
      ;; Align with the end of the parent.
      (goto-char (kotlin-mode--token-end parent))
      (backward-char)
      (make-instance 'kotlin-mode--indentation :position (point) :offset 0))))

(defun kotlin-mode--calculate-indent-after-comma ()
  "Return indentation after comma.

Assuming the point is on the comma."
  (kotlin-mode--align-with-next-token
   (kotlin-mode--find-parent-of-list-element nil)))

(defun kotlin-mode--find-parent-of-list-element (&optional utrecht-style)
  "Move point backward to the parent token of the comma under the point.

If UTRECHT-STYLE is non-nil, stop at a comma at bol.  Otherwise, stop at a
comma at eol."
  ;; Various examples:
  ;;
  ;; val x = foo( // simple case
  ;;   1,
  ;;   2,
  ;;   3
  ;; )
  ;;
  ;; val x = xs[
  ;;   1,
  ;;   2,
  ;;   3
  ;; ]
  ;;
  ;; class Foo<A>: A1(),
  ;;               B1 by object: C1,
  ;;                             C2 {
  ;;               },
  ;;               @AAA D1
  ;;     where A: B,
  ;;           A: C,
  ;;           A: D {
  ;; }
  ;;
  ;; when (foo) {
  ;;     object: B1,
  ;;             B2 {
  ;;     },
  ;;     object: B3 by object: B4 {
  ;;             },
  ;;             B5 {
  ;;     } ->
  ;;         1
  ;; }
  ;;
  ;; fun <T>foo(x: T): Int
  ;;     where
  ;;         T: A,
  ;;         T: B,
  ;;         T: C {
  ;; }
  ;;
  ;; val <T> A<T>.foo: Int
  ;;     where
  ;;         T: A,
  ;;         T: B,
  ;;         T: C
  ;;     get() = aaa
  ;;
  ;; enum class Foo(x: Int) {
  ;;   A(1), B(2) {},
  ;;   C(3)
  ;; }
  (let ((parent (kotlin-mode--backward-sexps-until
                 (append
                  (remove '\, kotlin-mode--expression-parent-tokens)
                  '("<" : {}))
                 (if utrecht-style nil '(\,))
                 (if utrecht-style '(\,) nil))))
    (cond
     ((equal (kotlin-mode--token-text parent) "<")
      (if (save-excursion
            (eq (kotlin-mode--token-type (kotlin-mode--forward-token-or-list))
                '<>))
          parent
        (kotlin-mode--find-parent-of-list-element utrecht-style)))

     ((eq (kotlin-mode--token-type parent) '{})
      (let* ((brace-type-and-keyword-token (kotlin-mode--curly-brace-type))
             (keyword-token (cdr brace-type-and-keyword-token)))
        (goto-char (kotlin-mode--token-start keyword-token)))
      (kotlin-mode--find-parent-of-list-element utrecht-style))

     ((eq (kotlin-mode--token-type parent) ':)
      (if (kotlin-mode--colon-before-delegation-specifiers-p parent)
          parent
        (kotlin-mode--find-parent-of-list-element utrecht-style)))

     (t
      parent))))

(defun kotlin-mode--find-parent-of-expression ()
  "Move point backward to the parent token of the expression under the point."
  ;; TODO Unify with kotlin-mode--find-parent-of-list-element
  (let ((parent (kotlin-mode--backward-sexps-until
                 (append kotlin-mode--expression-parent-tokens '("<" : {})))))
    (cond
     ((equal (kotlin-mode--token-text parent) "<")
      (if (save-excursion
            (eq (kotlin-mode--token-type (kotlin-mode--forward-token-or-list))
                '<>))
          parent
        (kotlin-mode--find-parent-of-expression)))

     ((eq (kotlin-mode--token-type parent) '{})
      (let* ((brace-type-and-keyword-token (kotlin-mode--curly-brace-type))
             (keyword-token (cdr brace-type-and-keyword-token)))
        (goto-char (kotlin-mode--token-start keyword-token)))
      (kotlin-mode--find-parent-of-expression))

     ((eq (kotlin-mode--token-type parent) ':)
      (if (kotlin-mode--colon-before-delegation-specifiers-p parent)
          parent
        (kotlin-mode--find-parent-of-expression)))

     (t
      parent))))

(defun kotlin-mode--colon-before-delegation-specifiers-p (token)
  "Return non-nil if TOKEN is a colon before delegationSpecifiers."
  (and
   (eq (kotlin-mode--token-type token) ':)
   (save-excursion
     (goto-char (kotlin-mode--token-start token))
     (let (previous-token)
       ;; Try backward primaryConstructor.
       (forward-comment (- (point)))
       (when (eq (char-before) ?\))
         (kotlin-mode--backward-token-or-list)
         (setq previous-token (kotlin-mode--backward-token))
         (if (equal (kotlin-mode--token-text previous-token) "constructor")
             (kotlin-mode--try-backward-modifiers)
           (goto-char (kotlin-mode--token-end previous-token))))
       (kotlin-mode--try-backward-type-parameters)
       (setq previous-token (kotlin-mode--backward-token-or-list))
       (or
        (equal (kotlin-mode--token-text previous-token) "object")
        (and
         (eq (kotlin-mode--token-type previous-token) 'atom)
         (member (kotlin-mode--token-text (kotlin-mode--backward-token-or-list))
                 '("class" "interface" "object"))))))))

(defun kotlin-mode--calculate-indent-after-beginning-of-template-expression
    (offset)
  "Return indentation after the beginning of a template expression.

It has offset specified with OFFSET.

Assuming the point is before the string chunk."
  (let ((pos (point)))
    (kotlin-mode--forward-string-chunk)
    (if (< pos (line-beginning-position))
        ;; The chunk has multiple lines.  Align with this line with offset.
        (progn
          (back-to-indentation)
          (make-instance 'kotlin-mode--indentation
                         :position (point)
                         :offset offset))
      ;; The chunk is single line.  Indent like a expression.
      (goto-char pos)
      (kotlin-mode--calculate-indent-of-expression offset offset))))

(defun kotlin-mode--calculate-indent-after-semicolon ()
  "Return indentation after semicolon.

Assuming the point is after the semicolon."
  (while (save-excursion
           (memq (kotlin-mode--token-type (kotlin-mode--backward-token))
                 '(\; implicit-\;)))
    (kotlin-mode--backward-token))
  (kotlin-mode--find-parent-and-align-with-next
   (cl-remove-if
    (lambda (e)
      (member e '(\; implicit-\; bare-else
                  \(\)-before-control-structure-body when-expression-arrow)))
    kotlin-mode--statement-parent-tokens)
   0
   '(\; implicit-\;)))

(defun kotlin-mode--calculate-indent-before-in ()
  "Return indentation after \"in\" token.

Assuming the point is before the token.
It is also assumed that the point is not just after \"{\" or \"<\"."
  (let* ((type-and-parent (kotlin-mode--find-parent-of-in))
         (type (car type-and-parent))
         (parent (cdr type-and-parent)))
    (if (memq type '(for when <>))
        (kotlin-mode--align-with-next-token parent)
      (kotlin-mode--calculate-indent-after-semicolon))))

(defun kotlin-mode--find-parent-of-in ()
  "Return parent token of \"in\" token.

Return a cons (TYPE . PARENT) where TYPE is one of symbol `for',
`when', `<>', or `other' and PARENT is the parent token, one of
`;', `implicit-;', `(', `{', or \"<\".

Assuming the point is before the token."
  ;; Examples:
  ;; for (
  ;;     x
  ;;     in // Align with token after "("
  ;;     xs
  ;; ) {}
  ;;
  ;; for (x
  ;;      in
  ;;      xs) {[]}
  ;;
  ;; when (x) {
  ;;     in xs -> 1
  ;;     in ys -> 1
  ;; }
  ;;
  ;; Foo<
  ;;     in X,
  ;;     @AAA
  ;;     in X
  ;; >
  ;;
  ;; val x = 1 // Line breaks are prohibited before infix "in" operator.
  ;; in array
  (let ((parent (save-excursion (kotlin-mode--backward-sexps-until
                                 '(\; implicit-\; \( { "<")))))
    (cond
     ;; Inside "for ()"
     ((and
       (eq (kotlin-mode--token-type parent) '\()
       (save-excursion
         (goto-char (kotlin-mode--token-start parent))
         (equal (kotlin-mode--token-text (kotlin-mode--backward-token)) "for")))
      (cons 'for parent))

     ;; Inside "when () {}"
     ((and
       (eq (kotlin-mode--token-type parent) '{)
       (save-excursion
         (goto-char (kotlin-mode--token-start parent))
         (eq (kotlin-mode--token-type (kotlin-mode--backward-token-or-list))
             '\(\))
         (equal (kotlin-mode--token-text (kotlin-mode--backward-token))
                "when")))
      (cons 'when parent))

     ;; Inside type parameters
     ((equal (kotlin-mode--token-text parent) "<")
      (if (save-excursion
            (goto-char (kotlin-mode--token-start parent))
            (eq (kotlin-mode--token-type
                 (kotlin-mode--forward-token-or-list))
                '<>))
          (cons '<> parent)
        (save-excursion
          (goto-char (kotlin-mode--token-start parent))
          (kotlin-mode--find-parent-of-in))))

     (t
      (cons 'other parent)))))

(defun kotlin-mode--backward-sexps-until (token-types
                                          &optional
                                          stop-at-eol-token-types
                                          stop-at-bol-token-types)
  "Backward sexps until a token with one of given token types appears.

Return the token.
When this function returns, the point is at the start of the token.

TOKEN-TYPES is a list of guard token types.  This function backs to a
token with one of those token types.
STOP-AT-EOL-TOKEN-TYPES is a list of token types that if we skipped
the end of a line just after a token with one of given token type, the
function returns.  Typically, this is a list of token types that
separates list elements (e.g.  ',', ';').  If STOP-AT-EOL-TOKEN-TYPES
is the symbol `any', it matches all tokens.
STOP-AT-BOL-TOKEN-TYPES is a list of token types that if we hit the
beginning of a line just before a token with one of given token types,
the function returns.  Typically, this is a list of token types that
starts list element .  If STOP-AT-BOL-TOKEN-TYPES is the symbol `any',
it matches all tokens."
  (let*
      ((parent (kotlin-mode--backward-token-or-list))
       (type (kotlin-mode--token-type parent))
       (text (kotlin-mode--token-text parent)))
    (while (not
            ;; Stop loop when...
            (or
             ;; Hit a guard token.
             (memq type token-types)
             (member text token-types)

             ;; Beginning of the buffer.
             (eq type 'outside-of-buffer)

             ;; When this function is called on "," token before position (1),
             ;; this function stops just before the "," token after "Foo".
             ;;
             ;; Foo,
             ;; Bar, Baz, // (1)
             ;; AAA
             (and stop-at-eol-token-types
                  (or (eq stop-at-eol-token-types 'any)
                      (member type stop-at-eol-token-types)
                      (member text stop-at-eol-token-types))
                  (save-excursion
                    (kotlin-mode--forward-token-or-list)
                    (forward-comment (- (point)))
                    (kotlin-mode--eol-other-than-comments-p)))

             ;; When this function is called on "," token before position
             ;; (1), this function stops just before ", Bar".
             ;;
             ;; , Foo
             ;; , Bar, Baz:
             ;; , AAA // (1)
             (and stop-at-bol-token-types
                  (and
                   (or
                    (eq stop-at-bol-token-types 'any)
                    (member type stop-at-bol-token-types)
                    (member text stop-at-bol-token-types))
                   (kotlin-mode--bol-other-than-comments-p)))))
      (setq parent (kotlin-mode--backward-token-or-list))
      (setq type (kotlin-mode--token-type parent))
      (setq text (kotlin-mode--token-text parent)))
    parent))

(defun kotlin-mode--align-with-next-token (parent &optional offset)
  "Return indentation with the next token of PARENT with OFFSET.

Example:

Suppose indenting \"B\":

foo {
  /* */ A()
  B()
}

The parent is \"{\".  We align with the comment before \"A\"."
  (let ((parent-end (kotlin-mode--token-end parent)))
    (goto-char parent-end)
    (forward-comment (point-max))
    (kotlin-mode--goto-non-comment-bol)
    (when (< (point) parent-end)
      (goto-char parent-end))
    (kotlin-mode--skip-whitespaces)
    (make-instance 'kotlin-mode--indentation
                   :position (point)
                   :offset (or offset 0))))

(defun kotlin-mode--align-with-token (token &optional offset)
  "Return indentation with the TOKEN with OFFSET.

If the token is preceded by comments on the same line, align with that
comments instead.

Example:

Suppose indenting \"B\":

foo {
  /* */ A()
  B()
}

We align with the comment before \"A\"."
  (goto-char (kotlin-mode--token-start token))
  (forward-comment (- (point)))
  (kotlin-mode--align-with-next-token (kotlin-mode--backward-token) offset))

(defun kotlin-mode--align-with-current-line (&optional offset)
  "Return indentation of the current line with OFFSET."
  (kotlin-mode--goto-non-comment-bol)
  (kotlin-mode--skip-whitespaces)
  (make-instance 'kotlin-mode--indentation
                 :position (point)
                 :offset (or offset 0)))

(defun kotlin-mode--goto-non-comment-bol-with-same-nesting-level
    (&optional use-backward-token-simple)
  "Back to the beginning of line that is not inside a comment.

See `kotlin-mode--backward-token-or-list' for USE-BACKWARD-TOKEN-SIMPLE."
  (while (not (kotlin-mode--bol-other-than-comments-p))
    (kotlin-mode--backward-token-or-list use-backward-token-simple)))


;;; indent-new-comment-line

(defun kotlin-mode--indent-new-comment-line (&optional soft)
  "Break the line at the point and indent the new line.

If the point is inside a comment, continue the comment.  If the comment is a
multiline comment, close the previous comment and start new one if
`comment-multi-line' is nil.
See `indent-new-comment-line' for SOFT."
  (interactive)
  (let* ((chunk (kotlin-mode--chunk-after))
         (comment-beginning-position (kotlin-mode--chunk-start chunk)))
    (if soft (insert-and-inherit ?\n) (newline 1))
    (delete-horizontal-space)

    ;; Insert a prefix and indent the line.
    (cond
     ((not (kotlin-mode--chunk-comment-p chunk))
      (indent-according-to-mode))

     ((kotlin-mode--chunk-single-line-comment-p chunk)
      (insert-and-inherit
       (save-excursion
         (goto-char comment-beginning-position)
         (looking-at "/+\\s *")
         (match-string-no-properties 0)))
      (indent-according-to-mode))

     ((not comment-multi-line)
      (insert-and-inherit
       (save-excursion
         (goto-char comment-beginning-position)
         (looking-at "/\\*+\\s *")
         (match-string-no-properties 0)))
      ;; Clean up and close the previous line.
      (save-excursion
        (beginning-of-line)
        (backward-char)
        (delete-horizontal-space)
        (insert-and-inherit " */"))
      (indent-according-to-mode))

     (t
      (kotlin-mode--format-multiline-comment-line-after-newline chunk soft)))
    ;; Clean up the previous line.
    (save-excursion
      (beginning-of-line)
      (backward-char)
      (delete-horizontal-space))))

(defun kotlin-mode--format-multiline-comment-line-after-newline (chunk soft)
  "Insert prefix and indent current line in multiline comment.

The point is assumed inside multiline comment and just after newline.

The closing delimiter is also inserted and/or formatted depending on custom
variables `kotlin-mode-auto-close-multiline-comment' and
`kotlin-mode-break-line-before-comment-close'.

CHUNK is the comment chunk.

See `indent-new-comment-line' for SOFT."
  (let ((comment-beginning-position (kotlin-mode--chunk-start chunk)))
    (cond
     ((save-excursion
        (forward-line -1)
        (<= (point) comment-beginning-position))
      ;; The point was on the 2nd line of the comment.

      ;; If the comment have only one line, delete a space after asterisk.
      ;;
      ;; Example:
      ;; /** aaa */
      ;; ↓
      ;; /**
      ;;  * aaa
      ;;  */
      ;;
      ;; /**  aaa */
      ;; ↓
      ;; /**
      ;;  *  aaa
      ;;  */
      ;;
      ;;
      ;; If the comment spans several lines, keep spaces.
      ;;
      ;; /** aaa
      ;;  */
      ;; ↓
      ;; /**
      ;;  *  aaa
      ;;  */
      (when (= (line-beginning-position)
               (save-excursion
                 (goto-char comment-beginning-position)
                 (forward-comment 1)
                 (line-beginning-position)))
        (save-excursion
          (goto-char comment-beginning-position)
          (forward-char)
          (skip-chars-forward "*")
          (when (looking-at " [ \t]*$")
            (delete-char 1))))

      ;; If the point is just before the closing delimiter, break the line.
      (when (and kotlin-mode-break-line-before-comment-close
                 (= (point)
                    (save-excursion
                      (goto-char comment-beginning-position)
                      (if (forward-comment 1)
                          (progn
                            (backward-char)
                            (skip-chars-backward "*")
                            (point))
                        -1))))
        (save-excursion
          (if soft (insert-and-inherit ?\n) (newline 1))
          (indent-according-to-mode)))

      ;; Invoke `kotlin-mode--indent-line`.
      (indent-according-to-mode)

      ;; Insert or replace a space to an asterisk.
      (when kotlin-mode-prepend-asterisk-to-comment-line
        (let ((columns-from-end (- (line-end-position) (point))))
          (move-to-column
           (save-excursion
             (goto-char comment-beginning-position)
             (forward-char)
             (current-column)))
          (insert-and-inherit "*")
          (when (eq (char-after) ?\s)
            (delete-char 1))
          (when (and
                 kotlin-mode-insert-space-after-asterisk-in-comment
                 (not (eq (char-after) ?\s)))
            (insert-and-inherit " "))
          (goto-char (- (line-end-position) columns-from-end)))))

     ;; The point was on the 3nd or following lines of
     ;; the comment.
     ;; Use the prefix of the previous line.

     ((and
       kotlin-mode-prepend-asterisk-to-comment-line
       (save-excursion
         (forward-line -1)
         (looking-at "\\s *\\(\\*+\\s *\\)")))
      ;; The previous line has a prefix.  Use it.
      (insert-and-inherit (match-string-no-properties 1))
      (indent-according-to-mode))

     (t
      ;; Use the default indentation.
      (indent-according-to-mode)))

    ;; Close incomplete multiline comment.
    (when (and kotlin-mode-auto-close-multiline-comment
               (kotlin-mode--incomplete-comment-p chunk))
      (save-excursion
        (end-of-line)
        (when comment-multi-line
          (if soft (insert-and-inherit ?\n) (newline 1)))
        (insert-and-inherit "*/")
        (indent-according-to-mode)))

    ;; Make sure the closing delimiter is on its own line.
    (when kotlin-mode-break-line-before-comment-close
      (save-excursion
        (goto-char comment-beginning-position)
        (when (forward-comment 1)
          (backward-char)
          (skip-chars-backward "*")
          (skip-syntax-backward " ")
          (when (not (bolp))
            (if soft (insert-and-inherit ?\n) (newline 1))
            (indent-according-to-mode)))))))

(defun kotlin-mode--post-self-insert ()
  "Miscellaneous logic for electric indentation."
  (cond
   ;; Indent electrically and insert a space when "*" is inserted at the
   ;; beginning of a line inside a multiline comment.
   ((and
     kotlin-mode-prepend-asterisk-to-comment-line
     (eq last-command-event ?*)
     (kotlin-mode--chunk-comment-p (kotlin-mode--chunk-after))
     (save-excursion (backward-char) (skip-syntax-backward " ") (bolp)))
    (when kotlin-mode-insert-space-after-asterisk-in-comment
      (insert-and-inherit " "))
    (when electric-indent-mode
      (indent-according-to-mode)))

   ;; Fixe "* /" at the end of a multiline comment to "*/".
   ((and
     kotlin-mode-fix-comment-close
     (eq last-command-event ?/)
     (let ((chunk (kotlin-mode--chunk-after))
           (pos (point)))
       (and
        (kotlin-mode--chunk-comment-p chunk)
        (save-excursion
          (beginning-of-line)
          (and
           (looking-at "^\\s *\\*\\s +/")
           (eq (match-end 0) pos)
           (kotlin-mode--incomplete-comment-p chunk))))))
    (backward-char)
    (delete-horizontal-space)
    (forward-char))

   ;; Indent electrically when "}" is inserted at bol as the end of a string
   ;; interpolation.
   ((and
     electric-indent-mode
     (eq last-command-event ?\})
     (save-excursion (backward-char) (skip-syntax-backward " ") (bolp))
     (eq (kotlin-mode--chunk-start (kotlin-mode--chunk-after)) (1- (point))))
    (indent-according-to-mode))

   ;; Indent electrically after newline inside strings and comments.
   ;; Unlike `electric-indent-mode', the previous line is not indented.
   ((and
     electric-indent-mode
     (eq last-command-event ?\n))
    (let ((chunk (kotlin-mode--chunk-after)))
      (if (kotlin-mode--chunk-multiline-comment-p chunk)
          (progn
            (delete-horizontal-space)
            (kotlin-mode--format-multiline-comment-line-after-newline
             chunk
             (not use-hard-newlines)))
        (indent-according-to-mode)))
    (save-excursion
      (beginning-of-line)
      (backward-char)
      (delete-horizontal-space)))))

(defun kotlin-mode-highlight-anchor (indentation)
  "Highlight the anchor point of the INDENTATION."
  (move-overlay
   kotlin-mode--anchor-overlay
   (kotlin-mode--indentation-position indentation)
   (1+ (kotlin-mode--indentation-position indentation)))

  (overlay-put kotlin-mode--anchor-overlay 'face 'highlight)

  (when kotlin-mode--anchor-overlay-timer
    (cancel-timer kotlin-mode--anchor-overlay-timer))

  (let ((buffer (current-buffer)))
    (setq kotlin-mode--anchor-overlay-timer
          (run-at-time
           "1 sec"
           nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (delete-overlay kotlin-mode--anchor-overlay)
                 (setq kotlin-mode--anchor-overlay-timer nil))))))))

(provide 'kotlin-mode-indent)

;;; kotlin-mode-indent.el ends here
