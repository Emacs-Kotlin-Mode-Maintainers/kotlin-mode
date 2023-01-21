;;; kotlin-mode-lexer.el --- Major mode for kotlin, lexer -*- lexical-binding: t; -*-

;; Copyright © 2015 Shodai Yokoyama
;; Copyright © 2019 taku0

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;;         taku0 (http://github.com/taku0)

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

;;  Lexical level routines.

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'eieio)

;; Terminology:
;;   https://kotlinlang.org/docs/reference/basic-types.html#string-templates
;;   See also doc/string.png.
;;
;;   String template:
;;     A string containing expressions to be evaluated and inserted into the
;;     string at run time.
;;     Example: "1 + 1 = ${1 + 1}" is evaluated to "1 + 1 = 2" at run time.
;;
;;   Template expression:
;;     An expression between ${ and } inside a string.
;;     Suppose a string "aaa${ foo() }bbb${ bar() }ccc",
;;     `foo()' and `bar()' are template expressions.
;;
;;   String chunk:
;;     A part of single-line/multiline string delimited with quotation marks
;;     or template expressions.
;;     Suppose a string "aaa${ foo() }bbb${ bar() }ccc",
;;     "aaa${, }bbb${, and }ccc" are string chunks.
;;
;;     This is not a official term; used only in kotlin-mode.

;;; Brackets

(defvar-local kotlin-mode--bracket-positions '()
  "List of position of brackets.

Element of the list is a cons (TYPE . POSITION) where TYPE is one of
\(, ), {, }, [, or ], and POSITION is the position of the token.

Elements are sorted by the position in descending order.")

(defun kotlin-mode--clear-bracket-positions-after (position)
  "Remove bracket positions after or equal to POSITION from cache."
  (while (and kotlin-mode--bracket-positions
              (<= position (cdar kotlin-mode--bracket-positions)))
    (pop kotlin-mode--bracket-positions)))

(defun kotlin-mode--find-containing-brackets (position)
  "Return start position of innermost brackets containing POSITION.

Return a cons (TYPE . POSITION) where TYPE is one of (, ), {, }, [, ],
or outside-of-buffer, and POSITION is the position of the token."
  (let ((brackets kotlin-mode--bracket-positions)
        (nesting-level 1))
    (while (and brackets (<= position (cdar brackets)))
      (pop brackets))
    (while (and brackets (not (zerop nesting-level)))
      (if (memq (caar brackets) '(\( \[ {))
          (setq nesting-level (1- nesting-level))
        (setq nesting-level (1+ nesting-level)))
      (unless (zerop nesting-level)
        (pop brackets)))
    (if brackets
        (car brackets)
      (cons 'outside-of-buffer (point-min)))))

;;; Text properties

;; See also doc/string_properties.png.
;;
;; Some properties are put by `syntax-propertize-function', that is
;; `kotlin-mode--syntax-propertize'.
;;
;; The beginning of and end of strings, character literals, backquoted
;; identifiers are marked with text property '(syntax-table (15)),
;; which indicates generic string delimiters.  Both single-line
;; strings and multiline strings are marked with it. The brackets
;; surrounding template expressions are also marked with
;; '(syntax-table (15)). The property helps font-lock and the
;; tokenizer to recognize strings.
;;
;; The entire string including template expressions, character
;; literal, backquoted identifiers are marked with text property
;; '(syntax-multiline t).  The property is used by
;; `kotlin-mode--syntax-propertize-extend-region' to avoid scanning
;; from the middle of strings.
;;
;; The brackets surrounding template expressions have text property
;; '(kotlin-property--matching-bracket POS), where POS is the position
;; of the matching bracket.  Strictly speaking, the POS on the closing
;; bracket refers to the dollar sign before the opening
;; parenthesis. The property speeds up the indentation logic.

(defun kotlin-mode--syntax-propertize-extend-region (start end)
  "Return region to be propertized.

The returned region contains the region (START . END).
If the region is not modified, return nil.
Intended for `syntax-propertize-extend-region-functions'."
  (syntax-propertize-multiline start end))

(defun kotlin-mode--syntax-propertize (start end)
  "Update text properties for strings.

Mark the beginning of and the end of single-line/multiline
strings, character literals, backquoted identifiers between the
position START and END as general string delimiters.

Intended for `syntax-propertize-function'.

Also track position of brackets in `kotlin-mode--bracket-positions'."
  (remove-text-properties start end
                          '(syntax-table
                            nil
                            syntax-multiline
                            nil
                            kotlin-property--matching-bracket
                            nil
                            kotlin-property--interpolation
                            nil))
  (kotlin-mode--clear-bracket-positions-after start)
  (let* ((chunk (kotlin-mode--chunk-after (syntax-ppss start))))
    (cond
     ((kotlin-mode--chunk-multiline-string-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "\"\"\""))

     ((kotlin-mode--chunk-single-line-string-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "\""))

     ((kotlin-mode--chunk-character-literal-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "'"))

     ((kotlin-mode--chunk-backquoted-identifier-p chunk)
      (kotlin-mode--syntax-propertize-end-of-string end "`"))

     ((kotlin-mode--chunk-comment-p chunk)
      (goto-char (kotlin-mode--chunk-start chunk))
      (forward-comment (point-max)))))

  (kotlin-mode--syntax-propertize-scan end 0))

(defun kotlin-mode--syntax-propertize-scan (end nesting-level)
  "Update text properties for strings.

Mark the beginning of and the end of single-line/multiline
strings and character literals between the current position and
END as general string delimiters.

Assuming the point is not on strings, character-literal,
backquoted identifier, nor comments.

If NESTING-LEVEL is non-zero, nesting of brackets are tracked and
the scan stops where the level becomes zero."
  (let ((found-matching-bracket nil)
        (pattern
         (rx (or "\"\"\"" "\"" "//" "/*" "{" "}" "(" ")" "[" "]" "'" "`")))
        match-string
        start)
    (while (and (not found-matching-bracket)
                (< (point) end)
                (search-forward-regexp pattern end t))
      (setq match-string (match-string-no-properties 0))
      (setq start (match-beginning 0))
      (cond
       ;; Quotes
       ((member match-string '("\"\"\"" "\"" "'" "`"))
        ;; Mark the start of the quotes as a generic string delimiter.
        (put-text-property start (1+ start)
                           'syntax-table
                           (string-to-syntax "|"))
        ;; Scan until end of the string.
        (kotlin-mode--syntax-propertize-end-of-string end match-string)
        ;; Mark whole string, including template expressions,
        ;; with `syntax-multiline'.  The property is used by
        ;; `kotlin-mode--syntax-propertize-extend-region'.
        (put-text-property start (point) 'syntax-multiline t)

        (when (equal match-string "`")
          ;; Backquotes cannot be escaped.  So declare the backslashes in
          ;; the identifier are normal characters, not escape-syntax characters.
          (put-text-property (1+ start) (1- (point))
                             'syntax-table
                             (string-to-syntax "w"))))

       ;; Single-line comment
       ((equal "//" match-string)
        (goto-char start)
        (forward-comment (point-max)))

       ;; Multiline comment
       ((equal "/*" match-string)
        (goto-char start)
        (forward-comment (point-max)))

       ;; Open curly bracket
       ((and (equal "{" match-string)
             (/= nesting-level 0))
        (push (cons (intern match-string) start) kotlin-mode--bracket-positions)
        (setq nesting-level (1+ nesting-level)))

       ;; Close curly bracket
       ((and (equal "}" match-string)
             (/= nesting-level 0))
        (push (cons (intern match-string) start) kotlin-mode--bracket-positions)
        (setq nesting-level (1- nesting-level))
        (when (= nesting-level 0)
          (setq found-matching-bracket t)))

       ;; Other brackets
       ((member match-string '("(" ")" "[" "]" "{" "}"))
        (push (cons (intern match-string) start)
              kotlin-mode--bracket-positions))))
    (unless found-matching-bracket
      (goto-char end))
    found-matching-bracket))

(defun kotlin-mode--syntax-propertize-end-of-string (end quotation)
  "Move point to the end of single-line/multiline string.

Assuming the point is on a string, a character literal, or a backquoted
identifier.
If the string go beyond END, stop there.
The string should be terminated with QUOTATION."
  (if (and (< (point) end)
           (search-forward-regexp
            (rx-to-string
             `(or ,quotation
                  "${"
                  (and "$" (or
                            (and (char alpha "_") (* (char alnum "_")))
                            (and "`" (+ (not (any "`\n"))) "`")))))
            end t))
      (cond
       ;; End of the string
       ((and (equal quotation (match-string-no-properties 0))
             (or (equal quotation "`")  ; backquotes cannot be escaped
                 (not (kotlin-mode--escaped-p (match-beginning 0)))))
        (put-text-property (1- (point)) (point)
                           'syntax-table
                           (string-to-syntax "|")))

       ;; Start of a template expression
       ((and (equal "${" (match-string-no-properties 0))
             (member quotation '("\"\"\"" "\""))
             ;; Dollar signs cannot be escaped, so we don't need to check it.
             )
        ;; Found an template expression. Skips the expression.
        ;; We cannot use `scan-sexps' because multiline strings are not yet
        ;; propertized.
        (let ((pos-after-open-bracket (point))
              (start
               (save-excursion
                 (backward-char) ;; {
                 (backward-char) ;; $
                 (point))))
          ;; Keep the position of the open bracket.
          (push (cons '{ (1+ start)) kotlin-mode--bracket-positions)
          ;; Declare the open bracket is a generic string delimiter.
          (put-text-property
           (1- pos-after-open-bracket) pos-after-open-bracket
           'syntax-table
           (string-to-syntax "|"))
          ;; Try to skip
          (when (kotlin-mode--syntax-propertize-scan end 1)
            ;; Found the matching bracket. Going further.
            ;; Declare the close bracket is a generic string delimiter.
            (put-text-property (1- (point)) (point)
                               'syntax-table
                               (string-to-syntax "|"))
            ;; Record the positions.
            (put-text-property (1- (point)) (point)
                               'kotlin-property--matching-bracket
                               start)
            (put-text-property start pos-after-open-bracket
                               'kotlin-property--matching-bracket
                               (1- (point)))
            ;; Proceed.
            (kotlin-mode--syntax-propertize-end-of-string end quotation))))

       ;; Template expression without braces
       ((and (eq (aref (match-string-no-properties 0) 0) ?$)
             (not (eq (aref (match-string-no-properties 0) 1) ?{)))
        (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                           'kotlin-property--interpolation
                           (match-data))
        (kotlin-mode--syntax-propertize-end-of-string end quotation))

       ;; Others
       (t
        (kotlin-mode--syntax-propertize-end-of-string end quotation)))
    (goto-char end)))

(defun kotlin-mode--escaped-p (position)
  "Return t if the POSITION in a string is escaped.

A position is escaped if it is proceeded by odd number of backslashes.
Return nil otherwise."
  (let ((p position)
        (backslash-count 0))
    (while (eq (char-before p) ?\\)
      (setq backslash-count (1+ backslash-count))
      (setq p (1- p)))
    (= (mod backslash-count 2) 1)))


;;; Comment or string chunks

(defclass kotlin-mode--chunk ()
  ((type :initarg :type
         :type symbol
         :documentation "The type of the chunk.

Valid values:
- single-line-string
- multiline-string
- single-line-comment
- multiline-comment
- character-literal
- backquoted-identifier")
   (start :initarg :start
          :type number
          :documentation "The start position of the chunk."))
  "String-chunks, comments, character literals, or backquoted identifiers.

It have the type and the start position.")

(defun kotlin-mode--chunk-type (chunk)
  "Return the type of the CHUNK."
  (and chunk (oref chunk type)))

(defun kotlin-mode--chunk-start (chunk)
  "Return the start position of the CHUNK."
  (and chunk (oref chunk start)))

(defun kotlin-mode--chunk-comment-p (chunk)
  "Return non-nil if the CHUNK is a comment."
  (and chunk
       (memq (kotlin-mode--chunk-type chunk)
             '(single-line-comment multiline-comment))))

(defun kotlin-mode--chunk-string-p (chunk)
  "Return non-nil if the CHUNK is a string."
  (and chunk
       (memq (kotlin-mode--chunk-type chunk)
             '(single-line-string multiline-string))))

(defun kotlin-mode--chunk-single-line-comment-p (chunk)
  "Return non-nil if the CHUNK is a single-line comment."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'single-line-comment)))

(defun kotlin-mode--chunk-multiline-comment-p (chunk)
  "Return non-nil if the CHUNK is a multiline comment."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'multiline-comment)))

(defun kotlin-mode--chunk-single-line-string-p (chunk)
  "Return non-nil if the CHUNK is a single-line string."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'single-line-string)))

(defun kotlin-mode--chunk-multiline-string-p (chunk)
  "Return non-nil if the CHUNK is a multiline string."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'multiline-string)))

(defun kotlin-mode--chunk-character-literal-p (chunk)
  "Return non-nil if the CHUNK is a character literal."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'character-literal)))

(defun kotlin-mode--chunk-backquoted-identifier-p (chunk)
  "Return non-nil if the CHUNK is a backquoted identifier."
  (and chunk (eq (kotlin-mode--chunk-type chunk) 'backquoted-identifier)))

(defun kotlin-mode--incomplete-comment-p (chunk)
  "Return t if the CHUNK is incomplete comment.

Return nil otherwise."
  (and (kotlin-mode--chunk-comment-p chunk)
       (save-excursion
         (goto-char (kotlin-mode--chunk-start chunk))
         (not (forward-comment 1)))))

(defun kotlin-mode--chunk-after (&optional parser-state)
  "Return the chunk at the point.

If the point is outside of strings and comments, return nil.

If PARSER-STATE is given, it is used instead of (syntax-ppss)."
  (save-excursion
    (when (number-or-marker-p parser-state)
      (goto-char parser-state))
    (when (or (null parser-state) (number-or-marker-p parser-state))
      (setq parser-state (save-excursion (syntax-ppss parser-state))))
    (cond
     ((nth 3 parser-state)
      ;; Syntax category "|" is attached to both single-line and multiline
      ;; string delimiters.  So (nth 3 parser-state) may be t even for
      ;; single-line string delimiters.
      (save-excursion
        (goto-char (nth 8 parser-state))
        (forward-char)
        (kotlin-mode--beginning-of-string)
        (cond
         ((looking-at "\"\"\"")
          (make-instance 'kotlin-mode--chunk
                         :type 'multiline-string
                         :start (nth 8 parser-state)))
         ((looking-at "`")
          (make-instance 'kotlin-mode--chunk
                         :type 'backquoted-identifier
                         :start (nth 8 parser-state)))
         ((looking-at "'")
          (make-instance 'kotlin-mode--chunk
                         :type 'character-literal
                         :start (nth 8 parser-state)))
         (t
          (make-instance 'kotlin-mode--chunk
                         :type 'single-line-string
                         :start (nth 8 parser-state))))))
     ((eq (nth 4 parser-state) t)
      (make-instance 'kotlin-mode--chunk
                     :type 'single-line-comment
                     :start (nth 8 parser-state)))
     ((nth 4 parser-state)
      (make-instance 'kotlin-mode--chunk
                     :type 'multiline-comment
                     :start (nth 8 parser-state)))
     ((and (eq (char-before) ?/) (eq (char-after) ?/))
      (make-instance 'kotlin-mode--chunk
                     :type 'single-line-comment
                     :start (1- (point))))
     ((and (eq (char-before) ?/) (eq (char-after) ?*))
      (make-instance 'kotlin-mode--chunk
                     :type 'multiline-comment
                     :start (1- (point))))
     (t
      nil))))

;; Syntax table

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

;; Token

(defclass kotlin-mode--token ()
  ((type :initarg :type
         :type symbol
         :documentation "The type of the token.

Token types is one of the following symbols:

- operator (including as, as?, is, !is, in, !in, ., and ->)
- annotation
  (e.g. @ABC, @ABC(def), @file:ABC, @[ABC DEF(ghi)], or @file:[ABC DEF(ghi)])
- atom (including keywords, numbers, strings, and unknown tokens)
- label
- return (including return@identifier)
- continue (including continue@identifier)
- beak (including beak@identifier)
- this (including this@identifier)
- super (including super@identifier)
- [
- ]
- {
- }
- (
- )
- )-before-control-structure-body
- ,
- ;
- implicit-;
- < (as an angle bracket)
- > (as an angle bracket)
- :
- string-chunk-after-template-expression (part of a string ending with \"}\")
- string-chunk-before-template-expression (part of a string ending with \"${\")
- bare-else (\"else\" not followed by \"if\" on the same line, \"->\", or \"{\")
- outside-of-buffer
- anonymous-function-parameter-arrow
- when-expression-arrow

Additionally, `kotlin-mode--backward-token-or-list' may return a parenthesized
expression as a token with one of the following types:
- ()
- ()-before-control-structure-body
- []
- {}
- <>" )
   (text :initarg :text
         :type string
         :documentation "The text of the token.")
   (start :initarg :start
          :type integer
          :documentation "The start position of the token.")
   (end :initarg :end
        :type integer
        :documentation "The point after the token."))
  "Token of Kotlin.")

(defun kotlin-mode--token-type (token)
  "Return the type of TOKEN."
  (and token (oref token type)))

(defun kotlin-mode--token-text (token)
  "Return the text of TOKEN."
  (and token (oref token text)))

(defun kotlin-mode--token-start (token)
  "Return the start position of TOKEN."
  (and token (oref token start)))

(defun kotlin-mode--token-end (token)
  "Return the end position of TOKEN."
  (and token (oref token end)))

;; Token level movements and predicates.

(defconst kotlin-mode--inheritance-modifier-keywords
  '("open" "final" "abstract"))

(defconst kotlin-mode--visibility-modifier-keywords
  '("public" "private" "internal" "protected"))

(defconst kotlin-mode--member-modifier-keywords
  '("lateinit" "override"))

(defconst kotlin-mode--companion-modifier-keywords
  '("companion"))

(defconst kotlin-mode--class-modifier-keywords
  '("enum" "sealed" "annotation" "data" "inner"))

(defconst kotlin-mode--property-modifier-keywords
  '("const"))

(defconst kotlin-mode--platform-modifier-keywords
  '("expect" "actual"))

(defconst kotlin-mode--parameter-modifier-keywords
  '("vararg" "crossinline" "noinline"))

(defconst kotlin-mode--function-modifier-keywords
  '("tailrec" "operator" "infix" "inline" "value" "external" "suspend"))

(defconst kotlin-mode--modifier-keywords
  (append kotlin-mode--inheritance-modifier-keywords
          kotlin-mode--visibility-modifier-keywords
          kotlin-mode--member-modifier-keywords
          kotlin-mode--class-modifier-keywords
          kotlin-mode--property-modifier-keywords
          kotlin-mode--platform-modifier-keywords
          kotlin-mode--parameter-modifier-keywords
          kotlin-mode--function-modifier-keywords))

(defun kotlin-mode--implicit-semi-p ()
  "Return non-nil if the point is after the end of a statement."
  (let ((previous-token (save-excursion
                          (kotlin-mode--extend-annotation-token-backward
                           (kotlin-mode--backward-token-simple))))
        (next-token (save-excursion
                      (kotlin-mode--extend-annotation-token-forward
                       (kotlin-mode--forward-token-simple)))))
    ;; If the point is on the empty line, pretend an identifier is on the line.
    (when (and
           (< (kotlin-mode--token-end previous-token) (line-beginning-position))
           (< (line-end-position) (kotlin-mode--token-start next-token)))
      (setq next-token (make-instance 'kotlin-mode--token
                                      :type 'atom
                                      :text ""
                                      :start (point)
                                      :end (point))))
    (cond
     ;; Tokens that end a statement
     ((memq (kotlin-mode--token-text previous-token)
            '("return" "continue" "break"))
      t)

     ;; .* in import declarations end a statement.
     ((and (equal (kotlin-mode--token-text previous-token) "*")
           (equal (kotlin-mode--token-text
                   (save-excursion
                     (goto-char (kotlin-mode--token-start previous-token))
                     (kotlin-mode--backward-token-simple)))
                  "."))
      t)

     ;; Suppress implicit semicolon after "if ()", "while ()", "for ()"
     ((kotlin-mode--close-parenthesis-before-control-structure-body-p
       previous-token)
      nil)

     ;; Annotations and modifiers, that cannot end a statement/declaration
     ((or
       ;; Annotations
       (and
        (eq (kotlin-mode--token-type previous-token) 'annotation)
        ;; Exclude super<A>@label.  Note that this@label is handled by
        ;; `kotlin-mode--backward-token-simple'
        (not (save-excursion
               (goto-char (kotlin-mode--token-start previous-token))
               (and (eq (char-before) ?>)
                    (kotlin-mode--try-backward-type-parameters)
                    (equal (kotlin-mode--token-text
                            (kotlin-mode--backward-token-simple))
                           "super")))))
       ;; Labels
       (eq (kotlin-mode--token-type previous-token) 'label)
       ;; Modifiers
       (and (member (kotlin-mode--token-text previous-token)
                    kotlin-mode--modifier-keywords)
            (not (equal (kotlin-mode--token-text previous-token) "value"))))
      nil)

     ;; Tokens that cannot end a statement
     ;;
     ;; TODO prefix ++ and --
     ;; TODO infix function call
     ;; TODO prefix !, especially !!
     ;;
     ;; Example:
     ;; var shl = 1
     ;; val x = shl shl shl
     ;; shl < 100 && foo() // this is not a continuation of the previous line.
     ;;
     ;; var shl = 1
     ;; val x = shl shl
     ;; shl < 100 && foo() // this is a continuation of the previous line.
     ;;
     ;; var shl = 1
     ;; val x = shl shl shl ++
     ;; shl < 100 && foo() // this is not a continuation of the previous line.
     ;;
     ;; var shl = 1
     ;; val x = shl shl ++
     ;; shl < 100 && foo() // this is a continuation of the previous line.
     ;;
     ;; val x = foo()!!
     ;; foo() // this is not a continuation of the previous line.
     ;;
     ;; val x = !!
     ;; foo() // this is a continuation of the previous line.
     ((or
       (memq
        (kotlin-mode--token-type previous-token)
        '(implicit-\; string-chunk-before-template-expression))
       (member
        (kotlin-mode--token-text previous-token)
        '("(" "{" "[" "*" "%" "/" "+" "-" "&&" "||" ":" "&"
          "=" "+=" "-=" "*=" "/=" "%="
          "->" "." ".." "..<" "::" "?:" "?." "<=" ">=" "!=" "!==" "==" "==="
          "as" "as?" "is" "!is" "in" "!in" "," ";" "{" "[" "("
          ;; "class" will be handled later.
          "package" "import" "interface" "fun" "object"
          "val" "var" "typealias" "constructor" "by" "companion" "init"
          "where" "if" "else" "when" "try" "catch" "finally" "for" "do"
          "while" "throw" "out" "reified"))
       ;; Inequality operator cannot end a statement/declaration.
       (and (eq (kotlin-mode--token-type previous-token) 'operator)
            (member (kotlin-mode--token-text previous-token) '("<" ">"))))
      nil)

     ;; "class" cannot end a statement unless preceded by "::".
     ;;
     ;; Example:
     ;;
     ;; class // No implicit semicolon here
     ;;     Foo {
     ;; }
     ;;
     ;; val x = Foo ::
     ;;   class // Implicit semicolon here
     ;; foo()
     ((and (equal (kotlin-mode--token-text previous-token) "class")
           (save-excursion
             (goto-char (kotlin-mode--token-start previous-token))
             (not (equal (kotlin-mode--token-text
                          (kotlin-mode--backward-token-simple))
                         "::"))))
      nil)

     ;; Annotations, labels, and modifiers, that start a statement/declaration,
     ;; except for getter and setter.
     ;;
     ;; "suspend" is excluded because it can be used as type modifier.
     ((or
       (memq (kotlin-mode--token-type next-token) '(annotation label))
       (member (kotlin-mode--token-text next-token)
               (remove "suspend" kotlin-mode--modifier-keywords)))
      (not (save-excursion
             (kotlin-mode--try-forward-modifiers)
             (member (kotlin-mode--token-text
                      (kotlin-mode--forward-token-simple))
                     '("get" "set")))))

     ;; Tokens that start a statement/declaration
     ((member
       (kotlin-mode--token-text next-token)
       ;; "class" will be handled later.
       '("package" "import" "interface" "val" "var" "typealias"
         "constructor" "companion" "init" "for" "do" "is"
         ;; While we should insert a semicolon before "in" in a "when"
         ;; expression or beginning of a expression, we should
         ;; suppress a semicolon before "in" in "for", type
         ;; parameters, or type arguments:
         ;;
         ;; when (x) {
         ;;     1 -> 1 // implicit semicolon here
         ;;     in xs -> 2
         ;; }
         ;;
         ;; // line breaks are prohibited before infix "in" operator.
         ;; val x = 1 // implicit semicolon here
         ;; in xs // Though this is an invalid expression anyway.
         ;;
         ;; for (
         ;;     x // no implicit semicolons here
         ;;     in
         ;;     xs
         ;; ) {}
         ;;
         ;; Foo< // no implicit semicolons here
         ;;   in X
         ;; >
         ;;
         ;; Because detecting the context is hard at lexer level, we
         ;; omit semicolon for now, and handle it later in the
         ;; indentation code.
         ))
      t)

     ;; While or do-while
     ((equal (kotlin-mode--token-text next-token) "while")
      ;; insert semicolon unless it is a part of do-while.
      (save-excursion (not (kotlin-mode--find-do-for-while))))

     ;; "class" starts a new declaration unless preceded by "::".
     ;;
     ;; Example:
     ;;
     ;; class Foo { // This start a class declaration.
     ;; }
     ;;
     ;; Foo ::
     ;;   class // This does not start a class declaration.
     ((and (equal (kotlin-mode--token-text next-token) "class")
           (not (equal (kotlin-mode--token-text previous-token) "::")))
      t)

     ;; Tokens that cannot start a statement/declaration.
     ((or (memq
           (kotlin-mode--token-type next-token)
           '(implicit-\; string-chunk-after-template-expression))
          (member
           (kotlin-mode--token-text next-token)
           '("*" "%" "/" "&" "&&" "||" ":" "=" "+=" "-=" "*=" "/=" "%="
             "->" "." ".." "..<" "::" "?:" "?." "?" "<" ">" "<=" ">="
             "!=" "!==" "==" "==="
             "," ";" ")" "]" "}"
             "as" "as?" "get" "set" "by" "where" "else" "catch" "finally"
             ;; Because detecting the context is hard at lexer level,
             ;; we omit semicolon before "in" for now, and handle it
             ;; later in the indentation code.
             "in"
             ;; Strictly speaking, open curly bracket may start a statement
             ;; as a part of lambda expression, it is rare case, so we
             ;; suppress semicolon before it.
             "{")))
      nil)

     ;; Open square bracket start a new statement unless preceded by a
     ;; colon (already handled above).
     ((eq (kotlin-mode--token-type next-token) '\[)
      t)

     ;; Open parenthesis start a new statement unless preceded by:
     ;; - a colon (already handled above),
     ;; - a comma (already handled above),
     ;; - a dot (already handled above),
     ;; - an equal sign (already handled above),
     ;; - "->" (already handled above),
     ;; - "is" (already handled above),
     ;; - "!is" (already handled above),
     ;; - "as" (already handled above),
     ;; - "as?" (already handled above),
     ;; - "if" (already handled above),
     ;; - "while" (already handled above),
     ;; - "for" (already handled above),
     ;; - "when" (already handled above),
     ;; - "catch" (already handled above),
     ;; or is a part of:
     ;; - secondary constructor declaration,
     ;; - class declaration,
     ;; - function declaration,
     ;; - variable declaration,
     ;; - getter declaration,
     ;; - setter declaration,
     ;; - constructor declaration call (example: constructor(): this(1)),
     ;;
     ;; Examples:
     ;;
     ;; public constructor() {}
     ;;
     ;; public class Foo() {}
     ;; public class Foo <A>() {}
     ;; public class Foo public constructor ()
     ;; public class Foo <A> @AAA constructor ()
     ;;
     ;; fun foo() {}
     ;; fun <A> foo() {}
     ;; fun (A).foo() {}
     ;; fun A.B<A, B>.C.foo() {}
     ;; fun @AAA (A).foo() {}
     ;; fun <A> (A).foo() {}
     ;; fun <A> @AAA (A).foo() {}
     ;; fun <A> @AAA A.B<A, B>.C.foo() {}
     ;;
     ;; fun () {}
     ;; fun (A).() {}
     ;; fun A.B<A, B>.C.() {}
     ;;
     ;; val (x, y) = foo()
     ;; var (x, y) = foo()
     ;; val <A> (x, y) = foo()
     ;; val (A).(x, y) = foo()
     ;; val <A> (A).(x, y) = foo()
     ;; val <A> @AAA A.(x, y) = foo()
     ;; val <A> @AAA A.B<A, B>.C.(x, y) = foo()
     ;;
     ;; get() {}
     ;; set(value) {}
     ;;
     ;; constructor (): this(1)
     ;; constructor (): super(1)
     ;;
     ;; Note that function argument cannot preceded by newline:
     ;;
     ;; val x = foo
     ;; (bar()) // This is not a continuation of the previous line.
     ((eq (kotlin-mode--token-type next-token) '\()
      (and
       (not (kotlin-mode--constructor-parameter-clause-p))
       (not (kotlin-mode--function-parameter-clause-p))
       (not (kotlin-mode--multi-variable-declaration-p))
       (not (kotlin-mode--constructor-delegation-call-p))))

     ;; Suppress implicit semicolon after the beginning of an interpolated
     ;; expression.
     ((eq (kotlin-mode--token-type previous-token)
          'string-chunk-before-interpolated-expression)
      nil)

     ;; Otherwise, inserts implicit semicolon.
     (t t))))

(defun kotlin-mode--close-parenthesis-before-control-structure-body-p (token)
  "Return t if TOKEN is close parenthesis before control structure body.

Return t if TOKEN is the close-parenthesis of \"if (...)\",
\"while(...)\", or \"for (...)\", but not followed by \"{\".

Return nil otherwise."
  (and
   (eq (kotlin-mode--token-type token) '\))
   (save-excursion
     (goto-char (kotlin-mode--token-end token))
     (not (eq (kotlin-mode--token-type (kotlin-mode--forward-token-simple))
              '\{)))
   (save-excursion
     (goto-char (kotlin-mode--token-end token))
     (condition-case nil
         (progn
           (backward-list)
           (let ((previous-token (kotlin-mode--backward-token-simple)))
             (or
              (member (kotlin-mode--token-text previous-token) '("if" "for"))
              (and
               (equal (kotlin-mode--token-text previous-token) "while")
               (not (save-excursion (kotlin-mode--find-do-for-while)))))))
       (scan-error
        nil)))))

(defun kotlin-mode--find-do-for-while ()
  "Return \"do\" token for \"while\" token.

If \"while\" is not part of do-while, return nil.

Assuming the point is before \"while\" token."
  ;; Example:
  ;;
  ;; do
  ;; while (false)
  ;; print(1)
  ;; while (false)
  ;; print(2)
  ;;
  ;; is parsed as three statements:
  ;;
  ;; do while (false);
  ;; print(1);
  ;; while (false) print(2);
  ;;
  ;; So we don't need to worry about bare while-statement in body of do-while.
  (let ((nesting-level 1)
        previous-token)
    (while (< 0 nesting-level)
      (setq previous-token (kotlin-mode--backward-token-or-list t))
      (cond
       ((equal (kotlin-mode--token-text previous-token) "while")
        (setq nesting-level (1+ nesting-level)))

       ((equal (kotlin-mode--token-text previous-token) "do")
        (setq nesting-level (1- nesting-level)))

       ((memq (kotlin-mode--token-type previous-token) '(outside-of-buffer {))
        ;; Unmatched
        (setq nesting-level 0))))
    (and (equal (kotlin-mode--token-text previous-token) "do")
         previous-token)))

(defun kotlin-mode--constructor-parameter-clause-p ()
  "Return non-nil if the point is before parameters of constructor declaration."
  ;; Examples:
  ;;
  ;; public constructor() {}
  ;;
  ;; public class Foo() {}
  ;; public class Foo <A>() {}
  ;; public class Foo public constructor ()
  ;; public class Foo <A> @AAA constructor ()
  (let ((previous-token (save-excursion (kotlin-mode--backward-token-simple))))
    (or
     (equal (kotlin-mode--token-text previous-token) "constructor")
     (save-excursion
       (kotlin-mode--try-backward-type-parameters)
       (and
        (eq (kotlin-mode--token-type (kotlin-mode--backward-token-simple))
            'atom)
        (equal (kotlin-mode--token-text (kotlin-mode--backward-token-simple))
               "class"))))))

(defun kotlin-mode--function-parameter-clause-p ()
  "Return non-nil if the point is before parameters of function declaration."
  ;; Examples:
  ;;
  ;; fun foo() {}
  ;; fun <A> foo() {}
  ;; fun (A).foo() {}
  ;; fun A.B<A, B>.C.foo() {}
  ;; fun @AAA (A).foo() {}
  ;; fun <A> (A).foo() {}
  ;; fun <A> @AAA (A).foo() {}
  ;; fun <A> @AAA A.B<A, B>.C.foo() {}
  ;;
  ;; fun () {}
  ;; fun (A).() {}
  ;; fun A.B<A, B>.C.() {}
  (let ((previous-token (save-excursion (kotlin-mode--backward-token-simple))))
    (cond
     ;; fun () {}
     ;; fun (A).() {} // for first parenthesis
     ;; fun (A).foo() {} // for first parenthesis
     ((equal (kotlin-mode--token-text previous-token) "fun")
      t)

     ;; fun (A).() {}
     ;; fun A.B<A, B>.C.() {}
     ((equal (kotlin-mode--token-text previous-token) ".")
      (save-excursion
        (and (kotlin-mode--try-backward-receiver-type)
             (equal (kotlin-mode--token-text
                     (kotlin-mode--backward-token-simple))
                    "fun"))))

     ;; fun @AAA (A).foo() {} // for first parenthesis
     ;; fun <A> (A).foo() {} // for first parenthesis
     ;; fun <A> @AAA (A).foo() {} // for first parenthesis
     ((save-excursion
        (kotlin-mode--try-backward-type-modifiers)
        (kotlin-mode--try-backward-type-parameters)
        (equal (kotlin-mode--token-text
                (kotlin-mode--backward-token-simple))
               "fun"))
      t)

     ;; fun foo() {}
     ;; fun <A> foo() {}
     ;; fun (A).foo() {} // for second parenthesis
     ;; fun A.B<A, B>.C.foo() {}
     ;; fun @AAA (A).foo() {} // for second parenthesis
     ;; fun <A> (A).foo() {} // for second parenthesis
     ;; fun <A> @AAA (A).foo() {} // for second parenthesis
     ;; fun <A> @AAA A.B<A, B>.C.foo() {}
     ((eq (kotlin-mode--token-type previous-token) 'atom)
      (save-excursion
        (goto-char (kotlin-mode--token-start previous-token))
        (kotlin-mode--try-backward-receiver-type)
        (kotlin-mode--try-backward-type-parameters)
        (equal (kotlin-mode--token-text
                (kotlin-mode--backward-token-simple))
               "fun"))))))

(defun kotlin-mode--try-backward-receiver-type ()
  "Try backward dot, type, and type modifiers.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  (let ((pos (point))
        (previous-token (kotlin-mode--backward-token-simple)))
    (if (and (equal (kotlin-mode--token-text previous-token) ".")
             (kotlin-mode--try-backward-type))
        (progn
          (kotlin-mode--try-backward-type-modifiers)
          t)
      (goto-char pos)
      nil)))

(defun kotlin-mode--try-backward-type ()
  "Try backward type but not type modifiers.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  ;; type
  ;;   : typeModifiers?
  ;;   ( parenthesizedType
  ;;   | nullableType
  ;;   | typeReference
  ;;   | functionType)
  ;;   ;
  ;;
  ;; typeReference
  ;;   : userType
  ;;   | DYNAMIC
  ;;   ;
  ;;
  ;; userType
  ;;     : simpleUserType (NL* DOT NL* simpleUserType)*
  ;;     ;
  ;;
  ;; simpleUserType
  ;;     : simpleIdentifier (NL* typeArguments)?
  ;;     ;
  ;;
  ;; functionType
  ;;   : (receiverType NL* DOT NL*)? functionTypeParameters NL* ARROW NL* type
  ;;   ;
  (let ((pos (point))
        (previous-token (kotlin-mode--backward-token-simple))
        result)
    (setq
     result
     (cond
      ;; parenthesizedType
      ((equal (kotlin-mode--token-text previous-token) ")")
       (goto-char (kotlin-mode--token-end previous-token))
       (condition-case nil
           (progn
             (backward-list)
             t)
         (scan-error
          (goto-char pos)
          nil)))

      ;; nullableType
      ((equal (kotlin-mode--token-text previous-token) "?")
       (kotlin-mode--try-backward-type))

      ;; typeReference
      (t
       (goto-char (kotlin-mode--token-end previous-token))
       (kotlin-mode--try-backward-type-reference))))
    ;; functionType
    (while (and result
                (equal (kotlin-mode--token-text
                        (save-excursion (kotlin-mode--backward-token-simple)))
                       "->"))
      ;; Skip "->"
      (kotlin-mode--backward-token-simple)
      ;; Skip parameters and a receiver type if any.
      (setq result (condition-case nil
                       (progn
                         (backward-list)
                         (kotlin-mode--try-backward-receiver-type)
                         t)
                     (scan-error
                      nil))))
    (unless result
      (goto-char pos))
    result))

(defun kotlin-mode--try-forward-type-reference ()
  "Try forward type reference.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  ;; typeReference
  ;;     : userType
  ;;     | DYNAMIC
  ;;     ;
  ;; userType
  ;;     : simpleUserType (NL* DOT NL* simpleUserType)*
  ;;     ;
  ;;
  ;; simpleUserType
  ;;     : simpleIdentifier (NL* typeArguments)?
  ;;     ;
  (let ((pos (point))
        result)
    (setq result (kotlin-mode--forward-simple-user-type))
    (while (and result
                (equal (kotlin-mode--token-text
                        (save-excursion (kotlin-mode--forward-token-simple)))
                       "."))
      ;; Skip "."
      (kotlin-mode--forward-token-simple)
      (setq result (kotlin-mode--forward-simple-user-type)))
    (unless result
      (goto-char pos))
    result))

(defun kotlin-mode--forward-simple-user-type ()
  "Forward simple user type.

Return non-nil if succeeds.  Otherwise, return nil and the point is undefined."
  ;; simpleUserType
  ;;     : simpleIdentifier (NL* typeArguments)?
  ;;     ;
  (let ((next-token (save-excursion (kotlin-mode--forward-token-simple))))
    (if (eq (kotlin-mode--token-type next-token) 'atom)
        ;; Found simpleIdentifier
        (progn
          (goto-char (kotlin-mode--token-end next-token))
          (when (equal (kotlin-mode--token-text
                        (save-excursion (kotlin-mode--forward-token-simple)))
                       "<")
            ;; Maybe type paramters
            (kotlin-mode--try-forward-type-parameters))
          t)
      ;; Not a simpleUserType
      nil)))

(defun kotlin-mode--try-backward-type-reference ()
  "Try backward type reference.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  ;; typeReference
  ;;     : userType
  ;;     | DYNAMIC
  ;;     ;
  ;; userType
  ;;     : simpleUserType (NL* DOT NL* simpleUserType)*
  ;;     ;
  ;;
  ;; simpleUserType
  ;;     : simpleIdentifier (NL* typeArguments)?
  ;;     ;
  (let ((pos (point))
        result)
    (setq result (kotlin-mode--backward-simple-user-type))
    (while (and result
                (equal (kotlin-mode--token-text
                        (save-excursion (kotlin-mode--backward-token-simple)))
                       "."))
      ;; Skip "."
      (kotlin-mode--backward-token-simple)
      (setq result (kotlin-mode--backward-simple-user-type)))
    (unless result
      (goto-char pos))
    result))

(defun kotlin-mode--backward-simple-user-type ()
  "Backward simple user type.

Return non-nil if succeeds.  Otherwise, return nil and the point is undefined."
  ;; simpleUserType
  ;;     : simpleIdentifier (NL* typeArguments)?
  ;;     ;
  (let ((pos (point))
        (previous-token (save-excursion (kotlin-mode--backward-token-simple))))
    (cond
     ;; simpleUserType with type arguments
     ((equal (kotlin-mode--token-text previous-token) ">")
      (if (and (kotlin-mode--try-backward-type-parameters)
               (eq (kotlin-mode--token-type
                    (kotlin-mode--backward-token-simple))
                   'atom))
          t
        (goto-char pos)
        nil))

     ;; Start of an annotation
     ((eq (kotlin-mode--token-type previous-token) 'annotation)
      (goto-char (kotlin-mode--token-start previous-token))
      (forward-char)
      t)

     ;; Other simpleUserType
     ((eq (kotlin-mode--token-type previous-token) 'atom)
      (goto-char (kotlin-mode--token-start previous-token))
      t)

     ;; Not a simpleUserType
     (t
      nil))))

(defun kotlin-mode--try-backward-type-modifiers ()
  "Try backward type modifiers.

Keep position if failed."
  (let ((previous-token (save-excursion (kotlin-mode--backward-token-simple))))
    (while (or
            (eq (kotlin-mode--token-type previous-token) 'annotation)
            (equal (kotlin-mode--token-text previous-token) "suspend"))
      (goto-char (kotlin-mode--token-start previous-token))
      (setq previous-token
            (save-excursion (kotlin-mode--backward-token-simple))))))

(defun kotlin-mode--multi-variable-declaration-p ()
  "Return non-nil if the point is before multi variable declaration."
  ;; Example:
  ;;
  ;; val (x, y) = foo()
  ;; var (x, y) = foo()
  ;; val <A> (x, y) = foo()
  ;; val (A).(x, y) = foo()
  ;; val <A> (A).(x, y) = foo()
  ;; val <A> @AAA A.(x, y) = foo()
  ;; val <A> @AAA A.B<A, B>.C.(x, y) = foo()
  (save-excursion
    (kotlin-mode--try-backward-receiver-type)
    (kotlin-mode--try-backward-type-parameters)
    (member (kotlin-mode--token-text
             (kotlin-mode--backward-token-simple))
            '("val" "var"))))

(defun kotlin-mode--constructor-delegation-call-p ()
  "Return non-nil if the point is before constructor delegation call arguments."
  ;; Example:
  ;;
  ;; constructor (): this(1)
  ;; constructor (): super(1)
  (save-excursion
    (and (member (kotlin-mode--token-text (kotlin-mode--backward-token-simple))
                 '("this" "super"))
         (eq (kotlin-mode--token-type (kotlin-mode--backward-token-simple))
             ':))))

(defun kotlin-mode--try-forward-modifiers ()
  "Try forward modifiers.

Keep position if failed."
  (let ((next-token (save-excursion (kotlin-mode--forward-token-simple))))
    (while (or
            (eq (kotlin-mode--token-type next-token) 'annotation)
            (member (kotlin-mode--token-text next-token)
                    kotlin-mode--modifier-keywords))
      (goto-char (kotlin-mode--token-end next-token))
      (setq next-token (save-excursion (kotlin-mode--forward-token-simple))))))

(defun kotlin-mode--try-backward-modifiers ()
  "Try forward modifiers.

Keep position if failed."
  (let ((previous-token (save-excursion (kotlin-mode--backward-token-simple))))
    (while (or
            (eq (kotlin-mode--token-type previous-token) 'annotation)
            (member (kotlin-mode--token-text previous-token)
                    kotlin-mode--modifier-keywords))
      (goto-char (kotlin-mode--token-start previous-token))
      (setq previous-token
            (save-excursion (kotlin-mode--backward-token-simple))))))

(defun kotlin-mode--backward-token-or-list (&optional use-backward-token-simple)
  "Move point to the start position of the previous token or list.

Return the token skipped.

If USE-BACKWARD-TOKEN-SIMPLE is non-nil, use
`kotlin-mode--backward-token-simple' instead of
`kotlin-mode--backward-token'."
  (let* ((previous-token (if use-backward-token-simple
                             (kotlin-mode--backward-token-simple)
                           (kotlin-mode--backward-token)))
         (previous-type (kotlin-mode--token-type previous-token))
         (previous-text (kotlin-mode--token-text previous-token))
         (previous-start (kotlin-mode--token-start previous-token))
         (previous-end (kotlin-mode--token-end previous-token)))
    (cond
     ;; Maybe list
     ((memq previous-type '(} \) \] \)-before-control-structure-body))
      (goto-char previous-end)
      (condition-case nil
          (progn
            (backward-list)
            (make-instance
             'kotlin-mode--token
             :type (assoc-default
                    previous-type
                    '((} . {})
                      (\) . \(\))
                      (\)-before-control-structure-body .
                       \(\)-before-control-structure-body)
                      (\] . \[\])))
             :text (buffer-substring-no-properties (point) previous-end)
             :start (point)
             :end previous-end))
        (scan-error
         (goto-char previous-start)
         previous-token)))

     ;; Maybe type parameter list
     ((equal previous-text ">")
      (goto-char previous-end)
      (if (kotlin-mode--try-backward-type-parameters)
          (make-instance
           'kotlin-mode--token
           :type '<>
           :text (buffer-substring-no-properties (point) previous-end)
           :start (point)
           :end previous-end)
        (goto-char previous-start)
        previous-token))

     ;; Other token
     (t previous-token))))

(defun kotlin-mode--forward-token-or-list (&optional use-forward-token-simple)
  "Move point to the end position of the next token or list.

Return the token skipped.

If USE-FORWARD-TOKEN-SIMPLE is non-nil, use
`kotlin-mode--forward-token-simple' instead of
`kotlin-mode--forward-token'."
  (let* ((next-token (if use-forward-token-simple
                         (kotlin-mode--forward-token-simple)
                       (kotlin-mode--forward-token)))
         (next-type (kotlin-mode--token-type next-token))
         (next-text (kotlin-mode--token-text next-token))
         (next-start (kotlin-mode--token-start next-token))
         (next-end (kotlin-mode--token-end next-token))
         result-type)
    (cond
     ;; Maybe list
     ((memq next-type '({ \( \[))
      (goto-char next-start)
      (condition-case nil
          (progn
            (forward-list)
            (setq
             result-type
             (if (kotlin-mode--close-parenthesis-before-control-structure-body-p
                  (save-excursion (kotlin-mode--backward-token-simple)))
                 '\(\)-before-control-structure-body
               (assoc-default next-type '(({ . {})
                                          (\( . \(\))
                                          (\[ . \[\])))))
            (make-instance
             'kotlin-mode--token
             :type result-type
             :text (buffer-substring-no-properties next-start (point))
             :start next-start
             :end (point)))
        (scan-error
         (goto-char next-end)
         next-token)))

     ;; Maybe type parameter list
     ((equal next-text "<")
      (goto-char next-start)
      (if (kotlin-mode--try-forward-type-parameters)
          (make-instance
           'kotlin-mode--token
           :type '<>
           :text (buffer-substring-no-properties next-start (point))
           :start next-start
           :end (point))
        (goto-char next-end)
        next-token))

     ;; Other token
     (t next-token))))

(defun kotlin-mode--try-backward-type-parameters ()
  "Move point to the start of the type parameter list.

Return non-nil if succeeds.  Keep position and return nil otherwise.

It is a type parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a type parameter list."
  (let ((pos (point)))
    (if (and
         (equal (kotlin-mode--token-text (kotlin-mode--backward-token-simple))
                ">")
         (kotlin-mode--try-skip-type-parameters
          (lambda () (kotlin-mode--backward-token-or-list t))
          "<" ">"))
        t
      (goto-char pos)
      nil)))

(defun kotlin-mode--try-forward-type-parameters ()
  "Move point to the end of the type parameter list.

Return non-nil if succeeds.  Keep position and return nil otherwise.

It is a type parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a type parameter list."
  (let ((pos (point)))
    (if (and
         (equal (kotlin-mode--token-text (kotlin-mode--forward-token-simple))
                "<")
         (kotlin-mode--try-skip-type-parameters
          (lambda () (kotlin-mode--forward-token-or-list t))
          ">" "<"))
        t
      (goto-char pos)
      nil)))

(defconst kotlin-mode--tokens-not-in-type-parameter-list
  ;; Whitelisting tend to be fragile. So we list tokens that are
  ;; unlikely to appear in type parameter lists in the current
  ;; version and future ones.
  ;;
  ;; Example of type parameters:
  ;; <
  ;;   A: B,
  ;;   @AAA(aaa) reified in C: @AAA suspend D<X, Y>.(x: Int) -> (X)?
  ;; >
  ;;
  ;; Example of type arguments
  ;; <
  ;;   A,
  ;;   @AAA(aaa) in *,
  ;;   suspend D<X, Y>.(x: Int) -> (X)?
  ;; >
  ;;
  ;; We don't need to consider the contents of inner brackets because
  ;; they are skipped by `kotlin-mode--backward-token-or-list'.
  ;;
  ;; String literals and numbers are also excluded by
  ;; `kotlin-mode--try-skip-type-parameters'.
  `(outside-of-buffer
    \;
    { } \( \) \[ \]
    "%" "/" "+" "-"
    "++" "--"
    "&&" "||"
    "!==" "!="
    ;; exclude "in"
    "is" "!is" "!in" "as" "as?"
    "!"
    "=" "+=" "-=" "*=" "/=" "%="
    ".." "..<" "::" "?:" "?." "<=" ">=" "==" "==="
    "package" "import" "class" "interface" "fun" "object" "val" "var"
    "typealias" "constructor" "by" "companion" "init" "if" "else" "when"
    "try" "catch" "finally" "for" "do" "while" "throw"
    return continue break "true" "false" "null"
    ,@(remove "suspend" kotlin-mode--modifier-keywords)))

(defun kotlin-mode--try-skip-type-parameters
    (skip-token-or-list-function matching-bracket-text unmatching-bracket-text)
  "Skip type parameters if the point is just before/after one.

Return non-nil if succeeds.  Keep position and return nil otherwise.

Assuming open/close bracket is already skipped.

SKIP-TOKEN-OR-LIST-FUNCTION is a function skipping forward/backward a
token or a list.
MATCHING-BRACKET-TEXT is a text of the matching bracket.
UNMATCHING-BRACKET-TEXT is a text of the current bracket."
  (let ((pos (point))
        result
        (prohibited-tokens (cons
                            unmatching-bracket-text
                            kotlin-mode--tokens-not-in-type-parameter-list))
        (next-token (funcall skip-token-or-list-function)))
    (while
        (cond
         ;; Prohibited tokens
         ((or
           (memq (kotlin-mode--token-type next-token) prohibited-tokens)
           (member (kotlin-mode--token-text next-token) prohibited-tokens)
           (string-match-p "^[\"'0-9]" (kotlin-mode--token-text next-token)))
          ;; Not a type parameter list.
          ;; Return to the initial position and stop the loop.
          (goto-char pos)
          (setq result nil)
          nil)

         ;; Matching bracket
         ((equal (kotlin-mode--token-text next-token) matching-bracket-text)
          ;; Found the matching open angle bracket.  Stop the loop.
          (setq result t)
          nil)

         ;; Otherwise, keep scanning
         (t t))
      (setq next-token (funcall skip-token-or-list-function)))
    result))

(defun kotlin-mode--forward-token ()
  "Move point forward to the next position of the end of a token.

Return the token object.  If no more tokens available, return a token with
type `outside-of-buffer'."
  (let ((pos (point)))
    ;; Skip comments and whitespaces.
    (let ((chunk (kotlin-mode--chunk-after)))
      ;; If point is inside a comment, go back to the beginning of the
      ;; comment before skipping it.
      (when (kotlin-mode--chunk-comment-p chunk)
        (goto-char (kotlin-mode--chunk-start chunk))))
    (forward-comment (point-max))
    (cond
     ;; Outside of buffer
     ((eobp)
      (make-instance 'kotlin-mode--token
                     :type 'outside-of-buffer
                     :text ""
                     :start (point)
                     :end (point)))

     ;; Implicit semicolon
     ((and
       ;; Check (forward-comment (point-max)) skipped a newline.
       (< pos
          (save-excursion
            (kotlin-mode--goto-non-comment-bol)
            (point)))
       (save-excursion (goto-char pos) (kotlin-mode--implicit-semi-p)))
      (make-instance 'kotlin-mode--token
                     :type 'implicit-\;
                     :text (buffer-substring-no-properties pos (point))
                     :start pos
                     :end (point)))

     (t
      (let ((token (kotlin-mode--extend-annotation-token-forward
                    (kotlin-mode--forward-token-simple))))
        (cond
         ;; Close parenthesis of "if ()", "while ()", or "for ()"
         ((kotlin-mode--close-parenthesis-before-control-structure-body-p token)
          (make-instance 'kotlin-mode--token
                         :type '\)-before-control-structure-body
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; "else" not followed by "if" on the same line or "{"
         ((kotlin-mode--bare-else-p token)
          (make-instance 'kotlin-mode--token
                         :type 'bare-else
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; super<A>@label
         ((save-excursion
            (and (equal (kotlin-mode--token-text token) "super")
                 (eq (char-after) ?<)
                 (kotlin-mode--try-forward-type-parameters)
                 (eq (char-after) ?@)))
          (kotlin-mode--try-forward-type-parameters)
          (forward-char)
          (skip-syntax-forward "w")
          (make-instance 'kotlin-mode--token
                         :type 'super
                         :text (buffer-substring-no-properties
                                (kotlin-mode--token-start token)
                                (point))
                         :start (kotlin-mode--token-start token)
                         :end (point)))

         ;; "->" of lambda parameters
         ((kotlin-mode--anonymous-function-parameter-arrow-p token)
          (make-instance 'kotlin-mode--token
                         :type 'anonymous-function-parameter-arrow
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; "->" of when expression
         ((kotlin-mode--when-expression-arrow-p token)
          (make-instance 'kotlin-mode--token
                         :type 'when-expression-arrow
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         (t token)))))))

(defun kotlin-mode--bare-else-p (token)
  "Return non-nil if TOKEN is bare \"else\" token.

A bare \"else\" token is a \"else\" token not followed by \"if\" on
the same line, \"->\",  or \"{\"."
  (let ((next-token (save-excursion
                      (goto-char (kotlin-mode--token-end token))
                      (kotlin-mode--forward-token-simple))))
    (and (equal (kotlin-mode--token-text token) "else")
         (not (eq (kotlin-mode--token-type next-token) '{))
         (not (eq (kotlin-mode--token-text next-token) "->"))
         (not (and
               (equal (kotlin-mode--token-text next-token) "if")
               (save-excursion
                 (= (progn
                      (goto-char (kotlin-mode--token-start next-token))
                      (kotlin-mode--goto-non-comment-bol)
                      (point))
                    (progn
                      (goto-char (kotlin-mode--token-start token))
                      (kotlin-mode--goto-non-comment-bol)
                      (point)))))))))

(defun kotlin-mode--anonymous-function-parameter-arrow-p (token)
  "Return non-nil if TOKEN is an arrow of lambda parameters."
  ;; Examples:
  ;;
  ;; { -> 1}
  ;; { x -> 1 }
  ;; { f: (Int) -> Int, x: Int -> f(x) }
  ;; { f: (Int) -> Int -> f(1) }
  ;; { f: (Int) -> (Int) -> Int -> f(1) }
  ;; { (x: Int, y: Int) -> x + y }
  ;;
  ;; If an arrow is not preceded by a close parenthesis, it is an end
  ;; of parameters.
  ;;
  ;; If an arrow is preceded by a close parenthesis and the
  ;; parentheses are preceded by an open curly bracket, the arrow is
  ;; an end of parameters.
  ;;
  ;; Otherwise, it is an arrow in function types.
  ;;
  ;; Furthermore, the curly open bracket should not be preceded by
  ;; "when ()".
  (and (equal (kotlin-mode--token-text token) "->")
       (save-excursion
         (goto-char (kotlin-mode--token-start token))
         (forward-comment (- (point)))
         (or
          ;; The arrow is not preceded by a close parenthesis.
          ;;
          ;; Examples:
          ;; { -> 1}
          ;; { x -> 1 }
          ;; { f: (Int) -> Int, x: Int -> f(x) } // last arrow
          ;; { f: (Int) -> Int -> f(1) } // last arrow
          ;; { f: (Int) -> (Int) -> Int -> f(1) } // last arrow
          (not (eq (char-before) ?\)))
          ;; The parentheses are preceded by an open curly bracket.
          ;;
          ;; Example:
          ;; { (x: Int, y: Int) -> x + y }
          (condition-case nil
              (progn (backward-list)
                     (forward-comment (- (point)))
                     (eq (char-before) ?{))
            (scan-error nil))))
       (save-excursion
         ;; The token is inside a curly brackets but not inside a
         ;; when-expression.
         (goto-char (kotlin-mode--token-start token))
         (let ((containing-bracket
                (kotlin-mode--find-containing-brackets (point))))
           (and (eq (car containing-bracket) '{)
                (not (kotlin-mode--inside-when-expression-p
                      containing-bracket)))))))

(defun kotlin-mode--when-expression-arrow-p (token)
  "Return non-nil if TOKEN is an arrow of when expression."
  ;; This function does not distinguish arrows of when expression and
  ;; ones of function types.
  (and (equal (kotlin-mode--token-text token) "->")
       (save-excursion
         (goto-char (kotlin-mode--token-start token))
         (kotlin-mode--inside-when-expression-p))))

(defun kotlin-mode--inside-when-expression-p (&optional containing-bracket)
  "Return non-nil if point is inside a when expression.

CONTAINING-BRACKET is a return value of
`kotlin-mode--find-containing-brackets'.  If ommitted,
\(kotlin-mode--find-containing-brackets (point)) is used."
  (unless containing-bracket
    (setq containing-bracket (kotlin-mode--find-containing-brackets (point))))
  (goto-char (cdr containing-bracket))
  (and (eq (car containing-bracket) '{)
       (eq (kotlin-mode--token-type (kotlin-mode--backward-token-or-list t))
           '\(\))
       (equal (kotlin-mode--token-text (kotlin-mode--backward-token-simple))
              "when")))

(defun kotlin-mode--forward-token-simple ()
  "Like `kotlin-mode--forward-token' without recursion.

This function does not return `implicit-;'."
  (forward-comment (point-max))
  (cond
   ;; Outside of buffer
   ((eobp)
    (make-instance 'kotlin-mode--token
                   :type 'outside-of-buffer
                   :text ""
                   :start (point)
                   :end (point)))

   ;; End of template expression
   ((and (eq (char-after) ?\})
         (equal (get-text-property (point) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-after-comment (point)))
      (kotlin-mode--forward-string-chunk)
      (make-instance
       'kotlin-mode--token
       :type 'string-chunk-after-template-expression
       :text (buffer-substring-no-properties pos-after-comment (point))
       :start pos-after-comment
       :end (point))))

   ;; ::
   ((looking-at "::")
    (forward-char 2)
    (make-instance 'kotlin-mode--token
                   :type 'operator
                   :text "::"
                   :start (- (point) 2)
                   :end (point)))

   ;; Separators and parentheses
   ((memq (char-after) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\) ?:))
    (forward-char)
    (make-instance 'kotlin-mode--token
                   :type (intern (string (char-before)))
                   :text (string (char-before))
                   :start (1- (point))
                   :end (point)))

   ;; Open angle bracket for type parameters or type arguments
   ;;
   ;; We use a heuristic: spaces are inserted around inequality sign, but not
   ;; for angle bracket, and a type parameter starts with upper case
   ;; characters, parentheses, asterisk, keyword 'reified', keyword 'in',
   ;; keyword 'out', keyword 'suspend', or annotations.
   ((and (eq (char-after) ?<)
         (looking-at
          (rx (seq "<" (or (any "_(@[*" upper)
                           (seq (or "reified" "in" "out" "suspend")
                                word-end))))))
    (forward-char)
    (make-instance 'kotlin-mode--token
                   :type '<
                   :text "<"
                   :start (1- (point))
                   :end (point)))

   ;; Close angle bracket for type parameters or type arguments
   ;;
   ;; Close angle bracket follows identifiers, parentheses, question symbols,
   ;; or other angle brackets (e.g. Foo<Bar<(String)?>>)
   ((and (eq (char-after) ?>)
         (save-excursion
           ;; You know that regular languages can be reversed.  Thus you may
           ;; think that `looking-back' reverses the given regexp and scans
           ;; chars backwards.  Nevertheless, `looking-back' function does not
           ;; do that.  It just repeats `looking-at' with decrementing start
           ;; position until it succeeds.  The document says that it is not
           ;; recommended to use.  So we use combination of
           ;; `skip-chars-backward', `skip-syntax-backward', and
           ;; `looking-at' here.
           (skip-chars-backward "?)>")
           (skip-syntax-backward "w")
           (looking-at "[[:upper:]_]")))
    (forward-char)
    (make-instance 'kotlin-mode--token
                   :type '>
                   :text ">"
                   :start (1- (point))
                   :end (point)))

   ;; Operator (other than as, in, or is)
   ((looking-at
     (rx
      (or
       "++" "--"
       "&&" "||"
       "!==" "!="
       (seq "!is" word-end) (seq "!in" word-end) "as?"
       "!"
       "->"
       "..<" ".." "." "?:" "?." "?" "<" ">" "<=" ">=" "===" "=="
       "=" "+=" "-=" "*=" "/=" "%="
       "*" "%" "/" "+" "-" "&")))
    (let ((text (match-string-no-properties 0))
          (start (match-beginning 0))
          (end (match-end 0)))
      (goto-char end)
      (make-instance 'kotlin-mode--token
                     :type 'operator
                     :text text
                     :start start
                     :end end)))

   ;; Backquoted identifier or character literal
   ((memq (char-after) '(?` ?'))
    (let ((pos-after-comment (point)))
      (kotlin-mode--forward-string-chunk)
      (make-instance
       'kotlin-mode--token
       :type 'atom
       :text (buffer-substring-no-properties pos-after-comment (point))
       :start pos-after-comment
       :end (point))))

   ;; String
   ((looking-at "\"")
    (let ((pos-after-comment (point)))
      (forward-char)
      (kotlin-mode--end-of-string)
      (make-instance
       'kotlin-mode--token
       :type 'atom
       :text (buffer-substring-no-properties pos-after-comment (point))
       :start pos-after-comment
       :end (point))))

   ;; Part of annotations.  Only @ and first identifier.
   ;;
   ;; This will be augmented by
   ;; `kotlin-mode--extend-annotation-token-forward' later.
   ((eq (char-after) ?@)
    (let ((pos-after-comment (point)))
      (forward-char)
      (if (eq (char-after) ?`)
          (kotlin-mode--forward-string-chunk)
        (skip-syntax-forward "w"))
      ;; Strictly speaking, single at sign is not an annotation, but
      ;; we treat it as an annotation.
      (make-instance
       'kotlin-mode--token
       :type 'annotation
       :text (buffer-substring-no-properties pos-after-comment (point))
       :start pos-after-comment
       :end (point))))

   ;; Other tokens including identifiers, keywords, labels, and numbers.
   ;;
   ;; Note that we parse 123.456e+2 as "123" "." "456e" "+" "2".
   (t
    (let* ((pos-after-comment (point))
           (text
            (cond
             ;; return@label, continue@label, break@label, this@label,
             ;; or super@label.
             ((looking-at (rx (or "return@"
                                  "continue@"
                                  "break@"
                                  "this@"
                                  "super@")))
              (skip-syntax-forward "w")
              (forward-char)
              (skip-syntax-forward "w")
              (buffer-substring-no-properties pos-after-comment (point)))

             ;; Identifiers, keywords, labels, or numbers
             ((eq (syntax-class (syntax-after (point)))
                  (syntax-class (string-to-syntax "w")))
              (skip-syntax-forward "w")
              ;; Skip an at sign if exists for label.
              (when (eq (char-after) ?@)
                (forward-char))
              (buffer-substring-no-properties pos-after-comment (point)))
             ;; Unknown character type. Treats as a single-letter token.
             (t
              (forward-char)
              (string (char-before)))))
           (type (cond
                  ((member text '("is" "in" "as"))
                   ;; Note that "as?" is already handled.
                   'operator)

                  ((eq (aref text (1- (length text))) ?@)
                   'label)

                  ((string-match "^return\\>" text)
                   'return)

                  ((string-match "^continue\\>" text)
                   'return)

                  ((string-match "^break\\>" text)
                   'break)

                  ((string-match "^this\\>" text)
                   'this)

                  ((string-match "^super\\>" text)
                   'super)

                  (t
                   'atom))))
      (make-instance 'kotlin-mode--token
                     :type type
                     :text text
                     :start (- (point) (length text))
                     :end (point))))))

(defun kotlin-mode--extend-annotation-token-forward (token)
  "Return annotation token if TOKEN is the start of an annotation.

TOKEN is assumed to be a return value of `kotlin-mode--forward-token-simple'."
  (if (eq (kotlin-mode--token-type token) 'annotation)
      (let ((pos (point)))
        (cond
         ;; Example: @file:Abc(def)
         ;; Example: @file:[Abc Def Ghi]
         ((save-excursion
            (forward-comment (point-max))
            (eq (char-after) ?:))
          (forward-comment (point-max))
          (forward-char)
          (unless (or (kotlin-mode--try-forward-multi-annotation-bracket)
                      (kotlin-mode--try-forward-unescaped-annotation))
            (goto-char pos)))

         ;; Example: @[Abc Def Ghi]
         ((equal (kotlin-mode--token-text token) "@")
          (kotlin-mode--try-forward-multi-annotation-bracket))

         ;; Example: @Abc(def)
         (t
          (goto-char (1+ (kotlin-mode--token-start token)))
          (kotlin-mode--try-forward-unescaped-annotation)))
        (make-instance 'kotlin-mode--token
                       :type 'annotation
                       :text (buffer-substring-no-properties
                              (kotlin-mode--token-start token)
                              (point))
                       :start (kotlin-mode--token-start token)
                       :end (point)))
    token))

(defun kotlin-mode--try-forward-multi-annotation-bracket ()
  "Move point forward to the end of square brackets.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  (let ((pos (point)))
    (when (save-excursion
            (forward-comment (point-max))
            (eq (char-after) ?\[))
      (condition-case nil
          (progn
            (forward-list)
            t)
        (scan-error
         (goto-char pos)
         nil)))))

(defun kotlin-mode--try-forward-unescaped-annotation ()
  "Move point forward to the end of unescapedAnnotation.

Skip userType and constructor arguments if exists.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  (if (kotlin-mode--try-forward-type-reference)
      (let ((pos (point)))
        (when (save-excursion
                ;; Line breaks are prohibited before bracket.
                (kotlin-mode--forward-comments-but-not-line-breaks)
                (eq (char-after) ?\())
          (condition-case nil
              (forward-list)
            (scan-error
             (goto-char pos))))
        t)
    nil))

(defun kotlin-mode--backward-token ()
  "Move point backward to the previous position of the end of a token.

Return the token object.  If no more tokens available, return a token with
type `outside-of-buffer'."
  (let ((pos (point)))
    ;; Skip comments and whitespaces.
    (let ((chunk (kotlin-mode--chunk-after)))
      (when (kotlin-mode--chunk-comment-p chunk)
        (goto-char (kotlin-mode--chunk-start chunk))))
    (forward-comment (- (point)))
    (cond
     ;; Outside of buffer
     ((bobp)
      (make-instance 'kotlin-mode--token
                     :type 'outside-of-buffer
                     :text ""
                     :start (point)
                     :end (point)))

     ;; Implicit semicolon
     ((and
       ;; Check (forward-comment (- (point))) skipped a newline.
       (< (save-excursion
            (kotlin-mode--goto-non-comment-eol)
            (point))
          pos)
       (save-excursion (goto-char pos) (kotlin-mode--implicit-semi-p)))
      (make-instance 'kotlin-mode--token
                     :type 'implicit-\;
                     :text (buffer-substring-no-properties (point) pos)
                     :start (point)
                     :end pos))

     (t
      (let ((token (kotlin-mode--extend-annotation-token-backward
                    (kotlin-mode--backward-token-simple))))
        (cond
         ;; Close parenthesis of "if ()", "while ()", or "for ()"
         ((kotlin-mode--close-parenthesis-before-control-structure-body-p token)
          (make-instance 'kotlin-mode--token
                         :type '\)-before-control-structure-body
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; "else" not followed by "if" on the same line or "{"
         ((kotlin-mode--bare-else-p token)
          (make-instance 'kotlin-mode--token
                         :type 'bare-else
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; super<A>@label
         ((save-excursion
            (and (eq (kotlin-mode--token-type token) 'annotation)
                 (eq (char-before) ?>)
                 (kotlin-mode--try-backward-type-parameters)
                 (equal (kotlin-mode--token-text
                         (kotlin-mode--backward-token-simple))
                        "super")))
          (kotlin-mode--try-backward-type-parameters)
          (kotlin-mode--backward-token-simple)
          (make-instance 'kotlin-mode--token
                         :type 'super
                         :text (buffer-substring-no-properties
                                (point)
                                (kotlin-mode--token-end token))
                         :start (point)
                         :end (kotlin-mode--token-end token)))

         ;; "->" of lambda parameters
         ((kotlin-mode--anonymous-function-parameter-arrow-p token)
          (make-instance 'kotlin-mode--token
                         :type 'anonymous-function-parameter-arrow
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         ;; "->" of when expression
         ((kotlin-mode--when-expression-arrow-p token)
          (make-instance 'kotlin-mode--token
                         :type 'when-expression-arrow
                         :text (kotlin-mode--token-text token)
                         :start (kotlin-mode--token-start token)
                         :end (kotlin-mode--token-end token)))

         (t token)))))))

(defun kotlin-mode--backward-token-simple ()
  "Like `kotlin-mode--backward-token' without recursion.

This function does not return `implicit-;'"
  (forward-comment (- (point)))
  (cond
   ;; Outside of buffer
   ((bobp)
    (make-instance 'kotlin-mode--token
                   :type 'outside-of-buffer
                   :text ""
                   :start (point)
                   :end (point)))

   ;; Beginning of template expression
   ((and (eq (char-before) ?\{)
         (equal (get-text-property (1- (point)) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-before-comment (point)))
      (kotlin-mode--backward-string-chunk)
      (make-instance
       'kotlin-mode--token
       :type 'string-chunk-before-template-expression
       :text (buffer-substring-no-properties (point) pos-before-comment)
       :start (point)
       :end pos-before-comment)))

   ;; ::, ?:
   ((member (buffer-substring-no-properties
             (max (point-min) (- (point) 2))
             (point))
            '("::" "?:"))
    (backward-char 2)
    (make-instance 'kotlin-mode--token
                   :type 'operator
                   :text (buffer-substring-no-properties (point) (+ 2 (point)))
                   :start (point)
                   :end (+ 2 (point))))

   ;; Separators and parentheses
   ((memq (char-before) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\) ?:))
    (backward-char)
    (make-instance 'kotlin-mode--token
                   :type (intern (string (char-after)))
                   :text (string (char-after))
                   :start (point)
                   :end (1+ (point))))

   ;; Operator (3 letters)
   ((member (buffer-substring-no-properties
             (max (point-min) (- (point) 3))
             (point))
            '("===" "!==" "!is" "!in" "..<"))
    (backward-char 3)
    (make-instance 'kotlin-mode--token
                   :type 'operator
                   :text (buffer-substring-no-properties (point) (+ 3 (point)))
                   :start (point)
                   :end (+ 3 (point))))

   ;; Operator (2 letters, other than as, in, or is)
   ((member (buffer-substring-no-properties
             (max (point-min) (- (point) 2))
             (point))
            '("++" "--"
              "&&" "||"
              "+=" "-=" "*=" "/=" "%="
              ".." "?."
              "<=" ">=" "!=" "=="
              "->"))
    (backward-char 2)
    (make-instance 'kotlin-mode--token
                   :type 'operator
                   :text (buffer-substring-no-properties (point) (+ 2 (point)))
                   :start (point)
                   :end (+ 2 (point))))

   ;; Open angle bracket for type parameters
   ;;
   ;; We use a heuristic: spaces are inserted around inequality sign,
   ;; but not for angle bracket, and a type parameter starts with an
   ;; upper case characters, parenthesis, or keyword 'reified',
   ;; keyword 'in', keyword 'out', keyword 'suspend', or annotations.
   ((and (eq (char-before) ?<)
         (looking-at
          (rx (or (any "_(@[*" upper)
                  (seq (or "reified" "in" "out")
                       word-end)))))
    (backward-char)
    (make-instance 'kotlin-mode--token
                   :type '<
                   :text "<"
                   :start (point)
                   :end (1+ (point))))

   ;; Close angle bracket for type parameters
   ;;
   ;; Close angle bracket follows identifier, parentheses, question symbols,
   ;; or other angle brackets (e.g. Foo<Bar<(String)?>>)
   ((and (eq (char-before) ?>)
         (save-excursion
           (skip-chars-backward "?)>")
           (skip-syntax-backward "w")
           (looking-at "[[:upper:]_]")))
    (backward-char)
    (make-instance 'kotlin-mode--token
                   :type '>
                   :text ">"
                   :start (point)
                   :end (1+ (point))))

   ;; ? or as?
   ((eq (char-before) ??)
    (let ((pos-before-comment (point)))
      (backward-char)
      (skip-syntax-backward "w")
      (unless (looking-at (rx "as?"))
        (goto-char pos-before-comment)
        (backward-char))
      (make-instance
       'kotlin-mode--token
       :type 'operator
       :text (buffer-substring-no-properties (point) pos-before-comment)
       :start (point)
       :end pos-before-comment)))

   ;; Operator (1 letter)
   ((member (buffer-substring-no-properties
             (max (point-min) (- (point) 1))
             (point))
            '("*" "%" "/" "+" "-" "!" "=" "." "?" "<" ">" "&"))
    (backward-char)
    (make-instance 'kotlin-mode--token
                   :type 'operator
                   :text (buffer-substring-no-properties (point) (1+ (point)))
                   :start (point)
                   :end (1+ (point))))

   ;; Backquoted identifier
   ((eq (char-before) ?`)
    (let ((pos-before-comment (point)))
      (kotlin-mode--backward-string-chunk)
      (when (eq (char-before) ?@)
        (backward-char))
      (make-instance
       'kotlin-mode--token
       :type (if (eq (char-after) ?@) 'annotation 'atom)
       :text (buffer-substring-no-properties (point) pos-before-comment)
       :start (point)
       :end pos-before-comment)))

   ;; character literal
   ((eq (char-before) ?')
    (let ((pos-before-comment (point)))
      (kotlin-mode--backward-string-chunk)
      (make-instance
       'kotlin-mode--token
       :type 'atom
       :text (buffer-substring-no-properties (point) pos-before-comment)
       :start (point)
       :end pos-before-comment)))

   ;; String
   ((eq (char-before) ?\")
    (let ((pos-before-comment (point)))
      (backward-char)
      (kotlin-mode--beginning-of-string)
      (make-instance
       'kotlin-mode--token
       :type 'atom
       :text (buffer-substring-no-properties (point) pos-before-comment)
       :start (point)
       :end pos-before-comment)))

   ;; Other tokens including identifiers, keywords, labels, numbers,
   ;; and annotations without bracket.
   ;;
   ;; Note that we parse 123.456e+2 as "123" "." "456e" "+" "2".
   (t
    (let* ((pos-before-comment (point))
           (text
            (cond
             ;; Labels (and single at sign)
             ((eq (char-before) ?@)
              (backward-char)
              (skip-syntax-backward "w")
              (buffer-substring-no-properties (point) pos-before-comment))

             ;; Identifiers, keywords, numbers, (part of) annotations
             ((eq (syntax-class (syntax-after (1- (point))))
                  (syntax-class (string-to-syntax "w")))
              (skip-syntax-backward "w")
              (when (eq (char-before) ?@)
                ;; Annotation, return@ or something like it.
                (backward-char)
                (let ((pos (point)))
                  (skip-syntax-backward "w")
                  (unless (looking-at
                           (rx (or "return"
                                   "continue"
                                   "break"
                                   "this"
                                   "super")))
                    (goto-char pos))))
              (buffer-substring-no-properties (point) pos-before-comment))

             ;; Unknown character type. Treats as a single-letter token.
             (t (backward-char) (string (char-after)))))
           (type (cond
                  ((member text '("as" "in" "is"))
                   'operator)

                  ((string-prefix-p "@" text)
                   ;; Strictly speaking, single at sign is not an
                   ;; annotation, but we treat it as an annotation.
                   'annotation)

                  ((string-match "^return\\>" text)
                   'return)

                  ((string-match "^continue\\>" text)
                   'return)

                  ((string-match "^break\\>" text)
                   'break)

                  ((string-match "^this\\>" text)
                   'this)

                  ((string-match "^super\\>" text)
                   'super)

                  ((eq (aref text (1- (length text))) ?@)
                   'label)

                  (t
                   'atom))))
      (make-instance 'kotlin-mode--token
                     :type type
                     :text text
                     :start (point)
                     :end (+ (point) (length text)))))))

(defun kotlin-mode--extend-annotation-token-backward (token)
  "Return annotation token if TOKEN is the end of an annotation.

TOKEN is assumed to be a return value of `kotlin-mode--backward-token-simple'."
  (let ((pos (point)))
    (goto-char (kotlin-mode--token-end token))
    (cond
     ;; Example: @[Abc Def Ghi]
     ;; Example: @file:[Abc Def Ghi]
     ((eq (char-before) ?\])
      (condition-case nil
          (progn
            (backward-list)
            (forward-comment (- (point)))
            (if (or (and (eq (char-before) ?@) (prog1 t (backward-char)))
                    (kotlin-mode--try-backward-annotation-use-site-target))
                (make-instance 'kotlin-mode--token
                               :type 'annotation
                               :text (buffer-substring-no-properties
                                      (point)
                                      (kotlin-mode--token-end token))
                               :start (point)
                               :end (kotlin-mode--token-end token))
              ;; Not an annotation.
              (goto-char pos)
              token))
        (scan-error
         ;; Not an annotation.
         (goto-char pos)
         token)))

     ;; Example: @Abc<A>(def)
     ;; Example: @file:Abc<A>(def)
     ;; Example: @Abc<A>
     ;; Example: @file:Abc<A>
     ;; Example: @Abc
     ((or (memq (char-before) '(?\) ?` ?>))
          (eq (syntax-class (syntax-after (1- (point))))
              (syntax-class (string-to-syntax "w"))))
      (when (eq (char-before) ?\))
        (condition-case nil
            (progn
              (backward-list)
              ;; Line breaks are prohibited before bracket.
              (kotlin-mode--backward-comments-but-not-line-breaks)
              (unless (or (memq (char-before) '(?` ?>))
                          (eq (syntax-class (syntax-after (1- (point))))
                              (syntax-class (string-to-syntax "w"))))
                (goto-char pos)))
          (scan-error
           (goto-char pos))))
      (if (and
           (kotlin-mode--try-backward-type-reference)
           (or (and (eq (char-before) ?@) (prog1 t (backward-char)))
               (kotlin-mode--try-backward-annotation-use-site-target)))
          (make-instance 'kotlin-mode--token
                         :type 'annotation
                         :text (buffer-substring-no-properties
                                (point)
                                (kotlin-mode--token-end token))
                         :start (point)
                         :end (kotlin-mode--token-end token))
        ;; Not an annotation.
        (goto-char pos)
        token))

     ;; Not an annotation.
     (t
      (goto-char pos)
      token))))

(defun kotlin-mode--try-backward-annotation-use-site-target ()
  "Try backward annotationUseSiteTarget.

Try to skip colon, word, and at sign.

Return non-nil if succeeds.  Keep position and return nil otherwise."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (if (eq (char-before) ?:)
        (progn
          (backward-char)
          (forward-comment (- (point)))
          (if (and (< (skip-syntax-backward "w") 0)
                   (eq (char-before) ?@))
              (progn
                (backward-char)
                t)
            (goto-char pos)
            nil))
      (goto-char pos)
      nil)))

(defun kotlin-mode--forward-string-chunk ()
  "Skip forward a string chunk.

A string chunk is a part of single-line/multiline string delimited with
quotation marks or template expressions."
  (condition-case nil
      (goto-char (scan-sexps (point) 1))
    (scan-error (goto-char (point-max)))))

(defun kotlin-mode--backward-string-chunk ()
  "Skip backward a string chunk.

A string chunk is a part of single-line/multiline string delimited with
quotation marks or template expressions."
  (condition-case nil
      (goto-char (scan-sexps (point) -1))
    (scan-error (goto-char (point-min)))))

(defun kotlin-mode--beginning-of-string ()
  "Move point to the beginning of single-line/multiline string.

Return the point of the beginning.

Assuming the point is on a string."
  (goto-char (or (nth 8 (syntax-ppss)) (point)))
  (let (matching-bracket)
    (while (and
            (setq matching-bracket
                  (get-text-property
                   (point)
                   'kotlin-property--matching-bracket))
            (< (point-min) matching-bracket))
      (goto-char matching-bracket)
      (goto-char (nth 8 (syntax-ppss))))
    (point)))

(defun kotlin-mode--end-of-string ()
  "Move point to the end of single-line/multiline string.

Assuming the point is on a string."
  (goto-char (or (nth 8 (syntax-ppss)) (point)))
  (let (matching-bracket)
    (kotlin-mode--forward-string-chunk)
    (while (and (setq matching-bracket
                      (get-text-property
                       (1- (point))
                       'kotlin-property--matching-bracket))
                (< matching-bracket (point-max)))
      (goto-char matching-bracket)
      (kotlin-mode--forward-string-chunk)))
  (point))

(defun kotlin-mode--goto-non-comment-bol ()
  "Back to the beginning of line that is not inside a comment."
  (beginning-of-line)
  (let (chunk)
    (while (progn
             (setq chunk (kotlin-mode--chunk-after))
             (kotlin-mode--chunk-comment-p chunk))
      ;; The point is in a comment. Backs to the beginning of the comment.
      (goto-char (kotlin-mode--chunk-start chunk))
      (beginning-of-line))))

(defun kotlin-mode--goto-non-comment-eol ()
  "Proceed to the end of line that is not inside a comment.

If this line ends with a single-line comment, goto just before the comment."
  (end-of-line)
  (let (chunk)
    (while (progn
             (setq chunk (kotlin-mode--chunk-after))
             (kotlin-mode--chunk-comment-p chunk))
      ;; The point is in a comment.
      (if (kotlin-mode--chunk-single-line-comment-p chunk)
          ;; This is a single-line comment
          ;; Back to the beginning of the comment.
          (goto-char (kotlin-mode--chunk-start chunk))
        ;; This is a multiline comment
        ;; Proceed to the end of the comment.
        (goto-char (kotlin-mode--chunk-start chunk))
        (forward-comment 1)
        (end-of-line)
        ;; If the comment is incomplete, back to the beginning of the comment.
        (when (and (eobp) (kotlin-mode--chunk-after))
          (goto-char (kotlin-mode--chunk-start (kotlin-mode--chunk-after))))))))

(defun kotlin-mode--bol-other-than-comments-p ()
  "Return t if there is nothing other than comments in the front of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  ;; Foo // ← bol
  ;; /* */ Foo // ← bol
  ;; X /* */ Foo // ← not bol
  ;;
  ;; /*
  ;; */ /* */ /*
  ;; */ Foo // ← bol
  ;;
  ;; X /*
  ;; */ /* */ /*
  ;; */ Foo // ← not bol
  ;;
  ;; X
  ;; /* */ /*
  ;; */ Foo // ← bol
  (save-excursion
    (let ((pos (point)))
      (kotlin-mode--goto-non-comment-bol)
      (forward-comment (point-max))
      (<= pos (point)))))

(defun kotlin-mode--eol-other-than-comments-p ()
  "Return t if there is nothing other than comments until the end of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  (save-excursion
    (let ((pos (point)))
      (kotlin-mode--goto-non-comment-eol)
      (forward-comment (- (point)))
      (<= (point) pos))))

(defun kotlin-mode--skip-whitespaces ()
  "Skip forward whitespaces and newlines."
  (skip-syntax-forward " >"))

(defun kotlin-mode--forward-comments-but-not-line-breaks ()
  "Skip forward comments but not line breaks."
  (goto-char (min
              (save-excursion
                (kotlin-mode--goto-non-comment-eol)
                (point))
              (save-excursion
                (forward-comment (point-max))
                (point)))))

(defun kotlin-mode--backward-comments-but-not-line-breaks ()
  "Skip backward comments but not line breaks."
  (goto-char (max
              (save-excursion
                (kotlin-mode--goto-non-comment-bol)
                (point))
              (save-excursion
                (forward-comment (- (point)))
                (point)))))

(provide 'kotlin-mode-lexer)

;;; kotlin-mode-lexer.el ends here
