;;; kotlin-mode-kdoc-comment-parser.el --- Major-mode for Kotlin, KDoc comment parser for filling. -*- lexical-binding: t -*-

;; Copyright (C) 2023 taku0

;; Author: taku0 (http://github.com/taku0)

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

;; Routines for parsing KDoc comment.  Only block level parser is implemented.

;;; Code:

(require 'rx)
(require 'cl-lib)

(cl-defun kotlin-mode--comment-node
    (&key
     type
     start
     end
     content-offset
     closed
     next
     previous
     parent
     first-child
     last-child
     (spans (list))
     code-fence
     list-marker-type)
  "Construct and return a comment node as a hash table.

TYPE: the type of the node.
  Valid types:

  - document
  - thematic-break
  - atx-heading
  - setext-heading
  - indented-code-block
  - fenced-code-block
  - paragraph
  - block-quote
  - list-item
  - list
  - single-line-comment (slashes at start of each lines comments)
  - multiline-comment (asterisk at start of each lines of comments)
  - comment-tag (at-tag of KDoc)

  HTML block nor link reference definitions are not implemented.

START: the start position.

END: the end position.

CONTENT-OFFSET: the start column of the content relative to parent.

CLOSED: the node is closed or not.

NEXT: the following sibling if any.

PREVIOUS: the preceding sibling if any.

PARENT: the parent if any.

FIRST-CHILD: the first child if any.

LAST-CHILD: the last child if any.

SPANS: the text spans.  A list of tuple (START START-COLUMN END) where START is
  the start position, START-COLUMN is the start column, that can be middle of a
  tab character, and END is the end position.  Reversed until finished.

CODE-FENCE: the marker of fence for fenced code block.

LIST-MARKER-TYPE: the type of list marker for list item.  One of symbols `-',
  `+', `*', `number-dot', or `number-parenthesis'."
  (let ((table (make-hash-table)))
    (puthash :type type table)
    (puthash :start start table)
    (puthash :end end table)
    (puthash :content-offset content-offset table)
    (puthash :closed closed table)
    (puthash :next next table)
    (puthash :previous previous table)
    (puthash :parent parent table)
    (puthash :first-child first-child table)
    (puthash :last-child last-child table)
    (puthash :spans spans table)
    (puthash :code-fence code-fence table)
    (puthash :list-marker-type list-marker-type table)
    table))

(defun kotlin-mode--comment-node-dump (node)
  "Return string representation of the NODE.

Intented for debugging and testing."
  (let ((indent "")
        (chunks '()))
    (kotlin-mode--comment-node-walk
     node
     (lambda (node event)
       (cond
        ((eq event 'visit)
         (when chunks
           (push "\n" chunks))
         (push (format
                "%s%s %d-%d"
                indent
                (gethash :type node)
                (line-number-at-pos (gethash :start node))
                ;; (save-excursion
                ;;   (goto-char (gethash :start node))
                ;;   (current-column))
                (line-number-at-pos (gethash :end node))
                ;; (save-excursion
                ;;   (goto-char (gethash :end node))
                ;;   (current-column))
                )
               chunks))
        ((eq event 'enter)
         (setq indent (concat indent "  ")))
        ((eq event 'leave)
         (setq indent (substring indent 0 (- (length indent) 2)))))))
    (apply #'concat (reverse chunks))))

(defun kotlin-mode--comment-node-walk (root handler)
  "Traverse tree from ROOT in depth first order.

Call HANDLER with two arguments NODE EVENT, where NODE is the visiting node and
EVENT is one of the symbols below:

- `visit': visiting the NODE.
- `enter': going to visit NODE's children.
- `leave': finished to visit NODE's children.

Don't consume stack even with deep tree."
  (let ((node root)
        (done nil))
    (while (not done)
      (funcall handler node 'visit)
      (cond
       ((gethash :first-child node)
        (funcall handler node 'enter)
        (setq node (gethash :first-child node)))
       ((gethash :next node)
        (setq node (gethash :next node)))
       (t
        (while (and (not (gethash :next node))
                    (not (eq node root)))
          (setq node (gethash :parent node))
          (unless (eq node root)
            (funcall handler node 'leave)))
        (if (eq node root)
            (setq done t)
          (setq node (gethash :next node))))))))

(defun kotlin-mode--comment-node-add-child (parent child)
  "Add a CHILD node to the given PARENT node.

If the PARENT is a list and the CHILD is not a list item, finalize the PARENT
and add the CHILD to the grandparent."
  (when (and (eq (gethash :type parent) 'list)
             (not (eq (gethash :type child) 'list-item)))
    (kotlin-mode--comment-node-finalize parent t)
    (setq parent (gethash :parent parent)))
  (setf (gethash :parent child) parent)
  (let ((last-child (gethash :last-child parent)))
    (if (null last-child)
        (progn
          (setf (gethash :first-child parent) child)
          (setf (gethash :last-child parent) child))
      (setf (gethash :next last-child) child)
      (setf (gethash :previous child) last-child)
      (setf (gethash :last-child parent) child))))

(defun kotlin-mode--comment-node-close-descendants (node close-at-previous-line)
  "Close descendants of the NODE.

Set the end position of the NODE to the end of the current line if
CLOSE-AT-PREVIOUS-LINE is nil.  Otherwise, set it to the end of the previous
line."
  (let ((tip (kotlin-mode--comment-node-last-node node)))
    (while (and tip (not (eq tip node)))
      (kotlin-mode--comment-node-finalize tip close-at-previous-line)
      (setq tip (gethash :parent tip)))))

(defun kotlin-mode--comment-node-trim-end (node)
  "Remove trailing blank lines from spans of the NODE."
  (let ((spans (reverse (gethash :spans node)))
        (end (gethash :end node)))
    (while (and spans
                (save-excursion
                  (goto-char (nth 0 (car spans)))
                  (skip-chars-forward "\s\t")
                  (<= (nth 2 (car spans)) (point))))
      (setq end (nth 2 (car spans)))
      (pop spans))
    (when spans
      (setq end (nth 2 (car spans))))
    (setf (gethash :end node) end)
    (setf (gethash :spans node) (reverse spans))))

(defun kotlin-mode--comment-node-finalize (node close-at-previous-line)
  "Close and set the end position of the NODE.

Set the end position of the NODE to the end of the current line if
CLOSE-AT-PREVIOUS-LINE is nil.  Otherwise, set it to the end of the previous
line.

The end position is adjusted according to the type of the NODE."
  (unless (gethash :closed node)
    (setf (gethash :closed node) t)
    (setf (gethash :spans node) (reverse (gethash :spans node)))
    (let ((end
           (if close-at-previous-line
               (save-excursion
                 (forward-line 0)
                 (unless (bobp) (backward-char))
                 (point))
             (line-end-position))))
      (setf (gethash :end node) end))
    (cond
     ;; Indented code block
     ((eq (gethash :type node) 'indented-code-block)
      ;; Indented code blocks don't contain trailing blank lines while fenced
      ;; code blocks do, even if it is closed implicitly:
      ;;
      ;; - ```
      ;;   The following line is a part of the code block:
      ;;
      ;; - This line is not a part of the code block.
      ;;   It has been closed implicitly.
      (kotlin-mode--comment-node-trim-end node))

     ;; List item
     ((eq (gethash :type node) 'list-item)
      (if (gethash :last-child node)
          (setf (gethash :end node)
                (gethash :end (gethash :last-child node)))
        (save-excursion
          (goto-char (gethash :start node))
          (setf (gethash :end node) (line-end-position)))))

     ;; List
     ((eq (gethash :type node) 'list)
      (setf (gethash :end node)
            (gethash :end (gethash :last-child node))))

     ;; Comment tag
     ((eq (gethash :type node) 'comment-tag)
      (if (gethash :last-child node)
          (setf (gethash :end node)
                (gethash :end (gethash :last-child node)))
        (save-excursion
          (goto-char (gethash :start node))
          (setf (gethash :end node) (line-end-position))))))))

(defun kotlin-mode--comment-node-last-node (node)
  "Return the last leaf of the NODE."
  (let ((tip node))
    (while (gethash :last-child tip)
      (setq tip (gethash :last-child tip)))
    tip))

(defun kotlin-mode--comment-node-can-continue-paragraph (node)
  "Return non-nil if and only if the current line can continue paragraph.

NODE is the partially parsed tree."
  (let ((tip (kotlin-mode--comment-node-last-node node)))
    (and (not (gethash :closed tip))
         (eq (gethash :type tip) 'paragraph))))

(defvar-local kotlin-mode--comment-parser-current-column 0
  "Current column of the comment parser, that can be the middle of a tab.")

(defun kotlin-mode--comment-parser-forward-column (&optional arg)
  "Forward the parser ARG columns."
  (kotlin-mode--comment-parser-move-to-column
   (+ kotlin-mode--comment-parser-current-column (or arg 1))))

(defun kotlin-mode--comment-parser-move-to-column (column)
  "Move the parser to COLUMNth columns."
  (setq kotlin-mode--comment-parser-current-column column)
  (move-to-column column))

(defun kotlin-mode--comment-parser-end-of-line ()
  "Move the parser to the end of the line."
  (end-of-line)
  (setq kotlin-mode--comment-parser-current-column (current-column)))

(defun kotlin-mode--comment-parser-add-span (parent)
  "Add the rest of the current line to the spans of the PARENT node."
  (push (list (point)
              kotlin-mode--comment-parser-current-column
              (line-end-position))
        (gethash :spans (kotlin-mode--comment-node-last-node parent)))
  (kotlin-mode--comment-parser-end-of-line))

(defun kotlin-mode--comment-parser-try-start (parent)
  "Try to start a new node and add to the PARENT.

If a node is created, return the new container node, that can be the new node
or the PARENT.  Otherwise, return nil.

Close the descendants of the PARENT if needed."
  (let ((indent (- (save-excursion
                     (skip-chars-forward "\s\t")
                     (current-column))
                   kotlin-mode--comment-parser-current-column)))
    (cond
     ;; Content of code block
     ((memq (gethash :type parent) '(indented-code-block fenced-code-block))
      (kotlin-mode--comment-parser-add-span parent)
      (kotlin-mode--comment-parser-end-of-line)
      nil)

     ;; Setext heading
     ((and (< indent 4)
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                (or (one-or-more "=")
                                    (>= 2 "-"))
                                (zero-or-more (any "\s\t"))
                                line-end)))
           (let ((preceding (gethash :last-child parent)))
             ;; FIXME it should not be link reference definitions.
             (and
              preceding
              (eq (gethash :type preceding) 'paragraph)
              (not (gethash :closed preceding)))))
      (kotlin-mode--comment-node-close-descendants parent nil)
      (setf (gethash :type (gethash :last-child parent)) 'setext-heading)
      (kotlin-mode--comment-parser-end-of-line)
      nil)

     ;; Thematic break
     ((and (< indent 4)
           (looking-at
            (rx (seq (zero-or-more (any "\s\t"))
                     (or (>= 3 (seq "-" (zero-or-more (any "\s\t"))))
                         (>= 3 (seq "_" (zero-or-more (any "\s\t"))))
                         (>= 3 (seq "*" (zero-or-more (any "\s\t")))))
                     (zero-or-one (any "\s\t"))
                     line-end))))
      (kotlin-mode--comment-node-close-descendants parent t)
      (kotlin-mode--comment-node-add-child parent
                                           (kotlin-mode--comment-node
                                            :type 'thematic-break
                                            :start (point)
                                            :end (line-end-position)
                                            :content-offset (line-end-position)
                                            :closed t))
      (kotlin-mode--comment-parser-end-of-line)
      nil)

     ;; List item
     ((and (< indent 4)
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                (or (any "-+*")
                                    (seq (** 1 9 (any "0-9"))
                                         (any ".)")))
                                (or (any "\s\t")
                                    line-end))))
           ;; When the first list item in a list interrupts a paragraph—that
           ;; is, when it starts on a line that would otherwise count as
           ;; paragraph continuation text—then (a) the lines Ls must not begin
           ;; with a blank line, and (b) if the list item is ordered, the start
           ;; number must be 1.
           ;; https://spec.commonmark.org/0.30/#list-items
           (or (not
                (and
                 (gethash :last-child parent)
                 (eq (gethash :type (gethash :last-child parent)) 'paragraph)
                 (kotlin-mode--comment-node-can-continue-paragraph parent)))
               (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                    (or (any "-+*")
                                        (seq "1" (any ".)")))
                                    (* (any "\s\t"))
                                    (not (any "\s\t\n")))))))
      (kotlin-mode--comment-node-close-descendants parent t)
      (let ((start (point))
            (start-column kotlin-mode--comment-parser-current-column)
            content-offset
            list
            item
            marker-type)
        (skip-chars-forward "\s\t")
        (skip-chars-forward "0-9")
        (setq marker-type (cond ((eq (char-after) ?-) '-)
                                ((eq (char-after) ?+) '+)
                                ((eq (char-after) ?*) '*)
                                ((eq (char-after) ?.) 'number-dot)
                                (t 'number-parenthesis)))
        (skip-chars-forward "-+*.)")
        (setq kotlin-mode--comment-parser-current-column (current-column))
        (setq content-offset
              (progn
                (cond
                 ;; Starting with blank line.
                 ((looking-at (rx (seq (zero-or-more (any "\s\t"))
                                       line-end)))
                  (kotlin-mode--comment-parser-forward-column))

                 ;; Starting with indented code.
                 ((save-excursion
                    (skip-chars-forward "\s\t")
                    (>= (- (current-column)
                           kotlin-mode--comment-parser-current-column)
                        5))
                  (kotlin-mode--comment-parser-forward-column))

                 ;; Otherwise.
                 (t
                  (skip-chars-forward "\s\t")
                  (setq kotlin-mode--comment-parser-current-column
                        (current-column))))
                (- kotlin-mode--comment-parser-current-column start-column)))
        ;; Add list node if needed.
        (if (eq (gethash :type parent) 'list)
            (if (eq (gethash :list-marker-type (gethash :first-child parent))
                    marker-type)
                (setq list parent)
              (kotlin-mode--comment-node-finalize parent t)
              (setq list (kotlin-mode--comment-node
                          :type 'list
                          :start start
                          :content-offset 0))
              (kotlin-mode--comment-node-add-child (gethash :parent parent)
                                                   list))
          (setq list (kotlin-mode--comment-node
                      :type 'list
                      :start start
                      :content-offset 0))
          (kotlin-mode--comment-node-add-child parent list))
        ;; Add item.
        (setq item (kotlin-mode--comment-node
                    :type 'list-item
                    :start start
                    :content-offset content-offset
                    :list-marker-type marker-type))
        (kotlin-mode--comment-node-add-child list item)
        item))

     ;; ATX heading
     ((and (< indent 4)
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                (** 1 6 "#")
                                (or (any "\s\t")
                                    line-end)))))
      (kotlin-mode--comment-node-close-descendants parent t)
      (let ((start (point))
            (start-column kotlin-mode--comment-parser-current-column))
        (skip-chars-forward "\s\t")
        (skip-chars-forward "#")
        (skip-chars-forward "\s\t")
        (kotlin-mode--comment-node-add-child
         parent
         (kotlin-mode--comment-node
          :type 'atx-heading
          :start start
          :end (line-end-position)
          :content-offset (- (current-column) start-column)
          :closed t)))
      (kotlin-mode--comment-parser-end-of-line)
      nil)

     ;; Indented code block
     ((and (>= indent 4)
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                (not (any "\s\t\n")))))
           (not (kotlin-mode--comment-node-can-continue-paragraph parent)))
      (kotlin-mode--comment-node-close-descendants parent t)
      (let ((code-block
             (kotlin-mode--comment-node
              :type 'indented-code-block
              :start (point)
              :end (line-end-position)
              :content-offset 4)))
        (kotlin-mode--comment-node-add-child parent code-block)
        (kotlin-mode--comment-parser-forward-column 4)
        (kotlin-mode--comment-parser-add-span parent)
        (kotlin-mode--comment-parser-end-of-line)
        nil))

     ;; Fenced code block
     ((and (< indent 4)
           (looking-at
            (rx (seq (zero-or-more (any "\s\t"))
                     (or (seq (>= 3 "`")
                              (zero-or-more (not (any "`")))
                              line-end)
                         (>= 3 "~"))))))
      (kotlin-mode--comment-node-close-descendants parent t)
      (let* ((start (point))
             (content-offset (progn
                               (skip-chars-forward "\s\t")
                               (- (current-column)
                                  kotlin-mode--comment-parser-current-column)))
             (code-fence (progn
                           (buffer-substring-no-properties
                            (point)
                            (progn
                              (skip-chars-forward (string (char-after)))
                              (point)))))
             (code-block (kotlin-mode--comment-node
                          :type 'fenced-code-block
                          :start start
                          :content-offset content-offset
                          :code-fence code-fence)))
        (kotlin-mode--comment-node-add-child parent code-block)
        (kotlin-mode--comment-parser-end-of-line)
        nil))

     ;; Block quote
     ((and (< indent 4)
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                ">"))))
      (kotlin-mode--comment-node-close-descendants parent t)
      (let ((start (point))
            block-quote)
        (skip-chars-forward "\s\t")
        (forward-char)
        (setq kotlin-mode--comment-parser-current-column (current-column))
        (setq block-quote (kotlin-mode--comment-node
                           :type 'block-quote
                           :start start
                           :content-offset 2))
        (kotlin-mode--comment-node-add-child parent block-quote)
        (when (memq (char-after) '(?\s ?\t))
          (kotlin-mode--comment-parser-forward-column))
        block-quote))

     ;; Comment tag
     ((and (< indent 4)
           (let ((parent-type (gethash :type parent)))
             (when (eq parent-type 'list)
               (setq parent-type (gethash :type (gethash :parent parent))))
             (memq parent-type
                   '(document single-line-comment multiline-comment)))
           (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                "@"
                                (any "a-zA-Z")))))
      ;; Markdown parser of KDoc is different to CommonMark.
      ;; We struggle to be consistent to CommonMark, but not be too strict.
      (kotlin-mode--comment-node-close-descendants parent t)
      (let* ((start (point))
             (tag-name (progn
                         (skip-chars-forward "\s\t")
                         (forward-char)
                         (looking-at (rx (zero-or-more (any "a-zA-Z0-9"))))
                         (match-string-no-properties 0)))
             (content-offset
              (progn
                (skip-chars-forward "a-zA-Z0-9")
                (when (member tag-name '("param"
                                         "property"
                                         "throws"
                                         "exception"))
                  (skip-chars-forward "\s\t")
                  (skip-chars-forward "^\s\t\n"))
                (setq kotlin-mode--comment-parser-current-column
                      (current-column))
                kotlin-mode-multiline-statement-offset))
             (comment-tag (kotlin-mode--comment-node
                           :type 'comment-tag
                           :start start
                           :content-offset content-offset)))
        (kotlin-mode--comment-node-add-child parent comment-tag)
        comment-tag))

     ;; Paragraph
     ((not (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                line-end))))
      (unless (kotlin-mode--comment-node-can-continue-paragraph parent)
        (kotlin-mode--comment-node-close-descendants parent t)
        (kotlin-mode--comment-node-add-child parent
                                             (kotlin-mode--comment-node
                                              :type 'paragraph
                                              :start (point)
                                              :content-offset 0)))
      (kotlin-mode--comment-parser-add-span parent)
      (kotlin-mode--comment-parser-end-of-line)
      nil)

     ;; Blank line
     (t
      (kotlin-mode--comment-node-close-descendants parent t)
      nil))))

(defun kotlin-mode--comment-parser-check-continued (node)
  "Check the current line is continuation of the NODE.

If the current line is continuation of the NODE, advance parser to the content
column of the NODE.

Otherwise, return nil and keep the point intact.

If the current line explicitly close the NODE, finalize the NODE."
  (let ((node-type (gethash :type node))
        (indent (- (save-excursion
                     (skip-chars-forward "\s\t")
                     (current-column))
                   kotlin-mode--comment-parser-current-column))
        (blank (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                    line-end)))))
    (cond
     ;; Document
     ((eq node-type 'document)
      t)

     ;; Thematic break
     ((eq node-type 'thematic-break)
      nil)

     ;; ATX heading
     ((eq node-type 'atx-heading)
      nil)

     ;; Setext heading
     ((eq node-type 'setext-heading)
      nil)

     ;; Indented code block
     ((eq node-type 'indented-code-block)
      (when (or (>= indent 4) blank)
        (kotlin-mode--comment-parser-forward-column 4)
        t))

     ;; Fenced code block
     ((eq node-type 'fenced-code-block)
      (let ((closed (and (< indent 4)
                         (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                              (or (one-or-more "`")
                                                  (one-or-more "~"))
                                              (zero-or-more (any "\s\t"))
                                              line-end)))
                         (save-excursion
                           (skip-chars-forward "\s\t")
                           (looking-at (gethash :code-fence node))))))
        (if closed
            (progn
              (kotlin-mode--comment-node-finalize node nil)
              (kotlin-mode--comment-parser-end-of-line)
              nil)
          (kotlin-mode--comment-parser-move-to-column
           (min (save-excursion
                  (skip-chars-forward "\s\t")
                  (current-column))
                (progn
                  (kotlin-mode--comment-parser-forward-column
                   (gethash :content-offset node))
                  kotlin-mode--comment-parser-current-column)))
          t)))

     ;; Paragraph
     ((eq node-type 'paragraph)
      (when blank
        (progn
          (kotlin-mode--comment-node-finalize node t)
          (kotlin-mode--comment-parser-end-of-line)))
      nil)

     ;; Block quote
     ((eq node-type 'block-quote)
      (when (and (< indent 4)
                 (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                      ">"))))
        (skip-chars-forward "\s\t")
        (forward-char)
        (setq kotlin-mode--comment-parser-current-column (current-column))
        (when (memq (char-after) '(?\s ?\t))
          (kotlin-mode--comment-parser-forward-column))
        t))

     ;; List item
     ((eq node-type 'list-item)
      (when (or (and
                 ;; List item can contain blank line.
                 blank
                 ;; List items cannot start with multiple blank lines.
                 (gethash :first-child node))
                (save-excursion
                  (skip-chars-forward "\s\t")
                  (<= (+ kotlin-mode--comment-parser-current-column
                         (gethash :content-offset node))
                      (current-column))))
        (kotlin-mode--comment-parser-forward-column
         (gethash :content-offset node))
        t))

     ;; List
     ((eq node-type 'list)
      t)

     ;; Single line comment
     ((eq node-type 'single-line-comment)
      (skip-chars-forward "\s\t")
      (skip-chars-forward "/")
      (setq kotlin-mode--comment-parser-current-column (current-column))
      (when (memq (char-after) '(?\s ?\t))
        (kotlin-mode--comment-parser-forward-column))
      t)

     ;; Multiline comment
     ((eq node-type 'multiline-comment)
      (if (and kotlin-mode-prepend-asterisk-to-comment-line
               (< indent (+ (gethash :content-offset node) 4))
               (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                    "*"))))
          (progn
            (skip-chars-forward "\s\t")
            (forward-char)
            (setq kotlin-mode--comment-parser-current-column (current-column))
            (when (and kotlin-mode-insert-space-after-asterisk-in-comment
                       (memq (char-after) '(?\s ?\t)))
              (kotlin-mode--comment-parser-forward-column)))
        (kotlin-mode--comment-parser-move-to-column
         (min (save-excursion
                (skip-chars-forward "\s\t")
                (current-column))
              (progn
                (kotlin-mode--comment-parser-forward-column
                 (gethash :content-offset node))
                kotlin-mode--comment-parser-current-column))))
      t)

     ;; Comment tag
     ((eq node-type 'comment-tag)
      (if (and (< indent 4)
               (looking-at (rx (seq (zero-or-more (any "\s\t"))
                                    "@"
                                    (any "a-zA-Z")))))
          (progn
            (kotlin-mode--comment-node-finalize node t)
            nil)
        (kotlin-mode--comment-parser-move-to-column
         (min (save-excursion
                (skip-chars-forward "\s\t")
                (current-column))
              (progn
                (kotlin-mode--comment-parser-forward-column
                 (gethash :content-offset node))
                kotlin-mode--comment-parser-current-column)))
        t)))))

(defun kotlin-mode--comment-parser-add-line (document)
  "Add a line to the DOCUMENT."
  (let ((node (gethash :last-child document))
        (parent document))
    (while node
      (unless (and (not (gethash :closed node))
                   (kotlin-mode--comment-parser-check-continued node))
        (setq node nil))
      (when node
        (setq parent node)
        (setq node (gethash :last-child node))))
    (while (setq parent (kotlin-mode--comment-parser-try-start parent))
      nil)))

(defun kotlin-mode--parse-kdoc-in-single-line-comments (start end)
  "Parse a block of single line comments as KDoc comment.

The block is assumed to be from START to END."
  (let ((document (kotlin-mode--comment-node
                   :type 'document
                   :start start
                   :content-offset 0)))
    (kotlin-mode--comment-node-add-child
     document
     (kotlin-mode--comment-node
      :type 'single-line-comment
      :start start
      :content-offset (save-excursion
                        (goto-char start)
                        (skip-chars-forward "\s\t")
                        (+ 3 (current-column)))))
    (kotlin-mode--parse-kdoc-comment start end document)))

(defun kotlin-mode--parse-kdoc-in-multiline-comment (start end)
  "Parse a multiline comment as KDoc comment.

The comment is assumed to be from START to END."
  (save-excursion
    (goto-char start)
    (when (looking-at (rx "/*"))
      (forward-char)
      (skip-chars-forward "*"))
    (setq start (point)))
  (save-excursion
    (goto-char end)
    (when (and (eq (char-before) ?/)
               (eq (char-before (1- (point))) ?*))
      (setq end (- end 2))))
  (let ((document (kotlin-mode--comment-node
                   :type 'document
                   :start start
                   :content-offset 0)))
    (kotlin-mode--comment-node-add-child
     document
     (kotlin-mode--comment-node
      :type 'multiline-comment
      :start start
      :content-offset (save-excursion
                        (goto-char start)
                        (skip-chars-forward "\s\t")
                        (forward-char 2)
                        (1+ (current-column)))))
    (kotlin-mode--parse-kdoc-comment start end document)))

(defun kotlin-mode--parse-kdoc-in-multiline-string (start end)
  "Parse a multiline string as KDoc comment.

The string is assumed to be from START to END."
  (save-excursion
    (goto-char start)
    (skip-chars-forward "\"")
    (setq start (point)))
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\"")
    (setq end (point)))
  (kotlin-mode--parse-kdoc-comment start
                                   end
                                   (kotlin-mode--comment-node
                                    :type 'document
                                    :start start
                                    :content-offset 0)))


(defun kotlin-mode--parse-kdoc-comment (start end &optional document)
  "Parse a KDoc comment.

The comment is assumed to be from START to END.

Results are appended to DOCUMENT as children.

If DOCUMENT is ommitted, it is created."
  (unless document
    (setq document (kotlin-mode--comment-node
                    :type 'document
                    :start start
                    :content-offset 0)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (not (eobp))
        (kotlin-mode--comment-parser-add-line document)
        (forward-line)
        (setq kotlin-mode--comment-parser-current-column 0))
      (kotlin-mode--comment-node-close-descendants document (bolp))
      (kotlin-mode--comment-node-finalize document (bolp))
      document)))

(provide 'kotlin-mode-kdoc-comment-parser)

;;; kotlin-mode-kdoc-comment-parser.el ends here
