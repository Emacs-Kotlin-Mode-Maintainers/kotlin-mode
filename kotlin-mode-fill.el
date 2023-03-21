;;; kotlin-mode-fill.el --- Major-mode for Kotlin, paragraph filling. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Josh Caswell, taku0

;; Author: Josh Caswell (https://github.com/woolsweater)
;;         taku0 (http://github.com/taku0)

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

;; Routines for paragraph filling

;; Fill only paragraphs in comments and some multiline strings.
;;
;; If a line conains both code and comment/string, it is not filled.
;;
;; `auto-fill-mode' is intentionally disabled in multiline strings.
;;
;; `fill-region' is effective if the region is included in a string, but not if
;; it is partially out of the string, so you can apply `fill-region' to the
;; entire buffer to fill paragraphs in comments.
;;
;; Contents in comments/strings are parses as KDoc.  Markdown paragraphs are
;; filled but other things like headers or block codes are not filled.

;;; Code:

(require 'rx)
(require 'kotlin-mode-lexer)
(require 'kotlin-mode-indent)
(require 'kotlin-mode-kdoc-comment-parser)

(defun kotlin-mode--find-single-line-comment-edges ()
  "Return start and end of a single-line comment block.

A single-line comment block is a continuous lines with same \"comment level\".

Comment level is number of slashes after indentation.

Return tuple (START . END) where START is the point before the first slash of
the block, and END is the end of the last comment, after the last line break if
any.

Point may be anywhere in a single-line comment when this is called."
  (let* ((current-comment-level (kotlin-mode--single-line-comment-level))
         (start
          (save-excursion
            (while (and (not (bobp))
                        (= (kotlin-mode--single-line-comment-level)
                           current-comment-level))
              (forward-line -1))
            (unless (= (kotlin-mode--single-line-comment-level)
                       current-comment-level)
              (forward-line 1))
            (back-to-indentation)
            (point)))
         (end
          (save-excursion
            (while (and (not (eobp))
                        (= (kotlin-mode--single-line-comment-level)
                           current-comment-level))
              (when (/= (forward-line 1) 0)
                (goto-char (point-max))))
            (point))))
    (cons start end)))
(defun kotlin-mode--single-line-comment-level ()
  "Return comment level of the current line.

Return 1.0e+INF if the line doesn't start with a single-line comment."
  (save-excursion
    (save-match-data
      (back-to-indentation)
      (if (looking-at "//+")
          (- (match-end 0) (match-beginning 0))
        ;; Not a comment
        1.0e+INF))))

(defun kotlin-mode--kdoc-comment-region (chunk)
  "Return region of KDoc containing CHUNK.

If CHUNK is a multiline comment or multiline string, return its region.

If CHUNK is a single-line comment, see
`kotlin-mode--find-single-line-comment-edges' for the return value."
  (cond
   ((or (kotlin-mode--chunk-multiline-comment-p chunk)
        (kotlin-mode--chunk-multiline-string-p chunk))
    (cons (kotlin-mode--chunk-start chunk)
          (kotlin-mode--chunk-end chunk)))
   ((kotlin-mode--chunk-single-line-comment-p chunk)
    (save-excursion
      (goto-char (kotlin-mode--chunk-start chunk))
      (kotlin-mode--find-single-line-comment-edges)))
   (t nil)))

(defun kotlin-mode--adaptive-fill ()
  "Return adaptive fill prefix for the current line."
  (let* ((chunk (kotlin-mode--chunk-after-spaces))
         (comment-region (kotlin-mode--kdoc-comment-region chunk))
         (start (car comment-region))
         (end (cdr comment-region))
         (kdoc
          ;; FIXME cache
          (cond
           ((kotlin-mode--chunk-multiline-comment-p chunk)
            (kotlin-mode--parse-kdoc-in-multiline-comment start end))
           ((kotlin-mode--chunk-single-line-comment-p chunk)
            (kotlin-mode--parse-kdoc-in-single-line-comments start end))
           ((kotlin-mode--chunk-multiline-string-p chunk)
            (kotlin-mode--parse-kdoc-in-multiline-string start end))
           (t nil)))
         (prefixes '())
         (current-column 0)
         (ancestors '())
         tip
         node
         node-type)
    (if (null kdoc)
        (save-excursion
          (back-to-indentation)
          (buffer-substring-no-properties (line-beginning-position) (point)))
      (save-excursion
        (end-of-line)
        (kotlin-mode--comment-node-walk
         kdoc
         (lambda (node event)
           (when (and (eq event 'visit)
                      (not (gethash :first-child node)))
             (cond
              ((null tip)
               (setq tip node))
              ((<= (gethash :start node) (point))
               (setq tip node))))))
        (setq node tip)
        (while node
          (push node ancestors)
          (setq node (gethash :parent node)))
        (while ancestors
          (setq node (pop ancestors))
          (setq node-type (gethash :type node))
          (goto-char (gethash :start node))
          (cond
           ((eq node-type 'single-line-comment)
            (goto-char start)
            ;; FIXME indent-tabs-mode
            (push (make-string (current-column) ?\s) prefixes)
            (push (buffer-substring-no-properties
                   (point)
                   (progn
                     (skip-chars-forward "/")
                     (point)))
                  prefixes)
            (setq current-column (current-column))
            (when (memq (char-after) '(?\s ?\t ?\n nil))
              (push " " prefixes)
              (setq current-column (1+ current-column))))
           ((eq node-type 'multiline-comment)
            (goto-char start)
            (forward-char)
            ;; FIXME indent-tabs-mode
            (push (make-string (current-column) ?\s) prefixes)
            (setq current-column (current-column))
            (when kotlin-mode-prepend-asterisk-to-comment-line
              (push "*" prefixes)
              (skip-chars-forward "*")
              (setq current-column (1+ current-column))
              (when (and kotlin-mode-insert-space-after-asterisk-in-comment
                         (memq (char-after) '(?\s ?\t ?\n nil)))
                (push " " prefixes)
                (setq current-column (1+ current-column)))))
           ((eq node-type 'block-quote)
            (skip-chars-forward "\s\t")
            (push (make-string (- (current-column) current-column) ?\s)
                  prefixes)
            (push ">" prefixes)
            (forward-char)
            (setq current-column (current-column))
            (when (memq (char-after) '(?\s ?\t ?\n nil))
              (push " " prefixes)
              (setq current-column (1+ current-column))))
           ((eq node-type 'comment-tag)
            (skip-chars-forward "\s\t")
            (push (make-string (+ (- (current-column) current-column)
                                  kotlin-mode-multiline-statement-offset)
                               ?\s)
                  prefixes)
            (setq current-column (+ (current-column)
                                    kotlin-mode-multiline-statement-offset)))
           (t
            (push (make-string (gethash :content-offset node) ?\s) prefixes)
            (setq current-column
                  (+ current-column (gethash :content-offset node))))))
        (apply #'concat (reverse prefixes))))))

(defun kotlin-mode--fill-paragraph (justify)
  "Fill paragraph in Kotlin code.

JUSTIFY is as the argument of the same name in `fill-region'."
  ;; TODO Handle trailing comments.
  (save-excursion
    (forward-line 0)
    (save-match-data
      (skip-syntax-backward " ")
      (let* ((paragraphs-around (kotlin-mode--kdoc-paragraphs-around-point))
             (tip (nth 0 paragraphs-around))
             (next-leaf (nth 2 paragraphs-around))
             target-paragraph)
        (when paragraphs-around
          (setq target-paragraph (or tip next-leaf))
          (when target-paragraph
            (fill-region-as-paragraph
             (gethash :start target-paragraph)
             (gethash :end target-paragraph)
             justify))))
      t)))

(defun kotlin-mode--fill-region-as-paragraph-advice
    (fill-region-as-paragraph from to &rest args)
  "Advice function for `fill-region-as-paragraph'.

FILL-REGION-AS-PARAGRAPH is the original function, and FROM, TO, and ARGS are
original arguments.

Fix up multiline comments.

- When the region contains other than one multline comment, fill normally:

  foo() /* abc def ghi */
  ↓
  foo() /* abc
  def ghi */

- Otherwise and when the region fits one line, fill as a line:

  /*
   * abc
   * def
   */
  ↓
  /* abc def */

- Otherwise and when the region was one line, insert breaks before and after
  the contents:

  /* abc def ghi */
  ↓
  /*
   * abc def
   * ghi
   */

- Otherwise, keep line breaks around the contents and fill the contents:

  /* abc def ghi
  */
  ↓
  /* abc def
   * ghi
   */"
  (if (eq major-mode 'kotlin-mode)
      (let ((chunk (save-excursion
                     (save-match-data
                       (goto-char from)
                       (kotlin-mode--chunk-after-spaces))))
            filling-entire-comment
            from-without-spaces
            to-without-spaces
            ;; Position of comment including delimiters.
            comment-start-pos
            comment-end-pos
            ;; Position of comment delimiters.
            comment-opener-end
            comment-closer-start
            ;; Position of contents (without comment delimiters and spaces)
            contents-start
            contents-end
            ;; Was the comment one line before filling?
            was-one-line
            ;; Is the contents (without delimiters) one line after filling?
            is-one-line
            ;; Are the contents and delimiters fits in one line?
            fits-one-line
            result)
        (if (kotlin-mode--chunk-multiline-comment-p chunk)
            (progn
              (setq comment-start-pos (kotlin-mode--chunk-start chunk))
              (setq comment-end-pos (kotlin-mode--chunk-end chunk))
              (setq from-without-spaces
                    (save-excursion
                      (goto-char from)
                      (skip-syntax-forward " ")
                      (when (eq (char-after) ?\n)
                        (forward-char)
                        (skip-syntax-forward " ")
                        (when (and kotlin-mode-prepend-asterisk-to-comment-line
                                   (eq (char-after) ?*))
                          (forward-char)
                          (skip-syntax-forward " ")))
                      (skip-syntax-forward " >")
                      (point)))
              (setq to-without-spaces
                    (save-excursion
                      (goto-char to)
                      (skip-syntax-backward " >")
                      (point)))
              (setq contents-start
                    (save-excursion
                      (goto-char comment-start-pos)
                      (forward-char)
                      (skip-chars-forward "*")
                      (skip-syntax-forward " ")
                      (when (eq (char-after) ?\n)
                        (forward-char)
                        (skip-syntax-forward " ")
                        (when (and kotlin-mode-prepend-asterisk-to-comment-line
                                   (eq (char-after) ?*))
                          (forward-char)
                          (skip-syntax-forward " ")))
                      (skip-syntax-forward " >")
                      (point)))
              (setq contents-end
                    (save-excursion
                      (goto-char comment-end-pos)
                      ;; Comment can be incomplete.
                      ;; Even worse, it can be "/*/".
                      (when (and (<= (+ comment-start-pos 2)
                                     (- comment-end-pos 2))
                                 (eq (char-before) ?/)
                                 (eq (char-before (1- (point))) ?*))
                        (backward-char)
                        (skip-chars-backward "*")
                        (skip-syntax-backward " >"))
                      (point)))
              (setq filling-entire-comment
                    (and (member from-without-spaces
                                 (list comment-start-pos contents-start))
                         (member to-without-spaces
                                 (list comment-end-pos contents-end))))
              (if filling-entire-comment
                  (progn
                    (setq comment-end-pos (copy-marker comment-end-pos))
                    (setq contents-end (copy-marker contents-end))
                    (setq was-one-line (kotlin-mode--same-line-p
                                        comment-start-pos
                                        comment-end-pos))
                    (setq result (apply
                                  fill-region-as-paragraph
                                  contents-start
                                  contents-end
                                  args))
                    (setq is-one-line (kotlin-mode--same-line-p contents-start
                                                                contents-end))
                    (cond
                     ((and was-one-line (not is-one-line))
                      (save-excursion
                        (goto-char contents-end)
                        (insert-and-inherit ?\n)
                        (indent-according-to-mode)
                        (goto-char contents-start)
                        (insert-and-inherit "* ")
                        (goto-char contents-start)
                        (delete-horizontal-space)
                        (insert-and-inherit ?\n)
                        (indent-according-to-mode)))
                     ((and (not was-one-line) is-one-line)
                      (setq comment-opener-end
                            (save-excursion
                              (goto-char comment-start-pos)
                              (forward-char)
                              (skip-chars-forward "*")
                              (point-marker)))
                      (setq comment-closer-start
                            (save-excursion
                              (goto-char contents-end)
                              (skip-syntax-forward " >")
                              (copy-marker (min (point) comment-end-pos))))
                      (setq contents-start (copy-marker contents-start))
                      ;; Determinate whether the contents and the delimiter
                      ;; fits one line.
                      ;; Using `transpose-regions' rather than
                      ;; `delete-and-extract-region' to keep markers.
                      ;;
                      ;; `transpose-regions' moves markers as if the markers
                      ;; were placed after their position:
                      ;;
                      ;; /**1
                      ;;  * 2abc def3
                      ;;  4*/5
                      ;;
                      ;; ↓ (transpose-regions m1 m1 m2 m3)
                      ;;
                      ;; /**2abc def1
                      ;;  * 3
                      ;;  4*/5
                      (transpose-regions comment-opener-end
                                         comment-opener-end
                                         contents-start
                                         contents-end)
                      ;; /**2abc def1
                      ;;  * 3
                      ;;  4*/5
                      ;;
                      ;; ↓ (transpose-regions m1 m1 m4 m5)
                      ;;
                      ;; /**2abc def4*/1
                      ;;  * 3
                      ;;  5
                      (transpose-regions comment-opener-end
                                         comment-opener-end
                                         comment-closer-start
                                         comment-end-pos)
                      (save-excursion
                        (goto-char comment-closer-start)
                        (insert-and-inherit ?\s)
                        (goto-char contents-start)
                        (insert-and-inherit ?\s)
                        (goto-char comment-opener-end)
                        (setq fits-one-line (<= (current-column) fill-column))
                        (goto-char contents-start)
                        (delete-char 1)
                        (goto-char comment-closer-start)
                        (delete-char 1))
                      ;; /**2abc def4*/1
                      ;;  * 3
                      ;;  5
                      ;;
                      ;; ↓ (transpose-regions m4 m1 m5 m5)
                      ;;
                      ;; /**2abc def1
                      ;;  * 3
                      ;;  4*/5
                      (transpose-regions comment-closer-start
                                         comment-opener-end
                                         comment-end-pos
                                         comment-end-pos)
                      ;; /**2abc def1
                      ;;  * 3
                      ;;  4*/5
                      ;;
                      ;; ↓ (transpose-regions m2 m1 m3 m3)
                      ;;
                      ;; /**1
                      ;;  * 2abc def3
                      ;;  4*/5
                      (transpose-regions contents-start
                                         comment-opener-end
                                         contents-end
                                         contents-end)
                      (when fits-one-line
                        (save-excursion
                          (delete-region comment-opener-end contents-start)
                          (goto-char comment-opener-end)
                          (insert-and-inherit ?\s)
                          (delete-region contents-end comment-closer-start)
                          (goto-char contents-end)
                          (insert-and-inherit ?\s)))
                      (set-marker contents-start nil)
                      (set-marker comment-closer-start nil)
                      (set-marker comment-opener-end nil)))
                    (set-marker contents-end nil)
                    (set-marker comment-end-pos nil)
                    result)
                (apply fill-region-as-paragraph from to args)))
          (apply fill-region-as-paragraph from to args)))
    (apply fill-region-as-paragraph from to args)))

(defun kotlin-mode--install-fill-region-as-paragraph-advice ()
  "Install advice around `fill-region-as-paragraph'."
  (advice-add 'fill-region-as-paragraph
              :around
              #'kotlin-mode--fill-region-as-paragraph-advice))

(defun kotlin-mode--fill-forward-paragraph (arg)
  "Forward ARG paragraphs for filling.

Returns the count of paragraphs left to move."
  (if (< arg 0)
      (kotlin-mode--fill-backward-paragraph (- arg))
    (let ((done nil))
      (while (and (< 0 arg)
                  (not done))
        (setq done (not (kotlin-mode--fill-skip-paragraph-1 'forward)))
        (unless done (setq arg (1- arg)))))
    arg))

(defun kotlin-mode--fill-backward-paragraph (arg)
  "Backward ARG paragraphs for filling.

Returns the count of paragraphs left to move."
  (if (< arg 0)
      (kotlin-mode--fill-forward-paragraph (- arg))
    (let ((done nil))
      (while (and (< 0 arg)
                  (not done))
        (setq done (not (kotlin-mode--fill-skip-paragraph-1 'backward)))
        (unless done (setq arg (1- arg)))))
    arg))

(defun kotlin-mode--fill-skip-paragraph-1 (direction)
  "Skip a paragraph for filling.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (save-match-data
    ;; Skip whitespaces.
    (if (eq direction 'backward)
        (skip-syntax-backward " ")
      (skip-syntax-forward " "))
    (let* ((chunk (kotlin-mode--chunk-after-spaces))
           (comment-region (kotlin-mode--kdoc-comment-region chunk))
           (start (car comment-region))
           (end (cdr comment-region))
           (paragraphs-around (kotlin-mode--kdoc-paragraphs-around-point))
           (tip (nth 0 paragraphs-around))
           (previous-leaf (nth 1 paragraphs-around))
           (next-leaf (nth 2 paragraphs-around))
           target-paragraph)
      (if (null paragraphs-around)
          (kotlin-mode--fill-skip-paragraph-in-code direction)
        (setq target-paragraph
              (cond
               ;; Point is between paragraphs.
               ((null tip)
                (if (eq direction 'backward)
                    previous-leaf
                  next-leaf))

               ;; Point is at the start of a paragraph.
               ((eq (gethash :start tip) (point))
                (if (eq direction 'backward)
                    previous-leaf
                  tip))

               ;; Point is at the end of a paragraph.
               ((eq (gethash :end tip) (point))
                (if (eq direction 'backward)
                    tip
                  next-leaf))

               ;; Point is at the middle of a paragraph.
               (t tip)))
        (if (eq direction 'backward)
            (if target-paragraph
                (progn
                  (goto-char (gethash :start target-paragraph))
                  t)
              (goto-char start)
              (kotlin-mode--fill-skip-paragraph-in-code direction))
          (if target-paragraph
              (progn
                (goto-char (gethash :end target-paragraph))
                (when (eq (char-after) ?\n)
                  (forward-char))
                (skip-chars-backward "\s\t")
                t)
            (goto-char end)
            (kotlin-mode--fill-skip-paragraph-in-code direction)))))))

(defun kotlin-mode--kdoc-paragraphs-around-point ()
  "Return KDoc paragraphs around the point.

If no KDoc is under the point, return nil.

Otherwise, return a list (TIP PREVIOUS NEXT) where TIP is the paragraph under
the point if any, PREVIOUS is the preceding paragraph if any, and the NEXT is
 the following paragraph if any."
  (let* ((chunk (kotlin-mode--chunk-after-spaces))
         (comment-region (kotlin-mode--kdoc-comment-region chunk))
         (start (car comment-region))
         (end (cdr comment-region))
         (kdoc
          ;; FIXME cache
          (cond
           ((kotlin-mode--chunk-multiline-comment-p chunk)
            (kotlin-mode--parse-kdoc-in-multiline-comment start end))
           ((kotlin-mode--chunk-single-line-comment-p chunk)
            (kotlin-mode--parse-kdoc-in-single-line-comments start end))
           ((kotlin-mode--chunk-multiline-string-p chunk)
            (kotlin-mode--parse-kdoc-in-multiline-string start end))
           (t nil)))
         tip
         next-leaf
         previous-leaf)
    (if (null kdoc)
        nil
      (kotlin-mode--comment-node-walk
       kdoc
       (lambda (node event)
         (when (and (eq event 'visit)
                    (eq (gethash :type node) 'paragraph))
           (when (<= (gethash :end node) (point))
             (setq previous-leaf node))
           (when (and (<= (gethash :start node) (point))
                      (< (point) (gethash :end node)))
             (setq tip node))
           (when (and (< (point) (gethash :start node))
                      (null next-leaf))
             (setq next-leaf node)))))
      (list tip previous-leaf next-leaf))))

(defun kotlin-mode--fill-skip-paragraph-in-code (direction)
  "Find next paragraph to fill.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Search a line that is a part of a comment, then search a paragraph in it.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (let ((pos (point))
        chunk)
    (if (eq direction 'backward)
        (progn
          (while (and
                  (zerop (forward-line -1))
                  (progn
                    (setq chunk (kotlin-mode--chunk-after-spaces))
                    (unless (kotlin-mode--chunk-comment-p chunk)
                      (setq chunk nil))
                    (null chunk)))
            t)
          (if chunk
              (progn
                (goto-char (kotlin-mode--chunk-end chunk))
                (cond
                 ((kotlin-mode--chunk-multiline-comment-p chunk)
                  (backward-char 2))
                 ((kotlin-mode--chunk-single-line-comment-p chunk)
                  (backward-char)))
                (kotlin-mode--fill-skip-paragraph-1 direction))
            ;; To prevent `fill-region' filling code when no comments found.
            (goto-char pos)))
      (while (and
              (not (eobp))
              (progn
                (setq chunk (kotlin-mode--chunk-after-spaces))
                (unless (kotlin-mode--chunk-comment-p chunk)
                  (setq chunk nil))
                (null chunk)))
        (forward-line 1))
      (when chunk
        (goto-char (kotlin-mode--chunk-start chunk))
        (kotlin-mode--fill-skip-paragraph-1 direction)))))

(defun kotlin-mode--do-auto-fill ()
  "Do auto fill at point.

Do nothing except in a comment or multiline.

If point is inside a multiline style comment (slash-star style comment) which
is actually in single line, insert line breaks before and after the contents,
then call `do-auto-fill'.

Example:

/* aaa bbb ccc */
↓
/*
 * aaa bbb
 * ccc
 */"
  (let ((current-fill-column (current-fill-column))
        (current-justification (current-justification))
        chunk
        comment-start-pos
        comment-end-pos
        fill-prefix)
    (when (and current-justification
               fill-column
               (not (and (eq current-justification 'left)
                         (<= (current-column) current-fill-column)))
               (setq chunk (kotlin-mode--chunk-after))
               (kotlin-mode--chunk-comment-p chunk))
      (when (kotlin-mode--chunk-multiline-comment-p chunk)
        (setq comment-start-pos (kotlin-mode--chunk-start chunk))
        (setq comment-end-pos (kotlin-mode--chunk-end chunk))
        (when (kotlin-mode--same-line-p comment-start-pos comment-end-pos)
          (save-excursion
            (goto-char comment-end-pos)
            (when (and (<= (+ comment-start-pos 2)
                           (- comment-end-pos 2))
                       (eq (char-before) ?/)
                       (eq (char-before (1- (point))) ?*))
              (backward-char)
              (skip-chars-backward "*")
              (skip-syntax-backward " ")
              (delete-horizontal-space)
              (insert-and-inherit ?\n))
            (indent-according-to-mode))
          (save-excursion
            (goto-char comment-start-pos)
            (forward-char)
            (skip-chars-forward "*")
            (delete-horizontal-space)
            (insert-and-inherit ?\n)
            (when kotlin-mode-prepend-asterisk-to-comment-line
              (insert ?*)
              (when kotlin-mode-insert-space-after-asterisk-in-comment
                (insert-and-inherit ?\s)))
            (indent-according-to-mode))))
      (setq fill-prefix (kotlin-mode--adaptive-fill))
      (do-auto-fill))))

(provide 'kotlin-mode-fill)

;;; kotlin-mode-fill.el ends here
