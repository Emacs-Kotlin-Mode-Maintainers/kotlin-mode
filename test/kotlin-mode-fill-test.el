;;; kotlin-mode-fill-test.el --- Test for kotlin-mode: filling -*- lexical-binding: t -*-
;; Copyright (C) 2016, 2022, 2023 taku0, Josh Caswell

;; Authors: taku0 (https://github.com/taku0)
;;        Josh Caswell (https://github.com/woolsweater)

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

;; Test for kotlin-mode: paragraph fill
;;
;; Strategy:
;;
;; 1. Make sure `fill-region-as-paragraph' is correct by manually crafted test
;;   cases.
;;
;; 2. Generate test cases for `fill-region' and `fill-paragraph' using
;;    `fill-region-as-paragraph' and template files.

;;; Code:

(require 'kotlin-mode)
(require 'kotlin-mode-fill)
(require 'ert)

(defvar kotlin-mode--test-directory
  (file-name-directory
   (if (fboundp 'macroexp-file-name) (macroexp-file-name)
     (or load-file-name buffer-file-name))))

(defun kotlin-mode--test-fill-region-as-paragraph
    (input
     expected
     fill-column-for-test)
  "Run a test for `fill-region-as-paragraph'.

INPUT is a text before filling.  Characters \"{\" and \"}\" represents the
region and removed before filling.

EXPECTED is the expected result.

FILL-COLUMN-FOR-TEST is used for `fill-column'."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert input)
    (kotlin-mode)
    (syntax-propertize (point-max))
    (let ((fill-column fill-column-for-test)
          start
          end)
      (goto-char (point-min))
      (search-forward "{")
      (delete-char -1)
      (setq start (point))
      (search-forward "}")
      (delete-char -1)
      (setq end (point))
      (fill-region-as-paragraph start end))
    (should (string-equal
             (buffer-substring-no-properties (point-min) (point-max))
             expected))))

(ert-deftest fill-region-as-paragraph--sinle-line-comments--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// {abc def ghi}
"
   ;; Expected
   "
/// abc def
/// ghi
"
   12))

(ert-deftest fill-region-as-paragraph--sinle-line-comments--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// {abc
/// def
/// ghi}
"
   ;; Expected
   "
/// abc def
/// ghi
"
   12))

(ert-deftest fill-region-as-paragraph--multiline-comments--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/* {abc def ghi} */
"
   ;; Expected
   "
/*
 * abc def
 * ghi
 */
"
   12))

(ert-deftest fill-region-as-paragraph--multiline-comments--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/*
 * {abc
 * def
 * ghi}
 */
"
   ;; Expected
   "
/*
 * abc def
 * ghi
 */
"
   12))

(ert-deftest fill-region-as-paragraph--multiline-comments--to-one-line ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/*
 * {abc
 * def
 * ghi}
 */
"
   ;; Expected
   "
/* abc def ghi */
"
   80))

(ert-deftest fill-region-as-paragraph--keep-line-break-after-open-delimiter ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/*
 * {abc
 * def
 * ghi} */
"
   ;; Expected
   "
/*
 * abc def
 * ghi */
"
   12))

(ert-deftest fill-region-as-paragraph--keep-line-break-before-close-delimiter ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/* {abc
 * def
 * ghi}
 */
"
   ;; Expected
   "
/* abc def
 * ghi
 */
"
   12))

(ert-deftest fill-region-as-paragraph--block-quote--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// > {abc def ghi}
"
   ;; Expected
   "
/// > abc def
/// > ghi
"
   16))

(ert-deftest fill-region-as-paragraph--block-quote--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// > {abc
/// > def
/// > ghi}
"
   ;; Expected
   "
/// > abc def
/// > ghi
"
   16))

(ert-deftest fill-region-as-paragraph--unordered-list-hyphen--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// - {abc def ghi}
"
   ;; Expected
   "
/// - abc def
///   ghi
"
   16))

(ert-deftest fill-region-as-paragraph--unordered-list-hyphen--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// - {abc
///   def
///   ghi}
"
   ;; Expected
   "
/// - abc def
///   ghi
"
   16))

(ert-deftest fill-region-as-paragraph--unordered-list-plus--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// + {abc def ghi}
"
   ;; Expected
   "
/// + abc def
///   ghi
"
   16))

(ert-deftest fill-region-as-paragraph--unordered-list-plus--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// + {abc
///   def
///   ghi}
"
   ;; Expected
   "
/// + abc def
///   ghi
"
   16))

(ert-deftest fill-region-as-paragraph--unordered-list-asterisk--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/*
 * * {abc def ghi}
 */
"
   ;; Expected
   "
/*
 * * abc def
 *   ghi
 */
"
   12))

(ert-deftest fill-region-as-paragraph--unordered-list-asterisk--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/*
 * * {abc
 *   def
 *   ghi}
 */
"
   ;; Expected
   "
/*
 * * abc def
 *   ghi
 */
"
   12))

(ert-deftest fill-region-as-paragraph--ordered-list-dot--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// 1. {abc def ghi}
"
   ;; Expected
   "
/// 1. abc def
///    ghi
"
   16))

(ert-deftest fill-region-as-paragraph--ordered-list-dot--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// 1. {abc
///    def
///    ghi}
"
   ;; Expected
   "
/// 1. abc def
///    ghi
"
   16))

(ert-deftest fill-region-as-paragraph--ordered-list-paren--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// 1) {abc def ghi}
"
   ;; Expected
   "
/// 1) abc def
///    ghi
"
   16))

(ert-deftest fill-region-as-paragraph--ordered-list-paren--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/// 1) {abc
///    def
///    ghi}
"
   ;; Expected
   "
/// 1) abc def
///    ghi
"
   16))

(ert-deftest fill-region-as-paragraph--comment-tag--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/**
 * @param test {abc def ghi}
 */
"
   ;; Expected
   "
/**
 * @param test abc
 *     def ghi
 */
"
   16))

(ert-deftest fill-region-as-paragraph--comment-tag--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
/**
 * @param test {abc
 *     def
 *     ghi}
 */
"
   ;; Expected
   "
/**
 * @param test abc
 *     def ghi
 */
"
   16))

(ert-deftest fill-region-as-paragraph--complex-prefix--insert-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
// *  >>  -  >10.  2)   {abc def ghi}
"
   ;; Expected
   "
// *  >>  -  >10.  2)   abc def
//    >>     >          ghi
"
   32))

(ert-deftest fill-region-as-paragraph--complex-prefix--delete-breaks ()
  (kotlin-mode--test-fill-region-as-paragraph
   ;; Input
   "
// *  >>  -  >10.  2)   {abc
//    >>     >          def
//    >>     >          ghi}
"
   ;; Expected
   "
// *  >>  -  >10.  2)   abc def
//    >>     >          ghi
"
   32))

(defun kotlin-mode--parse-fill-test ()
  "Parse the current buffer as a test file and return its structure.

The result is list of elements, which is one of:

- non-paragraph line, (literal STRING), where STRING is the line excluding a
  line break,

- paragraph, (paragraph PREFIX), where PREFIX is the prefix before the
  paragraph."
  (save-excursion
    (goto-char (point-min))
    (let ((result ()))
      (while (not (eobp))
        (push (if (looking-at "\\(.*\\)kotlin-mode--test-paragraph")
                  (list 'paragraph (match-string-no-properties 1))
                (list 'literal
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
              result)
        (forward-line))
      (reverse result))))

(defun kotlin-mode--test-fill-region (lines mode)
  "Define tests for `fill-region'.

LINES is the parsed lines of the test file.
See `kotlin-mode--parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines."
  (let (regions
        expected
        actual)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (kotlin-mode)
      (setq regions (kotlin-mode--insert-fill-test-lines lines mode))
      (syntax-propertize (point-max))
      (dolist (region (reverse regions))
        (fill-region-as-paragraph (nth 0 region) (nth 1 region)))
      (setq expected (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (kotlin-mode)
      (kotlin-mode--insert-fill-test-lines lines mode)
      (syntax-propertize (point-max))
      (fill-region (point-min) (point-max))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string-equal expected actual))))

(defun kotlin-mode--test-fill-paragraph (lines mode)
  "Run tests for `fill-paragraph'.

LINES is the parsed lines of the test file.
See `kotlin-mode--parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines."
  (let (regions
        original
        expected
        (point-placements '(beginning-of-line
                            after-indent
                            beginning-of-region
                            end-of-line
                            end-of-region)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (kotlin-mode)
      (setq regions (kotlin-mode--insert-fill-test-lines lines mode))
      (setq original (buffer-string))
      (dolist (region regions)
        (delete-region (point-min) (point-max))
        (insert original)
        (syntax-propertize (point-max))
        (fill-region-as-paragraph (nth 0 region) (nth 1 region))
        (setq expected (buffer-substring-no-properties (point-min) (point-max)))
        (dolist (point-placement point-placements)
          (kotlin-mode--do-test-fill-paragraph
           original
           expected
           region
           point-placement))))))

(defun kotlin-mode--do-test-fill-paragraph (original
                                            expected
                                            region
                                            point-placement)
  "Run single test for `fill-paragraph'.

ORIGINAL is a text before filling.

EXPECTED is the expected result.

REGION is the region of the paragraph.

POINT-PLACEMENT designates where to put the point before filling.  It must be
one of the following:
- `beginning-of-line'
- `after-indent'
- `beginning-of-region'
- `end-of-line'
- `end-of-region'"
  (let (actual)
    (delete-region (point-min) (point-max))
    (insert original)
    (syntax-propertize (point-max))
    (cond
     ((eq point-placement 'beginning-of-line)
      (goto-char (nth 0 region))
      (forward-line 0))
     ((eq point-placement 'after-indent)
      (goto-char (nth 0 region))
      (back-to-indentation))
     ((eq point-placement 'beginning-of-region)
      (goto-char (nth 0 region)))
     ((eq point-placement 'end-of-line)
      (goto-char (nth 0 region))
      (end-of-line))
     ((eq point-placement 'end-of-region)
      (goto-char (nth 1 region))))
    (fill-paragraph)
    (setq actual (buffer-substring-no-properties (point-min) (point-max)))
    (should (string-equal expected actual))))

(defun kotlin-mode--insert-fill-test-lines (lines mode)
  "Insert parsed lines at point.

LINES is the parsed lines of the test file.
See `kotlin-mode--parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines.

Return regions of paragraphs"
  (let (regions
        start)
    (dolist (line lines)
      (cond
       ((eq (car line) 'literal)
        (insert (nth 1 line) "\n"))
       ((eq (car line) 'paragraph)
        (setq start (kotlin-mode--insert-fill-test-paragraph (nth 1 line) mode))
        (push (list start (1- (point))) regions))))
    (reverse regions)))

(defun kotlin-mode--insert-fill-test-paragraph (prefix mode)
  "Insert a test paragraph at point.

PREFIX is inserted before the paragraph.

If MODE is `join', insert short multiple lines.  If MODE is `break', insert a
long line.

Return point after prefix."
  (let (start
        column-after-first-word)
    (insert prefix)
    (setq start (point))
    (insert "aaa ")
    (setq column-after-first-word (current-column))
    (dotimes (_ 100)
      (insert "aaa "))
    (insert "\n")
    (when (eq mode 'join)
      (let ((fill-column column-after-first-word))
        (fill-region-as-paragraph start (1- (point)))))
    start))

(defmacro kotlin-mode--define-fill-tests ()
  "Define paragraph fill tests for `kotlin-mode' from Kotlin files."
  (let* ((basedir (file-name-directory kotlin-mode--test-directory))
         (default-directory (concat (file-name-as-directory basedir) "fill"))
         (lines (make-symbol "lines"))
         definitions
         name)
    (dolist (kotlin-file (file-expand-wildcards "*.kt"))
      (dolist (mode '(break join))
        (dolist (test '(kotlin-mode--test-fill-region
                        kotlin-mode--test-fill-paragraph))
          (setq name (concat
                      (substring (symbol-name test)
                                 (length "kotlin-mode--test-"))
                      "--"
                      (symbol-name mode)
                      "--"
                      (file-name-base kotlin-file)))
          (push `(ert-deftest ,(intern name) ()
                   (let* ((default-directory ,default-directory)
                          (,lines (with-temp-buffer
                                    (switch-to-buffer (current-buffer))
                                    (insert-file-contents-literally
                                     ,kotlin-file)
                                    (let ((coding-system-for-read 'utf-8))
                                      (decode-coding-inserted-region
                                       (point-min)
                                       (point-max)
                                       ,kotlin-file))
                                    (kotlin-mode)
                                    (syntax-propertize (point-max))
                                    (kotlin-mode--parse-fill-test))))
                     (,test ,lines ',mode)))
                definitions))))
    (cons 'progn (reverse definitions))))

(eval
 '(kotlin-mode--define-fill-tests))

(provide 'kotlin-mode-fill-test)

;;; kotlin-mode-fill-test.el ends here
