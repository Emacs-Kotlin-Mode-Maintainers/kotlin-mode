;;; kotlin-mode-kdoc-comment-parser-test.el --- Test for kotlin-mode: KDoc parser -*- lexical-binding: t -*-
;; Copyright (C) 2023 taku0

;; Authors: taku0 (https://github.com/taku0)

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

;; Test for kotlin-mode: KDoc parser.

;;; Code:


(require 'ert)
(require 'kotlin-mode-kdoc-comment-parser)
(require 'kotlin-mode-test)
(require 'json)
(require 'seq)

(defvar kotlin-mode--test-directory
  (file-name-directory
   (if (fboundp 'macroexp-file-name) (macroexp-file-name)
     (or load-file-name buffer-file-name))))

(ert-deftest kotlin-mode--test-kdoc-comment-parser ()
  "Test KDoc comment parser.

Parse examples from CommonMark spec and compare results with parse result of
commonmark.js.

Only node types and start/end lines are checked."
  (let* ((default-directory (file-name-directory kotlin-mode--test-directory))
         (json-object-type 'hash-table)
         (cases (gethash "cases" (json-read-file "commonmark_cases.json")))
         (replacements '(("indented-code-block" . "code_block")
                         ("fenced-code-block" . "code_block")
                         ("atx-heading" . "heading")
                         ("setext-heading" . "heading")
                         ("list-item" . "item")
                         ("block-quote" . "block_quote")
                         ("thematic-break" . "thematic_break")))
         tree
         source
         actual
         expected)
    (seq-doseq (case cases)
      (with-temp-buffer
        (setq tab-width 4)
        (setq source (gethash "source" case))
        (insert source)
        (setq tree (kotlin-mode--parse-kdoc-comment (point-min) (point-max)))
        (setq actual (kotlin-mode--comment-node-dump tree))
        ;; `string-replace' is introduced at Emacs 28.
        ;; (setq actual (string-replace (car replacement)
        ;;                              (cdr replacement)
        ;;                              actual))
        (with-temp-buffer
          (insert actual)
          (dolist (replacement replacements)
            (goto-char (point-min))
            (while (search-forward (car replacement) nil t)
              (replace-match (cdr replacement) nil t)))
          (setq actual
                (buffer-substring-no-properties (point-min) (point-max))))
        (setq expected (gethash "expected" case))
        (unless (or
                 ;; HTML blocks are not supported yet.
                 (string-match-p "html" expected)
                 ;; Link reference definitions are not supported yet.
                 (string-match-p "]:" source))
          (if (and kotlin-mode--test-keep-going
                   (not (string= actual expected)))
              (message (concat "source:\n"
                               "````````````````````````````````\n"
                               source
                               "````````````````````````````````\n\n"
                               "expected:\n"
                               "````````````````````````````````\n"
                               expected
                               "\n````````````````````````````````\n\n"
                               "actual:\n"
                               "````````````````````````````````\n"
                               actual
                               "\n````````````````````````````````\n\n"))
            (should (equal (concat
                            "source:\n"
                            "````````````````````````````````\n"
                            source
                            "````````````````````````````````\n\n"
                            "parsed:\n"
                            "````````````````````````````````\n"
                            expected)
                           (concat
                            "source:\n"
                            "````````````````````````````````\n"
                            source
                            "````````````````````````````````\n\n"
                            "parsed:\n"
                            "````````````````````````````````\n"
                            actual)))))))))

(provide 'kotlin-mode-kdoc-comment-parser-test)

;;; kotlin-mode-kdoc-comment-parser-test.el ends here
