(require 'kotlin-mode)

(ert-deftest kotlin-mode--top-level-indent-test ()
  (with-temp-buffer
    (let ((text "package com.gregghz.emacs

import java.util.*
import foo.Bar
import bar.Bar as bBar
"))
      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string))))))

(ert-deftest kotlin-mode--single-level-indent-test ()
  (with-temp-buffer
    (let ((text "fun sum(a: Int, b: Int): Int {
return a + b
}"))

      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)
      (forward-line)

      (kotlin-mode--indent-line)
      (should (equal (buffer-string) "fun sum(a: Int, b: Int): Int {
    return a + b
}")))))

(ert-deftest kotlin-mode--lambda-body-indent-test ()
  (with-temp-buffer
    (let ((text "fun test(args: Array<String>) {
args.forEach(arg ->
println(arg)
)
"))
      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (forward-line)
      (kotlin-mode--indent-line)
      (forward-line)
      (kotlin-mode--indent-line)
      (forward-line)
      (kotlin-mode--indent-line)

      (should (equal (buffer-string) "fun test(args: Array<String>) {
    args.forEach(arg ->
        println(arg)
    )
"))
      )))

(ert-deftest kotlin-mode--chained-methods ()
  (with-temp-buffer
    (let ((text "names.filter { it.empty }
.sortedBy { it }
.map { it.toUpperCase() }
.forEach { print(it) }"))

      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (kotlin-mode--indent-line)

      (forward-line)
      (kotlin-mode--indent-line)

      (forward-line)
      (kotlin-mode--indent-line)

      (forward-line)
      (kotlin-mode--indent-line)

      (should (equal (buffer-string) "names.filter { it.empty }
    .sortedBy { it }
    .map { it.toUpperCase() }
    .forEach { print(it) }")))))

(ert-deftest kotlin-mode--ignore-comment-test ()
  (with-temp-buffer
    (let ((text "fun foo {
    bar()
    // }
    bar()
}"))
      (pop-to-buffer (current-buffer))
      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string))))))

(ert-deftest kotlin-mode--indent-comment-at-bob--test ()
  (with-temp-buffer
    (let ((text "/*
 *
 *
 */"))
      (pop-to-buffer (current-buffer))
      (insert text)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (forward-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string))))))

(defun next-non-empty-line ()
  "Moves to the next non-empty line"
  (forward-line)
  (while (and (looking-at "^[ \t]*$") (not (eobp)))
    (forward-line)))

(ert-deftest kotlin-mode--sample-test ()
  (with-temp-buffer
    (insert-file-contents "test/sample.kt")
    (goto-char (point-min))
    (kotlin-mode)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (setq-local kotlin-tab-width 4)
    (while (not (eobp))
      (let ((expected-line (thing-at-point 'line)))

        ;; Remove existing indentation
        (beginning-of-line)
        (delete-region (point) (progn (skip-chars-forward " \t") (point)))

        ;; Indent the line
        (kotlin-mode--indent-line)

        ;; Check that the correct indentation is re-applied
        (should (equal expected-line (thing-at-point 'line)))

        ;; Go to the next non-empty line
        (next-non-empty-line)))))
