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
  (dolist (filename '("test/sample.kt" "test/pathological.kt"))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (kotlin-mode)
      (setq-local indent-tabs-mode nil)
      (setq-local tab-width 4)
      (setq-local kotlin-tab-width 4)

      (while (not (eobp))
        (back-to-indentation)
        (let (;; (thing-at-point 'line) returns string with property.
              (expected-line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))
              actual-line
              (original-indent (current-column))
              (known-bug (looking-at ".*//.*KNOWN_BUG")))
          ;; Remove existing indentation, or indent to column 1 if
          ;; expected indentation is column 0.
          (if (= original-indent 0)
              (indent-line-to 1)
            (delete-horizontal-space))

          ;; Indent the line
          (kotlin-mode--indent-line)

          (setq actual-line (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))

          ;; Check that the correct indentation is re-applied
          (if known-bug
              (if (equal expected-line actual-line)
                  (message "%s:%s:info: KNOWN_BUG is fixed somehow"
                           filename
                           (line-number-at-pos))
                (back-to-indentation)
                (message "%s:%s:warn: (known bug) expected indentation to column %d but %d"
                         filename
                         (line-number-at-pos)
                         original-indent
                         (current-column)))
            (should
             (equal
              (format "%s:%s: %s" filename (line-number-at-pos) expected-line)
              (format "%s:%s: %s" filename (line-number-at-pos) actual-line))))

          ;; Restore to original indentation for KNOWN_BUG line.
          (indent-line-to original-indent)

          ;; Go to the next non-empty line
          (next-non-empty-line))))))
