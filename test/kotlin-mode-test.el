(load-file "kotlin-mode.el")

;(require 'kotlin-mode)

(ert-deftest kotlin-mode--top-level-indent-test ()
  (with-temp-buffer
    (let ((text "package com.gregghz.emacs

import java.util.*
import foo.Bar
import bar.Bar as bBar
"))
      (insert text)
      (beginning-of-buffer)
      (kotlin-mode--indent-line)

      (should (equal text (buffer-string)))

      (next-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (next-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (next-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string)))

      (next-line)
      (kotlin-mode--indent-line)
      (should (equal text (buffer-string))))))

(ert-deftest kotlin-mode--single-level-indent-test ()
  (with-temp-buffer
    (let ((text "fun sum(a: Int, b: Int): Int {
return a + b
}"))

      (insert text)
      (beginning-of-buffer)
      (next-line)

      (kotlin-mode--indent-line)
      (should (equal (buffer-string) "fun sum(a: Int, b: Int): Int {
	return a + b
}")))))
