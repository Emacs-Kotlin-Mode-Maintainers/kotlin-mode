;;; kotlin-mode.el --- Major mode for kotlin
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Keywords: languages

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

(defcustom kotlin-mode-hook nil
  "Hook run after entering `kotlin-mode'."
  :type 'hook
  :group 'kotlin
  )


(defvar kotlin-mode-map (make-sparse-keymap)
  "Keymap used by `kotlin-mode'."
  )


(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."
  :group 'kotlin
  )

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
