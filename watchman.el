;;; watchman.el --- Interact with watchman from within emacs.

;; The MIT License (MIT)

;; Copyright (c) 2019 nick96

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Nick Spain <nicholas.spain96@gmail.com>
;; Version: 0.0.1
;; Keywords: Productivity
;; Homepage: https://github.com/nick96/watchman.el
;; Package-Requires: ()

;;; Commentary:

;; Create, inspect, change and delete watchman watches and triggers easily from
;; within Emacs. This package is intended to make basic usage of watchman easy.

;;; Code:

(require 'json)
(require 's)
(require 'tabulated-list)

(defgroup watchman nil
  "Create, inspect, change and delete watchman watches and triggers"
  :prefix "watchman"
  :group 'tools)

(defcustom watchman-executable (executable-find "watchman")
  "Watchman executable"
  :group 'watchman
  :type 'string)

(defun watchman--cmd (cmd)
  "Execute a watchman subcommand CMD using `watchman-executable'."
  (shell-command-to-string (s-join " " (list watchman-executable cmd))))

(defun watchman--watch-list ()
  "Call watchman's watch-list subcommand and parse the JSON response."
  (append (cdr (assoc 'roots (json-read-from-string (watchman--cmd "watch-list")))) nil))

(define-derived-mode watchman-watch-menu-mode tabulated-list-mode "Watch menu"
  "Special mode for viewing and interacting with watchman's watches."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Root" 18 t)])
  (setq tabulated-list-entries '(lambda () (-map #'(lambda (x) (list nil (vector x nil))) (watchman--watch-list))))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun watchman-watch-list ()
  "View watchman's watches as a table."
  (interactive)
  (let ((buf (get-buffer-create "*Watchman watches*")))
    (with-current-buffer buf
      (message "setting file coding system")
      (setq buffer-file-coding-system 'utf8)
      (message "entering watchman-watch-menu-mode")
      (watchman-watch-menu-mode))
    (switch-to-buffer buf)))

(provide 'watchman)
;;; watchman.el ends here
