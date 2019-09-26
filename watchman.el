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

(defcustom watchman-make-executable (executable-find "watchman-make")
  "Watchman make executable"
  :group 'watchman
  :type 'string)

(defvar watchman--selected-watch nil)

(defun watchman--cmd (&rest cmds)
  "Execute a watchman subcommand CMDS using `watchman-executable'."
  (shell-command-to-string (s-join " " (append (list watchman-executable) cmds))))

(defun watchman--watch-list ()
  "Get the list of watches under watchman."
  (append (cdr (assoc 'roots (json-read-from-string (watchman--cmd "watch-list")))) nil))

(defun watchman--trigger-list (watch)
  "Get the list of triggers for WATCH."
  (append (cdr (assoc 'triggers (json-read-from-string (watchman--cmd "trigger-list" watch)))) nil))

(defun watchman--trigger-menu-entries (watch)
  "Return the triggers on the WATCH in the format required by `tabulated-list-entries'."
  (-map #'(lambda (trigger)
            (message "trigger: %s" trigger)
            (message "name: %s" (cdr (assoc 'name trigger)))
            (message "command: %s" (s-join " " (cdr (assoc 'command trigger))))
            (message "expression: %s" (format "%s" (cdr (assoc 'expression trigger))))
            (let ((name (cdr (assoc 'name trigger)))
                  (cmd (s-join " " (cdr (assoc 'command trigger))))
                  (expr (format "%s" (cdr (assoc 'expression trigger)))))
              (message "entries: %s" (list nil name cmd expr))
              (list name (vector name cmd expr))))
        (watchman--trigger-list watch)))

(defun watchman--trigger-list-button (&optional button)
  "Action invoke by a button BUTTON to move to the watchman trigger list."
  (watchman-trigger-list (tabulated-list-get-id)))

(defun watchman--watch-menu-entries ()
  "Return watchman's watches in the format required by `tabulated-list-entries'."
  (-map #'(lambda (watch)
            (message "watch: %s" watch)
            (list watch (vector (list watch 'action 'watchman--trigger-list-button))))
        (watchman--watch-list)))

(define-derived-mode watchman-watch-menu-mode tabulated-list-mode "Watch menu"
  "Special mode for viewing and interacting with watchman's watches."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Root" 18 t)])
  (setq tabulated-list-entries (watchman--watch-menu-entries))
  (message "entries: %s" tabulated-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))


(define-derived-mode watchman-trigger-menu-mode tabulated-list-mode "Trigger menu"
  "special mode for viewing and interacting with watchman triggers."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Name" 12 t) ("Command" 12 t) ("Trigger" 12 t)])
  (setq tabulated-list-entries '(lambda () (watchman--trigger-menu-entries watchman--selected-watch)))
  (message "entries: %s" tabulated-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

(defun watchman-watch-list ()
  "View watchman's watches as a table."
  (interactive)
  (let ((buf (get-buffer-create "*Watchman watches*")))
    (with-current-buffer buf
      (setq buffer-file-coding-system 'utf8)
      (watchman-watch-menu-mode))
    (switch-to-buffer buf)))

(defun watchman-trigger-list (watch)
  "View watchman's triggers for a given WATCH."
  (interactive "DWatch: ")
  (let ((buf (get-buffer-create (format "*Watchman triggers: %s" watch))))
    (setq watchman--selected-watch watch)
    (message "selected watch: %s" watchman--selected-watch)
    (with-current-buffer buf
      (setq buffer-file-coding-system 'utf8)
      (watchman-trigger-menu-mode))
    (switch-to-buffer buf)))

(defun watchman-make (pattern task)
  "Run watchman-make with the given PATTERN and TASK."
  (interactive "sPattern: \nsTask: ")
  (let ((cmd (format "%s -p '%s' -t %s"
                   (shell-quote-argument watchman-make-executable)
                   (shell-quote-wildcard-pattern pattern)
                   (shell-quote-argument task)
                   )
             ))
    (compile cmd t)))

(provide 'watchman)
;;; watchman.el ends here
