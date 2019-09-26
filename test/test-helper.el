;;; test-helper --- Test helper for watchman

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar watchman-test-path
  (f-dirname (f-this-file)))

(defvar watchman-root-path
  (f-parent watchman-test-path))

(defvar watchman-sandbox-path
  (f-expand "sandbox" watchman-test-path))

(when (f-exists? watchman-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" watchman-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory watchman-sandbox-path))
     (when (f-exists? watchman-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir watchman-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))

;; Undercover setup
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))

(require 'watchman)

;;; test-helper.el ends here
