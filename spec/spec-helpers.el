(defun sandbox-run-sandbox-tests ()
  (interactive)
  (ert-run-tests-interactively '(tag sandbox)))


(defvar sandbox-test-run-on-eval nil "should evaling a test definition run it immediately")
(setq sandbox-test-run-on-eval nil)
(defun sandbox-test--toggle-run-on-eval ()
  (interactive)
  (setq sandbox-test-run-on-eval (not sandbox-test-run-on-eval)))

(defun sandbox--testing-symbol-from-string (str)
  (intern (replace-regexp-in-string "[^a-z0-9]" "-" str)))


(defmacro sandbox-test (docstring &rest body)
  (declare (indent defun))
  (let* ((docstring (eval docstring))
         (test-name (sandbox--testing-symbol-from-string docstring)))
    `(let (the-test
           sandbox-after-test-hook)
       (setq the-test
             (ert-deftest ,test-name ()
               ,docstring
               :tags '(sandbox)
               (unwind-protect
                   ,@body
                 (run-hooks 'sandbox-after-test-hook))))
       (when sandbox-test-run-on-eval
         (ert-run-tests-interactively the-test))
       the-test)))


