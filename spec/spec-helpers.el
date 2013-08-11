
(add-to-list 'load-path
             (expand-file-name
              (concat (file-name-directory (or load-file-name (buffer-file-name))) "../")))

(require 'elisp-sandbox)

(defun sandbox-run-sandbox-tests ()
  (interactive)
  (ert-run-tests-interactively '(tag elisp-sandbox)))

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
         (test-name (sandbox--testing-symbol-from-string docstring))
         (tags (if (boundp 'additional-tags)
                   (concat additional-tags '(elisp-sandbox))
                 '(elisp-sandbox))))

    `(let (the-test
           sandbox-after-test-hook)
       (setq the-test
             (ert-deftest ,test-name ()
               ,docstring
               :tags ',tags               (unwind-protect
                   ,@body
                 (run-hooks 'sandbox-after-test-hook))))
       (when sandbox-test-run-on-eval
         (ert-run-tests-interactively the-test))
       the-test)))


(defvar elisp-sandbox-readme-text "")

(defun readme (arg)
  (setq elisp-sandbox-readme-text (concat elisp-sandbox-readme-text arg)))

(defun readme-start ()
  (setq elisp-sandbox-readme-text ""))

(defun readme-write ()
  "writes currently defined readme contents to README.creole"
  (interactive)
  (with-temp-file "README.creole"
    (erase-buffer)
    (insert elisp-sandbox-readme-text))
  (setq elisp-sandbox-readme-text ""))

(defmacro sandbox-defexample (description raw expected &optional output-should-equal)
  `(progn
     (sandbox-test ,description
       (let ((elisp-sandbox-evaluation-output nil))
         (should (equal (eval ',raw)
                        ',expected))
         (should (equal elisp-sandbox-evaluation-output
                        ',output-should-equal))))
     (readme (format "
{{{
%s
}}}

//=>//

{{{
%s
}}}
" (pp ',raw) (pp ',expected)))))





(defun readme-test ()
  (interactive)
  (ert-run-tests-interactively '(or (tag elisp-sandbox)
                                    (tag readme))))
