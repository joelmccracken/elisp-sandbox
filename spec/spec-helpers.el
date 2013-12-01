
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

(defmacro sandbox-defexample (description raw &rest expectations)
  (let ((expected-return (cadr (member :return expectations)))
        (expected-messages (cadr (member :*Messages* expectations)))
        (expected-error (cadr (member :error expectations))))
    `(progn
       (let ((expected-return ',expected-return)
             (expected-messages ',expected-messages)
             (expected-error ',expected-error)
             (raw ',raw))
         (sandbox-test ,description
           (let* ((evaluation-results (eval raw))

                  (evaluated-return (cdr (assoc :return results)))
                  (evaluated-messages (cdr (assoc :*Messages* results)))
                  (evaluated-error (cdr (assoc :error results))))

             (should (equal evaluated-return expected-return))
             (should (equal evaluated-messages expected-messages))
             (should (equal evaluated-error expected-error))))

         (sandbox-defexample--readme-doc raw
                                         expected-return
                                         expected-messages
                                         expected-error))
       )))


(defun sandbox-defexample--readme-doc (raw return messages error)
  (let ((return-str (if return (format "return: %s \n" (pp return)) ""))
        (messages-str (if return (format "messages: %s \n" (pp messages)) ""))
        (error-str (if return (format "error: %s \n" (pp error)) "")))
    (readme (format "
{{{
%s
}}}

//=>//

{{{
%s
}}}
"
                    (pp raw)
                    (concat return-str messages-str error-str)
))))



(defun readme-test ()
  (interactive)
  (ert-run-tests-interactively '(or (tag elisp-sandbox)
                                    (tag readme))))
