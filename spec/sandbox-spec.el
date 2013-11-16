(sandbox-test "sandbox returns nil when passed the empty list"
  (should (null (sandbox '()))))


(sandbox-test "sandbox rewrites any function as a function with a prefix"
  (should (equal (sandbox '(hi t))
                 '(elisp-sandbox-unsafe-env-hi t))))

(sandbox-test "sandbox allows t, nil, &rest, &optional..."
  (should (equal (sandbox '(t nil &rest &optional))
                 '(t nil &rest &optional))))

(sandbox-test "sandbox passes a quoted form along as quoted"
  ;; should this be a preference?
  (should (equal (sandbox ''(hi t))
                 (quote (quote (hi t))))))

(sandbox-test "sandbox--check-args is true for an empty list"
  (should (equal t (sandbox--check-args nil))))

(sandbox-test "sandbox--check-args is true for a list with a single symbol"
  (should (equal t (sandbox--check-args '(wtf)))))

(sandbox-test "sandbox--check-args is true for a list with multiple symbols"
  (should (equal t (sandbox--check-args '(wtf omg)))))

(sandbox-test "sandbox--check-args is false for a list with symbols where one is bound"
  (let ((omg 10))
    (should (sandbox--check-args '(wtf omg)))))

(sandbox-test "sandbox--safe-length-args-p is true for a small list"
  (should (sandbox--safe-length-args-p '(1 2 3) 0 100)))

(sandbox-test "sandbox--safe-length-args-p is false if the list is too long"
  (should-not (sandbox--safe-length-args-p '(1 2 3) 0 1)))

(sandbox-test "sandbox--safe-length-args-p should count sub-lists as lists"
  (should (sandbox--safe-length-args-p '((2 3) 2 5 6 7 ) 0 4))
  (should-not (sandbox--safe-length-args-p '((2 3) 2) 0 2)))

(sandbox-test "elisp-sandbox-defun makes functions in the sandboxed namespace"
  (progn
    (elisp-sandbox-defun testfn (one two) (+ one two))
    (should (eq 3 (elisp-sandbox-unsafe-env-testfn 1 2)))
    ))

(sandbox-test "sandbox-defun handles functions with docstrings too"
  (progn
    (elisp-sandbox-defun testfn (one two) "test function" (+ one two))
    (should (eq 3 (elisp-sandbox-unsafe-env-testfn 1 2)))))

(sandbox-test "sandbox-while wont allow infinite looping"
  (should-error
   (let ((i 0))
     (sandbox-while t (incf i)))))

(sandbox-test "sandbox-eval will eval defuns in a different namespace"
  ;; todo: make a test scenario that will actually pass
    (should (eq 3


                (elisp-sandbox-eval
                 '(progn (defun testfn (one two) (+ one two))
                         (testfn 1 2)))

                )))

(sandbox-test "sandbox forbids the user from executing the bad stuff"
  (with-mock2
    (defmock a-sensitive-function ())
    (should-error
     (eval (sandbox '(a-sensitive-function)))

     :type 'void-function)
    (should (= 0 (el-spec:called-count 'a-sensitive-function)))))


(sandbox-test "allows the user to execute the good stuff"
  (with-mock2
    (defmock elisp-sandbox-unsafe-env-not-sensitive ())
    (eval (sandbox '(not-sensitive)))
    (should (= 1 (el-spec:called-count 'elisp-sandbox-unsafe-env-not-sensitive)))))

(sandbox-test "an infinite loop condition cant allow looping, i guess"
  (should-error
   (eval (sandbox '(while t
                     (throw 'omg-should-not-even-be-allowed-to-run!!!)))))
  :type 'void-function)

(sandbox-test "an infinite loop condition wont loop forever with sandbox-eval"
  (should-error
   (sandbox-eval (sandbox '(while t (setq what-doing "looping"))))))


(sandbox-test "user trying to access an outside variable doesnt work"
  (let ((a-secret 'shhhhh))
    (should-error
     (eval (sandbox '(message a-secret)))
     :type 'void-variable)))


