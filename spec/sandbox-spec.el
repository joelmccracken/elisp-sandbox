
;; "characterization tests"
(describe "sandbox"
  (it "returns nil when passed the empty list"
    (should (null (sandbox '()))))

  (it "rewrites any function as a function with a prefix"
    (should (equal (sandbox '(hi t))
                   '(emacs-sandbox-hi t))))

  (it "allows t, nil, &rest, &optional..."
    (should (equal (sandbox '(t nil &rest &optional))
                   '(t nil &rest &optional))))

  (it "passes a quoted form along as quoted"
    ;; should this be a preference?
    (should (equal (sandbox ''(hi t))
                   (quote (quote (hi t)))))))

(describe "sandbox--check-args"
  (it "is true for an empty list"
    (should (equal t (sandbox--check-args nil))))

  (it "is true for a list with a single symbol"
    (should (equal t (sandbox--check-args '(wtf)))))

  (it "is true for a list with multiple symbols"
    (should (equal t (sandbox--check-args '(wtf omg)))))

  (it "is false for a list with symbols where one is bound"
    (let ((omg 10))
      (should (sandbox--check-args '(wtf omg))))))


(describe "sandbox--safe-length-args-p"
  (it "is true for a small list"
    (should (sandbox--safe-length-args-p '(1 2 3) 0 100)))

  (it "is false if the list is too long"
    (should-not (sandbox--safe-length-args-p '(1 2 3) 0 1)))

  (it "should count sub-lists as lists"
    (should (sandbox--safe-length-args-p '((2 3) 2 5 6 7 ) 0 4))
    (should-not (sandbox--safe-length-args-p '((2 3) 2) 0 2))))

(describe "sandbox-defun"
  (it "makes "))

(describe "sandbox-while"
  (it "wont allow infinite looping"
    (should-error
     (let ((i 0))
       (sandbox-while t (incf i))))))

;; actual spec tests
(describe "sandbox"
  (it "forbids the user from executing the bad stuff"
    (with-mock2
      (defmock a-sensitive-function ())
      (should-error
       (eval (sandbox '(a-sensitive-function)))

       :type 'void-function)
      (should (= 0 (el-spec:called-count 'a-sensitive-function)))))

  (it "allows the user to execute the good stuff"
    (with-mock2
      (defmock emacs-sandbox-not-sensitive ())
      (eval (sandbox '(not-sensitive)))
                (should (= 1 (el-spec:called-count 'emacs-sandbox-not-sensitive))))))


(describe "an infinite loop condition"
  (it "cant allow looping, i guess"
    (should-error
     (eval (sandbox '(while t
                       (throw 'omg-should-not-even-be-allowed-to-run!!!)))))
    :type 'void-function))

(defun emacs-sandbox-message (val))


(describe "user trying to access an outside variable"
  (it "doesnt work"
    (let ((a-secret 'shhhhh))
      (should-error
       (eval (sandbox '(message a-secret)))

       :type 'void-variable
       )

      )
    ))

;;;;;;;;;;;;;;;;
;; need two types of specs to advance:
;; the first is examples of
;;
