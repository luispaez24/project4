;;; Only define assert-equal ONCE
(defun assert-equal (actual expected)
  (if (equal actual expected)
      (format t "PASS: ~a = ~a~%" actual expected)
      (format t "FAIL: ~a ≠ ~a~%" actual expected)))

;;; Boolean XOR
(defun boolean-xor (a b)
  (if (equal a b)
      nil   ; if both are the same, return NIL (false)
      t))

;;; Boolean implication
(defun boolean-implies (a b)
  (cond
    ((and a (not b))   ; A is true, B is false → implication fails
     nil)
    (t                 ; all other cases → implication holds
     t)))

;;; Test function for XOR
(defun test-boolean-xor ()
  (format t "~%--- Running boolean-xor tests ---~%")
  (assert-equal (boolean-xor t t) nil)
  (assert-equal (boolean-xor t nil) t)
  (assert-equal (boolean-xor nil t) t)
  (assert-equal (boolean-xor nil nil) nil))

;;; Test function for IMPLIES
(defun test-boolean-implies ()
  (format t "~%--- Running boolean-implies tests ---~%")
  (assert-equal (boolean-implies t nil) nil)
  (assert-equal (boolean-implies nil nil) t)
  (assert-equal (boolean-implies t t) t)
  (assert-equal (boolean-implies nil t) t))

;;; Run all tests
(defun run-all-tests ()
  (test-boolean-xor)
  (test-boolean-implies))

;;; Call the tests when running as a script
(run-all-tests)