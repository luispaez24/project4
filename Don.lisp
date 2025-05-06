;; Return the bi-implication (if and only if) of a and b

;;

;; Examples:

;;  (boolean-iff t nil) => nil

;;  (boolean-iff nil nil) => t

(defun boolean-iff (a b)
  (or (and a b)
      (and (not a) (not b)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate a boolean expression.

;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.

;;

;; Examples:

;;  (boolean-eval '(and t nil)) => nil

;;  (boolean-eval '(and t (or nil t)) => t

(defun boolean-eval (exp)
  (cond
    ((or (equal exp t) (equal exp nil)) exp)
    ((equal (car exp) 'not)
     (not (boolean-eval (second exp))))
    ((equal (car exp) 'and)
     (let ((args (cdr exp)))
       (cond ((null args) t)
             ((equal (boolean-eval (car args)) nil) nil)
             (t (boolean-eval (cons 'and (cdr args)))))))
    ((equal (car exp) 'or)
     (let ((args (cdr exp)))
       (cond ((null args) nil)
             ((equal (boolean-eval (car args)) t) t)
             (t (boolean-eval (cons 'or (cdr args)))))))
    ((equal (car exp) 'xor)
     (let ((a (boolean-eval (second exp)))
           (b (boolean-eval (third exp))))
       (or (and a (not b))
           (and (not a) b))))
    ((equal (car exp) 'implies)
     (let ((a (boolean-eval (second exp)))
           (b (boolean-eval (third exp))))
       (or (not a) b)))
    ((equal (car exp) 'iff)
     (boolean-iff (boolean-eval (second exp))
                  (boolean-eval (third exp))))
    (t nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform merge sort on the lists.

;; Parameters:

;;   list: The list to sort

;;   predicate: A function to compare elements of the list

;;

;; Examples:

;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)

;;     (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0)

(defun merge-sort (list predicate)
  (labels ((merge-lists (left right)
             (cond
               ((null left) right)
               ((null right) left)
               ((funcall predicate (car left) (car right))
                (cons (car left) (merge-lists (cdr left) right)))
               (t
                (cons (car right) (merge-lists left (cdr right)))))))
    (if (or (null list) (null (cdr list)))
        list
        (let* ((mid (floor (length list) 2))
               (left (subseq list 0 mid))
               (right (subseq list mid)))
          (merge-lists (merge-sort left predicate)
                       (merge-sort right predicate))))))


(defun run-boolean-eval-tests ()
  (format t "~%Running boolean-eval tests...~%~%")
  
  (let ((test-cases
         '((("Simple AND" (boolean-eval '(and t t))) . t)
           (("AND with false" (boolean-eval '(and t nil))) . nil)
           (("Nested OR" (boolean-eval '(or nil (or nil t)))) . t)
           (("NOT true" (boolean-eval '(not t))) . nil)
           (("NOT false" (boolean-eval '(not nil))) . t)
           (("XOR true/false" (boolean-eval '(xor t nil))) . t)
           (("XOR false/false" (boolean-eval '(xor nil nil))) . nil)
           (("IMPLIES t -> nil" (boolean-eval '(implies t nil))) . nil)
           (("IMPLIES nil -> t" (boolean-eval '(implies nil t))) . t)
           (("IFF t t" (boolean-eval '(iff t t))) . t)
           (("IFF t nil" (boolean-eval '(iff t nil))) . nil)
           (("Complex expr" (boolean-eval '(and (or nil t) (not nil)))) . t)
           
           ;; Advanced/Harder test cases
           (("Deep nesting" 
             (boolean-eval '(and (or nil (not (not t))) (implies t (or nil t))))) 
            . t)

           (("XOR inside AND" 
             (boolean-eval '(and (xor t nil) (xor nil nil)))) 
            . nil)

           (("IFF with nested expressions" 
             (boolean-eval '(iff (and t t) (or nil t)))) 
            . t)

           (("Implies false premise" 
             (boolean-eval '(implies (and nil t) t))) 
            . t))))
    
    (dolist (test test-cases)
      (let* ((label (caar test))
             (form (cadar test))
             (expected (cdr test))
             (result (eval form)))
        (format t "~A~%  Result:   ~A~%  Expected: ~A~%" label result expected)
        (if (equal result expected)
            (format t "  Test passed.~%~%")
            (format t "  Test failed.~%~%"))))))


(run-boolean-eval-tests)


(defun run-merge-sort-tests ()
  (format t "~%Running merge-sort tests...~%~%")

  (let ((test-cases
         '((("Ascending sort" (merge-sort '(3 1 4 1 5 9 2 6) #'<)) . (1 1 2 3 4 5 6 9))
           (("Descending sort" (merge-sort '(3 1 4 1 5 9 2 6) #'>)) . (9 6 5 4 3 2 1 1))
           (("Empty list" (merge-sort '() #'<)) . ())
           (("Single element" (merge-sort '(42) #'<)) . (42))
           (("All same" (merge-sort '(7 7 7 7) #'<)) . (7 7 7 7))
           (("Already sorted" (merge-sort '(1 2 3 4 5) #'<)) . (1 2 3 4 5))
           (("Reverse sorted" (merge-sort '(5 4 3 2 1) #'<)) . (1 2 3 4 5))
           (("With negative numbers" (merge-sort '(-3 -1 -7 2 0) #'<)) . (-7 -3 -1 0 2))
           (("With mixed signs" (merge-sort '(-1 0 1 -2 2) #'<)) . (-2 -1 0 1 2))
           (("Short descending" (merge-sort '(2 1) #'>)) . (2 1))
           (("Short ascending" (merge-sort '(2 1) #'<)) . (1 2))
           (("Duplicates in reverse" (merge-sort '(5 5 3 3 1 1) #'<)) . (1 1 3 3 5 5)))))
    
    (dolist (test test-cases)
      (let* ((label (caar test))
             (form (cadar test))
             (expected (cdr test))
             (result (eval form)))
        (format t "~A~%  Result:   ~A~%  Expected: ~A~%" label result expected)
        (if (equal result expected)
            (format t "  Test passed.~%~%")
            (format t "  Test failed.~%~%"))))))

(run-merge-sort-tests)
