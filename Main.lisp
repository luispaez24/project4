(load "Rory.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #1 Member of Set Function *****
(defun member-equal (item lst)
       (if (equal lst nil)
         nil
         (if (equal item (car lst))
           t
           (member-equal item (cdr lst)))))

(defun is-in? (item lst)
       (member-equal item lst))

(defun set-union (set-1 set-2)
       (labels ((helper (l1 l2 result)
                        (if (equal l1 nil)
                          (append (reverse l2) result)
                          (let ((item (car l1)))
                               (if (not (is-in? item result))
                                 (helper (cdr l1) l2 (cons item result))
                                 (helper (cdr l1) l2 result))))))
               (reverse (helper set-1 set-2 nil))))

;; Test print the set-union function
(format t "Union of (1 2) and (2 4): ~A~%" (set-union '(1 2) '(2 4)))

;; Call the set-member function from Rory.lisp and print the result
(format t "Is 3 a member of (1 3 5) using set-member? ~A~%" (set-member '(1 3 5) 3))
; (format t "Is 4 a member of (1 3 5) using set-member? ~A~%" (set-member '(1 3 5) 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #2 Union Function *****
(defun member-equal (item lst)
       (if (equal lst nil)
         nil
         (if (equal item (car lst))
           t
           (member-equal item (cdr lst)))))

(defun is-in? (item lst)
       (member-equal item lst))

(defun set-union (set-1 set-2)
       (labels ((helper (l1 l2 result)
                        (if (equal l1 nil)
                          (append (reverse l2) result)
                          (let ((item (car l1)))
                               (if (not (is-in? item result))
                                 (helper (cdr l1) l2 (cons item result))
                                 (helper (cdr l1) l2 result))))))
               (reverse (helper set-1 set-2 nil))))

;; Test print the function
(format t "Union ~A~%" (set-union '(1 2) '(2 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #3 Intersection Function ***** 
(defun member-equal (item lst)
  (if (equal lst nil)
      nil
      (if (equal item (car lst))
          t
          (member-equal item (cdr lst)))))

(defun is-in? (item lst)
  (member-equal item lst))

;; Reverse function to fix format for running test cases
(defun reverse-list (lst)
  (labels ((reverse-helper (remaining result)
             (if (equal remaining nil)
                 result
                 (reverse-helper (cdr remaining) (cons (car remaining) result)))))
    (reverse-helper lst nil)))

(defun set-intersect (set-1 set-2)
  (labels ((helper (l1 l2 result)
             (if (equal l1 nil)
                 result
                 (let ((item (car l1)))
                   (if (and (member-equal item l2)
                            (not (is-in? item result)))
                       (helper (cdr l1) l2 (cons item result))
                       (helper (cdr l1) l2 result))))))
    (reverse-list (helper set-1 set-2 nil))))
    
;; Test print the function
(format t "Intersection of (1 2 3) and (2 3): ~A~%" (set-intersect '(1 2 3) '(2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #4 Difference Function *****
(defun member-equal (item lst)
  (if (equal lst nil)
      nil        
      (if (equal item (car lst))
          t
          (member-equal item (cdr lst)))))

(defun is-in? (item lst)
  (member-equal item lst))


(defun reverse-list (lst)
  (labels ((reverse-helper (remaining result)
             (if (equal remaining nil)
                 result
                 (reverse-helper (cdr remaining) (cons (car remaining) result)))))
    (reverse-helper lst nil)))
    

(defun set-diff (set-1 set-2)
  (labels ((helper (l1 l2 result)
              (if (equal l1 nil)
                  result
                  (let ((item (car l1)))
                    (if (not (member-equal item l2))
                        (helper (cdr l1) l2 (cons item result))
                        (helper (cdr l1) l2 result))))))
                    
    (reverse-list (helper set-1 set-2 nil))))


(format t "Difference of (1 2) and (2 4): ~A~%" (set-diff '(1 2) '(2 4))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #5 Exclusive Function *****
(defun boolean-xor (a b)
(cond
    ((and a b)         ; both true
     nil)
    ((and (not a) (not b)) ; both false
     nil)
    (t                 ; one true, one false
     t)))
;;; Testing XOR 
; Xor operation work if both are the same it returns false if they are different it returns true
(print (boolean-xor t t))       ; Expected: NIL
(print (boolean-xor t nil))     ; Expected: T
(print (boolean-xor nil t))     ; Expected: T
(print (boolean-xor nil nil))   ;should be false 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #6 Implication Function *****
(defun boolean-implies (a b)
  (cond
    ((and a (not b))   ; A is true, B is false → implication fails
     nil)
    (t                 ; all other cases → implication holds
     t)))

(defun test-boolean-implies ()
  (list
   (boolean-implies t nil)
   (boolean-implies nil nil)
   (boolean-implies t t)
   (boolean-implies nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #7 Bi-Implication Function *****
(defun boolean-iff (a b)
  (or (and a b)
      (and (not a) (not b)))
)

(defun run-boolean-iff-tests ()
  (format t "~%Running boolean-iff tests...~%~%")
  
  (let ((test-cases
         '((("Both true" (boolean-iff t t)) . t)
           (("Both false" (boolean-iff nil nil)) . t)
           (("One true, one false (1)" (boolean-iff t nil)) . nil)
           (("One true, one false (2)" (boolean-iff nil t)) . nil)
           ;; More complex logic cases
           (("Result of AND matches" 
             (boolean-iff (and t t) t)) . t)
           (("Result of OR vs false"
             (boolean-iff (or nil nil) nil)) . t)
           (("Nested NOT equality"
             (boolean-iff (not (not t)) t)) . t)
           (("Different computed values"
             (boolean-iff (and t nil) (or nil t))) . nil))))
    
    (dolist (test test-cases)
      (let* ((label (caar test))
             (form (cadar test))
             (expected (cdr test))
             (result (eval form)))
        (format t "~A~%  Result:   ~A~%  Expected: ~A~%" label result expected)
        (if (equal result expected)
            (format t "  Test passed.~%~%")
            (format t "  Test failed.~%~%"))))))

; (run-boolean-iff-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #8 Boolean-Expression Function *****
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


; (run-boolean-eval-tests)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #9 Merge-Sort Function *****
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

; (run-merge-sort-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
