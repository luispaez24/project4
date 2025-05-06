;; Return T if item is a member of set.

;; Return NIL if item is not a member of set.

;; The type of set is list.

;; Examples:

;;  (set-member '(1 2) 1) => T

;;  (set-member '(1 2) 3) =>  NIL

(defun set-member (set item)
       "Returns T if item is a member of set using EQUAL, NIL otherwise.
        Compliant with the restricted function list."
       (if (equal set nil) ;; Replaced NULL with EQUAL check against NIL
         nil
         (if (equal (car set) item) ;; Replaced EQL with EQUAL
           t
           (set-member (cdr set) item))))

(format t "Set-member '(1 2 3) 2: ~A~%" (set-member '(1 2 3) 2))

;;; Unit testing for set-member
(defun run-set-member-tests ()
       (assert (eql (set-member '(1 2 3) 2) t) nil "Set-member Test case 1 failed")
       (assert (eql (set-member '(1 2 3) 4) nil) nil "Set-member Test case 2 failed")
       (assert (eql (set-member '() 1) nil) nil "Set-member Test case 3 failed")
       (assert (eql (set-member '(a b c) 'a) t) nil "Set-member Test case 4 failed")
       (assert (eql (set-member '(a b c) 'c) t) nil "Set-member Test case 5 failed")
       (assert (eql (set-member '(5) 5) t) nil "Set-member Test case 6 failed")
       (assert (eql (set-member '(5) 6) nil) nil "Set-member Test case 7 failed")
       (format t "All set-member tests passed.~%"))

;; Run the set-member tests
(run-set-member-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the union of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;;   (set-union '(1 2) '(2 4)) => '(1 2 4)


(defun reverse-list (lst)
       (labels ((reverse-helper (remaining result)
                                (if (equal remaining nil)
                                  result
                                  (reverse-helper (cdr remaining) (cons (car remaining) result)))))
               (reverse-helper lst nil)))


(defun member-equal-custom (item lst)
       (if (equal lst nil)
         nil
         (if (equal item (car lst))
           t
           (member-equal-custom item (cdr lst)))))


(defun set-union (set-1 set-2)
  (labels ((process-list (source accumulator)
             (if (equal source nil)
                 accumulator
                 (let ((item (car source)))
                   (if (not (member-equal-custom item accumulator))
                       (process-list (cdr source) (cons item accumulator))
                       (process-list (cdr source) accumulator)))))
           )
    (let ((unique-elements (process-list set-1 nil)))
      (let ((final-reversed-result (process-list set-2 unique-elements)))
        (reverse-list final-reversed-result)))))




;; Test print the function
(format t "Union ~A~%" (set-union '(1 2) '(2 4)))

(defun run-union-tests ()
  ;; Numeric sets (use <)
  (assert (equal (sort (copy-list (set-union '(1 2 3) '(2 3 4))) #'<) '(1 2 3 4)) nil "Union Test case 1 failed")
  (assert (equal (sort (copy-list (set-union '(1 2 3) '(4 5 6))) #'<) '(1 2 3 4 5 6)) nil "Union Test case 3 failed")
  (assert (equal (sort (copy-list (set-union '(1 2 3) '(2 3 3))) #'<) '(1 2 3)) nil "Union Test case 4 failed")
  (assert (equal (sort (copy-list (set-union '(1 2) '(1 2))) #'<) '(1 2)) nil "Union Test case 5 failed")
  (assert (equal (sort (copy-list (set-union '() '(2 4 6))) #'<) '(2 4 6)) nil "Union Test case 6 failed")
  (assert (equal (sort (copy-list (set-union '(1 2 3) '())) #'<) '(1 2 3)) nil "Union Test case 7 failed")

  ;; Symbolic sets (use string<)
  (assert (equal (sort (copy-list (set-union '(a b c) '(b c d))) #'string<) '(a b c d)) nil "Union Test case 2 failed")

  (format t "All union tests passed.~%"))

(run-union-tests)
