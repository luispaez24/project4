;; Return T if item is a member of set.

;; Return NIL if item is not a member of set.

;; The type of set is list.

;; Examples:

;;  (set-member '(1 2) 1) => T

;;  (set-member '(1 2) 3) =>  NIL

(defun set-member (set item)
       "Returns T if item is a member of set, NIL otherwise.
       The set is represented as a list."
       (if (null set) ; If the set is empty
         nil       ; item is not a member
         (if (eql (car set) item) ; If the first element of the set is equal to the item
           t       ; item is a member
           (set-member (cdr set) item)))) ; Otherwise, check the rest of the set

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


(defun set-union (set-1 set-2)
       (labels ((helper (l1 l2 result)
                        (if (equal l1 nil)
                          (append (reverse l2) result) ; Add remaining elements of l2
                          (let ((item (car l1)))
                               (if (not (is-in? item result)) ; Add if not already in result
                                 (helper (cdr l1) l2 (cons item result))
                                 (helper (cdr l1) l2 result))))))
               (reverse (helper set-1 set-2 nil))))

;; Test print the function
(format t "Union ~A~%" (set-union '(1 2) '(2 4)))

;; Unit testing for union
(defun run-union-tests ()
       (assert (equal (sort (set-union '(1 2 3) '(2 3 4))) '(1 2 3 4)) nil "Union Test case 1 failed")
       (assert (equal (sort (set-union '(a b c) '(b c d))) '(a b c d)) nil "Union Test case 2 failed")
       (assert (equal (sort (set-union '(1 2 3) '(4 5 6))) '(1 2 3 4 5 6)) nil "Union Test case 3 failed")
       (assert (equal (sort (set-union '(1 2 3) '(2 3 3))) '(1 2 3)) nil "Union Test case 4 failed")
       (assert (equal (sort (set-union '(1 2) '(1 2))) '(1 2)) nil "Union Test case 5 failed")
       (assert (equal (sort (set-union '() '(2 4 6))) '(2 4 6)) nil "Union Test case 6 failed")
       (assert (equal (sort (set-union '(1 2 3) '())) '(1 2 3)) nil "Union Test case 7 failed")

       (format t "All union tests passed.~%"))

;; Run the union tests
(run-union-tests)
