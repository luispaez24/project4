;; Return the difference of set-1 and set-2. The result should contain no duplicates.
;; (Assume set-1 contains no duplicates and set-2 contains no duplicates.)

(defun member-equal (item lst)
  (if (equal lst nil)              ; Check if list is empty
      nil                          ; If empty, return nil
      (if (equal item (car lst))   ; If item matches head of list
          t                        ; Return t if found
          (member-equal item (cdr lst)))))  ; Recursively check tail

(defun is-in? (item lst)
  (member-equal item lst))  ; Check if item is in the list

;; Reverse function to fix format for running test cases
;; without the reverse function, the result appends to the 
;; front and messes with the unit testing
(defun reverse-list (lst)
  (labels ((reverse-helper (remaining result)
             (if (equal remaining nil)
                 result
                 (reverse-helper (cdr remaining) (cons (car remaining) result)))))
    (reverse-helper lst nil)))
    

(defun set-diff (set-1 set-2)     
  (labels ((helper (l1 l2 result)  ; Define helper functiion
              (if (equal l1 nil)   ; Base case if the first set is empty
                  result
                  (let ((item (car l1))) ; Take the first element from l1 (set-1)
                    (if (not (member-equal item l2))
                        (helper (cdr l1) l2 (cons item result))   ; Recurse with the rest of the items in set-1 and append to result
                        (helper (cdr l1) l2 result))))))          ; If item is found in set-2, continue recursively call while keep result unchanged
                    
    (reverse-list (helper set-1 set-2 nil)))) ; Return the final result after reversing the accumulated list

; Test print the function
(format t "Difference of (1 2) and (2 4): ~A~%" (set-diff '(1 2) '(2 4))) 

; Unit testing
(defun run-tests ()
  (assert (equal (set-diff '(1 2 3) '(2 4)) '(1 3)) nil "Test case 1 failed")
  (assert (equal (set-diff '(a b c) '(b c d)) '(a)) nil "Test case 2 failed")
  (assert (equal (set-diff '(1 2 3 4) '(1 2 3)) '(4)) nil "Test case 3 failed")
  (assert (equal (set-diff '(1 1 1 1 1) '(1 1 1)) '()) nil "Test case 4 failed")
  (assert (equal (set-diff '(cat dog mouse) '(dog cat rat)) '(mouse)) nil "Test case 5 failed")
  (assert (equal (set-diff '(5 10 15 20 25) '(1 5 10 15 25)) '(20)) nil "Test case 6 failed")
  (assert (equal (set-diff '(1 2 3 4 5 6) '(1 2 3 4 5 6)) nil) nil "Test case 7 failed")

  (format t "All tests passed.~%"))

;; Run the tests
(run-tests)
