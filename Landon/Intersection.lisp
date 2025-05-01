;; Return the intersection of set-1 and set-2. The result should contain no duplicates.

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

(defun set-intersect (set-1 set-2)
  (labels ((helper (l1 l2 result)   ; Define helper function
             (if (equal l1 nil)    ; Base case: if first list is empty, return result
                 result
                 (let ((item (car l1)))  ; Take the head of the list
                   (if (and (member-equal item l2)  ; Check if item is in set-2
                            (not (is-in? item result)))  ; Check if it's already in result
                       (helper (cdr l1) l2 (cons item result))  ; Add to result if both conditions met
                       (helper (cdr l1) l2 result))))))  ; Otherwise, continue with the rest of the list
    (reverse-list (helper set-1 set-2 nil))))  ; Call helper function with initial values
    
;; Test print the function
(format t "Intersection of (1 2 3) and (2 3): ~A~%" (set-intersect '(1 2 3) '(2 3)))

;; Unit testing
(defun run-tests ()
  (assert (equal (set-intersect '(1 2 3) '(2 3 4)) '(2 3)) nil "Test case 1 failed")
  (assert (equal (set-intersect '(a b c) '(b c d)) '(b c)) nil "Test case 2 failed")
  (assert (equal (set-intersect '(1 2 3) '(4 5 6)) nil) nil "Test case 3 failed")
  (assert (equal (set-intersect '(1 2 3) '(2 3 3)) '(2 3)) nil "Test case 4 failed")
  (assert (equal (set-intersect '(1 2) '(1 2)) '(1 2)) nil "Test case 5 failed")
  (assert (equal (set-intersect '() '(2 4 6)) nil) nil "Test case 6 failed")
  (assert (equal (set-intersect '(1 2 3) '(4 5 6)) nil) nil "Test case 7 failed")

  (format t "All tests passed.~%"))

;; Run the tests
(run-tests)