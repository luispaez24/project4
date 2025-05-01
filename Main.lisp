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
(format t "Is 4 a member of (1 3 5) using set-member? ~A~%" (set-member '(1 3 5) 4))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #6 Implication Function *****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #7 Bi-Implication Function *****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #8 Boolean-Expression Function *****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ***** #9 Merge-Sort Function *****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
