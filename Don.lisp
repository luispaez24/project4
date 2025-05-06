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


