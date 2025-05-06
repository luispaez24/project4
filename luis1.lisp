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

;
;; Return the implication of a and b

;;

;; Examples:

;;  (boolean-implies t nil) => nil

;;  (boolean-implies nil nil) => t

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