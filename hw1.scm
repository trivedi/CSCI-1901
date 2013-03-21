;;;;Author: Nishad Trivedi 
;;;;ID #: 4271100               
;;;;Lab Section: 11


;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "hw1.scm")  ; Change file name if copied to a new file.
)

(define (square x) (* x x))

(define (print . args)
  (for-each
    (lambda (item) (display item))
    args)
  (newline))

;;;; Problem 1 - Code Evaluation
;===========Part A==========
(define a 10) 
(define b 15) 
(define c 0) 

(or (> b a) (< b a) (= (/ a c) b))   

;Result: #t
;Explanation of result: The or statement uses "short circuit evaluation". 
;Since an or statment is being used, as long as the first subexpression expression evaluates to true, the entire expression will return true.

;===========Part B==========
(define x 2) 
(define (3args x y z) 
     (- (* x y) z)) 

(/ x (3args 5 12 16)) 

;Solution: 1/22
;Explanation of solution: Arguments 5, 12, and 16 are passed in to procedure 3args. Procedure 3args returns 44. Then, x (22) divided by 44 = 1/22

(3args x 30 50) 

;Solution: 10
;Explanation of solution: Arguments x, 30, and 50 are passed in to 3args. x is bound to 2. Therefore, (2 * 30) - 50 = 10

;===========Part C==========
(define a 3)
(define b (+ a 1))

(if (and (> b a) (< b (* a b)))
    (* b a)
    a)

;Solution: 12 
;Explanation of solution: The consequent, b * a (12), is returned when b is greater than a (4 > 3) and when b is less than the product of a and b (4 < 12)

(cond ((< a 4) 6)
      ((= b 4) (+ 8 7 6 a))
      (else 24))

;Solution: 6
;Explanation of solution: The first consequent is triggered because a, which is 3, is less than 4 -- thus 6 is returned

(+ 2 (if (> b a) b a))

;Solution: 6 
;Explanation of solution: b is greater than a (4 > 3) thus consequent of b (4) is returned out of the if statement and added to 2 -- thus 6 is returned

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 7))

;Solution: 40
;Explanation of solution: The second consequent is triggered because the second predicate evaluates to true. B is multiplied by the sum of a and 7. 4 * 10 = 40



;;;; Problem 2 - Conditional Procedure

;;; Input: 4 numbers
;;; Output: returns #f, "Invalid Input", area, perimeter, or sum of min and max, depending on the input
;;; Example: (myQuadrilateral 1 3 5 8) => 9
(define (myQuadrilateral a b c d)
  (cond ((or (<= a 0) (<= b 0) (<= c 0) (<= d 0)) "Invalid Input")
    	((or (> d (+ a b c)) (> c (+ a b d)) (> b (+ a c d)) (> a (+ b c d))) #f)
	((= a b c d) (square a))
	((or (= a b) (= b c) (= c d) (= a c) (= a d) (= b d)) (+ a b c d))
	(else (+ (min a b c d) (max a b c d)))
  )
)

;; Test Cases for compute
(newline) (newline)
(display "== Problem 2 Test Cases ==") (newline)
(display (myQuadrilateral 1 1 2 2)) (newline) ; result = 6
(display (myQuadrilateral 2 2 2 2)) (newline) ; result = 4
(display (myQuadrilateral 1 2 3 4)) (newline) ; result = 5
(display (myQuadrilateral 0 1 2 3)) (newline) ; result = "Invalid input"
(display (myQuadrilateral -1 1 2 3)) (newline) ; result = "Invalid input"
(display (myQuadrilateral 1 2 3 8)) (newline) ; result = #f 

(define (fact x)
  (if (= x 0)
     1
     (* x (fact (- x 1)))))

(define (test x y)
  (if (> x 0)
      x
      y))

;;(test 4 (fact -3))

;; Explanation of results
;;
;; An interpreter using applicative-order evaluation will return 'recursion depth exceeded'.
;; This is because applicative-order evaluates the arguments passed in when the procedure is applied. When (fact -3) is evaluated within the test procedure, 
;; there will be an infinite loop because the base case will never be reached.
;;
;; In contrast, normal-order evaluation will delay the evaluation of arguments until the actual argument is needed. Even though (fact -3) has the same effect as 
;; above, it will never be evaluated because the interpreter doesn't reach that far in the if statement. Since 4 is greater than 0, the consequent of 4 will be returned.
;;
;;


;;;; Problem 4 - Creating Tests

(define (test-proc a b c)
  (cond ((> a b c) (* 1.0 (/ (+ a c) b)))
        ((> b c a) (* b b))
        ((> c b a) (- (* c c) 1))
        ((= a b c) (* 2 a a))))

(newline)
(print "== Problem 4 Test Cases ==")
(print (test-proc 3 5 4)) ; Supposed to return 27, returns 25 because consequent #2 is written wrong
(print (test-proc 3 3 3)) ; Supposed to return 27, returns 18 because consequent #4 is written wrong
(print (test-proc -2 0 -1)) ; Supposed to return 2, returns 0 because consequent #2 is written wrong


; Need else statement for inputs like these:
(print (test-proc 1 2 1)) ; Supposed to return 6, returns unspecific because it doesn't fulfill any predicates ( b is greater than c, but c is less than a)
(print (test-proc 9 -5276 7)) ; Supposed to return 16/-5276, returns unspecific because it doesn't fulfill any predicates (a is greater than b, but b is less than c)
(print (test-proc 8 8 1)) ; Supposed to return unspecific, returns unspecific because predicate doesn't satisfy any consequents
		          ; (a is not greater than b, but is not less either)
(print (test-proc 8 5 6)) ; Supposed to return 1, returns unspecific because it doesn't fulfill any predicates (a is greater than b, but b is less than c)
(print (test-proc 6 3 9)) ; Supposed to return 80, returns unspecific because it doesn't fulfill any predicates (c is greater than b, but b is less than c)


;(display (test-proc 0 0 0)) ; Supposed to return 0, returns 0 but for the wrong reason (consequent is written wrong)
;(display (test-proc 7 0 -1)) ; Supposed to follow consequent #1, errors because of divide by 0




