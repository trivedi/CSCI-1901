;;;;CSci 1901 FallSpring 2013
;;;; HW 2
; ========
;;;;Author: Nishad Trivedi 
;;;;ID #: 4271100              
;;;;Lab Section: 11

;; NOTE: ANSWERS TO SOME TEST CASES ARE GIVEN BELOW. FOR THOSE TEST CASES
;; NOT HAVING GIVEN ANSWERS, YOU SHOULD EVALUATE THE PROBLEM MANUALLY AND FILL
;; IN THE EXPECTED RESULT.


;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "hw2.scm")  ; Change file name if copied to a new file.
)

(define (print items)
  (display items) (newline)
)
;;;; ====Problem 1 - Recursion / Iteration====
;;; Part A

;;; Input: An integer
;;; Output: Number of steps it takes to reach 1 using the Collatz Conjecture
(define (collatzR n)
  (cond ((< n 1) -1)
	((= n 1) 0)
	(else
	   (cond ((even? n) (+ 1 (collatzR (* .5 n))))
	         ((odd? n) (+ 1 (collatzR (+ 1 (* 3 n)))))
	   	 (else "Illegal input")
))))


(newline)(display "-------PROBLEM 1 Test cases---------")(newline)
(display "------- A) Recursive Tests ---------")(newline)
(display (collatzR 0))(newline)   ;Expected Result = -1
(display (collatzR 1))(newline)   ;Expected Result =  0
(display (collatzR 7))(newline)   ;Expected Result =  16
(display (collatzR 11))(newline)  ;Expected Result =  14
(display (collatzR 21))(newline)  ;Expected Result =  7
(display (collatzR -5))(newline)  ;Expected Result =  -1


;;; Part B
;;;
;;; Input: An integer
;;; Output: Number of steps it takes to reach 1 using the Collatz Conjecture
(define (collatzI n)
  (define (helper n count)
    (cond ((< n 1) -1)
      	  ((= n 1) count)
	  (else
	     (cond ((even? n) (helper (* .5 n) (+ 1 count)))
		   ((odd? n) (helper (+ 1 (* 3 n)) (+ 1 count)))
		   (else "Illegal input")
	     )
	  )
    )
  )
  (helper n 0)
)

(newline)(display "------- B) Iterative Tests ---------")(newline)
(display (collatzI 0))(newline)   ;Expected Result = -1
(display (collatzI 1))(newline)   ;Expected Result =  0
(display (collatzI 7))(newline)   ;Expected Result =  16
(display (collatzI 11))(newline)  ;Expected Result =  14
(display (collatzI 21))(newline)  ;Expected Result =  7
(display (collatzI -5))(newline)  ;Expected Result =  -1



;;;;; ====Problem 2 - Procedures as Parameters====
; PROBLEM 2 Solution 

; Input: A list of stops procedure and the number of stops you want to check against 
; Output: Returns the number of stops at which the bus picked up more than m people

(define (count-stops f m)
    (define (helper f m count sum)
          (if (> count 100)
	      sum
	      (if (> (f count) m)
		  (helper f m (+ count 1) (+ sum 1))
		  (helper f m (+ count 1) sum)
	      )
	   )
    )
    (helper f m 0 0)
)

; Problem 2 procedures for test cases.
(define (stops n)
   (cond ((= n 23)   2)
         ((= n 24)  37)
         ((= n 30)   2)
         ((= n 32)  90)
         ((= n 37)   1)
         ((= n 39)   3)
         ((= n 55)  15)
         ((= n 60)  34)
         ((= n 74)  23)
         (else 0)))

(define (stops-2 n)
   (cond ((= n 1)   15)
         ((= n 2)    1)
         ((= n 3)    7)
         ((= n 22)   3)
         ((= n 35)  22)
         ((= n 48)  17)
         ((= n 50)  10)
         ((= n 51)   3)
         ((= n 52)   8)
         ((= n 60)   1)
         ((= n 78)   8)
         (else 0)))

; Problem 2 test cases:
(newline)(display "-------PROBLEM 2 Test cases---------")(newline)
(display (count-stops stops  1))(newline)     ; Expected result: 8
(display (count-stops stops  5))(newline)     ; Expected result: 5
(display (count-stops stops  12))(newline)    ; Expected result: 5 
(display (count-stops stops  25))(newline)    ; Expected result: 3
(display (count-stops stops-2  0))(newline)   ; Expected result: 11
(display (count-stops stops-2  6))(newline)   ; Expected result: 7
(display (count-stops stops-2  90))(newline)  ; Expected result: 0


;;;;; ====PROBLEM 3 Procedures as Return Values====

;;; Input: Takes in values for parameters p, r, n to create a generalized function. Parameter t is a variable using the lambda function. 
;;; Output: Returns amount of money after the principle is compounded a certain number of times, at a certain rate, and a given number of years

(define (gen-comp-inter p r n)
  (lambda (t) (* p (expt (+ 1 (/ r n)) (* n t))))
)



; Problem 3 test cases
(newline)(display "-------PROBLEM 3 Test cases---------")(newline)
;; Uncomment these test cases when you have entered code
(define test-comp-inter (gen-comp-inter 10000 0.12 4))
(display (test-comp-inter 2)) (newline) ; Expected result: 12667.7

;; Write 5 additional test cases
(define test-comp-inter2 (gen-comp-inter 1000 0.15 2))
(print (test-comp-inter2 8)) ; Expected result: 3180.79

(define test-comp-inter3 (gen-comp-inter 12000 0.10 4))
(print (test-comp-inter3 6)) ; Expected result: 21704.71

(define test-comp-inter4 (gen-comp-inter 13600 0.16 2))
(print (test-comp-inter4 1)) ; Expected result: 15863.04

(define test-comp-inter5 (gen-comp-inter 8000 0.14 12))
(print (test-comp-inter5 10)) ; Expected result: 32179.77

(define test-comp-inter6 (gen-comp-inter 250 0.12 4))
(display (test-comp-inter6 19)) ; Expected result: 2363.57
