;; ====== LAB 4 ======  
;;    Author(s):  
;;                
;;  Lab Section:  


;;;; Utility Functions

;; Reloads the current file.
(define (reload)
 (load "lab4.scm")  ; Change file name if copied to a new file.
  )

(newline) ;; Ensures first line of test cases is on new line.


;;;;;;;;;;;;;;;;;;
;; REMINDER:
;;   You must include test cases for all procedures you write.
;;   Thoroughly test each procedure and be prepared to demonstrate that the code works as expected.
;;;;;;;;;;;;;;;;;;



;;;;
;;;; Step 1 - A Recursive Process
;;;;

;; count-div357


(define (count-div357 a b)
  (cond ((> a b) 0)
        ((or (= 0 (remainder a 3))
             (= 0 (remainder a 5))
             (= 0 (remainder a 7)))
	 (+ 1 (count-div357 (+ a 1) b))
	 )
	(else (+ 0 (count-div357 (+ a 1) b))
	)
))
 


;; Test Code
(display "=== STEP 1 TEST CASES ===") (newline)
(display (count-div357 42 1)) (newline)
(display (count-div357 1 3)) (newline)
(display (count-div357 1 15)) (newline)
(display (count-div357 1 80)) (newline)
;;;;
;;;; Step 2 - Now Try It Iteratively
;;;;

;; iter-div357
(define (iter-div357 a b count)
  (cond ((> a b) count)
        ((or (= 0 (remainder a 3))
             (= 0 (remainder a 5))
             (= 0 (remainder a 7)))
         (iter-div357 (+ a 1) b (+ 1 count))
	 )
        (else (iter-div357 (+ a 1) b count))
  )
)

;; Test Code
(display "=== STEP 2 TEST CASES ===") (newline)
(display (iter-div357 42 1 0))(newline)
(display (iter-div357 1 3 0)) (newline)
(display (iter-div357 1 15 0)) (newline)
(display (iter-div357 1 80 0)) (newline)





;;;;
;;;; Step 3 - Modulo Calculation
;;;;

(define (modulo a b)
  (if (< a b) 
      a
      (modulo (- a b) b)
   )
) 

;; Solution
;;



(display "=== STEP 3 TEST CASES ===") (newline)

;;;;
;;;; Step 4 - Tree Recursion
;;;;

;; Recursive Procedure

 (define (f n) 
    (cond ((< n 3) n) 
         (else (+ (f (- n 1)) 
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3))))
	       )
	 )
)

;; Test Code
(display "=== STEP 4 TEST CASES ===") (newline)
(display (f 1)) (newline)
(display (f 5)) (newline)
(display (f 7)) (newline)

;;;;
;;;; Step 5 - Solving for e
;;;

(define (e limit)
(if (= limit 1) 1
      (+ (/ 1 (factorial limit))  (e (- limit 1)))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial(- n 1)))
  )
)

;; Test Code
(display "=== STEP 5 TEST CASES ===") (newline)

(display "--- n = 3 ---") (newline)
(display (e 3))

(display "--- n = 5 ---") (newline)

(display (e 5))

(display "--- n = 10 ---") (newline)

(display (e 10))

;;;;
;;;; Step 6 - Revisiting Fibonacci From the Text
;;;;

;; Recursive Fibonacci Procedure From Text
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (time-fib x)
  (define start (runtime))
(fib x)
(- (runtime) start))
(display (fib 10)) (newline)
(display (fib 20)) (newline)
(display (fib 25)) (newline)
(display (fib 26)) (newline)
(display (fib 27)) (newline)
(display (fib 28)) (newline)
(display (fib 29)) (newline)
(display (fib 30)) (newline)

;; Draw Graph on Paper

;; How many times does (fib 2) get called when calculating (fib 5)?
;; Answer:  3




