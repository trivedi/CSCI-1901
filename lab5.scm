;; ====== LAB 5 ====== 
;;    Author(s): 
;;                
;;  Lab Section: 



;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab5.scm")  ; Change file name if copied to a new file.
)



;; REMINDER:
;;   You must include test cases for all procedures you write.
;;   Thoroughly test each procedure and be prepared to demonstrate that the code works as expected.


;;;;
;;;; Step 1 - Using the Sum Abstraction
;;;;

;; Sum Abstraction Procedure from Text
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; Test Code
(display "--- STEP 1 [PART A] TEST CASES ---") (newline)

(display "--- STEP 1 [PART B] TEST CASES ---") (newline)

(display "--- STEP 1 [PART C] TEST CASES ---") (newline)


;; Part A Answers:
;; 

;; Part B Answers:
;; 

;; Part C Answers:




;;;;
;;;; Step 2 - Writing the Product Abstraction
;;;;


(define (2+ x) (+ 2 x))
(define (ident x) x)
(define (0->1 x) (if (= x 0) 1 x))

;; 1.31 Recursive Solution for Product
; finds the product from a to b
;; takes two procedures term, next
(define (product a b term next)
  (if (> a b)
      1
      (* (term a) (product (next a) b term next))))

;; 1.31 Iterative Solution for Product
;; finds the product from a to b
;; takes two procedures term, next
(define (product-i a b term next)
  (define (helper a result)
    (if (> a b) result
        (helper (next a) (* result (term a)))))
  (helper a 1))

;; Test Code
(display "--- STEP 2 [PART A - FACTORIAL] TEST CASES ---") (newline)
(display (product 0 3 0->1 1+)) (newline) ; 6
(display (product 0 15 0->1 1+)) (newline); 1307674368000

(display "--- STEP 2 [PART A - PI] TEST CASES ---") (newline)
(display 
 (* 4.0  
   (/ 
    (* (product 2 10 ident 2+) (product 4 12 ident 2+))  
    (square (product 3 11 ident 2+))))) (newline) ; 3.275 ...
(display
 (* 4.0
    (/
     (* (product 2 50 ident 2+) (product 4 52 ident 2+))
     (square (product 3 51 ident 2+))))) (newline) ; 3.171...

(display "--- STEP 2 [PART B - FACTORIAL] TEST CASES ---") (newline)
(display (product-i 0 3 0->1 1+)) (newline) ; 6
(display (product-i 0 15 0->1 1+)) (newline); 1307674368000

(display "--- STEP 2 [PART B - PI] TEST CASES ---") (newline)
(display 
 (* 4.0  
   (/ 
    (* (product-i 2 10 ident 2+) (product-i 4 12 ident 2+))  
    (square (product-i 3 11 ident 2+))))) (newline) ; 3.275 ...
(display
 (* 4.0
    (/
     (* (product-i 2 50 ident 2+) (product-i 4 52 ident 2+))
     (square (product-i 3 51 ident 2+))))) (newline) ; 3.171...
;; Step 2 Helper Procedures


;; 1.31 Recursive Solution for Product


;; 1.31 Iterative Solution for Product


;; Test Code
(display "--- STEP 2 [PART A - FACTORIAL] TEST CASES ---") (newline)


(display "--- STEP 2 [PART A - PI] TEST CASES ---") (newline)


(display "--- STEP 2 [PART B - FACTORIAL] TEST CASES ---") (newline)


(display "--- STEP 2 [PART B - PI] TEST CASES ---") (newline)




;;;;
;;;; Step 3 - Taking the Abstraction Further
;;;;

;; 1.32 Recursive Solution

(define (ident x) x) ; helper procedures
(define (0->1 x)
  (if (= x 0)
      1
      x)
)

(define (accumulate combiner null-val term a next b)
    (if (> a b)
null-val
 (combiner (term a) (accumulate combiner null-val term (next a) next b))))

;; 1.32 Iterative Solution

(define (accumulate-i combiner null-val term a next b)
  (define (helper a result)
    (if (> a b) result
        (helper (next a) (combiner (term a) result))))
  (helper a null-val)
)

;; Test Code
(display "--- STEP 3 [PART A] TEST CASES ---") (newline)
(display (accumulate + 0 ident 0 1+ 5)) (newline)
(display (accumulate * 1 0->1 0 1+ 3)) (newline)

(display "--- STEP 3 [PART B] TEST CASES ---") (newline)
(display  (accumulate-i + 0 ident 0 1+ 5)) (newline)
(display (accumulate-i * 1 0->1 0 1+ 3)) (newline)



;;;;
;;;; Step 4 - Compound Procedure 
;;;;

;; 1.42 Solution

(define (inc x) (+ 1 x))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Test Code
(display "--- STEP 4 TEST CASES ---") (newline)
(display ((compose square inc) 6))(newline)
(display ((compose factorial inc) 3))(newline)
(display ((compose square factorial) 3))(newline)

;;;;
;;;; Step 5 - Estimating Cosine x
;;;;

(define PI 3.14159265358979)

(define (cos x limit)
  (accumulate +                        
              0                        
              (lambda (n)              
                (/
                  (* (expt (- 1) n) (expt x (* 2 n)))
                  (factorial (* 2 n))))
              0                       
              1+                       
              limit))                  



(display "--- STEP 5 TEST CASES ---") (newline)

(display (cos (/ PI 3) 5))(newline)
(display (cos (/ PI 3) 8))(newline)
(display (cos (/ PI 3) 12))(newline)



