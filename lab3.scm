;;;;    Author(s):  
;;;;                
;;;;  Lab Section:



;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab3.scm")  ; Change file name if copied to a new file.
)


;;;; Example Procedure

;; Determines if a object is a positive negative integer or real.
;; Inputs:  object -- a numerical value
;; Output:  String with number type information.
;; Usage Example: (num_type 5.2)

(define (num_type object)
  (cond ((eq? 0 object) "Zero")
        ((integer? object)      ; True if integer, big int, 
                                ;   or floating point integer
          (if (positive? object)
            "Positive Integer"
            "Negative Integer"))
        ((real? object)          ; True for all other floating point values.
          (if (positive? object) 
            "Positive Floating Point Number"
            "Negative Floating Point Number"))
        (else "Not A Number")))  ; Object is not a numerical type.


;; Test Cases (Examples)
(newline)(newline)
(display "== Example Test Cases ==") (newline)
(display (num_type 5))     ; "Positive Integer"
(newline)
(display (num_type 5.0))   ; "Positive Integer"
(newline)
(display (num_type -2.3))  ; "Negative Floating Point Number"
(newline)




;;;; Step 1 - Conditional Statements
;; Returns the amount of tax per bracket
;; Inputs:  income
;; Output:  tax
;; Usage Example:
(define (tax  a     ) 
 (if (<= a 35000)
     (* a .15)
     (if (<= a 100000)
	 (+ (* (- a 35000) .25) 5250 (* (- a 50000) .05))
	 (+ (* (- a 100000) .35) 21500 (* (- a 50000) .05))))
)

;; Test Cases for Progressive Tax
(display "== Step 1 Test Cases ==") (newline)
(display (tax 10000)) (newline)  ; Expected Result: 1500
(display (tax 35000)) (newline)  ; Expected Result: 5250
(display (tax 45000)) (newline)  ; Expected Result: 7750
(display (tax 50000)) (newline)  ; Expected Result: 9000
(display (tax 75000)) (newline)  ; Expected Result: 16500
(display (tax 100000)) (newline) ; Expected Result: 24000
(display (tax 200000)) (newline) ; Expected Result: 64000



;;;; Step 2 - Normal vs. Applicative Order Evaluation
;; Part A Answer:  20, because A is greater than 10 and thus it doesn't need to evaluate the second value
;; 

;; Part B Answer: 20, because A is greater than 10, but it will evaluate (+ a b) because it is part of the procedure ( it turns out to be undefined).
;; 



;;;; Step 3 - Write a Procedure
;; isTriangle?
(define (isTriangle? a b c)
  (if (> (+ a b) c)
      (if (> (+ b c) a)
	  (if (> (+ a c) b)
	      #t
	      #f)
	  #f)
      #f)
  )   


;; Test Cases for isTriangle?
(display "== Step 3 Test Cases ==") (newline)
(display (isTriangle? 1 1 1)) (newline) ; This triangle is #t since it passes each of the if statements as true.
(display (isTriangle? 1 3 4)) (newline) ; This triangle is #f since it fails the first if.
(display (isTriangle? 4 1 3)) (newline) ; This triangle is #f since it fails the second if.
(display (isTriangle? 1 4 3)) (newline) ; This triangle is #f since it fails the third if.



;;;; Step 4 - Logical Thinking
;; minimum1 -- return the smallest of 4 numbers
(define (minimum1 a b c d)
  (if (<= a b)
      (if (<= a c)
	  (if (<= a d)
	      a
	      d)
	  (if (<= c d)
	      c
	      d))
      b)
      
)
	  
	    
;; Test Cases for minimum1
(display "== Step 4a Test Cases ==") (newline)
(display (minimum1 1 2 3 4)) (newline) ; 1,  since it returns true on each if
(display (minimum1 2 2 3 4)) (newline) ; 2, since it returns false on the first if
(display (minimum1 2 3 2 4)) (newline) ; 2, since it returns false on second and then true
(display (minimum1 2 3 4 2)) (newline) ; 2, since it returns false on the third if.
(display (minimum1 2 4 2 1)) (newline) ; 1, since it returns false on the the second if and then false.

;; minimum2 -- return the smallest of 4 numbers (different algorithm)
(define (minimum2 a b c d)
  (if (and (<= a b) (<= a c) (<= a d))
	   a
	   (if (and (<= b c) (<= b d))
	       b
	       (if (<= c d)
		   c 
		   d)
	       )
	   )
)

	       
;; Test Cases for minimum2
(display "== Step 4b Test Cases ==") (newline)
(display (minimum2 2 2 3 4)) (newline) ; 2, since it returns true on first if
(display (minimum2 2 1 1 4)) (newline) ; 1, since it returns true on the second if
(display (minimum2 3 2 1 1)) (newline) ; 1, since it returns true on the third if
(display (minimum2 4 3 2 1)) (newline) ; 1, since it returns false on the thrid if

;;;; Step 5 - Encapsulation

(define a 1000)

(define pi 3.1415926)            ; first pi

(define radius 2)                ; first radius

(define (area radius)            ; second radius
    (* pi radius radius))        ; second pi, third radius

(define (circumference radius)   ; fourth radius
    (define pi 3.1)              ; third pi
    (define (diameter)    ; fifth radius
        (* 2 radius))            ; sixth radius
    (* pi (diameter))     ; fourth pi, seventh radius
)

(define (volume radius)          ; eighth radius
    (define pi 3)                ; fifth pi
    (* pi radius radius radius)  ; sixth pi, ninth radius
)

;; Evaluate The Following By Hand First, Then Check In Interpreter.
;; a. (area 100)         =>  
;; b. (circumference 10) =>  
;; c. (volume 1)         =>  
;; d. (area radius)      =>  
;; e. (circumference a)  =>  
;; f. (volume radius)    =>  
;; g. In general, how will the above code be affected if the third pi line
;;    is deleted?
;;    
;; h. In general, how will the above code be affected if "radius" is removed
;;    as a parameter of the diameter on the "fifth radius" line?
;;    


;;;; Step 6 - Special Cases

(define (iffy predicate consequent alternative)
  (cond (predicate consequent)
        (else alternative)
  )
)

;; Answer The Following:
;; Using iffy in the same way you would use if:
;; a. When will it work, if ever?
;; 
;; b. When will it fail, if ever?
;; 
;; c. Is it really nessecary for if to be a special form?  Why?
;; 




