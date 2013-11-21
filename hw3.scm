;;;;CSci 1901 Fall 2012
;;;;HW 3
; =========
;;;;Author: Nishad Trivedi 
;;;;ID #:    4271100
;;;;Lab Section: 7


;; NOTE: Answers to some test cases are given below. For those test
;; cases not having given answers, you should check your code result
;; against what you get when you evaluate the problem manually, and
;; write in what the expected result should be for each test case.
;; If there are no test cases, be sure to write some.

;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "hw3.scm")  ; Change file name if copied to a new file.
)

;; map, etc. from Section 2.2 in text. Used in Problem 2
(define (map proc items)
   (if (null? items)
       '()
       (cons (proc (car items))
             (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
   (cond ((null? sequence) '())
         ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
         (else (filter  predicate (cdr sequence)))))
       
;; other functions
(define (identity x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))


;=== Problem 1 ===
;Manipulating Lists

;=== Square n numbers in a list ===

(define (first-n-squared lst n) ; lst is the list, n is how many numbers in you want to square
(define (helper lst n count lst2) ;lst is the list, n is how many numbers in you want to square, count keeps track of n as it increases, and lst2 creates a new list with the output
(cond ((= n 0) ())
      ((> n (length lst)) (reverse (map square lst)))
      ((null? lst) ())
      ((>= count n) lst2)
      (else
	 (helper (cdr lst) n (+ 1 count) (cons (square (car lst)) lst2)))))
(reverse (helper lst n 0 ())))


;=== Mode of a list ===

(define (next lst) ; Helper procedure for finding the mode
  (cond ((null? lst) '())
        ((or (null? (cdr lst)) (not (equal? (car lst) (cadr lst)))) (next (cdr lst)))
           (else
	    (cons (car lst) (next (cdr lst)))))) ; creates a list that has the mode number in front and the null value in the end

(define (mode lst) ; lst is list
  (cond ((null? lst) '())
        ((null? (cdr lst)) (car lst)) ; if last value is null, returns the car of the list which is the mode
        (else (mode (next lst)))))
 

;=== Greatest product ==

(define (greatest-product lst1 lst2) ; lst1 and ls2 are the 2 lists we are compairng
  (define (helper lst prod) ; prod is the product
    (if (null? lst) 
	prod
	(helper (cdr lst) (* (car lst) prod)))) ; computes the product of a list

   (let ((a (helper lst1 1))
	 (b (helper lst2 1)))
	     (cond ((= a b) "equal")
	           ((> a b) (helper lst1 1))
	           ((< a b) (helper lst2 1))
		   (else "This shouldn't happen"
	           )
              )
    )
)


;TEST CASES
(newline)
(display "==== Problem 1a Test Cases ====")(newline)
(display (first-n-squared '(1 2 3 4 5 6) 3)) (newline) ;; (1 4 9)
(display (first-n-squared '(1 2 3 4 5 6) 8)) (newline) ;; (1 2 3 4 5 6)
(display (first-n-squared '(1 2 3 4 5 6) 0)) (newline) ;; ()
(display (first-n-squared '() 3)) (newline) ;; ()


(newline)
(display "==== Problem 1b Test Cases ====")(newline)
(display (mode '(1 2 2 3 3 3 4 5))) (newline) ;; 3
(display (mode '(1 2 2))) (newline) ;; 3
(display (mode '())) (newline) ;; ()

(newline)
(display "==== Problem 1c Test Cases ====")(newline)
(display (greatest-product '(1 2 3) '(4 5 6))) (newline) ;; 120
(display (greatest-product '(1 2 3) '(1 2 3))) (newline) ;; Equal
(display (greatest-product '(1 2 3) '())) (newline) ;; 6


;=== Problem 2 ===
; Using Existing Functions with Lists
; MAP, ACCUMULATE, etc.
; In each of the following, fill in the procedure body using sum,
; filter, and/or accumulate.

(define mylist '(("sdas" 89) (239 10) ("asd" "asda") (1238 .12) (-53 "sad")))

; 2a
(define (f2a items) ; items is the list
  (if (null? items)
      ()
      (cons (filter (lambda (x) (not (string? x))) (car items)) (f2a (cdr items))) ; Removes strings in the list
  )
)


; 2b
(define (f2b items) ; items is the list
     (map (lambda (a) (map (lambda (x) (if (and (integer? x) (negative? x)) (square x) x)) a)) items) ; Finds negative integers and squares them
)


; 2c
(define (f2c items) ; items is the list
(accumulate (lambda (a b)
 (+ (accumulate (lambda (z y) (if (positive? z) (+ z y) (+ 0 y))) 0 a) b)) 0 (f2a items)) ; Finds sum of psositive numbers
)
 

;(newline)

; TEST CASES

(display "==== Problem 2 Test Cases ====")(newline) 
(newline) (display "=== 2A Test Case ===") (newline)

(display (f2a '(("sdas" 89) (239 10) ("asd" "asda") (1238 .12) (-53 "sad")))) (newline)
; should return ((89) (239 10) () (1238 .12) (-53))

(newline) (display "=== 2B Test Case ===") (newline)
(display (f2b '(("sdas" 89) (-53 "sad")))) (newline)
; should return (("sdas" 89) (2809 "sad"))

(newline) (display "=== 2C Test Case ===") (newline)
(display (f2c '((1238 .12) (-4 5) (-53 "sad")))) (newline)
; should return 1243.12



;=== Problem 3 ===
; Sieve of Eratosthenes

(define (list-2-to-n n) ; n is the last number in the list
      (define (helper n list)
	(if (< n 2)
	    list
	    (helper (- n 1) (cons n list))))
      (helper n ())
      )

; My procedure to sieve through the list
; (define (sieve x n) ; n is the list, x is the car of the list
;   (if (null? n)
;	'()
;	(if (= 0 (modulo (car n) x)) ; If modulo is not zero, it means there aren't multiples and is thus a prime
;	    (sieve x (cdr n))
;	    (cons (car n) (sieve x (cdr n)))) ; Nested pairs of primes
;   )
;  )

; (define (sieve-primes n)
;   ; This procedure creates a list of prime numbers
;    (if (null? n)        ; by using the sieve procedure above. n is a list of integers
;	'()
;	(cons (car n) (sieve-primes (sieve (car n) (cdr n)))) ; Applying the list n to the sieve proc
;    )
;)
;

(define (sieve-primes n)
  (define (helper list newlist)
    (if (null? list)
        newlist
        (helper (filter (lambda (x) (if (= (modulo x (car list)) 0) #f #t)) list)
		                    (cons (car list) newlist))))
  (reverse (helper (list-2-to-n n) '()))
)


; TEST CASES
(newline) ; n is said to be an integer, so I didn't test floats - they would mess the proc up anyways
(display "==== Problem 3a Test Cases ====")(newline) 
(display (list-2-to-n -1)) (newline) ; () because negative numbers return () since it's below 2
(display (list-2-to-n 5)) (newline) ; (2 3 4 5)
(display (list-2-to-n 20)) (newline) ; (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
(display (list-2-to-n 40)) (newline) ; (2 3 4 5 ... 35 36 37 38 39 40))
;(display (list-2-to-n a)) Procedure is expecting an integer, so this will give an error

(newline)
(display "==== Problem 3b Test Cases ====")(newline)
(display (sieve-primes 1)) (newline) ; () nothing to sieve because list is empty
(display (sieve-primes 2)) (newline) ; (2) only number in list and is a prime
(display (sieve-primes 8)) (newline)  ; (2 3 5 7)
(display (sieve-primes 15)) (newline) ; (2 3 5 7 11 13)
(display (sieve-primes 46)) (newline) ; (2 3 5 7 11 13 17 19 23 29 31 37 41 43)
(display (sieve-primes 90)) (newline) ; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89)
;(display (sieve-primes (list-2-to-n a))) Procedure is expecting an integer, so this will give an error




