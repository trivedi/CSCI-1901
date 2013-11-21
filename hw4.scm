;;;;CSci 1901 Fall 2012
;;;;HW 4
; =========
;;;;Author:  Nishad Trivedi
;;;;ID #:    4271100
;;;;Lab Section: 7


;; Note: Answers to some test cases are given below. For those test
;; cases not having given answers, you should check your code result
;; against what you get when you evaluate the problem manually.  If 
;; there are no test cases, you should write some.


;; Reloads the current file.
(define (reload)
  (load "hw4.scm")  ; Change file name if copied to a new file.
)


;=== Problem 1 ===

(define (compute-frequencies looking-for-list pool-list) ;compute-frequencies takes 2 lists and compares how many times the items in one list appear in the other list and returns a list of the frequencies
  (define (helper looking-for-list pl count newlist) ; helper function, takes in two lists, keeps a frequency counter and maintains a new list that has the frequencies for each item
      (cond ((null? looking-for-list) newlist) ; if the first list is empty, just return empty list
          ((null? pl) (helper (cdr looking-for-list) pool-list 0 (cons count newlist))) ; if the pool list is empty got to the next item in the looking-for-list and put the final item count in to the new list
          ((equal? (car looking-for-list) (car pl)) (helper looking-for-list (cdr pl) (+ 1 count) newlist)) ; increment count by 1 if match found, then go to next item in pool list
          (else (helper looking-for-list (cdr pl) count newlist)))) ; go to next item in pool list
  (reverse (helper looking-for-list pool-list 0 ()))
)

; Problem 1 test cases
(define a '((1 5 10) 8 (9 2 3)))
(define b '(8 1 5 8 (9 2 3)))
(define c (list 'green 'green 'green 'gray 'blue 'green 'gray 'green))
(define d (list 'green 'blue 'red))
(newline)
(display "Problem 1 Test Cases")(newline)
(display (compute-frequencies a b)) (newline) ; should return (0 2 1)
(display (compute-frequencies b a)) (newline) ; should return (1 0 0 1 1)
(display (compute-frequencies c d)) (newline) ; should return (1 1 1 0 1 1 0 1)
(display (compute-frequencies d c)) (newline) ; should return (5 0 0)
(display (compute-frequencies a d)) (newline) ; should return (0 0 0)
(newline)

;=== Problem 2 ===

(define (reverse-column table) ; helper procedure to reverse columns
  (if (null? table)
      '()
      (cons (reverse (car table)) (reverse-column (cdr table))) ; reverses each sublist in the list
  )
)

(define (reverse-table table option) ; takes in a table and ['v, 'h, or 'b]
    (cond ((eq? option 'v) (reverse table))
	  ((eq? option 'h) (reverse-column table))
	  ((eq? option 'b) (reverse (reverse-column table)))
	  (else "Invalid option") ; user is an idiot and enters a non-existant option
    )
)


; Problem 2 Test Cases
(define tableA '((1 2 3)
                (4 5 6) 
                (7 8 9)))
(define tableB '((10 20 30)
                (40 50 60) 
                (70 80 90)))
(define tableC '((100 200 300)
                (400 500 600) 
                (700 800 900)))
(define tableD '((1 2 3 4 5 6)
                (4 5 6 7 8 9) 
                (7 8 9 10 11 12)
                (0 1 2 3 4 5)))
(define tableE '((10 20 30)
                (40 50 60) 
                (70 80 90)
                (11 12 13)
                (21 22 23)))
(define tableF '((100 200 300 400)
                (400 500 600 700) 
                (700 800 900 1000)))

(display "Problem 2 Test Cases")(newline)
(display (reverse-table tableA 'v))(newline)
; should return  ((7 8 9) (4 5 6) (1 2 3))
(display (reverse-table tableB 'h))(newline)
; should return  ((30 20 10) (60 50 40) (90 80 70)
(display (reverse-table tableC 'b))(newline)
; should return  ((900 800 700) (600 500 400) (300 200 100))
(display (reverse-table tableC 'q))(newline)
; should return  "Invalid option"
(display (reverse-table tableD 'v))(newline)
; should return ((0 1 2 3 4 5) (7 8 9 10 11 12) (4 5 6 7 8 9) (1 2 3 4 5 6))
(display (reverse-table tableE 'h))(newline)
; should ((30 20 10) (60 50 40) (90 80 70) (13 12 11) (23 22 21))
(display (reverse-table tableF 'b))(newline)
;should return ((1000 900 800 700) (700 600 500 400) (400 300 200 100))
(display (reverse-table tableF 'r))(newline)
; should return "Invalid option"

;=== Problem 3 ===

(define (make-hrm-mp lower upper) ; takes in a lower bound value and upper bound value
   (define (dispatch m)
     (cond ((eq? m 'lower) lower)
	  ((eq? m 'upper) upper)
	  ((eq? m 'range-width) (- upper lower))
	  ((eq? m 'hr-max) (lambda (age) (- 205.8 (* .685 age)))) ; lambda being used to compute hr-max
	  ((eq? m 'is-in-range) (lambda (bpm) (if (and (<= lower bpm) (>= upper bpm)) true false))) ; lambda being used to find out if BPM is within the interval defined up top
	  (else "Illegal operation for this type")) ; non-existant option entered
    )
    dispatch
)

; ==== Problem 3 Test Cases ====
(newline)
(display "Problem 3 Test Cases")(newline)
(define w (make-hrm-mp 100 140))
(display (w 'lower))(newline) ; should return 100
(display (w 'upper))(newline) ; should return 140
(display (w 'range-width))(newline) ; should return 40
(display ((w 'hr-max) 20))(newline) ; should return approximately 192.1 
(display ((w 'hr-max) 42))(newline) ; should return 177.03
(display ((w 'is-in-range) 120))(newline) ; should return true
(display ((w 'is-in-range) 80))(newline) ; should return false
(display (w 'bad-input-option))(newline) ; should return "Illegal operation for this type" because 'bad-input-optioon is not a choice

