;; ====== LAB 11 ======
;;    Author(s):  
;;                
;;  Lab Section:  

;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab11.scm")  ; Change file name if copied to a new file.
)

;; Square
(define (square x) (* x x))

;;;; Test Case Code:
;;;;   This will handle execution of the test cases we've included below.
;;;;   To run test cases for a step, uncomment the (do-tests #) line.
;;;;   Note:  This code will run on MIT Scheme, but would have to be modified
;;;;          to work with other versions of Scheme. 
;;;;          Change #t to #f in the line below to use for Dr Scheme / STk.
;;;;          Behavior under Dr Scheme / STk is not tested.
(define (do-tests n)
  (let* ((in-mit-scheme #t)  ;; ** Change this value 
	 (tests-symbol 
	  (string->symbol 
	   (string-append "test-cases-step-" 
			  (number->string n))))

	 (test-cases 
	  (if in-mit-scheme 
	      (eval tests-symbol user-initial-environment)
	      (eval tests-symbol)))

	 (display-string (string-append 
			  "\n--- Test Cases for Step "
			  (number->string n)
			  " ---\n")))

    (display display-string)

    (for-each 
     (lambda (x)
       (if (and (pair? x) (eq? (car x) 'define))
	   (if in-mit-scheme 
	       (eval x user-initial-environment) 
	       (eval x))
	   (begin 
	     (display x)
	     (newline)
	     (display (if in-mit-scheme 
			  (eval x user-initial-environment) 
			  (eval x)))
	     (newline))))
     test-cases)))
;;;;
;;;; Step 1.  A First Object
;;;;

(define count 
  (let ((c -1))
    (lambda ()
      (set! c (modulo (+ c 1) 3))
c))
)

; Suppose we have the following additional definitions:
;     (define counter1 count)
;     (define counter2 count)
;     (define counter3 count) ...
; Do these counters behave independently of each other when called as in
; the examples below?
;     (counter1)
;     (counter1)
;     (counter3)
;     (counter2) ...
; Explain:
; Your answer here...
; 
;

(define test-cases-step-1
  '(
    (count)
    (count)
    (count)
    (count)
    (count)))

(do-tests 1) 


;;;;
;;;; Step 2.  An Object Builder
;;;;

;; make-count

(define (make-count)
  (let ((c -1))
    (lambda ()
      (set! c (modulo (+ c 1) 3))
c))
)

(define test-cases-step-2
  '(
    (define a (make-count))
    (define b (make-count))
    (a) (a)
    (b) (b) (b) (b)
    (a)
))

 (do-tests 2) 


;;;;
;;;; Step 3.  What's all the flap about?
;;;;

(define (make-flip)
  (let ((c -1))
    (lambda ()
       (begin (set! c (modulo (+ c 1) 2))) c))
)


(define test-cases-step-3
  '(
    (define flip1 (make-flip))
    (define flip2 (make-flip))
    (flip1)
    (flip1)
    (flip2)
    (flip1)))

 (do-tests 3) 


;;;;
;;;; Step 4. Don't flip out.
;;;;

(define test-cases-step-4
  '(
    (define flip (make-flip))
    (define flap1 (flip))
    (define (flap2) (flip))
    (define flap3 flip)
    (define (flap4) flip)))

 (do-tests 4) 

; Evaluate each of these by hand in your interpreter and try to figure out what
; interaction causes each return value.
; Answers for each here:
(display flap1)(newline)
 (display (flap2))(newline)
(display flap4)(newline)
(display (flap4))(newline)
(display ((flap4)))(newline)
;(display (flap1))(newline)
(display flap2)(newline)
(display (flap3))(newline)


;;;;
;;;; Step 5. List Mutation
;;;;

(define (list-set-nth lst n newvalue)
   (cond ((null? lst) '())
         ((= n 0) (set-car! lst newvalue)
         lst)
        (else (cons (car lst) (list-set-nth (cdr lst) (- n 1) newvalue))))
)

(define test-cases-step-5
  '(
    (define x '(9 2 7 4 5))
    (define y '(10 45 4 89))
    (list-set-nth x 3 87)
    x
    (list-set-nth y 2 'hello)
    y
))

 (do-tests 5) 

;;;;
;;;; Step 6.  Message Passing
;;;;

;; Write a "grades" object with local state that accepts the following
;; messages: ’add-grade ’get-student-average ’get-assignment-average.
;;
;; Example
;; (define grades (make-grades))
;;                      student ID   assignment #   grade
;; ((grades ’add-grade) 12           1              89)
;; ((grades ’add-grade) 19           1              94)
;; ((grades ’add-grade) 38           1              100)
;; ((grades ’add-grade) 42           1              92)
;; ((grades ’add-grade) 12           2              86)
;; ((grades ’add-grade) 19           2              84)
;; ((grades ’add-grade) 38           2              91)
;; ((grades ’add-grade) 42           2              97)
;;
;; Then
;; ((grades ’get-student-average) 12) -> 87.5
;; ((grades ’get-assignment-average) 1) -> 93.75
;;
;; If you're having trouble imagining this, it may be helpful to load
;; lab10 and examine how we represented the table. To do this, type t
;; in the interpreter after loading the lab.




(define test-cases-step-6
  '(
	(define grades (make-grades))
	;;                   student ID    assignment # grade
	((grades 'add-grade) 12            1            89)
	((grades 'add-grade) 19            1            94)
	((grades 'add-grade) 38            1            100)
	((grades 'add-grade) 42            1            92)
	((grades 'add-grade) 12            2            86)
	((grades 'add-grade) 19            2            84)
	((grades 'add-grade) 38            2            91)
	((grades 'add-grade) 42            2            97)
	((grades 'get-student-average) 12)
	((grades 'get-assignment-average) 1)

	))



(define (make-grades)
  (let ((grades ()))

    (define (get-student-average id)
      (define (helper grades num-grades total-pts)
        (cond ((and (null? grades) (= num-grades 0)) 'no-scores)
              ((null? grades) (/ total-pts num-grades))
              ((= id (caar grades))
               (helper (cdr grades) (+ num-grades 1.0) (+ total-pts (caddar grades))))
              (else
               (helper (cdr grades) num-grades total-pts))))
      (helper grades 0 0))

    (define (get-assignment-average anum)
      (define (helper grades num-grades total-pts)
        (cond ((and (null? grades) (= num-grades 0)) 'no-scores)
              ((null? grades) (/ total-pts num-grades))
              ((= anum (cadar grades))
               (helper (cdr grades) (+ num-grades 1.0) (+ total-pts (caddar grades))))
              (else
               (helper (cdr grades) num-grades total-pts))))
      (helper grades 0 0))

    (define (add-grade id anum pts)
      (set! grades (cons (list id anum pts) grades))
      grades)

    (lambda (msg)
      (cond ((eq? msg 'add-grade)
             add-grade)
            ((eq? msg 'get-student-average)
             get-student-average)
            ((eq? msg 'get-assignment-average)
             get-assignment-average)
            (else
             (error "UNKNOWN -- MAKE-GRADES")))))
  )

(define (get-student-average id)
  (define (helper grades num-grades total-pts)
    (cond ((and (null? grades) (= num-grades 0)) 'no-scores)
          ((null? grades) (/ total-pts num-grades))
          ((= id (caar grades))
           (helper (cdr grades) (+ num-grades 1) (+ total-pts (caddr grades))))
          (else
           (helper (cdr grades) num-grades total-pts))))
  (helper grades 0 0))

(define (get-assignment-average anum)
  (define (helper grades num-grades total-pts)
    (cond ((and (null? grades) (= num-grades 0)) 'no-scores)
          ((null? grades) (/ total-pts num-grades))
          ((= anum (cadar grades))
           (helper (cdr grades) (+ num-grades 1) (+ total-pts (caddr grades))))
          (else
           (helper (cdr grades) num-grades total-pts))))
  (helper grades 0 0))

(define (add-grade id anum pts)
  (set! grades (cons (list id anum pts) grades))
  grades)


(define test-cases-step-6
  '(
        (define grades (make-grades))
        ;;                   student ID    assignment # grade
        ((grades 'add-grade) 12            1            89)
        ((grades 'add-grade) 19            1            94)
        ((grades 'add-grade) 38            1            100)
        ((grades 'add-grade) 42            1            92)
        ((grades 'add-grade) 12            2            86)
        ((grades 'add-grade) 19            2            84)
        ((grades 'add-grade) 38            2            91)
        ((grades 'add-grade) 42            2            97)
        ((grades 'get-student-average) 12)
        ((grades 'get-assignment-average) 1)

        ))


 (do-tests 6)


;;;;
;;;; Step 7. A Gentle Introduction to the Final Project [10 pts]
;;;;
