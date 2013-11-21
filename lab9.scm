;; ====== LAB 9 ======
;;    Author(s):  
;;                
;;  Lab Section:  

;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab9.scm")  ; Change file name if copied to a new file.
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
;;;; Step 1 - More on Abstractions
;;;;

;;  expmod Procedure From the Text
;; --------------------------------
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))         


;;  list-expmod -- Without Using Map
;; ----------------------------------
;; Do not use map
(define (list-expmod item-list exp m)
  (cond ((null? item-list) ())
        (else
         (cons (expmod (car item-list) exp m) (list-expmod (cdr item-list) exp m))))
)

;;  map-expmod -- Using Map
;; -------------------------
(define (map-expmod item-list exp m)
  (map (lambda (x) (expmod x exp m)) item-list)
)


;;  deep-list-expmod
;; ------------------
(define (deep-list-expmod item-list exp m)
  (cond ((null? item-list) ())
        ((not (pair? item-list)) item-list)
        ((list? (car item-list))
         (cons (deep-list-expmod (car item-list) exp m)
               (deep-list-expmod (cdr item-list) exp m)))
        (else
         (cons (expmod (car item-list) exp m) (deep-list-expmod (cdr item-list) exp m))))
)



;;  Test Cases
;; ------------
(define test-cases-step-1
 '(
    (list-expmod '(1 2 3 4 5) 2 3)  ; (1 1 0 1 1)
    (list-expmod '(1 2) 3 10)  ; (1 8)
    (list-expmod '(3) 3 10)  ; (7)
    (list-expmod '() 2 3)  ; ()

    (map-expmod '(1 2 3 4 5) 2 3)  ; (1 1 0 1 1)
    (map-expmod '(1 2) 3 10)  ; (1 8)
    (map-expmod '(3) 3 10)  ; (7)
    (map-expmod '() 2 3)  ; ()

    (deep-list-expmod '(1 2 3 4 5) 2 3)  ; (1 1 0 1 1)
    (deep-list-expmod '(1 2) 3 10)  ; (1 8)
    (deep-list-expmod '(3) 3 10)  ; (7)
    (deep-list-expmod '() 2 3)  ; ()
    (deep-list-expmod '(((3))) 3 10)  ; (((7)))
    (deep-list-expmod '((1 2) (3 ((4) 5))) 2 3)  ; ((1 1) (0 ((1) 1)))
  ))

(do-tests 1)

;;;;
;;;; Step 2 - A Quick Warm Up
;;;;


(define test-cases-step-2
 '(

   (define step1_list1 '(1 3 (5 7) 9))
   ;; Write Solution On Next Line:

(car (cdr (car (cdr (cdr step1_list1)))))

   (define step1_list2 '((7)))
   ;; Write Solution On Next Line:

(car (car step1_list2))
   (define step1_list3 '(1 (2 (3 (4 (5 (6 7)))))))
   ;; Write Solution On Next Line:
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr step1_list3))))))))))))
))

(do-tests 2)



;;;;
;;;; Step 3 - Set Representation and 
;;;;          Computational Complexity
;;;;

;;; Part A

(define (make-set-from-list list1)
  ;; Hint: The builtin function (member elem l) can be 
  ;; used to test is elem is a member of list l. Similar
  ;; to element-of-set from the book.

(define (helper l val)
    (cond ((null? l) val)
          ((member (car l) val)
           (helper (cdr l) val))
          (else
           (helper (cdr l) (cons (car l) val)))))
  (helper list1 ())

)

;;  Test Cases
;; ------------
(define test-cases-step-3
 '(
    (make-set-from-list '(5 2 7 4 5 2 1 1 2 5)) ; (7 4 1 2 5)
    (make-set-from-list '(7 7)) ; (7)
    (make-set-from-list '(7)) ; (7)
    (make-set-from-list '()) ; ()
  ))

(do-tests 3)


;;; Part B

;; Computational Complexity of make-set-from-list:
;; 
   ;; O(n^2)



;;;;
;;;; Step 4 - Extending the Set Abstraction
;;;;

(define (set-diff setA setB)
  ;; In *linear time* return a list of all the elements in ordered
  ;; list setA, but not in ordered list setB.
  (cond ((null? setA) ())
        ((null? setB) setA)
        ((= (car setA) (car setB))
         (set-diff (cdr setA) (cdr setB)))
        ((> (car setA) (car setB))
         (set-diff setA (cdr setB)))
        (else
         (cons (car setA) (set-diff (cdr setA) setB))))
)

;;  Test Cases
;; ------------
(define test-cases-step-4
 '(
    (set-diff '(1 5 7 9) '(1 7 8 9 10)) ; (5)
    (set-diff '(1 5 7 9) '(7 8 9 10))   ; (1 5)
    (set-diff '(1 5 7 9) '(1 7 8 9))    ; (5)
    (set-diff '(0 1 5 7 9) '(1 7 8 9))  ; (0 5)
    (set-diff '(1 5 7 9) '())           ; (1 5 7 9)
    (set-diff '(9) '(1 7 8 9 10))       ; ()
    (set-diff '(1 5 7 9) '(1 5 7 9))    ; ()
    (set-diff '() '())                  ; ()
    (set-diff '() '())                  ; ()
  ))


(do-tests 4)


