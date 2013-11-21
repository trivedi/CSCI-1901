;==== LAB 10 ====
; Author(s): Nishad Trivedi, Rahil Kaka
; Lab Section: 7

;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab10.scm")  ; Change file name if copied to a new file.
)

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



;; Square
(define (square x) (* x x))

; Tagged data code from text page 176

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)       
      (error "Bad typed datum -- CONTENTS")))       

; Two-Dimensional Table code Modified from Text Section 3.3

(define (2d-get key-1 key-2 table)
          (let ((subtable (assoc key-1 (cdr table))))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (cdr record)
                      ()))
                ())))

(define (2d-put! key-1 key-2 value table)
          (let ((subtable (assoc key-1 (cdr table))))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (set-cdr! record value)
                      (set-cdr! subtable
                                (cons (cons key-2 value)
                                      (cdr subtable)))))
                (set-cdr! table
                          (cons (list key-1
                                      (cons key-2 value))
                                (cdr table)))))
          'ok)

(define (make-table)
          (list '*table*))

; Generic operator analagous to more general apply-generic from page 184
; in the text.  Uses a specified table to be compatible with 2dtable.scm 
; for systems that do not include tables by default

(define (operate op obj t)
    (let ((proc (2d-get (type-tag obj) op t)))
        (if (not (null? proc))
            (proc (contents obj))
            (error "undefined operator for this type")
        )
    )
)  



;; Main Table for Data Directed Programming
(define T (make-table))

;; REMINDER:
;;   You must include test cases for all procedures you write.
;;   No credit will be given without test cases showing working code.
;;   
;;   Be prepared to demonstrate that the code works as expected.


;;;;
;;;; Step 1 - Writing a Temperature Package for Fahrenheit Temperatures
;;;;

;Returns a Farenhiet temperature object given a Fahrenheit, Kevlin, or Celsius value
(define (install-F-package)


  (define (make-from-F value)
    (attach-tag 'f value)
  )


  (define (make-from-C value)
    (attach-tag 'f (+ 32 (* value 1.8)))
  )


  (define (make-from-K value)
    (attach-tag 'f (+ (* (- value 273.15) 1.8) 32))
  )

 
  (define (get-F value)
    value
  )


  (define (get-C value)
    (/ (- value 32) 1.8)
  )
  

  (define (get-K value)
    (+ (/ (- value 32) 1.8) 273.15)
  )


  (2d-put! 'F 'make-from-f make-from-f t)
  (2d-put! 'F 'make-from-c make-from-c t)
  (2d-put! 'F 'make-from-k make-from-k t)
  (2d-put! 'F 'get-f get-f t)
  (2d-put! 'F 'get-c get-c t)
  (2d-put! 'F 'get-k get-k t)

  ; Here insert all the above procedures into the 2D table T with
  ; appropriate labels - key 1 should be the procedure name and
  ; key 2 should be the type (for example: 'F). 

  ; Return value
  'done
)


;;;;
;;;; Step 2 - Writing a Temperature Package for Celsius and Kelvin Representations
;;;;

;Returns a the converted Celsius temperature given a Fahrenheit, Kevlin, or Celsius value
(define (install-C-package)


  (define (make-from-F value)
    (attach-tag 'c (/ (- value 32) 1.8))
  )

 
  (define (make-from-C value)
    (attach-tag 'c value)
  )

  
  (define (make-from-K value)
    (attach-tag 'c (- value 273.15))
  )

  (define (get-F value)
    (+ (* 1.8 value) 32)
  )


  (define (get-C value)
    value
  )


  (define (get-K value)
    (+ value 273.15)
  )

  ;Inserts the table T with the Celsius procedures and label
  (2d-put! 'C 'make-from-f make-from-f t)
  (2d-put! 'C 'make-from-c make-from-c t)
  (2d-put! 'C 'make-from-k make-from-k t)
  (2d-put! 'C 'get-f get-f t)
  (2d-put! 'C 'get-c get-c t)
  (2d-put! 'C 'get-k get-k t)
  ;; Define all of the same procedures, but for the Celsius
  ;; representation

  ;; Here insert all the above procedures into the 2D table T with
  ;; appropriate labels - key 1 should be the procedure name and
  ;; key 2 should be the type (for example: 'C).
  
  
 ;; Return value
  'done
  )

;Returns a the converted Celsius temperature given a Fahrenheit, Kelvin, or Celsius value
(define (install-K-package)

  
  (define (make-from-F value)
    (attach-tag 'k (+ (/ (- value 32) 1.8) 273.15))
  )

  
  (define (make-from-C value)
    (attach-tag 'k (+ value 273.15))
  )

 
  (define (make-from-K value)
    (attach-tag 'k value)
  )


  (define (get-F value)
    (+ 32 (* 1.8 (- value 273.15)))
  )


  (define (get-C value)
    (- value 273.15)
  )

  (define (get-K value)
    value
  )

  (2d-put! 'K 'make-from-f make-from-f t)
  (2d-put! 'K 'make-from-c make-from-c t)
  (2d-put! 'K 'make-from-k make-from-k t)
  (2d-put! 'K 'get-f get-f t)
  (2d-put! 'K 'get-c get-c t)
  (2d-put! 'K 'get-k get-k t)
  ;; Define all of the same procedures, but for the Kelvin
  ;; representation

  ;; Here insert all the above procedures into the 2D table T with
  ;; appropriate labels - key 1 should be the procedure name and
  ;; key 2 should be the type (for example: 'K).
  
  
  ; Return value
  'done
  )


;;;;
;;;; Step 3 - Generic Temperature Operations and Installation
;;;;


(define (make-F-from-F value)
  ((2d-get 'F 'make-from-f t) value)
)


(define (make-F-from-C value)
  ((2d-get 'F 'make-from-c t) value)
)


(define (make-F-from-K value)
  ((2d-get 'F 'make-from-k t) value)
)

(define (make-C-from-F value)
  ((2d-get 'C 'make-from-f t) value)
)


(define (make-C-from-C value)
  ((2d-get 'C 'make-from-c t) value)
)


(define (make-C-from-K value)
  ((2d-get 'C 'make-from-k t) value)
)


(define (make-K-from-F value)
  ((2d-get 'k 'make-from-f t) value)
)


(define (make-K-from-C value)
  ((2d-get 'k 'make-from-c t) value)
)


(define (make-K-from-K value)
  ((2d-get 'k 'make-from-k t) value)
)


(define (get-F temp-object)
  (if (procedure? temp-object)
    (temp-object 'get-f)
    (operate 'get-f temp-object t))
)


(define (get-C temp-object)
  (if (procedure? temp-object)
    (temp-object 'get-c)
    (operate 'get-c temp-object t))
)

(define (get-K temp-object)
  (if (procedure? temp-object)
    (temp-object 'get-k)
    (operate 'get-k temp-object t))
)


;; Test Code for Steps 1-3:
(display "=== TEST CASES [Steps 1-3] ===") (newline)

;; Install Packages as Shown in Lab Write-Up
(display "------ Install Packages ------") (newline)
(install-F-package)
(install-C-package)
(install-K-package)

(display "------ Created Objects and Test Cases ------")

(define test-cases-step-3
 '(
   (define a (make-F-from-F 212)) ;212
   (define b (make-F-from-C 100)) ;212
   (define c (make-F-from-K 373.15)) ;212
   (define d (make-C-from-F 212))    ;100
   (define e (make-C-from-C 100))    ;100
   (define f (make-C-from-K 373.15)) ;100
   (define g (make-K-from-F 212))    ;373.15
   (define h (make-K-from-C 100))    ;373.15
   (define i (make-K-from-K 373.15)) ;373.15

   (get-F a) ;212
   (get-F b) ;212
   (get-F c) ;212
   (get-C a) ;100
   (get-C b) ;100
   (get-C c) ;100
   (get-K a) ;373.15
   (get-K b) ;373.15
   (get-K c) ;373.15

   (get-F d) ;212
   (get-F e) ;212
   (get-F f) ;212
   (get-C d) ;100
   (get-C e) ;100
   (get-C f) ;100
   (get-K d) ;373.15
   (get-K e) ;373.15
   (get-K f) ;373.15

   (get-F g) ;212
   (get-F h) ;212
   (get-F i) ;212
   (get-C g) ;100
   (get-C h) ;100
   (get-C i) ;100
   (get-K g) ;373.15
   (get-K h) ;373.15
   (get-K i) ;373.15

 ))

(do-tests 3)


;;;;
;;;; Step 4 - Hot, Cool, Cold?
;;;;
;Returns the temperature category closet to the given temperature

(define (closest-temp-category temp temp-list)
  (define (helper t temp-list val best-fit)
    (cond ((null? temp-list) best-fit)
          ((>= (abs (- (get-c (caar temp-list)) t)) val)
           (helper t (cdr temp-list) val best-fit))
          (else
            (helper t (cdr temp-list) (abs (- (get-c (caar temp-list)) t)) (cdar temp-list)))))
  (helper (get-c temp) (cdr temp-list) (abs (- (get-c (caar temp-list)) (get-c temp))) (cdar temp-list)))

(display "=== TEST CASES [STEP 4] ===") (newline)

  (define temp-list '(((C . 0) . freezing)
                      ((C . 10) . cool) 
                      ((C . 20) . warm)
                      ((C . 30) . hot)))

  (define t1 (make-F-from-F 100)) 
  (define t2 (make-C-from-F 30)) 
  (define t3 (make-K-from-C 15)) 
  (define t4 (make-F-from-K 290)) 
  (define t5 (make-C-from-K 320)) 
    
(define test-cases-step-4
 '(
  (closest-temp-category t1 temp-list) ; hot
  (closest-temp-category t2 temp-list) ; freezing
  (closest-temp-category t3 temp-list) ; cool
  (closest-temp-category t4 temp-list) ; warm
  (closest-temp-category t5 temp-list) ; hot
  ))

(do-tests 4)

;;;;
;;; Step 5 - An Intelligent Upgrade
;;;;

;; Part A
;; make-mp-from-F

(define (make-mp-from-F value)
  (define (dispatch m)
    (cond ((eq? m 'get-f) value)
          ((eq? m 'get-c)
           (/ (- value 32) 1.8))
          ((eq? m 'get-k)
           (+ 273.15 (/ (- value 32) 1.8)))
          (else
	   (error "I'm sorry, I'm afraid I can't do that"))))
  dispatch)
;; make-mp-from-C

(define (make-mp-from-C value)
  (define (dispatch m)
    (cond ((eq? m 'get-c) value)
          ((eq? m 'get-f)
           (+ 32 (* 1.8 value)))
          ((eq? m 'get-k)
           (+ value 273.15))
          (else
            (error "I'm sorry I'm afraid I can't do that"))))
  dispatch)

;; Part B: Modified

;; Part c:
;; Answer
;; No.  We modified the 'get-___' procedures from Step 3 by using an if statement to check if the temperature was a procedure  capable of handling messages




(display "--- STEP 5 TEST CASE ---") (newline)
(define j (make-mp-from-F 212))
(define k (make-mp-from-C 100))
(define t6 (make-mp-from-F 100)) 
(define t7 (make-mp-from-F 30)) 
(define t8 (make-mp-from-C 15)) 
(define t9 (make-mp-from-C 16)) 


(define test-cases-step-5
 '(
    (get-F a) ; 212
    (get-F j) ; 212
    (get-F k) ; 212
    (get-C a) ; 100
    (get-C j) ; 100
    (get-C k) ; 100
    (get-K a) ; 373.15
    (get-K j) ; 373.15
    (get-K k) ; 373.15

    (closest-temp-category t6 temp-list) ; hot
    (closest-temp-category t7 temp-list) ; freezing
    (closest-temp-category t8 temp-list) ; cool
    (closest-temp-category t9 temp-list) ; warm
   
))



(do-tests 5)
