;; ====== LAB 7 ======  
;;    Author(s):  
;;               
;;  Lab Section: 


;;;; Utility Functions

;; Reloads the current file.
(define (reload)
  (load "lab7.scm")  ; Change file name if copied to a new file.
)

;; display+ displays all of the values with a newline at the end.
(define (display+ . args)
  (for-each
    (lambda (item) (display item))
    args)
  (newline))



;; REMINDER:
;;   You must include test cases for all procedures you write.
;;   No credit will be given without test cases showing working code.
;;   
;;   This lab gives specific instructions for testing the code.
;;   These are minimum requirements, but you may do further tests as needed.
;;   Use define to store the results of your tests so they can be used in future
;;   steps.
;;
;;   Read through the lab writeup for full instructions and hints on how to
;;   approach this lab.
;;
;;   Also pay attention to hints and clarifications provided in this template
;;   regarding the test cases.



;;;;
;;;; Step 1 - Getting Warmed Up
;;;;

;; Recursive accumulate procedure from Lab 5:

(define (accumulate combiner null-value term a next b)
  (cond ((> a b) null-value)
        (else (combiner
                (term a)
                (accumulate combiner null-value term (next a) next b)))))


(define (ident x) x)

(newline)
(display+ "--- STEP 1 - Integers From 1 to 10 ---")
;; Example Of How To Call/Display:
;;  (display+ (accumulate ... ))
(display+ (accumulate cons () ident 1 1+ 10)) (newline)
(display+ "--- STEP 1 - Squares of Integers From 23 to 28 ---")
(display+ (accumulate cons () square 23 1+ 28)) (newline)
(display+ "--- STEP 1 - Powers of 2 from 2 to 4096 ---")
(display+ (accumulate cons () (lambda (n) (expt 2 n)) 1 1+ 12))    ;
(newline)

;iterative process of accumulate
(define (accumulate-i combiner null-value term a next b)
  (define (helper a null-value)
    (cond ((> a b) null-value)
          (else (helper (next a) (combiner (term a) null-value)))))
   (helper a null-value)
)
(display+ "--- STEP 1 - Integers from 1 to 10 (Iterative) ---")
(display+ (accumulate-i cons ()  (lambda (x) (- 10 x)) 0 1+ 9 ))



;;;;
;;;; Step 2 - Point Abstraction: Starting a 2-Dimensional Point System
;;;;

;; make-point

(define (make-point x y z)
  (list x y z))

;; get-x
(define (get-x point) 
  (car point))

;; get-y
(define (get-y point)
  (cadr point))

;; get-z
(define (get-z point)
 (caddr point))

;; Test Code Instructions:
;;   Define a new point.  Display it.
;;   Display the x and y values separately using your selectors.
;;   You may use this point in future tests as well.

;; Note:
;;   The above is done for you below -- just uncomment those lines.
;;   You may want to define some other points here to use in future steps.

(display+ "--- STEP 2 TEST CASES ---")
;; Example Test Case:
 (define pt1 (make-point 2 4 3))
  (display+ "Point: "pt1)            ;; Expecting (2 . 4 . 3)
   (display+ "X-Coord: " (get-x pt1)) ;; Expecting 2
   (display+ "Y-Coord: " (get-y pt1)) ;; Expecting 4
  (display+ "Z-Coord: " (get-z pt1)) ;; expecting 3
;; Define Additional Points:



 ; (define pt2 (make-point 3 6 8))
   (display+ "Point: "pt2)            ;; Expecting (3 . 6 . 8)
   (display+ "X-Coord: " (get-x pt2)) ;; Expecting 3
   (display+ "Y-Coord: " (get-y pt2)) ;; Expecting 6
   (display+ "Z-coord: " (get-z pt2)) ;; Expecting 8


;;;;
;;;; Step 3 - Maintaining a List of Points
;;;;


;; make-pt-list
(define (make-pt-list p pt-list)
(cons p pt-list))

;; the-empty-pt-list
(define the-empty-pt-list '())


;; get-first-point
(define (get-first-point my-point-list)
(car my-point-list))

;; get-rest-points
(define (get-rest-points my-point-list )
(cdr my-point-list))

(define my-point-list (accumulate make-pt-list the-empty-pt-list (lambda (x) (make-point x (+ 1 x) x)) 1  1+  10))
(newline)
(display+ my-point-list)
;; Test Code:
;;   Using make-pt-list and the-empty-pt-list, define a list with 6+ points.
;;   Show the list after each point is added.
;;   Display the entire list, the first point, and all but the first point.
;;   Display the second point.
;;   Display all except the first two points.



(display+ "--- STEP 3 - Building The List ---")


(display+ "--- STEP 3 - First Point ---")
(display+ (car my-point-list))  

(display+ "--- STEP 3 - Second Point ---")
(display+ (cadr my-point-list))

(display+ "--- STEP 3 - All Except First Two Points ---")
(display+ (cddr my-point-list))




;;;;
;;;; Step 4 - Operations on pt-lists
;;;;

;; sum-xcoord
(define (sum-xcoord my-point-list)
  (if (null? my-point-list)
      0
      (+ (get-x (get-first-point my-point-list)) (sum-xcoord (get-rest-points my-point-list)))))


;; max-xcoord
(define (max-xcoord my-point-list)
  (define (helper l val)
    (cond ((null? l) val)
          (else (helper (get-rest-points l) (max (get-x (get-first-point l)) val)))))
  (helper my-point-list (get-x (get-first-point my-point-list)))
)


(define (distance pt1 pt2)
  (sqrt (+
	 (square (- (get-x pt2) (get-x pt1)))
	 (square (- (get-y pt2) (get-y pt1)))
	 (square (- (get-z pt2) (get-z pt1)))
	 )
  )
)
	 

;; max-distance

(define (max-distance p my-point-list)
  (define (helper l d)
      (cond ((null? l) d)
            (else (helper (get-rest-points l) (max d (distance p (get-first-point l)))))))
  (helper my-point-list 0)
)

;; Test Code
;;   Use the list you created in step 3 and the point created in step 2.
;;   Show the results you get using these values in the above operations.
;;   Test the procedures with an empty point list as well.

(display+ "--- STEP 4 - sum-xcoord ---")
(display+ "List: " my-point-list)
(display+ "Sum of x values: " (sum-xcoord my-point-list))

(display+ "--- STEP 4 - max-xcoord ---")
(display+ "List: " my-point-list)
(display+ "Largest X-coordinate: " ( max-xcoord my-point-list))


(display+ "--- STEP 4 - distance ---")
(display+ "Points: " (get-first-point my-point-list) " and " (make-point 3 8 5))
(display+ "List: " my-point-list)
(display+ "Distance between Point 1 and Point 2: " (distance (get-first-point my-point-list) (make-point 3 8 5))) 

(display+ "--- STEP 4 - max-distance ---")
(display+ "List: " my-point-list)
(display+ "Max distance between (0 . 0 . 0) and a coordinate pair in my list: " (max-distance (make-point 0 0 0) my-point-list))

;;;;
;;;; Step 5 - One More Operation on pt-lists
;;;;

;; max-range

(define (max-range my-point-list)
  (define (helper l val)
    (cond ((null? l) val)
          (else
           (helper (get-rest-points l) (max val (max-distance (get-first-point l) my-point-list))))))
  (helper my-point-list 0))


;; Test Code:
;;   Use the list from part 3 to test this operation.
;;   Create a second point list with at least 5 entries for additional tests.

(display+ "--- STEP 5 TEST CASES ---")

 (display+ "List: " my-point-list)
 (display+ "The max range is: " (max-range my-point-list))
 

;;;;
;;;; Step 6 - A Question
;;;;

;; Answer to Question:
;;
;; It makes more sense semantically to abstract procedures based on their function. Someone reading through the code would have a bit more difficulty knowing what (car list) does compared to the procedure of get-x. 
;; Also, if there needs to be a change to a procedure, we can minimize effort by just changing the one abstracted procedure rather than everywhere that abstraction was reimplemented. 
;;
;;




;;;;
;;;; Step 7 - Maintaining a Sorted Point-List
;;;;

;; make-sorted-pt-list
(define origin (make-point 0 0 0))

(define (make-sorted-pt-list p my-point-list)
  (cond ((null? my-point-list) (make-pt-list p the-empty-pt-list))
        ((< (distance origin p) (distance origin (get-first-point my-point-list)))
         (make-pt-list p my-point-list))
        (else
          (make-pt-list (get-first-point my-point-list) (make-sorted-pt-list p (get-rest-points my-point-list))))))


;; Answer to Question:
;;
;; It's easier to visualize the points when they're sorted.
;;

;; Test Code:
;;   Create a sorted list of at least 6 points.
;;   Be sure to test addition of points to the front, back, and middle.
;;   Show the list after each point is added.


(display+ "--- STEP 7 TEST CASES ---")

(define my-sorted-pt-list (make-sorted-pt-list (make-point 5 5 3) the-empty-pt-list))
(display+ my-sorted-pt-list)
(define my-sorted-pt-list (make-sorted-pt-list (make-point 1 2 8) my-sorted-pt-list))
(display+ my-sorted-pt-list)
(define my-sorted-pt-list (make-sorted-pt-list (make-point 6 7 8) my-sorted-pt-list))
(display+ my-sorted-pt-list)
(define my-sorted-pt-list (make-sorted-pt-list (make-point 4 3 1) my-sorted-pt-list))
(display+ my-sorted-pt-list)
(define my-sorted-pt-list (make-sorted-pt-list (make-point 6 6 4) my-sorted-pt-list))
(display+ my-sorted-pt-list)
(define my-sorted-pt-list (make-sorted-pt-list (make-point 7 7 3) my-sorted-pt-list))
(display+ my-sorted-pt-list)

