#lang racket/base
;--- 2.1 ------------------------------------------

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (denom-sign (if (> d 0)
                        1
                        (- 1))))
    (cons (* (/ n g) denom-sign)
          (* (/ d g) denom-sign))))

(define (numer x) (car x))
(define (denom x) (cdr x))

; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; display
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;--- 2.2 ------------------------------------------

; constructor
(define (make-segment point1 point2) (cons point1 point2))
(define (make-point x y) (cons x y))

; selector
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (x-point x) (car x))
(define (y-point y) (cdr y))

; procedure
; average of the coordinates of the endpoints
(define (midpoint-segment line-segment)
  (make-point (average (x-point (start-segment line-segment))
                       (x-point (end-segment line-segment)))
              (average (y-point (start-segment line-segment))
                       (y-point (end-segment line-segment)))))

(define (average a b)
  (/ (+ a b) 2))

(define (line-segment x1 y1 x2 y2)
  (define point1
    (make-point x1 y1))
  (define point2
    (make-point x2 y2))
  (make-segment point1 point2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;--- 2.3 ------------------------------------------

; first representation for rectangle
(define (make-rect height width)
  (cons height width))

(define (height-rect r) (car r))
(define (width-rect r) (cdr r))

; procedure
; perimeter and area of a given rectangle
(define (perimeter-rect r)
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

; second representation for rectangle using vectors
(define (make-rect-vector p1 p2 p3)
  (if (orthogonal? (sub-vector p2 p1)
                   (sub-vector p3 p1))
      (cons p1 (cons p2 p3))
      (error "Points should make a rectangle.")))

(define (p1-rect r) (car r))
(define (p2-rect r) (car (cdr r)))
(define (p3-rect r) (cdr (cdr r)))

; dot product to check for orthogonality
(define (orthogonal? v1 v2)
  (= 0.0 (dot-product v1 v2)))

(define (dot-product p1 p2)
  (+ (* (x-point p1) (x-point p2))
     (* (y-point p1) (y-point p2))))

(define (sub-vector v1 v2)
  (make-point (- (x-point v1) (x-point v2))
              (- (y-point v1) (y-point v2))))

; distance between two points: square-root( |x2-x1|^2 + |y2-y1|^2 )
(define (distance-point p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (square x)
  (* x x))

(define (height-rect-vector r) (distance-point (p1-rect r) (p2-rect r)))
(define (width-rect-vector r) (distance-point (p1-rect r) (p3-rect r)))

; procedure
; perimeter and area of given rectangle using vectors
(define (perimeter-rect-vector r)
  (+ (* 2 (width-rect-vector r)) (* 2 (height-rect-vector r))))

(define (area-rect-vector r)
  (* (width-rect-vector r) (height-rect-vector r)))

;--- 2.4 ------------------------------------------

;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

;--- 2.5 ------------------------------------------

; 2^a * 3^b = answer
; divide answer with 2^a until the remainder has a decimal
; divide the remainder of 2^a-1 with 3^b until b is found
(define (cons-mod a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-mod x)
  (define (car-iter x count)
    (if (= 0 (remainder x 2))
        (car-iter (/ x 2) (+ 1 count))
        count))
  (car-iter x 0))

(define (cdr-mod x)
  (define (cdr-iter x count)
    (if (= 0 (remainder x 3))
        (cdr-iter (/ x 3) (+ 1 count))
        count))
  (cdr-iter x 0))

;--- 2.6 ------------------------------------------

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; define one and two not in terms of zero and add-1

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;--- 2.7 ------------------------------------------

; x and y are intervals with an upper and a lower bound
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

;--- 2.8 ------------------------------------------

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

;--- 2.9 ------------------------------------------

; Interval Arithmetics

; Addition
; given:
; z-width = (z-upper - z-lower)/2
; z-lower = x-lower + y-lower
; z-upper = x-upper + y-upper

; z-width = (z-upper - z-lower)/2
; z-width = ((x-upper + y-upper) - (x-lower + y-lower))/2
; z-width = (x-upper - x-lower)/2 + (y-upper - y-lower)/2
; z-width = x-width + y-width

; Subtraction
; given:
; z-lower = x-lower - y-upper
; z-upper = x-upper - y-lower

; z-width = (z-upper - z-lower)/2
; z-width = ((x-upper - y-lower) - (x-lower - y-upper))/2
; z-width = (x-upper - x-lower)/2 + (y-upper - y-lower)/2
; z-width = x-width + y-width

(define (interval-width a)
  (/ (- (upper-bound a) (lower-bound a)) 2))

; Multiplication
; possible case:
; z-lower = x-lower * y-lower
; z-upper = x-upper * y-upper

; z-width = (z-upper - z-lower)/2
; z-width = ((x-upper * y-upper) - (x-lower * y-lower))/2
; cant be simplified further

; Divistion
; z-lower = x-lower * 1/y-lower
; z-upper = x-upper * 1/y-upper

; z-width = (z-upper - z-lower)/2
; z-width = ((x-upper * 1/y-upper) - (x-lower * 1/y-lower))/2
; cant be simplified further

(define (interval-width-multi x y)
  (/ (- (upper-bound (mul-interval x y))
        (lower-bound (mul-interval x y))) 2))

(define (interval-width-div x y)
  (/ (- (upper-bound (div-interval x y))
        (lower-bound (div-interval x y))) 2))

;--- 2.10 -----------------------------------------

; an interval that spans zero has lower and upper bounds with different signs
; i.e. upper-bound > 0 and lower-bound < 0
; therefore multiplying lower and upper bounds will yield <= 0

(define (div-interval-mod x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "Division error (interval spans 0)" y)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

;--- 2.11 -----------------------------------------

; Ben's suggestion: test signs of endpoints of intervals to break mul-interval
; into nine cases
; both x endpoints are positive or 0:  (+,+)  (+,-)  (0,0)
; both x endpoints are negative:       (-,-)  (-,+)
; x endpoint spans 0:                  (0,+)  (0,-)
; x and y endpoints span 0:            (-,0)  (+,0)

 ; patt |  min  |  max 
 ; ++++ | al bl | ah bh 
 ; ++-+ | ah bl | ah bh 
 ; ++-- | ah bl | al bh 
 ; -+++ | al bh | ah bh 
 ; -+-+ | trouble case 
 ; -+-- | ah bl | al bl 
 ; --++ | al bh | ah bl 
 ; ---+ | al bh | al bl 
 ; ---- | ah bh | al bl

(define (mul-interval-mod x y)
  (define (endpoint-sign i)
    (cond ((and (>= (upper-bound i) 0)
                (>= (lower-bound i) 0))
          1) ; if both endpoints are non-netagive
          ((and (< (upper-bound i) 0)
                (< (lower-bound i) 0))
           -1) ; if both endpoints are negative
          (else 0))) ; if endpoints have opposite signs

  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-up (upper-bound x))
        (x-lo (lower-bound x))
        (y-up (upper-bound y))
        (y-lo (lower-bound y)))

    (cond ((> es-x 0) ; if both x endpoints are positive or 0
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-lo) (* x-up y-up)))
                 ((< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-up)))
                 (else
                  (make-interval (* x-up y-lo) (* x-up y-up)))))
          ((< es-x 0) ; if both x endpoints are negative
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-up) (* x-up y-lo)))
                 ((< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-lo)))
                 (else
                  (make-interval (* x-lo y-up) (* x-lo y-lo)))))
          (else ; x spans 0
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-up) (* x-up y-up)))
                 ((< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-lo)))
                 (else
                  (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                 (max (* x-lo y-lo) (* x-up y-up)))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

;--- 2.12 -----------------------------------------

; tolerance is in percent
; (c +/- |c*(t/100)|)
(define (make-center-percent c t)
  (make-interval (- c (abs (* c (/ t 100)))) (+ c (* c (abs (/ t 100))))))

(define (display-interval i)
  (newline)
  (display "[")
  (display (car i))
  (display ",")
  (display (cdr i))
  (display "]"))

;--- 2.13 -----------------------------------------

; x = [Cx*(1 - 0.5*Tx), Cx*(1 + 0.5*Tx)]
; y = [Cy*(1 - 0.5*Ty), Cy*(1 + 0.5*Ty)]
; x*y = [Cx*Cy*(1 - 0.5*(Tx + Ty) + 0.25*Tx*Ty),
;        Cx*Cy*(1 - 0.5*(Tx + Ty) + 0.25*Tx*Ty)]
; Tx*Ty will be a sufficiently small number and can be ignored, therefore
; the tolerance of the product will approx be the sum of component tolerances
; x*y = [Cx*Cy*(1 - 0.5*(Tx + Ty)),
;        Cx*Cy*(1 - 0.5*(Tx + Ty))]

;--- 2.14 -----------------------------------------

(define (par1 r1 r2)
  (div-interval-mod (mul-interval r1 r2)
                    (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval-mod  one
                       (add-interval (div-interval-mod one r1)
                                     (div-interval-mod one r2)))))

; functions Lem wrote assume variables are independent
; if variables are dependent, the result is wrong

; if A and B are two numbers contained in intervals:
; A = [2,8] , B = [2,8] -- they can be any number between 2 and 8
; A/A must be 1 assuming A =/= 0
; however, if A/B must be between the intervals
; [Ax*(1/By),Ay*(1/Bx)] = [0.25,4] according to interval arithmetic
; then A/A must also be between [0.25,4], it might not necessarily be 1
; this uncertainty poses a problem when encoded into variables which are then
; used for more calculations

; for par1 representing (r1*r2)/(r1+r2), the intervals r1 and r2 appear multiple
; times in calculation. The range of the interval included in the product
; calculation replaces the corresponding interval, and when the rewritten
; interval is called again in the addition calculation, it has become
; dependent on the result of the previous calculation, which results
; in the wrong answer.
; for par2, r1 and r2 only appear once in calculations and are considered
; independent because the range of the corresponding intervals have not been
; rewritten by a previous calculation, resulting in the correct answer.

; interval arithmetic can be used if all intervals appear only once
; so that a variable mapped to interval calculations does not have different
; values. However, when used in calculation multiple times, interval arithmetics
; does not translate well to arithmetic of ranges and functions. Functions will
; need to be rewritten such that it preserves the the same intervals throughout
; all calculations (this is know as the dependency problem). The purpose of this
; exercise is to show that data can be encoded in multiple ways

;--- 2.15 -----------------------------------------

; Eva is correct since the error of dependent intervals that propogate in par1
; calculations does not exist in par2 calculations

;--- 2.17 -----------------------------------------

(define (last-pair items)
  (cond ((null? items) items)
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))

;--- 2.18 -----------------------------------------

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (reverse list)
  (define (iter items size)
    (if (< size 0)
        '()
        (cons (list-ref items size) (iter items (- size 1)))))
  (let ((len (- (length list) 1)))
    (iter list len)))

;--- 2.19 -----------------------------------------

