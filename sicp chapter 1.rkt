#lang racket/base
(define (runtime) (current-inexact-milliseconds))

;--- 1.21 -----------------------------------------

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

;--- 1.22 -----------------------------------------

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      "nothing"))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lower upper)
  (if (even? lower)
      (search-for-primes (+ 1 lower) upper)
      (cond ((> lower upper)
             (newline) (display "done"))
            (else (timed-prime-test lower)
                  (search-for-primes (+ 2 lower) upper)))))

;--- 1.23 -----------------------------------------

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;--- 1.24 -----------------------------------------

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m ))
                    m))))

(define (fermat-timed-prime-test n)
  (start-fermat-prime-test n (runtime)))

(define (start-fermat-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      "nothing"))

(define (fermat-search-for-primes lower upper)
  (if (even? lower)
      (search-for-primes (+ 1 lower) upper)
      (cond ((> lower upper)
             (newline) (display "done"))
            (else (fermat-timed-prime-test lower)
                  (search-for-primes (+ 2 lower) upper)))))

;--- 1.27 -----------------------------------------
; numbers that evaluate to #t are prime according to fermat-test
; however ex. 561 returns #t but can be factored into 3*187, 11*51, 17*33, etc
; making it a pseudoprime or a Carmichael number

(define (carmichael-test n)
  (define (try-base n b)
    (cond ((= b 1) #t)
          ((not (= (expmod b n n) b)) #f) ; if a^n%n =/= a, not a prime
          (else (try-base n (- b 1)))))
  (try-base n (- n 1))) ; trying n-1 as base and decrementing until b=1

;--- 1.28 -----------------------------------------

(define (miller-rabin-test n)
  (define (try-base b)
    (= (MR-expmod b (- n 1) n) 1)) ; --- a^(n-1)%n = 1
  (try-base (+ 1 (random (- n 1))))) ; --- random uses applicative-order
; --- it evaluates n-1 first and returns one less than the input

(define (MR-remainder-square exp m)
  (if (and (not (or (= exp 1)
                    (= exp (- m 1))))
           (= (remainder (* exp exp) m) 1))
      0 ; --- checks for nontrivial square root of 1 mod n, indicates number
        ; --- is not prime
      (remainder (* exp exp) m)))
; --- if exp =/= 1 or m-1 and exp^2%m = 1 -> 0, exp not prime
; --- else exp^2%m

(define (MR-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (MR-remainder-square (MR-expmod base (/ exp 2) m)
                    m))
        (else
         (remainder (* base (MR-expmod base (- exp 1) m ))
                    m))))
(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n)
         (miller-rabin-prime? n (- times 1)))
        (else #f)))

;--- 1.29 -----------------------------------------

; normal integral
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; simpson's rule integral
(define (integral-simp f a b n)
  (define h
    (/ (- b a) n))
  (define (next-a x)
    (+ x (* 2 h)))
  (* (/ h 3) (+ (f a)
                (* 4 (sum f (+ a h) next-a b))
                (* 2 (sum f a next-a b))
                (f b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

;--- 1.30 -----------------------------------------

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;--- 1.31 -----------------------------------------
; 1)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; 2)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; factorial in terms of product

(define (factorial n)
  (product identity 1 incr-n n))

(define (factorial-iter n)
  (product-iter identity 1 incr-n n))

(define (incr-n n)
  (+ n 1))

(define (identity x)
  x)

; wallis product for pi

(define (wallis-product n)
  (define (term n)
    (* (/ (* 2 n)
          (- (* 2 n) 1))
       (/ (* 2 n)
          (+ (* 2 n) 1))))
  (product term 1.0 incr-n n))

;--- 1.32 -----------------------------------------
; 1)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; 2)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; redefined sum procedure

(define (acc-sum term a next b)
  (accumulate + 0 term a next b))

(define (acc-sum-iter term a next b)
  (accumulate + 0 term a next b))

; redefined product procedure

(define (acc-product term a next b)
  (accumulate * 1 term a next b))

(define (acc-product-iter term a next b)
  (accumulate * 1 term a next b))

;--- 1.33 -----------------------------------------

(define (filtered-accum predicate? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (if (predicate? a)
           (term a)
           null-value)
       (filtered-accum predicate? combiner null-value term (next a) next b))))

; 1) sum of squares of prime numbers from a to b

(define (sum-sq-prime a b)
  (filtered-accum prime? + 0 square a incr-n b))

; 2) product of positive ints less than n relatively prime to n
; all positive integers i < n such that GCD(i,n) = 1

(define (prod-int-prime n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accum relative-prime? * 1 identity 1 incr-n n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;--- 1.34 -----------------------------------------

(define (f g)
  (g 2))

;--- 1.35 -----------------------------------------

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fib x)
  (+ 1 (/ 1 x)))

;--- 1.36 -----------------------------------------

(define (fixed-point-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-pwr-x x)
  (/ (log 1000) (log x)))

(define (x-pwr-x-damp x)
  (average x (/ (log 1000) (log x))))

(define (average x y)
  (/ (+ x y) 2))

;--- 1.37 -----------------------------------------

; 1)
(define (cont-frac n d k)
  (define (recur i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
  (recur 1))

; 2)
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (sub1 i) (/ (n i) (+ result (d i))))))
  (iter (sub1 k) (/ (n k) (d k))))

;--- 1.38 -----------------------------------------

(define (d-euler i)
  (if (= (modulo i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

;--- 1.39 -----------------------------------------

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (* x x -1)))
             (lambda (i) (- (* 2.0 i) 1))
             k))

;--- 1.40 -----------------------------------------

; Newton's method : f(x) = x - (g(x)/Dg(x))

; g : procedure
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define dx 0.00001)

; Newton's method fixed-point process

(define (newton-transform g)
  (lambda (x) (- x (/ (g x)
                      ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;--- 1.41 -----------------------------------------

(define (double f)
  (lambda (x) (f (f x))))

;--- 1.42 -----------------------------------------

; x -> f(g(x))

(define (compose f g)
  (lambda (x) (f (g x))))

;--- 1.43 -----------------------------------------

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;--- 1.44 -----------------------------------------

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f (x))
                    (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;--- 1.45 -----------------------------------------

(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

; average-damp (x+f(x))/2 repeat [damp] times for y=x*y^(n-1)
; using the function produced by ((repeated average-damp damp) lambda),
; try first-guess 1.0 to find the fixed point
(define (nth-root-damped x nth damp)
  (fixed-point
   ((repeated average-damp damp)
    (lambda (y)
      (/ x (power y (- nth 1)))))
   1.0))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

; for x=2, incrementing damp so nth will converge instead of loop shows a pattern
; where minimal damp converges every time nth reaches a power of 2
; (floor (log 63 2)) = 5
; (floor (log 64 2)) = 6 ... etc.

(define (nth-root x nth)
  (fixed-point
   ((repeated average-damp (floor (log nth 2)))
    (lambda (y)
      (/ x (power y (- nth 1)))))
   1.0))

;--- 1.46 -----------------------------------------

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (let ((next (improve-guess guess)))
      (if (good-enough? guess next)
          next
          (iter next))))
  (lambda (guess) (iter guess)))

(define (sqrt-improve x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? next guess)
    (< (abs (- (square next) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point-improve f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (improve-guess guess)
    (f guess))
  (define (try guess)
    (let ((next (improve-guess guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fixed-point x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))
