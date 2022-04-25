#lang scheme

;1

(define (min a b)
  ( if (< a b) a b))

(define (min_dig x)
  (if (< x 10) x (min (remainder x 10) (min_dig (quotient x 10)))
      ))

;2

(define (eq_dig x)
  (if (< x 10) #t (and (eq_dig (quotient x 10)) (= (remainder (quotient x 10) 10) (remainder x 10)))))

;3

(define (test n)
  (define (iter p c m)
    (if (>= p m)
        p
        (iter (* c p) (+ c 1) m)))
  (= n (iter 1 1 n)))

;4

(define (fib n)
  (define (it a b n)
    (if (>= b n) (if (< (- b n) (- n a)) b a) (it b (+ a b) n)))
  (it 1 1 n))

;5

(define (ideal n)
  (define (iter p c m)
    (if (>= c m)
        (- p 1)
        (if (and (= (remainder m c) 0) (not (= c m))) (iter (+ c p) (+ c 1) m) (iter p (+ c 1) m))))
  (= n (iter 1 1 n)))
