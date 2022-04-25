#lang scheme

;1

(define (div x)
  (define (iter n)   
    (if (= n 0) #t (and (not (= (remainder n 10) 0)) (= (remainder x (remainder n 10)) 0) (iter (quotient n 10)))))
  (iter x))

;2

(define (log2 x)
  (define (iter n c)
    (if (>= c x) (if (= c x) n #f) (iter (+ n 1) (* c 2))))
  (iter 0 1))

;3

(define (sequence a0 n)
  (define (iter p c)
    (display p)
    (newline)
    (cond [(< c n) (iter (if (even? p) (/ p 2) (+ 1 (* p 3))) (+ c 1))]))
  (iter a0 1))

;4

(define (prod_of_prime? n)
  (define (iter c d1)
    (if (< c n)
        (if (= 0 (remainder n c))
            (if (= d1 1)
                (if (= n (sqr c)) #t (iter (+ c 1) c))
                (= n (* d1 c)))
            (iter (+ c 1) d1))
        #f))
  (iter 2 1))

;5

;вспомогательные функции
(define (deg x n)
  (define (iter c p)
    (if (= c n) p
        (iter (+ 1 c) (* p x)))
    )
  (iter 0 1))

(define (num x y n)
  (define (iter p s)
    (if (= n s) (remainder p 10)
        (iter (quotient p 10) (- s 1)))
    )
  (iter x y))

; основная функция
(define (note n) 
  (define (iter x y s)
    (if (> y (* 9 (deg 10 x) (+ 1 x)))
        (iter (+ x 1) (- y (* 9 (deg 10 x) (+ 1 x))) (+ s (* 9 (deg 10 x))))
        (if ( = (remainder y ( + 1 x)) 0) (remainder (+ s (quotient y (+ 1 x))) 10)
            (num (+ s (quotient y (+ 1 x)) 1) (+ 1 x) (remainder y (+ 1 x))))
        ))
  (iter 0 n 0))
