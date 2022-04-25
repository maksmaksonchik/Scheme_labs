#lang scheme

(define (deg a b)
  (define (iter c k)
    (if (= k b) c (iter (* c a) (+ k 1))))
  (iter 1 0))

;1

(define (pair x k)
  (cons (quotient x (deg 10 k)) (remainder x (deg 10 k))))

;2

(define (even_list? lst)
  (if (empty? lst) #t
      (and (even? (car lst)) (even_list? (cdr lst)))))

;3

(define (neighbor lst)
  (define (iter cur prev)
    (if (empty? (cdr cur)) (car cur)
        (if (and (>= (car cur) (cadr cur)) (or (empty? prev) (>= (car cur) prev))) (car cur) (iter (cdr cur) (car cur)))))
  (iter lst '( )))

;4

(define (reverse+ lst)
  (define (iter rev bld)
    (if (empty? rev) bld (iter (cdr rev) (cons (car rev) bld))))
  (iter (cdr lst) lst))

;5

(define (prod lst n)
  (define (iter p cur r)
    (if (empty? cur)
        (if (and (= 1 p) (not (member 1 lst))) 0 p)
        (iter (if (< (car cur) r) (* p (car cur)) p) (cdr cur) r)))
  (iter 1 lst (deg 10 n)))
