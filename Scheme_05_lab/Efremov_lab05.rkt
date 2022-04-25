#lang scheme

;1

(define (prime_factors a)
  (define (prime? n)
    (define (iter c)
      (if (> (sqr c) n) #t
          (if (= 0 (remainder n c)) #f
              (iter (+ c 2)))))
    (cond [(= n 1) #f]
          [(= n 2) #t]
          [(even? n) #f]
          [else (iter 3)]))
  (define (iter n fact prev k bld)
    (if (= n 1) (reverse (cons (cons prev k) bld))
        (if (and (prime? fact) (= 0 (remainder n fact)))
            (if (= fact prev)
                (iter (quotient n fact) fact fact (+ k 1) bld)
                (iter (quotient n fact) fact fact 1 (if (> k 0) (cons (cons prev k) bld) bld)))
            (iter n (+ fact 1) prev k bld))))
  (iter a 2 2 0 '()))

;2
;элементы списка нумеруются с 0

(define (average lst)
  (define (aver lst)
    (define (iter lst1 s k)
      (if (empty? lst1)
          (/ s k)
          (iter (cdr lst1) (+ s (car lst1)) (+ k 1))))
    (iter lst 0 0))
  (define (iter lst1 c near bld sr)
    (if (empty? lst1) (reverse bld)
        (cond [(and (< (- sr (car lst1)) (- sr near)) (<= (car lst1) sr)) (iter (cdr lst1) (+ c 1) (car lst1) (cons c (cons (car lst1) '())) sr)]
              [(= (- sr (car lst1)) (- sr near)) (iter (cdr lst1) (+ c 1) near (cons c bld) sr)]
              [else (iter (cdr lst1) (+ c 1) near bld sr)])))
  (iter lst 0 (car lst) (cons (car lst) '()) (aver lst)))

;3

(define (interval lst)
  (define (iter newlst prev c mx)
    (if (empty? newlst) (if (= mx 0) 1 mx)
        (if (= (remainder (car newlst) 2) (remainder prev 2))
            (iter (cdr newlst) (car newlst) (+ c 1) (if (> (+ c 1) mx) (+ c 1) mx))
            (iter (cdr newlst) (car newlst) 1 (if (> c mx) c mx)))))
  (iter lst (car lst) 0 0))

;4
;элементы списка нумеруются с 0

(define (aliquot lst)
  (define (iter newlst bld c)
    (if (empty? newlst) (reverse bld)
        (iter (cdr newlst) (if (= (remainder (car newlst) c) 0) (cons (car newlst) bld) bld) (+ c 1))))
  (if (empty? lst) '() (iter (cdr lst) '() 1)))

;5

(define (mirror lst1 lst2)
  (define (rev_dig n)
    (define (iter f p)
      (if (= f 0) p (iter (quotient f 10) (+ (* p 10) (remainder f 10)))))
    (iter n 0))
  (define (iter cur1 rev2)
    (if (empty? cur1) #t
        (and (= (car cur1) (rev_dig (car rev2)))
             (not (= (remainder (car cur1) 10) 0))
             (not (= (remainder (car rev2) 10) 0))
             (iter (cdr cur1) (cdr rev2)))))
  (iter lst1 (reverse lst2)))
