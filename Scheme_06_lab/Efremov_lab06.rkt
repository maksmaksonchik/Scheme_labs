#lang scheme

(define (mem? n lst)
  (if (empty? lst) #f
      (if (= (car lst) n) #t (mem? n (cdr lst)))))

;1

(define (num_of_members lst)
  (define (iter lst m-lst k)
    (if (empty? lst) k
        (if (mem? (car lst) m-lst)
            (iter (cdr lst) m-lst k)
            (iter (cdr lst) (cons (car lst) m-lst) (+ k 1)))))
  (iter lst '() 0))

;2

(define (all_members lst)
  (define (entrys a lst)
    (define (iter num lst)
      (if (empty? lst) (cons a num)
          (iter (if (= (car lst) a) (+ num 1) num) (cdr lst))))
    (iter 0 lst))
  (define (iter lst m-lst bld)
    (if (empty? lst) (reverse bld)
        (if (mem? (car lst) m-lst)
            (iter (cdr lst) m-lst bld)
            (iter (cdr lst) (cons (car lst) m-lst) (cons (entrys (car lst) lst) bld)))))
  (iter lst '() '()))

;3

;вспомогательные функции
(define (max_of_list lst)
  (define (iter ma lst)
    (if (empty? lst) ma
        (iter (if (> (car lst) ma) (car lst) ma) (cdr lst))))
  (iter 0 lst))

(define (fib-list a)
  (define (iter cur prev bld)
    (cond [(= cur a) (reverse bld)]
          [(> cur a) (reverse (cdr bld))]
          [else (iter (+ cur prev) cur (cons (+ cur prev) bld))]))
  (iter 2 1 '(2 1)))

;основная функция
(define (num_of_fib lst)
  (define (iter lst fib-lst fib-bld num)
    (if (empty? lst) num
        (if (and (mem? (car lst) fib-lst) (not (mem? (car lst) fib-bld)))
            (iter (cdr lst) fib-lst (cons (car lst) fib-bld) (+ 1 num))
            (iter (cdr lst) fib-lst fib-bld num))))
  (iter lst (fib-list (max_of_list lst)) '() 0))

;4

;вспомогательные функции
(define (coprime? a b)
  (define (iter c m ma)
    (if (> c m) #t
        (if (and (= 0 (remainder m c)) (= 0 (remainder ma c))) #f
            (iter (+ c 1) m ma))))
  (if (or (= (abs a) 1) (= (abs b) 1)) #t (iter 2 (min (abs a) (abs b)) (max (abs a) (abs b)))))

;основная функция
(define (coprime_pairs lst1 lst2)
  (define (iter lst1 lst2 bld)
    (if (or (empty? lst1) (empty? lst2)) (reverse bld)
        (iter (cdr lst1) (cdr lst2) (if (coprime? (car lst1) (car lst2))
                                        (cons (cons (car lst1) (car lst2)) bld)
                                        bld))))
  (iter lst1 lst2 '()))

;5

(define (1-lst lst)
  (define (iter rev bld)
    (if (empty? rev) bld
        (iter (cdr rev) (cons (reverse rev) bld))))
  (iter (reverse lst) '()))
