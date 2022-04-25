#lang scheme

;1 - список чисел обладающих кратными делителями

(define (multiple-divisors a b)
  (define (prime? n)
  (define (iter c)
    (if (> (sqr c) n) #t
        (if (= 0 (remainder n c)) #f
            (iter (+ c 2)))))
  (cond [(= n 1) #f]
        [(= n 2) #t]
        [(even? n) #f]
        [else (iter 3)]))
  (define prime-divisors ; - список простых чисел от 2 до корня из Б
    (reverse (foldl (λ [n bld] (if (prime? n) (cons n bld) bld))
                    '() (build-list (integer-sqrt b) (λ [x] (+ x 2))))))
  (define a-b-list (build-list (+ 1 (- b a)) (λ [x] (+ x a)))) ; - список чисел от а до б
  (define (multiple-divisors? m)
    (define (iter prime-lst)
      (if (empty? prime-lst) #f
          (if (= 0 (remainder m (sqr (car prime-lst)))) #t
              (iter (cdr prime-lst)))))
    (iter prime-divisors))
  (if (empty? prime-divisors) '() (reverse (foldl (λ [el bld] (if (multiple-divisors? el) (cons el bld) bld)) '() a-b-list))))

;2 - сумма индексов Фибоначчи
; список нумеруется с нуля

(define (sum-of-Fib-idx lst)
  (define (bld-Fib-lst pprev prev l bld)
    (if (> (car bld) l) (reverse (cdr bld))
        (bld-Fib-lst prev (+ prev pprev) l (cons (+ prev pprev) bld))))
  (define Fib-lst (bld-Fib-lst 1 1 (length lst) '(1 1)))
  (foldl (λ [x idx sum]
           (if (member idx Fib-lst) (+ sum x) sum))
         0 lst (build-list (length lst) values)))

;3 - пара дружественных чисел

(define (friendly-numbers lst)
  (define (sum-of-divisors a) ; - сумма собственных делителей числа
    (define (iter s cur-div)
      (if (>= cur-div a) s
          (iter (if (= 0 (remainder a cur-div)) (+ s cur-div) s)
                (+ 1 cur-div))))
    (iter 0 1))
  (define (iter lst)
    (if (= 1 (length lst)) #f
        (if (< (car lst) 220) ; - меньше 220 дружественных чисел нет
            (iter (cdr lst))
            (let ((mem (member (sum-of-divisors (car lst)) (cdr lst))))
              (if mem (if (= (car lst) (sum-of-divisors (car mem))) (cons (car lst) (car mem)) (iter (cdr lst)))
                  (iter (cdr lst)))))))
  (iter lst))

;4 - тройки геометрических прогрессий

(define (threes-of-geom lst)
  (define (iter pprev prev bld lst)
    (if (empty? lst) (reverse bld)
        (iter prev (car lst) (if (= (/ prev pprev) (/ (car lst) prev)) (cons (list pprev prev (car lst)) bld) bld) (cdr lst))))
  (iter (car lst) (cadr lst) '() (cddr lst)))

;5 - минимальная строка

(define (min-num-row mtx)
  (cdr (foldl (λ [row prod&row]
                (if (andmap (λ [x] (and (< x 10) (>= x 0) (integer? x))) row)
                    (if (<= (apply * row) (car prod&row))
                        (cons (apply * row) row)
                        prod&row)
                    prod&row))
              (list (* (length (car mtx)) 10)) mtx)))
