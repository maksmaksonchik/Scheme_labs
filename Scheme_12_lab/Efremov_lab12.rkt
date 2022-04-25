#lang scheme

;1

(define (nearest-number-with-digsum n s)
  (define (digits-sum n)
    (define (iter n s)
      (if (= n 0)
          s
          (iter (quotient n 10) (+ s (remainder n 10)))))
    (iter n 0))
  (define (list->int lst)
    (define (iter lst n)
      (if (empty? lst)
          n
          (iter (cdr lst) (+ (* 10 n) (car lst)))))
    (iter lst 0))
  (define (build-number s)
    (define (iter lst s)
      (if (< s 10)
          (list->int (cons s lst))
          (iter (cons 9 lst) (- s 9))))
    (iter '() s))
  (define (next-sum n)
    (define s (digits-sum n))
    (define (iter k)
      (define next-k (+ k 9))
      (if (= s (digits-sum next-k))
          next-k
          (iter next-k)))
    (iter n))
  (define (iter m k result)
    (define diff (abs (- n k)))
    (if(> diff m)
       result
       (iter diff (next-sum k) k)))
  (let ([builded-number (build-number s)])
    (iter (abs (- n builded-number)) builded-number 0)))

;2

(define (sum-triplet lst)
  (define (find-c a b lst)
    (if (empty? lst) #f
        (if (= (+ a b) (car lst))
            (list a b (car lst))
            (find-c a b (cdr lst)))))
  (define (iter-b a lst)
    (if (empty? lst) #f
        (let* ([b (car lst)] [cdr-lst (cdr lst)] [result (find-c a b cdr-lst)])
          (if result result
              (iter-b a cdr-lst)))))
  (define (iter-lst lst)
    (if (empty? lst) #f
        (let* ([cdr-lst (cdr lst)] [result (iter-b (car lst) cdr-lst)])
          (if result result
              (iter-lst cdr-lst)))))
  (iter-lst (sort lst <)))

;3

(define (deg-check a b)
  (define mn (min a b))
  (define mx (max a b))
  (if (= 1 mn) (if (= mx 1) 1 #f)
      (if (= (remainder mx mn) 0)
          (if (= mx mn)
              mn
              (deg-check (quotient mx mn) mn))
          #f)))

;4

(define (str-sum str)
  (define (chr->dig chr)
    (cond [(equal? chr #\0) 0]
          [(equal? chr #\1) 1]
          [(equal? chr #\2) 2]
          [(equal? chr #\3) 3]
          [(equal? chr #\4) 4]
          [(equal? chr #\5) 5]
          [(equal? chr #\6) 6]
          [(equal? chr #\7) 7]
          [(equal? chr #\8) 8]
          [(equal? chr #\9) 9]))
  (define dig-list '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define (iter lst curr sum)
    (if (empty? lst) (+ curr sum)
        (if (member (car lst) dig-list)
            (iter (cdr lst) (+ (* 10 curr) (chr->dig (car lst))) sum)
            (iter (cdr lst) 0 (+ sum curr)))))
  (iter (string->list str) 0 0))

;5 (max-prime 7)

(define (max-prime n)
  (define (int->list n)
    (define (f m lst)
      (if(= m 0)
         lst
         (f (quotient m 10) (cons (remainder m 10) lst))))
    (f n null))

  (define (list->int lst)
    (define (iter lst n)
      (if (empty? lst)
          n
          (iter (cdr lst) (+ (* 10 n) (car lst)))))
    (iter lst 0))

  (define (div? a b)
    (= (remainder a b) 0))
  (define (prime? number)
    (define (iter i)
      (cond
        [(= number 1) #f]
        [(= number 2) #t]
        [(even? number) #f]
        [(> (sqr i) number) #t]
        [(div? number i) #f]
        [else (iter (+ i 2))]))
    (iter 3))
  (define lst (int->list n))
  (define (iter-lst primes j)
    (define (iter-num i flst slst primes)
      (define num (list->int (append (append flst (list i)) slst)))
      (if (> i 9)
          primes
          (if (prime? num)
              (iter-num (+ i 1) flst slst (cons num primes))
              (iter-num (+ i 1) flst slst primes))))
    (define flst (take lst j))
    (define slst (drop lst j))
    (if (empty? slst)
        (append primes (iter-num 0 flst slst '()))
        (iter-lst (append primes (iter-num 0 flst slst '())) (+ j 1))))
  (let ([result (iter-lst '() 0)])
    (if (empty? result)
        #f
        (apply max result))))
