#lang scheme

;1

(define (formatter input-path output-path . args)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (iter s num)
    (define ch (read-char in))
    (cond [(equal? ch eof) (begin (close-input-port in)
                                  (close-output-port out)
                                  (display "done"))]
          [(= s 0) (if (equal? ch #\%)
                       (iter 1 0)
                       (begin (display ch out) (iter 0 0)))]
          [(= s 1) (if (equal? ch #\%)
                       (if (= num 0)
                           (begin (display ch out) (iter 0 0))
                           (begin (display (list-ref args (- num 1)) out) (iter 0 0)))
                       (iter 1 (+ (* 10 num) (- (char->integer ch) 48))))]))
  (iter 0 0))

;2

(define (digits-in-ascending-order? number) ; проверка на возрастание цифр
  (define (check number prev-digit)
    (if (< number 10)
        (< number prev-digit)
        (and (< (remainder number 10) prev-digit)
             (check (quotient number 10) (remainder number 10)))))
  (check (quotient number 10) (remainder number 10)))

(define (ascending-order-sums input-path output-path) ;основная функция
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define lines (port->lines in))
  (define (get-ascending-order-sums-list lst bld)
    (if (empty? lst) bld
        (let* ([nums (string-split (car lst) "+")]
               [sum (+ (string->number (car nums)) (string->number (cadr nums)))])
          (if (digits-in-ascending-order? sum)
              (get-ascending-order-sums-list (cdr lst) (cons sum bld))
              (get-ascending-order-sums-list (cdr lst) bld)))))
  (begin (map (λ [num] (displayln num out)) (sort (get-ascending-order-sums-list lines '()) >))
         (close-input-port in)
         (close-output-port out)))

;3

(define (function? lst)
  (and (list? lst) (equal? (car lst) 'define) (list? (cadr lst))))

(define (get-function-body lst)
  (cddr lst))

(define (count-functions lst)
  (define a 2)
  (if (empty? lst)
      0
      (if (function? (car lst))
          (+ 1 (count-functions (get-function-body (car lst))) (count-functions (cdr lst)))
          (count-functions (cdr lst)))))

(define (get-obj-list input-path)
  (define in (open-input-file input-path))
  ;(read-line in) здесь можно добавить костыль, чтобы не падало от #lang, но только если он гарантировано в 1 строке
  (define (iter bld)
    (define obj (read in))
    (if (equal? obj eof)
        (begin (close-input-port in) bld)
        (iter (cons obj bld))))
  (iter '()))

;основная функция
(define (count-functions-in-file input-path) 
  (count-functions (get-obj-list input-path)))

;4

(define (get-function-name lst)
  (caadr lst))

(define (find-functions lst)
  (define a 2)
  (if (empty? lst)
      '()
      (if (function? (car lst))
          (list (get-function-name (car lst)) (find-functions (get-function-body (car lst))) (find-functions (cdr lst)))
          (find-functions (cdr lst)))))

(define (find-functions-in-file input-path)
  (find-functions (get-obj-list input-path)))
          

(define (rec? expr name) 
  (if (or (empty? expr) (not (list? expr)))
      #f
      (if (list? (car expr))
          (or (rec? (car expr) name) (rec? (cdr expr) name))
          (if (equal? (car expr) name)
              #t
              (rec? (cdr expr) name)))))

        
(define (partition pred lst)
  (define (iter lst true-lst false-lst)
    (if (empty? lst)
        (append (list true-lst) (list false-lst))
        (if (pred (car lst))
            (iter (cdr lst) (cons (car lst) true-lst) false-lst)
            (iter (cdr lst) true-lst (cons (car lst) false-lst)))))
  (iter lst '() '()))

(define (find-recursive expr name) 
  (define partitioned (partition function? expr))
  (define funcs (car partitioned))
  (define exprs (cdr partitioned))
  (append
   (foldl (lambda [func res] (append res (find-recursive (get-function-body func) (get-function-name func)))) '() funcs)
   (filter-not empty? (foldl (lambda [exp result]
                               (if (rec? exp name)
                                   (cons (cons name 'recursive) result)
                                   (if name
                                       (cons (cons name 'non_recursive) result)
                                       (cons '() result)))) '() exprs))))
; основная функция                                                       
(define (find-recursive-in-file input-path out-nonrec-path out-rec-path)
  (define in (open-input-file input-path))
  (define out-nonrec (open-output-file out-nonrec-path  #:exists 'replace))
  (define out-rec (open-output-file out-rec-path  #:exists 'replace))
  (define (next)
    (define expr (read in))
    (if (eof-object? expr)
        (begin (close-input-port in) (close-output-port out-nonrec) (close-output-port out-rec))
        (begin
          (map (lambda [func]
                 (if (equal? (cdr func) 'recursive)
                     (displayln (car func) out-rec)
                     (displayln (car func) out-nonrec)))
               (find-recursive (cons expr '()) #f))
          (next))))
  (next))
