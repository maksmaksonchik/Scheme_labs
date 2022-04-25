#lang scheme

;1 - quicksort

(define (quicksort lst)
  (define (Hoare-split lst head lst< lst= lst>)
    (cond [(empty? lst) (list lst< lst= lst>)]
          [(< (car lst) head) (Hoare-split (cdr lst) head (cons (car lst) lst<) lst= lst>)]
          [(= (car lst) head) (Hoare-split (cdr lst) head lst< (cons (car lst) lst=) lst>)]
          [(> (car lst) head) (Hoare-split (cdr lst) head lst< lst= (cons (car lst) lst>))]))
  (if (empty? lst) lst
      (let* ([split-lst (Hoare-split lst (car lst) '() '() '())]
             [lst< (car split-lst)]
             [lst= (cadr split-lst)]
             [lst> (caddr split-lst)])
        (append (quicksort lst<)
                lst=
                (quicksort lst>)))))

;2

(define (-dig-sum-sort lst)
  (define (dig-sum n)
    (if (< n 10) n (+ (remainder n 10) (dig-sum (quotient n 10)))))
  (define (insert n lst)
    (if (empty? lst) (list n)
        (cond [(>= (dig-sum n) (dig-sum (car lst))) (cons n lst)]
              [(< (dig-sum n) (dig-sum (car lst))) (cons (car lst) (insert n (cdr lst)))])))
  (if (empty? lst) '()
      (insert (car lst) (-dig-sum-sort (cdr lst)))))

;3

(define (find-numbers input-path output-path)
  (define (dig? ch) (and (>= (char->integer ch) 48) (<= (char->integer ch) 57)))
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (iter curr-lst)
    (define ch (read-char in))
    (cond [(equal? ch eof) (begin (close-input-port in)
                                  (close-output-port out)
                                  (display "done"))]
          [(dig? ch) (begin (display ch out)
                            (iter (cons ch curr-lst)))]
          [(empty? curr-lst) (iter '())]
          [else (begin (newline out)
                       (iter '()))]))
  (iter '()))

;4

(define (remove-wraps input-path output-path)
  (define (my-display ch out)
    (cond [(not (null? ch)) (display ch out)])) 
  (define in (open-input-file input-path))
  (define out (open-output-file output-path  #:exists 'replace))
  (define (iter flag ppprev pprev prev)
    (define ch (read-char in))
    (cond [(equal? ch eof)
           (begin (my-display ppprev out) (my-display pprev out) (my-display prev out)
                  (close-input-port in) (close-output-port out)
                  (display "done"))]
          [(and (equal? ch #\newline) (equal? prev #\return) (equal? pprev #\-) (not (equal? ppprev #\space)))
           (iter #t null null ppprev)]
          [(and (equal? ch #\space) flag)
           (begin (my-display ppprev out) (my-display pprev out)
                  (iter #f prev #\return #\newline))]
          [else
           (begin (my-display ppprev out) (iter flag pprev prev ch))]
          ))
  (iter #f null null null))

;5

(define (translate-file input-path dict-path output-path)
  (define (build-dict dict-path)
    (define in (open-input-file dict-path))
    (define (iter curr word dict)
      (define ch (read-char in))
      (cond [(equal? ch eof) (begin (close-input-port in) (cons (list word (list->string (reverse curr))) dict))]
            [(equal? ch #\space) (iter '() (list->string (reverse curr)) dict)]
            [(equal? ch #\newline) (iter '() '() (cons (list word (list->string (reverse curr))) dict))]
            [(equal? ch #\return) (iter curr word dict)]
            [else (iter (cons ch curr) word dict)]))
    (iter '() '() '()))
  (define (translate word dict)
    (cadr (assoc word dict)))
  (define (delimetr? ch)
    (or (equal? ch #\newline) (equal? ch #\return) (equal? ch #\space)))
  (define in (open-input-file input-path))
  (define out (open-output-file output-path  #:exists 'replace))
  (define dict (build-dict dict-path))
  (define (iter curr)
    (define ch (read-char in))
    (cond [(equal? ch eof) (begin (display (translate (list->string (reverse curr)) dict) out) (display "done")
                                  (close-input-port in) (close-output-port out))]
          [(delimetr? ch)
           (begin (cond [(not (empty? curr)) (display (translate (list->string (reverse curr)) dict) out)])
                  (display ch out)
                  (iter '()))]
          [else (iter (cons ch curr))]))
  (iter '()))
