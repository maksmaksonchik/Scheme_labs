#lang scheme

;1

(define (string->num str)
  (define (digit x)
    (cond [(equal? x #\1) 1]
          [(equal? x #\2) 2]
          [(equal? x #\3) 3]
          [(equal? x #\4) 4]
          [(equal? x #\5) 5]
          [(equal? x #\6) 6]
          [(equal? x #\7) 7]
          [(equal? x #\8) 8]
          [(equal? x #\9) 9]
          [(equal? x #\0) 0]))
  (foldl (λ [x res]
           (+ (* 10 res) (digit x))) 0 (string->list str)))

;2

(define (num-of-words str delim)
  (define delimiters (string->list delim))
  (cdr (foldl (λ [sym prev+res] (if (member sym delimiters)
                                    (cons #t (cdr prev+res))
                                    (if (car prev+res) (cons #f (+ 1 (cdr prev+res))) (cons #f (cdr prev+res)))))
              (cons #t 0) (string->list str))))

;3

(define (split str delim)
  (define delimiters (string->list delim))
  (define (iter string-list current-word result)
    (if (empty? string-list)
        (if (empty? current-word) result (cons (list->string current-word) result))
        (if (member (car string-list) delimiters)
            (iter (cdr string-list) '() (if (not (empty? current-word))
                                            (cons (list->string current-word) result)
                                            result))
            (iter (cdr string-list) (cons (car string-list) current-word) result))))
  (iter (reverse (string->list str)) '() '()))

;4

(define (equal-words str delim)
  (define (iter word-list bld)
    (if (empty? word-list) #f
        (if (member (car word-list) bld) (car word-list)
            (iter (cdr word-list) (cons (car word-list) bld)))))
  (iter (split str delim) '()))
