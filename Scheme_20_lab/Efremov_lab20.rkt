#lang scheme

;1

(define (num-of-iso-verts input-path)
  (define in (open-input-file input-path))
  (define (iter current-vertice num)
    (define line (read-line in))
    (cond [(eof-object? line) (begin (close-input-port in) num)]
          [(andmap (λ (x) (equal? x "0")) (string-split line))
           (iter (+ 1 current-vertice) (+ 1 num))]
          [else (iter (+ 1 current-vertice) num)]))
  (iter 0 0))

;2

(define (complete-graph? input-path)
  (define in (open-input-file input-path))
  (define num-of-vertices (string->number (car (string-split (read-line in)))))
  (define (iter)
    (define line (read-line in))
    (cond [(eof-object? line) #t]
          [(= (- num-of-vertices 1) (string->number (car (string-split line))))
           (iter)]
          [else #f]))
  (iter))

;3

(define (adjacency-lists->list-of-edges input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (print-edge vert1 vert2)
    (cond [(<= vert1 vert2) (begin (display vert1 out) (display " " out) (displayln vert2 out))]))
  (read-line in)
  (define (iter current-vertice)
    (define line (read-line in))
    (if (eof-object? line)
        (begin (close-input-port in)
               (close-output-port out)
               (display "done"))
        (let ([adjacent-edges (cdr (string-split line))])
          (if (empty? adjacent-edges)
              (iter (+ 1 current-vertice))
              (begin (map (λ (str) (print-edge current-vertice (string->number str))) adjacent-edges)
                     (iter (+ 1 current-vertice)))))))
  (iter 0))

;4

(define (adjacency-matrix->adjacency-lists input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define lines (port->lines in))
  (define (get-adjacency-list row curr-n bld)
    (cond [(empty? row) (cons (length bld) (reverse bld))]
          [(equal? (car row) "1") (get-adjacency-list (cdr row) (+ 1 curr-n) (cons curr-n bld))]
          [else (get-adjacency-list (cdr row) (+ 1 curr-n) bld)]))
  (define (iter lines)
    (if (empty? lines)
        (begin (close-input-port in) (close-output-port out) (display "done"))
        (begin (map (λ (x) (begin (display x out) (display " " out)))
                    (get-adjacency-list (string-split (car lines)) 0 '()))
               (display #\newline out)
               (iter (cdr lines)))))
  (displayln (length lines) out)
  (iter lines))
  

;5

(define (adjacency-lists->adjacency-matrix input-path output-path)
  (define in (open-input-file input-path))
  (define out (open-output-file output-path #:exists 'replace))
  (define (get-row n verts-list curr-n bld) ;verts-list отсортирован
    (cond [(empty? verts-list) (append (reverse bld) (build-list (- n curr-n) (λ (x) 0)))]
          [(> curr-n n) (reverse bld)]
          [(= (string->number (car verts-list)) curr-n)
           (get-row n (cdr verts-list) (+ 1 curr-n) (cons 1 bld))]
          [else (get-row n verts-list (+ 1 curr-n) (cons 0 bld))]))
  (define num-of-vertices (string->number (car (string-split (read-line in)))))  
  (define (iter)
    (define line (read-line in))
    (if (eof-object? line)
        (begin (close-input-port in)
               (close-output-port out)
               (display "done"))
        (begin (map (λ (x) (begin (display x out) (display " " out)))
                    (get-row num-of-vertices (cdr (string-split line)) 0 '()))
               (display #\newline out)
               (iter))))
  (iter))
