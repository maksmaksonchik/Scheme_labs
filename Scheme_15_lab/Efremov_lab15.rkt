#lang scheme

(define root car)
(define L-tree cadr)
(define R-tree caddr)
(define (leaf? tree)
  (and (empty? (R-tree tree)) (empty? (L-tree tree))))

;1

(define (aliquot-descendant tree)
  (define (div? a b)
    (cond [(= 0 b) #f]
          [(= 0 (remainder a b)) #t]
          {else #f}))
  (define (f tree)
    (define RT (root tree))
    (define -R? (empty? (R-tree tree)))
    (define -L? (empty? (L-tree tree)))
    (cond [(and -R? -L?) #f]
          [-R? (if (div? RT (root (L-tree tree))) RT #f)]
          [-L? (if (div? RT (root (R-tree tree))) RT #f)]
          [else (if (or (div? RT (root (L-tree tree))) (div? RT (root (R-tree tree)))) RT #f)]))
  (if (empty? tree)
      #f
      (or (aliquot-descendant (R-tree tree)) (aliquot-descendant (L-tree tree)) (f tree))))

;2

(define (mirrored-tree tree)
  (if (empty? tree) tree
      (list (root tree) (R-tree tree) (L-tree tree))))

;3

(define (left-descendants-num tree)
  (if (empty? tree) 0
      (let ((L (L-tree tree)) (R (R-tree tree)))
        (if (empty? L) (+ (left-descendants-num L) (left-descendants-num R))
            (+ 1 (left-descendants-num L) (left-descendants-num R))))))

;4

(define (min-leaf tree)
  (if (empty? tree) +inf.0
      (if (leaf? tree) (root tree)
          (min (min-leaf (R-tree tree)) (min-leaf (L-tree tree))))))

;5

(define (floors tree)
  (define (fold-lists L R)
    (define L-len (length L))
    (define R-len (length R))
    (if (> L-len R-len)
        (map append L (append R (build-list (- L-len R-len) (λ [x] '()))))
        (map append (append L (build-list (- R-len L-len) (λ [x] '()))) R)))
  (if (empty? tree) #f
      (let ((L (floors (L-tree tree))) (R (floors (R-tree tree))) (RT (root tree)))
        (cond [(and L R) (cons (list RT) (fold-lists L R))]
              [(and L (not R)) (cons (list RT) L)] ; в R пусто
              [(and R (not L)) (cons (list RT) R)] ; в L пусто
              [else (list (list RT))]))))
