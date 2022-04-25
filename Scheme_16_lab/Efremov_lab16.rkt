#lang scheme

;1

(define (Fano-cond-catalog? lst)
  (define (L-tree tree)
    (if (or (empty? tree) (not tree)) tree (car tree)))
  (define (R-tree tree)
    (if (or (empty? tree) (not tree))
        tree
        (cdr tree)))
  (define (not-leaf? tree)
    (not (equal? (L-tree tree) 'leaf)))
  (define (add val path tree)
    (if (not-leaf? tree)
        (if (empty? path)
            (if (and (empty? (R-tree tree)) (empty? (L-tree tree)))
                (cons 'leaf val)
                #f)
            (if (= 1 (car path))
                (let ((add-right (add val
                                      (cdr path)
                                      (R-tree tree))))
                  (if add-right
                      (cons (L-tree tree) add-right)
                      #f))
                (let ((add-left (add val
                                     (cdr path)
                                     (L-tree tree))))
                  (if add-left
                      (cons add-left (R-tree tree))
                      #f))))
        #f))
  (define (f pair tree)
    (if tree (add (car pair) (cdr pair) tree) #f))
  (define (reverse-calalog lst)
    (map (λ [x] (cons (car x) (reverse (cdr x)))) lst))  
  (if (foldl f '() lst)
      #t
      (if (foldl f '() (reverse-calalog lst))
          #t
          #f)))

;2

(define (min-code lst)
  (define (L-tree tree)
    (if (empty? tree) tree (car tree)))
  (define (R-tree tree)
    (if (empty? tree) tree (cdr tree)))
  (define (leaf? tree)
    (equal? (L-tree tree) 'leaf))
  (define (add val path tree)
    (if (empty? path)
        (cons 'leaf val)
        (if (= 1 (car path))
            (cons (L-tree tree)
                  (add val
                       (cdr path)
                       (R-tree tree)))
            (cons (add val
                       (cdr path)
                       (L-tree tree))
                  (R-tree tree)))))
  (define (build-tree lst)
    (foldl (λ [pair tree] (add (car pair) (cdr pair) tree)) '() lst))
  (define (build-rev-tree lst)
    (foldl (λ [pair tree] (add (car pair) (reverse (cdr pair)) tree)) '() lst))
  (define (height tree result)
    (if (empty? tree)
        (if result (reverse result) #f)
        (if (leaf? tree) #f
            (let ([lresult (height (L-tree tree) (cons 0 result))]
                  [rresult (height (R-tree tree) (cons 1 result))])
              (if lresult
                  (if rresult
                      (if (< (length lresult) (length rresult))
                          lresult
                          rresult)
                      lresult)
                  rresult)))))  
  (define tree (build-tree lst))
  (define rev-tree (build-rev-tree lst))
  (define str (height tree '()))
  (define rev (height rev-tree '()))
  (if (< (length str) (length rev)) str rev))

;3

(define (reduce-code code)
  (define (iter code1)
    (cond [(empty? code1) #f]
          [(equal? (cdar code1) (min-code (remove (car code1) code))) (iter (cdr code1))]
          [else (caar code1)]))
  (iter code))

;4
;(frequency '((((() (() (leaf . #\A)))) leaf . #\B) ()))

(define (frequency tree)
  (define L-tree car)
  (define R-tree cdr)
  (define (leaf? tree)
    (equal? (L-tree tree) 'leaf))
  (define (height tree)
    (if (or (empty? tree) (leaf? tree))
        1
        (* 2 (max (height (L-tree tree)) (height (R-tree tree))))))
  (define freq (* (height tree) 2))
  (define (iter tree freq)
    (if (empty? tree)
        '()
        (if (leaf? tree)
            (list (cons (R-tree tree) freq))
            (append (iter (L-tree tree) (/ freq 2))
                    (iter (cadr tree)  (/ freq 2))))))
  (iter tree freq))
