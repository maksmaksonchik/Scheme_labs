#lang scheme

(define root car)
(define L-tree cadr)
(define R-tree caddr)
(define (leaf? tree)
  (and (empty? (R-tree tree)) (empty? (L-tree tree))))

;1 тройки

(define (triplets lst)
  (define (get-triplets lst0 lst1 lst2 bld)
    (if (or (empty? lst0) (empty? lst1) (empty? lst2))
        bld
        (get-triplets (cdr lst0) (cdr lst1) (cdr lst2) (cons (list (car lst0) (car lst1) (car lst2)) bld))))
  (define (iter lst lst0 lst1 lst2)
    (if (empty? lst) (get-triplets lst0 lst1 lst2 '())
        (let* ((el (car lst)) (rem (remainder el 3)))
          (cond [(= rem 0) (iter (cdr lst) (cons el lst0) lst1 lst2)]
                [(= rem 1) (iter (cdr lst) lst0 (cons el lst1) lst2)]
                [(= rem 2) (iter (cdr lst) lst0 lst1 (cons el lst2))]))))
  (iter lst '() '() '()))

;2 каталог

(define (path lst)
  (define (disk? name)
    (eq? #\/ (last (string->list name))))
  (define (iter lst bld)
    (if (empty? lst)
        (if (empty? bld) #f (reverse bld))
        (iter (cdr lst) (if (disk? (car lst))
                            (list (car lst))
                            (if (empty? bld)
                                bld
                                (if (> (length bld) 2)
                                    (if (eq? (cadr bld) (car lst))
                                        (cdr bld)
                                        (cons (car lst) bld))
                                    (cons (car lst) bld)))))))
  (define path-list (iter lst '()))
  (if path-list
      (foldl (λ [el idx bld] (if (< idx 2) (string-append bld el) (string-append bld "/" el)))
             "" path-list (build-list (length path-list) values))
      #f))

;3 - количество вхождений максимума

(define (num-of-Max tree)
  (define (num-of-max tree)
    (if (empty? tree) (cons -inf.0 0)
        (let* ((left (num-of-max (L-tree tree)))
               (right (num-of-max (R-tree tree)))
               (max-branch (if (and (not (= -inf.0 (car left))) (= (car left) (car right))) ; ветви равны, но не пустые
                               (cons (car left) (+ (cdr left) (cdr right)))
                               (if (> (car left) (car right))
                                   left
                                   right))))
          (if (= (root tree) (car max-branch))
              (cons (car max-branch) (+ (cdr max-branch) 1))
              (if (> (car max-branch) (root tree))
                  max-branch
                  (cons (root tree) 1))))))
  (define result-pair (num-of-max tree))
  (cdr result-pair))

;4 - количество листьев

(define (num-of-leafs tree)
  (if (empty? tree) 0
      (if (leaf? tree) 1
          (+ (num-of-leafs (R-tree tree)) (num-of-leafs (L-tree tree))))))

;5 - условие

(define (f tree)
  (or (leaf? tree)
      (and (= 3 (length tree))
           (or (leaf? tree)
               (not (or (empty? (L-tree tree)) (empty? (R-tree tree)))))
           (f (L-tree tree))
           (f (R-tree tree)))))
