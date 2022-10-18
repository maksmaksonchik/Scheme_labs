#lang scheme
(define root car)
(define L-tree cadr)
(define R-tree caddr)
(define (leaf? tree)
  (and (empty? (R-tree tree)) (empty? (L-tree tree))))

#| Примеры:
'(0 (1 () ()) (2 () ()))
'(0 (1 (3 () ()) (4 () ())) (2 () ()))
|#

(define (get-graph- tree prev) ; работает только для полных деревьев
  (if (leaf? tree)
      (list (list (root tree) (list prev)))
      (cons (list (root tree) (if prev
                                  (list prev (root (L-tree tree)) (root (R-tree tree)))
                                  (list (root (L-tree tree)) (root (R-tree tree)))))
            (append (get-graph- (L-tree tree) (root tree))
                    (get-graph- (R-tree tree) (root tree))))))

(define (tree->graph tree)
  (map cadr (sort (get-graph- tree #f) (λ [x y] (< (car x) (car y))))))