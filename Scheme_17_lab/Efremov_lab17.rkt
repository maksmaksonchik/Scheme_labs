#lang scheme

(define root car)
(define L-tree cadr)
(define R-tree caddr)
(define (leaf? tree)
  (and (empty? (R-tree tree)) (empty? (L-tree tree))))
(define make-tree list)

;1

(define (tree-map func tree)
  (if (empty? tree)
      tree
      (make-tree ((eval func) (root tree)) (tree-map func (L-tree tree)) (tree-map func (R-tree tree)))))

;2

(define (tree-foldl func prod tree)
  (if (empty? tree) prod
      (tree-foldl func (tree-foldl func (func (root tree) prod) (L-tree tree)) (R-tree tree))))

;3 
;(remove-chains '(1 (2 (4 () ()) ()) (3 (5 () ()) (6 () (7 (8 (9 () ()) (10 () ())) ())))))

(define (remove-chains tree)
  (let ([-R? (empty? (R-tree tree))]
        [-L? (empty? (L-tree tree))])
    (cond [(and (not -R?) -L?) (remove-chains (R-tree tree))]
          [(and -R? (not -L?)) (remove-chains (L-tree tree))]
          [(and -R? -L?) tree]
          [else (make-tree (root tree) (remove-chains (L-tree tree)) (remove-chains (R-tree tree)))])))

;4
;(rebuild-tree '((2 . 4) (4 . 5) (1 . 3) (4 . 6) (1 . 2)))

(define (rebuild-tree edge-list)
  (define (find-root edge-list)
    (foldl (λ [parent-pair s] (if s s
                                  (if (foldl (λ [child-pair f] (or f (equal? (car parent-pair) (cdr child-pair)))) #f edge-list)
                                      #f
                                      (car parent-pair)))) #f edge-list))
  (define (get-node-triplets edge-list bld)
    (if (empty? edge-list) bld
        (let ([2nd-edge (findf (λ [edge] (eq? (caar edge-list) (car edge))) (cdr edge-list))])
          (get-node-triplets (remove 2nd-edge (cdr edge-list))
                             (cons (list (caar edge-list)
                                         (cdar edge-list)
                                         (if 2nd-edge (cdr 2nd-edge) '()))
                                   bld)))))
  (define (add cur-root triplets)
    (if (empty? cur-root) cur-root
        (let ([triplet (findf (λ [triplet] (equal? (car triplet) cur-root)) triplets)])
          (if triplet
              (list cur-root (add (cadr triplet) (remove triplet triplets)) (add (caddr triplet) (remove triplet triplets)))
              (list cur-root '() '())))))
  (add (find-root edge-list) (get-node-triplets edge-list '())))
