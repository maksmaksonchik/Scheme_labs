#lang scheme

;1

(define (transpose-graph next-lists)
  (define (iter curr-n next-lists bld)
    (if (empty? next-lists) bld
        (iter (- curr-n 1)
              (cdr next-lists)
              (foldl (λ [next-vert bld] (list-update bld next-vert (λ [lst] (cons curr-n lst))))
                     bld (car next-lists)))))
  (iter (- (length next-lists) 1) (reverse next-lists) (make-list (length next-lists) '())))

;2

(define (find-complete-subgraphs adj-lists)
  (define n (length adj-lists))
  (define (iter-a a result)
    (define (iter-b b result-temp1)
      (define (iter-c c result-temp2)
        (cond
          [(= c n) (iter-b (+ b 1) (append result-temp1 result-temp2))]
          [{and (member b (list-ref adj-lists a))
                (member c (list-ref adj-lists a))
                (member c (list-ref adj-lists b))}
           (iter-c (+ c 1) (cons (list a b c) result-temp2))]
          [else (iter-c (+ c 1) result-temp2)]))
      (if (= b (- n 1))
          (iter-a (+ a 1)  (append result result-temp1))
          (iter-c (+ b 1) '())))
    (if (= a (- n 2))
        result
        (iter-b (+ a 1) '())))
  (let ([res (iter-a 0 '())])
    (if (empty? res)
        #f
        res)))

;3

(define root car)
(define L-tree cadr)
(define R-tree caddr)
(define (caddaar lst) (car (cddaar lst)))
(define (caadaar lst) (car (cadaar lst)))

(define (tree->graph tree)
  (define (iter-nodes buff-tree prev)
    (define (get-nodes tree)
      (cons (list (L-tree tree) (R-tree tree) prev) (car tree)))
    (if (empty? buff-tree)
        '()
        (append (list (get-nodes buff-tree))
                (iter-nodes (L-tree buff-tree) (car buff-tree))
                (iter-nodes (R-tree buff-tree) (car buff-tree)))))
  (define unordered-graph (sort (iter-nodes tree '()) < #:key cdr))
  (define (order-graph n unordered-graph ordered-graph)
    (if (empty? unordered-graph)
        ordered-graph
        (order-graph (+ n 1) (cdr unordered-graph)
                           (append ordered-graph
                                   (list (flatten (append (if (empty? (cddaar unordered-graph)) '() (list (caddaar unordered-graph)))
                                                          (if (empty? (caaar unordered-graph)) '() (list (caaaar unordered-graph)))
                                                          (if (empty? (cadaar unordered-graph)) '() (list (caadaar unordered-graph)))
                                                          )))))))
  (order-graph 0 unordered-graph '()))

;4

(define (graph->tree adj-lists)
  (define n (length adj-lists))
  (if (bin-tree? adj-lists n)
      (make-tree adj-lists)
      #f))

(define (make-tree adj-lists)
  (define root (index-where adj-lists (λ [adj] (<= (length adj) 2))))
  (define (iter-tree node prev-node)
    (define adj-nodes (remove prev-node (list-ref adj-lists node)))
    (if (empty? adj-nodes)
        (list node '() '())
        (list node
              (iter-tree (car adj-nodes) node)
              (if (empty? (cdr adj-nodes))
                  '()
                  (iter-tree (cadr adj-nodes) node)))))
  (iter-tree root #f))

(define (bin-tree? adj-lists n)
  (define (dfs vert prev-vert stack unused-verts)
    (foldl (λ [curr-vert stack]
             (if stack
                 (if (or (= curr-vert prev-vert) (not (member curr-vert unused-verts)))
                     stack
                     (if (member curr-vert stack)
                         #f
                         (dfs curr-vert vert (cons curr-vert stack) unused-verts)))
                 #f))
           stack (list-ref adj-lists vert)))
  (define verts (build-list n values))
  (define (acyclic? unused-verts verts)
    (cond
      [(empty? verts) #t]
      [(member (car verts) unused-verts)
       (let* ([maybe-new-component (dfs (car verts) (car verts) '() unused-verts)]
              [new-component (if (empty? maybe-new-component) (list (car verts)) maybe-new-component)])
         (if new-component
             (acyclic? (remove* new-component unused-verts) (cdr verts))
             #f))]
      [else (acyclic? unused-verts (cdr verts))]))
  (define (check-degrees adj-lists)
    (andmap (λ [lst]
              (<= (length lst) 3)) adj-lists))
  (define (check-edges adj-lists n)
    (= (- (* 2 n) 2)
       (foldl (λ [lst counter]
                (+ counter (length lst))) 0 adj-lists)))
  (and (acyclic? verts verts) (check-degrees adj-lists) (check-edges adj-lists n)))
