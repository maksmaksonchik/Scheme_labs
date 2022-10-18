#lang scheme

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