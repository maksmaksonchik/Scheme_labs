#lang scheme

;в каждом списке смежности находятся только вершины, смежные данным

;1

;возвращаются списки смежности отсортированного графа
;если нужно вернуть список вершин в отсортированном порядке, нужно убрать последний foldr и вернуть stack

(define (top-sort adj-lists)
  (define (dfs vertex stack)
    (cons vertex (foldl (λ [current-vertex stack]
                          (if (member current-vertex stack)
                              stack
                              (dfs current-vertex stack))) stack (list-ref adj-lists vertex))))
  (define vertexes-list (build-list (length adj-lists) values))
  (define stack (foldl (λ [current-vertex stack]
                         (if (member current-vertex stack)
                             stack
                             (dfs current-vertex stack))) '() vertexes-list))
  (define (index n k lst)
    (if (= (car lst) n) k
        (index n (+ k 1) (cdr lst))))
  (foldr (λ [old-vertex res]
           (cons (map (λ [old-vertex-num] (index old-vertex-num 0 stack)) (list-ref adj-lists old-vertex)) res)) '() stack))

;2

(define (remove-vertexes edges-list vertexes-to-remove)
  (define (normalize x y)
    (if (> x y)
        (- x 1)
        x))
  (define (cadr/cdr maybe-list)
    (if (list? maybe-list)
        (cadr maybe-list)
        (cdr maybe-list)))
  (foldl (λ [vertex-to-remove new-vertex rez]
           (foldl (λ [edge res]
                    (let ([first (car edge)] [second (cadr/cdr edge)] [diff (- vertex-to-remove new-vertex)])
                      (if (or (= first diff) (= second diff))
                          res
                          (cons (cons (normalize first diff) (normalize second diff)) res)))) '() rez))
         edges-list (sort vertexes-to-remove <) (build-list (length vertexes-to-remove) values)))

;3

(define (graph-addition adj-lists)
  (define vertexes (build-list (length adj-lists) values))
  (foldr (λ [adj-list vertex res]
           (cons (remove vertex (remove* adj-list vertexes)) res)) '() adj-lists vertexes))

;4

(define (connected-components adj-lists)
  (define (dfs vertex stack unused-vertexes)
    (foldl (λ [current-vertex stack]
             (if (or (member current-vertex stack) (not (member current-vertex unused-vertexes)))
                 stack
                 (dfs current-vertex (cons current-vertex stack) unused-vertexes))) stack (list-ref adj-lists vertex)))
  (define vertexes (build-list (length adj-lists) values))
  (define (find-components components unused-vertexes vertexes)
    (cond
      [(empty? vertexes) components]
      [(member (car vertexes) unused-vertexes)
       (let* ([maybe-new-component (dfs (car vertexes) '() unused-vertexes)]
              [new-component (if (empty? maybe-new-component) (list (car vertexes)) maybe-new-component)])
         (find-components (cons new-component components) (remove* new-component unused-vertexes) (cdr vertexes)))]
      [else (find-components components unused-vertexes (cdr vertexes))]))
  (find-components '() vertexes vertexes))

;5

(define (BFS a b G)
  (define (iter prosm queue)
    (if (> (list-ref prosm a) 0) (list-ref prosm (car queue))
        (if (empty? queue) #f
            (let* ((pos (car queue))

                   (next (filter (lambda (x) (= 0 (list-ref prosm x)))
                                 (list-ref G pos)))
                   (step (list-ref prosm pos)))
              (if (equal? next #f) (iter prosm (cdr queue))
                  (iter (map (lambda (i)
                               (if (= 0 (list-ref prosm i))
                                   (if (equal? (member i next) #f) 0

                                       (+ step 1))
                                   (list-ref prosm i)))
                             (build-list (length G) values))
                        (append (cdr queue) next)))))))
  (iter (build-list (length G) (λ (i) (if (= i b) 1 0))) (list b)))

(define (find-graph-center adj-lists) ; - основная
  (define (make-dist-matrix adj-lists)
    (define vertexes (build-list (length adj-lists) values))
    (foldr (λ [vertex-from matrix]
             (cons
              (foldr (λ [vertex-to row]
                       (let ([dist (BFS vertex-from vertex-to adj-lists)])
                         (if dist
                             (cons dist row)
                             (cons +inf.0 row)))) '() vertexes)
              matrix)) '() vertexes))
  (define dist-matrix (make-dist-matrix adj-lists))
  (define eccentricity-list (apply map max dist-matrix))
  (define (iter lst center i min)
    (cond
      [(empty? lst) center]
      [(< (car lst) min)
       (iter (cdr lst) (list i) (+ i 1) (car lst))]
      [(= (car lst) min)
       (iter (cdr lst) (cons i center) (+ i 1) min)]
      [else (iter (cdr lst) center (+ i 1) min)]))
  (iter eccentricity-list '() 0 +inf.0))
