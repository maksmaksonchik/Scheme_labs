#lang scheme

;1

(define (sparse-sequence sequence)
  (define (iter-sequence sequence sum trace)
    (if (empty? sequence)
        (reverse (car trace))
        (let* ([elem (car sequence)][inclusive-sum (+ elem (cadr sum))]
                                    [non-inclusive-sum (car sum)]) 
          (if (> inclusive-sum non-inclusive-sum)
              (iter-sequence (cdr sequence) (cons inclusive-sum sum) (cons (cons elem (cadr trace)) trace))
              (iter-sequence (cdr sequence) (cons non-inclusive-sum sum) (cons (car trace) trace))))))
  (iter-sequence sequence '(0 0) '(()())))

;2
;(containers '(3 1 4 2 5))

(define (split sequence)
  (define (bsearch-piles x len piles)
    (let aux ([lo 0]
              [hi (- len 1)])
      (if (> lo hi)
          lo
          (let ([mid (quotient (+ lo hi) 2)])
            (if (> (car (list-ref piles mid)) x)
                (aux (+ mid 1) hi)
                (aux lo (- mid 1)))))))
  (define (make-piles sequence len piles)
    (if (empty? sequence) 
        piles
        (let* ([x (car sequence)]
               [index (bsearch-piles x len piles)])
          (if (= index len)
              (make-piles (cdr sequence) (+ len 1) (append piles (list (list x))))
              (make-piles (cdr sequence) len (list-update piles index
                                                          (lambda [pile] (cons x pile))))))))
  (make-piles sequence 0 '()))

(define (containers schedule)
  (define containers-num (length schedule))
  (define splited-containers (split schedule))
  (define splits-num (length splited-containers))
  (define (get-answer i splits result)
    (if (= i 0)
        (cons splits-num result)
        (get-answer (- i 1) (cdr splits)
                    (foldl (lambda [elem result] (list-set result (- elem 1) i)) result (car splits)))))
  (get-answer splits-num splited-containers (build-list containers-num values)))
  
;3
;(classmates n adj-list id)

(define (classmates n adj-list id)
  (define (directed->undirected adj-list)
    (foldl (lambda [node result]
             (foldl (lambda [current-node modified]
                      (list-update modified (- current-node 1)
                                   (lambda [x]
                                     (if (member node x)
                                         x
                                         (cons node x))))) result (list-ref adj-list (- node 1)))) adj-list (build-list n add1)))
  (dfs id #f (directed->undirected adj-list)))
    
    

(define (dfs node parent adj-list)
  (define times (sort (foldl (lambda [current-node result]
                               (if (equal? current-node parent)
                                   result
                                   (cons (dfs current-node node adj-list) result))) '() (list-ref adj-list (- node 1))) >))
  (define (iter-times times i max-t)
    (if (empty? times)
        max-t
        (if (> (+ (car times) i) max-t) 
            (iter-times (cdr times) (+ i 1) (+ (car times) i))
            (iter-times (cdr times) (+ i 1) max-t))))
  (iter-times times 1 0))

  
  
  
  
