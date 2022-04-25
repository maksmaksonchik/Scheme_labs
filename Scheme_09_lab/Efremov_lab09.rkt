#lang scheme

;1

(define (max-zeros-row mtx)
  (define (num-of-zeros row)
    (foldl (λ (x num) (if (= x 0) (+ num 1) num)) 0 row))
  (cdr (foldl (λ (row res)
                (let ([cur-num (num-of-zeros row)]
                      [max-num (car res)])
                  (if (> cur-num max-num)
                      (cons cur-num row)
                      res))) (cons 0 #f) mtx)))

;2

(define (sandglass-matrix n)
  (build-list n (λ (i) (build-list n (λ (j) (if (< i (/ n 2))
                                                (if (and (> j i) (< j (- (- n 1) i))) 0 1)
                                                (if (and (> j (- (- n 1) i)) (< j i)) 0 1)))))))

;3

(define (saddle-points mtx)
  (define result
    (foldl (λ (col result)
             (append result (foldl (λ (x raw res)
                                     (if (and (= x (apply max col)) (= x (apply min raw)))
                                         (cons x res)
                                         res))
                                   '() col mtx)))
           '() (apply map list mtx)))
  (if (empty? result) #f result))

;4

(define (magic-square? mtx)
  (define (list-of-str-sum mtx)
    (map (λ (lst) (foldl (λ (x s) (+ x s)) 0 lst)) mtx))
  (define (list-of-col-sum mtx)
    (foldl (λ (lst res) (map + res lst)) (build-list (length mtx) (λ (x) 0)) mtx))
  (define (main-diag-sum mtx)
    (foldl (λ (lst idx s) (+ s (list-ref lst idx))) 0 mtx (build-list (length mtx) values)))
  (define (side-diag-sum mtx)
    (foldl (λ (lst idx s) (+ s (list-ref lst (- (- (length lst) 1) idx)))) 0 mtx (build-list (length mtx) values)))
  (andmap (λ (x) (= x (main-diag-sum mtx))) (cons (side-diag-sum mtx) (append (list-of-str-sum mtx) (list-of-col-sum mtx)))))

;5

(define (row=col mtx)
  (define (transpose mtx)
    (apply map list mtx))
  (foldl (λ (col res) (if res res (if (ormap (λ (row) (equal? row col)) mtx) col #f)))
         #f (transpose mtx)))
