#lang scheme

;1

(define (sum-matrix mtx1 mtx2)
  (map (λ (str1 str2) (map (λ (lst1 lst2) (+ lst1 lst2)) str1 str2)) mtx1 mtx2))

;2

(define (nonnegative-matrix? mtx)
  (andmap (λ (lst) (andmap  (λ (x) (>= x 0)) lst)) mtx))

;3

(define (low-tri-matrix? mtx)
  (andmap (λ (lst i) (andmap (λ (x j) (or (>= i j) (= x 0))) lst (build-list (length lst) add1))) mtx (build-list (length mtx) add1)))

;4

(define (diog-dominant-matrix? mtx)
  (and 
   (andmap (λ (lst j) (>= (abs (list-ref lst (- j 1))) (- (foldl (λ (x s) (if (>= x 0) (+ s x) (- s x)))
                                                                 0 lst) (abs (list-ref lst (- j 1))))))
           mtx (build-list (length mtx) add1))
   (ormap (λ (lst j) (> (abs (list-ref lst (- j 1))) (- (foldl (λ (x s) (if (>= x 0) (+ s x) (- s x)))
                                                               0 lst) (abs (list-ref lst (- j 1))))))
          mtx (build-list (length mtx) add1))))

;5

(define (n-1-matrix n)
  (cons (build-list (- n 1) add1)
        (map (λ (x) (append (member (- n x) (build-list (- n 1) add1)) (build-list (- (- n 1) x) add1)))
             (build-list (- n 2) add1))))

;6

(define (zero-str mtx)
  (foldl (λ (lst num) (if (andmap (λ (x) (= x 0)) lst) (+ 1 num) num)) 0 mtx))
