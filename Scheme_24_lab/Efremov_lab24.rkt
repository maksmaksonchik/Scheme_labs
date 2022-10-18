#lang scheme

;1

(define (get-transpositions lst) ; - вспомогательная
  (define (iter lst list-end)
    (if (empty? lst)
        (list list-end)
        (append-map (λ [x] (iter (remove x lst =) (cons x list-end))) lst)))
  (iter lst '()))

(define (max-gcd-sum lst) ; - основная функция
  (define (gcd-sum lst)
    (define (iter lst sum)
      (if (empty? (cdr lst))
          sum
          (iter (cdr lst) (+ sum (gcd (car lst) (cadr lst))))))
    (iter lst 0))
  (define transpositions (get-transpositions lst))
  (cdr (foldl (λ [transposition answer] (let ([current-sum (gcd-sum transposition)])
                                          (if (> current-sum (car answer))
                                              (cons current-sum transposition)
                                              answer)))
              '(0) transpositions)))

;2
;(find-subsets '((5 . 5) (-5 . 5) (-5 . -5) (5 . -5)))

(define (center-sum-sq points-list)
  (define center (cons (/ (apply + (map car points-list)) (length points-list)) (/ (apply + (map cdr points-list)) (length points-list))))
  (define (distance-to-center point)
    (+ (sqr (- (car point) (car center))) (sqr (- (cdr point) (cdr center)))))
  (foldl (λ [point sum]
           (+ sum (distance-to-center point))) 0 points-list))
           
(define (get-boolean lst)
  (if (empty? lst)
      '(())
      (append-map (λ [x] (list x (cons (car lst) x))) (get-boolean (cdr lst)))))

(define (find-subsets points-list) ; - главная функция
  (define proper-subsets (cdr (drop-right (get-boolean points-list) 1)))
  (define (iter-subsets subsets max-sum result)
    (if (empty? subsets)
        result
        (let* ([current-subset (car subsets)] [current-sum (center-sum-sq current-subset)])
          (if (> current-sum max-sum)
              (iter-subsets (cdr subsets) current-sum (list current-subset))
              (if (= current-sum max-sum)
                  (iter-subsets (cdr subsets) max-sum (cons current-subset result))
                  (iter-subsets (cdr subsets) max-sum result))))))
  (iter-subsets proper-subsets 0 '()))

;3
;(gen-max-chain '(1 2 4 6 3 5 7 8) 12)

(define (gen-max-chain lst n)
  (define (max-len-list lst1 lst2)
    (if (> (length lst1) (length lst2)) lst1 lst2))
  (define (max-chain lst curr-max result)
    (if (empty? lst)
        (reverse (max-len-list curr-max result))
        (if (= (gcd (car curr-max) (car lst)) 1)
            (if (= (length curr-max) 1)
                (max-chain (cdr lst) curr-max result)
                (max-chain lst (list n) (max-len-list curr-max result)))
            (max-chain (cdr lst) (cons (car lst) curr-max) result))))
  (argmax length (foldl (λ [transposition chains]
                          (cons (max-chain transposition (list n) (list n)) chains)) '() (get-transpositions lst))))
