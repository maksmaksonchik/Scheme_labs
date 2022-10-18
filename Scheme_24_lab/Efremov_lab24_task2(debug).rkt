#lang scheme
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
  (define all-subsets (cons points-list (cdr (drop-right (get-boolean points-list) 1))))
  (define (iter-subsets subsets max-sum result)
    (if (empty? subsets)
        result
        (let* ([current-subset (car subsets)] [current-sum (center-sum-sq current-subset)])
          (if (> current-sum max-sum)
              (iter-subsets (cdr subsets) current-sum (list current-subset))
              (if (= current-sum max-sum)
                  (iter-subsets (cdr subsets) max-sum (cons current-subset result))
                  (iter-subsets (cdr subsets) max-sum result))))))
  (iter-subsets all-subsets 0 '()))