#lang scheme

;1 - производная многочлена

(define (derivative coef-lst)
  (define (iter n i lst bld)
    (if (= 1 (length lst)) (reverse bld)
        (iter n (+ i 1) (cdr lst) (cons (* (car lst) (- n i)) bld))))
  (iter (- (length coef-lst) 1) 0 coef-lst '()))

;2 - сумма многочленов

(define (polynomials-sum p1 p2)
  (define l-p1 (length p1))
  (define l-p2 (length p2))
  (cond [(= l-p1 l-p2) (map + p1 p2)]
        [(> l-p1 l-p2)
         (map + p1 (append (build-list (- l-p1 l-p2) (λ [x] 0)) p2))]
        [(< l-p1 l-p2)
         (map + p2 (append (build-list (- l-p2 l-p1) (λ [x] 0)) p1))]
        ))

;3 - произведение многочленов

(define (polynomials-prod p1 p2)
  (define L (length p1)) ; - длина первого многочлена
  (foldl (λ [i el1 prod] (map + prod (append (build-list i (λ [x] 0)) (map (λ [el2] (* el1 el2)) p2) (build-list (- (- L 1) i) (λ [x] 0)))))
         (build-list (- (+ L (length p2))1) (λ [x] 0)) (build-list L values) p1))

;4 - системы счисления

(define (sum-of-weird-nums base . nums) ; - сумма н многочленов
  (define (split-number n bld) ; - разделение числа на список цифр
    (if (< n 10) (cons n bld)
        (split-number (quotient n 10) (cons (remainder n 10) bld))))
  (define (deg10 d prod) ; - степень 10
    (if (= d 0) 1
        (if (= d 1) (* 10 prod)
            (deg10 (- d 1) (* prod 10)))))
  (define (polynom->num lst0) 
    (define (iter lst num rem i)
      (if (and (empty? lst) (= 0 rem))  num
          (let ((cur (if (empty? lst) rem (+ (car lst) rem))))
            (iter (if (< (length lst) 1) '() (cdr lst)) (+ num (* (deg10 i 1) (remainder cur base))) (quotient cur base) (+ i 1)))))
    (iter (reverse lst0) 0 0 0))
  (polynom->num (foldl (λ [n s] (let* ((p (split-number n '())) (pol-len (length p)) (sum-len (length s)))
                                  (cond [(> pol-len sum-len) (map + (append (build-list (- pol-len sum-len) (λ [x] 0)) s) p)]
                                        [(< pol-len sum-len) (map + (append (build-list (- sum-len pol-len) (λ [x] 0)) p) s)]
                                        [(= pol-len sum-len) (map + p s)])))
                       '() nums)))

;5 - Метод Ньютона

(define (Newton-method p x0 e)
  (define (Horner p x)
    (define (iter p prod) 
      (if (empty? p) prod
          (iter (cdr p) (+ (* x prod) (car p)))))
    (iter (cdr p) (car p)))
  (define (next-Newton p xn)
    (- xn (/ (Horner p xn) (Horner (derivative p) xn))))
  (define (iter xn)
    (let ((xn+1 (next-Newton p xn))) (if (< (abs (- xn+1 xn)) e) xn
                                         (iter xn+1))))
  (iter x0))
