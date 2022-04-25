#lang scheme

;1.1

(define (index-pairs lst)
  (map (λ (lst1 idx) (cons lst1 idx)) lst (build-list (length lst) add1)))

;1.2

(define (div-by-idx lst)
  (filter (λ (pr) (= 0 (remainder (car pr) (cdr pr)))) (index-pairs lst)))

;2

(define (3-dig-sum lst)
  (foldl (λ (x s) (if (and (< x 1000) (> x 99)) (+ s x) s)) 0 lst))

;3

(define (operations nums funcs)
  (foldl (λ (num func res) ((eval func) res num)) (car nums) (cdr nums) funcs))

;4

(define (list-of-lists lst)
  (reverse (foldl (λ (bld res) (cons (drop lst bld) res)) '() (build-list (length lst) values))))

;5

(define (bank sum r operations)
  (define R (+ 1 (/ r 100)))
  (define days-with-oper (remove-duplicates (map (λ (oper) (car oper)) operations)))
  (define (day-sum sum day)
    (* R (foldl (λ (oper sum)
                  ((eval (cadr oper)) sum (caddr oper)))
                sum (filter (λ (oper) (= (car oper) day)) operations))))
  (foldl (λ (day sum)
           (if (member day days-with-oper)
               (day-sum sum day)
               (* sum R)))
         sum (build-list (- 31 (caar operations)) (λ(x)(+ (caar operations) x)))))
