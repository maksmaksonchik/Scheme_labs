#lang scheme

;1
    
(define (R a x y)
  (cond ((> y 0) (abs (- (sqrt (+ (sqr x) (sqr y))) a)))
        ((and (and (> y (- 0 a)) (<= y 0)) (>= (abs x) a)) (- (abs x) a))
        ((and (< y (- 0 a)) (<= (abs x) a)) (- (abs y) a))
        ((and (<= y (- 0 a)) (>= (abs x) a)) (- (sqrt (+ (sqr x) (sqr y))) (sqrt (* 2 (sqr a)))))
        ((and (and (<= y 0) (> y (- 0 a))) (<(abs x) a)) (if (< (- a (abs x)) (- a (abs y))) (- a (abs x))
                                                             (- a (abs y))))))
(R 1 1 -1)

;2

(define (deg h m)
  (abs (- (* m 6) (+ (* m 1/2) (* (remainder h 12) 30)))))

(define (F h m)
  (cond ((> (deg h m) 180) (- 360 (deg h m)))
        ((<= (deg h m) 180) (deg h m))))

;3

(define (d3 x)
  (cond ((= x 1) (display "один"))
        ((= x 2) (display "два"))
        ((= x 3) (display "три"))
        ((= x 4) (display "четыре"))
        ((= x 5) (display "пять"))
        ((= x 6) (display "шесть"))
        ((= x 7) (display "семь"))
        ((= x 8) (display "восемь"))
        ((= x 9) (display "девять"))
        ((= x 10) (display "десять"))
        ((= x 11) (display "одиннадцать"))
        ((= x 12) (display "двенадцать"))
        ((= x 13) (display "тринадцать"))
        ((= x 14) (display "четырнадцать"))
        ((= x 15) (display "пятнадцать"))
        ((= x 16) (display "шестнадцать"))
        ((= x 17) (display "семнадцать"))
        ((= x 18) (display "восемнадцать"))
        ((= x 19) (display "девятнадцать"))
        ((= (quotient x 10) 2) (and (display "двадцать ") (d3(remainder x 10))))
        ((= (quotient x 10) 3) (and (display "тридцать ") (d3(remainder x 10))))
        ((= (quotient x 10) 4) (and (display "сорок ") (d3(remainder x 10))))
        ((= (quotient x 10) 5) (and (display "пятьдесят ") (d3(remainder x 10))))
        ((= (quotient x 10) 6) (and (display "шестьдесят ") (d3(remainder x 10))))
        ((= (quotient x 10) 7) (and (display "семьдесят ") (d3(remainder x 10))))
        ((= (quotient x 10) 8) (and (display "восемьдесят ") (d3(remainder x 10))))
        ((= (quotient x 10) 9) (and (display "девяносто ") (d3(remainder x 10))))
        ((= x 100) (display "сто"))))

;4

(define (bishop x1 y1 x2 y2)
  (if ( = (remainder (+ x1 y1 x2 y2) 2) 1)
      (display "No solution")
      (if (= (abs (- x1 x2)) (abs (- y1 y2)))
          (display "1")
          (display "2"))))

;5

(define (king x1 y1 x2 y2)
  (if ( > (- y1 y2) 1)
      (display "No")
      (if (> (abs (- x1 x2)) y1)
          (display "No")
          (display "Yes"))))
