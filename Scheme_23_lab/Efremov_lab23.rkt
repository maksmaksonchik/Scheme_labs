#lang scheme

;1

(define (build-polygon n)
  (build-list n (lambda [i]
                (let ([angle (/ (* 2 pi i) n)])
                  (cons (fcos angle) (fsin angle))))))

(define (fcos angle)
  (/ (round (* (cos angle) 100000)) 100000))

(define (fsin angle)
  (/ (round (* (sin angle) 100000)) 100000))

;2
;(polygon-triangulation '((0 . 1) (-1 . 0) (0 . -1) (1 .  0)))

(define (polygon-triangulation polygon)
  (define start-point (car polygon))
  (define (iter current-point next-point polygon triangles)
    (if (empty? (cdr polygon))
       (cons (list start-point current-point next-point) triangles)
       (iter next-point (cadr polygon) (cdr polygon) (cons (list start-point current-point next-point) triangles))))
  (iter (cadr polygon) (caddr polygon) (cddr polygon) '()))

;3
;(parallelotope-shell '((1 3 7) (2 -2 4) (5 -1 3) (6 2 4) (-3 5 2) (4 3 -5)))

(define (parallelotope-shell points) 
  (define mins (apply map min points))
  (define maxs (apply map max points))
  (define min-max (map list mins maxs))
  (apply cartesian-product min-max))

;4
;(transformed-polygon? '((2 . 3) (1 . 1) (3 . 1)) '((5 . 1) (9 . 1) (7 . 5))))

(define (scaling-factor polygon1 polygon2)
  (define (compute-aspect-ratio polygon)
    (define xs (map car polygon))
    (define ys (map cdr polygon))
    (define deltax  (- (apply max xs) (apply min xs)))
    (define deltay (- (apply max ys) (apply min ys)))
    (cons deltax deltay))
  (define s1 (compute-aspect-ratio polygon1))
  (define s2 (compute-aspect-ratio polygon2))
  (if (= (/ (car s1) (cdr s1)) (/ (car s2) (cdr s2)))
      (/ (car s2) (car s1))
      #f))

(define (top-left polygon)
  (define (iter polygon top-left-vertex)
    (if (empty? polygon)
        top-left-vertex
        (let ([vertex (car polygon)])
          (cond
            [(< (car vertex) (car top-left-vertex)) (iter (cdr polygon) vertex)]
            [(> (car vertex) (car top-left-vertex))  (iter (cdr polygon) top-left-vertex)]
            [(> (cdr vertex) (cdr top-left-vertex)) (iter (cdr polygon) vertex)]
            [else (iter (cdr polygon) top-left-vertex)]))))
  (iter (cdr polygon) (car polygon)))

(define (~= a b)
  (<= (abs (- a b)) 0.000001))

(define (contain-vertex? vertex polygon)
  (ormap (lambda [other-vertex]
           (and (~= (car vertex) (car other-vertex)) (~= (cdr vertex) (cdr other-vertex)))) polygon))

(define (transformed-polygon? polygon1 polygon2) ; - основная функция
  (define s (scaling-factor polygon1 polygon2))
  (if s
      (let* (
      [top-left1 (top-left polygon1)]
      [top-left2 (top-left polygon2)]
      [xtl (car top-left1)]
      [ytl (cdr top-left1)]
      [Xtl (car top-left2)]
      [Ytl (cdr top-left2)])
        (define (translation vertex)
          (define X (+ (* s (- (car vertex) xtl)) Xtl))
          (define Y (+ (* s (- (cdr vertex) ytl)) Ytl))
          (cons X Y))
        (andmap (lambda [vertex]
                  (contain-vertex? (translation vertex) polygon2)) polygon1))
  #f))

;5
;(center '((5 . 4) (2 . 1) (-3 . 7)))

(define (center triangle)
  (define x1 (caar triangle))
  (define y1 (cdar triangle))
  (define x2 (caadr triangle))
  (define y2 (cdadr triangle))
  (define x3 (caaddr triangle))
  (define y3 (cdaddr triangle))
  (define x12 (- x1 x2))
  (define x23 (- x2 x3))
  (define x31 (- x3 x1))
  (define y12 (- y1 y2))
  (define y23 (- y2 y3))
  (define y31 (- y3 y1))
  (define z1 (+ (* x1 x1) (* y1 y1)))
  (define z2 (+ (* x2 x2) (* y2 y2)))
  (define z3 (+ (* x3 x3) (* y3 y3)))
  (define z (- (* x12 y31) (* y12 x31)))
  (define center-x (- (/ (+ (* y12 z3) (* y23 z1) (* y31 z2)) (* 2 z))))
  (define center-y (/ (+ (* x12 z3) (* x23 z1) (* x31 z2)) (* 2 z)))
  (cons center-x center-y))
