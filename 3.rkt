#lang racket
(require racket/block)

;;; 8*n tall i avstand n (for n!=0)

(struct point (x y) #:transparent)
(define (distance p)
  (+ (abs (point-x p)) (abs (point-y p)))
  )

(define (corner? p)
  (and (= (distance p) (* 2 (abs (point-x p)))) (not (= (point-x p) 0)))
  )

(define (next-point p prev-point)
  (let ((x (point-x p))
        (y (point-y p))
        (px (point-x prev-point))
        (py (point-y prev-point))
        )
    (if (and (= x 0) (= y 0))
        (point 1 0)
        (cond ((and (= x y) (> x 0))
               (point (- x 1) y))
              ((and (= (* -1 x) y) (> y 0))
               (point x (- y 1)))
              ((and (= x y) (< x 0))
               (point (+ x 1) y))
              ((and (= (* -1 x) y) (< y 0))
               (point (+ x 1) y))
              (else (cond ((and (corner? prev-point) (> x 0) (< y 0))
                           (point x (+ y 1)))
                          ((and (= x px) (> x 0))
                           (point x (+ y 1)))
                          ((and (= y py) (> y 0))
                           (point (- x 1) y))
                          ((and (= x px) (< x 0))
                           (point x (- y 1)))
                          ((and (= y py) (< y 0))
                           (point (+ x 1) y))
                          (else (point 1 1))
                          )
                    )
              )
        )
    ))

(define (combine-n n proc start)
  (define (iter k x y)
    (if (= k 0)
        x
        (iter (- k 1) (proc x y) x)
        )
    )
  (iter n start start)
  )

(distance (combine-n 312050 next-point (point 0 0)))