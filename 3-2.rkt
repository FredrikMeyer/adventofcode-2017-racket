#lang racket
(require racket/block)

;;; 8*n tall i avstand n (for n!=0)

(struct point (x y)
  #:transparent
  )

(define points (make-hash))
(hash-set! points (point 0 0) 1)
(define last-value-written 0)



(define (distance p)
  (+ (abs (point-x p)) (abs (point-y p)))
  )

(define (corner? p)
  (and (= (distance p) (* 2 (abs (point-x p)))) (not (= (point-x p) 0)))
  )

(define (adjacent-sum p)
  (foldl (lambda (x y) (+ x y))
         0
         (map (lambda (x)
                (foldl (lambda (x y) (+ x y))
                       0
                       (map (lambda (y)
                              (if (and (= x 0) (= y 0))
                                  0
                                  (hash-ref points (point (+ (point-x p) x) (+ (point-y p) y)) 0)
                                  )
                    
                              ) (list -1 0 1))))
              (list -1 0 1)
              ))
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


       ;(combine-n 10 (lambda (p prev-point) 
        ;              (let ((new-p (next-point p prev-point)))
         ;               (hash-set! points new-p (adjacent-sum new-p))
          ;              new-p)) (point 0 0))


(define (combine-until-larger-than z proc start)
  (define (iter s x y)
    (if (> s z)
        x
        (let* (
            (new-p (proc x y))
            (value-p (adjacent-sum new-p))
            )
          (block (hash-set! points new-p value-p)
                 (set! last-value-written value-p)
           (iter (hash-ref points new-p) new-p x)
           )
          )
        )
    )
  (iter 0 start start)
  )
  
(define (answer n)
  (block
   (combine-until-larger-than n next-point (point 0 0))
   last-value-written
   )
  )

;(distance (combine-n 312050 next-point (point 0 0)))