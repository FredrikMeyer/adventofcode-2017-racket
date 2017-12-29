#lang racket

(define (inp->complex-list inp)
  (map dir->cplx (string-split inp ","))
  )

(define (dir->cplx str)
  (define (str->non-unit str)
    (cond ((equal? str "ne")
           (+ 1 (/ (+ 1 (sqrt -3)) 2)))
          ((equal? str "n")
           (sqrt -3))
          ((equal? str "nw")
           (+ -1 (/ (+ -1 (sqrt -3)) 2)))
          ((equal? str "sw")
           (+ -1 (/ (- -1 (sqrt -3)) 2)))
          ((equal? str "s")
           (* -1 (sqrt -3)))
          ((equal? str "se")
           (+ 1 (/ (- 1 (sqrt -3)) 2)))
          )
    )
  (let* (
         (cpx (str->non-unit str))
         (mag (magnitude cpx))
         )
    (/ cpx mag)
    )
  )


(define (greedy-direction start goal)
  (let (
        (alts (list  "ne" "n" "nw" "sw" "s" "se"))
        )
    (argmin (lambda (d) (magnitude (- goal (+ start (dir->cplx d))))) alts)
    )
  )

(define (number-of-steps start goal)
  (if (< (magnitude (- start goal)) 0.5)
      0
      (+ 1 (number-of-steps (+ start (dir->cplx (greedy-direction start goal))) goal))
      )
  )

(define (file->commands filename)
  (string-split (string-trim (file->string filename)) ",")
  )

;(number-of-steps 0 (apply + (map dir->cplx (list "se" "sw" "se" "sw" "sw"))))

(define (compute-max-dist l)
  (let (
        (len (length l))
        )
    (apply max (map (lambda (n) (number-of-steps 0 (apply + (map dir->cplx (take l n)))))
         (range 1 (length l))))
    ))