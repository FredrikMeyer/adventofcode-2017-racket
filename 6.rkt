#lang racket
(require racket/block)

(define ll (vector 4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3))
;(define l (vector 0 2 7 0))

(define (largest-index vec)
  (define (walker k maxval maxindex)
    (if (>= k 0)
        (let ((val-at-k (vector-ref vec k))
              )
          (if (>= val-at-k maxval)
              (walker (- k 1) val-at-k k)
              (walker (- k 1) maxval maxindex)
              )
          )
        maxindex
        )
    )
  (walker (- (vector-length vec) 1) 0 0)
  )

(define (redistribute l i)
  (let (
        (val (vector-ref l i))
        (len (vector-length l))
        )
    (block
     (vector-set! l i 0)
     (define (walker ind left)
       (if (= left 0)
           l
           (let*
               ((current-ind (remainder (+ ind 1) len))
                (current-val (vector-ref l current-ind))
                )
             (block
              (vector-set! l current-ind (+ current-val 1))
              (walker current-ind (- left 1))
              )
             )
           )
       )
     (walker i val)
     )    
    )
  )

;(redistribute l 2)

;(define seen-states (set (vector-copy ll)))
(define (redistribute-until-loop vec times seen-states)
  (let*
      ((max-index (largest-index vec))
       (res (redistribute vec max-index))
       )
    (if (set-member? seen-states res)
        (cons times res)
        (block                
         (redistribute-until-loop res (+ times 1) (set-add seen-states res))
         )
        )
    )
  )

(redistribute-until-loop ll 1 (set (vector-copy ll)))
(redistribute-until-loop (vector 0 14 13 12 11 10 8 8 6 6 5 3 3 2 1 10) 1 (set (vector-copy (vector 0 14 13 12 11 10 8 8 6 6 5 3 3 2 1 10))))