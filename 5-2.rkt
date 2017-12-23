#lang racket
(require racket/block)

(struct state (vec pos)
  #:transparent
  )

(define (file->state filename)
  (let (
        (filelist (map string->number (string-split (file->string filename) "\n")))
        )
    (vector->immutable-vector (list->vector filelist))
  )
  )

(define ll (state (file->state "input5.txt") 0))

(define l (state (vector-immutable 0 3 0 1 -3) 0))


(define (proceed currentstate)
  (let* (
         (veccopy (vector-copy (state-vec currentstate)))
         (current-val (vector-ref (state-vec currentstate) (state-pos currentstate)))
         (next-pos (+ (state-pos currentstate) current-val))
         (new-val (if (>= current-val 3)
                      (- current-val 1)
                      (+ current-val 1)
                      ))
         )
     (block 
      (vector-set! veccopy (state-pos currentstate) new-val)
      (state veccopy next-pos)
      )
    )
  )

(define (state-outside? st)
  (or (< (state-pos st) 0) (>= (state-pos st) (vector-length (state-vec st))))
  )

(define (iterate-until-outside st)
  (define (counter beginning-state c)
    (if (state-outside? beginning-state)
        c
        (counter (proceed beginning-state) (+ c 1))
        )
    )
  (counter st 0)
  )
