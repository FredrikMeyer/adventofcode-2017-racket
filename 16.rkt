#lang racket
(require racket/block)

(define (parse-command str)
  (cond ((string-prefix? str "s")
         (rotate-vector-by-amount (string->number (substring str 1 (string-length str))))
         )
        ((string-prefix? str "x")
         (let* (
                (ij (map string->number (string-split (substring str 1 (string-length str)) "/")))
                (i (car ij))
                (j (cadr ij))
                )
           (exchange-vector-indices i j)
           )
         )
        ((string-prefix? str "p")
         (let* (
                (ab (string-split (substring str 1 (string-length str)) "/"))
                (a (car ab))
                (b (cadr ab))
                )  
         (exchange-vector-elements a b)
           )
         )
        )
  )


(define (rotate-vector-by-amount amount)
  (lambda (v)
    (let (
          (w (vector-copy v))
          (len (vector-length v))
          )
      (define (place k)
        (cond ((>= k 0)
               (block 
                (vector-set! w (remainder (+ k amount) len) (vector-ref v k))
                (place (- k 1))
                )
               ))
        )
      (place (- len 1))
      w
      )
    ))

(define (exchange-vector-indices i j)
  (lambda (v)
    (let
        (
         (w (vector-copy v))
         (el-i (vector-ref v i))
         (el-j (vector-ref v j))
         )
      (vector-set! w i el-j)
      (vector-set! w j el-i)
      w
      )
    )
  )

(define (exchange-vector-elements a b)
  (lambda (v)
    (let
        (
         (a-index (vector-member a v))
         (b-index (vector-member b v))
         )
      ((exchange-vector-indices a-index b-index) v)
      )
    ))

(define inp (list->vector (map string (string->list "abcdefghijklmnop"))))
;(define inps (string-split "s1,x3/4,pe/b" ","))
(define inps (string-split (string-trim (file->string "input16.txt")) ","))

(apply string-append (vector->list (foldl (lambda (s w) ((parse-command s) w)) inp inps)))