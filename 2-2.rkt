#lang racket

(define s
  "5 9 2 8
9 4 7 3
3 8 6 5")

(define (read-inp-str s)
  (map (lambda (l) (map string->number l)) (map string-split (string-split s "\n")))
  )

(define l (read-inp-str s))
(define ll
  (read-inp-str (file->string "input2.txt")))

(define (find-div d l)
  (cond ((empty? l) l)
        ((= (car l) d) (find-div d (cdr l)))
        ((= (* (remainder (car l) d) (remainder d (car l))) 0)
         (let ((p (list (car l) d)))
           (cons (apply min p) (apply max p))
           )
         )
        (else (find-div d (cdr l)))
        )
  )

(define (find-div-pair l)
  (define (find-div-pair-iter seq)
    (cond ((empty? seq) seq)
          ((empty? (find-div (car seq) l))
           (find-div-pair-iter (cdr seq))
           )
          (else (find-div (car seq) l))
          )
    )
  (find-div-pair-iter l)
  )
                        

(define (compute-check-sum l)
  (foldl
   (lambda (x y) (+ x y))
   0
   (map (lambda (p) (/ (cdr p) (car p)))
        (map find-div-pair l)))
  )