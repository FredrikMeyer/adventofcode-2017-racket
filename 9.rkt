#lang racket

(define inp (string-trim (file->string "input9.txt")))

(struct group (groups) #:transparent)

(define g (group (list (group '()) (group '()))))

(define (remove-garbage s)
  (let* (
         (explams-cancelled (regexp-replace* #rx"!." s ""))
         (garbage-removed (regexp-replace* #rx",?<[^>]*>,?" explams-cancelled ""))
         )
    
    (string-replace garbage-removed "," "")
    
    )
  )

;; given start index of {, find index of matching }
(define (find-closing-bracket str start)
  (define (iter current-index l-found r-found)
    (cond ((= l-found r-found) (- current-index 1))
          ((equal? (string-ref str current-index) #\{)
           (iter (+ current-index 1) (+ l-found 1) r-found))
          ((equal? (string-ref str current-index) #\})
           (iter (+ current-index 1) l-found (+ r-found 1)))
          (else
           (iter (+ current-index 1) l-found r-found))
          )
    )
  (iter (+ start 1) 1 0)
  )

(define (string-is-atomic-group str)
  (= (find-closing-bracket str 0) 1)
  )
           
;(define (string->groups str)
;(cond ((string-is-atomic-group str) (group '()))
;
;
(define prep-inp (remove-garbage inp))

;; String->List
;; Ex
;; {} -> '()
;; {{},{}} -> '({}, {})
(define (find-sub-expressions str)
  (let ((max-ind (- (string-length str) 1)))
    (define (iter current-ind)
      (if (>= current-ind max-ind) '()
          (let ((next-ind (find-closing-bracket str current-ind))
                )
            (cons (substring str current-ind (+ next-ind 1)) (iter (+ 1 next-ind)))
            )
          )
      )
    (iter 1))
  )

(struct node (children) #:transparent)

(define (expression->tree expr)
  (if (empty? (find-sub-expressions expr))
      (node '())
      (node (map expression->tree (find-sub-expressions expr)))
      )
  )

(define (score-tree n)
  (define (iter current-score m)
    (if (empty? (node-children m))
        current-score
        (+ current-score (foldl + 0 (map (lambda (c) (iter (+ 1 current-score) c)) (node-children m))))
        ))
  (iter 1 n)
  )

(score-tree (expression->tree prep-inp))

;; part 2

(define (count-garbage str)
  (let* (
        (after-cancel (regexp-replace* #rx"!." str ""))
        (garbages (regexp-match* #rx"<[^>]*>" after-cancel))
        )
    (foldl (lambda (g acc) (- (+ g acc) 2)) 0 (map string-length garbages))
    )
  )
    
            