#lang racket
(require graph)

(define test-str "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
")

(define (parse-inp str)
  (define (node->neighbour-pair l)
    (let (
          (from (string->number (car l)))
          (to (map string->number (map string-trim (string-split (cadr l) ","))))
          )
      (list from to)
      )
    )
  (map node->neighbour-pair (map (lambda (i) (string-split i " <-> ")) (string-split (string-trim str) "\n")))
  )

(define (add-list-of-neighbours g l)
  (let (
        (start (car l))
        (neighbours (cadr l))
        )
    (map (lambda (n) (add-edge! g start n)) neighbours)
    g
    )
  )
               

(define (node-edges->graph l)
  (let (
        (g (undirected-graph '()))
        )
    (map (lambda (q) (add-list-of-neighbours g q)) l)
    g
    )
  )

(define (connected-component-of g x)
  (findf (lambda (c) (findf (lambda (el) (equal? el x)) c)) (cc g))
  )


(define inp-str (file->string "input12.txt"))
(define g (node-edges->graph (parse-inp test-str)))
(define answer (length (connected-component-of g 0)))