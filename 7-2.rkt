#lang racket

(define testf "input7t.txt")
(define inpf "input7.txt")

(define (file->flattree filename)
  (inputs->nodes (map (lambda (s)
                        (let (
                              (splitted (string-split s "->"))
                              )                
                          (cons (parse-program (car splitted)) (parse-children (cdr splitted)))))
                      (string-split (file->string filename) "\n")))
  )

(define (string->program str list-of-programs)
  (car (filter (lambda (p) (equal? (program-name p) str)) list-of-programs))
  )

(define (inputs->nodes inps)
  (let (
        (list-of-programs (map car inps))
        (cadr2 (lambda (l) (if (> (length l) 1) (cdr l) '())))
        )
    (map (lambda (x) (node (car x) (map (lambda (s) (string->program s list-of-programs)) (cadr2 x)))) inps)
    )
  )

(define (parse-program s)
  (let* (
         (splitted (string-split s))
         (length-weight (string-length (cadr splitted)))
         (weight (string->number (substring (cadr splitted) 1 (- length-weight 1))))
         )
    (program (car splitted) weight)
    )
  )

(define (parse-children l)
  (if (empty? l)
      l
      (string-split (string-trim (car l)) ", ")
      )
  )

(define (find-parent flattree program)
  (define (iter l)
    (if (empty? l) '()
        (if (findf (lambda (p) (equal? p program)) (node-children (car l)))
            (car l)
            (iter (cdr l))
            )
        )
    )
  (iter flattree)
  )

(define (find-node flattree program)
  (define (iter l)
    (if (empty? l) '()
        (if (equal? (node-content (car l)) program)
            (car l)
            (iter (cdr l))
            )
        )
    )
  (iter flattree)
  )

(define (find-bottom flat-tree)
  (car (filter (lambda (n) (not (node? (find-parent flat-tree (node-content n))))) flat-tree))
  )

(define (flattree->tree flat-tree bottom)
  (if (empty? bottom) '()
      (node (node-content bottom)
            (map (lambda (n) (flattree->tree flat-tree n))
                 (map (lambda (p) (find-node flat-tree p)) (node-children bottom)))
            )
      )
  )

(define (map-tree fn tree)
  (node (fn (node-content tree))
        (if (empty? (node-children tree)) '()
            (map (lambda (n) (map-tree fn n)) (node-children tree))
            )
        )
  )

(define (map-tree-node fn tree)
  (node (fn tree)
        (if (empty? (node-children tree)) '()
            (map (lambda (n) (map-tree-node fn n)) (node-children tree))
            )
        )
  )
                    

(define (tree-reduce fn st tree)
  (if (empty? (node-children tree))
      (fn st (node-content tree))
      (foldl fn (node-content tree) (map (lambda (t) (tree-reduce fn st t)) (node-children tree)))
      )
  )

(define (tree-filter pred tree)
  (if (empty? (node-children tree))
      (if (pred (node-content tree))
          (node-content tree)
          '()
          )
      (if (pred (node-content tree))
          (cons (node-content tree) (map (lambda (n) (tree-filter pred n)) (node-children tree)))
          (map (lambda (n) (tree-filter pred n)) (node-children tree))
          )
      )
  )

(define (tree-filter-node pred tree)
  (if (empty? (node-children tree))
      (if (pred tree)
          tree
          '()
          )
      (if (pred tree)
          (cons tree (map (lambda (n) (tree-filter-node pred n)) (node-children tree)))
          (map (lambda (n) (tree-filter-node pred n)) (node-children tree))
          )
      )
  )
          
              

(define (node-weight node)
  (tree-reduce + 0 (map-tree program-weight node))
  )

(define (all-equal? l)
  (define (iter seq m)
    (if (empty? seq) #t
        (if (equal? m (car seq))
            (iter (cdr seq) m)
            #f
            )
        )
    )
  (if (empty? l) #t
      (iter l (car l))
      )
  )

(struct program (name weight) #:transparent)
(struct node (content children) #:transparent)

(define ft (file->flattree testf))
(define ift (file->flattree inpf))

(define tt (flattree->tree ft (find-bottom ft)))
(define ff (flattree->tree ift (find-bottom ift)))


(define (is-balanced n)
  (all-equal? (map node-weight (node-children n)))
  )

;; finn inbalanced node with balanced children

(define (all xs)
  (cond
    [(empty? xs) #t]
    [(pair?  xs) (and (first xs) (all (rest xs)))]
    [else        (error 'all "expected a list, got: " xs)]))

(define (have-balanced-children n)
   (all (map is-balanced (node-children n)))
  )

(define (problem-child tree)
  (if (and (have-balanced-children tree) (not (is-balanced tree)))
      tree
      (flatten (map problem-child (node-children tree)))
      )
  )

(problem-child ff) ;; jfrda

(map-tree-node node-weight (car (problem-child ff)))

;; Note that the weights below jfrda are 1437, 1429, 1429. So lnpuarm is 8 too much.
;; That is 910 is the right answer.