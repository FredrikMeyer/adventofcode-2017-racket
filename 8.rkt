#lang racket
(require racket/block)

(define filename "input8.txt")

(define (file->splitlines filename)
  (let (
        (inplist (map string-split (file->lines filename)))
        )
    inplist
    ))

(define register-instructions (file->splitlines filename))


(define register-names (set->list (list->set (map car register-instructions))))

(define register
  (block 
   (define empty-reg (make-hash))
   (map (lambda (name) (hash-set! empty-reg name 0)) register-names)
   empty-reg
   )
  )

(define inc (lambda (a b) (+ a b)))
(define dec (lambda (a b) (- a b)))

(define (parse-operator op)
  (cond ((equal? op ">") >)N
        ((equal? op "<") <)
        ((equal? op ">=") >=)
        ((equal? op "<=") <=)
        ((equal? op "!=") (lambda (a b) (not (equal? a b))))
        ((equal? op "==") equal?)
        (else (error "unknown"))
        )
  )

(define max-so-far 0)
(define (if-greater-than-max-set b)
  (if (> b max-so-far) (set! max-so-far b)
      #f)
  )
         

(define (process-instruction bus instr)
  (let*
      (
       (op (if (equal? (car instr) "inc") inc dec))
       (rest (cdr instr))
       (amount (string->number (car rest)))
       (rrest (cdr (cdr rest)))
       (condition (let
                      ((a (hash-ref register (car rrest)))
                       (b (string->number (cadr (cdr rrest))))
                       )
                    ((parse-operator (cadr rrest)) a b)
                    )
                  )
       )
    (if condition
        (let
            ((new-value (op (hash-ref register bus) amount))
             )
          (if-greater-than-max-set new-value)
          (hash-set! register bus new-value)
          )
        #f
        )
    )
  )

(block 
 (map (lambda (inst) (process-instruction (car inst) (cdr inst))) register-instructions)
 (apply max (hash-values register))
 )
