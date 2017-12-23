#lang racket

(define filename "input4.txt")

(define s
  "aa bb cc dd ee")

(define ss
  "aa bb cc dd aa")

(define sss
  "aa bb cc dd aaa")

(define (no-repetitions s)
  (let*
      (
       (as-list (string-split s))
       (ignore-anagrams (list->set (map (lambda (q) (list->set (string->list q))) (string-split s))))
       )
    (= (length (set->list ignore-anagrams)) (length as-list))
    )
  )

(define (read-file filename)
 (string-split (file->string filename) "\n")
  )

(length (filter no-repetitions (read-file filename)))