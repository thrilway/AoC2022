#lang racket
(require "day3data.rkt")

(define (priority ch)
  (if (char-lower-case? ch)
      (- (char->integer ch) 96)
      (- (char->integer ch) 38)))


(define (part1 in)
  (for/fold ((out '())
             #:result (apply + (map priority out)))
            ((line (in-lines in)))
    (append out
            (set->list
             (apply set-intersect
                    (map list->set
                         (call-with-values
                          (lambda ()
                            (split-at (string->list line) (/ (string-length line) 2)))
                          list)))))))
(define (part2 str)
  (define (string->set s)
    (list->set (string->list s)))
  (define (singleton st)
    (car (set->list st)))
  (let loop ((rem (string-split str "\n")) (acc 0))
    (if (null? rem)
        acc
        (loop (drop rem 3) (+ acc
                              (priority
                               (singleton
                                (apply set-intersect
                                       (map string->set (take rem 3))))))))))

(part2 data)
  