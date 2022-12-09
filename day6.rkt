#lang racket

(require "day6data.rkt")

(define (part1 str)
  (let loop ((rem (string->list str)) (window '()) (i 0))
    (cond
      ((< (length window) 4) (loop (cdr rem) (cons (car rem) window) (add1 i)))
      ((equal? window (remove-duplicates window)) i)
      (else (loop (cdr rem) (cons (car rem) (take window 3)) (add1 i))))))

(define (part2 str)
  (let loop ((rem (string->list str)) (window '()) (i 0))
    (cond
      ((< (length window) 14) (loop (cdr rem) (cons (car rem) window) (add1 i)))
      ((equal? window (remove-duplicates window)) i)
      (else (loop (cdr rem) (cons (car rem) (take window 13)) (add1 i))))))

(for
    ((str (in-list test-data-lst)))
  (display (part2 str))
  (newline))

(part2 data)
   