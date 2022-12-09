#lang racket

(require "day4data.rkt")

(define (part1 lst)
  (define (either-contains? r1 r2)
    (let ((lower (min (car r1) (car r2)))
          (upper (max (cdr r1) (cdr r2))))
      (if (or
           (equal? (cons lower upper) r1)
           (equal? (cons lower upper) r2))
          #t
          #f
          )))
  (let loop ((rem lst) (acc 0))
    (cond
      ((null? rem) acc)
      ((either-contains? (caar rem) (cdar rem)) (loop (cdr rem) (add1 acc)))
      (else (loop (cdr rem) acc)))))

(define (part2 lst)
  (define (overlap? r1 r2)
    (if
     (or
      (and
       (>= (car r2) (car r1))
       (>= (cdr r1) (car r2)))
      (and
       (>= (car r1) (car r2))
       (>= (cdr r2) (car r1))))
     #t
     #f))
  (let loop ((rem lst) (acc 0))
    (cond ((null? rem) acc)
          ((overlap? (caar rem) (cdar rem))
           (loop (cdr rem) (add1 acc)))
          (else (loop (cdr rem) acc)))))
       

(part2 test-data)
(part2 data)
