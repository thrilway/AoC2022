#lang racket
(require racket/mpair)
(require "day2data.rkt")

(define winners
    (list
     (cons 'R 'S)
     (cons 'S 'P)
     (cons 'P 'R)))
(define (rps them you)  
  (cond
    ((equal? them you) 'draw)
    ((member (cons you them) winners) 'win)
    (else 'loss)))

(define (part1 lst)
  (define (conv code)
    (cond
      ((member code (list 'A 'X)) 'R)
      ((member code (list 'B 'Y)) 'P)
      ((member code (list 'C 'Z)) 'S)))
  (define (score game)
    (let ((you (conv (cdr game)))
          (them (conv (car game))))
      (let
          ((base (cond
                  ((equal? you 'R) 1)
                  ((equal? you 'P) 2)
                  ((equal? you 'S) 3)))
           (res (rps them you)))
        (+ base
           (cond ((equal? res 'win) 6)
                 ((equal? res 'draw) 3)
                 (else 0))))))
  (let loop ((rem lst) (acc 0))
    (if
     (null? rem)
     acc
     (loop (cdr rem) (+ acc (score (car rem)))))))

(define (part2 lst)
  (define scores (hash 'A 1 'B 2 'C 3))
  (define (win them)
    (case them
      ((A) 'B)
      ((B) 'C)
      ((C) 'A)))
  (define (lose them)
    (case them
      ((A) 'C)
      ((B) 'A)
      ((C) 'B)))
  (define (play game)
    (case (cdr game)
      ((X) (hash-ref scores (lose (car game))))
      ((Y) (+ 3 (hash-ref scores (car game))))
      ((Z) (+ 6 (hash-ref scores (win (car game)))))))
  (let loop ((rem lst) (acc 0))
    (if
     (null? rem)
     acc
     (loop (cdr rem) (+ acc (play (car rem)))))))
(part2 data)
      
  