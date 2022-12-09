#lang racket

(require "day1data.rkt")

(define (process in)
  (let loop ((cur (read-line in)) (acc 0) (top 0))
    (cond ((eof-object? cur) top)
          ((string=? cur "") (loop (read-line in)  0 (max acc top)))
          (else
           (loop (read-line in) (+ acc (string->number cur)) top)))))

(define (part-two in)
  (let loop ((cur (read-line in)) (acc 0) (top '(0 0 0)))
    (cond ((eof-object? cur) (apply + (cons (max acc (car top)) (cdr top))))
          ((string=? cur "") (loop (read-line in)  0 (sort (cons (max acc (car top)) (cdr top)) <)))
          (else
           (loop (read-line in) (+ acc (string->number cur)) top)))))

(call-with-input-string data part-two)
  