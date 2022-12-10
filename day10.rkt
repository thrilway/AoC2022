#lang racket
(require "day10data.rkt")

(define (part1 in)
  (let loop ((cur (read-line in)) (hold '()) (x 1) (cycle 1) (acc 0))
    (let ((next-acc (if (= (remainder cycle 40) 20) (+ acc (* cycle x)) acc)))
      (cond ((eof-object? cur) next-acc)            
            ((not (null? hold)) (loop (read-line in) '() (+ x hold) (add1 cycle) next-acc))
            ((string=? cur "noop") (loop (read-line in) hold x (add1 cycle) next-acc))
            (else (loop cur (string->number (second (string-split cur " "))) x (add1 cycle) next-acc))))))
(define (part2 in)
  (let loop ((cur (read-line in)) (hold '()) (X 1) (cycle 0) (acc ""))
    (cond ((eof-object? cur) 
           (begin
             (display acc)
             (newline)
             ))
          ((= 40 cycle)
           (begin
             (display acc)
             (newline)
             (loop cur hold X 0 "")))
          ((or (= X cycle) (= (sub1 X) cycle) (= (add1 X) cycle))
           (cond
             ((not (null? hold)) (loop (read-line in) '() (+ X hold) (add1 cycle) (string-append acc "█")))
             ((string=? cur "noop") (loop (read-line in) hold X (add1 cycle) (string-append acc "█")))
             (else (loop cur (string->number (second (string-split cur " "))) X (add1 cycle) (string-append acc "█")))))
          (else
           (cond
             ((not (null? hold)) (loop (read-line in) '() (+ X hold) (add1 cycle) (string-append acc " ")))
             ((string=? cur "noop") (loop (read-line in) hold X (add1 cycle) (string-append acc " ")))
             (else (loop cur (string->number (second (string-split cur " "))) X (add1 cycle) (string-append acc " "))))))))
    

(call-with-input-string data part2)
         
