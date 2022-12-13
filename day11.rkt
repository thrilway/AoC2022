#lang racket

(require "day11data.rkt" math/number-theory)

(define (process str)
  (let ((monkeys (make-hash)))
    (let loop ((rem (map string-trim (string-split str "\n"))) (monkey-n '()))
      (cond ((null? rem) monkeys)
            ((string=? (car rem) "")
             (loop (cdr rem) '()))
            ((string-prefix? (car rem) "Monkey")
             (let ((n (string->number (string-trim (second (string-split (car rem))) ":"))))
               (begin
                 (hash-set! monkeys n (make-hash))
                 (loop (cdr rem) n))))
          ((string-prefix? (car rem) "Starting items:")
           (let ((is (map (lambda (x) (string->number (string-trim x ","))) (cddr (string-split (car rem))))))
             (begin
               (hash-set! (hash-ref monkeys monkey-n) 'items is)
               (loop (cdr rem) monkey-n))))
          ((string-prefix? (car rem) "Operation:")
           (let ((operands (take-right (string-split (car rem)) 2)))
             (begin
               (hash-set! (hash-ref monkeys monkey-n) 'op (car operands))
               (hash-set! (hash-ref monkeys monkey-n) 'operand (cadr operands))
               (loop (cdr rem) monkey-n))))
          ((string-prefix? (car rem) "Test:")
           (let ((div (string->number (last (string-split (car rem)))))
                 (y (string->number (last (string-split (cadr rem)))))
                 (n (string->number (last (string-split (caddr rem))))))
             (begin
               (hash-set! (hash-ref monkeys monkey-n) 'div div)
               (hash-set! (hash-ref monkeys monkey-n) 'yes y)
               (hash-set! (hash-ref monkeys monkey-n) 'no n)
               (loop (drop rem 3) monkey-n))))))))

(define (run h rounds)
  (define cycle
    (apply * (map (lambda (x) (hash-ref x 'div)) (hash-values h))))
  (define (inspect m)
    (for/fold
     ((out 0)
      #:result out)
     ((item (hash-ref m 'items)))
      (begin
        (let ((next
               ((if (string=? (hash-ref m 'op) "*")
                           *
                           +)
                       item
                       (if (string=? (hash-ref m 'operand) "old")
                           item
                           (string->number (hash-ref m 'operand))))))
          (if (zero? (remainder next (hash-ref m 'div)))
              (hash-update! (hash-ref h (hash-ref m 'yes)) 'items (lambda (x) (cons (remainder next cycle) x)) '())
              (hash-update! (hash-ref h (hash-ref m 'no)) 'items (lambda (x) (cons (remainder next cycle) x)) '())))
      (add1 out))))
          (let loop ((cur 0) (rem rounds) (acc (hash)))
            (cond ((zero? rem)
                   (begin
                     (display acc)
                     (newline)
                     (apply * (take (sort (hash-values acc) >) 2))))
                  ((not (hash-has-key? h cur)) (loop 0 (sub1 rem) acc))
                  (else
                   (let ((x (inspect (hash-ref h cur))))
                     (begin
                       (hash-set! (hash-ref h cur) 'items '())
                       (loop (add1 cur) rem (hash-update acc cur (lambda (n) (+ n x)) 0))))))))
                     
;(display (process test-data))
(run (process data) 10000)

;(class? monkey%)
