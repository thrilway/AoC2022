#lang racket
(require "day9data.rkt")

(define (touching? c1 c2)
  (if (and (<= (abs (- (car c1) (car c2))) 1)
           (<= (abs (- (cdr c1) (cdr c2))) 1))
      #t
      #f))
(define (catch-up t h)
    (let ((dx (sgn                      ;(sgn n) -> 1 if n>0, -1 if n<0, 0 if n=0
                (- (car h) (car t))))   
          (dy (sgn                      ;(sgn n) -> 1 if n>0, -1 if n<0, 0 if n=0
                (- (cdr h) (cdr t)))))
      (cons (+ (car t) dx) (+ (cdr t) dy))))

(define (part1 in)
  (define (move head tail dir count)
    (let loop ((rem count) (h head) (t tail) (visits '()))
      (if (zero? rem)
          (values h t visits)
          (let
              ((next-h (cond ((string=? dir "U") (cons (car h) (add1 (cdr h))))
                             ((string=? dir "D") (cons (car h) (sub1 (cdr h))))
                             ((string=? dir "R") (cons (add1 (car h)) (cdr h)))
                             ((string=? dir "L") (cons (sub1 (car h)) (cdr h))))))
            (let
                ((next-t (if (touching? t next-h) t (catch-up t next-h))))
              (loop (sub1 rem) next-h next-t (append visits (list next-t))))))))
  (let loop ((cur (read-line in)) (head (cons 0 0)) (tail (cons 0 0)) (acc (list (cons 0 0))))
    (if (eof-object? cur)
        (length acc)
        (let ((args (string-split cur " ")))
          (call-with-values (lambda () (move head tail (first args) (string->number (second args)))) (lambda (h t v) (loop (read-line in) h t (remove-duplicates (append acc v)))))))))

(define (part2 in)
  (define (move head tails dir count)
    (let loop ((rem count) (h head) (t tails) (visits '()))
      (if (zero? rem)
          (values h t visits)
          (let
              ((next-h (cond ((string=? dir "U") (cons (car h) (add1 (cdr h))))
                             ((string=? dir "D") (cons (car h) (sub1 (cdr h))))
                             ((string=? dir "R") (cons (add1 (car h)) (cdr h)))
                             ((string=? dir "L") (cons (sub1 (car h)) (cdr h))))))
            (let i-loop ((lead next-h) (i-rem t) (acc '()))
              (if (null? i-rem)
                  (loop (sub1 rem) next-h acc (append visits (list lead)))
                  (let ((next (if (touching? (car i-rem) lead) (car i-rem) (catch-up (car i-rem) lead))))
                    (i-loop next (cdr i-rem) (append acc (list next))))))))))
  (let loop ((cur (read-line in))  (head (cons 0 0)) (tails (make-list 9 (cons 0 0))) (acc (list (cons 0 0))))
    (if (eof-object? cur)
        (length acc)
        (let ((args (string-split cur " ")))
          (call-with-values (lambda () (move head tails (first args) (string->number (second args)))) (lambda (h t v) (loop (read-line in) h t (remove-duplicates (append acc v)))))))))
          

(call-with-input-string data part2)
