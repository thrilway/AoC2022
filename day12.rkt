#lang racket
(require "day12data.rkt" graph)

(define (process in)
  (for/fold
   ((out (hash))
    #:result out)
   ((line (in-lines in))
    (y (in-naturals)))
    (for/fold
     ((acc out)
      #:result acc)
     ((ch (in-string line))
      (x (in-naturals)))
      (cond ((char=? ch #\S) (hash-set (hash-set acc 'start (cons x y)) (cons x y) 0))
            ((char=? ch #\E) (hash-set (hash-set acc 'end (cons x y)) (cons x y) (- (char->integer #\z) 95)))
            (else (hash-set acc (cons x y) (- (char->integer ch) 96)))))))

(define (adjacents c)
  (list
   (cons (car c) (add1 (cdr c)))
   (cons (car c) (sub1 (cdr c)))
   (cons (add1 (car c)) (cdr c))
   (cons (sub1 (car c)) (cdr c))))

(define (grid->graph grid)
  (let loop ((rem (filter pair? (hash-keys grid))) (acc '()))
    (if (null? rem)
        (unweighted-graph/directed acc)
        (let ((adj (filter (lambda (x) (and
                                        (hash-has-key? grid x)
                                        (<= (- (hash-ref grid x) (hash-ref grid (car rem))) 1)))
                           (adjacents (car rem)))))
          (loop (cdr rem) (append acc (map (lambda (x) (list (car rem) x)) adj)))))))
(define (part1 grid)
  (let ((g (grid->graph grid)) (s (hash-ref grid 'start)) (e (hash-ref grid 'end)))
    (let-values (((counts points) (dijkstra g s)))
      (hash-ref counts e))))
(define (part2 grid)
  (let ((g (grid->graph grid)) (e (hash-ref grid 'end)))
    (let loop ((rem (filter (lambda (x) (= (hash-ref grid x) 1)) (filter pair? (hash-keys grid)))) (shortest +inf.0))
      (if (null? rem)
          shortest
          (loop (cdr rem) (min shortest (call-with-values (lambda () (dijkstra g (car rem))) (lambda (x y) (hash-ref x e)))))))))

(part2 (call-with-input-string data process))