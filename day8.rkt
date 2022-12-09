#lang racket

(require "day8data.rkt")

(define (crossroads coord coll)
  (let ((size (hash-ref coll 'size)))
    (list
     (map (lambda (y) (cons (car coord) y)) (range (cdr coord)))
     (map (lambda (y) (cons (car coord) y)) (range (add1 (cdr coord)) (cdr size)))
     (map (lambda (x) (cons x (cdr coord))) (range (car coord)))
     (map (lambda (x) (cons x (cdr coord))) (range (add1 (car coord)) (car size))))))

(define (blockers paths height coll)
    (let loop ((rem paths) (cur height) (acc '()))
      (cond ((null? rem) acc)
            ((> cur 9) (loop (cdr rem) height acc))
            (else
             (let ((bs (filter (lambda (c) (member c (hash-ref coll cur))) (car rem))))
               (if (null? bs)
                   (loop rem (add1 cur) acc)
                   (loop (cdr rem) height (cons bs acc))))))))

(define (visible? paths height coll)
    (if (equal? 4 (length (blockers paths height coll)))
        #f
        #t))

(define scan
  (let ((seen (make-hash)))
    (lambda (coll)
      (define (scan-from dir)
        (let ((incr
               (cond
                 ((equal? dir 'N)
                  (lambda (lst) (map (lambda (c) (cons (cons (caar c) (add1 (cdar c))) (cdr c))) lst)))
                 ((equal? dir 'S)
                  (lambda (lst) (map (lambda (c) (cons (cons (caar c) (sub1 (cdar c))) (cdr c))) lst)))
                 ((equal? dir 'E)
                  (lambda (lst) (map (lambda (c) (cons (cons (sub1 (caar c)) (cdar c)) (cdr c))) lst)))
                 ((equal? dir 'W)
                  (lambda (lst) (map (lambda (c) (cons (cons (add1 (caar c)) (cdar c)) (cdr c))) lst)))))
              (edge
               (cond
                 ((equal? dir 'N)
                  (map (lambda (x) (cons (cons x 0) -1)) (range (cdr (hash-ref coll 'size)))))
                 ((equal? dir 'S)
                  (map (lambda (x) (cons (cons x (sub1 (cdr (hash-ref coll 'size)))) -1)) (range (cdr (hash-ref coll 'size)))))
                 ((equal? dir 'E)
                  (map (lambda (y) (cons (cons (sub1 (car (hash-ref coll 'size))) y) -1)) (range (car (hash-ref coll 'size)))))
                 ((equal? dir 'W)
                  (map (lambda (y) (cons (cons 0 y) -1)) (range (car (hash-ref coll 'size)))))
                 )))
          (let loop ((cur edge))
            (if (or
                 (null? cur)
                 (not (hash-has-key? coll (caar cur))))
                (void)
                (let i-loop ((rem cur) (acc '()))
                  (cond ((null? rem) (loop (incr acc)))
                        ((hash-has-key? seen (caar rem)) (i-loop (cdr rem) (append acc (list (cons (caar rem) (hash-ref seen (caar rem)))))))
                        (else
                         (let ((h (hash-ref coll (caar rem))))
                           (cond
                             ((= h 9)
                              (begin
                                 (hash-set! seen (caar rem) h)
                                 (i-loop (cdr rem) acc)))
                             ((> h (cdar rem))
                              (begin
                                (hash-set! seen (caar rem) h)
                                (i-loop (cdr rem) (append acc (list (cons (caar rem) h))))))
                             (else
                              (i-loop (cdr rem) (append acc (list (car rem))))))))))))))
      (begin
        (for
            ((d '(N W E S)))
          (scan-from d))
        (length (hash-keys seen)))
      )))

(define (hollow points)
  (define (inside? point)
    (and
     (findf (lambda (p) (and (equal? (car p) (car point)) (> (cdr p) (cdr point)))) points)
     (findf (lambda (p) (and (equal? (car p) (car point)) (< (cdr p) (cdr point)))) points)
     (findf (lambda (p) (and (> (car p) (car point)) (equal? (cdr p) (cdr point)))) points)
     (findf (lambda (p) (and (< (car p) (car point)) (equal? (cdr p) (cdr point)))) points)))
  (for/fold ((out '())
             #:result out)
            ((p (in-list points)))
    (if (inside? p)
        out
        (cons p out))))

(define (part1 coll)
  (let loop ((i 9) (edges '()) (acc '()))
    (if (< i 0)
        (length acc)
        (let ((next (hollow (append edges (hash-ref coll i)))))
          (loop (sub1 i) next (foldl
                               (lambda (p a) (if (member p next) (cons p a) a))
                               acc
                               (hash-ref coll i)))))))
(define (part2 coll)
  (define (on-edge? tree)
    (or
     (zero? (car tree))
     (zero? (cdr tree))
     (= 1 (- (car (hash-ref coll 'size)) (car tree)))
     (= 1 (- (cdr (hash-ref coll 'size)) (cdr tree)))))
  (define (scenic-score tree blockers)
    (if (on-edge? tree)
        0
        (*
         (let ((xs (map car (filter (lambda (b) (and (> (car b) (car tree)) (= (cdr b) (cdr tree)))) blockers))))
           (if (null? xs)
               (- (sub1 (car (hash-ref coll 'size))) (car tree))
               (- (apply min xs) (car tree))))
         (let ((xs (map car (filter (lambda (b) (and (< (car b) (car tree)) (= (cdr b) (cdr tree)))) blockers))))
           (if (null? xs)
               (car tree)
               (- (car tree) (apply max xs))))
         (let ((ys (map cdr (filter (lambda (b) (and (= (car b) (car tree)) (> (cdr b) (cdr tree)))) blockers))))
           (if (null? ys)
               (- (sub1 (cdr (hash-ref coll 'size))) (cdr tree))
               (- (apply min ys) (cdr tree))))
         (let ((ys (map cdr (filter (lambda (b) (and (= (car b) (car tree)) (< (cdr b) (cdr tree)))) blockers))))
           (if (null? ys)
               (cdr tree)
               (- (cdr tree) (apply max ys)))))))
  (let loop ((i 9) (cmp '()) (best 0))
    (if (< i 0)
        best
        (let i-loop ((rem (hash-ref coll i)) (i-cmp (append cmp (hash-ref coll i)))  (i-best best))
          (if
           (null? rem)
           (loop (sub1 i) i-cmp i-best)
           (i-loop (cdr rem) i-cmp (max i-best (scenic-score (car rem) i-cmp))))))))
          
    
(define (process in)
  (let y-loop ((line (read-line in)) (y 0) (y-coll (foldl (lambda (n h) (hash-set h n '())) (hash) (range 10))) (max-y 0) (max-x 0))
    (if (eof-object? line)
        (hash-set y-coll 'size (cons max-x max-y))
        (let x-loop ((x-rem (map (lambda (c) (string->number (list->string (list c)))) (string->list line))) (x 0) (x-coll y-coll))
          (if (null? x-rem)
              (y-loop (read-line in) (add1 y) x-coll (max max-y (add1 y)) x)
              (x-loop (cdr x-rem) (add1 x) (hash-update (hash-set x-coll (cons x y) (car x-rem)) (car x-rem) (lambda (l) (cons (cons x y) l)) '())))))))

(part2 (call-with-input-string data process))