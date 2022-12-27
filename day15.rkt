#lang racket

(require "day15data.rkt")
(require simple-svg)
;(require "sutherland-hodgman.rkt")
;(require 'sutherland-hodgman)

(define (process in)
  (define rgx #px"^Sensor at x=(-?\\d*), y=(-?\\d*): closest beacon is at x=(-?\\d*), y=(-?\\d*)$")
  (define (radius c p)
    (+
     (abs (- (car c) (car p)))
     (abs (- (cdr c) (cdr p)))))
  (let loop ((cur (read-line in)) (scanners (hash)) (beacons '()))
    (if (eof-object? cur)
        (values scanners (remove-duplicates beacons))
        (let ((m (regexp-match rgx cur)))
          (loop (read-line in)
                (hash-set scanners
                                         (cons
                                          (string->number (list-ref m 1))
                                          (string->number (list-ref m 2)))
                                         (radius
                                          (cons
                                           (string->number (list-ref m 1))
                                           (string->number (list-ref m 2)))
                                          (cons
                                           (string->number (list-ref m 3))
                                           (string->number (list-ref m 4)))))
                (cons  (cons (string->number (list-ref m 3)) (string->number (list-ref m 4))) beacons)
                )))))

(define (part1 scanners beacons line)
  (define grid-corners
    (for/fold
     ((nx +inf.0)
      (xx -inf.0)
      (ny +inf.0)
      (xy -inf.0)
      #:result (list (cons nx ny) (cons xx xy)))
     (((c r) (in-hash scanners)))
      (values
       (inexact->exact (min nx (- (car c) r)))
       (inexact->exact (max xx (+ (car c) r)))
       (inexact->exact (min ny (- (cdr c) r)))
       (inexact->exact (max xy (+ (cdr c) r))))))
  (let loop ((cur (caar grid-corners)) (acc 0))
    (if (> cur (caadr grid-corners))
        acc
        (let ((coord (cons cur line)))
          (cond ((member coord beacons) (loop (add1 cur) acc))
                ((for/or
                     (((c r) (in-hash scanners)))
                   (<= (+
                        (abs (- cur (car c)))
                        (abs (- line (cdr c))))
                       r))
                 (loop (add1 cur) (add1 acc)))
                (else (loop (add1 cur) acc)))))))

(define (part2 scanners)
  (define blank-row
    (make-vector 4000001))
  (define (overlaps? r1 r2)
    (if
     (or
      (and
       (<= (car r1) (car r2))
       (<= (car r2) (cdr r1)))
      (and
       (<= (car r2) (car r1))
       (<= (car r1) (cdr r2))))
     #t
     #f))
  (define (scanner-range-at scanner y)
    (let ((y-dist (abs (- y (cdar scanner)))))
      (if (> y-dist (cdr scanner))
          '()
          (let ((x-part (- (cdr scanner) y-dist)))
            (cons
             (max 0
                  (- (caar scanner) x-part))
             (min 4000000
                  (+ (caar scanner) x-part)))))))
  (define (merge-range-into range ranges)
    (if (null? ranges)
        (list range)
        (let loop ((rem (sort (cons range ranges) (lambda (x y) (< (car x) (car y))))) (acc '()))
          (cond ((null? (cdr rem))
                 (append acc rem))
                ((overlaps? (car rem) (cadr rem))
                 (let ((nums (list (caar rem) (cdar rem) (caadr rem) (cdadr rem))))
                   (loop (cons (cons (apply min nums) (apply max nums)) (cddr rem)) acc)))
                (else (loop (cdr rem) (append acc (list (car rem)))))))))
  (define (scan-row y)
    (let loop ((rem scanners) (acc '()))
      (cond
        ((equal? acc (list (cons 0 4000000))) '())
        ((null? rem)
         (for/fold
          ((out '())
           #:result (filter (lambda (x) (and (>= x 0) (<= x 4000000))) out))
          ((range acc))
           (append out (list (sub1 (car range)) (add1 (cdr range))))))
        (else
         (let ((range (scanner-range-at (car rem) y)))
            (if (null? range)
                (loop (cdr rem) acc)
                (loop (cdr rem) (merge-range-into range acc))))))))
  (let loop ((y 0))
    (if (> y 4000000)
        #f
        (let ((x (scan-row y)))
          (if (not (null? x))
              (display (format "Solution: ~a" (+ y (* (car x) 4000000))))
              (loop (add1 y)))))))

(let-values (((s b) (call-with-input-string data process)))
  (part2 (hash->list s)))