#lang racket

(define test-vec (vector (vector #f 'A #f)
                         (vector 'A #f 'B)
                         (vector 'B #f #f)))

(define (wall-distro wall)
  (define bad-idea (make-vector 5 0))
  (for ([vec (in-vector wall)])
    (for ([t (in-vector vec)])
      (cond [(eqv? t t0) (let ([a (vector-ref bad-idea 0)])
                           (vector-set! bad-idea 0 (add1 a)))]
            [(eqv? t t1) (let ([a (vector-ref bad-idea 1)])
                           (vector-set! bad-idea 1 (add1 a)))]
            [(eqv? t t2) (let ([a (vector-ref bad-idea 2)])
                           (vector-set! bad-idea 2 (add1 a)))]
            [(eqv? t t3) (let ([a (vector-ref bad-idea 3)])
                           (vector-set! bad-idea 3 (add1 a)))]
            [(eqv? t t4) (let ([a (vector-ref bad-idea 4)])
                           (vector-set! bad-idea 4 (add1 a)))]
            [else #f])))
  
  bad-idea)

(foo test-vec)

#;
(check-equal? (foo test-vec)
              (vector 2 2))
