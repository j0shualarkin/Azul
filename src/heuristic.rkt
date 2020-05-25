#lang racket

(require "board.rkt"
         "factory.rkt"
         "tile.rkt"
         "state.rkt")

(provide (all-defined-out))



; h-0: State -> Integer
(define (h-0 s)
  (match s
    [(state np ps bs t (factory-set m fs) nf seed bag)
     (define turn-distance (add1 t))
     (define (h b)
       (+ (board-score b) (analysis b) (overflow-error (board-error-count b))))
     (define (analysis b)
       (let ([new-board (update-score b)])
         (+ (board-score new-board)
            (calculate-bonus new-board)
            (predict new-board)
            (overflow-error (board-error-count new-board)))))
     (define tile-counts-fs (map (compose length (lambda(x)(group-by identity x eqv?)))
                                 fs))
     (define tile-counts-m (map length (group-by identity m eqv?)))
     (define eq=? (curry eqv?))
     (define tile-c-counts-fs
       (foldr (λ (t m) (hash-set m t (map (curry count (eq=? t)) fs)))
              (make-immutable-hash) tiles))
     (define tile-c-counts-m
       (foldr (λ (t mp) (hash-set mp t (list (count (eq=? t) m))))
              (make-immutable-hash) tiles))
     (define (predict b)
       (define (stage-analysis sg-line)
         (match sg-line
           [`(,i ,n . ,p)
            (let ([filled (if p (car p) 0)])
              (if (= (sub1 n) filled) 1 0))
            #;
            (cond
              [(not p) (if (= n 1) 1 0)]
              [(= (- n 2) (car p)) 1]
              [else (if (= (sub1 n) (car p)) 1 0)])]))
       (foldr + 0 (map stage-analysis (map cons (build-list 5 identity)
                                           (vector->list (board-staging b))))))
     (let ([bs (state-bs s)])
       (- (h (car bs))
          (h (cadr bs))))]))

(define (h-0* s)
  (match s
    [(state np ps bs t (factory-set m fs) nf seed bag)
     (define turn-distance (add1 t))
     (define (h b)
       (+ (board-score b) (analysis b) (overflow-error (board-error-count b))))
     (define (analysis b)
       (let ([new-board (update-score b)])
         (+ (board-score new-board)
            (calculate-bonus new-board)
            (predict new-board)
            (overflow-error (board-error-count new-board)))))
     (define tile-counts-fs (map (compose length (lambda(x)(group-by identity x eqv?)))
                                 fs))
     (define tile-counts-m (map length (group-by identity m eqv?)))
     (define eq=? (curry eqv?))
     (define tile-c-counts-fs
       (foldr (λ (t m) (hash-set m t (map (curry count (eq=? t)) fs)))
              (make-immutable-hash) tiles))
     (define tile-c-counts-m
       (foldr (λ (t mp) (hash-set mp t (list (count (eq=? t) m))))
              (make-immutable-hash) tiles))
     (define (predict b)
       (define (stage-analysis sg-line)
         (match sg-line
           [`(,i ,n . ,p)
            (let ([filled (if p (car p) 0)])
              (if (= (sub1 n) filled) 1 0))
            #;
            (cond
              [(not p) (if (= n 1) 1 0)]
              [(= (- n 2) (car p)) 1]
              [else (if (= (sub1 n) (car p)) 1 0)])]))
       (foldr + 0 (map stage-analysis (map cons (build-list 5 identity)
                                           (vector->list (board-staging b))))))
     (map h (state-bs s))]))

(define (h-1 s)
  (define (h b)
    (match b
      [(board score wall stage errs 1-t)
       (define-values (full non-empty empty) (split-stage stage))
       (define calc-these (sort (append full non-empty)
                                (λ (a b) (< (car a) (car b)))))
       (define u-wall wall)
       (define s1 (for/sum ([line calc-these])
                    (define-values (tile row)
                      (match line
                        [(cons n (cons k t)) (values t (sub1 n))]))
                    (define tile-goes-here (get-tile-spot row tile wall))
                    (set! u-wall (place-move/v2 tile-goes-here u-wall))
                    (count-points tile-goes-here u-wall)))
       (define bonus-score (calculate-bonus (board #f u-wall #f #f #f)))
       (max (- (+ score
                  s1
                  bonus-score)
               errs)
            0)]))
  (match s
    [(state num-plyr plyrs brds turn fact-set avail-factories seed bag)
     (- (h (list-ref brds turn))
        (h (list-ref brds (modulo (add1 turn) num-plyr))))]))


(define (h-2 state)
  (board-score (update-score (bonusify-board (car (state-bs state)))))
  #;`(,m . ,(let-values ([(b _)(add-tiles board (state-fset state)
                                        (sub1 (car m)) (add1 (caddr m)) (cadr m))])
            (board-score (update-score (bonusify-board b))))))
