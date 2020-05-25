#lang racket

(require "tile.rkt")
(provide (all-defined-out))

(define (make-bag)
  (shuffle
   (append
    (build-list 20 (const t0))
    (build-list 20 (const t1))
    (build-list 20 (const t2))
    (build-list 20 (const t3))
    (build-list 20 (const t4)))))

(define +factory-size+ 4)

(define (necessary-factories players)
  (+ 5 (* 2 (- players 2))))

(define (split-by lst n)
  (cond
    ((empty? lst) empty)
    ((< (length lst) n) (list lst))
    (else (cons (take lst n) (split-by (drop lst n) n)))))

;; Factory is a (List Tile) with a length of at most +factory-size+
;; Middle is a (List Tile) with an arbitrary length

;; factory-set is a structure of:
;; - middle:    Middle
;; - factories: (List Factory)

(struct factory-set (middle factories)
  #:transparent)

;; restock :: Bag -> Number -> (Values Bag Factory-Set)
;; Given the current bag, create a new factory set for the beginning of a round

;; Returns the new bag and a factory set
(define (restock bag n)
  (define-values (tiles new-bag) (split-at (shuffle bag) (* +factory-size+ n)))
  (define factories (split-by tiles +factory-size+))
  (values new-bag (factory-set `(,one-tile) factories)))

;; partition-factory :: Tile Factory -> (Values [Number] Factory)
;; splits the factory into a list of tiles that are the same color
;;   as the given tile and a list of different colored tiles
(define (partition-factory tile f)
  (define pred (λ (x) (or (eq? x tile) (eq? x one-tile))))
  ;; if the 1 tile is there (the middle), have to take it
  (partition pred f))

;; pull-from-factory :: Number Tile Factory-Set -> (Values (ListOf Tile) Factory-Set)
;; removes the tiles (matching the given tile) at the given factory (index)
;; people ask for the middle tile by asking for the -1 factory in a Factory

;; Returns the taken values, and the new factory set
(define/match (pull-from-factory idx color factories)
  ((idx color (factory-set mid others))
   (cond
     ((negative? idx)
      (let-values ([(same diff) (partition-factory color mid)])
        (values same (factory-set diff others))))
     (else
      (let*-values ([(desired-factory) (list-ref others idx)]
                    [(same diff) (partition-factory color desired-factory)])
        (values same (factory-set (append diff mid) (list-set others idx empty))))))))

;##################################################
; Printing Utilties
;##################################################

; fact->los: Factory Int -> [List String]
; creates the string representation of the given factory, with the factories index
; each line is retured separately
(define (fact->los f i)
  (define label (string-append "f-" (number->string i)
                               (make-string +factory-size+ #\space)))
  (define tiles (foldr (λ (x y) (string-append (tile->str x) y)) "" f))
  (list (string-append "|" label)
        (string-append "|  "
                       (make-string (- +factory-size+ (length f)) #\space)
                       tiles " ")))

; mid->los: Middle -> [List String]
; creates the string representation of the given mid
; each line is returned separately
(define (mid->los m)
  (define label (string-append "middle" (make-string (max 0 (- (length m) 4)) #\space)))
  (define tiles (foldr (λ (x y) (string-append (tile->str x) y)) "" m))
  (list (string-append "|" label)
        (string-append "|  " tiles)))
