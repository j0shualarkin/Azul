#lang racket

(provide (all-defined-out))

(require "board.rkt"
         "factory.rkt"
         "tile.rkt")

;; (define vector vector-immutable)

; State is a (state NumPlayer
;                   [List Player]
;                   [List Board]
;                   Turn
;                   Factory-Set
;                   Factories
;                   [Vector Number]
;                   [List Tile])


; NumPlayer is an Integer indicating how many people are playing
; Player is a Type containing a `make-move` function
; Board is the Board type from board.rkt
; Turn is an Integer indicating whose turn it is
; Factory-Set is the Factory-Set type from factory.rkt
; Factories are the number of available factories in the current round

(struct state [np
               ps
               bs
               t
               fset
               nf

               seed
               bag]
  #:transparent)

(define-values (bag fset) (restock (make-bag) 9))

(define s (state 4 (list null null null null) (list b b b b) 0 fset 9 (vector 0) bag))


;##################################################
; Utilities
;##################################################

; last-round?: State -> Boolean
; true if any board has a complete row
(define last-round?
  (match-lambda [(state _ _ bs _ _ _ _ _) (ormap contains-full-row? bs)]))

; round-end?: State -> Boolean
; true if all factories are empty
(define round-end?
  (match-lambda [(state _ _ _ _ (factory-set mid facts) _ _ _) (andmap empty? (cons mid facts))]))

(define/match (round-over? st)
  (((state np ps bs t (factory-set middle factories) nf seed bag))
   (and (empty? middle)
        (andmap empty? factories))))

; next-turn: State -> Integer
; computes the next turn in the given state
(define next-turn
  (match-lambda [(state np _ _ t _ _ _ _)
                 (modulo (add1 t) np)]))

; play-move: Move State -> State
; plays the given move and returns the updated state
(define (play-move player-move game-state)
  (match game-state
    [(state np ps bs t fset nf seed bag)
     (match player-move
       [(list move-factory-idx move-tile move-line-num)
        (define-values (new-board new-factory-set)
          (add-tiles (list-ref bs t)
                     fset
                     (sub1 move-factory-idx)
                     (add1 move-line-num)
                     move-tile))
        (define new-state
          (struct-copy state game-state
                       (bs (list-set bs t new-board))
                       (t (next-turn game-state))
                       (fset new-factory-set)))
        new-state])]))

; given an end state, returns:
; - final scores of each board (including bonus)
; - list of winners
; - final boards
; - ranked boards
(define/match (end-info game-state)
  (((state np ps bs t fset nf seed bag))
   (define new-boards (map bonusify-board bs))
   (define (compare-board l r)
     (cond
       [(= (board-score l) (board-score r))
        (cond
          [(= (num-full-rows l) (num-full-rows r)) 0]
          [(> (num-full-rows l) (num-full-rows r)) -1]
          [else 1])]
       [(> (board-score l) (board-score r)) -1]
       [else 1]))
   (define (sort-fun l r)
       (= (compare-board l r) -1))
   (define ranked-boards
     (sort new-boards sort-fun))
   (define winning-boards
     (takef ranked-boards (lambda (x) (= (compare-board (car ranked-boards) x) 0))))
   (values (map board-score new-boards)
           (map (lambda (x) (index-of new-boards x)) winning-boards)
           new-boards
           ranked-boards)))


;##################################################
; Printing Utilties
;##################################################

; state->string: State -> String
; returns the string representation of the given state
(define/match (state->string s)
  [((state np ps bs t fset nf seed bag))
   (define factories (factory-set-factories fset))
   (define mid (sort (factory-set-middle fset) tile<?))
   (define parted-facts (split-by (map cons factories
                                       (build-list (length factories) add1)) 5))
   (define fact-str-sample (fact->los (list t0 t0 t0 t0) 0))
   (define fact-sep (make-string (string-length (car fact-str-sample)) #\+))
   (define factories-sep (foldr string-append "\n" (build-list 5 (const fact-sep)))) 
   (define fact-str
     (foldr (λ (facts s)
              (foldr string-append (string-append factories-sep s)
                     (foldr (match-lambda* [`((,f . ,i) ,b)
                                            (map string-append (fact->los f i) b)])
                            (build-list (length fact-str-sample)
                                        (const "\n")) facts))) "" parted-facts))
   (define parted-boards (split-by (map cons bs (build-list np add1)) 2))
   (define board-str-sample (board->los b 0))
   (define board-pad (make-string 10 #\space))
   (define board-str (boards->string bs))
   (string-append factories-sep
                  fact-str
                  (foldr (λ (x y) (string-append x "\n" y))
                         ""
                         (mid->los mid)) factories-sep
                  board-str)])
