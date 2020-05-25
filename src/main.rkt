#lang racket

(require "board.rkt"
         "factory.rkt"
         "player.rkt"
         "state.rkt"
         "tile.rkt"
         "heuristic.rkt"
         racket/trace
         pmap)

(provide (all-defined-out))

;(random-seed 0)

(define *num-players* (make-parameter 3))


(define move-list (make-hash))

;; A Player is a function [Board State -> (List Factory Color Row)]

;; creates a player
(define (playback-player moves)
  (lambda (state board)
    (cond
      ((empty? moves) (manual-player state board))
      (else (begin0 (car moves)
              (displayln (format "Playing back ~a" (car moves)))
              (set! moves (cdr moves)))))))

(define (reconstruct-players move-list)
  (for/list ((i (in-range (hash-count move-list))))
    (playback-player (hash-ref move-list i))))

(define (game-state)
  (let* ((start-bag (make-bag))
         (num-players (*num-players*))
         #;(players (cons rojo-ai (build-list (sub1 num-players) (const random-player))))
         #;(players (cons random-player (build-list (sub1 num-players) (const rojo-ai))))
         #;(players (build-list num-players (const rojo-ai)))
         #;(players (list rojo-ai rojo-ai rojo-ai minimax-ai))
         ;; (players (list rojo-ai rojo-ai (minimax-ai h-0* 3 filter-dumb g-avg) rojo-ai))
         (players (list (minimax-ai h-0* 4 filter-good g-avg)
                        manual-player
                        manual-player))
         #;(players (list (minimax-ai h-0 3) (minimax-ai h-1 3)))
         #;(players (list manual-player manual-player))
         (boards (build-list num-players (const blank-board)))
         (num-factories (necessary-factories num-players)))
    (let-values (((the-bag factory-set) (restock start-bag num-factories)))
      (state num-players
             players
             boards
             (random num-players)
             factory-set
             num-factories

             null
             the-bag))))

(define (clean-up game-state [game-number #f])
  (match game-state
    [(state np ps bs t fset nf seed bag)
     (define next-turn (or (index-where bs board-1-t) 0))
     (define-values (the-bag factory-set) (restock bag nf))
     (define new-boards
       (map update-score bs))
     (define new-state
       (struct-copy state game-state
                    (t next-turn)
                    (bs new-boards)
                    (bag (make-bag)) ; TODO: Technically not correct-- fix this!!
                    (fset factory-set)))
     (next-state new-state game-number)]))

;; make-a-move does one step of `recur`
(define (make-a-move game-state [game-number #f])
  (match game-state
    [(state np ps bs t fset nf seed bag)
     #;(displayln (state->string game-state))
     (define current-player (list-ref ps t))
     (define current-board (list-ref bs t))
     (define player-move (current-player game-state current-board)) ;; can't be a match-lambda
     (hash-update! move-list t (curry cons player-move) (list player-move))
     (define new-state (play-move player-move game-state))
     (next-state new-state game-number)]))

(define (end-game game-state [game-number #f])
  (match game-state
    [(state np ps bs t fset nf seed bag)
     (define-values (_ winners new-boards ranked-boards) (end-info game-state))
     (define f-winners (map add1 winners))
     (if game-number
         (begin (displayln (format "game ~a winners: ~a" game-number f-winners))
                f-winners)
         (begin (displayln "GAME OVER")
                (displayln "Final Boards")
                (displayln (boards->string (state-bs game-state)))
                (display "Winner(s): ")
                (displayln f-winners)
                (newline)
                (for ((b ranked-boards)
                      (i (in-naturals 1)))
                  (displayln (format "~a. Player ~a\t~a"
                                     i
                                     (add1 (index-of new-boards b))
                                     (board-score b))))))]))

(define (next-state game-state [game-number #f])
  (cond
    ((last-round? game-state) (end-game game-state game-number))
    ((round-over? game-state) (clean-up game-state game-number))
    (else (make-a-move game-state game-number))))


;; to run the game you can run either go or azul
(define (go)
  (set! move-list (make-hash))
  (next-state (game-state)))
(define azul go)

(define (run-games n)
  (define (add-winners w m)
    (if (empty? (cdr w))
        (hash-update m (car w) add1 0)
        (hash-update m 't (λ (x) (cons w x)) '())))
  (define (% n t) (exact->inexact (/ (* n 100) t)))
  (define win-counts (foldl (λ (g m) (add-winners (next-state (game-state) g) m))
                            (make-immutable-hash)
                            (build-list n add1)))
  (map (λ (p) (if (eqv? (car p) 't)
                  `(ties = ,(length (hash-ref win-counts 't)))
                  `(,(car p) = ,(% (hash-ref win-counts (car p))
                                   (- n (length (hash-ref win-counts 't '()))))%)))
       (sort (hash->list win-counts) (λ (x y) (if (list? x) x
                                                  (if (list? y)
                                                      x
                                                      (> (cdr x) (cdr y))))))))

(define (parallel-run-games n)
  (define game-places (for/list ((i n))
                        (place ch
                               (place-channel-put ch (next-state (game-state) 0)))))
  (define wins (map place-channel-get game-places))
  (define winners (map car (filter (lambda (x) (null? (cdr x))) wins)))
  (define (% n t) (exact->inexact (/ (* n 100) t)))
  (define win-counts (foldl (lambda (val hsh)
                              (hash-update hsh val add1 0))
                            (make-immutable-hash) winners))
  (map (λ (p) (if (eqv? (car p) 't)
                  `(ties = ,(length (hash-ref win-counts 't)))
                  `(,(car p) = ,(% (hash-ref win-counts (car p))
                                   (- n (length (hash-ref win-counts 't '()))))%)))
       (sort (hash->list win-counts) (λ (x y) (if (list? x) x
                                                  (if (list? y)
                                                      x
                                                      (> (cdr x) (cdr y))))))))
