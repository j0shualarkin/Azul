#lang racket
(provide (all-defined-out))

; Tile is one of:
; - 'blue
; - 'yellow
; - 'red
; - 'black
; - 'green
; - '1

(define t0 'blue)
(define t1 'yellow)
(define t2 'red)
(define t3 'black)
(define t4 'lightblue)
(define one-tile 'λ)

;; b/c i can't remember which is which and im looking at real pieces for tests
(define R 'red)
(define B 'blue)
(define Y 'yellow)
(define K 'black)
(define L 'lightblue)

; setup like the top row of a wall in Azul
(define tiles (list t0 t1 t2 t3 t4))

; idx->tile: Number -> Tile
(define idx->tile (curry list-ref tiles))

; tile->idx : Tile -> Number
(define tile->idx
  (λ (t)
    (cond
      [(eqv? t0 t) 0]
      [(eqv? t1 t) 1]
      [(eqv? t2 t) 2]
      [(eqv? t3 t) 3]
      [(eqv? t4 t) 4]
      [(eqv? one-tile t) 6])))

; tile<?: Tile Tile -> Boolean
(define tile<?
  (λ (ta tb)
    (cond
      [(equal? ta one-tile) #f]
      [(equal? tb one-tile) #t]
      [else (< (index-of tiles ta)
               (index-of tiles tb))])))

;##################################################
; Printing Utilties
;##################################################

; ansi color codes
(define f-blu "\x1B[34m")
(define f-yel "\x1B[33m")
(define f-red "\x1B[31m")
(define f-gry "\x1B[32m")
(define f-mag "\x1B[35m")
(define f-grn "\x1B[36m")
(define norm "\x1B[0m")

(define colors (list f-blu f-yel f-red f-gry f-mag))

; mapping colors to their ansi escape codes
(define color-map
  (map cons tiles colors))

; tile->col: Tile -> String
; returns the ansi color code for the tile
(define (tile->col t)
  (cond
    [(assv t color-map) => cdr]
    [(error 'tile->col "unknown tile: ~a" t)]))

; f->b: String -> String
; converts a forground color to a background color
(define (f->b s)
  (string-replace s "3" "4" #:all? #f
                  ))

(define tile-map
  (cons (cons one-tile (string-append "\x1B[36m" (symbol->string one-tile) norm))
        (map (match-lambda* [`((,t . ,es-c) ,i) (cons t (string-append es-c i norm))])
             color-map
             (build-list (length tiles) (λ (x) (number->string x))))))

; tile->char: Tile -> String
; returns the string representation of tile for CLI uses
(define (tile->str t)
  (cond
    [(assv t tile-map) => cdr]
    [(error 'tile->str "unknown tile: ~a" t)]))
