#lang racket

(require rackunit
         "tile.rkt"
         "factory.rkt")

(provide (all-defined-out))

; Avoid side-effects?
(define vector vector-immutable)

;; vec-get :: Vector[Vector[X]] Num Num -> X
(define (vec-get w x y)
  (vector-ref (vector-ref w x) y))

;##################################################
; Data Structures
;##################################################

; Board is a (board Score Wall StagingArea Extra 1-Tile)

; Score is an Int

; Wall is a [Vector [Vector (Pair Tile Boolean)]]. Using vectors to make bonus
; score calculations easier. Boolean indicates whether it is occupied or not

; StagingArea is (Vector StagingLine)
; StagingLine is a (Pair Int StagingState) where Int repesents max amount of tiles

; StagingState is one of:
; - #f
; - (Pair Int Tile) where Int represents how many tiles have been placed

; An Extra is a Int
; Extra keeps track of the amount of overflow tiles a board has taken
; the list `overflow` accounts for the negative score a board takes for
; an amount of Extra

; 1-Tile is a Boolean that indicates whether the board contains the 1 tile

; A Posn is a (Pair Int Int)

(struct board [score wall staging error-count 1-t] #:transparent)

; a wall as seen in Azul
(define wall (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                     (vector `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f))
                     (vector `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                     (vector `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f))
                     (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f))))


(define almost-done-wall
  (vector (vector `(,t0 . #t) `(,t1 . #t) `(,t2 . #t) `(,t3 . #t) `(,t4 . #f))
          (vector `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f))
          (vector `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
          (vector `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f))
          (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f))))

; An empty first staging line
(define sg-ln0 (cons 1 #f))
; Example of a filled first staging line
(define sg-ln0-filled (cons 1 (cons 1 t0)))

; remaining empty staging lines
(define sg-ln1 (cons 2 #f))
(define sg-ln2 (cons 3 #f))
(define sg-ln3 (cons 4 #f))
(define sg-ln4 (cons 5 #f))

;Azul's default staging area
(define sg-ar (vector sg-ln0 sg-ln1 sg-ln2 sg-ln3 sg-ln4))


;; split-stage :: StagingArea -> (Values [StageLine] [StageLine] [StageLine])
(define (split-stage s)
  (let loop ([idx 0] [full empty] [non-empty empty] [mt empty])
    (cond
      [(> idx 4) (values full non-empty mt)]
      [else (let ([line (vector-ref s idx)])
              (cond
                [(and (cdr line) (= (car line) (cadr line)))
                 ;; non-empty and full line
                 (loop (add1 idx)
                       (cons line full)
                       non-empty
                       mt)]
                [(cdr line)
                 ;; non-empty line
                 (loop (add1 idx)
                       full
                       (cons line non-empty)
                       mt)]
                [else
                 ;; empty line
                 (loop (add1 idx)
                       full
                       non-empty
                       (cons line mt))]))])))

(let-values ([(f n e) (split-stage (vector (cons 1 #f) (cons 2 (cons 2 t0))
                                           (cons 3 #f)
                                           (cons 4 (cons 3 t1))
                                           (cons 5 (cons 5 t2))))])
  (check-equal? f (list (cons 5 (cons 5 t2))
                        (cons 2 (cons 2 t0))))
  (check-equal? n (list (cons 4 (cons 3 t1))))
  (check-equal? e (list (cons 3 #f)
                        (cons 1 #f))))


;; color-distro :: Wall -> (Vectorof Number)
;; returns a vector of length 5
;; each index of the vector represents a tile and the value stored is the amount
;; of that tile collected so far 
(define (color-distro wall)
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

; Azul's default overflow
(define overflow (list -1 -1 -2 -2 -2 -3 -3))

(define b (board 0 wall sg-ar 0 #f))
(define blank-board b)
(define almost-done-board (board 0 almost-done-wall sg-ar 0 #f))

; some utilities that could be useful for testing or the game itself

; put-staging: Board StagingArea -> Board
; replaces the given board's staging area with the new one
(define put-staging
  (match-lambda* [`(,(board score wall _  oc 1t) ,staging)
                  (board score wall staging oc 1t)]))

(check-equal? (put-staging b (vector sg-ln0 sg-ln0 sg-ln0 sg-ln0 sg-ln0))
              (board 0 wall (vector sg-ln0 sg-ln0 sg-ln0 sg-ln0 sg-ln0) 0 #f))

; put-wall: Board Wall -> Board
; replaced the given board's wall with the new one
(define put-wall
  (match-lambda* [`(,(board score _ sg oc 1t) ,wall)
                  (board score wall sg oc 1t)]))

(check-equal? (put-wall b (build-vector 5 (λ (_) (build-vector 5 (λ (_) `(,t0 . #t))))))
              (board 0
                     (build-vector 5 (λ (_) (build-vector 5 (λ (_) `(,t0 . #t)))))
                     sg-ar
                     0 #f))

; place-move: Posn Board -> Board
(define place-move
  (match-lambda*
    [`((,x . ,y) ,(board s w sg-ar oc 1t))
     (define new-wall
       (match-let*-values ([(top bot)    (vector-split-at w x)]
                           [(left right) (vector-split-at (vector-ref bot 0) y)])
         (match-let* ([`(,t . #f) (vector-ref right 0)]
                      [new-right (vector-append (vector `(,t . #t))
                                                (vector-drop right 1))]
                      [new-row (vector-append left new-right)]
                      [new-bot (vector-append (vector new-row)
                                              (vector-drop bot 1))])
           (vector-append top new-bot))))
     (board s new-wall sg-ar oc 1t)]))


;; place-move/v2 :: Posn Wall -> Wall
(define place-move/v2
  (match-lambda*
    [`((,x . ,y) ,w)
     (match-let*-values ([(top bot) (vector-split-at w x)]
                         [(left right) (vector-split-at (vector-ref bot 0) y)])
       (match-let* ([`(,t . #f) (vector-ref right 0)]
                    [new-right (vector-append (vector `(,t . #t))
                                              (vector-drop right 1))]
                    [new-row (vector-append left new-right)]
                    [new-bot (vector-append (vector new-row)
                                            (vector-drop bot 1))])
         (vector-append top new-bot)))]))

; a move can be placed anywhere on a (5 X 5) board
(for ([p (foldr append '() (build-list 5 (λ (x) (build-list 5 (λ (y) (cons x y))))))])
  (match p
    [`(,x . ,y) (check-equal? (place-move/v2 (cons x y)
                                           (build-vector 5 (λ (_) (build-vector 5 (λ (_) `(,t0 . #f))))))
                               (build-vector 5 (λ (x0) (build-vector 5 (λ (y0) `(,t0 . ,(equal? (cons x0 y0) (cons x y))))))))]))

;##################################################
; Utilities
;##################################################

; count-points: (Pair Int Int) Wall -> Int
; given a zero based position on the wall, and the board that contains the wall,
; computes the score when a tile is placed at the position
; Note that X position points to the row, Y points to the item in the row
(define count-points
  (match-lambda*
    [`((,x . ,y) ,w)
     (define top-vec   (build-vector x (λ (i) (vector-ref (vector-ref w i) y))))
     (define top-list  (takef (reverse (vector->list top-vec)) cdr))

     (define bot-vec   (build-vector (- (vector-length w) x)
                                     (λ (i) (vector-ref (vector-ref w (+ x i)) y))))
     (define bot-list  (takef (cdr (vector->list bot-vec)) cdr))
     ;                         cdr to not count the tile at the given position

     (define left-vec  (vector-take (vector-ref w x) y))
     (define left-list (takef (reverse (vector->list left-vec)) cdr))

     ;                          add1, similar to cdr above
     (define right-vec (vector-drop (vector-ref w x) (add1 y)))
     (define right-list  (takef (vector->list right-vec) cdr))

     (define vert-count (+ (length bot-list) (length top-list)))
     (define hori-count (+ (length left-list) (length right-list)))

     ;                          add 2 points iff both hori and count are non-zero
     (+ 1 vert-count hori-count (if (zero? (* hori-count vert-count)) 0 1))]))

; 1 point when a tile is placed anywhere on an empty board
(for ([p (foldr append '() (build-list 5 (λ (x) (build-list 5 (λ (y) (cons x y))))))])
  (check-equal? (count-points p (board-wall b)) 1))

; n points for n consecutive tiles in the same row or column
; in both directions (left<->right) (bot<->top)
(for ([x 5])
  (let* ([b0 (place-move (cons x 0) b)]
         [b1 (place-move (cons x 1) b0)]
         [b2 (place-move (cons x 2) b1)]
         [b3 (place-move (cons x 3) b2)])
    (check-equal? (count-points (cons x 1) (board-wall b0)) 2)
    (check-equal? (count-points (cons x 2) (board-wall b1)) 3)
    (check-equal? (count-points (cons x 3) (board-wall b2)) 4)
    (check-equal? (count-points (cons x 4) (board-wall b3)) 5))
  (let* ([b0 (place-move (cons x 4) b)]
         [b1 (place-move (cons x 3) b0)]
         [b2 (place-move (cons x 2) b1)]
         [b3 (place-move (cons x 1) b2)])
    (check-equal? (count-points (cons x 3) (board-wall b0)) 2)
    (check-equal? (count-points (cons x 2) (board-wall b1)) 3)
    (check-equal? (count-points (cons x 1) (board-wall b2)) 4)
    (check-equal? (count-points (cons x 0) (board-wall b3)) 5))
  (let* ([b0 (place-move (cons 0 x) b)]
         [b1 (place-move (cons 1 x) b0)]
         [b2 (place-move (cons 2 x) b1)]
         [b3 (place-move (cons 3 x) b2)])
    (check-equal? (count-points (cons 1 x) (board-wall b0)) 2)
    (check-equal? (count-points (cons 2 x) (board-wall b1)) 3)
    (check-equal? (count-points (cons 3 x) (board-wall b2)) 4)
    (check-equal? (count-points (cons 4 x) (board-wall b3)) 5))
  (let* ([b0 (place-move (cons 4 x) b)]
         [b1 (place-move (cons 3 x) b0)]
         [b2 (place-move (cons 2 x) b1)]
         [b3 (place-move (cons 1 x) b2)])
    (check-equal? (count-points (cons 3 x) (board-wall b0)) 2)
    (check-equal? (count-points (cons 2 x) (board-wall b1)) 3)
    (check-equal? (count-points (cons 1 x) (board-wall b2)) 4)
    (check-equal? (count-points (cons 0 x) (board-wall b3)) 5)))

; 10 points when placed in the mid and surrounded by other tiles
(let* ([b0 (place-move (cons 2 0) b)]
       [b1 (place-move (cons 2 1) b0)]
       [b2 (place-move (cons 2 3) b1)]
       [b3 (place-move (cons 2 4) b2)]
       [b4 (place-move (cons 0 2) b3)]
       [b5 (place-move (cons 1 2) b4)]
       [b6 (place-move (cons 3 2) b5)]
       [b7 (place-move (cons 4 2) b6)])
  (check-equal? (count-points (cons 2 2) (board-wall b7)) 10))

(define j/wall
  (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
          (vector `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
          (vector `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
          (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f))
          (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #f))))

(check-equal? (count-points (cons 0 2) j/wall) 1)

; reset-lines: Board -> Board
; resets staging lines of the given board at the end of a round per Azul rules
(define reset-lines
  (match-lambda
    [(board x y staging z 1t)
     (board x
            y
            (vector-map (match-lambda [`(,max-c . ,s)
                                       `(,max-c . ,(and s (> max-c (car s)) s))])
                        staging)
            z 1t)]))

; id if cdr is #f
(check-equal? (reset-lines (put-staging b (vector `(4 . #f))))
              (put-staging b (vector `(4 . #f))))
(check-equal? (reset-lines (put-staging b (vector `(4 . #f) `(1 . #f))))
              (put-staging b (vector `(4 . #f) `(1 . #f))))
; id if cadr =/ car
(check-equal? (reset-lines (put-staging b (vector `(4 . (3 . ,t0)) `(1 . (0 . ,t1)))))
              (put-staging b (vector `(4 . (3 . ,t0)) `(1 . (0 . ,t1)))))
(check-equal? (reset-lines (put-staging b (vector `(4 . (3 . ,t0)) `(20 . (19 . ,t1)))))
              (put-staging b (vector `(4 . (3 . ,t0)) `(20 . (19 . ,t1)))))
; b if all lines are full
(check-equal? (reset-lines (put-staging b (vector `(1 . (1 . ,t0))
                                                  `(2 . (2 . ,t1))
                                                  `(3 . (3 . ,t3))
                                                  `(4 . (4 . ,t4))
                                                  `(5 . (5 . ,t3))))) b)

; removes only those that are full, leaving other as they were
(check-equal? (reset-lines (put-staging b (vector `(1 . #f)
                                                  `(2 . (1 . ,t1))
                                                  `(3 . (3 . ,t3))
                                                  `(4 . (4 . ,t4))
                                                  `(5 . (4 . ,t3)))))
              (put-staging b (vector `(1 . #f)
                                     `(2 . (1 . ,t1))
                                     `(3 . #f)
                                     `(4 . #f)
                                     `(5 . (4 . ,t3)))))

;; stageline-ready? :: StagingLine Wall -> Either #f Tile
;; return true iff the given stage line has as many tiles as it is long
;; check if the car is equal to the cdar of the staging area
(define stageline-ready?
  (match-lambda
    [`(,n . (,c . ,t)) #:when (= n c) t]
    [_ #f]))

;; overflow-error :: Int [Int] -> Int
;; returns the total error accumulated by having n overflow counts, using the overflow scores
(define overflow-error
  (λ (n)
    (letrec ([count/acc ;; count/acc : Int [Int] Int -> Int
              ;; accumulates the negative points as long as n != 0
              (λ (n m acc)
                (if (zero? n)
                    acc
                    (count/acc (sub1 n) (cdr m) (+ (car m) acc))))])
      (count/acc n overflow 0))))

(define (sum ls) (foldr + 0 ls))

(check-equal? (overflow-error 0) 0)
(check-equal? (overflow-error 1) -1)
(check-equal? (overflow-error 2) -2)
(check-equal? (overflow-error 3) -4)
(check-equal? (overflow-error 4) -6)
(check-equal? (overflow-error 5) -8)
(check-equal? (overflow-error 6) -11)
(check-equal? (overflow-error 7) (sum (list -1 -1 -2 -2 -2 -3 -3)))




;; update-score : Board -> Board
;; computes the error score (overflow tiles) and the fully staged tiles score
;; returns a board with an updated score, overflow count, and staging area
(define update-score
  (match-lambda
    [(board score wall staging oc 1t)
     (let*-values ([(reduced-score)    (+ (overflow-error oc) score)]
                   [(net-pts new-wall) (place-and-score-staging reduced-score staging wall)]
                   [(score) (max 0 net-pts)])
       (reset-lines (board score new-wall staging 0 #f)))]))



;; place-and-score-staging :: Int StagingArea Wall -> (Values Int Wall)
;; accumulates the score earned by placing tiles from filled staging lines to the wall
;; also updates the wall with each placement
(define place-and-score-staging
  (λ (score staging wall)
    (help-score-staging 0 score staging wall)))


;; get-tile-spot :: Int Tile Wall -> Posn
;; returns the position the tile belongs to, given the row and the wall
(define get-tile-spot
  (λ (i t w)
    (let ([line (vector-ref w i)])
      (let loop ([j 0])
        (if (equal? (car (vector-ref line j)) t)
            (cons i j)
            (loop (add1 j)))))))

(define columns (build-list 5 (λ (n) n)))
(check-equal? (map (λ (n) (get-tile-spot n t0 wall)) columns)
              (build-list 5 (λ (n) (cons n n))))
(check-equal? (map (λ (n) (get-tile-spot n t1 wall)) columns)
              (append (build-list 4 (λ (n) (cons n (add1 n)))) (list  (cons 4 0))))
(check-equal? (map (λ (n) (get-tile-spot n t2 wall)) columns)
              (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 0) (cons 4 1)))
(check-equal? (map (λ (n) (get-tile-spot n t3 wall)) columns)
              (list (cons 0 3) (cons 1 4) (cons 2 0) (cons 3 1) (cons 4 2)))
(check-equal? (map (λ (n) (get-tile-spot n t4 wall)) columns)
              (list (cons 0 4) (cons 1 0) (cons 2 1) (cons 3 2) (cons 4 3)))



;; help-score-staging :: Int Int StagingArea Wall -> Int
;; i := updating index of which row to consider;
;;   invariant! i should always start at 0
;; score := accumulating score of points earned by placing tiles
;; stage := vector of current staging area
;; wall := wall from the game containing already placed tiles
;; function: for each line in the staging area,
;;   if it has the proper amount of tiles in it =>
;;     place one of them, clear that staging line,
;;     add the tile's points to the score score
;;   o/w move on to the next staging line
(define help-score-staging
  (λ (i score stage w)
    (when (not (<= 0 i 5))
      (error 'help-score-staging
             (format "starting index (i) should be in [0,5] given: ~a" i)))
    (cond
      [(= i 5) (values score w)]
      [(stageline-ready? (vector-ref stage i))
       => (λ (staged-tile)
            (let* ([tile-goes-here (get-tile-spot i staged-tile w)]
                   [tile-points (count-points tile-goes-here w)])
              (help-score-staging (add1 i)
                                  (+ tile-points score)
                                  stage
                                  (place-move/v2 tile-goes-here w))))]
      [else (help-score-staging (add1 i) score stage w)])))



;; tsa = test-staging-area
(define tsa1 (vector
              (cons 1 #f)
              (cons 2 (cons 2 t4))
              (cons 3 (cons 3 t3))
              (cons 4 (cons 4 t1))
              (cons 5 (cons 5 t0))))

(define tsa2 (vector
              (cons 1 #f)
              (cons 2 (cons 1 t4))
              (cons 3 (cons 3 t3))
              (cons 4 (cons 4 t1))
              (cons 5 (cons 4 t0))))


(let-values ([(score wall) (help-score-staging 0 3 tsa1 j/wall)])
  (check-equal? score 11)
  (check-equal?
   wall
   (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
           (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
           (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
           (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #t))
           (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #t)))))

(let-values ([(score wall) (help-score-staging 0 3 tsa2 j/wall)])
  (check-equal? score 5)
  (check-equal?
   wall
   (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
           (vector `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
           (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
           (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #t))
           (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #f)))))

(define j/board
  (board 3 j/wall tsa1 0 #f))

(check-equal? (update-score j/board)
              (board 11
                     (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                             (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
                             (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                             (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #t))
                             (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #t)))
                     sg-ar
                     0 #f))

(define j/board/2
  (board 3 j/wall tsa1 2 #f))
(check-equal? (update-score j/board/2)
              (board 9
                     (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                             (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
                             (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                             (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #t))
                             (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #t)))
                     sg-ar
                     0 #f))


(define j/board/3
  (board 3 j/wall tsa1 4 #f))

(check-equal? (update-score j/board/3)
              (board 5
                     (vector (vector `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                             (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #t) `(,t3 . #f))
                             (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                             (vector `(,t2 . #f) `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #t))
                             (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #t) `(,t0 . #t)))
                     sg-ar
                     0 #f))





;; there's a can-place for the staging line and a can place for the wall too
;; i think we should just do for the wall and include the staging line placement as well

;; can-place? :: Tile StagingLine -> Boolean
;; return true iff the given kind of tile
;;  can be placed in the given staging area
(define can-place?
  (λ (t s)
    (match s
      [`(,n . #f) #t]
      [`(,n . (,c . ,tt)) (and (eqv? t tt) (< c n))])))


;; unoccupied? :: Tile Wall Int -> Boolean
;; return true iff there is not a tile of the given color in the row indexed into wall
(define unoccupied?
  (λ (color wall idx)
    (let ([row (vector-ref wall idx)])
      (let loop ([i 0])
        (and (< i 5)
             (or (match-let ([`(,c . ,b) (vector-ref row i)])
                   (and (eqv? c color) (not b)))
                 (loop (add1 i))))))))

(check-true (unoccupied? t0 j/wall 0))
(check-true (unoccupied? t0 j/wall 1))
(check-true (unoccupied? t0 j/wall 2))
(check-true (unoccupied? t0 j/wall 3))
(check-true (unoccupied? t0 j/wall 4))

(check-true (unoccupied? t1 j/wall 0))
(check-true (unoccupied? t1 j/wall 1))
(check-true (unoccupied? t1 j/wall 2))
(check-true (unoccupied? t1 j/wall 3))
(check-true (unoccupied? t1 j/wall 4))

(check-true (unoccupied? t2 j/wall 0))
(check-true (not (unoccupied? t2 j/wall 1)))
(check-true (unoccupied? t2 j/wall 2))
(check-true (unoccupied? t2 j/wall 3))
(check-true (unoccupied? t2 j/wall 4))

(check-true (unoccupied? t3 j/wall 0))
(check-true (unoccupied? t3 j/wall 1))
(check-true (unoccupied? t3 j/wall 2))
(check-true (not (unoccupied? t3 j/wall 3)))
(check-true (unoccupied? t3 j/wall 4))

(check-true (unoccupied? t4 j/wall 0))
(check-true (unoccupied? t4 j/wall 1))
(check-true (unoccupied? t4 j/wall 2))
(check-true (unoccupied? t4 j/wall 3))
(check-true (not (unoccupied? t4 j/wall 4)))


;; possible-homes :: Tile Board -> [Int]
;; return the indices for staginglines that the given color of tile can be placed
(define possible-homes
  (λ (color b)
    (match b
      [(board score wall stage extra 1t)
       (let loop ([i 0])
         (cond
           [(>= i 5) empty]
           [(and  (unoccupied? color wall i)
                  (can-place? color (vector-ref stage i)))
            (cons i (loop (add1 i)))]
           [else (loop (add1 i))]))])))

(define add-tiles-err-msg
  (λ (c c2)
    (format (string-append  "can't add tiles to a staging line that "
                            "has a different color already;"
                            "\n expected: ~a \n given: ~a")
            c
            c2)))



;; add-tiles-to-line :: Number Tile StagingLine -> StagingLine
;; this is the line we want, add the tiles, give back the staging area and how many extra
(define add-tiles-to-line
  (λ (amt color line)
    (let* ([capacity (car line)]
           [reasonable-amt (min amt capacity)])
      (match line
         [`(,idx . #f)
          `(,idx . (,reasonable-amt . ,color))]
         [`(,idx . (,k . ,color2))
          (when (not (eqv? color color2))
            (error 'add-tiles-to-line (add-tiles-err-msg color color2)))
          (let* ([k-cap (- idx k)]
                 [new-k (+ k (min reasonable-amt k-cap))])
            `(,idx . (,new-k . ,color2)))]))))


(define update-1t
  (lambda (1t 1t?)
    (or 1t (and 1t? #t))))

;; wipe-turn :: Board [Tile] Factory-Set Boolean -> (Values Board FactorySet)
(define wipe-turn
  (λ (b tiles factoryset 1t?)
    (match b
      [(board score wall stage extra 1t)
       (define amt-tiles (length tiles))
       (define new-extra (min (if 1t? (add1 amt-tiles) amt-tiles)
                              (length overflow)))
       (values (board score wall stage new-extra (update-1t 1t 1t?)) factoryset)])))

(define ex-fact-set (factory-set (list L L Y
                                       B R Y K)
                                 (list (list L Y K B))))

(define bug-line (cons 5 (cons 4 L)))
;; pull the lightblues from middle and put on 4th staging line
;; 4th stage line has 4 lightblues already!
 
#;
(define already-placed-here
  (λ (i c w)
    (match-let ([`(,x . ,y) (get-tile-spot (sub1 i) c w)])
      (cdr (vec-get w x y)))))
#;
(define already-placed-error
  (λ (color idx wall)
    (format
     (string-append  "can't add color ~a to this staging line because"
                     "there's already a tile on the wall for line ~a of this color"
                     " \n wall: ~a")
     color (sub1 idx) wall)))

;; should come up with a way to automate tests like:
;; start a game, both players take directly to their overflow
;; verify no tiles in staging area or wall, took minus however many tiles they
;; picked up

;; add-tiles :: Board Factory Number Number Color -> (Values Board FactorySet)
;; the idx is 1-based! this is so we can encapsulate a staging-area's capacity by its idx
;; todo item: need to make sure users arent adding tiles to stagelines
;; where the stageline already has a tile on the board
(define add-tiles
  (λ (b fset f idx color)
    (match b
      [(board score wall stage extra 1t)
       #;
       (when (already-placed-here idx color wall)
         (error 'add-tiles (already-placed-error color idx wall)))
       (define ->overflow? (> idx 5))
       (define-values (tiles new-fset) (pull-from-factory f color fset))
       (define 1t? (member one-tile tiles))
       (cond
         (->overflow? (wipe-turn b tiles new-fset 1t?))
         (else  
          (define amount-to-add
            (let ([n (length tiles)])
              (if 1t? (sub1 n) n)))
          ;; given a stage area, an index for a stage line
          ;; return how many tiles are there right now
          (define current-amount
            (cond
              [(and (not ->overflow?) (cdr (vector-ref stage (sub1 idx)))) => car]
              [else 0]))
          #;
          (pretty-print (list "amt:" amount-to-add
                              "idx" idx
                              "current-amount:" current-amount))
          (define leftover
            (max (- amount-to-add (- idx current-amount)) 0))
          #;(pretty-print (list "leftover:" leftover))
          (define new-extra
            (min  (+ leftover (if 1t? (add1 extra) extra))
                  (length overflow)))
          (define new-stage
            (vector-map (match-lambda  
                          [`(,a . ,d) #:when (eqv? a idx)
                                      (begin #;(pretty-print (list "amt-to-add" amount-to-add))
                                             #;(pretty-print (list "pr" (cons a d)))
                                             )
                                      (add-tiles-to-line amount-to-add color `(,a . ,d))]
                          [e e])
                        stage))
          (values
           (board score wall new-stage new-extra (update-1t 1t 1t?))
           new-fset)))])))
#;
(add-tiles (put-staging  blank-board
                         (vector sg-ln0 sg-ln1 sg-ln2 sg-ln3 bug-line))
           ex-fact-set
           -2
           5
           L)


;; j/board uses tsa1 which is board w/ a few pieces, but only one free stageline
;; so we can test it for any color on that staging line with any amount of pieces
;; this will show the overflow count as well

(define test-mid
  (list one-tile Y R R K K B L))

(define test-others
  (list
   (list Y R K L)
   (list Y R K B)
   (list K K Y Y)
   (list B B B B)))

(define test-factory
  (factory-set test-mid test-others))

(define update-tsa1
  (λ  (x)
    (vector
     (cons 1 (cons 1 x))
     (cons 2 (cons 2 t4))
     (cons 3 (cons 3 t3))
     (cons 4 (cons 4 t1))
     (cons 5 (cons 5 t0)))))


(define tsa1-new
  (update-tsa1 R))

(define tsa1.5
  (update-tsa1 L))

; one overflow R plus one 1 tile
;; (check-equal?  (add-tiles j/board test-mid 1 R)
;;                (board 3 j/wall tsa1-new 2 #t))
;; (check-equal?  (add-tiles j/board test-mid 1 L)
;;                (board 3 j/wall tsa1.5 1 #t))
;; (check-equal?  (add-tiles j/board (first test-others) 1 L)
;;                (board 3 j/wall tsa1.5 0 #f))

; contains-full-row?: Board -> Boolean
; true if one of the rows in board's wall is full
(define contains-full-row?
  (match-lambda [(board _ w _ _ _)
                 (= (vector-length (vector-ref w 0))
                    (vector-argmax identity
                                   (vector-map (curry vector-count cdr) w)))]))

(define (num-full-rows b)
  (define w (board-wall b))
  (define wall-width (vector-length (vector-ref w 0)))
  (vector-count (lambda (row) (= wall-width (vector-count cdr row))) w))

; initial board has no full rows
(check-equal? (contains-full-row? b) #f)

; returns #t when any of board's row is full
(for ([row 5])
  (check-equal? (contains-full-row? (foldr (λ (x y) (place-move (cons row x) y))
                                           b '(4 3 2 1 0))) #t))

(define/match (calculate-bonus b)
  (((board _ wall _ _ _))
   (define ls-wall (map vector->list (vector->list wall)))
   (define flat-wall (apply append ls-wall))
   (define (transpose m)
     (apply map list m))

   (define (count-full-rows ls)
     (for/sum ((row ls))
       (if (andmap cdr row) 1 0)))

   (define horizontals (count-full-rows ls-wall))
   (define verticals (count-full-rows (transpose ls-wall)))
   (define fives
     (let* ((existing-tiles (filter cdr flat-wall))
            (groups (group-by car existing-tiles)))
       (count (lambda (x) (>= (length x) 5)) groups)))
   (+ (* fives 10)
      (* horizontals 2)
      (* verticals 7))))

(define (bonusify-board b)
  (struct-copy board b
               (score (+ (board-score b) (calculate-bonus b)))))

(define (test-dummy-board wall)
  (calculate-bonus
   (board 0 wall null null null)))

(check-equal? (test-dummy-board
               (vector (vector `(,t0 . #t) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                       (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f))
                       (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                       (vector `(,t2 . #t) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f))
                       (vector `(,t1 . #t) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f))))
              7)
(check-equal? (test-dummy-board
               (vector (vector `(,t0 . #t) `(,t1 . #t) `(,t2 . #t) `(,t3 . #t) `(,t4 . #t))
                       (vector `(,t4 . #t) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f))
                       (vector `(,t3 . #t) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f) `(,t2 . #f))
                       (vector `(,t2 . #t) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f) `(,t1 . #f))
                       (vector `(,t1 . #t) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #f))))
              9)
(check-equal? (test-dummy-board
               (vector (vector `(,t0 . #t) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f))
                       (vector `(,t4 . #f) `(,t0 . #t) `(,t1 . #f) `(,t2 . #f) `(,t3 . #f))
                       (vector `(,t3 . #f) `(,t4 . #f) `(,t0 . #t) `(,t1 . #f) `(,t2 . #f))
                       (vector `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #t) `(,t1 . #f))
                       (vector `(,t1 . #f) `(,t2 . #f) `(,t3 . #f) `(,t4 . #f) `(,t0 . #t))))
              10)

;##################################################
; Printing Utilties
;##################################################

; board->los : Board Int -> [List String]
; given the player number, produces the string representation of the board
; each line is returned separatelty
(define board->los
  (match-lambda* [`(,(board s w sg-a e 1t?) ,p)
                  (define sg-line->str
                    (match-lambda [`(,max-c . ,s)
                                   (define empty-char ".")
                                   (define char (if s (tile->str (cdr s)) empty-char ))
                                   (define occ-slots (if s (car s) 0))
                                   (define filled-slots (build-list occ-slots (λ (_) char)))
                                   (define rest-slots (build-list (- max-c occ-slots)
                                                                  (λ (_) empty-char)))
                                   (append rest-slots filled-slots)]))
                  (define w-line->str
                    (compose (curry map (λ (x) (if (cdr x)
                                                   (string-append " " (tile->str (car x)) " ")
                                                   (string-append (f->b (tile->col (car x)))
                                                                  "[*]" norm))))
                             vector->list))
                  (define board-rows
                    (map (match-lambda* [`(,i ,l ,w) `(,(number->string i)
                                                       ,(sg-line->str l)
                                                       ,(w-line->str w))])
                         (build-list (vector-length w) (λ (x) x))
                         (vector->list sg-a)
                         (vector->list w)))
                  (define largest-line
                    (vector-argmax (λ (x) x) (vector-map (λ (x) (if x (car x) 0)) sg-a)))
                  (define row->string
                    (match-lambda* [`((,i ,l ,w) ,len)
                                    (define padding (build-list (- len (length l))
                                                                (λ (_) " ")))
                                    (define l-str
                                      (foldr (λ (x y) (string-append x " " y))
                                             "" (append padding l)))
                                    (define w-str
                                      (foldr string-append "" w))
                                    (string-append i "| " l-str " |" w-str)]))

                  (define player-str (string-append "Player #" (number->string p)))
                  (define sep (string-append "+" (make-string (string-length player-str) #\-)
                                             "+ "))
                  (define sep-pad 18)
                  (define separator (string-append sep (make-string sep-pad #\space)))
                  (define score-str (string-append "score: " (number->string s)))
                  (define overflow-str (string-append "#over: "
                                                      (number->string e)
                                                      (if 1t? " *" "")))
                  (define board-strs (map (λ (x) (row->string x largest-line))
                                          board-rows))
                  (define board-str-length (string-length (car board-strs)))

                  (define score-string (string-append sep score-str))
                  (define score-pad (- (string-length separator)
                                       (string-length score-string)))

                  (define player-string (string-append player-str " | " overflow-str))
                  (define player-pad (- (string-length separator)
                                        (string-length player-string)))

                  (append (list (string-append score-string
                                               (make-string score-pad #\space))
                                (string-append player-string
                                               (make-string player-pad #\space))
                                separator)
                          board-strs)]))

; boards->string: [ListOf Board] -> String
; prints the given list of boards
(define (boards->string bs)
  (define parted-boards (split-by (map cons bs (build-list (length bs) add1)) 2))
  (define board-str-sample (board->los b 0))
  (define board-pad (make-string 10 #\space))
  (foldr (λ (boards s)
           (foldr string-append
                  (string-append "\n" s)
                  (foldr (match-lambda* [`((,b . ,i) ,rest)
                                         (map (λ (x y) (string-append x board-pad y))
                                              (board->los b i) rest)])
                         (build-list (length board-str-sample)
                                     (const "\n")) boards))) "\n" parted-boards))
