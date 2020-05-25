#lang racket

(require racket/class
         "tile.rkt"
         "factory.rkt"
         "board.rkt"
         "state.rkt")

(provide (all-defined-out))

(define -inf -10000000)
(define +inf  10000000)


; Player is a (State Board -> Move)
; Move is a `(,FactoryIdx ,Number ,LineNum)

; valid-default-azul-moves: -> [ListOf Move]
; all possible moves that can be made on the current board with the current state
; including moving tiles to trash
(define (valid-default-azul-moves state board)
  (define factories (necessary-factories (state-np state)))
  (define num-staging-lines (vector-length (board-staging board)))
  (define valid-factories (cons -1 (build-list factories add1)))
  (define valid-lines (cons 6 (build-list num-staging-lines identity)))
  (for*/list ([f valid-factories]
              [l valid-lines]
              [t (length tiles)]
              #:when (valid-move? (list f t l) board state))
    (sanitize f t l)))

; analyze-move: Move State Board -> (Pair Move Integer)
(define (analyze-move m state board)
  `(,m . ,(let-values ([(b _) (add-tiles board (state-fset state)
                                         (sub1 (car m)) (add1 (caddr m)) (cadr m))])
            (board-score (bonusify-board (update-score b))))))

;; ___B    ->   _ _ _ _ _
;; _BBB    ->
;; ____
;; __BB

(define (filter-good state ls board)
  (map car (take (sort (map (λ (m) (analyze-move m state board)) ls)
                       (λ (x y) (> (cdr x) (cdr y))))
                 (min 15 (length ls)))))

(define (filter-dumb state ls board)
  (take (shuffle ls) (min 10 (length ls))))

(define (filter-none state ls board) ls)

(define (minimax-final-score state)
  (define TIE-SCORE (/ +inf 2))
  (define WIN-SCORE +inf)
  (define LOSE-SCORE -inf)
  (define-values (scores winners _ __) (end-info state))
  (define len (length winners))
  (build-list (state-np state)
              (lambda (x)
                (if (member (add1 x) winners)
                    (if (>= len 2) TIE-SCORE WIN-SCORE) LOSE-SCORE))))

; minimax :: State
;            (State -> Integer)
;            Integer
;            (State Board [Move] -> [Move])
;            -> (Pair [Either Move #false] Integer)
;; returns the best move and the score associated with that move
;; implements two player minimax

(define (delete x ls)
  (cond
    ((empty? ls) null)
    ((zero? x) (cdr ls))
    (else (cons (car ls) (delete (sub1 x) (cdr ls))))))

(define (g-avg ls turn)
  (let ((l (delete turn ls)))
    (- (list-ref ls turn) (/ (apply + l) (length l)))))

(define (g-sum ls turn)
  (let ((l (delete turn ls)))
    (- (list-ref ls turn) (apply + l))))

(define (g-max ls turn)
  (let ((l (delete turn ls)))
    (- (list-ref ls turn) (apply max l))))

;; For multiplayer minimax, we can return an nDimensional `vector`, and have each
;; player maximize their index. That's easy to do, so we should first try to beat
;; rojo-ai by making minimax better.
(define (minimax state h depth filt g)
  (cond
    ; end of Azul, return scores based on who won/lost
    [(last-round? state) (cons #f (minimax-final-score state))]
    ; end of current round/recursion, return scores based on heuristic `h`
    [(or (round-end? state) (zero? depth)) (cons #f (h state))]
    [else
     (match-let* ([board (list-ref (state-bs state) (state-t state))]
                  [valid-moves (filt state (valid-default-azul-moves state board) board)]
                  [turn (state-t state)]
                  [best-val (build-list (state-np state)
                                        (lambda (x)
                                          (if (= x turn) -inf +inf)))])
       (foldr (λ (curr-move best-res)
                (match (minimax (play-move curr-move state) h (sub1 depth) filt g)
                  [`(,_ . ,curr-val) (if (> (g curr-val turn) (g (cdr best-res) turn))
                                         `(,curr-move . ,curr-val)
                                         best-res)]))
              `(#f . ,best-val) valid-moves))]))


; AI using minimax to make the next move
(define (minimax-ai heuristic depth filt g)
  (λ (state board)
    (match (minimax state heuristic depth filt g)
     [`(,move . ,_) #;(displayln (format "Playing: ~a" move)) move]
     [_ (error 'minimax-ai "minimax didn't return an expected pair")])))

(define (αβ α β state h depth filt)
  (define (comp α β bv cv bm cm turn)
    (cond
      [(zero? turn)`(,(max α cv) ,β ,(if (or (> cv bv) (not bv)) cm bm) ,(max bv cv))]
      [else        `(,α ,(min β cv) ,(if (or (< cv bv) (not bv)) cm bm) ,(min bv cv))]))
  (cond
    ; end of Azul, return scores based on who won/lost
    [(last-round? state) (cons #f (minimax-final-score state))]
    ; end of current round/recursion, return scores based on heuristic `h`
    [(or (round-end? state)
         (zero? depth))  (cons #f (h state))]
    [else (match-let* ([board       (list-ref (state-bs state) (state-t state))]
                       [valid-moves (filt state (valid-default-azul-moves state board) board)]
                       [trn         (state-t state)]
                       [best-val    (if (zero? trn) -inf +inf)]
                       [fold-f (λ (k) (match-lambda*
                                        [`(,curr-move (,α ,β ,bm ,bv))
                                         (if (>= α β)
                                             (k `(,α ,β #f ,bv))
                                             (match (αβ α β (play-move curr-move state) h (sub1 depth) filt)
                                               [`(,_ . ,curr-val) (comp α β bv curr-val bm curr-move trn)]))]))]
                       [`(,α ,β ,bm ,bv) (let/cc k (foldr (fold-f k) `(,α ,β #f ,best-val) valid-moves))])
            (cons bm bv))]))

(define (αβ-ai heuristic depth filt)
  (λ (state board)
    (match (αβ -inf +inf state heuristic depth filt)
     [`(,move . ,_) #;(displayln (format "Playing: ~a" move)) move]
     [_ (error 'αβ-ai "αβ didn't return an expected pair")])))

; AI as seen in Rojo
; Tried to make this look as similar as possible
(define (rojo-ai state board)
  ;(displayln (state->string state))
  (define non-full-sglines
    (filter (match-lambda [`(,mx . ,ss) (or (not ss) (> mx (car ss)))])
            (vector->list (board-staging board))))

  ; assuming the default Azul board below
  (define demands
    (append
     (map (λ (t) `(demand 6 ,t -1000 1)) tiles)
     (for*/list ([sg-line non-full-sglines]
                 [t tiles]
                 #:when (not (invalid-tile-move? t (sub1 (car sg-line)) board)))
       (define row (sub1 (car sg-line)))
       (define wall (board-wall board))
       (define missing (if (cdr sg-line) (cadr sg-line) (add1 row)))
       (define p (get-tile-spot row t wall))
       `(demand ,row
                ,t
                ,(+ (count-points p wall)
                    (calculate-bonus (place-move p board)))
                ,(- (car sg-line) missing)))))

  (define factories (factory-set-factories (state-fset state)))
  (define mid (factory-set-middle (state-fset state)))
  (define mid-contains-one? (member one-tile mid))

  ; the following is disgusting, but it's a translation of rojoAI
  (define-values (highest-score best-tile-set)
    (for*/fold ([highest-score -1000]
                [best-option null])
               ([f (cons (cons -1 mid)
                         (map cons (build-list (length factories) add1)
                              factories))]
                [t tiles] #:when (member t (cdr f)))
      (define fact-t-count (length (filter (λ (t0) (equal? t t0)) (cdr f))))
      (define-values (best-score best-line)
        (for/fold ([best-score -1000]
                   [best-line null])
                  ([d demands] #:when (equal? (caddr d) t))

          (define row     (cadr d))
          (define score   (cadddr d))
          (define missing (car (cddddr d)))
          (define adjusted-score (if (and mid-contains-one?
                                          (negative? (car f)))
                                     (sub1 score) score))

          (define perfect-fit (if (= missing fact-t-count) 1 0))
          (define bonus-score
            (if (<= fact-t-count missing)
                (+ adjusted-score
                   perfect-fit
                   (+ (* 1.1 fact-t-count)
                      (* 0.1 missing)))
                (- adjusted-score (sum (take overflow (min (- fact-t-count missing) (length overflow)))))))
          (if (> bonus-score best-score)
              (values bonus-score row)
              (values best-score best-line))))

      (if (> best-score highest-score)
          (values best-score `(tile-set ,(car f) ,t ,fact-t-count ,best-score ,best-line))
          (values highest-score best-option))))
  (match best-tile-set
    [`(tile-set ,f ,t ,fact-t-count ,best-score ,best-line)
     (define move `(,f ,t ,best-line))
     ;(displayln (format "Playing: ~a" move))
     move]
    [_ (pretty-print demands)]))

(define (random-ai state board)
  ;(displayln (state->string state))
  ;(newline)
  ;(displayln (format "Random move for player ~s " (add1 (state-t state))))

  (define all-moves (valid-default-azul-moves state board))
  (define move (list-ref all-moves (random (length all-moves))))
  ;(displayln (format "Playing: ~s " move))
  move)

(define (manual-player state board)
  (displayln (state->string state))
  (newline)
  (displayln (format "You are player ~s " (add1 (state-t state))))
  (display (format "Input (factory-number tile-number line-number): "))
  (try-again state board (λ () (read))))

(define (try-again state board input [str #f])
  (when str
    (displayln str)
    (displayln "try again..."))
  (match (input)
    [(list f-id tile stg-lin-num)
     (cond
       [(not (valid-f-id? f-id (necessary-factories (state-np state))))
        (try-again  state board (λ () (read)) (make-bad-f-id-str f-id))]
       [(not (valid-tile? tile))
        (try-again  state board (λ () (read)) (make-bad-tile-str tile))]
       [(not (valid-line-num? stg-lin-num))
        (try-again  state board (λ () (read)) (make-bad-line-str stg-lin-num))]

       [(invalid-factory-move (idx->tile tile) f-id (state-fset state))
        => (λ (str) (try-again state board (λ () (read)) str))]

       [(invalid-tile-move? (idx->tile tile) stg-lin-num board)
        => (λ (str) (try-again state board (λ () (read)) str))]

       [else (sanitize f-id tile stg-lin-num)])]

    [x (try-again state board  (λ () (read)) (make-bad-input-str x))]))

(define (sanitize f-id tile line-num)
  (define f-id^ (clean-f-id f-id))
  (define tile^ (idx->tile tile))
  (list f-id^ tile^ line-num))


;##################################################
; Validation Utilities
;##################################################

(define (valid-f-id? x num-factories)
  (or (mid-id? x)
      (and (number? x)
           (<= 1 x num-factories))))

;; valid-tile? :: X -> Boolean
(define (valid-tile? x)
  (and (number? x) (< x (length tiles))))

(define (valid-line-num? x)
  (and (number? x)
       (or (<= 0 x 4) (= x 6))))

;; invalid-factory-move :: Tile Number Factory-Set -> (Either String #f)
;; middle: cant take just the 1tile
;;         cant take no tiles
;;         cant take a color thats not there
;; others: cant ask for a color that isnt there

;; check to see the color is at the factory
;; check to see they arent
(define (invalid-factory-move tile f-id fset)
  (define factory-in-question
    (if (mid-id? f-id)
        (factory-set-middle fset)
        (list-ref (factory-set-factories fset) (sub1 f-id))))
  (cond
    [(empty? factory-in-question)
     "can't ask for tiles from empty factory"]
    [(and (eqv? (first factory-in-question) one-tile)
          (empty? (rest factory-in-question)))
     "can't only take the one-tile"]
    [(not (in? tile factory-in-question))
     "cant take a color not in the file factory"]
    [else #f]))

;; invalid-tile-move? : Tile Number Board -> (Either String Boolean)
(define (invalid-tile-move? tile stg-lin-num b)
  (if (>= stg-lin-num (vector-length (board-staging b)))
      #f
      (match b
        [(board score wall stage extra 1t?)
         (define stg-line (vector-ref stage stg-lin-num))
         (define valid-line? (or (not (cdr stg-line)) (equal? (cddr stg-line) tile)))
         (cond
           [(not valid-line?)
            "can't place a tile on a line that contains a different colored tile."]
           [(ormap (λ (x) (and (equal? (car x) tile) (cdr x)))
                   (vector->list (vector-ref (board-wall b) stg-lin-num)))
            "can't place a tile on the same row as the wall where the tile is already placed."]
           [else #f])])))

; valid-move?: Move Board State -> Move
; uses the above predicates to see if the move is valid
(define (valid-move? m b s)
  (match m
    [`(,f ,t ,l) (and (not (invalid-factory-move (idx->tile t) f (state-fset s)))
                      (not (invalid-tile-move? (idx->tile t) l b)))]))

(define (in? x y)
  (and (member x y) #t))

(define (mid-id? x)
  (define middle-ids
    '(middle mid m -1))
  (in? x middle-ids))

(define (clean-f-id f-id)
  (if (mid-id? f-id) -1 f-id))

(define (make-bad-tile-str x)
  (format "oops! expected tile to be one of the listed numbers, got: ~a" x))

(define (make-bad-line-str x)
  (format "oops! expected line number to be between [0,4] or 6, got: ~a" x))

(define (make-bad-f-id-str x)
  (format "oops! expected either 'middle or a number between 1 and 5; \n given: ~a " x))

(define (make-bad-input-str x)
  (format "oops! expected input to be a (list Number Tile Number), like (1 red 1); \n given: ~a" x))
