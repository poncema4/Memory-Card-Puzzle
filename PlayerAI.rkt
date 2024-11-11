(require 2htdp/image)
(require 2htdp/universe)

;;TABLE CONSTANTS
(define TABLE-WIDTH  600)
(define TABLE-HEIGHT 400)
(define TABLE-COLOR  'darkred)
(define TABLE (rectangle TABLE-WIDTH TABLE-HEIGHT 'solid TABLE-COLOR))

;;CARD CONSTANTS
(define CARD-IMG-WIDTH  100)
(define CARD-IMG-HEIGHT 150)
(define CARD-IMG-BASE-COLOR   'darkblue)
(define CARD-IMG-BORDER-COLOR 'darkyellow)
(define CARD-IMG-BODY (rectangle (- CARD-IMG-WIDTH 10) (- CARD-IMG-HEIGHT 10) 'solid CARD-IMG-BASE-COLOR))
(define CARD-IMG-BORDER (rectangle CARD-IMG-WIDTH CARD-IMG-HEIGHT 'solid CARD-IMG-BORDER-COLOR))
(define FACE-UP   'face-up)
(define FACE-DOWN 'face-down)
(define MATCHED   'matched)
(define UNMATCHED 'unmatched)
(define CARD-BACK-SYMBOL "$")
(define CARD-FACE-VAL-1 "1")
(define CARD-FACE-VAL-2 "2")
(define CARD-FACE-VAL-3 "3")
(define CARD-FACE-VAL-4 "4")
(define CARD-FACE-VAL-5 "5")
(define MAX-CARDS 10)
(define STARTING-NUM 1)
(define STARTING-POSITION 0)

#|DATA DEFINITIONS
   
   A card is a structure (define-struct card flip-status match-status card-num position),
   1. flip-status is either the symbol either:
      1. FACE-UP   ('face-up)
      2. FACE-DOWN ('face-down)
   2. match-status is either the symbol:
      1. MATCHED   ('matched)
      2. UNMATCHED ('unmatched)
   3. card-num is an integer 1-5
   4. position is an integer 1-10

   A LoC is a listof Cards

   A turn is either the symbol:
   1. P1-TURN ('p1)
   2. P2-TURN ('p2)
   3. AI-TURN ('ai)

   A move counter (MC) is an int 0-2 that counts the number of moves a give player has made

   A world is a either:
   1. 'uninit-world
   2. a structure (define-struct world LoC turn MC)


|#
;;Structure definitions
(define-struct card (flip-status match-status num position))
(define-struct world (cards turn MC))

;;int int int-> listof Cards
;;Purpose: Builds a listof cards in ascending order
(define (LoC-maker STARTING-NUM STARTING-POSITION MAX-CARDS)
  (if (= STARTING-POSITION MAX-CARDS)
      '()
      (cond [(< STARTING-POSITION (- (/ MAX-CARDS 2) 1))
             (cons (make-card FACE-DOWN
                              UNMATCHED
                              STARTING-NUM
                              STARTING-POSITION)
                   (LoC-maker (add1 STARTING-NUM) (add1 STARTING-POSITION) MAX-CARDS))]
            [(= STARTING-POSITION (/ MAX-CARDS 2))
             (cons (make-card FACE-DOWN
                              UNMATCHED
                              (add1 STARTING-NUM)
                              STARTING-POSITION)
                   (LoC-maker STARTING-NUM (add1 STARTING-POSITION) MAX-CARDS))]
            [else (cons (make-card FACE-DOWN
                                   UNMATCHED
                                   STARTING-NUM
                                   STARTING-POSITION)
                        (LoC-maker (sub1 STARTING-NUM) (add1 STARTING-POSITION) MAX-CARDS))])))

;;Sample Expressions
(define 12-CARD-DECK (if (= 0 12)
                         '()
                         (cond [(< 0 (- (/ 12 2) 1))
                                (cons (make-card FACE-DOWN
                                                 UNMATCHED
                                                 1
                                                 0)
                                      (LoC-maker (add1 1) (add1 0) 12))]
                               [(= 0 (/ MAX-CARDS 2))
                                (cons (make-card FACE-DOWN
                                                 UNMATCHED
                                                 (add1 1)
                                                 0)
                                      (LoC-maker 1 (add1 0) 12))]
                               [else (cons (make-card FACE-DOWN
                                                      UNMATCHED
                                                      1
                                                      0)
                                           (LoC-maker (sub1 1) (add1 0) 12))])))
(define 8-CARD-DECK (if (= 0 8)
                         '()
                         (cond [(< 0 (- (/ 8 2) 1))
                                (cons (make-card FACE-DOWN
                                                 UNMATCHED
                                                 1
                                                 0)
                                      (LoC-maker (add1 1) (add1 0) 8))]
                               [(= 0 (/ MAX-CARDS 2))
                                (cons (make-card FACE-DOWN
                                                 UNMATCHED
                                                 (add1 1)
                                                 0)
                                      (LoC-maker 1 (add1 0) 8))]
                               [else (cons (make-card FACE-DOWN
                                                      UNMATCHED
                                                      1
                                                      0)
                                           (LoC-maker (sub1 1) (add1 0) 8))])))

;;Sample test using sample expressions
(check-expect (LoC-maker 1 0 12) 12-CARD-DECK)
(check-expect (LoC-maker 1 0 8)  8-CARD-DECK)

;;Sample test using sample values
(check-expect (LoC-maker 1 0 4) (list
                                 (make-card FACE-DOWN UNMATCHED 1 0)
                                 (make-card FACE-DOWN UNMATCHED 2 1)
                                 (make-card FACE-DOWN UNMATCHED 2 2)
                                 (make-card FACE-DOWN UNMATCHED 1 3)))

;;num -> LoC
;;Purpose: Picks a formation of random card
(define (position-shuffler a-num)
  (cond [(= a-num 0)  (list(make-card 'face-down 'unmatched 5 0)
                           (make-card 'face-down 'unmatched 1 1)
                           (make-card 'face-down 'unmatched 4 2)
                           (make-card 'face-down 'unmatched 3 3)
                           (make-card 'face-down 'unmatched 2 4)
                           (make-card 'face-down 'unmatched 2 5)
                           (make-card 'face-down 'unmatched 1 6)
                           (make-card 'face-down 'unmatched 4 7)
                           (make-card 'face-down 'unmatched 3 8)
                           (make-card 'face-down 'unmatched 5 9))]
        [(= a-num 1) (list (make-card 'face-down 'unmatched 1 0)
                           (make-card 'face-down 'unmatched 2 1)
                           (make-card 'face-down 'unmatched 4 2)
                           (make-card 'face-down 'unmatched 3 3)
                           (make-card 'face-down 'unmatched 5 4)
                           (make-card 'face-down 'unmatched 3 5)
                           (make-card 'face-down 'unmatched 1 6)
                           (make-card 'face-down 'unmatched 2 7)
                           (make-card 'face-down 'unmatched 5 8)
                           (make-card 'face-down 'unmatched 4 9))]
        [(= a-num 2) (list (make-card 'face-down 'unmatched 4 0)
                           (make-card 'face-down 'unmatched 3 1)
                           (make-card 'face-down 'unmatched 1 2)
                           (make-card 'face-down 'unmatched 5 3)
                           (make-card 'face-down 'unmatched 2 4)
                           (make-card 'face-down 'unmatched 5 5)
                           (make-card 'face-down 'unmatched 4 6)
                           (make-card 'face-down 'unmatched 1 7)
                           (make-card 'face-down 'unmatched 3 8)
                           (make-card 'face-down 'unmatched 2 9))]
        [else (LoC-maker STARTING-NUM STARTING-POSITION MAX-CARDS)]))

;;Sample tests using sample expressions
(check-random (position-shuffler (random 4)) (position-shuffler (random 4)))

;;GENERAL CONTSTANTS
(define INIT-LoC (LoC-maker STARTING-NUM STARTING-POSITION MAX-CARDS))
(define FINISHED-LoC (list
                      (make-card FACE-UP MATCHED 1 0)
                      (make-card FACE-UP MATCHED 2 1)
                      (make-card FACE-UP MATCHED 3 2)
                      (make-card FACE-UP MATCHED 4 3)
                      (make-card FACE-UP MATCHED 5 4)
                      (make-card FACE-UP MATCHED 5 5)
                      (make-card FACE-UP MATCHED 4 6)
                      (make-card FACE-UP MATCHED 3 7)
                      (make-card FACE-UP MATCHED 2 8)
                      (make-card FACE-UP MATCHED 1 9)))
(define STARTING-CARDS (position-shuffler (random 4)))
(define P1-TURN 'p1)
(define P2-TURN 'p2)
(define AI-TURN 'ai)
(define INIT-MC 1)
(define UNINIT-WORLD 'uninit-world)
(define INIT-WORLD (make-world INIT-LoC P1-TURN INIT-MC))
(define INIT-WORLD2 (make-world
                     (list
                      (make-card 'face-down 'unmatched 1 0)
                      (make-card 'face-up   'matched   2 1)
                      (make-card 'face-down 'unmatched 3 2)
                      (make-card 'face-down 'unmatched 4 3)
                      (make-card 'face-down 'unmatched 5 4)
                      (make-card 'face-down 'unmatched 5 5)
                      (make-card 'face-down 'unmatched 4 6)
                      (make-card 'face-down 'unmatched 3 7)
                      (make-card 'face-up   'matched   2 8)
                      (make-card 'face-down 'unmatched 1 9))
                     P1-TURN
                     3))
(define INIT-WORLD3 (make-world INIT-LoC P2-TURN INIT-MC))
(define INIT-WORLD4 (make-world INIT-LoC AI-TURN INIT-MC))
(define INIT-SHUFFLE-WORLD (make-world STARTING-CARDS P1-TURN INIT-MC))
(define FINISHED-P1-WORLD (make-world FINISHED-LoC P1-TURN INIT-MC))
(define FINISHED-P2-WORLD (make-world FINISHED-LoC P2-TURN INIT-MC))
(define FINISHED-AI-WORLD (make-world FINISHED-LoC AI-TURN INIT-MC))
(define DENIED-STR "Connection denied: game is full")

;;Marshalling and Unmarshalling
;;card -> list
;;Purpose: Marshals the given card
(define (marshal-cards a-card)
  (list (card-flip-status  a-card)
        (card-match-status a-card)
        (card-num          a-card)
        (card-position     a-card)))

;;Sample expressions
(define marshal-5card (list (card-flip-status  (make-card FACE-UP MATCHED 5 5))
                            (card-match-status (make-card FACE-UP MATCHED 5 5))
                            (card-num          (make-card FACE-UP MATCHED 5 5))
                            (card-position     (make-card FACE-UP MATCHED 5 5))))
(define marshal-2card (list (card-flip-status  (make-card FACE-DOWN MATCHED 2 1))
                            (card-match-status (make-card FACE-DOWN MATCHED 2 1))
                            (card-num          (make-card FACE-DOWN MATCHED 2 1))
                            (card-position     (make-card FACE-DOWN MATCHED 2 1))))

;;Sample tests using sample expressions
(check-expect (marshal-cards (make-card FACE-UP MATCHED 5 5))   marshal-5card)
(check-expect (marshal-cards (make-card FACE-DOWN MATCHED 2 1)) marshal-2card)

;;Sample tests using sample values
(check-expect (marshal-cards (make-card FACE-UP UNMATCHED 1 9)) (list 'face-up 'unmatched 1 9))

;;Sample tests using sample values
;;world -> MW
;;Purpose: Marshals the given world
(define (marshal-world a-world)
  (list (map marshal-cards (world-cards a-world))
        (world-turn a-world)
        (world-MC a-world)))

;;Sample expressions
(define M-INITW (list (map marshal-cards INIT-LoC)
                      P1-TURN
                      INIT-MC))
(define M-FP2W (list (map marshal-cards FINISHED-LoC)
                      P2-TURN
                      INIT-MC))

;;Sample tests using sample expressions
(check-expect (marshal-world INIT-WORLD)        M-INITW)
(check-expect (marshal-world FINISHED-P2-WORLD) M-FP2W)

;;Sample tests using sample values
(check-expect (marshal-world FINISHED-P2-WORLD)
              (list
               (list
                (list 'face-up 'matched 1 0)
                (list 'face-up 'matched 2 1)
                (list 'face-up 'matched 3 2)
                (list 'face-up 'matched 4 3)
                (list 'face-up 'matched 5 4)
                (list 'face-up 'matched 5 5)
                (list 'face-up 'matched 4 6)
                (list 'face-up 'matched 3 7)
                (list 'face-up 'matched 2 8)
                (list 'face-up 'matched 1 9))
               'p2
               1))

;;MW -> world
;;Purpose: Marshals the given world
(define (unmarshal-world a-mw)
  (make-world (map (λ (a-card) (make-card (first a-card) (second a-card)
                                          (third a-card) (fourth a-card))) (first a-mw))
              (second a-mw)
              (third a-mw)))

;;Sample expressions
(define UMW-INITW (make-world (map (λ (a-card) (make-card (first a-card) (second a-card)
                                                          (third a-card) (fourth a-card))) (first M-INITW))
                              (second M-INITW)
                              (third M-INITW)))
(define UMW-FP2W (make-world (map (λ (a-card) (make-card (first a-card) (second a-card)
                                                         (third a-card) (fourth a-card))) (first M-FP2W))
                             (second M-FP2W)
                             (third M-FP2W)))

;;Sample test using sample expressions
(check-expect (unmarshal-world M-INITW) UMW-INITW)
(check-expect (unmarshal-world M-FP2W)  UMW-FP2W)

;;Sample test using sample values
(check-expect (unmarshal-world (list
                                (list
                                 (list 'face-up 'matched 1 0)
                                 (list 'face-up 'matched 2 1)
                                 (list 'face-up 'matched 3 2)
                                 (list 'face-up 'matched 4 3)
                                 (list 'face-up 'matched 5 4)
                                 (list 'face-up 'matched 5 5)
                                 (list 'face-up 'matched 4 6)
                                 (list 'face-up 'matched 3 7)
                                 (list 'face-up 'matched 2 8)
                                 (list 'face-up 'matched 1 9))
                                'p2
                                1)) FINISHED-P2-WORLD)

;;world -> scene
;;Purpose: To draw the world into a scene
(define (draw-world a-world)
  (local [;;LoC -> image
          ;;Purpose: Turn the given LoC into images
          (define (draw-world a-LoC)
            (local [;;string -> image
                    ;;Purpose: Create a text with a given string
                    (define (card-symbol-maker a-string)
                      (text a-string 150 'white))

                    (define CARD-SYMBOL (card-symbol-maker CARD-BACK-SYMBOL))
                    (define CARD-NUM-1  (card-symbol-maker CARD-FACE-VAL-1))
                    (define CARD-NUM-2  (card-symbol-maker CARD-FACE-VAL-2))
                    (define CARD-NUM-3  (card-symbol-maker CARD-FACE-VAL-3))
                    (define CARD-NUM-4  (card-symbol-maker CARD-FACE-VAL-4))
                    (define CARD-NUM-5  (card-symbol-maker CARD-FACE-VAL-5))

                    ;;image image image -> card
                    ;;Purpose: Makes a card
                    (define (card-img-maker a-card-symbol)
                      (overlay a-card-symbol
                               CARD-IMG-BODY
                               CARD-IMG-BORDER))

                    (define FACE-DOWN-CARD (card-img-maker CARD-SYMBOL))
                    (define FACE-UP-1CARD  (card-img-maker CARD-NUM-1))
                    (define FACE-UP-2CARD  (card-img-maker CARD-NUM-2))
                    (define FACE-UP-3CARD  (card-img-maker CARD-NUM-3))
                    (define FACE-UP-4CARD  (card-img-maker CARD-NUM-4))
                    (define FACE-UP-5CARD  (card-img-maker CARD-NUM-5))
          
                    ;;card -> image
                    ;;Purpose: Translates the given card into its corresponding image
                    (define (card-img a-card)
                      (cond [(eq? (card-flip-status a-card) FACE-DOWN) FACE-DOWN-CARD]
                            [(eq? (card-num a-card) 1)                 FACE-UP-1CARD]
                            [(eq? (card-num a-card) 2)                 FACE-UP-2CARD]
                            [(eq? (card-num a-card) 3)                 FACE-UP-3CARD]
                            [(eq? (card-num a-card) 4)                 FACE-UP-4CARD]
                            [else FACE-UP-5CARD]))]
              (overlay (above (beside (card-img (list-ref a-LoC 0))
                                      (card-img (list-ref a-LoC 1))
                                      (card-img (list-ref a-LoC 2))
                                      (card-img (list-ref a-LoC 3))
                                      (card-img (list-ref a-LoC 4)))
                              (beside (card-img (list-ref a-LoC 5))
                                      (card-img (list-ref a-LoC 6))
                                      (card-img (list-ref a-LoC 7))
                                      (card-img (list-ref a-LoC 8))
                                      (card-img (list-ref a-LoC 9)))
                              (beside (text "Turn: " 60 'white)
                                      (text (symbol->string (world-turn a-world)) 60 'white)))
                       TABLE)))]
    (if (eq? a-world UNINIT-WORLD)
        TABLE
        (draw-world (world-cards a-world)))))

;;Sample Expressions
(define draw-init-world (if (eq? INIT-WORLD UNINIT-WORLD)
                            TABLE
                            (draw-world INIT-WORLD)))
(define draw-uninit-world (if (eq? UNINIT-WORLD UNINIT-WORLD)
                              TABLE
                              (draw-world UNINIT-WORLD)))

;;Sample test using sample expressions
(check-expect (draw-world INIT-WORLD)   draw-init-world)
(check-expect (draw-world UNINIT-WORLD) draw-uninit-world)

;;Sample test using sample values
(check-expect (draw-world FINISHED-P1-WORLD)
              .)
                                                                                          
;;world key turn-change
(define (process-key a-world a-key)
  (local [;;LoC -> LoC 
          ;;Purpose: Updates the match status of cards
          (define (match-card a-LoC)
            (local [;;card -> boolean
                    ;;Purpose: Determines whether the match for the card has been found
                    (define (match-found? a-card a-LoC)
                      (ormap (λ (a-card2)
                               (and (not (eq? a-card  a-card2))
                                    (eq? (card-flip-status a-card)  FACE-UP)
                                    (eq? (card-flip-status a-card2) FACE-UP)
                                    (=   (card-num a-card)
                                         (card-num a-card2))))
                             a-LoC))]
              (map (λ (a-card)
                   (if (match-found? a-card a-LoC)
                           (make-card FACE-UP
                                      MATCHED
                                      (card-num a-card)
                                      (card-position a-card))
                       (make-card FACE-DOWN
                                  UNMATCHED
                                  (card-num a-card)
                                  (card-position a-card)))) a-LoC)))
          ;;LoC card -> LoC
          ;;Purpose: Change the flip status of a card
          (define (flip-card a-LoC a-card)
            (map (λ (a-card2)
                   (if (and (eq? (card-flip-status a-card2) FACE-DOWN)
                            (eq? a-card a-card2))
                       (make-card FACE-UP
                                  UNMATCHED
                                  (card-num a-card2)
                                  (card-position a-card2))
                       a-card2)) a-LoC))
          ;;turn -> turn
          ;;Purpose: Updates the turn
          (define (turn-change a-turn)
            (cond [(eq? a-turn P1-TURN) P2-TURN]
                  [(eq? a-turn P2-TURN) AI-TURN]
                  [else P1-TURN]))
          ;;key world -> world or package
          ;;Purpose: Creates a new world based on the given key and MC 
          (define (key-process a-key a-world)
            (if (not (eq? (remainder (world-MC a-world) 3) 0))
                (cond [(key=? a-key "1") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (first (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "2") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (second (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "3") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (third (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "4") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (fourth (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "5") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (fifth (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "6") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (sixth (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "7") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (seventh (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "8") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (eighth (world-cards a-world)))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "9") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (second (reverse (world-cards a-world))))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [(key=? a-key "0") (local [(define nw (make-world
                                                             (flip-card (world-cards a-world)
                                                                        (first (reverse (world-cards a-world))))
                                                             (world-turn a-world)
                                                             (add1 (world-MC a-world))))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'update-world mw)))]
                      [else a-world])
                (cond [(key=? a-key "1") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "2") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "3") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "4") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "5") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "6") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "7") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "8") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "9") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [(key=? a-key "0") (local [(define nw (make-world
                                                             (match-card (world-cards a-world))
                                                             (turn-change (world-turn a-world))
                                                             INIT-MC))
                                                 (define mw (marshal-world nw))]
                                           (make-package nw (list 'turn-change mw)))]
                      [else a-world])))]
    
    (if (not (eq? a-world UNINIT-WORLD))
        (if (eq? (world-turn a-world) AI-TURN)
            (key-process a-key a-world)
            a-world)
        a-world)))

;;Sample values using sample expressions
(check-expect (process-key UNINIT-WORLD "1") UNINIT-WORLD)
(check-random (process-key INIT-WORLD4 (number->string (random 10)))
              (process-key INIT-WORLD4 (number->string (random 10))))
(check-expect (process-key INIT-WORLD3 "0") INIT-WORLD3)

;;world tickcounter -> a-world or a package
;;Purpose: Processes the world at a given tick
(define (process-tick a-world)
  (local [(define INIT-TICK-COUNTER 8)
          (define (process-tick a-world INIT-TICK-COUNTER)
            (if (= INIT-TICK-COUNTER 0)
                (process-key a-world (number->string (random 10)))
                (process-tick a-world (sub1 INIT-TICK-COUNTER))))]
    (if (eq? (world-turn a-world) AI-TURN)
      (process-tick a-world INIT-TICK-COUNTER)
      a-world)))

;;Sample expression for process-tick
(define PT-W1 (local [(define INIT-TICK-COUNTER 8)
                      (define (process-tick INIT-WORLD INIT-TICK-COUNTER)
                        (if (= INIT-TICK-COUNTER 0)
                            (process-key INIT-WORLD (number->string (random 10)))
                            (process-tick INIT-WORLD (sub1 INIT-TICK-COUNTER))))]
                (if (eq? (world-turn INIT-WORLD) AI-TURN)
                    (process-tick INIT-WORLD INIT-TICK-COUNTER)
                    INIT-WORLD)))
(define PT-W4 (local [(define INIT-TICK-COUNTER 8)
                      (define (process-tick INIT-WORLD4 INIT-TICK-COUNTER)
                        (if (= INIT-TICK-COUNTER 0)
                            (process-key INIT-WORLD4 (number->string (random 10)))
                            (process-tick INIT-WORLD4 (sub1 INIT-TICK-COUNTER))))]
                (if (eq? (world-turn INIT-WORLD4) AI-TURN)
                    (process-tick INIT-WORLD4 INIT-TICK-COUNTER)
                    INIT-WORLD4)))

;;Sample values for process-tick
(check-expect (process-tick INIT-WORLD)  PT-W1)

;;world tpm -> world
;;Purpose: Processes the given message from the server
(define (process-message a-world a-tpm)
  (local [(define tag (first a-tpm))]
    (cond [(eq? tag 'send-world)
           (local [(define name (second a-tpm))]
             (make-package (make-world (world-cards a-world)
                                       P1-TURN
                                       (world-MC a-world)) (cons 'world-back
                                                                 (cons name
                                                                       (marshal-world a-world)))))]
          [(eq? tag 'start)
           (local [(define world (unmarshal-world (second a-tpm)))
                   (define cards (world-cards world))
                   (define MC    (world-MC world))]
             (make-world cards P1-TURN MC))]
          [(eq? tag 'new-player)
           (make-world (world-cards a-world)
                       P1-TURN
                       (world-MC a-world))]
          [(eq? tag 'update-world)
                (local [(define world (unmarshal-world (second a-tpm)))
                        (define cards (world-cards world))
                        (define turn (world-turn world))
                        (define MC (world-MC world))]
                  (make-world cards turn MC))]
          [(eq? tag 'turn-change)
                (local [(define world (unmarshal-world (second a-tpm)))
                        (define cards (world-cards world))
                        (define turn  (world-turn world))
                        (define MC (world-MC world))]
                  (make-world cards turn MC))]
          [else (error (format "Unknown message type received: ~s" (first a-tpm)))])))

;;Sample expressions
(define MW M-INITW)
(define ST-MSG (list 'start MW))
(define SW-MSG (list 'send-world "world2"))
(define NP-MSG (list 'new-player INIT-WORLD))
(define UW-MSG (list 'update-world MW))
(define TC-MSG (list 'turn-change MW))

(define PM-ST (local [(define world (unmarshal-world (second ST-MSG)))
                      (define cards (world-cards world))
                      (define MC    (world-MC world))]
                (make-world cards P1-TURN MC)))
(define PM-SW (local [(define name (second SW-MSG))]
             (make-package (make-world (world-cards INIT-WORLD)
                                       P1-TURN
                                       (world-MC INIT-WORLD)) (cons 'world-back
                                                                 (cons name
                                                                       (marshal-world INIT-WORLD))))))
(define PM-NP (make-world (world-cards INIT-WORLD)
                       P1-TURN
                       (world-MC INIT-WORLD)))
(define PM-UW (local [(define world (unmarshal-world (second UW-MSG)))
                      (define cards (world-cards world))
                      (define turn (world-turn world))
                      (define MC (world-MC world))]
                (make-world cards turn MC)))
(define PM-TC (local [(define world (unmarshal-world (second TC-MSG)))
                      (define cards (world-cards world))
                      (define turn (world-turn world))
                      (define MC (world-MC world))]
                (make-world cards turn MC)))

;;Sample tests using sample expressions
(check-expect (process-message INIT-WORLD ST-MSG) PM-ST)
(check-expect (process-message INIT-WORLD SW-MSG) PM-SW)
(check-expect (process-message INIT-WORLD NP-MSG) PM-NP)
(check-expect (process-message INIT-WORLD UW-MSG) PM-UW)
(check-expect (process-message INIT-WORLD TC-MSG) PM-TC)

;;Sample tests using sample values
(check-error (process-message INIT-WORLD (list 'connection-denied MW)) "Unknown message type received: connection-denied")
                                                                   
;;a-world -> boolean
;;Purpose: Determines if the game is over
(define (game-over? a-world)
  (or  (eq? a-world UNINIT-WORLD)
       (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
               (world-cards a-world))))

;;Sample Expressions
(define NOT-GAME-OVER1 (or (eq? UNINIT-WORLD UNINIT-WORLD)
                           (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
                                   (world-cards UNINIT-WORLD))))
(define NOT-GAME-OVER2 (or (eq? INIT-WORLD UNINIT-WORLD)
                           (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
                                   (world-cards INIT-WORLD))))
(define GAME-OVER1 (or (eq? FINISHED-P1-WORLD UNINIT-WORLD)
                       (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
                               (world-cards FINISHED-P1-WORLD))))
(define GAME-OVER2 (or (eq? UNINIT-WORLD UNINIT-WORLD)
                       (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
                               (world-cards FINISHED-P1-WORLD))))
;;Sample tests using sample expressions
(check-expect (game-over? UNINIT-WORLD)      NOT-GAME-OVER1)
(check-expect (game-over? INIT-WORLD)        NOT-GAME-OVER2)
(check-expect (game-over? FINISHED-P1-WORLD) GAME-OVER1)
(check-expect (game-over? UNINIT-WORLD) GAME-OVER2)

;;Sample tests using sample values
(check-expect (game-over? (make-world
                           (list
                            (make-card 'face-down 'unmatched 1 0)
                            (make-card 'face-up   'matched   2 1)
                            (make-card 'face-down 'unmatched 3 2)
                            (make-card 'face-down 'unmatched 3 3)
                            (make-card 'face-up   'matched   2 4)
                            (make-card 'face-down 'unmatched 1 5))
                           'p2
                           0))
              #false)
(check-expect (game-over? (make-world
                           (list
                            (make-card 'face-up 'matched 1 0)
                            (make-card 'face-up 'matched 2 1)
                            (make-card 'face-up 'matched 3 2)
                            (make-card 'face-up 'matched 3 3)
                            (make-card 'face-up 'matched 2 4)
                            (make-card 'face-up 'matched 1 5))
                           'p2
                           0))
              #true)

;; world --> scene 
;; Purpose: To draw the game's final scene
(define (draw-last-world a-world)
  (if (and (andmap (λ (a-card) (eq? (card-match-status a-card) MATCHED)) 
                       (world-cards a-world))
               (eq? (world-turn a-world) AI-TURN))
          (place-image (text "!!!YOU WON!!!" 90 'Green)
                       (/ TABLE-WIDTH 2)
                       (/ TABLE-HEIGHT 2)
                       (draw-world a-world))
          (place-image (text "YOU LOST :(" 90 'pink)
                       (/ TABLE-WIDTH 2)
                       (/ TABLE-HEIGHT 2)
                       (draw-world a-world))))
    
        
;;Sample Expressions
(define LW-P1-LOSS (place-image (text "YOU LOST :(" 90 'Pink)
                       (/ TABLE-WIDTH 2)
                       (/ TABLE-HEIGHT 2)
                       (draw-world FINISHED-P1-WORLD)))
(define LW-P2-LOSS (place-image (text "YOU LOST :(" 90 'Pink)
                                   (/ TABLE-WIDTH 2)
                                   (/ TABLE-HEIGHT 2)
                                   (draw-world FINISHED-P2-WORLD)))

;;Sample tests using sample expressions
(check-expect (draw-last-world FINISHED-P1-WORLD) LW-P1-LOSS)
(check-expect (draw-last-world FINISHED-P2-WORLD) LW-P2-LOSS)
                                                                                         
;;Sampke tests using sample values
(check-expect (draw-last-world FINISHED-AI-WORLD)
              .)

(define (run a-name)
  (local [(define TICK-RATE 1/3)]
    (big-bang INIT-SHUFFLE-WORLD
      [on-draw draw-world]
      [name a-name]
      [on-key process-key]
      [on-tick process-tick TICK-RATE]
      [stop-when game-over? draw-last-world]
      [register LOCALHOST]
      [on-receive process-message])))
