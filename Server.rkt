(require 2htdp/image)
(require 2htdp/universe)

;; A universe is a (listof iworld)
(define INIT-UNIV '())
(define UNIV2 (list iworld1))
(define UNIV3 (list iworld1 iworld2))
(define UNIV4 (list iworld1 iworld2 iworld3))

;; A to-server message, tsm, is:
;; 1. 'update-world
;; 2. 'turn-change
;; 3. 'world-back
 
#|DATA DEFINITIONS
   
   A card is a structure (define-struct card flip-status match-status card-num position),
   1. flip-status is either the symbol either:
      1. FACE-UP   ('face-up)
      2. FACE-DOWN ('face-down)
   2. match-status is either the symbol:
      1. MATCHED   ('matched)
      2. UNMATCHED ('unmatched)
   3. card-num is an integer 1-5
   4. position is an integer 0-9

   A LoC is a listof Cards

   A turn is either the symbol:
   1. P1-TURN ('p1)
   2. P2-TURN ('p2)
   3. AI-TURN ('ai)

   A move counter (MC) is an int 1-3 that counts the number of moves a give player has made

   A world is a either:
   1. 'uninit-world
   2. a structure (define-struct world LoC turn MC)

|#

;;Structure definitions
(define-struct card (flip-status match-status num position))
(define FACE-UP   'face-up)
(define FACE-DOWN 'face-down)
(define MATCHED   'matched)
(define UNMATCHED 'unmatched)
(define MAX-CARDS 10)
(define STARTING-NUM 1)
(define STARTING-POSITION 0)
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
(define-struct world (cards turn MC))
(define P1-TURN 'p1)
(define P2-TURN 'p2)
(define AI-TURN 'ai)
(define INIT-MC 1)

;;int int -> listof Cards
;;Purpose: Builds a listof cards
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
(define STARTING-CARDS (position-shuffler (random 4)))
(define INIT-SHUFFLE-WORLD (make-world STARTING-CARDS P1-TURN INIT-MC))
(define INIT-LoC (LoC-maker STARTING-NUM STARTING-POSITION MAX-CARDS))
(define INIT-WORLD (make-world INIT-LoC P1-TURN INIT-MC))
(define FINISHED-P2-WORLD (make-world FINISHED-LoC P2-TURN INIT-MC))

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
(check-expect (marshal-cards (make-card FACE-UP UNMATCHED 1 9)) (list 'face-up 'unmatched 1 9))

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

;; universe iworld → bundle Purpose: Add world universe
(define (add-player u iw)
  (if (member? (iworld-name iw) (map iworld-name u))
      (make-bundle u
                   '()
                   (list iw))
      (if (empty? u)
          (make-bundle
           (cons iw u)
           (list (make-mail iw (list 'start (marshal-world INIT-SHUFFLE-WORLD))))
           '())
          (make-bundle
           (cons iw u)
           (cons (make-mail (first u) (list 'send-world (iworld-name iw)))
                 (map
                  (λ (iw)
                    (make-mail iw (list 'new-player (iworld-name iw))))
                  u))
           '()))))

;; Sample expressions for add-new-world
(define ADD1 (make-bundle
              (cons iworld1 INIT-UNIV)
              (list (make-mail iworld1 (list 'start (marshal-world INIT-SHUFFLE-WORLD))))
              '()))
(define ADD2 (make-bundle
              (cons iworld2 UNIV2)
              (cons (make-mail (first UNIV2) (list 'send-world (iworld-name iworld2)))
                    (map
                     (λ (iw)
                       (make-mail iw (list 'new-player (iworld-name iw))))
                     UNIV2))
              '()))

;; Tests using sample computation for add-new-world
(check-expect (add-player INIT-UNIV iworld1) ADD1)
(check-expect (add-player UNIV2 iworld2) ADD2)

;; Tests using sample values for add-new-world
(check-expect (add-player (list iworld2 iworld3) iworld3)
              (make-bundle (list iworld2 iworld3)
                           '()
                           (list iworld3)))
;; universe iworld → universe
;; Purpose: Remove the given iworld from the given universe
(define (rm-player u iw)
  (if (not (empty? u))
      (filter (λ (w) (not (equal? w iw))) u)
      u))

;; Sample expressions for rm-iworld
(define RM1 (filter (λ (w) (not (equal? w iworld2))) UNIV2))
(define RM2 (filter (λ (w) (not (equal? w iworld1))) INIT-UNIV))

;; Tests using sample computations for rm-iworld
(check-expect (rm-player UNIV2 iworld2) RM1)
(check-expect (rm-player UNIV2 iworld1) RM2)

;; Tests using sample values for rm-iworld
(check-expect (rm-player (list iworld1 iworld2 iworld3) iworld2)
              (list iworld1 iworld3))

;; universe iworld tsm → bundle
;;Purpose: Process to-server message
(define (process-message univ an-iw a-tsm)
  (local [(define tag (first a-tsm))
          (define send-list (if (eq? tag 'world-back)
                                (filter (λ (iw) (eq? (iworld-name iw)
                                                     (second a-tsm)))
                                        univ)
                                (filter
                                 (λ (iw) (not (eq? (iworld-name iw)
                                                   (iworld-name an-iw))))
                                 univ)))]
    (cond [(eq? tag 'update-world)
           (make-bundle univ
                        (map (λ (iw) (make-mail iw a-tsm)) send-list)
                        '())]
          [(eq? tag 'turn-change)
           (make-bundle univ
                        (map (λ (iw) (make-mail iw a-tsm)) send-list)
                        '())]
          [(eq? tag 'world-back)
           (make-bundle univ
                        (list (make-mail (first send-list)
                                         (list 'start (rest (rest a-tsm)))))
                        '())]
          [else (error (format "Unknown message received by server: ~s." a-tsm))])))

;; Sample expressions for process-message
(define MW M-INITW)
(define UW-MSG (list 'update-world MW))
(define TC-MSG (list 'turn-change MW))
(define PM-UW (local [(define tag (first UW-MSG))
                      (define send-list (if (eq? tag 'world-back)
                                            (filter (λ (iw) (eq? (iworld-name iw)
                                                                 (second UW-MSG)))
                                                    UNIV3)
                                            (filter
                                             (λ (iw) (not (eq? (iworld-name iw)
                                                               (iworld-name iworld2))))
                                             UNIV3)))]
                (make-bundle UNIV3
                         (map (λ (iw) (make-mail iw UW-MSG)) send-list)
                         '())))
(define PM-TC (local [(define tag (first TC-MSG))
                      (define send-list (if (eq? tag 'world-back)
                                            (filter (λ (iw) (eq? (iworld-name iw)
                                                                 (second TC-MSG)))
                                                    UNIV3)
                                            (filter
                                             (λ (iw) (not (eq? (iworld-name iw)
                                                               (iworld-name iworld3))))
                                             UNIV3)))]
                (make-bundle UNIV3
                             (map (λ (iw) (make-mail iw TC-MSG)) send-list)
                             '())))
;; Tests using sample expressions for process-message
(check-expect (process-message UNIV3 iworld2 UW-MSG) PM-UW)
(check-expect (process-message UNIV3 iworld3 TC-MSG) PM-TC)

;; Any → universe
;; Purpose: Run the universe server
(define (run-server a-z)
  (universe
   INIT-UNIV
   (on-new add-player)
   (on-msg process-message)
   (on-disconnect rm-player)))
