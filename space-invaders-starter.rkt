;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT (image-height TANK))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (list I1))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of invaders

(define LOM1 empty)
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)
                   (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main g)
  (big-bang g                    ; Game
    (on-tick   advance-game)     ; Game -> Game
    (to-draw   render-game)      ; Game -> Image
    (stop-when stop-game)        ; Game -> Boolean
    (on-key    handle-key)))     ; Game KeyEvent -> Game

;; Game -> Game
;; Advance the game
;; !!!
(check-random (advance-game G0)
              (make-game (create-invader-random (advance-invaders (remove-invaders (game-invaders G0) (game-missiles G0))))
                         (advance-missiles (remove-missiles (game-missiles G0) (game-invaders G0)))
                         (advance-tank     (game-tank G0))))

(define (advance-game g)
  (make-game  (create-invader-random
               (advance-invaders (remove-invaders (game-invaders g) (game-missiles g))))
              (advance-missiles (remove-missiles (filter-missiles (game-missiles g)) (game-invaders g)))
              (advance-tank     (game-tank g))))

;; ListOfInvaders -> ListOfInvaders
;; Advance invaders to next positions
(check-expect (advance-invaders
               (list (make-invader 150 100 12)
                     (make-invader 200 50 -12)))
              (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12)
                    (make-invader (+ 200 (* -12 INVADER-X-SPEED)) (+ 50 INVADER-Y-SPEED) -12)))

;; (define (advance-invaders loi) empty) ;stub
;; Template from ListOfInvaders
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (advance-invader (first loi))
                    (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; Advance the invader to next position
(check-expect (advance-invader (make-invader 100 250 10))        ;going right
              (make-invader (+ 100 (* 10 INVADER-X-SPEED))
                            (+ 250 INVADER-Y-SPEED)
                            10))

(check-expect (advance-invader (make-invader 100 250 -10))       ;going left
              (make-invader (+ 100 (* -10 INVADER-X-SPEED))
                            (+ 250 INVADER-Y-SPEED)
                            -10))

(check-expect (advance-invader (make-invader WIDTH 100 10))     
              (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -10))  ;reaching right edge

(check-expect (advance-invader (make-invader 0 300 -12))         ;reaching left edge
              (make-invader 0 (+ 300 INVADER-Y-SPEED) 12))

;; (define (advance-invader i) (make-invader 0 0 1)) ;stub
;; Template from Invader data definition
(define (advance-invader i)
  (cond [(>= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH)
         (make-invader WIDTH
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (* (invader-dx i) -1))]
        [(<= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) 0)
         (make-invader 0
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (* (invader-dx i) -1))]
        [else
         (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Remove the invaders which have been collided with missiles
;; !!!
(check-expect (remove-invaders (list (make-invader 250 300 1))
                               (list (make-missile 100 60)))
              (list (make-invader 250 300 1)))

(check-expect (remove-invaders (list (make-invader 100 250 1)
                                     (make-invader 250 200 -1))
                               (list (make-missile 100 250)
                                     (make-missile 350 400)))
              (list (make-invader 250 200 -1)))

(check-expect (remove-invaders (list (make-invader 50 150 1)
                                     (make-invader 200 60 -1))
                               (list (make-missile 40 140)
                                     (make-missile 210 70)))
              empty)

;; (define (remove-invaders loi lom) empty) ;stub
;; Template from ListOfInvaders
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else (if (hit-invader? (first loi) lom)
                  (remove-invaders (rest loi) lom)
                  (cons (first loi) (remove-invaders (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; Return true if any missile is in the hit region of invader
;; !!!
(check-expect (hit-invader? (make-invader 100 150 1)
                            (list (make-missile 75 100)
                                  (make-missile 50 75)))
              false)

(check-expect (hit-invader? (make-invader 100 150 1)
                            (list (make-missile 90 150)
                                  (make-missile 50 70)))
              true)
               
;; (define (hit-invader? i lom) false) ;stub
;; Template from ListOfMissile
(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else (if (hit? i (first lom))
                  true
                  (hit-invader? i (rest lom)))]))

;; Invader Missile -> Boolean
;; Return true if Missile has hit the Invader
(check-expect (hit? (make-invader 100 150 1)
                    (make-missile 50 70))
              false)

(check-expect (hit? (make-invader 50 75 -1)
                    (make-missile 45 70))
              true)

(check-expect (hit? (make-invader 125 250 1)
                    (make-missile 115 260))
              true)

;; (define (hit? i m) false) ;stub
(define (hit? i m)
   (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
           (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; ListOfInvader -> ListOfInvader
;; Randomly create a new invader at random position
(check-random (create-invader-random empty)
              (if (create-invader? INVADE-RATE)
                  (create-invader empty)
                  empty))

;; (define (create-invader-random loi) empty) ;stub
(define (create-invader-random loi)
  (if (create-invader? INVADE-RATE)
      (create-invader loi)
      loi))

;; ListOfInvader -> ListOfInvader
;; create new invader at random x coordinate
(check-random (create-invader empty)
              (cons (make-invader (random WIDTH) 0 1)
                    empty))

(check-random (create-invader (list (make-invader 162 75 -1)
                                    (make-invader 250 162 1)))
              (cons (make-invader (random WIDTH) 0 1)
                    (list (make-invader 162 75 -1)
                          (make-invader 250 162 1))))

;; (define (create-invader loi) empty) ;stub
;; Template from ListOfInvader data definition
(define (create-invader loi)
  (cons (make-invader (random WIDTH) 0 1) loi))

;; INVADE-RATE -> Boolean
;; Randomly create true or false
(check-random (create-invader? 100)
              (< (random 1000) 100)) 

(define (create-invader? i)
  (< (random 1000) i))

;; ListOfMissiles -> ListOfMissiles
;; Advance missiles to next positions
;; !!!
(check-expect (advance-missiles
               (list (make-missile 175 200)
                     (make-missile 300 150)))
              (list (make-missile 175 (- 200 MISSILE-SPEED))
                    (make-missile 300 (- 150 MISSILE-SPEED))))

;; (define (advance-missiles lom) empty) ;stub
;; Template from ListOfMissile
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (advance-missile (first lom))
                    (advance-missiles (rest lom)))]))

;; Missile -> Missile
;; Advnace the position of single missile
;; !!!
(check-expect (advance-missile (make-missile 150 200))
              (make-missile 150 (- 200 MISSILE-SPEED)))

;; (define (advance-missile m) (make-missile 0 0)) ;stub
;; Template from Missile data definition
(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; Remove missiles with less than 0 height
;; !!!
(check-expect (filter-missiles (list (make-missile 100 50)
                                     (make-missile 100 180)
                                     (make-missile 100 0)
                                     (make-missile 100 -10)))
              (list (make-missile 100 50)
                    (make-missile 100 180)))

;; (define (filter-missiles lom) empty) ;stub
;; Template from ListOfMissile data definition
(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (on-screen? (first lom))
                  (cons (first lom) (filter-missiles (rest lom)))
                  (filter-missiles (rest lom)))]))

;; Missile -> Boolean
;; Produce true if missile is still on screen and false otherwise
(check-expect (on-screen? (make-missile 100 150)) true)
(check-expect (on-screen? (make-missile 50 0)) false)
(check-expect (on-screen? (make-missile 50 -5)) false)

;; (define (on-screen? m) false) ;stub
;; Template from Missile
(define (on-screen? m)
  (> (missile-y m) 0))

;; ListOfMissile ListOfInvader -> ListOfMissiles
;; Remove missiles which have collided with invaders
(check-expect (remove-missiles (list (make-missile 100 60))    ;no hits
                               (list (make-invader 250 300 1)))
              (list (make-missile 100 60)))

(check-expect (remove-missiles (list (make-missile 100 250)    ;single hit
                                     (make-missile 350 400))
                               (list (make-invader 100 250 1)   
                                     (make-invader 250 200 -1)))
              (list (make-missile 350 400)))

(check-expect (remove-missiles (list (make-missile 40 140)      ;all hit
                                     (make-missile 210 70))
                               (list (make-invader 50 150 1)    
                                     (make-invader 200 60 -1)))
              empty)
              
;; (define (remove-missiles lom loi) empty) ;stub
;; Template from ListOfMissile data definition
(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (hit-missile? (first lom) loi)
                  (remove-missiles (rest lom) loi)
                  (cons (first lom) (remove-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; Return true if any invader is in the hit region of missile
;; !!!
(check-expect (hit-missile? (make-missile 100 150)
                            (list (make-invader 75 100 1)
                                  (make-invader 50 75 -1)))
              false)

(check-expect (hit-missile? (make-missile 100 150)
                            (list (make-invader 90 150 1)
                                  (make-invader 50 70 1)))
              true)
               
;; (define (hit-invader? m loi) false) ;stub
;; Template from ListOfInvader
(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [else (if (hit? (first loi) m)
                  true
                  (hit-missile? m (rest loi)))]))

;; Tank -> Tank
;; Advance tank to next position
(check-expect (advance-tank (make-tank CTR-X 0)) ;tank still
              (make-tank CTR-X 0))                
(check-expect (advance-tank (make-tank 100 1))   ;tank going right
              (make-tank (+ 100 TANK-SPEED) 1))
(check-expect (advance-tank (make-tank 100 -1))  ;tank going left
              (make-tank (+ 100 (* -1 TANK-SPEED)) -1))        
(check-expect (advance-tank (make-tank WIDTH 1)) ;tank raching right edge
              (make-tank WIDTH -1))
(check-expect (advance-tank (make-tank 0 -1))    ;tank reaching left edge
              (make-tank 0 1))

;; (define (advance-tank t) T0) ;stub
(define (advance-tank t)
  (cond [(>= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH)
         (make-tank WIDTH -1)]
        [(<= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0)
         (make-tank 0 1)]
        [else 
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

;; Game -> Image
;; render the game
;; !!!
(check-expect (render-game G3)
              (render-invaders (game-invaders G3)
                               (render-missiles (game-missiles G3)
                                                (render-tank (game-tank G3)))))

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))

;; ListOfInvaders -> Image
;; Render the list of invaders
;; !!!
(check-expect (render-invaders (list (make-invader 100 150 12)
                                     (make-invader 50 250 -12)) BACKGROUND)
              (place-image INVADER 100 150
                           (place-image INVADER 50 250 BACKGROUND)))

;; (define (render-invaders loi) empty-image) ;stub
;; Template from ListOfInvader data definition
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (render-invader (first loi)
                              (render-invaders (rest loi) img))]))

;; Invader Image -> Image
;; Render single invader
(check-expect (render-invader (make-invader 100 200 10) BACKGROUND)
              (place-image INVADER 100 200 BACKGROUND))

;; (define (render-invader i) empty-image) ;stub
;; Template from Invader data definition
(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissiles -> Image
;; Render the list of missiles
;; !!!
(check-expect (render-missiles (list (make-missile 100 150)
                                     (make-missile 50 250)) BACKGROUND)
              (place-image MISSILE 100 150
                           (place-image MISSILE 50 250 BACKGROUND)))

;; (define (render-missiles lom) empty-image) ;stub

;; Template from ListOfMissile data definition
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (render-missile (first lom)
                              (render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; Render single missile
(check-expect (render-missile (make-missile 100 200) BACKGROUND)
              (place-image MISSILE 100 200 BACKGROUND))

;; (define (render-missile m) empty-image) ;stub
;; Template from Missile data definition
(define (render-missile i img)
  (place-image MISSILE (missile-x i) (missile-y i) img))

;; Tank -> Image
;; Render the tank
(check-expect (render-tank (make-tank CTR-X 1))
              (place-image/align TANK
                                 CTR-X HEIGHT
                                 "center" "bottom"
                                 BACKGROUND))

;; (define (render-tank t) empty-image) ;stub
;; Template from Tank definition
(define (render-tank t)
  (place-image/align TANK (tank-x t) HEIGHT "center" "bottom" BACKGROUND))

;; Game -> Boolean
;; stop the game
;; !!!
(check-expect (stop-game (make-game (list (make-invader 100 150 1)
                                          (make-invader 100 200 -1))
                                    empty
                                    (make-tank 100 1)))
              false)

(check-expect (stop-game (make-game (list (make-invader 100 HEIGHT 1)
                                          (make-invader 75 20 1))
                                    empty
                                    (make-tank 50 -1)))
              true)

;; (define (stop-game g) false) ;stub
(define (stop-game g)
  (invaders-win? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; Produce true if any invader has reached the height
;; !!!
(check-expect (invaders-win? (list (make-invader 100 150 1)
                                   (make-invader 100 200 -1)))
              false)

;; (define (invader-win? loi) false) ;stub
;; Template from ListOfInvader
(define (invaders-win? loi)
  (cond [(empty? loi) false]
        [else (if (invader-win? (first loi))
                  true
                  (invaders-win? (rest loi)))]))

;; Invader -> Boolean
;; Produce true if invader has reacher the bottom of screen i.e; height
(check-expect (invader-win? (make-invader 100 (+ HEIGHT 10) 1))
              true)
(check-expect (invader-win? (make-invader 150 HEIGHT -1))
              true)
(check-expect (invader-win? (make-invader 100 200 1))
              false)

;; (define (invader-win? i) false) ;stub
;; Template from Invader definition
(define (invader-win? i)
  (>= (invader-y i) HEIGHT))

;; Game KeyEvent -> Game
;; handle key
(check-expect (handle-key
               (make-game empty empty (make-tank CTR-X 0))
               "right")
              (make-game empty empty (make-tank CTR-X 1)))

(check-expect (handle-key
               (make-game empty empty (make-tank 120 1))
               "left")    
              (make-game empty empty (make-tank 120 -1)))

(check-expect (handle-key
               (make-game empty empty (make-tank 120 1))
               " ")
              (make-game empty
                         (list (make-missile 120 (- HEIGHT TANK-HEIGHT)))
                         (make-tank 120 1)))

(check-expect (handle-key
               (make-game empty empty T0)
               "a")
              (make-game empty empty T0)) 

;; (define (handle-key g ke) g) ;stub
(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (change-tank-direction (game-tank g) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (change-tank-direction (game-tank g) 1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (shoot-missile (game-missiles g) (game-tank g))
                    (game-tank g))]
        [else g]))

;; Tank Natural[-1, 1] -> Tank
;; change tank dir to the given Natural
(check-expect (change-tank-direction   ;right -> left
               (make-tank 100 1) -1)
              (make-tank 100 -1))

(check-expect (change-tank-direction   ;left -> right
               (make-tank 100 -1) 1) 
              (make-tank 100 1))

(check-expect (change-tank-direction   ;right -> stop 
               (make-tank 100 1) 0) 
              (make-tank 100 0))

;; (define (change-tank-direction t d) T0) ;stub
;; Template from Tank
(define (change-tank-direction t d)
  (cond [(= d -1) (make-tank (tank-x t) -1)]
        [(= d 0) (make-tank (tank-x t) 0)]
        [(= d 1) (make-tank (tank-x t) 1)]))

;; ListOfMissile -> ListOfMissile
;; Create a new missile at the tank x location
(check-expect (shoot-missile empty (make-tank 35 1))
              (list (make-missile 35 (- HEIGHT TANK-HEIGHT))))
(check-expect (shoot-missile (list (make-missile 25 100)
                                   (make-missile 55 75))
                             (make-tank 100 -1))
              (cons (make-missile 100 (- HEIGHT TANK-HEIGHT)) (list (make-missile 25 100)
                                                                    (make-missile 55 75))))

;; (define (shoot-missile lom t) empty) ;stub
;; Template from ListOfMissile data definition
(define (shoot-missile lom t)
  (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT)) lom))

(main (make-game empty empty (make-tank CTR-X 1)))
