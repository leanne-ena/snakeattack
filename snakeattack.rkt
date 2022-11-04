#lang racket
(require racket/gui)

;--------------------------------------------------------------------------------
; DEFINE VARIABLES

; using list, define the position of the snake
(define snake (list (list 2 1) (list 1 1)))

; define initial direction of snake
(define direction 'd)

; set position of purple snack
(define purplesnack (list 15 15))

; set points counter to 0
(define points 0)

; define length of blocks
; define size of each block in pixels
(define block_length 24)
(define block_size 16)

;--------------------------------------------------------------------------------
; GAME FUNCTIONS

; movement function: remove the last element from the list and add a new one at the beginning of it
(define (movement x y) 
  (reverse (append (cdr (reverse snake)) (list(list x y)))))

; puzzle function: get the element in the index position of the first element in the list
(define (puzzle position lst)
  (list-ref (list-ref lst 0) position))

; move-snake function: define buttons for how the snake can move
; snake will move to the following position ('l(left), 'r(right), 'u(up),'d(down))
(define (move-snake position)
  (case position
    ['l (set! snake (movement (- (puzzle 0 snake) 1) (puzzle 1 snake)))]
    ['r (set! snake (movement (+ (puzzle 0 snake) 1) (puzzle 1 snake)))]
    ['u (set! snake (movement (puzzle 0 snake) (- (puzzle 1 snake) 1)))]
    ['d (set! snake (movement (puzzle 0 snake) (+ (puzzle 1 snake) 1)))]))

; leaned-block function: checks if purplesnack points is in snake list
(define (leaned-block snake block [i 0] [g 666]) 
  ;(displayln g)
  (if (> (length snake) i)  ; length (snake) > i
    (if (and (not (= g i)) (and 
      (eq? (list-ref (list-ref snake i) 0) (list-ref block 0)) 
      (eq? (list-ref (list-ref snake i) 1) (list-ref block 1)))) 
        #t
      (leaned-block snake block (+ i 1) g))
    #f))

; grow-snake function: make the snake grow, copying the last element of the snake list, moving the snake to another position and adding the last element taken earlier
(define grow-snake (lambda () 
  (define x (car (reverse snake)))
  (set! purplesnack (list (inexact->exact (round (* (random) (- block_length 1)))) (inexact->exact (round (* (random) (- block_length 1)))) ))
  (move-snake direction)
  (set! points (add1 points))
  (set! snake (append snake (list x)))))

; restart function: set all variables to their original state
(define restart (lambda()
  (set! direction 'd)
  (set! purplesnack (list 15 15))
  (set! snake (list (list 2 1) (list 1 1)))
  (set! points 0)
))

; startgame function: actualized variables for starting of game
(define startgame (lambda () 
  (draw-block dc (list-ref purplesnack 0) (list-ref purplesnack 1) "purple") ; draw purplesnack
  (cond [(leaned-block snake purplesnack) (grow-snake)] [else (move-snake direction)]) ; check for consumption of purplesnack
  (send dc draw-text (number->string points) (-(* block_length block_size) 30) 10)
  (for ([block snake]) (
    if (eq? block (car snake)) 
      (draw-block dc (list-ref block 0) (list-ref block 1) "green") 
      (draw-block dc (list-ref block 0) (list-ref block 1) "green")))))

; lostgame function: draws text on screen after losing game
(define lostgame (lambda ()
  (send dc draw-text "You Lost Snake Attack :(" (- (round (/ (* block_length block_size) 2)) 110) (- (round (/ (* block_length block_size) 2)) 20))
  (send dc draw-text "total points: "(-(* block_length block_size) 135) 10)
  (send dc draw-text (number->string points) (-(* block_length block_size) 30) 10)
  (send dc draw-text "press s to start again" (- (round (/ (* block_length block_size) 2)) 100) (- (round (/ (* block_length block_size) 2)) 0))
))

; draw-block function: draws blocks on the screen
(define (draw-block screen x y color) 
  (send screen set-brush color 'solid)
  (send screen draw-rectangle (* x block_size) (* y block_size) block_size block_size))

; frame function: create gui window for snake
(define frame (new frame% 
  [label "Snake Attack"]
  [width (+ (* block_length block_size) 16)]
  [height (+ (* block_length block_size) 40)])
  )

; canvas function
(define (canvas-key frame) (class canvas%
  (define/override (on-char key-event)
    (cond
      [(eq? (send key-event get-key-code) 'left) (set! direction 'l)]
      [(eq? (send key-event get-key-code) 'right) (set! direction 'r)]
      [(eq? (send key-event get-key-code) 'up) (set! direction 'u)]
      [(eq? (send key-event get-key-code) 'down) (set! direction 'd)]
      [(eq? (send key-event get-key-code) '#\s) (restart)]))
  (super-new [parent frame])))

; setting up the canvas
(define canvas (
  new (canvas-key frame)))
(define dc (send canvas get-dc))

; setting font style and color
(send dc set-font (make-object font% 14 'default))
(send dc set-text-foreground "white")

; make frame appear
(send frame show #t)

; setting up timer
(define timer (new timer%
  [notify-callback (lambda()
    (send dc clear)
    (send dc set-brush "Dark Slate Gray" 'solid)        
    (send dc draw-rectangle 0 0 (* block_length block_size) (* block_length block_size))
    
    (define collision #f)
    (for ([block snake]
         [j (in-naturals 0)])
      (cond 
            [(or (> (list-ref block 0) block_length) (> 0 (list-ref block 0))) (set! collision #t )]
            [(or (> (list-ref block 1) block_length) (> 0 (list-ref block 1))) (set! collision #t)]
            [(eq? #f collision) (set! collision (eq? #t (leaned-block snake block 0 j)))]))
    
    (if collision (lostgame) (startgame)))]
  [interval #f]))

; start screen for game
(define (screen)
  (message-box "Snake Attack" "WELCOME TO SNAKE ATTACK!!!
Use the arrow keys to move and eat as much purple snack as you can. Click OK to start.                          " #f '(ok))
  (send timer start 80)
)

; starts the game
(screen)