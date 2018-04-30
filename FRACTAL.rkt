#lang racket
(require racket/gui)
(require rackunit)
;(require 2htdp/image)
;(require 2htdp/universe)

(define-simple-check (check-pos=? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(define-simple-check (check-pos-list=? lop1 lop2)
  (andmap check-pos=? lop1 lop2))

#|
world state
- base image
- composed image
- replication info - 1 or more sets of:
    - location
    - scale
    - (rotation)
program state
- need to sense drawing for a while -- while f key down


goal: fractal generator, interactive program

capture mouse location to draw a pattern on the screen

store as graphic representation -- list of something

find where to place it in the base image

rescale it and continually place there

wish list
- capture user mouse movement and place onto bitmap
- place bitmap onto another bitmap
- rotate and scale bitmap

- capture user input only when in capture mode
- or only for a certain amount of time
- when not in capture mode, click (+ maybe drag)
    sets place + size of smaller fractal piece
- rotation?

- is bitmap transparent where no pixels? yes if you set it
- how to overlay bitmap
  (send [bitmap drawing context] draw-bitmap [bitmap w other img] [x] [y])

Still Seeking
- gui setting callback for mouse events:
   on-subwindow-event
   on-subwindow-char
- how to add pixel to bitmap
hi 
where is on-event for windows/frames/canvas?
must subclass canvas
|#
(define-struct pos [x y])

(define-struct rep [start-loc end-loc scale orient])
; start-loc and end-loc are pos's
; scale is a decimal 0-1 representing the scale factor applied to the replica
; orient is the CCW rotation (in degrees) to be applied to the replica.
;     360 = 0 = no rotation

(define-struct world [base composed replicas])
; base: the base fractal image, as a list of points
; composed: the composed fractal image
; replicas: list of reps for where to place composed img onto base

(define-struct line [points color width style])
; points: list of the points in the line
; color: color% of the line
; width: a number, width of the line
; style: symbol, the style of the line

; ------------ CONSTANTS ------------------

(define DRAWING 'drawing-fractal)
(define SETUP 'setup-fractal)
(define REPLICATING 'replicating-fractal)
(define STATE DRAWING)

(define WINDOW-SIZE 500)
(define IMAGE-SIZE (- WINDOW-SIZE 25))
(define EMPTY-BITMAP (make-object bitmap% WINDOW-SIZE WINDOW-SIZE))
(define ORIGIN (make-pos 0 0))
(define CENTER (make-pos (/ WINDOW-SIZE 2) (/ WINDOW-SIZE 2)))

;; Program state ;;
;; Draw mode indicates that the pen is currently being held down
;; so mouse movements are captured in a line.
(define *PEN-DOWN? #false)
;; In fractal creation mode, user can draw lines to add to the base fractal.
;; If not in creation mode, user can click to set replication points,
;; and hit enter to have the fractal progress another step.
;(define *CREATION-MODE #true)

; The line currently being drawn (added to while mouse is held down)
(define *CURRENT-POINTS '())
; The set of lines comprising the original fractal image
(define *BASE-FRACTAL-LINES '())
; The locations in the window where the fractal image will be added
(define *REPLICA-POINTS '())

; The built-up fractal image that will be continually redrawn
(define *COMBINED-FRACTAL '())

; also need:
; a bitmap-- the composed fractal image
; a set of rep points for the replication locations
; current drawing color
(define COLOR-BLACK (make-object color% 0 0 0))
(define COLOR-WHITE (make-object color% 255 255 255))
(define COLOR-RED (make-object color% 255 0 0))
(define COLOR-GREEN (make-object color% 0 255 0))
(define COLOR-BLUE (make-object color% 0 0 255))
(define *PEN-COLOR COLOR-BLACK)
(define *PEN-STYLE 'solid)
; current drawing thickness
(define *PEN-WIDTH 4)

; ------------ UTILITY FUNCTIONS ------------------

;;;;;; ----- DELEGATE TO STATES ----- ;;;;;;;;
(define (execute-state draw-func setup-func replicate-func)
  (match STATE
    ['drawing-fractal     (or (not draw-func)      (draw-func))]
    ['setup-fractal       (or (not setup-func)     (setup-func))]
    ['replicating-fractal (or (not replicate-func) (replicate-func))]))

(define (wrap func args ...)
  (λ () (func args ...)))


;;;;;;;;;;;; ------- MATH ------- ;;;;;;;;;;;;

; calculate-angle : Number Number -> Angle
; Calculate the angle (in radians) between two points,
; given their dx (x2 - x1) and dy (y2 - y1)
(define (calculate-angle dx dy)
  (let* ([dy/dx (if (= dx 0)
                    ; when on the y axis, should angle be pi/2 or 3pi/2
                    (if (> dy 0) -1000000000 1000000000)
                    ; else we are not on the y axis and can divide normally
                    (/ dy dx))]
         [base-angle (* -1 (atan dy/dx))]
         [angle (if (>= 0 dx)
                    (+ pi base-angle)
                    base-angle)])
    ;(display dy/dx) (display " -- dy/dx\n")
    ;(display base-angle) (display " -- base-angle\n")
    angle))

(check-= (calculate-angle 1 0) 0 .0001)            ; 0º
(check-= (calculate-angle 1 -1) (* pi 1/4) .0001)  ; 45º
(check-= (calculate-angle 0 -1) (* pi 2/4) .0001)  ; 90º
(check-= (calculate-angle -1 -1) (* pi 3/4) .0001) ; 135º
(check-= (calculate-angle -1 0) pi .0001)          ; 180º
(check-= (calculate-angle -1 1) (* pi 5/4) .0001)  ; 225º
(check-= (calculate-angle 0 1) (* pi 6/4) .0001)   ; 270º
(check-= (calculate-angle 1 1) (* pi -1/4) .0001)  ; 315º

(define rt3 (sqrt 3))
(define -rt3 (- (sqrt 3)))
(check-= (calculate-angle rt3 -1) (* pi 1/6) .0001)   ; 30º
(check-= (calculate-angle 1 -rt3) (* pi 2/6) .0001)   ; 60º
(check-= (calculate-angle -1 -rt3) (* pi 4/6) .0001)  ; 120º
(check-= (calculate-angle -rt3 -1) (* pi 5/6) .0001)  ; 150º
(check-= (calculate-angle -rt3 1) (* pi 7/6) .0001)   ; 210º
(check-= (calculate-angle -1 rt3) (* pi 8/6) .0001)   ; 240º
(check-= (calculate-angle 1 rt3) (* pi -2/6) .0001)   ; -60º
(check-= (calculate-angle rt3 1) (* pi -1/6) .0001)   ; -30º

;;;;;; --- POINT --- ;;;;;;;;;

; dist-to-origin : Pos -> Number
; Calculate a point's absolute distance to the origin.
(define (dist-to-origin p)
  (sqrt (+ (sqr (pos-x p)) (sqr (pos-y p)))))

; dist-between-pos : Pos Pos -> Number
; Calculate the distance between the two positions.
(define (dist-between-pos p1 p2)
  (sqrt (+ (sqr (- (pos-x p1) (pos-x p2)))
           (sqr (- (pos-y p1) (pos-y p2))))))

; shift-pos : Pos Number Number -> Pos
; Shift the position by the given x and y.
(define (shift-pos p x y)
  (make-pos (+ (pos-x p) x) (+ (pos-y p) y)))

; pos-max : Pos Pos -> Pos
; Return the posn that is closer to the origin (p1 if equal)
(define (pos-max p1 p2)
  (if (<= (dist-to-origin p1) (dist-to-origin p2))
      p1
      p2))

; pos-min : Pos Pos -> Pos
; Return the posn that is further from the origin
(define (pos-min p1 p2)
  (if (> (dist-to-origin p1) (dist-to-origin p2))
      p1
      p2))

; rotate-pos : Pos Number -> Pos
; rotate the given point around the origin by the given angle
(define (rotate-pos p angle)
  (let ([angle-sin (sin angle)]
        [angle-cos (cos angle)])
    (make-pos (+ (* angle-sin (pos-y p))
                 (* angle-cos (pos-x p)))
              (+ (* angle-cos (pos-y p))
                 (* -1 angle-sin (pos-x p))))))

;;;;;; --- LIST OF POINTS --- ;;;;;;;;;

; top-leftmost : [Listof Pos] -> Point
; Find the top leftmost corner of points in the list (closest to origin)
(define (top-leftmost lop)
  (if (empty? lop)
      (error 'top-leftmost "list of points is empty")
      (make-pos
       (foldr (λ (p leftmost-x) (min (pos-x p) leftmost-x))
              (pos-x (first lop)) (rest lop))
       (foldr (λ (p highest-y) (min (pos-y p) highest-y))
              (pos-y (first lop)) (rest lop)))))

(check-pos=? (top-leftmost (list (make-pos 3 4)
                                 (make-pos 1 1)
                                 (make-pos 1 0)))
             (make-pos 1 0))
(check-pos=? (top-leftmost (list (make-pos 3 4)
                                 (make-pos 1 1)
                                 (make-pos 1 0)
                                 (make-pos 0 1)))
             (make-pos 0 0))

; bottom-rightmost : [Listof Pos] -> Point
; Find the top leftmost position in the list (closest to origin)
(define (bottom-rightmost lop)
  (if (empty? lop)
      (error 'bottom-rightmost "list of points is empty")
      (make-pos
       (foldr (λ (p rightmost-x) (max (pos-x p) rightmost-x))
              (pos-x (first lop)) (rest lop))
       (foldr (λ (p lowest-y) (max (pos-y p) lowest-y))
              (pos-y (first lop)) (rest lop)))))

(check-pos=? (bottom-rightmost (list (make-pos 3 4)
                                     (make-pos 1 1)
                                     (make-pos 1 0)))
             (make-pos 3 4))
(check-pos=? (bottom-rightmost (list (make-pos 3 4)
                                     (make-pos 4 3)
                                     (make-pos 1 1)
                                     (make-pos 1 0)))
             (make-pos 4 4))


; shift-pos-list : [Listof Pos] Number Number -> [Listof Pos]
; shift every Pos in the list by the given x and y value
(define (shift-pos-list lop x y)
  (map (λ (p) (shift-pos p x y)) lop))

(check-pos-list=? (shift-pos-list (list (make-pos 3 4)
                                        (make-pos 1 1)
                                        (make-pos 1 0))
                                  2 7)
                  (list (make-pos 5 11)
                        (make-pos 3 8)
                        (make-pos 3 7)))

; scale-pos-list : [Listof Pos] Number Pos -> [Listof Pos]
; scale the positions in the list by shifting them
; by their distance from top-left posn * some scale factor
; 1. shift to origin
; 2. scale by dist to origin
; 3. shift back
; *** can make more efficient by moving origin shifting
;    calculations into pos-shifter
(define (scale-pos-list lop scalar p)
  (let* ([top-left-pos p]
         [x-dist-to-shift (pos-x top-left-pos)]
         [y-dist-to-shift (pos-y top-left-pos)]
         [lop-at-origin (shift-pos-list lop (* -1 x-dist-to-shift)
                                        (* -1 y-dist-to-shift))]
         [pos-shifter
          (λ (p) (make-pos (* (pos-x p) scalar)
                           (* (pos-y p) scalar)))])
    (shift-pos-list
     (map pos-shifter lop-at-origin)
     x-dist-to-shift y-dist-to-shift)))

(check-pos-list=? (scale-pos-list (list (make-pos 3 4)
                                        (make-pos 1 1)
                                        (make-pos 1 0))
                                  3
                                  (make-pos 1 0))
                  (list (make-pos 7 12)
                        (make-pos 1 3)
                        (make-pos 1 0)))


; rotate-pos-list : [Listof Pos] Number -> [Listof Pos]
; rotate the positions in the list around the origin
(define (rotate-pos-list lop angle)
  (map (λ (p) (rotate-pos p angle)) lop))


;;;;;;;;;; --- LINE --- ;;;;;;;;;;;;;

; top-leftmost-of-line : Line -> Pos
; Find the top leftmost point in a line
(define (top-leftmost-of-line l)
  (top-leftmost (line-points l)))

; shift-line : Line Number Number -> Line
; Shift the points in the line by the given x and y.
(define (shift-line l x y)
  (make-line (shift-pos-list (line-points l) x y)
             (line-color l)
             (line-width l)
             (line-style l)))

; scale-line : Line Number -> Line
; Scale the line's points by the given scalar, relative to the given point
(define (scale-line l scalar p)
  (make-line (scale-pos-list (line-points l) scalar p)
             (line-color l)
             (line-width l)
             (line-style l)))

; rotate-line : Line Number Pos -> Line
; Rotate the line around the given point, by the given angle.
(define (rotate-line l angle p)
  (make-line (rotate-pos-list (line-points l)
                              angle)
             (line-color l)
             (line-width l)
             (line-style l)))

;;;;;;;;;; --- LIST OF LINE --- ;;;;;;;;;;;;;

; top-leftmost-of-lines : [Listof Line] -> Pos
; Find the top leftmost point in a list of line
(define (top-leftmost-of-lines lines)
  (top-leftmost (map (λ(l) (top-leftmost (line-points l))) lines)))

; shift-lines : [Listof Line] Number Number -> [Listof Line]
; Shift the lines by the given x and y.
(define (shift-lines lines x y)
  (map (λ (l) (shift-line l x y)) lines))

; scale-lines : [Listof Line] Number Number -> [Listof Line]
; Scale the lines by the given scalar, relative to the given point.
(define (scale-lines lines scalar p)
  (map (λ (l) (scale-line l scalar p)) lines))

; rotate-lines : [Listof Line] Number Pos -> [Listof Line]
; Rotate the lines around the given point, by the given angle.
(define (rotate-lines lines angle p)
  (map (λ (l) (rotate-line l angle p)) lines))

; crop-lines : [Listof Line] -> [Listof Line]
; move the top left corner point to (0,0) and shift the rest
; of the points accordingly
(define (crop-lines lines)
  ; determine distance to origin
  ; shift points to origin
  (let* ([top-left-pt (top-leftmost
                       (map (λ (ln) (top-leftmost (line-points ln)))
                            lines))]
         [lines-at-origin
          (map (λ (ln) (shift-line ln (* -1 (pos-x top-left-pt))
                                   (* -1 (pos-y top-left-pt))))
               lines)])
    lines-at-origin))

; re-center-fractal : [Listof Line] -> [Listof Line]
; move the original top left corner point to (0,0)
; and shift the rest of the points accordingly
; -- used when replicating the fractal
(define (re-center-fractal lines)
  (let* ([top-left-pt (top-leftmost
                       (map (λ (ln) (top-leftmost (line-points ln)))
                            *BASE-FRACTAL-LINES))]
         [lines-at-origin
          (map (λ (ln) (shift-line ln (* -1 (pos-x top-left-pt))
                                   (* -1 (pos-y top-left-pt))))
               lines)])
    lines-at-origin))

; crop-and-scale-lines : [Listof Line] -> [Listof Line]
; move the lines to the top left corner, scale them to fit the screen
(define (crop-and-scale-lines lines)
  ; determine distance to origin
  ; shift points to origin
  ; determine largest dimension
  ; scale by (largest dim / WINDOW-SIZE)
  (let* ([top-left-pt (top-leftmost
                       (map (λ (ln) (top-leftmost (line-points ln)))
                            lines))]
         [lines-at-origin
          (map (λ (ln) (shift-line ln (* -1 (pos-x top-left-pt))
                                   (* -1 (pos-y top-left-pt))))
               lines)]
         [bottom-right-pt (bottom-rightmost
                           (map (λ (ln) (bottom-rightmost (line-points ln)))
                                lines-at-origin))]
         [largest-dimension (max (pos-x bottom-right-pt)
                                 (pos-y bottom-right-pt))]
         [scale-factor (/ IMAGE-SIZE largest-dimension)])
    ;(display scale-factor)
    (map (λ (ln) (scale-line ln scale-factor (make-pos 0 0)))
         lines-at-origin)))


; --------------------------------------------------
; ------------ PAINTING FUNCTIONS ------------------

; wrap a list of points in a line struct
(define (get-line points)
  (make-line points
             (make-object color% *PEN-COLOR)
             *PEN-WIDTH
             *PEN-STYLE))

(define (my-on-paint base dc)
  (let ([draw-func
         (λ() (draw-fractal-lines dc (cons (get-line *CURRENT-POINTS)
                                           *BASE-FRACTAL-LINES)))]
        [setup-func
         (λ() (draw-fractal-lines dc (append (reps->lines) *COMBINED-FRACTAL)))]
        [repl-func
         (λ() (draw-fractal-lines dc *COMBINED-FRACTAL))])
    (execute-state draw-func setup-func repl-func)))

; note: can draw-lines onto a bitmap-dc and then get its bitmap
; should look into drawing directly onto a bitmap too
; essentilaly will call on-paint onto the side bitmap

; Helper function for drawing a list of lines onto the given canvas
; dc may be a bitmap-dc or a dc.
(define (draw-fractal-lines dc lines)
  (map (λ (ln)
         (send dc set-pen (line-color ln) (line-width ln) (line-style ln))

         (send dc draw-lines (map pair->point (line-points ln))))
       lines))

; convert a pair to a point% object
(define (pair->point p)
  (make-object point% (pos-x p) (pos-y p)))

; Get the lines to draw out of the list of rep points
(define (reps->lines)
  (foldr (λ(r done-lines)
           (if (rep-end-loc r)
               (cons (make-line (list (rep-start-loc r) (rep-end-loc r))
                                COLOR-BLUE 1 'long-dash)
                     done-lines)
               done-lines))
         '() *REPLICA-POINTS))


; ----------- MOUSE HANDLERS ----------------

; helper to update replication point's second point
(define (update-endpoint r me)
  (make-rep (rep-start-loc r)
            (make-pos (send me get-x) (send me get-y))
            (rep-scale r) (rep-orient r)))

; helper to finalize replication point's second point and scale, orientation
(define (finalize-rep r me)
  (let* ([startpoint (rep-start-loc r)]
         [endpoint (make-pos (send me get-x) (send me get-y))]
         [rep-length (dist-between-pos startpoint endpoint)]
         [scale (/ rep-length WINDOW-SIZE)]
         [dx (- (pos-x endpoint) (pos-x startpoint))]
         [dy (- (pos-y endpoint) (pos-y startpoint))]
         [angle (calculate-angle dx dy)])
    (make-rep startpoint endpoint scale angle)))

; mouse drag handler
; append the point to the current line being made
(define (handle-mouse-drag line me)
  (let ([draw-func
         (λ() (unless (not *PEN-DOWN?)
                (let ([new-line
                       (cons (make-pos (send me get-x) (send me get-y)) line)])
                  (set! *CURRENT-POINTS new-line))))]
        [setup-func
         (λ() (unless (not *PEN-DOWN?)
                (set! *REPLICA-POINTS
                      (cons (finalize-rep (first *REPLICA-POINTS) me)
                            (rest *REPLICA-POINTS)))))]
        [repl-func #f])
    (execute-state draw-func setup-func repl-func)))

; left click mouse down handler
(define (handle-mouse-down me)
  (let ([draw-func (λ() (toggle-drawing-state #t))]
        [setup-func
         (λ() (unless *PEN-DOWN?
                (set! *PEN-DOWN? #t)
                (set! *REPLICA-POINTS
                      (cons (make-rep
                             (make-pos (send me get-x) (send me get-y))
                             #f #f #f)
                            *REPLICA-POINTS))))]
        [repl-func #f])
    (execute-state draw-func setup-func repl-func)))

; left click mouse up handler
(define (handle-mouse-up me)
  (let ([draw-func (λ() (toggle-drawing-state #f))]
        [setup-func
         (λ() (unless (not *PEN-DOWN?)
                (set! *PEN-DOWN? #f)
                (set! *REPLICA-POINTS
                      (cons (finalize-rep (first *REPLICA-POINTS) me)
                            (rest *REPLICA-POINTS)))))]
        [repl-func #f])
    (execute-state draw-func setup-func repl-func)))

; drawing state toggle
(define (toggle-drawing-state state)
  (let ([draw-func
         (λ() (if state
                  (set! *PEN-DOWN? #t)
                  (begin (unless (empty? *CURRENT-POINTS)
                           (set! *BASE-FRACTAL-LINES
                                 (cons (get-line *CURRENT-POINTS)
                                       *BASE-FRACTAL-LINES)))
                         (set! *CURRENT-POINTS '())
                         (set! *PEN-DOWN? state))))]
        [setup-func #f]
        [repl-func #f])
    (execute-state draw-func setup-func repl-func)))


; ----------- KEY HANDLERS ----------------

(define (handle-enter-key)
  (let ([draw-func
         (λ() (set! STATE SETUP)
           (set! *COMBINED-FRACTAL *BASE-FRACTAL-LINES))]
        [setup-func
         (λ() (set! STATE REPLICATING))]
        [repl-func
         (λ() (set! *COMBINED-FRACTAL (create-next-replication)))])
    (execute-state draw-func setup-func repl-func)))

(define (handle-delete-key)
  (let ([draw-func
         (λ() (set! *BASE-FRACTAL-LINES '())
           (set! *COMBINED-FRACTAL '())
           (set! *REPLICA-POINTS '()))]
        [setup-func
         (λ() (set! *REPLICA-POINTS '())
           (set! *BASE-FRACTAL-LINES '())
           (set! STATE DRAWING))]
        [repl-func
         (λ() (set! STATE SETUP)
           (set! *COMBINED-FRACTAL *BASE-FRACTAL-LINES)
           (set! *REPLICA-POINTS '()))])
    (execute-state draw-func setup-func repl-func)))

; scale-pen-size : [Listof Line] -> [Listof Line]
(define (scale-pen-size lines scalar)
  (map (λ (ln) (make-line (line-points ln) (line-color ln)
                          (max 0.5 (* scalar (line-width ln))) (line-style ln)))
         lines))

; create the list of points that is the base fractal with the
; old fractal added to it at each replication point
(define (create-next-replication)
  (foldr (λ (r rep-lines) (append (scale-pen-size (create-replica-lines r) 0.9) rep-lines))
         *BASE-FRACTAL-LINES
         *REPLICA-POINTS))

;(define-struct rep [start-loc end-loc scale orient])

; create-replica-lines : Rep -> [Listof Line]
; given a replication instruction, create a transformed copy of comb.d frac
(define (create-replica-lines r)
  ; crop then rotate (at origin) then scale then shift
  (shift-lines (scale-lines (rotate-lines (re-center-fractal *COMBINED-FRACTAL)
                                          (rep-orient r) (rep-start-loc r))
                            (rep-scale r)
                            ORIGIN)
               (pos-x (rep-start-loc r))
               (pos-y (rep-start-loc r))))

; Change the pen color
(define (change-pen-color key-char)
  (match key-char
    [#\w (set! *PEN-COLOR COLOR-WHITE)]
    [#\b (set! *PEN-COLOR COLOR-BLACK)]
    [#\r (set! *PEN-COLOR COLOR-RED)]
    [#\g (set! *PEN-COLOR COLOR-GREEN)]
    [#\l (set! *PEN-COLOR COLOR-BLUE)]
    [else #f]))

; -----------CANVAS CLASS -------------

; Class for the drawing canvas
(define fractal-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc refresh)

    (define/override (on-paint)
      (send (get-dc) suspend-flush)
      (my-on-paint *BASE-FRACTAL-LINES (get-dc))
      (send (get-dc) resume-flush))

    (define/override (on-event mouse-event)
      (let ([left-down (send mouse-event button-down? 'left)]
            [left-up (send mouse-event button-up? 'left)]
            [moving (send mouse-event moving?)])
        (begin
          (cond
            [moving     (handle-mouse-drag *CURRENT-POINTS mouse-event)]
            [left-down  (handle-mouse-down mouse-event)]
            [left-up    (handle-mouse-up mouse-event)])
          (refresh))))

    (define/override (on-char key-event)
      (let ([key-down (send key-event get-key-code)]
            [key-up (send key-event get-key-release-code)])
        (begin
          (cond
            [(and (char? key-down) (char=? key-down #\space))
             (set! *BASE-FRACTAL-LINES (crop-lines *BASE-FRACTAL-LINES))]
            [(and (char? key-down) (char=? key-down #\backspace))
             (handle-delete-key)]
            [(and (char? key-down) (char=? key-down #\return))
             (handle-enter-key)]
            [(and (char? key-down) (char=? key-down #\p))
             (map (λ (rp) (display "\n- - - replica point - - -\n")
                    (display (pos-x (rep-start-loc rp))) (display " : start x, ")
                    (display (pos-y (rep-start-loc rp))) (display " : start y\n")
                    (display (pos-x (rep-end-loc rp))) (display " : end x, ")
                    (display (pos-y (rep-end-loc rp))) (display " : end y\n")
                    (display (rep-orient rp))  (display " -- angle\n")
                    (display (rep-scale rp))  (display " -- scale\n"))
                  *REPLICA-POINTS)]
            [(char? key-down)
             (change-pen-color key-down)])
          (refresh))))))

; -------- GUI COMPONENTS -----------------

(define frame (new frame%
                   [label "F R A C T A L S"]
                   [width WINDOW-SIZE]
                   [height WINDOW-SIZE]))

; create the canvas inside the frame
(new fractal-canvas%
     [parent frame]
     [style '(transparent)])

; Display the gui window!
(send frame show #t)