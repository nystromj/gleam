; add second hand / more joints?
; divide into folders
; study tries and add functionality to c file
; for diagonal gestures if not declared separately: call both directional handlers
; take out repetitive code and replace with higher order procedures
; add-joint function
; gesture names?
; delete gesture function -> converts diagonals into combinations

(define DONE #t)


(define x 0)
(define y 1)
(define z 2)

;;; makes an axis vector that stores positions of the joint along the axis 
(define make-axis
   (lambda () 
      (vector 0
              0
              (make-vector 100 (cons 0 0)))))

;;; creates a joint vector out of three axis vectors
(define make-joint
   (lambda ()
      (vector (make-axis)        ;; x
              (make-axis)        ;; y
              (make-axis))))     ;; z
 
;;; joint vectors     
(define left (make-joint))
(define right (make-joint))

;;; the amount a joint must moved before it is considered a movement
(define movement-threshold 0.05)

;; speed thresholds to determine if the joint is moving slowly or quickly
(define fast-threshold .15)
(define slow-threshold .01)

(io:osc:start-server 7110)
(define (io:osc:receive timestamp address . args)
   (cond ((string=? (list-ref args 0) "l_hand")
          (if (movement? left x (list-ref args 2))
              (store-pos! left x (list-ref args 2))
              (check-axis-still! left x))
          (if (movement? left y (list-ref args 3))
              (store-pos! left y (list-ref args 3))
              (check-axis-still! left y))
          (if (movement? left z (list-ref args 4))
              (store-pos! left z (list-ref args 4))
              (check-axis-still! left z)))))

;;; PROCEDURE:
;;;   movement?
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;;   new-pos, the position the joint is currently at
;;; PURPOSE: 
;;;   determines whether the joint has moved from its previous point
;;; PRODUCED: 
;;;   boolean, whether the joint as moved
;;; PRECONDITIONS:
;;;   movement-threshold defines the amount the joint must move before 
;;;   considered a movement 
;;; POSTCONDITIONS:
;;;   returns true if the distance between the last saved joint position and the 
;;;   current joint position is greater than or equal to movement-threshold
(define movement? 
   (lambda (joint axis new-pos)
      (if (equal? axis z)
          (> (abs (- (/ (get-current-pos joint axis) 100) new-pos)) 
             (* 2 movement-threshold))
          (> (abs (- (/ (get-current-pos joint axis) 100) new-pos)) 
             movement-threshold))))


;;; PROCEDURE:
;;;   increment-counter!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   increments axis counter by one
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   counter should be between 0 and 99
;;; POSTCONDITIONS:
;;;   axis counter has been increased by one. 
;;;   if the counter was 99 at call, it is now 0
(define increment-counter! 
   (lambda (joint axis)
      (if (equal? (get-counter joint axis) 99)
          (set-counter! joint axis 0)
          (set-counter! joint axis (+ (get-counter joint axis) 1)))))

;;; PROCEDURE:
;;;   set-counter!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;;   counter, an int indicating the new counter value
;;; PURPOSE: 
;;;   sets axis counter to given int
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   counter should be between 0 and 99
;;; POSTCONDITIONS:
;;;   
(define set-counter! 
   (lambda (joint axis counter)
      (vector-set! (vector-ref joint axis) 0 counter)))

;;; PROCEDURE:
;;;   get-prevous-counter
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   determines the index of the previous position stored
;;; PRODUCED: 
;;;   int, the index of the previous position stored
;;; PRECONDITIONS:
;;; 
;;; POSTCONDITIONS:
;;;   counter is between 0 and 99
(define get-previous-counter 
   (lambda (joint axis)
      (if (equal? (get-counter joint axis) 0)
          99
          (- (get-counter joint axis) 1))))

;;; PROCEDURE:
;;;   get-prevous-pos
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   returns the position of the joint before the current position
;;; PRODUCED: 
;;;   int, the position
;;; PRECONDITIONS:
;;;   at least two values have been stored
;;; POSTCONDITIONS:
;;;   if less than two positions have been stored, returns 0
(define get-previous-pos
   (lambda (joint axis)
      (car (vector-ref (vector-ref (vector-ref joint axis) 2) 
                       (get-previous-counter joint axis)))))

;;; PROCEDURE:
;;;   get-prevous-time
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   returns the time that the value before the last value stored was stored
;;; PRODUCED: 
;;;   int, the time at storage
;;; PRECONDITIONS:
;;;   at least two values have been stored
;;; POSTCONDITIONS:
;;;   if less than two positions have been stored, returns 0
;;;   time is given by Impromptu's (now) function
(define get-previous-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                        (get-previous-counter joint axis)))))

;;; PROCEDURE:
;;;   get-counter
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   returns the counter index for the axis vector
;;; PRODUCED: 
;;;   int, the counter
;;; PRECONDITIONS:
;;;   counter points to the last stored position
;;; POSTCONDITIONS:
;;;   counter is between 0 and 99
(define get-counter 
   (lambda (joint axis)
      (vector-ref (vector-ref joint axis) 0)))

;;; PROCEDURE:
;;;   get-current-pos
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   returns the last stored position
;;; PRODUCED: 
;;;   int, the position given
;;; PRECONDITIONS:
;;;   
;;; POSTCONDITIONS:
;;;   if no positions have been stored, returns 0
(define get-current-pos
   (lambda (joint axis)
      (car (vector-ref  (vector-ref (vector-ref joint axis) 2)
                        (get-counter joint axis)))))

;;; PROCEDURE:
;;;   get-current-time
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;; PURPOSE: 
;;;   returns the time that the last stored value was stored
;;; PRODUCED: 
;;;   int, the time of storage
;;; PRECONDITIONS:
;;;   
;;; POSTCONDITIONS:
;;;   if no positions have been stored, returns 0
;;;   time is given by Impromptu's (now) function
(define get-current-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (get-counter joint axis)))))

;;; PROCEDURE:
;;;   store-pos!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;;   new-data, the new position
;;; PURPOSE: 
;;;   stores the given position at the appropriate point in the axis vector
;;; PRODUCED: 
;;;   NULL
;;; PRECONDITIONS:
;;;   
;;; POSTCONDITIONS:
;;;   the new position is now stored at the appropriate point in the axis vector, attainable by get-current-pos
;;;   the new position is paired with the time it was stored
;;;   calls track-changes to determine if the new position indicates a change in direction or speed
(define store-pos! 
   (lambda (joint axis new-data)
      (increment-counter! joint axis)      
      (vector-set! (vector-ref (vector-ref joint axis) 2)
                   (get-counter joint axis) 
                   (cons (* 100 new-data) (now)))
      (if (direction-changed? joint axis)
          (set-direction! joint axis (calculate-direction joint axis)))))

;;; PROCEDURE:
;;;   set-direction!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;;   direction, an int
;;; PURPOSE: 
;;;   stores the given direction in the axis vector
;;; PRODUCED: 
;;;   NULL
;;; PRECONDITIONS:
;;;   
;;; POSTCONDITIONS:
;;;   direction value is now stored in the axis vector to be obtained with get-direction
(define set-direction! 
   (lambda (joint axis direction)
      (vector-set! (vector-ref joint axis) 1 direction)))

;;; PROCEDURE:
;;;   get-direction
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector   
;;; PURPOSE: 
;;;   returns the direction stored for the given joint axis
;;; PRODUCED: 
;;;   int, the direction stored
;;; PRECONDITIONS:
;;;   parameter vectors have been defined
;;; POSTCONDITIONS:
;;;   if no direction stored, returns zero
(define get-direction
   (lambda (joint axis)
       (vector-ref (vector-ref joint axis) 1)))

;;; PROCEDURE:
;;;   direction-changed?
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector   
;;; PURPOSE: 
;;;   determines whether the most recent direction matches the stored direction
;;; PRODUCED: 
;;;   boolean, whether direction has changed
;;; PRECONDITIONS:
;;;   a direction has been stored in axis vector
;;;   at least two positions have been stored in axis vector
;;; POSTCONDITIONS:
;;;   does not store new direction
(define direction-changed?
   (lambda (joint axis)
      (not (same-sign? (get-direction joint axis) (calculate-direction joint axis)))))

;;; PROCEDURE:
;;;   same-sign?
;;; PARAMETERS:
;;;   x, an int
;;;   y, an int   
;;; PURPOSE: 
;;;   determines whether the two ints are both positive or both negative
;;; PRODUCED: 
;;;   boolean, whether the ints have the same sign
;;; PRECONDITIONS:
;;;   
;;; POSTCONDITIONS:
;;;   returns true if ints are both positive or both negative
;;;   returns false if one of the ints is zero
;;;   returns true if both ints are zero
(define same-sign? 
   (lambda (x y)
      (if (and (equal? x 0) (equal? y 0))
          #t
          (> (abs (+ x y)) (abs (- x y))))))
     
;;; PROCEDURE:
;;;   calculate-speed
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector   
;;; PURPOSE: 
;;;   calculates the speed between the 2 most recently saved points
;;; PRODUCED: 
;;;   int, the speed
;;; PRECONDITIONS:
;;;   at least 2 positions have recently been stored in joint's axis vector
;;; POSTCONDITIONS:
;;;   if returned int is negative, joint is moving towards origin    
(define calculate-speed
   (lambda (joint axis)
     (/ (calculate-direction joint axis) 
           (/ (- (get-current-time joint axis) (get-previous-time joint axis)) 100))))

;;; PROCEDURE:
;;;   calculate-direction
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector   
;;; PURPOSE: 
;;;   calculates the direction between the 2 most recently saved points
;;; PRODUCED: 
;;;   int, the direction
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's axis vector
;;; POSTCONDITIONS:
;;;   if returned int is negative, joint is moving towards origin
(define calculate-direction
   (lambda (joint axis)
      (- (get-current-pos joint axis) (get-previous-pos joint axis))))

(define axis-stopped? 
   (lambda (joint axis)
      (>= (- (now) (get-current-time joint axis)) (* *second* 2))))

(define check-axis-still!
   (lambda (joint axis)
      (if (and (axis-stopped? joint axis) (not (axis-still? joint axis)))
          (set-direction! joint axis 0))))

(define axis-still? 
   (lambda (joint axis)
      (equal? (get-direction joint axis) 0)))

(define joint-still? 
   (lambda (joint)
      (and (axis-still? joint x)
           (axis-still? joint y)
           (axis-still? joint z))))

;;; PROCEDURE:
;;;   joint-right?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved right
;;; PRODUCED: 
;;;   boolean, whether joint moved right
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's x vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved right
(define joint-right? 
   (lambda (joint)
      (and (< (calculate-direction joint x) 0) (not (axis-stopped? joint x)))))

;;; PROCEDURE:
;;;   joint-left?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved left
;;; PRODUCED: 
;;;   boolean, whether joint moved left
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's x vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved left
(define joint-left? 
   (lambda (joint)
      (and (> (calculate-direction joint x) 0) (not (axis-stopped? joint x)))))

;;; PROCEDURE:
;;;   joint-up?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved up
;;; PRODUCED: 
;;;   boolean, whether joint moved up
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's y vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved up
(define joint-up? 
   (lambda (joint)
      (and (< (calculate-direction joint y) 0) (not (axis-stopped? joint y)))))

;;; PROCEDURE:
;;;   joint-down?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved down
;;; PRODUCED: 
;;;   boolean, whether joint moved down
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's y vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved down
(define joint-down? 
   (lambda (joint)
      (and (> (calculate-direction joint y) 0) (not (axis-stopped? joint y)))))

;;; PROCEDURE:
;;;   joint-forward?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved forward
;;; PRODUCED: 
;;;   boolean, whether joint moved forward
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's z vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved forward
(define joint-forward?
   (lambda (joint)
      (and (< (calculate-direction joint z) 0) (not (axis-stopped? joint z)))))

;;; PROCEDURE:
;;;   joint-backward?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint has moved backward
;;; PRODUCED: 
;;;   boolean, whether joint moved backward
;;; PRECONDITIONS:
;;;   positions have recently been stored in joint's z vector
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved backward
(define joint-backward?
   (lambda (joint)
      (and (> (calculate-direction joint z) 0) (not (axis-stopped? joint z)))))

;;; PROCEDURE:
;;;   movement-fast?
;;; PARAMETERS:
;;;   speed, an int 
;;; PURPOSE: 
;;;   checks the speed against the fast-threshold 
;;; PRODUCED: 
;;;   boolean, whether the speed is fast
;;; PRECONDITIONS:
;;;   fast-threshold is defined
;;; POSTCONDITIONS:
;;;   returns true if speed is greater than the fast threshold
(define movement-fast? 
   (lambda (speed)
      (>= (abs speed) fast-threshold)))

;;; PROCEDURE:
;;;   movement-slow?
;;; PARAMETERS:
;;;   speed, an int 
;;; PURPOSE: 
;;;   checks the speed against the slow-threshold 
;;; PRODUCED: 
;;;   boolean, whether the speed is slow
;;; PRECONDITIONS:
;;;   slow-threshold is defined
;;; POSTCONDITIONS:
;;;   returns true if speed is less than the slow threshold
(define movement-slow? 
   (lambda (speed)
      (< (abs speed) slow-threshold)))

;;; CONTROLS

(define start!
   (lambda ()
      (set! DONE #f)
      (set! left (make-joint))
      (track-gestures)))

(define stop! 
   (lambda ()
      (set! DONE #t)))

(define resume! 
   (lambda ()
      (set! DONE #f)
      (track-gestures)))

(stop!)
(resume!)
(start!)

;;; GESTURE HELPERS

(define track-gestures
   (lambda ()
      (when (not DONE)
            (check-gestures)
            (callback (+ (now) (* *second* .1)) 'track-gestures))))


(define simultaneous? 
   (lambda (joint axis1 axis2)
      (<= (abs (- (get-current-time joint axis1) (get-current-time joint axis2))) 100000)))

(define all-simultaneous? 
   (lambda (joint)
      (and (simultaneous? joint x y) (simultaneous? joint y z))))

;;; GESTURES AND HANDLERS


; left hand up
(define gesture-left-up
   (lambda ()
      (joint-up? left)))

;; (define left-up-handler print-up)

; left hand down
(define gesture-left-down
   (lambda ()
      (joint-down? left)))

;; (define left-down-handler print-down)

; left hand right
(define gesture-left-right
   (lambda ()
      (joint-right? left)))

;; (define left-right-handler print-right)

; left hand left
(define gesture-left-left
   (lambda ()
      (joint-left? left)))

;; (define left-left-handler print-left)

; left hand forward
(define gesture-left-forward
   (lambda ()
      (joint-forward? left)))

;; (define left-forward-handler print-forward)

; left hand backward
(define gesture-left-backward
   (lambda ()
      (joint-backward? left)))

;; (define left-backward-handler print-backward)

; left hand up and right
(define gesture-left-up-right
   (lambda ()
      (and (joint-up? left)
           (joint-right? left)
           (simultaneous? left x y))))
                                 
; left hand up and left
(define gesture-left-up-left
   (lambda ()
      (and (joint-up? left)
           (joint-left? left)
           (simultaneous? left x y))))

; left hand down and right
(define gesture-left-down-right
   (lambda ()
      (and (joint-down? left)
           (joint-right? left)
           (simultaneous? left x y))))

; left hand down and left
(define gesture-left-down-left
   (lambda ()
      (and (joint-down? left)
           (joint-left? left)
           (simultaneous? left x y))))

;;; HANDLERS

(define print-left
   (lambda ()
      (print 'left)))

(define print-right
   (lambda ()
      (print 'right)))

(define print-up
   (lambda ()
      (print 'up)))

(define print-down
   (lambda ()
      (print 'down)))

(define print-forward
   (lambda ()
      (print 'forward)))

(define print-backward
   (lambda () 
      (print 'backward)))

(define print-still
   (lambda ()
      (print 'still)))

(define alternate-print-left
   (lambda ()
      (print 'hand-is-moving-left)))

(define gesture-left-still
   (lambda ()
      (joint-still? left)))

;;; HANDLER HOLDERS

(define gesture-handlers
   (list (cons gesture-left-up print-up)
         (cons gesture-left-down print-down)
         (cons gesture-left-right print-right)
         (cons gesture-left-left print-left)))

(define get-gesture-handler 
   (lambda (gesture)
      (let ((p (assoc gesture gesture-handlers)))
         (if p
             (cdr p)
             (error "handler not found")))))

(define check-gesture
   (lambda (gesture-pair)
      (let ((gesture (car gesture-pair)))
         (if (gesture)
             (evaluate-gesture-handler gesture)))))

;delete-gesture! stub

(define add-gesture! 
   (lambda (gesture handler)
      (append gesture-handlers (cons gesture handler)))))
            
(define check-gestures
   (lambda ()
      (map check-gesture gesture-handlers)))

(define evaluate-gesture-handler
   (lambda (gesture)
      ((get-gesture-handler gesture))))

(define gesture-change-handler! 
   (lambda (gesture function)
      (let ((p (assoc gesture gesture-handlers)))
         (if p
             (set-cdr! p function)
             (error "handler not found")))))

(define combine-handlers
   (lambda (handler1 handler2)
      (handler1)
      (handler2)))