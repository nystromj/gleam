; take out repetitive code and replace with higher order procedures
; document and distinguish program and background functions
; make a wiki?

; what to do with the joint vectors (global vs. in assoc list)

; init and joint vectors
; does storage work?

; change two-handed gestures to work with oscstrings and check if both hands are active
; change oscstring to oscjoint?
; modify start and stop?

; should my map procedures be for-eaches? 
; fast and slow! 
; default gestures

; right now the hand that always has to be enabled is right hand. keep or change? 

;----------; GLOBALS ;----------;

; determines whether gestures should be detected. If true, no gestures are being recognized.
(define DONE #t)

; binds axes with index variables for referencing axis vectors
(define x 0)
(define y 1)
(define z 2)

;;; the amount a joint must moved before it is considered a movement
(define movement-threshold 0.05)

;; speed thresholds to determine if the joint is moving slowly or quickly
(define fast-threshold .15)
(define slow-threshold .01)


;----------; KINECT ;----------;

;;; PROCEDURE:
;;;   kinect-start!
;;; PARAMETERS:
;;;   port, the port that the kinect is sending message to
;;; PURPOSE: 
;;;   makes impromptu start listenin for oscmessages from the kinect
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   The kinect should be sending oscmessages to port
;;; POSTCONDITIONS:
;;;   Impromptu can now process osc signals if io:osc:receive is defined
(define kinect-start! 
   (lambda (port)
      (io:osc:start-server port))) 

;;; PROCEDURE:
;;;   io:osc:receive
;;; PARAMETERS:
;;;   timestamp, when the oscmessage is received
;;;   address, the address string of the oscmessage
;;;   args, a list of args defined by the oscmessage
;;; PURPOSE: 
;;;   handles incoming oscmessages and calls storage procedure when the user is user 1
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   the osc server must be started with io:osc:start-server, called in kinect-start!
;;;   the kinect user must be user 1
;;; POSTCONDITIONS:
;;;   calls oscdata-process, defined later
(define io:osc:receive 
   (lambda (timestamp address . args)
   (if (equal? (list-ref args 1) 1)
       (oscdata-process args))))


;----------; JOINTS ;----------;


; list of enabled joints -> left hand and right hand are default
(define joints
   (list (cons "l_hand" (make-joint))
         (cons "r_hand" (make-joint))))

;;; PROCEDURE:
;;;   make-joint
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   creates a joint vector
;;; PRODUCED: 
;;;   the new joint vector
;;; PRECONDITIONS:
;;;   NULL 
;;; POSTCONDITIONS:
;;;   the created joint vector contains 3 axis vectors, one for each x y and z
(define make-joint
   (lambda ()
      (vector (make-axis)        ;; x
              (make-axis)        ;; y
              (make-axis))))     ;; z

;;; PROCEDURE:
;;;   make-axis
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   creates a vector that stores the values for an axis
;;; PRODUCED: 
;;;   the new axis vector
;;; PRECONDITIONS:
;;;   NULL 
;;; POSTCONDITIONS:
;;;   the created vector has 3 elements: a vector that stores the positions of that axis, 
;;;   a counter variable to reference that vector, and a variable that keeps track of direction
(define make-axis
   (lambda () 
      (vector 0
              0
              (make-vector 100 (cons 0 0)))))

;;; PROCEDURE:
;;;   joint-add! 
;;; PARAMETERS:
;;;   oscjoint, an osc string for a joint
;;; PURPOSE: 
;;;   sets up the joint so gestures are now recognized
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscjoint is not already enabled
;;; POSTCONDITIONS:
;;;   the joint is now enabled and gestures can be recognized. 
;;;   all gesture handlers for joint are automatically 'NULL
(define joint-add! 
   (lambda (oscjoint)
      (if (enabled? oscjoint)
          (error "joint has already been enabled")
          (begin (list-prepend! joints (cons oscjoint (make-joint)))
                 (joint-add-to-gestures! oscjoint)))))

;;; PROCEDURE:
;;;   joint-add-to-gestures!
;;; PARAMETERS:
;;;   oscjoint, the osc string for a joint
;;; PURPOSE: 
;;;   adds the joint to the handler list for each gesture
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should be called only by joint-add! 
;;; POSTCONDITIONS:
;;;   each gesture now has a 'NULL handler for oscjoint
(define joint-add-to-gestures! 
   (lambda (oscjoint)
      (map (lambda (gesture-pair)
              (list-prepend! (cdr gesture-pair) (cons oscjoint 'NULL)))
           gestures)))

;;; PROCEDURE:
;;;   joint-remove!
;;; PARAMETERS:
;;;   oscjoint, the oscstring for a joint
;;; PURPOSE: 
;;;   removes the joint from the joints list and erases all handlers
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscjoint must be enabled
;;;   oscjoint cannot be the last joint in the joints list
;;; POSTCONDITIONS:
;;;   oscjoint is no longer enabled and its gesture handlers have been removed from
;;;   each gesture's handler list
(define joint-remove!
   (lambda (oscjoint)
      (if (cond ((not (enabled? oscjoint))
                 (error "joint is not currently enabled"))
                ((equal? (list-index joints oscjoint) (- (length joints) 1)))
                 (error "must have at least one joint activated"))
                (else (list-delete! joints (assoc oscjoint joints))
                      (joint-remove-from-gestures! oscjoint))))))

;;; PROCEDURE:
;;;   joint-remove-from-gestures!
;;; PARAMETERS:
;;;   oscjoint, the oscstring for a joint
;;; PURPOSE: 
;;;   removes the joint from the handler list for each gesture
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should only be called by joint-removed!
;;; POSTCONDITIONS:
;;;   the handler for the oscjoint has been removed from each gesture
(define joint-remove-from-gestures! 
   (lambda (oscjoint)
      (map (lambda (gesture-pair)
              (list-delete! (cdr gesture-pair) (assoc oscjoint (cdr gesture-pair))))
           gestures)))

;;; PROCEDURE:
;;;   enabled?
;;; PARAMETERS:
;;;   oscjoint, the oscstring for a joint
;;; PURPOSE: 
;;;   determines whether the joint is enabled
;;; PRODUCED: 
;;;   boolean, whether the joint is being recognized by the kinect
;;; PRECONDITIONS:
;;;   NULL 
;;; POSTCONDITIONS:
;;;   returns true if oscjoint is a memeber of joints list
(define enabled? 
   (lambda (oscjoint)
      (let ((joint (assoc oscjoint joints)))
         (if joint 
             #t
             #f))))

;;; PROCEDURE:
;;;   oscstring->joint
;;; PARAMETERS:
;;;   oscstring, the oscstring for a joint
;;; PURPOSE: 
;;;   looks up oscstring in joints list and returns corresponding joint vector
;;; PRODUCED: 
;;;   vec, the joint vector for oscjoint
;;; PRECONDITIONS:
;;;   oscstring joint should be enabled 
;;; POSTCONDITIONS:
;;;   returns an error if the joint is not in the list
(define oscstring->joint
   (lambda (oscstring)
      (let ((joint-pair (assoc oscstring joints)))
         (if joint-pair
             (cdr joint-pair)
             (error "osc string not found")))))


;----------; PROCESSING ;----------;

;;; PROCEDURE:
;;;   oscdata-process
;;; PARAMETERS:
;;;   oscdata, the list of arguments bundled in an oscmessage
;;; PURPOSE: 
;;;   checks whether joint is enabled and sends information to process-joint-coordinates
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscdata is a list of arguments of the form "sifff" 
;;; POSTCONDITIONS:
;;;   if the oscjoint string bundled in oscdata matches a joint in the enabled joints list, 
;;;   process-joint-coordinates is called with the remaining arguments
(define oscdata-process
   (lambda (oscdata)
      (let ((joint (assoc (car oscdata) joints)))
         (if joint
             (process-joint-coordinates (cdr joint) (cddr oscdata))))))

;;; PROCEDURE:
;;;   process-joint-coordinates
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   coords, the list of coordinates for the current x y and z positions of the joint
;;; PURPOSE: 
;;;   calls process-axis-coordinates for each axis of the joint
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   only called by oscdata-process
;;;   coords is a list of three floats that correspond to x y and z
;;;   joint is a joint vector that matches an oscjoint in joints
;;; POSTCONDITIONS:
;;;   calls process-axis-coordinates
(define process-joint-coordinates
   (lambda (joint coords)
      (for-each (lambda (axis coord)
              (process-axis-coordinates joint axis coord))
                (list x y z)
                coords)))

;;; PROCEDURE:
;;;  process-axis-coordinates
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, the x y or z vector
;;;   coord, the position the joint is currently at
;;; PURPOSE: 
;;;   determines whether to store the new coordinate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called only by process-joint-coordinates
;;; POSTCONDITIONS:
;;;   if the joint has moved along the give axis, it stores the new position
;;;   if the joint has not moved, checks to see if the axis can now be considered "still"
(define process-axis-coordinates
   (lambda (joint axis coord)
      (if (movement? joint axis coord)
          (store-pos! joint axis coord)
          (check-axis-still! joint axis))))
     
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


;----------; STORAGE ;----------;

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
;;;   sets a new direction for the axis if direction has changed
(define store-pos! 
   (lambda (joint axis new-data)
      (increment-counter! joint axis)      
      (vector-set! (vector-ref (vector-ref joint axis) 2)
                   (get-counter joint axis) 
                   (cons (* 100 new-data) (now)))
      (if (direction-changed? joint axis)
          (set-direction! joint axis (calculate-direction joint axis)))))

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
;;;   called only by store-pos!
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
;;;   called only by increment-counter!
;;;   counter should be between 0 and 99
;;; POSTCONDITIONS:
;;;   the vector counter is now set to counter
(define set-counter! 
   (lambda (joint axis counter)
      (vector-set! (vector-ref joint axis) 0 counter)))

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
;;;   called only by store-pos!
;;; POSTCONDITIONS:
;;;   direction value is now stored in the axis vector to be obtained with get-direction
(define set-direction! 
   (lambda (joint axis direction)
      (vector-set! (vector-ref joint axis) 1 direction)))


;----------; INTERPRETING DATA ;----------;

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
;;;   called only by direction-changed?
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


;----------; JOINT BEHAVIOR ;----------;

(define joint-still? 
   (lambda (joint)
      (and (axis-still? joint x)
           (axis-still? joint y)
           (axis-still? joint z))))

(define axis-still? 
   (lambda (joint axis)
      (equal? (get-direction joint axis) 0)))

(define check-axis-still!
   (lambda (joint axis)
      (if (and (axis-stopped? joint axis) (not (axis-still? joint axis)))
          (set-direction! joint axis 0))))

(define axis-stopped? 
   (lambda (joint axis)
      (>= (- (now) (get-current-time joint axis)) (* *second* 2))))

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


;----------; GESTURES ;----------;

(define gesture-joint-still
   (lambda (joint)
      (joint-still? joint)))

(define gesture-joint-right
   (lambda (joint)
      (joint-right? joint)))

(define gesture-joint-left
   (lambda (joint)
      (joint-left? joint)))

(define gesture-joint-up
   (lambda (joint)
      (joint-up? joint)))

(define gesture-joint-down
   (lambda (joint)
      (joint-down? joint)))

(define gesture-joint-forward
   (lambda (joint)
      (joint-forward? joint)))

(define gesture-joint-backward
   (lambda (joint)
      (joint-backward? joint)))

(define gesture-joint-right-up
   (lambda (joint)
      (and (joint-up? joint)
           (joint-right? joint)
           (simultaneous? joint x joint y))))

(define gesture-joint-right-down
   (lambda (joint)
      (and (joint-down? joint)
           (joint-right? joint)
           (simultaneous? joint x joint y))))

(define gesture-joint-right-forward
   (lambda (joint)
      (and (joint-forward? joint)
           (joint-right? joint)
           (simultaneous? joint x joint z))))

(define gesture-joint-right-backward
   (lambda (joint)
      (and (joint-backward? joint)
           (joint-right? joint)
           (simultaneous? joint x joint z))))

(define gesture-joint-left-up
   (lambda (joint)
      (and (joint-up? joint)
           (joint-left? joint)
           (simultaneous? joint x joint y))))

(define gesture-joint-left-down
   (lambda (joint)
      (and (joint-down? joint)
           (joint-left? joint)
           (simultaenous? joint x joint y))))

(define gesture-joint-left-forward
   (lambda (joint)
      (and (joint-forward? joint)
           (joint-left? joint)
           (simultaneous? joint x joint z))))

(define gesture-joint-left-backward
   (lambda (joint)
      (and (joint-backward? joint)
           (joint-left? joint)
           (simultaneous? joint x joint z))))

(define gesture-joint-up-forward
   (lambda (joint)
      (and (joint-forward? joint)
           (joint-up? joint)
           (simultaneous? joint y joint z))))

(define gesture-joint-up-backward
   (lambda (joint)
      (and (joint-backward? joint)
           (joint-up? joint)
           (simultaneous? joint y joint z))))

(define gesture-joint-down-forward 
   (lambda (joint)
      (and (joint-forward? joint)
           (joint-down? joint)
           (simultaneous? joint y joint z))))

(define gesture-joint-down-backward
   (lambda (joint)
      (and (joint-backward? joint)
           (joint-down? joint)
           (simultaneous? joint y joint z))))

(define gesture-joint-right-up-forward
   (lambda (joint)
      (and (gesture-joint-right-up joint)
           (gesture-joint-right-forward joint))))

(define gesture-joint-right-up-backward
   (lambda (joint)
      (and (gesture-joint-right-up joint)
           (gesture-joint-right-backward joint))))

(define gesture-joint-right-down-forward
   (lambda (joint)
      (and (gesture-joint-right-down joint)
           (gesture-joint-right-forward joint))))

(define gesture-joint-right-down-backward 
   (lambda (joint)
      (and (gesture-joint-right-down joint)
           (gesture-joint-right-backward joint))))

(define gesture-joint-left-up-forward
   (lambda (joint)
      (and (gesture-joint-left-up joint)
           (gesture-joint-left-forward joint))))

(define gesture-joint-left-up-backward
   (lambda (joint)
      (and (gesture-joint-left-up joint)
           (gesture-joint-left-backward joint))))

(define gesture-joint-left-down-forward
   (lambda (joint)
      (and (gesture-joint-left-down joint)
           (gesture-joint-left-forward joint))))

(define gesture-joint-left-down-backward
   (lambda (joint)
      (and (gesture-joint-left-down joint)
           (gesture-joint-left-backward joint))))

(define gesture-both-hands-right
   (lambda ()
      (and (gesture-joint-right left)
           (gesture-joint-right right)
           (simultaneous? left x right x))))

(define gesture-both-hands-left
   (lambda ()
      (and (gesture-joint-left left)
           (gesture-joint-left right)
           (simultaneous? left x right x))))

(define gesture-both-hands-in
   (lambda ()
      (and (gesture-joint-right left)
           (gesture-joint-left right)
           (simultaneous? left x right x))))

(define gesture-both-hands-away
   (lambda ()
      (and (gesture-joint-right right)
           (gesture-joint-left left)
           (simultaneous? left x right x))))

(define gesture-both-hands-up
   (lambda ()
      (and (gesture-joint-up left)
           (gesture-joint-up right)
           (simultaneous? left y right y)))) 

(define gesture-both-hands-down
   (lambda ()
      (and (gesture-joint-down left)
           (gesture-joint-down right)
           (simultaneous? left y right y))))

(define gesture-right-hand-up-left-hand-down
   (lambda ()
      (and (gesture-joint-up right)
           (gesture-joint-down left)
           (simultaneous? left y right y))))      

(define gesture-right-hand-down-left-hand-up
   (lambda ()
      (and (gesture-joint-up left)
           (gesture-joint-down left)
           (simultaneous? left y right y))))

(define gesture-both-hands-forward
   (lambda ()
      (and (gesture-joint-forward left)
           (gesture-joint-forward right)
           (simultaneous? left z right z))))

(define gesture-both-hands-backward
   (lambda ()
      (and (gesture-joint-backward left)
           (gesture-joint-backward right)
           (simultaneous? left z right z))))

(define gesture-both-hands-up-in
   (lambda ()
      (and (gesture-joint-right-up left)
           (gesture-joint-left-up right)
           (simultaneous? left y right y))))

(define gesture-both-hands-down-out
   (lambda ()
      (and (gesture-joint-left-down left)
           (gesture-joint-right-down right)
           (simultaneous? left y right y))))

(define gesture-both-hands-up-out
   (lambda ()
      (and (gesture-joint-right-up right)
           (gesture-joint-left-up left)
           (simultaneous? left y right y))))

(define gesture-both-hands-down-in 
   (lambda ()
      (and (gesture-joint-right-down left)
           (gesture-joint-left-down right)
           (simultaneous? left y right y))))

(define gesture-both-hands-still 
   (lambda ()
      (and (gesture-joint-still left)
           (gesture-joint-still right))))

(define simultaneous? 
   (lambda (joint1 axis1 joint2 axis2)
      (<= (abs (- (get-current-time joint1 axis1) (get-current-time joint2 axis2))) 100000)))


;----------; GESTURE SET UP ;----------;

(define two-handed-gestures
   (list (cons gesture-both-hands-right 'NULL)
         (cons gesture-both-hands-left 'NULL)
         (cons gesture-both-hands-in 'NULL)
         (cons gesture-both-hands-away 'NULL)
         (cons gesture-both-hands-up 'NULL)
         (cons gesture-both-hands-down 'NULL)
         (cons gesture-right-hand-up-left-hand-down 'NULL)
         (cons gesture-right-hand-down-left-hand-up 'NULL)
         (cons gesture-both-hands-forward 'NULL)
         (cons gesture-both-hands-backward 'NULL)
         (cons gesture-both-hands-up-in 'NULL)
         (cons gesture-both-hands-down-out 'NULL)
         (cons gesture-both-hands-up-out 'NULL)
         (cons gesture-both-hands-down-in 'NULL)
         (cons gesture-both-hands-still 'NULL)))

(define two-handed?
   (lambda (gesture) 
      (let ((member (assoc gesture two-handed-gestures)))
       (if member
           #t
           #f))))

(define simple-gestures
   (list gesture-joint-up
      gesture-joint-down
      gesture-joint-right
      gesture-joint-left
      gesture-joint-forward
      gesture-joint-backward
      gesture-joint-still))

(define simple? 
   (lambda (gesture)
         (if (list-index simple-gestures gesture)
             #t
             #f)))

(define multidirectional-gestures 
   (list gesture-joint-right-up
         gesture-joint-right-down
         gesture-joint-right-forward
         gesture-joint-right-backward
         gesture-joint-left-up
         gesture-joint-left-down
         gesture-joint-left-forward
         gesture-joint-left-backward
         gesture-joint-up-forward
         gesture-joint-up-backward
         gesture-joint-down-forward
         gesture-joint-down-backward
         gesture-joint-right-up-forward
         gesture-joint-right-up-backward
         gesture-joint-right-down-forward
         gesture-joint-right-down-backward
         gesture-joint-left-up-forward
         gesture-joint-left-up-backward
         gesture-joint-left-down-forward
         gesture-joint-left-down-backward))

(define multidirectional? 
   (lambda (gesture)
         (if (list-index multidirectional-gestures gesture)
             #t
             #f)))

(define gestures (map (lambda (elt)
                         (cons elt (list (cons "l_hand" 'NULL)
                                         (cons "r_hand" 'NULL))))
                      (append multidirectional-gestures simple-gestures)))
         
;;; TEST HANDLERS

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

; get change-handlers to work
; make sure evaluate-all-gestures works!
; gesture-evaluate-list adds to ignored for no reason! 

;----------; GESTURE TRACKING ;----------;


(define ignored '(NULL))

(define ignored? 
   (lambda (oscstring)
      (if (list-index ignored oscstring)
          #t
          #f)))

(define track-gestures
   (lambda ()
      (when (not DONE)
            (gesture-evaluate-list)
            (callback (+ (now) (* *second* .1)) 'track-gestures))))

; adds joints to ignored whne it shouldn't
(define gesture-evaluate-list
   (lambda ()
      (set! ignored '(NULL))
      (if (and (enabled? "l_hand") (enabled? "r_hand"))
          (gesture-evaluate-two-handed-gestures two-handed-gestures))
      (for-each gesture-evaluate-pair gestures)))

; tested
(define gesture-evaluate-pair
   (lambda (gesture-pair)
      (map (lambda (handler-pair)
              (gesture-evaluate-joint (car gesture-pair) (car handler-pair) (cdr handler-pair)))
           (cdr gesture-pair))))

;tested
(define gesture-evaluate-joint
   (lambda (gesture joint handler)
      (if (and (not (ignored? joint)) (not (equal? handler 'NULL)))
          (if (gesture (oscstring->joint joint))
              (gesture-evaluate-handler gesture joint handler)))))

;tested
(define gesture-evaluate-handler 
   (lambda (gesture joint handler)      
      (handler)
      (set! counter (+ counter 1))
      (if (multidirectional? gesture)
          (list-prepend! ignored joint))))

; tested
(define gesture-evaluate-two-handed-gestures
   (lambda (lst)
      (cond ((null? lst)
             #f)
            ((gesture-evaluate-two-handed-gesture-pair (car lst)))
            (else (gesture-evaluate-two-handed-gestures (cdr lst))))))

; tested
(define gesture-evaluate-two-handed-gesture-pair
   (lambda (gesture-pair)
      (cond ((and ((car gesture-pair)) (not (equal? (cdr gesture-pair) 'NULL)))
             ((cdr gesture-pair))
             (list-prepend! ignored "l_hand")
             (list-prepend! ignored "r_hand")
             (set! counter (+ counter 1))
             #t)
            (else #f))))

;;; TESTS ;;;

(define counter 0)

(gesture-evaluate-two-handed-gesture-pair (assoc gesture-both-hands-still two-handed-gestures))

(gesture-evaluate-two-handed-gestures two-handed-gestures)
(print counter)

(gesture-evaluate-two-handed-gestures two-handed-gestures)

(gesture-evaluate-list)

(print ignored)

(for-each gesture-evaluate-pair gestures)

(gesture-disable! gesture-both-hands-still)


;----------; HANDLER MANAGEMENT ;---------;

; works for two-handed-gestures but not for simple gestures! 
(define gesture-change-handler! 
   (lambda (gesture function . oscstring)
      (if (null? oscstring)
          (gesture-two-handed-change-handler! gesture function)
          (gesture-simple-change-handler! gesture function (car oscstring)))))

; seems to work, but not within call to gesture-change-handler!
(define gesture-simple-change-handler! 
   (lambda (gesture function oscstring)
      (let ((pair (assoc gesture gestures)))
         (if (and pair (enabled? oscstring))
             (set-cdr! (assoc oscstring (cdr pair)) function)
             (error "gesture not found")))))

; tested
(define gesture-two-handed-change-handler! 
   (lambda (gesture function)
      (let ((pair (assoc gesture two-handed-gestures)))
         (if pair 
             (set-cdr! pair function)
             (error "gesture not found")))))

; not working for simple gestures even though gesture-change-handler! seems to work
(define gesture-disable! 
   (lambda (gesture . oscstring)
      (if (null? oscstring)
          (gesture-change-handler! gesture 'NULL)
          (gesture-change-handler! gesture 'NULL (car oscstring)))))


;----------; PROGRAMMER CONTROLS ;----------;

(define movement-threshold-set! 
   (lambda (new-threshold)
      (set! movement-threshold new-threshold)))

(define fast-threshold-set! 
   (lambda (new-threshold)
      (set! fast-threshold new-threshold)))

(define slow-threshold-set! 
   (lambda (new-threshold)
      (set! slow-threshold new-threshold)))

(define start!
   (lambda ()
      (set! DONE #f)
      (track-gestures)))

(define stop! 
   (lambda ()
      (set! DONE #t)))

;----------; LIST HELPERS ;----------;

(define list-append!
  (lambda (lst val)
    (if (null? (cdr lst))
        (set-cdr! lst (cons val '()))
        (list-append! (cdr lst) val))
    lst))                             

(define get-index 
  (lambda (lst val current)
      (cond ((equal? 0 (length lst))
             #f)
            ((equal? (car lst) val)
             current)
            (else
             (get-index (cdr lst) val (+ current 1))))))
             
(define list-index 
  (lambda (lst val)
    (get-index lst val 0)))

(define list-delete!
  (lambda (lst val)
    (cond
      ; Can't delete the last thing in a list
      ((null? (cdr lst)))
      ; Delete the first thing in the list
      ((equal? val (car lst))
       (set-car! lst (cadr lst))
       (set-cdr! lst (cddr lst)))
      ; Everything else
      (else
       (list-delete! (cdr lst) val)))
    lst))

(define list-prepend!
  (lambda (lst val)
    ; Build a new cons cell for the old list
    (let ((newtail (cons (car lst) (cdr lst))))
      ; Shove the new value at the front
      (set-car! lst val)
      ; Shove the new cons cell at the back
      (set-cdr! lst newtail)
      ; And we're done
      lst)))