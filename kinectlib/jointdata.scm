;----------; GLOBALS ;----------;

;;; determines whether gestures should be detected
(define DONE #t)

;;; binds axes with index variables for referencing axis vectors
(define x 0)
(define y 1)
(define z 2)

;;; determines the user whose gestures are being evaluated
(define user-id 1)

;;; the amount a joint must move before it is considered a movement
(define movement-threshold 0.05)

;;; speed thresholds to determine if the joint is moving slowly or quickly
(define fast-threshold .15)
(define slow-threshold .01)

;;; the amount of time that must pass before joint is registered as stop
(define stop-threshold (* *second* 2))

;;; the max amount of time that can occur between movements before they are considered 
;;; out of sync. 
(define simultaneous-threshold 100000)

;;; how often track-gestures is called
(define refresh-rate (* *second* .1))


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
   (if (and (equal? (list-ref args 1) user-id) (not DONE))
       (oscdata-process args))))


;----------; JOINTS ;----------;

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

;;; list of enabled joints -> left hand and right hand are default
(define joints
   (list (cons "l_hand" (make-joint))
         (cons "r_hand" (make-joint))))

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
      (for-each (lambda (gesture-pair)
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
      (for-each (lambda (gesture-pair)
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
      (car (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (get-counter joint axis)))))

;;; PROCEDURE:
;;;   get-previous-pos
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
;;;   get-previous-counter
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
;;;   get-previous-time
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

;;; PROCEDURE:
;;;   joint-still? 
;;; PARAMETERS:
;;;   joint, the joint vector
;;; PURPOSE: 
;;;   determines whether the joint has stopped making registered movements
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored recently for the joint's axes
(define joint-still? 
   (lambda (joint)
      (and (axis-still? joint x)
           (axis-still? joint y)
           (axis-still? joint z))))

;;; PROCEDURE:
;;;   axis-still? 
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to the axis vector of the joint
;;; PURPOSE: 
;;;   determines whether the joint is moving along the given axis
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored recently for that axis
;;;   and the direction of the joint is zero.
(define axis-still? 
   (lambda (joint axis)
      (equal? (get-direction joint axis) 0)))

;;; PROCEDURE:
;;;   check-axis-still! 
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to the axis vector for the joint
;;; PURPOSE: 
;;;   checks to see if the joint has stopped moving along the axis
;;;   changes the joint's direction if appropriate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   the direction of the joint is not already zero
;;; POSTCONDITIONS:
;;;   if the joint has stopped moving along the axis, direction for that axis is zero
(define check-axis-still!
   (lambda (joint axis)
      (if (and (axis-stopped? joint axis) (not (axis-still? joint axis)))
          (set-direction! joint axis 0))))

;;; PROCEDURE:
;;;   axis-stopeed? 
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to the axis vector for the joint
;;; PURPOSE: 
;;;   determines whether the joint axis has stored any new positions recently
;;; PRODUCED: 
;;;   boolean, whether positions have been stored recently
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored recently for the joint's axes
(define axis-stopped? 
   (lambda (joint axis)
      (>= (- (now) (get-current-time joint axis)) stop-threshold)))

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
           (simultaneous? joint x joint y))))

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
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right left)
              (gesture-joint-right right)
              (simultaneous? left x right x)))))

(define gesture-both-hands-left
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left left)
              (gesture-joint-left right)
              (simultaneous? left x right x)))))

(define gesture-both-hands-in
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right left)
              (gesture-joint-left right)
              (simultaneous? left x right x)))))

(define gesture-both-hands-away
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right right)
              (gesture-joint-left left)
              (simultaneous? left x right x)))))

(define gesture-both-hands-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up left)
              (gesture-joint-up right)
              (simultaneous? left y right y)))))

(define gesture-both-hands-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-down left)
              (gesture-joint-down right)
              (simultaneous? left y right y)))))

(define gesture-right-hand-up-left-hand-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up right)
              (gesture-joint-down left)
              (simultaneous? left y right y)))))      

(define gesture-right-hand-down-left-hand-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up left)
              (gesture-joint-down left)
              (simultaneous? left y right y)))))

(define gesture-both-hands-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-forward left)
              (gesture-joint-forward right)
              (simultaneous? left z right z)))))

(define gesture-both-hands-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-backward left)
              (gesture-joint-backward right)
              (simultaneous? left z right z)))))

(define gesture-both-hands-up-in
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-up left)
              (gesture-joint-left-up right)
              (simultaneous? left y right y)))))

(define gesture-both-hands-down-out
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-down left)
              (gesture-joint-right-down right)
              (simultaneous? left y right y)))))

(define gesture-both-hands-up-out
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-up right)
              (gesture-joint-left-up left)
              (simultaneous? left y right y)))))

(define gesture-both-hands-down-in 
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-down left)
              (gesture-joint-left-down right)
              (simultaneous? left y right y)))))

(define gesture-both-hands-still 
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-still left)
              (gesture-joint-still right)))))

;;; PROCEDURE:
;;;   simultaneous? 
;;; PARAMETERS:
;;;   joint1, a joint vector
;;;   axis1, an index to an axis vector of joint1
;;;   joint2, a joint vector
;;;   axis1, an index to an axis vector of joint2
;;; PURPOSE: 
;;;   determines whether the two axis of the joints are moving at the same time
;;; PRODUCED: 
;;;   boolean, whether the movements of the axes are simultaneous
;;; PRECONDITIONS:
;;;   simultaneous-threshold is the maximum amount of time that can occur between
;;;   movements before they are considered out of sync
;;; POSTCONDITIONS:
;;;   returns true if the difference between the times of the last movements of 
;;;   each of the joints is less than simultaneous-threshold
(define simultaneous? 
   (lambda (joint1 axis1 joint2 axis2)
      (<= (abs (- (get-current-time joint1 axis1) 
                  (get-current-time joint2 axis2)))
          simultaneous-threshold)))


;----------; GESTURE SET UP ;----------;

;;; two-handed gestures do not take joints as parameters and have only one handler
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

(gesture-evaluate-two-handed-gesture-pair (assoc gesture-right-hand-down-left-hand-up two-handed-gestures))
(two-handed? gesture-both-hands-right)

;;; PROCEDURE:
;;;   two-handed? 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;; PURPOSE: 
;;;   determines whether the gesture is two-handed
;;; PRODUCED: 
;;;   boolean, whether the gesture is two-handed
;;; PRECONDITIONS:
;;;   gesture is a predefined function
;;; POSTCONDITIONS:
;;;   returns true if gesture is a member of two-handed-gestures list
(define two-handed?
   (lambda (gesture) 
      (let ((member (assoc gesture two-handed-gestures)))
       (if member
           #t
           #f))))

;;; simple gestures take a joint as a parameter and only deal with one axis
(define simple-gestures
   (list gesture-joint-up
      gesture-joint-down
      gesture-joint-right
      gesture-joint-left
      gesture-joint-forward
      gesture-joint-backward
      gesture-joint-still))

;;; PROCEDURE:
;;;   simple? 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;; PURPOSE: 
;;;   determines whether the gesture function is a simple gesture
;;; PRODUCED: 
;;;   boolean, whether the gesture is simple
;;; PRECONDITIONS:
;;;   gesture is a predefined function
;;; POSTCONDITIONS:
;;;   returns true if gesture is a member of simple-gestures list
(define simple? 
   (lambda (gesture)
         (if (list-index simple-gestures gesture)
             #t
             #f)))

;;; multidirectional gestures take a joint as a parameter and determine if there
;;; is movement in multiple directions
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

;;; PROCEDURE:
;;;   multidirectional? 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;; PURPOSE: 
;;;   determines whether the gesture function is a multidirectional gesture
;;; PRODUCED: 
;;;   boolean, whether the gesture is multidirecitonal
;;; PRECONDITIONS:
;;;   gesture is a predefined function
;;; POSTCONDITIONS:
;;;   returns true if gesture is a member of multidirectional-gestures list
(define multidirectional? 
   (lambda (gesture)
         (if (list-index multidirectional-gestures gesture)
             #t
             #f)))

;;; appends the list of joint gestures together and adds default handlers
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


;----------; GESTURE TRACKING ;----------;

;;; list of joints that should be ignored
(define ignored '(NULL))

;;; PROCEDURE:
;;;   ignored? 
;;; PARAMETERS:
;;;   oscjoint, the osc string for the joint
;;; PURPOSE: 
;;;   determines whether the joint is being ignored
;;; PRODUCED: 
;;;   boolean, whether the joint is ignored
;;; PRECONDITIONS:
;;;   should only be called by handler-evaluating functions
;;;   a joint is set ignored by handler-evaluating functions
;;; POSTCONDITIONS:
;;;   returns true if oscjoint is a member of ignored list
(define ignored? 
   (lambda (oscjoint)
      (if (list-index ignored oscjoint)
          #t
          #f)))

;;; PROCEDURE:
;;;   track-gestures 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   runs a loop that checks for gestures
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called by start!, not explicity by programmer
;;; POSTCONDITIONS:
;;;   NULL
(define track-gestures
   (lambda ()
      (when (not DONE)
            (gesture-evaluate-list)
            (callback (+ (now) refresh-rate) 'track-gestures))))

;;; PROCEDURE:
;;;   gesture-evaluate-list 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   checks the gestures in the gesture list once
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if both hands are enabled it evaluates two-handed gestures before evaluating
;;;   the gestures list
(define gesture-evaluate-list
   (lambda ()
      (set! ignored (list 'NULL))
      (if (and (enabled? "l_hand") (enabled? "r_hand"))
          (gesture-evaluate-two-handed-gestures two-handed-gestures))
      (for-each gesture-evaluate-pair gestures)))

(gesture-change-handler! gesture-both-hands-still print-down)
(gesture-evaluate-two-handed-gestures two-handed-gestures)
(print ignored)
(gesture-evaluate-list)
(gestures-stop!)
(gestures-start!)

;;; PROCEDURE:
;;;   gesture-evaluate-pair 
;;; PARAMETERS:
;;;   gesture-pair, a cons cell
;;; PURPOSE: 
;;;   calls gesture-evaluate-joint for each joint in the handler list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   gesture-pair is a cons cell whose car is a gesture function and whose cdr 
;;;   is the handler list for that gesture
;;; POSTCONDITIONS:
;;;   gesture-evaluate-joint is called on every element in the handler list
(define gesture-evaluate-pair
   (lambda (gesture-pair)
      (map (lambda (handler-pair)
              (gesture-evaluate-joint (car gesture-pair) 
                                      (car handler-pair) 
                                      (cdr handler-pair)))
           (cdr gesture-pair))))

;;; PROCEDURE:
;;;   gesture-evaluate-joint 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;;   joint, a joint vector
;;;   handler, a handler function
;;; PURPOSE: 
;;;   determines if the gesture is true for the joint and calls handler processing
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should be called only by gesture-evaluate-pair
;;;   in order for handler to evaluate, joint should not be ignored and the handler
;;;   should not be null.
;;; POSTCONDITIONS:
;;;   if the preconditions are met and the gesture is true for the joint, calls
;;;   gesture-evaluate-handler
(define gesture-evaluate-joint
   (lambda (gesture joint handler)
      (if (and (not (ignored? joint)) (not (equal? handler 'NULL)))
          (if (gesture (oscstring->joint joint))
              (gesture-evaluate-handler gesture joint handler)))))

;;; PROCEDURE:
;;;   gesture-evaluate-handler 
;;; PARAMETERS:
;;;   gesture, a gseture function
;;;   joint, a joint vecor
;;;   handler, a handler function
;;; PURPOSE: 
;;;   evaluates the handler function then sets the joint to ignored if 
;;;   gesture if multi-directional
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should only be called by gesture-evaluate-joint
;;; POSTCONDITIONS:
;;;   if gesture was multidirectional, joint is now ignored
(define gesture-evaluate-handler 
   (lambda (gesture joint handler)      
      (handler)
      (if (multidirectional? gesture)
          (list-prepend! ignored joint))))

;;; PROCEDURE:
;;;   gesture-evaluate-two-handed-gestures 
;;; PARAMETERS:
;;;   lst, a list of cons cells
;;; PURPOSE: 
;;;   calls gesture-evaluate-two-handed-gesture pair for each element in list
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   should only be called by gesture-evaluate-list
;;;   each cons cell in list has a two-handed-gesture function as its 
;;;   car and a handler as its cdr.
;;; POSTCONDITIONS:
;;;   stops executing after a two-handed gesture in lst returns true
(define gesture-evaluate-two-handed-gestures
   (lambda (lst)
      (cond ((null? lst)
             #f)
            ((gesture-evaluate-two-handed-gesture-pair (car lst)))
            (else (gesture-evaluate-two-handed-gestures (cdr lst))))))

;;; PROCEDURE:
;;;   gesture-evaluate-two-handed-gesture-pair 
;;; PARAMETERS:
;;;   gesture-pair, a cons cell
;;; PURPOSE: 
;;;   determines if gesture is true for joint and executes handler
;;; PRODUCED: 
;;;   boolean, whether the handler was executed
;;; PRECONDITIONS:
;;;   gesture-pair is a cons cell whose car is a two-handed gesture 
;;;   function and whose cdr is the handler for that gesture
;;;   handler must not be 'NULL
;;; POSTCONDITIONS:
;;;   if the gesture test returned true and handler was not 'NULL, handler 
;;;   was executed and both hands are added to the ignored list.
(define gesture-evaluate-two-handed-gesture-pair
   (lambda (gesture-pair)
      (cond ((and ((car gesture-pair)) 
                  (not (equal? (cdr gesture-pair) 'NULL)))
             ((cdr gesture-pair))
             (list-prepend! ignored "l_hand")
             (list-prepend! ignored "r_hand")
             #t)
            (else #f))))

(define fun-list (list print-up print-forward print-still))
((list-ref fun-list 1))

(map gesture-evaluate-two-handed-gesture-pair
     two-handed-gestures)

(gesture-evaluate-list)
((car (cons gesture-both-hands-still print-still)))
(gesture-disable! gesture-both-hands-still)
(print (assoc gesture-both-hands-still two-handed-gestures))
(gesture-change-handler! gesture-both-hands-still print-still)
(gesture-evaluate-two-handed-gesture-pair (assoc gesture-both-hands-still two-handed-gestures))
(print ignored)
(gesture-evaluate-two-handed-gestures two-handed-gestures)
(for-each gesture-evaluate-pair gestures)
(gesture-two-handed-change-handler! gesture-both-hands-still print-forward)
(gestures-stop!)

;----------; HANDLER MANAGEMENT ;---------;

;;; PROCEDURE:
;;;   gesture-change-handler!
;;; PARAMETERS:
;;;   gesture, a gesture function
;;;   function, the new handler
;;;   [oscjoint], optional osc string for joint
;;; PURPOSE: 
;;;   changes the handler for the given gesture
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscjoint is NULL if the gesture is a two-handed gesture
;;; POSTCONDITIONS:
;;;   changes the gesture's handler to the given handler
(define gesture-change-handler! 
   (lambda (gesture function . oscjoint)
      (if (null? oscjoint)
          (gesture-two-handed-change-handler! gesture function)
          (gesture-simple-change-handler! gesture function (car oscjoint)))))

(gesture-change-handler! gesture-both-hands-up print-down)
(gesture-change-handler! gesture-joint-up print-forward "l_hand")
(gesture-change-handler! gesture-both-hands-still print-still)
(gesture-disable! gesture-both-hands-still)
(gestures-stop!)
(kinect-start! 7110)
(print (oscstring->joint "l_hand"))


;;; PROCEDURE:
;;;   gesture-simple-change-handler!
;;; PARAMETERS:
;;;   gesture, a simple or multidirectioanl gesture
;;;   function, the new handler
;;;   oscjoint, an oscstring for a joint
;;; PURPOSE: 
;;;   changes the gesture's handler for the given joint
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should be called only by gesture-change-handler!
;;; POSTCONDITIONS:
;;;   changes the gesture's handler for the joint to the given function
;;;   returns an error if the joint is not enabled
(define gesture-simple-change-handler! 
   (lambda (gesture function oscjoint)
      (let ((pair (assoc gesture gestures)))
         (if (and pair (enabled? oscjoint))
             (set-cdr! (assoc oscjoint (cdr pair)) function)
             (error "gesture not found")))))

;;; PROCEDURE:
;;;   gesture-two-handed-change-handler!
;;; PARAMETERS:
;;;   gesture, a gesture function
;;;   function, the new handler
;;; PURPOSE: 
;;;   changes the handler for the given two-handed gesture
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should be called only by gesture-change-handler!
;;; POSTCONDITIONS:
;;;   changes the gesture's handler to the given function
;;;   returns an error if gesture is not in the two-handed-gestures list
(define gesture-two-handed-change-handler! 
   (lambda (gesture function)
      (let ((pair (assoc gesture two-handed-gestures)))
         (if pair 
             (set-cdr! pair function)
             (error "gesture not found")))))

;;; PROCEDURE:
;;;   gesture-disable! 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;;   [oscjoint], an optional osc string for a joint
;;; PURPOSE: 
;;;   sets the handler for the given gesture to 'NULL
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscjoint is null if gesture is a two-handed-gesture
;;; POSTCONDITIONS:
;;;   calls gesture-change-handler! with 'NULL as an argument
;;;   the gesture will no longer be evaluated
(define gesture-disable! 
   (lambda (gesture . oscjoint)
      (if (null? oscjoint)
          (gesture-change-handler! gesture 'NULL)
          (gesture-change-handler! gesture 'NULL oscjoint))))


;----------; PROGRAMMER CONTROLS ;----------;

;;; PROCEDURE:
;;;   set-user! 
;;; PARAMETERS:
;;;   new-id, an integer
;;; PURPOSE: 
;;;   sets the user-id to new-id
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   user-id is the id of the user whose gestures are being recognized
;;; POSTCONDITIONS:
;;;   user-id is now new-id
(define set-user!
   (lambda (new-id)
      (set! user-id new-id)))

;;; PROCEDURE:
;;;   movement-threshold-set!
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   changes the movement-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side-effects
;;; PRECONDITIONS:
;;;   movement-threshold is the distance a joint must move along an axis
;;;   until its movement is registered
;;; POSTCONDITIONS:
;;;   movement-threshold is now new-threshold
(define movement-threshold-set! 
   (lambda (new-threshold)
      (set! movement-threshold new-threshold)))

;;; PROCEDURE:
;;;   fast-threshold-set! 
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   sets the fast-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   fast-threshold is the speed a joint must be going to be considered fast
;;; POSTCONDITIONS:
;;;   fast-threshold is now new-threshold
(define fast-threshold-set! 
   (lambda (new-threshold)
      (set! fast-threshold new-threshold)))

;;; PROCEDURE:
;;;   slow-threshold-set! 
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   sets the slow-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   slow-threshold is the speed a joint must be going to be considered slow
;;; POSTCONDITIONS:
;;;   slow-threshold is now new-threshold
(define slow-threshold-set! 
   (lambda (new-threshold)
      (set! slow-threshold new-threshold)))

;;; PROCEDURE:
;;;   stop-threshold-set! 
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   sets the stop-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   stop-threshold is the time it takes for joint to be registered as stopped
;;;   new-threshold should be some fraction of *second* for best results
;;; POSTCONDITIONS:
;;;   stop-threshold is now new-threshold
(define stop-threshold-set!
   (lambda (new-threshold)
      (set! stop-threshold new-threshold)))

;;; PROCEDURE:
;;;   simultaneous-threshold-set! 
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   sets the simultaneous-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   simultaneous-threshold is the max time between movements before those movements
;;;   are considered out of sync 
;;;   new-threshold should be some fraction of *second* for best results
;;; POSTCONDITIONS:
;;;   simultaneous-threshold is now new-threshold
(define simultaneous-threshold-set! 
   (lambda (new-threshold)
      (set! simultaneous-threshold new-threshold)))

;;; PROCEDURE:
;;;   refresh-rate-set! 
;;; PARAMETERS:
;;;   new-rate, an integer
;;; PURPOSE: 
;;;   sets the refresh-rate to new-rate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   refresh-rate determines how often track-gestures is called 
;;;   new-rate should be some fraction of *second* for best results
;;; POSTCONDITIONS:
;;;   refresh-rate is now new-rate
(define refresh-rate-set! 
   (lambda (new-rate)
      (set! refresh-rate new-rate)))

;;; PROCEDURE:
;;;   gestures-start! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   activates gesture-recognition
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   DONE is currently #t
;;; POSTCONDITIONS:
;;;   DONE is now #f and gestures are being processed
(define gestures-start!
   (lambda ()
      (set! DONE #f)
      (track-gestures)))

;;; PROCEDURE:
;;;   gestures-stop! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   stops gesture-recognition
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   DONE is currently #f
;;; POSTCONDITIONS:
;;;   DONE is now #t and gestures are not being processed
(define gestures-stop! 
   (lambda ()
      (set! DONE #t)))


;----------; LIST HELPERS ;----------;

;;; PROCEDURE:
;;;   list-append! 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   adds val to the end of list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   lst cannot be empty
;;; POSTCONDITIONS:
;;;   val is now at the end of lst
(define list-append!
  (lambda (lst val)
    (if (null? (cdr lst))
        (set-cdr! lst (cons val '()))
        (list-append! (cdr lst) val))
    lst))                             

;;; PROCEDURE:
;;;   list-prepend! 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   adds val to the start of list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   lst cannot be empty
;;; POSTCONDITIONS:
;;;   val is now at the start of lst
(define list-prepend!
  (lambda (lst val)
    (let ((newtail (cons (car lst) (cdr lst))))
      (set-car! lst val)
      (set-cdr! lst newtail)
      lst)))


;;; PROCEDURE:
;;;   list-delete! 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   deletes first instance of val from list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   val cannot be the last value in the list
;;; POSTCONDITIONS:
;;;   the first instance of val is removed
(define list-delete!
  (lambda (lst val)
    (cond
      ((null? (cdr lst)))
      ((equal? val (car lst))
       (set-car! lst (cadr lst))
       (set-cdr! lst (cddr lst)))
      (else
       (list-delete! (cdr lst) val)))
    lst))

;;; PROCEDURE:
;;;   list-index 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   returns the position of val in lst
;;; PRODUCED: 
;;;   pos, the position of val
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   pos if false if val is not in list
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