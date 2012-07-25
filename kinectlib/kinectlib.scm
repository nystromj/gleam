; functions that should not be called by the programmer are prepended with a '~ character

;----------| GLOBALS |----------;

;;; determines whether gestures should be detected
(define TRACKING #f)

;;; determines whether joint information should be passed to handlers
(define PARAMETERS #f)

;;; binds axes with index variables for referencing axis vectors
(define x 0)
(define y 1)
(define z 2)

;;; determines the user whose gestures are being evaluated
(define user-id 1)

;;; the amount a joint must move along the x or y axis before it is considered a movement
(define movement-threshold 0.05)

;;; the amount a joint must move along the z axis before it is considered a movement.
(define z-movement-threshold .1)

;;; speed thresholds to determine if the joint is moving slowly or quickly
(define fast-threshold .15)
(define slow-threshold .01)

;;; the amount of time that must pass before joint is registered as stop
(define still-threshold (* *second* 1.5))

;;; the max time that can occur between movements before they are considered out of sync. 
(define simultaneous-threshold 100000)

;;; how often ~track-gestures is called
(define refresh-rate (* *second* .1))


;----------| LIST HELPERS |----------;

;;; PROCEDURE:
;;;   ~list-prepend! 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   adds val to the start of list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;;   lst cannot be empty
;;; POSTCONDITIONS:
;;;   val is now at the start of lst
(define ~list-prepend!
  (lambda (lst val)
    (let ((newtail (cons (car lst) (cdr lst))))
      (set-car! lst val)
      (set-cdr! lst newtail)
      lst)))

;;; PROCEDURE:
;;;   ~list-delete! 
;;; PARAMETERS:
;;;   lst, a list
;;;   val, a value
;;; PURPOSE: 
;;;   deletes first instance of val from list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;;   val cannot be the last value in the list (the val at (- (length lst) 1))
;;; POSTCONDITIONS:
;;;   the first instance of val is removed
(define ~list-delete!
  (lambda (lst val)
    (cond
      ((null? (cdr lst)))
      ((equal? val (car lst))
       (set-car! lst (cadr lst))
       (set-cdr! lst (cddr lst)))
      (else
       (~list-delete! (cdr lst) val)))
    lst))


;----------| KINECT |----------;

;;; PROCEDURE:
;;;   kinect-start!
;;; PARAMETERS:
;;;   port, the port that the kinect is sending messages to
;;; PURPOSE: 
;;;   reads oscmessages from the given port
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   The kinect should be sending oscmessages to port
;;; POSTCONDITIONS:
;;;   Impromptu can now process osc signals if io:osc:receive is defined
(define kinect-start! 
   (lambda (port)
      (io:osc:start-server port)))


;----------| JOINT VECTORS SETUP |----------;

;;; PROCEDURE:
;;;   ~make-axis
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   creates a vector that stores the values for an axis
;;; PRODUCED: 
;;;   the new axis vector
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user 
;;; POSTCONDITIONS:
;;;   the created vector has 3 elements: a vector that stores the positions of that axis, 
;;;   a counter variable to reference position vector, and a variable that keeps track
;;;   of axis direction
(define ~make-axis
   (lambda () 
      (vector 0
              0
              (make-vector 100 (cons 0 0)))))

;;; PROCEDURE:
;;;   ~make-joint
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   creates a joint vector
;;; PRODUCED: 
;;;   the new joint vector
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user 
;;; POSTCONDITIONS:
;;;   the created joint vector contains 3 axis vectors, one for each x y and z and can be 
;;;   assigned to an oscstring
(define ~make-joint
   (lambda ()
      (vector (~make-axis)        ;; x
              (~make-axis)        ;; y
              (~make-axis))))     ;; z

;;; list of enabled joints -> left hand and right hand are default
(define joints
   (list (cons "l_hand" (~make-joint))
         (cons "r_hand" (~make-joint))))

;;; PROCEDURE:
;;;   enabled?
;;; PARAMETERS:
;;;   oscjoint, the oscstring for a joint
;;; PURPOSE: 
;;;   determines whether the data for joint is being stored
;;; PRODUCED: 
;;;   boolean, whether the joint is enabled
;;; PRECONDITIONS:
;;;   NULL 
;;; POSTCONDITIONS:
;;;   returns true if oscjoint is a member of joints list
(define enabled? 
   (lambda (oscjoint)
         (if (assoc oscjoint joints) 
             #t
             #f)))

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
             (print-error 'oscstring->joint: oscstring 
                          (string->symbol "is not an enabled joint."))))))

;----------| JOINT VECTOR GETTERS AND SETTERS |----------;

;;; PROCEDURE:
;;;   ~axis-get-counter
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   returns the counter index for the axis vector
;;; PRODUCED: 
;;;   int, the counter
;;; PRECONDITIONS:
;;;   should not be called directly by user
;;;   counter points to the last stored position
;;; POSTCONDITIONS:
;;;   counter is between 0 and 99
(define ~axis-get-counter 
   (lambda (joint axis)
      (vector-ref (vector-ref joint axis) 0)))

;;; PROCEDURE:
;;;   ~axis-set-counter!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;;   counter, an int
;;; PURPOSE: 
;;;   sets axis counter to counter
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called only by ~axis-increment-counter!
;;;   counter should be between 0 and 99
;;; POSTCONDITIONS:
;;;   the vector counter is now set to counter
(define ~axis-set-counter! 
   (lambda (joint axis counter)
      (vector-set! (vector-ref joint axis) 0 counter)))

;;; PROCEDURE:
;;;   ~axis-increment-counter!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   increments axis counter by one
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called only by ~axis-store-pos!
;;;   counter should be between 0 and 99
;;; POSTCONDITIONS:
;;;   axis counter has been increased by one 
;;;   if the counter was 99, it is now 0
(define ~axis-increment-counter! 
   (lambda (joint axis)
      (if (equal? (~axis-get-counter joint axis) 99)
          (~axis-set-counter! joint axis 0)
          (~axis-set-counter! joint axis (+ (~axis-get-counter joint axis) 1)))))

;;; PROCEDURE:
;;;   axis-get-current-pos
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   returns the last stored position for the axis
;;; PRODUCED: 
;;;   int, the last position stored
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if no positions have been stored, returns 0
(define axis-get-current-pos
   (lambda (joint axis)
      (car (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (~axis-get-counter joint axis)))))

;;; PROCEDURE:
;;;   axis-get-current-time
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   returns the time that the last value was stored
;;; PRODUCED: 
;;;   int, the time of storage
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if no positions have been stored, returns 0
;;;   time is given by Impromptu's (now) function
(define axis-get-current-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (~axis-get-counter joint axis)))))

;;; PROCEDURE:
;;;   ~axis-get-previous-counter
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   determines the index of the previous position stored
;;; PRODUCED: 
;;;   int, the index of the previous position stored
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   counter is between 0 and 99
(define ~axis-get-previous-counter 
   (lambda (joint axis)
      (if (equal? (~axis-get-counter joint axis) 0)
          99
          (- (~axis-get-counter joint axis) 1))))

;;; PROCEDURE:
;;;   axis-get-previous-pos
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   returns the position of the joint before the last stored position
;;; PRODUCED: 
;;;   int, the previous position
;;; PRECONDITIONS:
;;;   at least two values have been stored
;;; POSTCONDITIONS:
;;;   if less than two positions have been stored, returns 0
(define axis-get-previous-pos
   (lambda (joint axis)
      (car (vector-ref (vector-ref (vector-ref joint axis) 2) 
                       (~axis-get-previous-counter joint axis)))))

;;; PROCEDURE:
;;;   axis-get-direction
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors   
;;; PURPOSE: 
;;;   returns the direction stored for the given joint axis
;;; PRODUCED: 
;;;   int, the direction stored
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   if no direction stored, returns zero
(define axis-get-direction
   (lambda (joint axis)
       (vector-ref (vector-ref joint axis) 1)))

;;; PROCEDURE:
;;;   ~axis-set-direction!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;;   direction, an int
;;; PURPOSE: 
;;;   stores the given direction to the axis vector
;;; PRODUCED: 
;;;   NULL
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   direction value is now stored in the axis vector to be obtained with axis-get-direction
(define ~axis-set-direction! 
   (lambda (joint axis direction)
      (vector-set! (vector-ref joint axis) 1 direction)))


;----------| JOINT VECTOR DATA INTERPRETATION |----------;

;;; PROCEDURE:
;;;   ~axis-calculate-direction
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors   
;;; PURPOSE: 
;;;   calculates the direction between the 2 most recent saved points
;;; PRODUCED: 
;;;   int, the direction
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   if direction is negative, joint is moving towards origin
(define ~axis-calculate-direction
   (lambda (joint axis)
      (- (axis-get-current-pos joint axis) (axis-get-previous-pos joint axis))))

;;; PROCEDURE:
;;;   ~same-sign?
;;; PARAMETERS:
;;;   x, an int
;;;   y, an int   
;;; PURPOSE: 
;;;   determines whether the two ints are both positive or both negative
;;; PRODUCED: 
;;;   boolean, whether the ints have the same sign
;;; PRECONDITIONS:
;;;   called only by axis-direction-changed?
;;; POSTCONDITIONS:
;;;   returns true if ints are both positive or both negative
;;;   returns false if one of the ints is zero
;;;   returns true if both ints are zero
(define ~same-sign? 
   (lambda (x y)
      (if (and (equal? x 0) (equal? y 0))
          #t
          (> (abs (+ x y)) (abs (- x y))))))

;;; PROCEDURE:
;;;   ~axis-direction-changed?
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors   
;;; PURPOSE: 
;;;   determines whether the current direction matches the stored direction
;;; PRODUCED: 
;;;   boolean, whether direction has changed
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;;   a direction has been stored in axis vector
;;;   at least two positions have been stored in axis vector
;;; POSTCONDITIONS:
;;;   does not store new direction
(define ~axis-direction-changed?
   (lambda (joint axis)
      (not (~same-sign? (axis-get-direction joint axis) 
                        (~axis-calculate-direction joint axis)))))

;;; PROCEDURE:
;;;   ~axis-moved?
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;;   new-pos, the position the joint is currently at
;;; PURPOSE: 
;;;   determines whether the joint has moved from its previous stored point
;;; PRODUCED: 
;;;   boolean, whether the joint as moved
;;; PRECONDITIONS:
;;;   procedure should be called only by ~axis-store-pos!
;;;   movement-threshold defines the amount the joint must move up or down before movement
;;;   is established
;;;   z-movement-threshold is the amount the joint must move along the z axis before movement
;;; POSTCONDITIONS:
;;;   returns true if the distance between the last saved joint position and the 
;;;   current joint position is greater than or equal to movement-threshold or z-movement-threshold
(define ~axis-moved? 
   (lambda (joint axis new-pos)
      (if (equal? axis z)
          (> (abs (- (/ (axis-get-current-pos joint axis) 100) new-pos)) 
             z-movement-threshold)
          (> (abs (- (/ (axis-get-current-pos joint axis) 100) new-pos)) 
             movement-threshold))))

;;; PROCEDURE:
;;;   ~axis-still? 
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to the axis vector of the joint
;;; PURPOSE: 
;;;   determines whether the joint is moving along the given axis
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored recently for that axis
;;;   and the direction of the joint is zero.
(define ~axis-still? 
   (lambda (joint axis)
      (equal? (axis-get-direction joint axis) 0)))

;;; PROCEDURE:
;;;   ~axis-stopped?
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   determines whether the joint axis has stored any new positions in time defined by 
;;;   still-threshold
;;; PRODUCED: 
;;;   boolean, whether positions have been stored recently
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored in time defined by still-threshold
(define ~axis-stopped? 
   (lambda (joint axis)
      (>= (- (now) (axis-get-current-time joint axis)) still-threshold)))


;----------| OSCMESSAGE HANDLING AND STORAGE |----------;

;;; PROCEDURE:
;;;   ~axis-update! 
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   checks to see if the joint has stopped moving along the axis
;;;   changes the joint's direction if appropriate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;;   the direction of the joint is not already zero
;;; POSTCONDITIONS:
;;;   if the joint has stopped moving along the axis, direction for that axis is zero
(define ~axis-update!
   (lambda (joint axis)
      (if (and (~axis-stopped? joint axis) (not (~axis-still? joint axis)))
          (~axis-set-direction! joint axis 0))))

;;; PROCEDURE:
;;;   ~axis-store-pos!
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;;   new-pos, the new position
;;; PURPOSE: 
;;;   stores the given position at the appropriate point in the joint's axis vector
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   the new position is now stored at the appropriate point in the axis vector
;;;   new-pos is now attainable by axis-get-current-pos
;;;   the new position is paired with the time it was stored
;;;   sets a new direction for the axis if direction has changed
(define ~axis-store-pos! 
   (lambda (joint axis new-pos)
      (~axis-increment-counter! joint axis)      
      (vector-set! (vector-ref (vector-ref joint axis) 2)
                   (~axis-get-counter joint axis) 
                   (cons (* 100 new-pos) (now)))
      (if (~axis-direction-changed? joint axis)
          (~axis-set-direction! joint axis (~axis-calculate-direction joint axis)))))

;;; PROCEDURE:
;;;  ~oscdata-process-axis-coordinates
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;;   new-pos, the position the joint is currently at
;;; PURPOSE: 
;;;   determines whether to store the new coordinate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called only by ~oscdata-process-joint-coordinates
;;; POSTCONDITIONS:
;;;   if the joint has moved along the given axis, it stores the new position
;;;   if the joint has not moved, calls ~axis-update!
(define ~oscdata-process-axis-coordinates
   (lambda (joint axis coord)
      (if (~axis-moved? joint axis new-pos)
          (~axis-store-pos! joint axis new-pos)
          (~axis-update! joint axis))))

;;; PROCEDURE:
;;;   ~oscdata-process-joint-coordinates
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   coords, the list of coordinates for the current x y and z positions of the joint
;;; PURPOSE: 
;;;   calls ~oscdata-process-axis-coordinates for each axis of the joint
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called only by ~oscdata-process
;;;   coords is a list of three floats that correspond to x y and z
;;;   joint is a joint vector that matches an oscjoint in joints
;;; POSTCONDITIONS:
;;;   calls ~oscdata-process-axis-coordinates for each axis
(define ~oscdata-process-joint-coordinates
   (lambda (joint coords)
      (for-each (lambda (axis coord)
                   (~oscdata-process-axis-coordinates joint axis coord))
                (list x y z)
                coords)))

;;; PROCEDURE:
;;;   ~oscdata-process
;;; PARAMETERS:
;;;   oscdata, the list of arguments bundled in an oscmessage
;;; PURPOSE: 
;;;   checks whether joint is enabled and sends information to ~oscdata-process-joint-coordinates
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   oscdata is a list of arguments of the form "sifff" 
;;; POSTCONDITIONS:
;;;   if the oscjoint string bundled in oscdata matches a joint in the enabled joints list, 
;;;   ~oscdata-process-joint-coordinates is called with the remaining arguments
(define ~oscdata-process
   (lambda (oscdata)
      (let ((joint (assoc (car oscdata) joints)))
         (if joint
             (~oscdata-process-joint-coordinates (cdr joint) (cddr oscdata))))))

;;; PROCEDURE:
;;;   io:osc:receive
;;; PARAMETERS:
;;;   timestamp, when the oscmessage is received
;;;   address, the address string of the oscmessage
;;;   args, a list of args defined by the oscmessage
;;; PURPOSE: 
;;;   handles incoming oscmessages and calls storage procedure when the user matches user-id
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user (~ omission necessary)
;;;   the osc server must be started with io:osc:start-server, called in kinect-start!
;;;   the kinect user must be user 1
;;; POSTCONDITIONS:
;;;   calls ~oscdata-process if user's id matches the current user-id
(define io:osc:receive 
   (lambda (timestamp address . args)
      (if (and (equal? (list-ref args 1) user-id) TRACKING)
          (~oscdata-process args))))


;----------| SPEED |----------;                            

;;; PROCEDURE:
;;;   axis-get-previous-time
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   returns the time that the value before the current value was stored
;;; PRODUCED: 
;;;   int, the time at storage
;;; PRECONDITIONS:
;;;   at least two values have been stored
;;; POSTCONDITIONS:
;;;   if less than two positions have been stored, returns 0
;;;   time is given by Impromptu's (now) function
(define axis-get-previous-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (~axis-get-previous-counter joint axis)))))

;;; PROCEDURE:
;;;   axis-calculate-speed
;;; PARAMETERS:
;;;   joint, the joint vector
;;;   axis, index to one of joint's axis vectors   
;;; PURPOSE: 
;;;   calculates the speed between the 2 most recently saved points
;;; PRODUCED: 
;;;   int, the speed
;;; PRECONDITIONS:
;;;   at least 2 positions have recently been stored in joint's axis vector
;;; POSTCONDITIONS:
;;;   if returned int is negative, joint is moving towards origin    
(define axis-calculate-speed
   (lambda (joint axis)
      (/ (axis-get-direction joint axis) 
         (/ (- (axis-get-current-time joint axis) (axis-get-previous-time joint axis)) 100))))

;;; PROCEDURE:
;;;   axis-movement-fast?
;;; PARAMETERS:
;;;   joint, a joint vector
;;;   axis, index to one of joint's axis vectors 
;;; PURPOSE: 
;;;   checks the speed of the axis movement against the fast-threshold 
;;; PRODUCED: 
;;;   boolean, whether the axis is moving quickly
;;; PRECONDITIONS:
;;;   fast-threshold is defined
;;; POSTCONDITIONS:
;;;   returns true if calculated axis speed is greater than the fast threshold
(define axis-movement-fast? 
   (lambda (joint axis)
      (>= (abs (axis-calculate-speed joint axis)) fast-threshold)))

;;; PROCEDURE:
;;;   axis-movement-slow?
;;; PARAMETERS:
;;;   joint, a joint vector
;;;   axis, index to one of joint's axis vectors
;;; PURPOSE: 
;;;   checks the speed of the axis movement against the slow-threshold 
;;; PRODUCED: 
;;;   boolean, whether the axis is moving slowly
;;; PRECONDITIONS:
;;;   slow-threshold is defined
;;; POSTCONDITIONS:
;;;   returns true if calculated axis speed is less than the slow threshold
(define axis-movement-slow? 
   (lambda (speed)
      (< (abs (axis-calculate-speed joint axis)) slow-threshold)))


;----------| JOINT BEHAVIOR |----------;

;;; PROCEDURE:
;;;   ~joint-still? 
;;; PARAMETERS:
;;;   joint, the joint vector
;;; PURPOSE: 
;;;   determines whether the joint has stopped making registered movements
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS:
;;;   if true, no new positions have been stored recently for the joint's axes
(define ~joint-still? 
   (lambda (joint)
      (and (~axis-still? joint x)
           (~axis-still? joint y)
           (~axis-still? joint z))))

;;; PROCEDURE:
;;;   ~joint-right?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving right
;;; PRODUCED: 
;;;   boolean, whether joint is moving right
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved right
(define ~joint-right? 
   (lambda (joint)
      (and (< (axis-get-direction joint x) 0) (not (~axis-stopped? joint x)))))

;;; PROCEDURE:
;;;   ~joint-left?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving left
;;; PRODUCED: 
;;;   boolean, whether joint is moving left
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved left
(define ~joint-left? 
   (lambda (joint)
      (and (> (axis-get-direction joint x) 0) (not (~axis-stopped? joint x)))))

;;; PROCEDURE:
;;;   ~joint-up?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving up
;;; PRODUCED: 
;;;   boolean, whether joint is moving up
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved up
(define ~joint-up? 
   (lambda (joint)
      (and (< (axis-get-direction joint y) 0) (not (~axis-stopped? joint y)))))

;;; PROCEDURE:
;;;   ~joint-down?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving down
;;; PRODUCED: 
;;;   boolean, whether joint is moving down
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved down
(define ~joint-down? 
   (lambda (joint)
      (and (> (axis-get-direction joint y) 0) (not (~axis-stopped? joint y)))))

;;; PROCEDURE:
;;;   ~joint-forward?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving forward
;;; PRODUCED: 
;;;   boolean, whether joint is moving forward
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved forward
(define ~joint-forward?
   (lambda (joint)
      (and (< (axis-get-direction joint z) 0) (not (~axis-stopped? joint z)))))

;;; PROCEDURE:
;;;   ~joint-backward?
;;; PARAMETERS:
;;;   joint, the joint vector 
;;; PURPOSE: 
;;;   determines whether joint is moving backward
;;; PRODUCED: 
;;;   boolean, whether joint is moving backward
;;; PRECONDITIONS:
;;;   procedure should not be called directly by user
;;; POSTCONDITIONS: 
;;;   returns true if joint has just moved backward
(define ~joint-backward?
   (lambda (joint)
      (and (> (axis-get-direction joint z) 0) (not (~axis-stopped? joint z)))))


;----------| GESTURES |----------;

;;; PROCEDURE:
;;;   ~simultaneous? 
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
;;;   procedure should not be called directly by user
;;;   simultaneous-threshold is the maximum amount of time that can occur between
;;;   movements before they are considered out of sync
;;; POSTCONDITIONS:
;;;   returns true if the difference between the times of the last movements of 
;;;   each of the joints is less than simultaneous-threshold
(define ~simultaneous? 
   (lambda (joint1 axis1 joint2 axis2)
      (<= (abs (- (axis-get-current-time joint1 axis1) 
                  (axis-get-current-time joint2 axis2)))
          simultaneous-threshold)))


;---| simple gestures |---;

;;; GESTURE: returns whether joint is moving to the right
(define gesture-joint-right
   (lambda (joint)
      (~joint-right? joint)))

;;; GESTURE: returns whether joint is moving to the left
(define gesture-joint-left
   (lambda (joint)
      (~joint-left? joint)))

;;; GESTURE: returns whether joint is moving up
(define gesture-joint-up
   (lambda (joint)
      (~joint-up? joint)))

;;; GESTURE: returns whether joint is moving down
(define gesture-joint-down
   (lambda (joint)
      (~joint-down? joint)))

;;; GESTURE: returns whether joint is moving forward
(define gesture-joint-forward
   (lambda (joint)
      (~joint-forward? joint)))

;;; GESTURE: returns whether joint is moving backward
(define gesture-joint-backward
   (lambda (joint)
      (~joint-backward? joint)))

;;; GESTURE: returns whether joint is still
(define gesture-joint-still
   (lambda (joint)
      (~joint-still? joint)))


;---| multidirectional gestures |---;

;;; GESTURE: returns whether joint is moving to the right and up 
(define gesture-joint-right-up
   (lambda (joint)
      (and (gesture-joint-up joint)
           (gesture-joint-right joint)
           (~simultaneous? joint x joint y))))

;;; GESTURE: returns whether joint is moving to the right and down 
(define gesture-joint-right-down
   (lambda (joint)
      (and (gesture-joint-down joint)
           (gesture-joint-right joint)
           (~simultaneous? joint x joint y))))

;;; GESTURE: returns whether joint is moving to the right and forward 
(define gesture-joint-right-forward
   (lambda (joint)
      (and (gesture-joint-forward joint)
           (gesture-joint-right joint)
           (~simultaneous? joint x joint z))))

;;; GESTURE: returns whether joint is moving to the right and backward 
(define gesture-joint-right-backward
   (lambda (joint)
      (and (gesture-joint-backward joint)
           (gesture-joint-right joint)
           (~simultaneous? joint x joint z))))

;;; GESTURE: returns whether joint is moving to the left and up 
(define gesture-joint-left-up
   (lambda (joint)
      (and (gesture-joint-up joint)
           (gesture-joint-left joint)
           (~simultaneous? joint x joint y))))

;;; GESTURE: returns whether joint is moving to the left and down 
(define gesture-joint-left-down
   (lambda (joint)
      (and (gesture-joint-down joint)
           (gesture-joint-left joint)
           (~simultaneous? joint x joint y))))

;;; GESTURE: returns whether joint is moving to the left and forward 
(define gesture-joint-left-forward
   (lambda (joint)
      (and (gesture-joint-forward joint)
           (gesture-joint-left joint)
           (~simultaneous? joint x joint z))))

;;; GESTURE: returns whether joint is moving to the left and backward 
(define gesture-joint-left-backward
   (lambda (joint)
      (and (gesture-joint-backward joint)
           (gesture-joint-left joint)
           (~simultaneous? joint x joint z))))

;;; GESTURE: returns whether joint is moving up and forward 
(define gesture-joint-up-forward
   (lambda (joint)
      (and (gesture-joint-forward joint)
           (gesture-joint-up joint)
           (~simultaneous? joint y joint z))))

;;; GESTURE: returns whether joint is moving up and backward 
(define gesture-joint-up-backward
   (lambda (joint)
      (and (gesture-joint-backward joint)
           (gesture-joint-up joint)
           (~simultaneous? joint y joint z))))

;;; GESTURE: returns whether joint is moving down and forward 
(define gesture-joint-down-forward 
   (lambda (joint)
      (and (gesture-joint-forward joint)
           (gesture-joint-down joint)
           (~simultaneous? joint y joint z))))

;;; GESTURE: returns whether joint is moving down and backward 
(define gesture-joint-down-backward
   (lambda (joint)
      (and (gesture-joint-backward joint)
           (gesture-joint-down joint)
           (~simultaneous? joint y joint z))))

;;; GESTURE: returns whether joint is moving to the right, up, and forward 
(define gesture-joint-right-up-forward
   (lambda (joint)
      (and (gesture-joint-right-up joint)
           (gesture-joint-right-forward joint))))

;;; GESTURE: returns whether joint is moving to the right, up, and backward
(define gesture-joint-right-up-backward
   (lambda (joint)
      (and (gesture-joint-right-up joint)
           (gesture-joint-right-backward joint))))

;;; GESTURE: returns whether joint is moving to the right, down, and forward 
(define gesture-joint-right-down-forward
   (lambda (joint)
      (and (gesture-joint-right-down joint)
           (gesture-joint-right-forward joint))))

;;; GESTURE: returns whether joint is moving to the right, down, and backward
(define gesture-joint-right-down-backward 
   (lambda (joint)
      (and (gesture-joint-right-down joint)
           (gesture-joint-right-backward joint))))

;;; GESTURE: returns whether joint is moving to the left, up, and forward
(define gesture-joint-left-up-forward
   (lambda (joint)
      (and (gesture-joint-left-up joint)
           (gesture-joint-left-forward joint))))

;;; GESTURE: returns whether joint is moving to the left, up, and backward
(define gesture-joint-left-up-backward
   (lambda (joint)
      (and (gesture-joint-left-up joint)
           (gesture-joint-left-backward joint))))

;;; GESTURE: returns whether joint is moving to the left, down, and forward
(define gesture-joint-left-down-forward
   (lambda (joint)
      (and (gesture-joint-left-down joint)
           (gesture-joint-left-forward joint))))

;;; GESTURE: returns whether joint is moving to the left, down, and backward
(define gesture-joint-left-down-backward
   (lambda (joint)
      (and (gesture-joint-left-down joint)
           (gesture-joint-left-backward joint))))


;---| two-handed-gestures |---;

;;; GESTURE: returns whether both hands are moving to the right
(define gesture-both-hands-right
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right left)
              (gesture-joint-right right)
              (~simultaneous? left x right x)))))

;;; GESTURE: returns whether both hands are moving to the left
(define gesture-both-hands-left
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left left)
              (gesture-joint-left right)
              (~simultaneous? left x right x)))))

;;; GESTURE: returns whether both hands are moving towards the body
(define gesture-both-hands-in
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right left)
              (gesture-joint-left right)
              (~simultaneous? left x right x)))))

;;; GESTURE: returns whether both hands are moving away from the body
(define gesture-both-hands-away
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right right)
              (gesture-joint-left left)
              (~simultaneous? left x right x)))))

;;; GESTURE: returns whether both hands are moving up
(define gesture-both-hands-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up left)
              (gesture-joint-up right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving down
(define gesture-both-hands-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-down left)
              (gesture-joint-down right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether the right hand is moving up while the left is moving down
(define gesture-right-hand-up-left-hand-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up right)
              (gesture-joint-down left)
              (~simultaneous? left y right y)))))      

;;; GESTURE: returns whether the right hand is moving down while the left is moving up
(define gesture-right-hand-down-left-hand-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up left)
              (gesture-joint-down right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving forward
(define gesture-both-hands-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-forward left)
              (gesture-joint-forward right)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether both hands are moving backward
(define gesture-both-hands-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-backward left)
              (gesture-joint-backward right)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether the right hand is moving forward while the left is moving backward
(define gesture-right-hand-forward-left-hand-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-forward right)
              (gesture-joint-backward left)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether the right hand is moving backward while the left is moving forward
(define gesture-right-hand-backward-left-hand-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-forward left)
              (gesture-joint-backward right)
              (~simultaneous? left z right z)))))

;;; GESTIRE: both hands are moving to the right and up
(define gesture-both-hands-right-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-up right)
              (gesture-joint-right-up left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving to the right and down
(define gesture-both-hands-right-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-down right)
              (gesture-joint-right-down left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving to the right and forward
(define gesture-both-hands-right-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-forward right)
              (gesture-joint-right-forward left)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether both hands are moving to the right and backward
(define gesture-both-hands-right-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-backward right)
              (gesture-joint-right-backward left)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether both hands are moving to the left and up
(define gesture-both-hands-left-up
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-up right)
              (gesture-joint-left-up left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving to the left and down
(define gesture-both-hands-left-down
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-down right)
              (gesture-joint-left-down left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving to the left and forward
(define gesture-both-hands-left-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-forward right)
              (gesture-joint-left-forward left)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether both hands are moving to the left and backward
(define gesture-both-hands-left-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-backward right)
              (gesture-joint-left-backward left)
              (~simultaneous? left z right z)))))

;;; GESTURE: returns whether both hands are moving up and towards the body
(define gesture-both-hands-up-in
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-up left)
              (gesture-joint-left-up right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving up and away from the body
(define gesture-both-hands-up-out
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-up right)
              (gesture-joint-left-up left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving down and towards the body
(define gesture-both-hands-down-in 
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-right-down left)
              (gesture-joint-left-down right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving down and away from the body
(define gesture-both-hands-down-out
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-left-down left)
              (gesture-joint-right-down right)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving up and forward
(define gesture-both-hands-up-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up-forward right)
              (gesture-joint-up-forward left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving up and backward
(define gesture-both-hands-up-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-up-backward right)
              (gesture-joint-up-backward left)
              (~simultaneous? left y right y))))) 

;;; GESTURE: returns whether both hands are moving down and forward
(define gesture-both-hands-down-forward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-down-forward right)
              (gesture-joint-down-forward left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are moving down and backward
(define gesture-both-hands-down-backward
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-down-backward right)
              (gesture-joint-down-backward left)
              (~simultaneous? left y right y)))))

;;; GESTURE: returns whether both hands are still
(define gesture-both-hands-still 
   (lambda ()
      (let ((left (oscstring->joint "l_hand"))
            (right (oscstring->joint "r_hand")))
         (and (gesture-joint-still left)
              (gesture-joint-still right)))))


;----------| GESTURE SET UP |----------;

;;; simple gestures take a joint as a parameter and only deal with one axis
(define simple-gestures
   (list gesture-joint-right
         gesture-joint-left
         gesture-joint-up
         gesture-joint-down
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
         (if (member gesture simple-gestures)
             #t
             #f)))

;;; multidirectional gestures take a joint as a parameter and determine if there
;;; is movement in multiple directions
(define multidirectional-gestures 
   (list gesture-joint-right-up-forward
         gesture-joint-right-up-backward
         gesture-joint-right-down-forward
         gesture-joint-right-down-backward
         gesture-joint-left-up-forward
         gesture-joint-left-up-backward
         gesture-joint-left-down-forward
         gesture-joint-left-down-backward
         gesture-joint-right-up
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
         gesture-joint-down-backward))

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
         (if (member gesture multidirectional-gestures)
             #t
             #f)))

;;; appends the list of joint gestures together and adds default handlers
(define single-joint-gestures (map (lambda (elt)
                                      (cons elt (list (cons "l_hand" 'NULL)
                                                      (cons "r_hand" 'NULL))))
                                   (append multidirectional-gestures simple-gestures)))

;;; PROCEDURE:
;;;   single-joint? 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;; PURPOSE: 
;;;   determines whether the gesture function is a single-joint gesture
;;; PRODUCED: 
;;;   boolean, whether the gesture is single-joint
;;; PRECONDITIONS:
;;;   gesture is a predefined function
;;; POSTCONDITIONS:
;;;   returns true if gesture is a member of single-joint-gestures list
(define single-joint? 
   (lambda (gesture)
         (if (assoc gesture single-joint-gestures)
             #t
             #f)))

;;; two-handed gestures do not take joints as parameters and have only one handler
(define two-handed-gestures
   (map (lambda (gesture)
           (cons gesture 'NULL))
        (list gesture-both-hands-right
              gesture-both-hands-left
              gesture-both-hands-in
              gesture-both-hands-away
              gesture-both-hands-up
              gesture-both-hands-down
              gesture-right-hand-up-left-hand-down
              gesture-right-hand-down-left-hand-up
              gesture-both-hands-forward
              gesture-both-hands-backward
              gesture-right-hand-forward-left-hand-backward
              gesture-right-hand-backward-left-hand-forward
              gesture-both-hands-right-up
              gesture-both-hands-right-down
              gesture-both-hands-right-forward
              gesture-both-hands-right-backward
              gesture-both-hands-left-up
              gesture-both-hands-left-down
              gesture-both-hands-left-forward
              gesture-both-hands-left-backward
              gesture-both-hands-up-in
              gesture-both-hands-up-out
              gesture-both-hands-down-in
              gesture-both-hands-down-out
              gesture-both-hands-up-forward
              gesture-both-hands-up-backward
              gesture-both-hands-down-forward
              gesture-both-hands-down-backward
              gesture-both-hands-still)))

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
      (if (assoc gesture two-handed-gestures)
          #t
          #f)))


;----------| ADDING AND REMOVING JOINTS |----------;

;;; PROCEDURE:
;;;   ~joint-add-to-gestures!
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
(define ~joint-add-to-gestures! 
   (lambda (oscjoint)
      (for-each (lambda (gesture-pair)
                   (~list-prepend! (cdr gesture-pair) (cons oscjoint 'NULL)))
                single-joint-gestures)))

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
          (print-notification 'joint-add!: oscjoint (string->symbol "has already been enabled."))
          (begin (~list-prepend! joints (cons oscjoint (~make-joint)))
                 (~joint-add-to-gestures! oscjoint)
                 (print-notification 'joint-add!: oscjoint (string->symbol "is now enabled."))))))

;;; PROCEDURE:
;;;   ~joint-remove-from-gestures!
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
(define ~joint-remove-from-gestures! 
   (lambda (oscjoint)
      (for-each (lambda (gesture-pair)
                   (~list-delete! (cdr gesture-pair) (assoc oscjoint (cdr gesture-pair))))                                
                single-joint-gestures)))

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
                 (print-notification 'joint-remove!: oscjoint 
                                     (string->symbol "is not currently enabled.")))
                ((equal? oscjoint (car (list-ref joints (- (length joints) 1))))
                 (print-error 'joint-remove!: 
                              (string->symbol "cannot remove last joint in list.")))
                (else (~list-delete! joints (assoc oscjoint joints))
                      (~joint-remove-from-gestures! oscjoint)
                      (print-notification 'joint-remove!: oscjoint
                                          (string->symbol "is no longer enabled.")))))))

;;; TEST HANDLERS

(define print-left
   (lambda vals
      (if (not (null? vals))
          (print vals 'is 'moving 'left)
          (print 'left))))

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


;----------| GESTURE TRACKING |----------;

;;; list of joints that should be ignored
(define ignored '(NULL))

;;; PROCEDURE:
;;;   ~ignored? 
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
(define ~ignored? 
   (lambda (oscjoint)
      (if (member oscjoint ignored)
          #t
          #f)))

;;; PROCEDURE:
;;;   ~gesture-evaluate-two-handed-gesture-pair 
;;; PARAMETERS:
;;;   gesture-pair, a cons cell
;;; PURPOSE: 
;;;   determines if gesture is true for joint and executes handler if true
;;; PRODUCED: 
;;;   boolean, whether the handler was executed
;;; PRECONDITIONS:
;;;   should not be called directly by user
;;;   gesture-pair is a cons cell whose car is a two-handed gesture 
;;;   function and whose cdr is the handler for that gesture
;;;   handler must not be 'NULL
;;; POSTCONDITIONS:
;;;   if the gesture test returned true and handler was not 'NULL, handler 
;;;   was executed and both hands are added to the ignored list.
(define ~gesture-evaluate-two-handed-gesture-pair
   (lambda (gesture-pair)
      (cond ((and ((car gesture-pair)) 
                  (not (equal? (cdr gesture-pair) 'NULL)))
             (if PARAMETERS
                 ((cdr gesture-pair) (list "r_hand" "l_hand"))
                 ((cdr gesture-pair)))
             (~list-prepend! ignored "l_hand")
             (~list-prepend! ignored "r_hand")
             #t)
            (else #f))))

;;; PROCEDURE:
;;;   ~gesture-evaluate-two-handed-gestures 
;;; PARAMETERS:
;;;   lst, a list of cons cells
;;; PURPOSE: 
;;;   calls ~gesture-evaluate-two-handed-gesture-pair for each element in list
;;; PRODUCED: 
;;;   boolean, whether the joint is still
;;; PRECONDITIONS:
;;;   should only be called by ~gesture-evaluate-all
;;;   each cons cell in list has a two-handed-gesture function as its 
;;;   car and a handler as its cdr.
;;; POSTCONDITIONS:
;;;   stops executing after a two-handed gesture in lst returns true
(define ~gesture-evaluate-two-handed-gestures
   (lambda (lst)
      (cond ((null? lst)
             #f)
            ((~gesture-evaluate-two-handed-gesture-pair (car lst)))
            (else (~gesture-evaluate-two-handed-gestures (cdr lst))))))

;;; PROCEDURE:
;;;   ~gesture-evaluate-single-joint-handler 
;;; PARAMETERS:
;;;   gesture, a gseture function
;;;   oscjoint, an oscstring for a joint
;;;   handler, a handler function
;;; PURPOSE: 
;;;   evaluates the handler function then sets the joint to ignored if 
;;;   gesture is multidirectional
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should only be called by ~gesture-evaluate-single-joint
;;; POSTCONDITIONS:
;;;   if gesture was multidirectional, joint is now ignored
(define ~gesture-evaluate-single-joint-handler 
   (lambda (gesture oscjoint handler)
      (if PARAMETERS      
          (handler oscjoint)
          (handler))
      (if (multidirectional? gesture)
          (~list-prepend! ignored oscjoint))))

;;; PROCEDURE:
;;;   ~gesture-evaluate-single-joint 
;;; PARAMETERS:
;;;   gesture, a gesture function
;;;   oscjoint, an oscstring for a joint
;;;   handler, a handler function
;;; PURPOSE: 
;;;   determines if the gesture is true for the joint and calls handler processing
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   should be called only by ~gesture-evaluate-single-joint-pair
;;;   in order for handler to evaluate, joint should not be ignored and the handler
;;;   should not be null.
;;; POSTCONDITIONS:
;;;   if the preconditions are met and the gesture is true for the joint, calls
;;;   ~gesture-evaluate-single-joint-handler
(define ~gesture-evaluate-single-joint
   (lambda (gesture oscjoint handler)
      (if (and (not (~ignored? oscjoint)) (not (equal? handler 'NULL)))
          (if (gesture (oscstring->joint oscjoint))
              (~gesture-evaluate-single-joint-handler gesture oscjoint handler)))))

;;; PROCEDURE:
;;;   ~gesture-evaluate-single-joint-pair 
;;; PARAMETERS:
;;;   gesture-pair, a cons cell
;;; PURPOSE: 
;;;   calls ~gesture-evaluate-single-joint for each joint in the handler list
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should only be called by ~gesture-evaluate-all
;;;   gesture-pair is a cons cell whose car is a gesture function and whose cdr 
;;;   is the handler list for that gesture
;;; POSTCONDITIONS:
;;;   ~gesture-evaluate-single-joint is called on every element in the handler list
(define ~gesture-evaluate-single-joint-pair
   (lambda (gesture-pair)
      (for-each (lambda (handler-pair)
                   (~gesture-evaluate-single-joint (car gesture-pair) 
                                                   (car handler-pair) 
                                                   (cdr handler-pair)))
                (cdr gesture-pair))))

;;; PROCEDURE:
;;;   ~gesture-evaluate-all 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   checks all the gestures and calls handlers where appropriate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   procedure should only be called by ~track-changes
;;; POSTCONDITIONS:
;;;   if both hands are enabled it evaluates two-handed gestures before evaluating
;;;   the gestures list
(define ~gesture-evaluate-all
   (lambda ()
      (set! ignored (list 'NULL))
      (if (and (enabled? "l_hand") (enabled? "r_hand"))
          (~gesture-evaluate-two-handed-gestures two-handed-gestures))
      (for-each ~gesture-evaluate-single-joint-pair single-joint-gestures)))

;;; PROCEDURE:
;;;   ~track-gestures 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   runs a loop that checks for gestures 
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   called by start!, not explicity by programmer
;;;   frequency of loop is determined by refresh-rate
;;; POSTCONDITIONS:
;;;   continues until TRACKING is declared #f
(define ~track-gestures
   (lambda ()
      (when TRACKING
            (~gesture-evaluate-all)
            (callback (+ (now) refresh-rate) '~track-gestures))))


;----------| HANDLER MANAGEMENT |---------;

;;; PROCEDURE:
;;;   ~gesture-two-handed-change-handler!
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
(define ~gesture-two-handed-change-handler! 
   (lambda (gesture function)
      (let ((pair (assoc gesture two-handed-gestures)))
         (if pair 
             (set-cdr! pair function)
             (print-error 'gesture-change-handler!:
                          (string->symbol "gesture not found."))))))

;;; PROCEDURE:
;;;   ~gesture-single-joint-change-handler!
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
(define ~gesture-single-joint-change-handler! 
   (lambda (gesture function oscjoint)
      (let ((pair (assoc gesture single-joint-gestures)))
         (if (and pair (enabled? oscjoint))
             (set-cdr! (assoc oscjoint (cdr pair)) function)
             (print-error 'gesture-change-handler!:
                          (string->symbol "gesture not found."))))))

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
          (~gesture-two-handed-change-handler! gesture function)
          (~gesture-single-joint-change-handler! gesture function (car oscjoint)))))

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

;;; PROCEDURE:
;;;   gesture-disable-two-handed-gestures! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   sets the handler for all two-handed-gestures to 'NULL
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   only changes gestures that had non-null handlers
;;;   no two-handed gestures will be active until handlers are changed from 'NULL
(define gesture-disable-two-handed-gestures!
   (lambda ()
      (for-each (lambda (gesture-pair)
                   (if (not (equal? (cdr gesture-pair) 'NULL))
                       (set-cdr! gesture-pair 'NULL)))
                two-handed-gestures)))

;;; PROCEDURE:
;;;   gesture-disable-single-joint-gestures! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   sets the handler for all single-joint-gestures to 'NULL
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   only changes gestures that had non-null handlers
;;;   no single-joint gestures will be active until handlers are changed from 'NULL
(define gesture-disable-single-joint-gestures!
   (lambda ()
      (for-each (lambda (gesture-pair)
                   (for-each (lambda (handler-pair)
                                (if (not (equal? (cdr handler-pair) 'NULL))
                                    (set-cdr! handler-pair 'NULL)))
                             (cdr gesture-pair)))
                single-joint-gestures)))

;;; PROCEDURE:
;;;   gesture-disable-all! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   sets the handler for all gestures to 'NULL
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   only changes gestures that had non-null handlers
;;;   no gestures will be active until handlers are changed from 'NULL
(define gesture-disable-all! 
   (lambda ()
      (gesture-disable-two-handed-gestures!)
      (gesture-disable-single-joint-gestures!)))


;----------| PROGRAMMER CONTROLS |----------;

;;; PROCEDURE:
;;;   context-print-status
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   prints the current context of the environment
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   NULL
;;; POSTCONDITIONS:
;;;   the current settings are printed as a notification to the log window.
(define context-print-status
   (lambda ()
      (let ((print-note (lambda (str . args)
                           (if (null? args)
                               (print-notification (string->symbol str))
                               (print-notification (string->symbol str) (car args))))))
         (print-note "---------------------------------------------------------")
         (print-note "|   CURRENT SETTINGS   |")
         (print-note "------------------------")
         (if TRACKING
             (print-note "  Gestures are currently being tracked.")
             (print-note "  Gestures are not being tracked."))
         (if PARAMETERS
             (print-note "  Joint information is currently being sent to handlers.")
             (print-note "  Joint information is not being sent to parameters."))
         (print-note "  Currently tracking user" user-id)
         (print-note "  Movement-threshold is:" movement-threshold)
         (print-note "  Z-movement-threshold is:" z-movement-threshold)
         (print-note "  Fast-threshold is:" fast-threshold)
         (print-note "  Slow-threshold is:" slow-threshold)
         (print-note "  Still-threshold is:" still-threshold)
         (print-note "  Simultaneous-treshold is:" simultaneous-threshold)
         (print-note "  Refresh-rate is:" refresh-rate)
         (print-note "---------------------------------------------------------"))))

;;; PROCEDURE:
;;;   context-toggle-parameter-passing! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   turns parameter passing on or off
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   PARAMETERS is state of parameter passing
;;; POSTCONDITIONS:
;;;   switches the state of PARAMETERS
;;;   if PARAMETERS is #t, joint info is passed to handlers
;;;   if PARAMETERS is #f, handlers are called without parameters
(define context-toggle-parameter-passing!
   (lambda ()
      (set! PARAMETERS (not PARAMETERS))
      (if PARAMETERS
          (print-notification 'context-toggle-parameter-passing!:
                              (string->symbol "joint info is now being sent to handlers"))
          (print-notification 'context-toggle-parameter-passing!:
                              (string->symbol "joint info no longer being sent to handlers")))))

;;; PROCEDURE:
;;;   context-set-user! 
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
(define context-set-user!
   (lambda (new-id)
      (set! user-id new-id)))

;;; PROCEDURE:
;;;   context-set-movement-threshold!
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   changes the movement-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side-effects
;;; PRECONDITIONS:
;;;   movement-threshold is the distance a joint must move along the x or y axis
;;;   until its movement is registered
;;; POSTCONDITIONS:
;;;   movement-threshold is now new-threshold
(define context-set-movement-threshold! 
   (lambda (new-threshold)
      (set! movement-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-z-movement-threshold!
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   changes the z-movement-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side-effects
;;; PRECONDITIONS:
;;;   z-movement-threshold is the distance a joint must move along the z axis
;;;   until its movement is registered
;;; POSTCONDITIONS:
;;;   z-movement-threshold is now new-threshold
(define context-set-z-movement-threshold!
   (lambda (new-threshold)
      (set! z-movement-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-fast-threshold! 
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
(define context-set-fast-threshold! 
   (lambda (new-threshold)
      (set! fast-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-slow-threshold! 
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
(define context-set-slow-threshold! 
   (lambda (new-threshold)
      (set! slow-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-still-threshold! 
;;; PARAMETERS:
;;;   new-threshold, an integer
;;; PURPOSE: 
;;;   sets the still-threshold to new-threshold
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   still-threshold is the time it takes for joint to be registered as stopped
;;;   new-threshold should be some fraction of *second* for best results
;;; POSTCONDITIONS:
;;;   still-threshold is now new-threshold
(define context-set-still-threshold!
   (lambda (new-threshold)
      (set! still-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-simultaneous-threshold! 
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
(define context-set-simultaneous-threshold! 
   (lambda (new-threshold)
      (set! simultaneous-threshold new-threshold)))

;;; PROCEDURE:
;;;   context-set-refresh-rate! 
;;; PARAMETERS:
;;;   new-rate, an integer
;;; PURPOSE: 
;;;   sets the refresh-rate to new-rate
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   refresh-rate determines how often ~track-gestures is called 
;;;   new-rate should be some fraction of *second* for best results
;;; POSTCONDITIONS:
;;;   refresh-rate is now new-rate
(define context-set-refresh-rate! 
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
;;;   TRACKING is currently #f
;;; POSTCONDITIONS:
;;;   TRACKING is now #t and gestures are being processed
(define gestures-start!
   (lambda ()
      (set! TRACKING #t)
      (~track-gestures)
      (print-notification 'gestures-start!: (string->symbol "now tracking gestures!"))))

;;; PROCEDURE:
;;;   gestures-stop! 
;;; PARAMETERS:
;;;   NULL
;;; PURPOSE: 
;;;   stops gesture-recognition
;;; PRODUCED: 
;;;   NULL, called for side effects
;;; PRECONDITIONS:
;;;   TRACKING is currently #t
;;; POSTCONDITIONS:
;;;   TRACKING is now #f and gestures are not being processed
(define gestures-stop! 
   (lambda ()
      (set! TRACKING #f)
      (print-notification 'gestures-stop!: (string->symbol "gesture tracking has been stopped."))))