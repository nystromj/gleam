; troubleshoot
; get a way to change direction to still
; load onto git
; divide into folders
; document
; make a list of gestures
; start defining gestures


(define x 0)
(define y 1)
(define z 2)

(define make-axis
   (lambda () 
      (vector 0
              0
              (make-vector 100 (cons 0 0)))))

(define make-joint
   (lambda ()
      (vector (make-axis)
              (make-axis)
              (make-axis))))
      
(define left (make-joint))

(vector-ref (vector-ref left z) 2)

(define movement-threshold 0.05)
(define fast-threshold .15)
(define slow-threshold .01)

(io:osc:start-server 7110)
(define (io:osc:receive timestamp address . args)
   (if (string=? (list-ref args 0) "l_hand")
       (cond ((movement? left x (list-ref args 2))
              (store-pos! left x (list-ref args 2)))
             ((movement? left y (list-ref args 3))
              (store-pos! left y (list-ref args 3)))
             ((movement? left z (list-ref args 4))
              (store-pos! left z (list-ref args 4))))))

(define movement? 
   (lambda (joint axis new-pos)
      (> (abs (- (/ (get-current-pos joint axis) 100) new-pos)) movement-threshold)))

(store-pos! left y .32)
(movement? left y .29)

; works
(define increment-counter! 
   (lambda (joint axis)
      (if (equal? (get-counter joint axis) 99)
          (set-counter! joint axis 0)
          (set-counter! joint axis (+ (get-counter joint axis) 1)))))

; works
(define set-counter! 
   (lambda (joint axis counter)
      (vector-set! (vector-ref left y) 0 counter)))

; works
(define get-previous-counter 
   (lambda (joint axis)
      (if (equal? (get-counter joint axis) 0)
          99
          (- (get-counter joint axis) 1))))

;works
(define get-previous-pos
   (lambda (joint axis)
      (car (vector-ref (vector-ref (vector-ref joint axis) 2) 
                       (get-previous-counter joint axis)))))

;works
(define get-previous-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                        (get-previous-counter joint axis)))))

; works
(define get-counter 
   (lambda (joint axis)
      (vector-ref (vector-ref joint axis) 0)))

;works
(define get-current-pos
   (lambda (joint axis)
      (car (vector-ref  (vector-ref (vector-ref joint axis) 2)
                        (get-counter joint axis)))))

;works
(define get-current-time
   (lambda (joint axis)
      (cdr (vector-ref (vector-ref (vector-ref joint axis) 2)
                       (get-counter joint axis)))))

;works
(define store-pos! 
   (lambda (joint axis new-data)
      (increment-counter! joint axis)      
      (vector-set! (vector-ref (vector-ref joint axis) 2)
                   (get-counter joint axis) 
                   (cons (* 100 new-data) (now)))
     (track-changes joint axis)
      ))

(define set-direction! 
   (lambda (joint axis direction)
      (vector-set! (vector-ref joint axis) 1 direction)))

(define get-direction
   (lambda (joint axis)
       (vector-ref (vector-ref joint axis) 1)))

(define direction-changed?
   (lambda (joint axis)
      (not (same-sign? (get-direction joint axis) (calculate-direction joint axis)))))

(define same-sign? 
   (lambda (x y)
      (if (and (equal? x 0) (equal? y 0))
          #t
          (> (abs (+ x y)) (abs (- x y))))))
     
(define calculate-speed
   (lambda (joint axis)
     (/ (calculate-direction joint axis) 
           (/ (- (get-current-time joint axis) (get-previous-time joint axis)) 100))))

(define calculate-direction
   (lambda (joint axis)
      (- (get-current-pos joint axis) (get-previous-pos joint axis))))

(define joint-up? 
   (lambda (joint)
      (< (calculate-direction joint y) 0)))

(define joint-down? 
   (lambda (joint)
      (not (joint-up? joint))))

(define joint-right? 
   (lambda (joint)
      (< (calculate-direction joint x) 0)))

(define joint-left? 
   (lambda (joint)
      (not (joint-right? joint))))

(define joint-forward?
   (lambda (joint)
      (< (calculate-direction joint z) 0)))

(define joint-backward?
   (lambda (joint)
      (not (joint-forward? joint))))

(define movement-fast? 
   (lambda (speed)
      (>= (abs speed) fast-threshold)))

(define movement-slow? 
   (lambda (speed)
      (< (abs speed) slow-threshold)))

(define track-changes
   (lambda (joint axis)
      (cond ((direction-changed? joint axis)
             (set-direction! joint axis (calculate-direction joint axis))
             (cond ((equal? axis x)
                    (if (joint-right? joint)
                        (print "left hand is now moving right")
                        (print "left hand is now moving left")))
                   ((equal? axis y)
                    (if (joint-up? joint)
                        (print "left hand is now moving up")
                        (print "left hand is now moving down")))
                   ((equal? axis z)
                    (if (joint-forward? joint)
                        (print "left hand is now moving forward")
                        (print "left hand is now moving backward"))))))))
                 
            