#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require (planet clements/rsound))
(provide list)

(define road-color (color 105 105 105 255))
(define road-white (color 255 255 255 255)) 
(define outroad (color 245 222 179 255))
(define outroad2 (color 226 206 169 255))
(define user-angle 0)
(define collision #f)
(define t2 0)
(define collision-speed 0)

(define basic-track1
  (add-curve
   (add-curve
    (add-curve
   (add-curve
    (rectangle 1024 720 "solid" outroad)
    512 50 0 3
    600 360 270 1/16
    (make-pen road-color 100 "solid" "round" "round"))
   600 360 270 1/16
   512 670 180 3
   (make-pen road-color 100 "solid" "round" "round"))
   512 670 180 3
   424 360 90 1/16
   (make-pen road-color 100 "solid" "round" "round"))
   424 360 90 1/16
   512 50 0 3
   (make-pen road-color 100 "solid" "round" "round")))

(define track1(add-line
              (add-line(add-curve
                        (add-curve
                         (add-curve
                          (add-curve
                           basic-track1
                           512 50 0 3
                           600 360 270 1/16
                           (make-pen "white" 8 'long-dash "round" "round"))
                          600 360 270 1/16
                          512 670 180 3
                          (make-pen "white" 8 'long-dash "round" "round"))
                         512 670 180 3
                         424 360 90 1/16
                         (make-pen "white" 8 'long-dash "round" "round"))
                        424 360 90 1/16
                        512 50 0 3
                        (make-pen "white" 8 'long-dash "round" "round"))
                       512 720 512 620
                       (make-pen "white" 10 "solid" "butt" "bevel"))
              452 718 452 618
              (make-pen "white" 10 "solid" "butt" "bevel" )))

(define track track1)



(define basic-track2 (add-curve
                     (add-curve
                      (add-curve
                      (add-curve
                      (rectangle 1024 720 "solid" outroad)
                      512 50 0 0
                      974 360 270 0
                      (make-pen road-color 100 "solid" "round" "round"))
                      974 360 270 0
                      512 670 180 0
                      (make-pen road-color 100 "solid" "round" "round"))
                      512 670 180 0
                      50 360 90 0
                      (make-pen road-color 100 "solid" "round" "round"))
                      50 360 90 0
                      512 50 0 0
                      (make-pen road-color 100 "solid" "round" "round")))

(define track2 (add-line
              (add-line(add-curve
                        (add-curve
                         (add-curve
                          (add-curve
                           basic-track2
                           512 50 0 0
                      974 360 270 0
                      (make-pen "white" 8 'long-dash "round" "round"))
                      974 360 270 0
                      512 670 180 0
                      (make-pen "white" 8 'long-dash "round" "round"))
                      512 670 180 0
                      50 360 90 0
                      (make-pen "white" 8 'long-dash "round" "round"))
                      50 360 90 0
                      512 50 0 0
                      (make-pen "white" 8 'long-dash "round" "round"))
                       512 720 512 608
                       (make-pen "white" 10 "solid" "butt" "bevel"))
                       452 690 452 570
                      (make-pen "white" 10 "solid" "butt" "bevel")))


(define basic-track3
  (add-curve
   (add-curve
    (add-curve 
   (add-curve
    (rectangle 1024 720 "solid" outroad)
    512 50 0  0.3
    974 360 270 0.3
    (make-pen road-color 100 "solid" "round" "round"))
   974 360 270 0
   512 670 180 0
   (make-pen road-color 100 "solid" "round" "round"))
   512 670 180 3
   424 360 90 1/16 
   (make-pen road-color 100 "solid" "round" "round"))
   424 360 90 1/16
   512 50 0 3
   (make-pen road-color 100 "solid" "round" "round")))

(define track3(add-line
              (add-line(add-curve
                        (add-curve
                         (add-curve
                          (add-curve
                           basic-track3
                           512 50 0 0.3
                           974 360 270 0.3
                           (make-pen "white" 8 'long-dash "round" "round"))
                          974 360 270 0
                          512 670 180 0
                          (make-pen "white" 8 'long-dash "round" "round"))
                         512 670 180 3
                         424 360 90 1/16
                         (make-pen "white" 8 'long-dash "round" "round"))
                        424 360 90 1/16
                        512 50 0 3
                        (make-pen "white" 8 'long-dash "round" "round"))
                       512 720 512 610
                       (make-pen "white" 10 "solid" "butt" "bevel"))
              452 718 452 618
              (make-pen "white" 10 "solid" "butt" "bevel" )))




(define image-pixel (image->color-list track1))

(define (get-pixel-color x y)
  (let* [(value(+ x (* 1024 y)))]
    (if (or (< x 0)(> x 1024) (> y 718) (< y 0)) outroad
    (list-ref image-pixel value))))


(define user-car (bitmap "user.png"))

(define (keep-distance x y angle unit)
  (if (equal? (get-pixel-color (inexact->exact (floor (+ x (* (cos angle) unit)))) (inexact->exact (floor (- y (* (sin angle) unit))))) outroad)
       #t #f))

(define (inside x y)
      (let*[(cos-angle(/ (- x 512) (sqrt (+ (* (- y 360) (- y 360)) (* (- x 512) (- x 512))))))
            (sin-angle(/ (- 360 y) (sqrt (+ (* (- y 360) (- y 360)) (* (- x 512) (- x 512))))))
            (x (get-pixel-color (inexact->exact (floor (+ x (* cos-angle 55)))) (inexact->exact (floor (- y (* sin-angle  55))))))]
        (or (equal? x road-color) (equal? x road-white))))
    

(define centre-coordinate (list 482 690 (- 90) 0))
(define alpha 0.6435011)
(define me-point1 (cons 0 0))
(define me-point2 (cons 0 0))
(define me-point3 (cons 0 0))
(define me-point4 (cons 0 0))
(define comp-speed 0)
(define user-speed 0)
(define comp-angle 90)

(define (keep-usercar-going lis)

(let*[(current-state( car (cdr lis)))]
  (begin (set! centre-coordinate current-state)
  (let*[(celement1(car lis))
        (celement2(car(cdr lis)))
        (cstate(car(cdr(cdr lis))))
        (cangle(car(cdr(cdr cstate))))
        (cx (car cstate))
        (cy (car (cdr cstate)))
        (dis 10 )
        (cspeed(car(cdr(cdr(cdr cstate)))))
        (cpoint1 (cons (+ cx (* 25 (cos (+ alpha (* (/ pi 180) (+ 90 cangle)))))) (- cy (* 25 (sin (+ alpha (* (/ pi 180) (+ 90 cangle))))))))
        (cpoint2 (cons (+ cx (* 25 (cos (- (* (/ pi 180)(+ 90 cangle)) alpha)))) (- cy (* 25 (sin (- (* (/ pi 180) (+ 90 cangle)) alpha))))))
        (cpoint3 (cons (- cx (* 25 (cos (- (* (/ pi 180) (- (+ 180 cangle) 90)) alpha)))) (+ cy (* 25 (sin (- (* (/ pi 180) (- (+ 180 cangle) 90)) alpha))))))
        (cpoint4 (cons (- cx (* 25 (cos (+ (* (/ pi 180) (- (+ 180 cangle) 90)) alpha)))) (+ cy (* 25 (sin (+ (* (/ pi 180) (- (+ 180 cangle) 90)) alpha) )))))]
            (define (equation1 x y)
        (- (- y (cdr cpoint1)) (/ (* (- x (car cpoint1)) (- (cdr cpoint1) (cdr cpoint3))) (- (car cpoint1) (car cpoint3)))))
    (define (equation2 x y)
        (- (- y (cdr cpoint2)) (/ (* (- x (car cpoint2)) (- (cdr cpoint2) (cdr cpoint4))) (- (car cpoint2) (car cpoint4)))))
    (define (equation3 x y)
        (- (- y (cdr cpoint1)) (/ (* (- x (car cpoint1)) (- (cdr cpoint1) (cdr cpoint2))) (- (car cpoint1) (car cpoint2)))))
    (define (equation4 x y)
        (- (- y (cdr cpoint3)) (/ (* (- x (car cpoint3)) (- (cdr cpoint3) (cdr cpoint4))) (- (car cpoint3) (car cpoint4)))))
   (define (keep-distance1 x y unit)
  (if (< (* (equation3 x y) (equation4 x y)) 7) #t #f))
 
     (define (keep-distance3 x y unit)
  (if (< (* (equation1 x y) (equation2 x y)) 7)

       #t #f))
                   (let*[(angle(car(cdr(cdr current-state))))
        (x (car centre-coordinate))
        (y (car (cdr centre-coordinate)))
        (speed(car(cdr(cdr(cdr centre-coordinate)))))
        (point1 (cons (+ x (* 25 (cos (+ alpha (* (/ pi 180) (+ 90 angle)))))) (- y (* 25 (sin (+ alpha (* (/ pi 180)
                                                                                                    (+ 90 angle))))))))
        (point2 (cons (+ x (* 25 (cos (- (* (/ pi 180)(+ 90 angle)) alpha)))) (- y (* 25 (sin (- (* (/ pi 180)
                                                                                              (+ 90 angle)) alpha))))))
        (point3 (cons (- x (* 25 (cos (-(* (/ pi 180) (- (+ 180 angle) 90)) alpha) ))) (+ y (* 25 (sin (-(* (/ pi 180)
                                                                                           (- (+ 180 angle) 90)) alpha))))))
        (point4 (cons (- x (* 25 (cos (+(* (/ pi 180) (- (+ 180 angle) 90)) alpha) ))) (+ y (* 25 (sin (+(* (/ pi 180) (- (+ 180 angle) 90)) alpha) )))))
                                                                                    
        (cond1(keep-distance (car point1) (cdr point1) (* (/ pi 180) (+ 90 angle)) 3))
        (cond2(keep-distance (car point2) (cdr point2) (* (/ pi 180) (+ 90 angle)) 3))
        (cond3(keep-distance (car point3) (cdr point3) (* (/ pi 180) (+ 90 angle)) (- 3)))
        (cond4(keep-distance (car point4) (cdr point4) (* (/ pi 180) (+ 90 angle)) (- 3)))
        (con1(keep-distance1 (car point1) (cdr point1) 7))
        (con3(keep-distance3 (car point1) (cdr point1)  7))
        (con11(keep-distance1 (car point2) (cdr point2) 7))
        (con31(keep-distance3 (car point2) (cdr point2)  7))
        (con12(keep-distance1 (car point3) (cdr point3) 7))
        (con32(keep-distance3 (car point3) (cdr point3)  7))
        (con13(keep-distance1 (car point4) (cdr point4) 7))
        (con33(keep-distance3 (car point4) (cdr point4)  7))]

     (define (equation11 x y)
      (- (- y (cdr point1)) (/ (* (- x (car point1)) (- (cdr point1) (cdr point3))) (- (car point1) (car point3)))))
    (define (equation12 x y)
        (- (- y (cdr point2)) (/ (* (- x (car point2)) (- (cdr point2) (cdr point4))) (- (car point2) (car point4)))))
    (define (equation13 x y)
        (- (- y (cdr point1)) (/ (* (- x (car point1)) (- (cdr point1) (cdr point2))) (- (car point1) (car point2)))))
    (define (equation14 x y)
        (- (- y (cdr point3)) (/ (* (- x (car point3)) (- (cdr point3) (cdr point4))) (- (car point3) (car point4)))))

  (define (keep-distance11 x y unit)
  (if (< (* (equation13 x y) (equation14 x y)) 7) 
       #t #f))
                     
   (define (keep-distance13 x y unit)
  (< (* (equation11 x y) (equation12 x y)) 7))
       
        (let*[(con5(keep-distance11 (car cpoint1) (cdr cpoint1) 5))
              (con6(keep-distance13 (car cpoint1) (cdr cpoint1) 5))
              (con51(keep-distance11 (car cpoint2) (cdr cpoint2) 5))
              (con61(keep-distance13 (car cpoint2) (cdr cpoint2) 5))
              (con52(keep-distance11 (car cpoint3) (cdr cpoint3) 5))
              (con62(keep-distance13 (car cpoint3) (cdr cpoint3) 5))
              (con53(keep-distance11 (car cpoint4) (cdr cpoint4) 5))
              (con63(keep-distance13 (car cpoint4) (cdr cpoint4) 5))
              (u speed)]
                     
    (begin (set! me-point1 point1) (set! me-point2 point2) (set! me-point3 point3) (set! me-point4 point4) (set! user-angle angle) (set! user-speed speed)
           (cond[(and (> y 500) (< x 480) (> x 455)) (replace lis 2 (list 482 690 90 0))]
                [ (or (and con1 con3) (and con11 con31) (and con12 con32) (and con13 con33) (and con5 con6)
                      (and con51 con61) (and con52 con62) (and con53 con63)) (if (= u 0) (replace lis 2 (list x y angle (- (/ u 5))))
                                                                                 (begin  (cond [(> (car lis) (+ t2 1)) (begin (play snare) (set! t2 (car lis)))])
                                                                                   (set! collision #t) (set! collision-speed  (* 2 u) )
                                                                                                                                (replace lis 2 (list (- x (* (sin (* (/ pi 180) angle )) (- speed)))  (- y (* (cos (* (/ pi 180) angle )) (- speed))) angle (-  (/ u  3) )))))]
                [(> speed 0) (if (or cond1 cond2) (replace lis 2 (list x y angle 0))
                                 (replace lis 2 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle speed)))]
                [else (if (or cond3 cond4) (replace lis 2 (list x y angle 0))
                                 (replace lis 2 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle speed)))]))))))))
               
               
 (define (add-3-to-state lis key)
  (let*[(current-state( car (cdr lis)))
       (angle(car(cdr(cdr current-state))))
        (x (car current-state))
        (y (car (cdr current-state)))
        (speed (car(cdr(cdr(cdr current-state)))))]
        (cond [(key=? key "up") (if (< speed 6)  (replace lis 2 (list x y angle (+ speed 0.3))) lis)]
        [(key=? key "down")(if (> speed (- 5)) (replace lis 2 (list x y angle (- speed 0.3))) lis)]
        [(key=? key "left") (replace lis 2 (list x y (+ angle 8) speed))]
        [(key=? key "right") (replace lis 2 (list x y (- angle 8) speed))]
         [else lis])))

(define comp-car1 (bitmap "comp.png"))


(define (computer lis)
  (let*[(element1(car lis))
        (element2(car(cdr lis)))
        (state(car(cdr(cdr lis))))
        (angle(car(cdr(cdr state))))

        (x (car state))
        (y (car (cdr state)))
        (speed(car(cdr(cdr(cdr state)))))
        (point1 (cons (+ x (* 25 (cos (+ alpha (* (/ pi 180) (+ 90 angle)))))) (- y (* 25 (sin (+ alpha (* (/ pi 180) (+ 90 angle))))))))
        (point2 (cons (+ x (* 25 (cos (- (* (/ pi 180)(+ 90 angle)) alpha)))) (- y (* 25 (sin (- (* (/ pi 180) (+ 90 angle)) alpha))))))
        (point3 (cons (- x (* 25 (cos (- (* (/ pi 180) (- (+ 180 angle) 90)) alpha)))) (+ y (* 25 (sin (- (* (/ pi 180) (- (+ 180 angle) 90)) alpha))))))
        (point4 (cons (- x (* 25 (cos (+ (* (/ pi 180) (- (+ 180 angle) 90)) alpha)))) (+ y (* 25 (sin (+ (* (/ pi 180) (- (+ 180 angle) 90)) alpha) )))))
         (centre-distance (sqrt (+ (*(- (car centre-coordinate) x) (- (car centre-coordinate) x)) (*(- (car (cdr centre-coordinate)) y) (- (car (cdr centre-coordinate)) y)))))                                                                               
        (cond1(keep-distance (car point1) (cdr point1) (* (/ pi 180) (+ 90 angle)) 2))
        (cond2(keep-distance (car point2) (cdr point2) (* (/ pi 180) (+ 90 angle)) 2))
        (cond3(keep-distance (car point3) (cdr point3) (* (/ pi 180)  (+ 90 angle)) (- 2)))
        (cond4(keep-distance (car point4) (cdr point2) (* (/ pi 180) (+ 90 angle)) (- 2)))]
    
    (define (equation1 x y)
      (- (- y (cdr point1)) (/ (* (- x (car point1)) (- (cdr point1) (cdr point3))) (- (car point1) (car point3)))))
    (define (equation2 x y)
        (- (- y (cdr point2)) (/ (* (- x (car point2)) (- (cdr point2) (cdr point4))) (- (car point2) (car point4)))))
    (define (equation3 x y)
        (- (- y (cdr point1)) (/ (* (- x (car point1)) (- (cdr point1) (cdr point2))) (- (car point1) (car point2)))))
    (define (equation4 x y)
        (- (- y (cdr point3)) (/ (* (- x (car point3)) (- (cdr point3) (cdr point4))) (- (car point3) (car point4)))))
    (define (forward x1 y1 x2 y2)
      (cond[(and (> x2 512) (< x1 512)) #t]
           [(and (< x2 512) (> x1 512)) #f]
           [(and(> x1 512) (> x2 512)) ( if (< y1 y2) #t (if (and (< (abs (- y1 y2)) 32) (< (abs (- x1 x2)) 42) (< centre-distance 55)) #t #f))]
           [else (if (> y1 y2) #t (if (and(< (abs (- y1 y2)) 32) (< (abs (- x1 x2)) 42) (< centre-distance 55)) #t #f))]))
    
    
    (let*[(cond5(<= (* (equation1 (car me-point1) (cdr me-point1)) (equation2 (car me-point1) (cdr me-point1))) 0))
          (cond6(<= (* (equation1 (car me-point2) (cdr me-point2)) (equation2 (car me-point2) (cdr me-point2))) 0))
          (cond7(<= (* (equation1 (car me-point3) (cdr me-point3)) (equation2 (car me-point3) (cdr me-point3))) 0))
          (cond8(<= (* (equation1 (car me-point4) (cdr me-point4)) (equation2 (car me-point4) (cdr me-point4))) 0))
          (cond11(<= (* (equation1 (car centre-coordinate) (car(cdr centre-coordinate))) (equation2 (car centre-coordinate) (car(cdr centre-coordinate))))  0))
          (cond9 (forward (car centre-coordinate) (car(cdr centre-coordinate)) x y)) 
          (cond10(inside x y))]
          
   (begin (set! comp-speed speed) (set! comp-angle angle)
          (cond[collision  (if (< collision-speed 1) (begin (set! collision #f) lis) (if (or cond1 cond2 cond3 cond4) (begin  (set! collision #f) (replace lis 3 (list x y angle 0)))
                          (begin (set! collision-speed (- collision-speed 0.5)) (replace lis 3 (list (- x (* (sin (* (/ pi 180) user-angle )) collision-speed))  (- y (* (cos (* (/ pi 180) user-angle )) collision-speed)) angle 0)))))]
           [(< speed 0) (replace lis 3 (list x y angle 0))]
           [(and (not(= speed 0)) (or cond5 cond6 cond7 cond8 cond11) (< centre-distance  80) (or cond9 cond11 ))
            (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle 0))] 
           [(and cond1 (> speed 0)) (if cond10 (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) 0.4))  (- y (* (cos (* (/ pi 180) angle )) 0.4)) (- angle 8) speed))
                                        (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) 0.4))  (- y (* (cos (* (/ pi 180) angle )) 0.4)) (+ angle 8) speed)))]
           [(and cond2 (> speed 0)) (if cond10 (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) 0.4))  (- y (* (cos (* (/ pi 180) angle )) 0.4)) (- angle 8) speed))
                                        (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) 0.4))  (- y (* (cos (* (/ pi 180) angle )) 0.4)) (+ angle 8) speed)))]
           [(and cond7 cond9 (< centre-distance  150)) (if (keep-distance (car me-point3) (cdr me-point3) (* (/ pi 180) (+ 180 angle)) 35) (replace lis 3
                                                                           (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) (- angle 3)  speed ))
                                  (replace lis 3 (list x y (+ angle 2) speed)))]
           [(and cond8 cond9 (< centre-distance  150)) (if (keep-distance (car me-point4) (cdr me-point4) (* (/ pi 180) angle) 35) (replace lis 3
                                                                           (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) (+ angle 3)  speed ))
                                  (replace lis 3 (list x y (- angle 2) speed)))]
           [(and cond5 cond9 (< centre-distance  150)) (if (keep-distance (car me-point3) (cdr me-point3) (* (/ pi 180) (+ 180 angle)) 35) (replace lis 3
                                                                           (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) (- angle 3) speed ))
                                  (replace lis 3 (list x y (+ angle 2) speed)))]
           [(and cond6 cond9 (< centre-distance  150)) (if (keep-distance (car me-point4) (cdr me-point4) (* (/ pi 180) angle) 35) (replace lis 3
                                                                           (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) (+ angle 3) speed ))
                                  (replace lis 3 (list x y (- angle 2) speed)))]
           [else (if (> speed 6) (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle 6))
                   (if (< speed 6) (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle (+ speed 0.05)))
                       (replace lis 3 (list (- x (* (sin (* (/ pi 180) angle )) speed))  (- y (* (cos (* (/ pi 180) angle )) speed)) angle speed))))])))))


(define (replace l k x)
  (if (= k 1) (cons x (cdr l)) (cons (car l) (replace (cdr l) (- k 1) x))))

(define (draw-a-car-onto-an-empty-scene lis)
  (let*[(user-state (car (cdr lis)))
        (comp-state (car(cdr(cdr lis))))]
   (place-image (rotate (car(cdr(cdr user-state))) user-car)
                                                             (car user-state) (car(cdr user-state))
              
      (place-image (rotate (car(cdr(cdr comp-state))) comp-car1)
                                                            (car comp-state) (car(cdr comp-state))
               track))))

(define (collaborate lis)
  (keep-usercar-going
   (computer (timer lis))))

(define (timer cs)
  (list (+ (car cs) (/ 1 28)) (cadr cs) (caddr cs) ))

(define l (cons 0 (cons '(482 690 -90 0) (cons '(482 650 -90 0)'()))))

(define (end-game cs)
  (if (or (and (< 452 (caadr cs)) (> 462 (caadr cs)) (< 512 (cadadr cs))) (and (< 452 (caaddr cs)) (> 462 (caaddr cs)) (< 512 (car (cdaddr cs))))) #t #f))

(define (times cs)
  (cond [(and (< 452 (caadr cs)) (> 462 (caadr cs))) (place-image (text "Your Time:          s" 40 "indigo") 502 420
                                                      (place-image (text (number->string (exact->inexact (/ (floor (* 100 (car cs))) 100))) 40 "indigo")
                                                                   587 420
                                                                  (place-image (text "You Win" 40 "indigo") 512 360 (empty-scene 1024 720))))]
        [(and (< 452 (caaddr cs)) (> 462 (caaddr cs))) (place-image (text "You Lose" 40 "indigo") 512 360 (empty-scene 1024 720))]))

(define (main1)
  (begin (set! track track1)
         (set! image-pixel (image->color-list track1))
  (big-bang l
    (on-tick collaborate)
    ;(on-tick timer)
    (on-key add-3-to-state)
    (to-draw draw-a-car-onto-an-empty-scene)
    (stop-when end-game times)
    (close-on-stop 3))))

(define (main2)
  (begin (set! track track2)
         (set! image-pixel (image->color-list track2))
  (big-bang l
    (on-tick collaborate)
    ;(on-tick timer)
    (on-key add-3-to-state)
    (to-draw draw-a-car-onto-an-empty-scene)
    (stop-when end-game times)
    (close-on-stop 3))))

(define (main3)
  (begin (set! track track3)
         (set! image-pixel (image->color-list track3))
  (big-bang l
    (on-tick collaborate)
    ;(on-tick timer)
    (on-key add-3-to-state)
    (to-draw draw-a-car-onto-an-empty-scene)
    (stop-when end-game times)
    (close-on-stop 3))))


(define (start-screen cs) (place-image (text "Race On" 120 "red") 512 150
                                       (place-image (text "Press 3 for Third Track" 40 "indigo") 512 420
                                       (place-image (text "Press 2 for Second Track" 40 "indigo") 512 360
                                       (place-image (text "Press 1 for First Track" 40 "indigo") 512 300 (rectangle 1024 720 "solid" "yellow"))))))
          
(define (start state key)
  (cond [(key=? key "1") (play (rs-read "step_out_gert.wav" ))
         (main1)]
        [(key=? key "2") (play (rs-read "step_out_gert.wav" ))
         (main2)]
        [(key=? key "3") (play (rs-read "step_out_gert.wav" ))
         (main3)]))

(big-bang l
  (to-draw start-screen)
  (on-key start))
;(stop-when give-true))
