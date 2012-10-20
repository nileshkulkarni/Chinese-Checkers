#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define new1 (open-viewport "Main Screen" (make-posn 600 400)))

((draw-pixmap new1) "instruction.jpg" (make-posn 0 0))


(define m1 (get-mouse-click new1))
(define m2 (get-mouse-click new1))

(define m12 (get-mouse-click-posn m1))

(define m22 (get-mouse-click-posn m2))

(cons (posn-x m12) (posn-y m12))

(cons (posn-x m22) (posn-y m22))