#lang racket/gui
(require racket/draw)
 (require 2htdp/image)

(define bg "LightCyan")

(define (create-ball color)
  (define target (make-bitmap  30 30))
  (define dc (new bitmap-dc%  [bitmap  target]))

  (send dc set-brush (new brush% [color bg]))

  (send dc set-pen (new pen% [style 'transparent]))
  (send dc set-background bg)
 (send dc set-smoothing 'smoothed)
  (send dc draw-rectangle 0 0 30 30)
  (send dc set-brush (new brush% [color "gray"]))
  (send dc draw-ellipse 2.5 2.5 25 25)
  (send dc set-brush (new brush% [color (symbol->string color)]))
  (send dc draw-ellipse 7.5 7.5 15 15)
  
  (make-object image-snip% target)

  (send target save-file (string-append (symbol->string color) ".png") 'png))

(create-ball 'red)
(create-ball 'blue)
(create-ball 'green)
(create-ball 'orange)
(create-ball 'black)
(create-ball 'yellow)
  

;(define target1 (make-bitmap  30 30))
;(define dc1 (new bitmap-dc%  [bitmap  target1]))
;
;(send dc1 set-brush (new brush% [color "navy"]))
;
;(send dc1 set-pen (new pen% [style 'transparent]))
;
;(send dc1 draw-ellipse 5 5 20 20)
;
;(make-object image-snip% target1)

;(send target1 save-file "blue.png" 'png)



(define target1 (make-bitmap  30 30))
(define dc1 (new bitmap-dc%  [bitmap  target1]))

(send dc1 set-brush (new brush% [color bg]
                                 [style 'solid]))

(send dc1 set-pen (new pen% [color "blue"]
                            [width 1]))
(send dc1 set-background "#f")
(send dc1 set-pen (new pen% [color "blue"]
                            [width 1]
                            [style 'transparent]))
(send dc1 draw-rectangle 0 0 30 30)
(send dc1 set-pen (new pen% [color "black"]
                            [width 3]))
; (send dc1 set-brush (new brush% [color "gray"]))
;  (send dc1 draw-ellipse 2.5 2.5 25 25)
(send dc1 draw-ellipse 5 5 20 20)
(send dc1 set-smoothing 'aligned)
(make-object image-snip% target1)

(send target1 save-file "hole.png" 'png)




(define bg2 "yellow")
(define bg3 "blue")
(define target2 (make-bitmap 50 50))
(define dc3 (new bitmap-dc% [bitmap target2]))

(define (gen-nos i)
 (define img1 (text/font (number->string i)  30	"dark blue"	#f 'modern 'normal 'bold	#f))
  (if (= i 7) (void) 
  (begin
  (let ([img2 (place-image img1 20 20 (rectangle 40 40 "solid" bg2))]
        [img3 (place-image img1 20 20 (rectangle 40 40 "solid" bg3))])
    (begin
      (save-image img2 (string-append (number->string i ) "y.png") 40 40)
      (save-image img3 (string-append (number->string i ) "b.png") 40 40))
      )
  (gen-nos (+ i  1)))))
(gen-nos 0)


(define img3 (place-image  (text/font "Human-Players"  30	"dark blue"	#f 'modern 'normal 'bold #f) 125 50
                          (rectangle 300 100 "solid" bg2)))
(save-image img3 "human-player.png" 250 100)


(define img4 (place-image  (text/font "Computer-Players"  30	"dark blue"	#f 'modern 'normal 'bold #f) 150 50
                          (rectangle 300 100 "solid" bg2)))
(save-image img4 "computer-players.png" 300 100)


(define img5 (place-image  (text/font "Play"  30	"dark blue"	#f 'modern 'normal 'bold #f) 50 50
                          (rectangle 300 100 "solid" bg2)))
(save-image img5 "Play.png" 100 100)







