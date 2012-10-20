

(define (myshow x) (begin (newline) (newline) (newline) (display x) (newline)(newline) x))
(define (last-elm-list l)
  (cond[(equal? l '()) (error "list is empty")]
       [(equal? (cdr l) '()) (car l)]
       [else (last-elm-list (cdr l))]))

;(define chance (open-viewport "Player Play" 100 100))
(define (play-game vec);;vec is the hplayer vector;
 ; (random-seed (abs (current-milliseconds)))
  (define i (random 6))
  
;  (define (next) 
;    (begin
;      (set! i (remainder (+ i 1) 6))
;      (newline)
;      (display i)   ;remove display
;      (newline)
;      i))
  
  (define (helper-computer player i);;player is an object of class ball-set
    (begin   ;remove
      ;(send player get-color)
      ;((draw-solid-rectangle  chance) (make-posn 0 0)  100 100  "white" )
     ; ((draw-string  view-port) (make-posn 20 50) "Current  Player")
      ((draw-pixmap view-port)  (string-append (symbol->string (send player get-color))".png") (make-posn 50 70) )

      ;((draw-string chance) (make-posn 20 20) (symbol->string (send player get-color)))
      (let* ([search_tree (search-tree player)]   ;remove show
             [y (begin 
                  ;(display search_tree) 
                  ;(display (send (move_-player (car (gnode-moves_list (car (gnode-move_list search_tree))))) get-color))
                  (best-possible-move search_tree player))] ;remove show
             [x (last-elm-list (leaf-moves (cdr y)) )])
        (begin
        ;  (display "wtf")
          ;(display (send player get-color)) ;remove this
          (make-move x)
          (let([t (remainder (+ i 1) 6)])
            (if (game-win (color->index (send player get-color))) (begin
                               ((draw-pixmap view-port) (string-append (symbol->string (send player get-color)) ".png") (make-posn 850 50))                               
                               ((draw-string view-port) (make-posn 800 80) "    Wins the game")
                               )
                
                (if(vector-ref vec t) (begin
                                        (helper-human (vector-ref vec t) t))
                   
                   (helper-computer (vector-ref cplayer t) t))))))))
  
  
  (define (helper-human player i);;
    
    (define (clicking) 
      (begin
        (myshow (send player get-color))
        (let* ([mouse-clk (get-mouse-click view-port)]
               [mouse (mouse-click-posn mouse-clk)])
          (let ([x (find-ball (posn-x mouse) (posn-y mouse))])
            (cond [(not x) (clicking)]
                  ;[(right-mouse-click? mouse-clk) (right-mouse x)]   ;;;left mouse click implies the jump is not complete yet
                  [(equal? 'null (send x get-state)) (clicking)]
                  [(left-mouse-click? mouse-clk) (left-mouse x)] ;;
                  )))))
    
    (define (show x ) (begin (display x) x))
    
    (define (left-mouse hole)
      (let* ([mouse-clk (get-mouse-click view-port)]
             [mouse (mouse-click-posn mouse-clk)])
        (let ([x (find-ball (posn-x mouse) (posn-y mouse))])
          (cond [(not x) (left-mouse hole)]
                [(or (right-mouse-click? mouse-clk) (not (equal? (send x get-state) 'null))) (clicking)] 
                ;[(left-mouse-click? mouse-clk) (right-mouse hole x)]
                [(left-mouse-click? mouse-clk) (if (equal? (send x get-state)'null);;;remember to put an if here
                                                   ;for the if the hole doesnot contain a ball
                                                   (let* ([ball (get-ball hole)])
                                                     (if ball
                                                         (if (equal? (send ball get-color) (send (vector-ref hplayer i) get-color))
                                                             (let* ([moves (send ball get-possible-index)]
                                                                    [s (my-assq (send x get-index) moves)])
                                                               (begin
                                                                 (display moves)
                                                                 (newline)
                                                                 (if (not s) (clicking)
                                                                     (begin
                                                                       (send ball index-set! (send x get-index))
                                                                       (let ([col (send ball get-color)]
                                                                             [posin (change-a-bit(send hole get-position))]
                                                                             [posfin (change-a-bit(send x get-position))])
                                                                         (begin
                                                                           (newline)
                                                                           (display i)   ;remove all this nonsense
                                                                           (newline)     
                                                                           (display "i hai ye")
                                                                           
                                                                           
                                                                           ((draw-pixmap view-port) "hole.png" posin)
                                                                           ((draw-pixmap view-port) (string-append (symbol->string col) ".png") posfin)
                                                                           (let ([t (remainder (+ i 1) 6)])
                                                                             (if (game-win i) (begin
                                                                                               ((draw-pixmap view-port) (string-append (symbol->string (send player get-color)) "1.jpg") (make-posn 0 0))
                                                                                               )
                                                                                 (if (vector-ref hplayer (myshow t)) (helper-human (vector-ref hplayer t) t)
                                                                                     (helper-computer    (vector-ref cplayer t) t)))))
                                                                         (clicking))))))
                                                             (clicking))
                                                         (clicking)))
                                                   (clicking))]))))
    (begin)
    ((draw-pixmap view-port)  (string-append (symbol->string (send player get-color))".png") (make-posn 50 70) )
    (clicking))
  
  
  
  
  (begin
    
    (if(vector-ref vec i) (helper-human (vector-ref hplayer i) i)
     (helper-computer (vector-ref cplayer i) i))))