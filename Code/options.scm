

(define (open-options)
  
  ((draw-pixmap option) "splash.jpg" (make-posn 0 0))
  
  (define x-play1 250)
  (define y-play1 322)
  (define x-play2 416)
  (define y-play2 398)
  
  
  
  (define x-help1 162)
  (define y-help1 391)
  (define x-help2 308)
  (define y-help2 471)
  
  
  
  (define l1
    (list (cons 65 241)  (cons 145 219) (cons 164 107) (cons 244 169) (cons 359 235) (cons 440 113) (cons 505 135)))
  
  
  
  (define l2 (list (cons 104 282) (cons 177 245) (cons 202 151) (cons 284 213) (cons 402 276) (cons 476 169) (cons 566 193)))
  
  
  
  (define (myzip l1 l2)
    (if(null? l1) '()
       (cons (cons (car l1) (car l2)) (myzip (cdr l1) (cdr l2)))))
  
  
  
  
  (define (find-number x y)
    (define (helper i l)
      (if (null? l) #f
          (let([fst (car l)])
            (cond[(and  (> x (caar fst)) (< x (cadr fst))
                        (> y (cdar fst)) (< y (cddr fst))) i]
                 [else (helper (+ i 1) (cdr l))]))))
    (helper 0 (myzip l1 l2))) 
  
  (define (clicking-options)
    
    (define (get-true vec)
      (define (helper i)
        (cond[(= i 7) #f]
             [(vector-ref vec i) i]
             [else (helper (+ i 1))]))
      (helper 0))
    
    (let* ([mouse (get-mouse-click option)]
           [mouse-clk (mouse-click-posn mouse)]
           [i (posn-x mouse-clk)]
           [j (posn-y mouse-clk)])
      
      (cond [(and (> i x-play1) (< i x-play2) (> j y-play1) 
                  (< j y-play2) (> no-human-ply -1)  (< no-human-ply 7)) (begin
                                                                           (assign-human-players no-human-ply)
                                                                           (close-viewport option)
                                                                           (draw-board)
                                                                           (play-game hplayer))]
            [(and (> i x-help1) (< i x-help2) (> j y-help1) 
                  (< j y-help2)) ;(> no-human-ply -1)  (< no-human-ply 7))
             (begin
               (open-help))]
            
            
            [(find-number i j) (let([r (find-number i j)])
                                 (begin
                                   ((draw-pixmap option ) (string-append (number->string r) "1.jpg") (make-posn 0 0))
                                   (set! no-human-ply r)
                                   (clicking-options)))]
            [else  (clicking-options)])))
  
  
  (clicking-options))

(define x-back1 455)

(define x-back2 582)

(define y-back1 334)

(define y-back2 393)

(define (open-help)
  (define help-port (open-viewport "Help" (make-posn 600 400)))
  
  (define (help-clicking)
    
    (let* ([mouse (get-mouse-click help-port)]
           [mouse-clk (mouse-click-posn mouse)]
           [k (posn-x mouse-clk)]
           [w (posn-y mouse-clk)])
      
      (if (and (> k x-back1) (< k x-back2) (> w y-back1) 
               (< w y-back2)) 
          (begin 
            (close-viewport help-port)
            (open-options))
          
          (help-clicking))))
  (begin
  ((draw-pixmap help-port) "instruction.jpg" (make-posn 0 0))
  (help-clicking)))



