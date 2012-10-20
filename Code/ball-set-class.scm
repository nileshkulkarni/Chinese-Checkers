(define ball-set%
  (class object%
    
    (init col)
    (define color col)
    (define comp-col (get-comp-color color)) 
    (define bindex (color->index color))
    (define compind (color->index comp-col))
    (super-new)
    (define target (vector-map (lambda(x)  (send x get-index)) (build-vector 10 (lambda (i) (send cur-board get-ball  compind i)))))
    (define ball-vector (build-vector 10 (lambda (i) (send cur-board get-ball  bindex i))))
    (define/public (get-color) color)
    (define/public (get-all-balls) ball-vector)
    (define/public (get-ball  i)
      (vector-ref ball-vector i))
    (define previous-moves '()) 
    
    (define/public (check-for-game-win-or-lose)
      
        (define (check-all-places l)
          (cond [(null? l) #t]
              [(equal? (car l) color) (check-all-places (cdr l))]
              [else #f]))      
        (let ([l (vector->list (vector-map (lambda(x) (send (send cur-board get-elm (car x) (cdr x)) get-state)) target ))])
          (check-all-places l)))
    
    (define/public (previous-move! move)
      (if (< (length previous-moves) 3) (cons move previous-moves)
          (set! previous-moves (append (car previous-moves) (list move)))))
    
           
           
    (define/public (get-previous-2-moves)
      previous-moves)
    
    (define/public (generate-all-moves-for-the-set)
      (define nums (build-vector 10(lambda (i) i)));;vector of nos 0 to 9;;basically indexes
      
      (define (evaluate-for-best-of-a-ball index)
      (let* ([b (vector-ref ball-vector index)]
             [bindex (send b get-index)]
             [l (send b get-possible-index)]
             [l1 (map (lambda (x) (move_  bindex (car x) (get-distance bindex (car x)) this)) l)])
        (helper2 l1)))
    
     (define (helper2 l1)
      (foldr (lambda(x res) (if (> (move_-val x) (move_-val res)) x res))
             (move_ '() '() 0 (vector-ref all-balls bindex)) ;(vector-re....replaces this
                l1))
      
       
      
    (define (get-distance from to)
      (let ([ifrom (car from)]
            [jfrom (cdr from)]
            [ito   (car to)]
            [jto   (cdr to)])
        (+  (* ( - ito ifrom) ( - ito ifrom)) (* ( - jto jfrom) ( - jto jfrom)))))
      
      
      (define (helper-1 ind) ;;returns the best move a ball can play (move_rate  from to val)
         (evaluate-for-best-of-a-ball ind))
        
      (define (helper-2);;;rerurns all the moves a set of balls can play ;;ie available to a player.vector of such 
        (let ([l2 (vector-map (lambda(x) (helper-1 x)) nums)])                          ; form(cons (moves ball1) 1)          
           (vector-filter (lambda (x) (not (null? (move_-from x)))) l2)))
      
      (vector->list (helper-2)))
      
))
