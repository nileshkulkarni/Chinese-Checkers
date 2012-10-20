(define ball-set%
  (class object%
    
    (init col)
    (define color col)
    
    (define bindex (color->index color))
    (super-new)
    
    (define ball-vector (build-vector 10 (lambda (i) (send cur-board get-ball  bindex i))))
    (define/public (get-color) color)
    (define/public (get-all-balls) ball-vector)
    (define/public (get-ball  i)
      (vector-ref ball-vector i))
    
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
             (move_ '() '() 0)
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
      
      (helper-2))
      
))
