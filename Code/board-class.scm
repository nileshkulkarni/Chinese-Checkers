 
(define board%
  (class object%
    (define balls (make-vector-2d 6 10 #\_))
    (define board-star 
      (build-2d-vector 17 17 (lambda(i j) 
                               (let([r (obtain-color i j)])
                                 (begin
                                   (if(not (or (equal? r 'null) (equal? r 'invalid)))
            
                                      (2d-vector-set! balls 
                                                      (color->index r) 
                                                      (vector-member #\_  (vector-ref balls (color->index r)))
                                                      (new ball% [ind (cons i j)]
                                                                 [col r]))
                                      (void))
                                   (new hole%    [state (obtain-color i j)]
                                        [index (cons i j)]))))))
    
    ;(define (get-board-status pos))
    (define/public (get-elm x y)
      (2d-vector-ref board-star x y))


     (define/public (get-row x)
      (vector-ref board-star x))
       
    (define/public (get-ball x y)
      (2d-vector-ref balls x y))
    
    
    (super-new)
    ))
