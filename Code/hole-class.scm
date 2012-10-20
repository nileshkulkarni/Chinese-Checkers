(define hole% 
  (class object%
    (init state)
    (init index)
    (define color state) ;will be either color,invaid,null
    (define ind index)
    
    (define pos (if(equal? color 'invalid) 'invalid
                   (get-position-from-index (car ind) (cdr ind))))
    
    
    (define/public (color-set! x)
      (set! color x))
    
    (define/public (get-state) color)
    (define/public (get-position) pos);;it is consed pair
    (define/public (get-index) ind)
    
    ;(define (draw-image)
      ;draw its bitmap using "state")
   ;   )
    
    (super-new)))
