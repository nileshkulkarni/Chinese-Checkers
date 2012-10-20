(define (best-move-ai-3 player)
  
  (define (max-move mlist val)
      (cond[(null? mlist) val]
           [else (begin
                   (do-many-move (list (car mlist)))
                   (let([r (evaluate-board player)])
                    (undo-many-move (list (car mlist)))
                     (if(> r val) (max-move (cdr mlist) r)
                        (max-move (cdr mlist) val))))]))
    
    (define (max-move-2 mlist move)
      (cond[(null? mlist) move]
           [(> (caar mlist) (car move)) (max-move-2 (cdr mlist) (car mlist))]
           [else (max-move-2 (cdr mlist) move)]))
    
  (let* ([moves (send player generate-all-moves-for-the-set)]
         [mlist 
                 (map (lambda(t) 
                       (begin
                         (do-many-move (list t))
                         (let* ([mlist-2 (send player generate-all-moves-for-the-set)]
                            ;(let* ([mlist (send player generate-all-moves-for-the-set)]
                                [second-move-eval (max-move mlist-2 -1)])
                           (begin
                             (undo-many-move (list t))
                             (cons second-move-eval t))))) moves)])
    
    
                              
                                 
    
   (cdr (max-move-2 mlist (cons 0 0)))))  

                          
                                   
                           
      
   