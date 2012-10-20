 (define (show1 x) (let([a x]) a)) ;remove

(define (get-n-ind k)
    (cond [(= k 0) 1]
           [(= k 1) 3]
           [(= k 2) 0]
           [(= k 3) 5]
           [(= k 4) 2]
           [(= k 5) 4]))
  
(define (next-ai player)
           
  (let([i (color->index (send player get-color))])
    
    
    (vector-ref all-balls (get-n-ind i))))


(define (search-tree player)
  (define (search-tree-h player l depth)
    (if(> depth 1) (leaf l)   ;change the depth to 3 or something
       (let* ([ind (color->index (send player get-color))]
              [all-moves 
               (begin
                ; (newline)
                 ; (display "*****************************************")        
                 ;(display "reached search tree with move list as ")
                ; (display l)
                ; (newline)
                ; (newline)
                 (do-many-move l)
                 ;(display "crossed do-many-move-l") ;remove
                 (let([x (send player generate-all-moves-for-the-set)]) ;;remove myshow
                   (begin
                     (undo-many-move l)
                     ;(display "croosed undo-many-move-l") ;remove
                   (show1  x))))])  
         (begin 
           ;(set! depth (+ depth 1))
          ;(if(equal? l null) (gnode l (map (lambda(x) (search-tree-h player (cons x l) (+ 1 depth))) all-moves)) 
           (gnode l (map (lambda(y) (search-tree-h (next-ai player) (cons y l) (+ 1 depth))) all-moves))))))
  (search-tree-h player '() 0))




(define (show x) (begin (display x) x))



(define (best-possible-move search-tree player)
  ;(define move (cons 0 0))
;   (define evaluated-tree '())
;    (define (helper tree) 
;    (cond[(leaf? tree)  (begin 
;                          (do-many-move (leaf-moves tree))
;                          (let ([z (evaluate-board player)])
;                            (begin
;                              (undo-many-move (leaf-moves tree))
;                              (if(> z (car move)) (set! move (cons z (leaf-moves tree)))  (void)))))]
;         [else (map (lambda(x) (helper x)) (gnode-move_list tree))])) ;remove this
;  (begin (helper search-tree)
;          move))
;  
  

  
  
  
  (define (concat a)
    (if (null? a)'()
       (cons (car a) (concat (cdr a)))))
  
  (define (get-all-leaves tree)
    (if (leaf? tree) tree
         (flatten (map (lambda(x) (get-all-leaves x)) (gnode-move_list tree)))))
  
  (define  eval-list (map (lambda (x) (begin
                                        (do-many-move (leaf-moves x))
                                        (let ([z (evaluate-board player)])
                                          (begin
                                            (undo-many-move (leaf-moves x))
                                            (cons z x))))) (get-all-leaves search-tree))) ;remove
  (define (get-max list);;;list (cons val movestrcut)
    (define (helper m l)
      (cond [(null? l) m]
            [(< (car m) (caar l)) (helper (car l) (cdr l))]
            [else (helper m (cdr l))]))
    (helper (cons 0 0) list))
  
   (define a  (get-max eval-list))
  
  (define (play-second-best)
    (define (helper l)
      (cond [(null? l) l] 
            [(= (caar l) (car a)) (cdr l)]
            [else (cons (car l) (helper (cdr l)))]))
    (define new-l (helper eval-list))
    new-l)
      
  (define (remove-from-list move l)
    (cond [(null? l) '()]
          [(= (car move) (caar l)) (cdr l)]
          [else (cons (car l) (remove-from-list move (cdr l)))]))
  
  (define (remove-repeated-moves move)
    (let ([a (last-elm-list (leaf-moves (cdr move)))]
          [prev (send player get-previous-2-moves)])
      (define (helper l)
        (cond[(null? l) eval-list]
             [(equal? a (car l)) (remove-from-list move eval-list)] 
             [else (helper (cdr l))]))
      
      (helper prev)))
  
         (if (= (car a) (evaluate-board player))
               (let ([b (play-second-best)])
                 (begin
                   (let ([z (get-max (remove-repeated-moves (get-max b)))]) 
                   (begin
                     (send player previous-move! (last-elm-list (leaf-moves (cdr z))))
                     z)
                   )))
             (let ([c (get-max eval-list)])
                 (begin
                   (let ([z (get-max (remove-repeated-moves c))])
                   (send player previous-move! (last-elm-list (leaf-moves (cdr z)) ))
                     z
                   )))))
; (define evaluated-tree  (map (lambda(x) 
;                            (if (gnode? x) (map (lambda(y) (helper y player)) (gnode-move_list x))
;                                (begin 
;                                  (do-many-move (leaf-moves x))
;                                  (let ([z (evaluate-board player)])
;                                    (begin
;                                      (undo-many-move (leaf-moves x))
;                                      (if(> z (car move)) (set! move (cons z (leaf-moves x)))  (void)))) 
;                              (list search-tree)))
;  
;  (foldr (lambda (fst acc) (if (> (car fst) (car acc)) fst acc)) (cons 0 0) evaluated-tree)
;  
; )
;  
;    
;    
;  
;     