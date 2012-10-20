(define (show1 x) (let([a x]) (begin (newline) (newline) (display a) (newline) a))) ;remove
(define (next-ai player)
  (let([i (color->index (send player get-color))])
    (vector-ref all-balls (remainder (+ 1 i) 6))))


(define (search-tree player)
  (define (search-tree-h player l depth)
    (if(> depth 3) (leaf l)   ;change the depth to 3 or something
       (let* ([ind (color->index (send player get-color))]
              [all-moves 
               (begin
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
                                            (cons z x))))) (show1 (get-all-leaves search-tree)))) ;remove
  (define (get-max list)
    (define (helper m l)
      (cond [(null? l) m]
            [(< (car m) (car(car l))) (helper  (car l) (cdr l))]
            [else (helper m (cdr l))]))
    (helper (cons 0 0) list))
  
(get-max eval-list))
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