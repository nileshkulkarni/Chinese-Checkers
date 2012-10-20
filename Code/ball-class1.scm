(define ball%
  (class object%
    
    (init ind)
    (init col)
    (define index ind)
    (define color col)
    (define position (get-position-from-index (car index) (cdr index)))
    (define/public (change-pos! new-pos)
      (set! position new-pos))
    ;(define all-poss-moves (get-possible-index))
    (super-new)
    (define/public (check-valid-move given-index) 
      ;returns (cons 'move-invalid 'reason) if move is invalid else (cons 'correct-move '1-step/'2-step)
      (let* ([final-index given-index]
             [iint (car index)]
             [jint (cdr index)]
             [ifin (car final-index)]
             [jfin (cdr final-index)]
             [nhole (2d-vector-ref cur-board ifin jfin)]
             [nstate (send nhole get-state)])
        (cond[(equal? nstate 'invaild) (cons'move-invalid 'position-invalid)]
             [(not (equal? 'null  nstate)) (cons 'move-invalid 'position-already-occupied)]
             
             [(or (= (abs (- iint ifin)) 1) (= (abs (- jint jfin)) 1)) (cons 'correct-move '1-step)]
                  
             [(or (= (abs (- iint ifin)) 2) (= (abs (- jint jfin)) 2))
              (let* ([mid-hole (2d-vector-ref  cur-board
 (quotient (+ iint ifin) 2) (quotient (+ jint jfin) 2))]
                     [mid-state (send mid-hole get-state)])     
               (cond
                 [(equal? mid-state 'null)   (cons 'move-invalid 'midstate-null)]
                 [(equal? mid-state 'invalid) (cons 'move-invalid 'midstate-invalid)]
                 [else (cons 'correct-move '2-step)]))])))
    
    (define/public (get-color) color)
    (define/public (get-index) index)
    (define/public (get-position) position);;it is consed pair
    
    (define/public (index-set! i) 
      (begin
        (send (send cur-board get-elm (car index) (cdr index)) color-set! 'null)
        (send (send cur-board get-elm (car i) (cdr i)) color-set! color)
        (set! index i)
        (set! position (get-position-from-index (car index) (cdr index)))))
    
    ;(define/public (get-possible-index 
    (define/public (get-possible-index)   ;returns all single possible moves(index)
     (define move-list '())
      (define (helper-index ind)
        (let*([i (car ind)]
              [j (cdr ind)]
              [x1 (cons (- i 1) (- j 1))]
              [x2 (cons (- i 1)  j)]
              [x3 (cons  i (+ 1 j))]
              [x4 (cons (+ i 1) (+ j 1))]
              [x5 (cons (+ i 1) j)]
              [x6 (cons i  (- j 1))])
    
        (define l (list x1 x2 x3 x4 x5 x6))
        (set! l (map (λ(x) (if (and (>= (car x) 0) (<= (car x) 16) (>= (cdr x) 0) (<= (cdr x) 16)) x
                               #f)) l))
        (set! l (filter (lambda(x) x) l))  
        (define l1 (map (λ(x) (cons 
                               x 
                               (send (send cur-board get-elm (car x) (cdr x)) get-state ))) l))

        (define (create-valid-lst l)
          (cond [(null? l) '()]
                [(my-assq (caar l) move-list) (create-valid-lst (cdr l))]
                [(equal? (cdar l) 'null) 
                 (begin
                   (set! move-list (cons (cons (caar l) 1)  move-list))
                   (create-valid-lst (cdr l)))] ;1 for single move
                [else (create-valid-lst (cdr l))])) 
                ;[else  
                 
;                 (let* ([i1 (caaar l)];ith index
;                             [j1 (cdaar l)];i index
;                             [x (cons (- (* 2 i1) i) (- (* 2 j1) j))])
;                 (if (and (<= 0 (car x)) (>= 16 (car x)) (<= 0 (cdr x)) (>= 16 (cdr x))) 
;                    (if (equal? 'null (send (send  cur-board get-elm  (car x) (cdr x)) get-state ))
;                       (begin    
;                          (if(not (my-assq x move-list))
;                            (begin
;                             (set! move-list  (cons (cons x 2) move-list)) ;2 for single jump
;                             ;(set! move-list (append (helper-index x) move-list)))
;                             (helper2-index x))
;                            (void))
;                          (create-valid-lst (cdr l)))
;                       (create-valid-lst (cdr l))) 
;                    (void)))]))
;            
            (create-valid-lst l1)))
      
      
      (define (helper2-index ind)
        (let*([i (car ind)]
              [j (cdr ind)]
              [x1 (cons (- i 1) (- j 1))]
              [x2 (cons (- i 1)  j)]
              [x3 (cons  i (+ 1 j))]
              [x4 (cons (+ i 1) (+ j 1))]
              [x5 (cons (+ i 1) j)]
              [x6 (cons i  (- j 1))])
          
        (define l (list x1 x2 x3 x4 x5 x6))
        (set! l (map (λ(x) (if (and (<= 0 (car x)) (>= 16 (car x)) (<= 0 (cdr x)) (>= 16 (cdr x))) x
                               #f)) l))
        (set! l (filter (lambda(x) x) l))  
        (define l2 (map (λ(x) (cons 
                               x 
                               (send (send cur-board get-elm (car x) (cdr x)) get-state ))) l))
          
        (define (create-valid-lst-2 l)
          (cond [(null?  l) '()]
                [(equal? 'null (cdar l)) (create-valid-lst-2 (cdr l))]
                [(equal? 'invalid (cdar l)) (create-valid-lst-2 (cdr l))]
                [else (let* ([i1 (caaar l)];ith index
                             [j1 (cdaar l)];i index
                             [x (cons (- (* 2 i1) i) (- (* 2 j1) j))])
                 (if (and (<= 0 (car x)) (>= 16 (car x)) (<= 0 (cdr x)) (>= 16 (cdr x)))
                     (if (equal? 'null   (send (send  cur-board get-elm  (car x) (cdr x)) get-state ))
                       (begin    
                          (if(not (my-assq x move-list))
                            (begin
                             (set! move-list  (cons (cons x 2) move-list)) ;2 for single jump
                             ;(set! move-list (append (helper-index x) move-list)))
                             (helper2-index x))
                            (void)))
                       (create-valid-lst-2 (cdr l))) (void)))]))
          (create-valid-lst-2 l2)))
  
      (begin
        (helper-index index)
        (helper2-index index)
        move-list)))) 

