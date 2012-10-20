(define (make-move move);;;(mlist mov3 mov2 mov1)
        (let*([from (move_-from move)]
              [rate (move_-val move)]
              [to (move_-to move)]
              [ball (cdr (search-ball from (move_-player move)))]
              [c (send ball get-color)]
              [f1 (change-a-bit (get-position-from-index (car from) (cdr from)))]
              [t1 (change-a-bit (get-position-from-index (car to) (cdr to)))])
          (begin
            (send ball index-set! to)
            ((draw-pixmap view-port) (string-append (symbol->string c) ".png") t1)
            ((draw-pixmap view-port) "hole.png" f1)
            )))