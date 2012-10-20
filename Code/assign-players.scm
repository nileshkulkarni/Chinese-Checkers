

;;;now i am creating a function for placing players


(define (set-comp-players)
  (define (helper i)
    (if (= i 6) (void)
    (if (vector-ref hplayer i) (helper (+ i 1))
        (begin
          (vector-set! cplayer i (vector-ref all-balls i))
          (helper (+ i 1))))))
  (helper 0))

(define (set-human-players)
  (define (helper i)
    (if (= i 6) (void)
    (if (vector-ref cplayer i) (helper (+ i 1))
        (begin
          (vector-set! hplayer i (vector-ref all-balls i))
          (helper (+ i 1))))))
  (helper 0))

(define (assign-human-players n)
  (cond [(= n 0) (begin
                   (set-comp-players))]
        [(= n 1) (begin
                   (vector-set! hplayer 0 (vector-ref all-balls 0))
                   (set-comp-players))]
        [(= n 2) (begin
                   (vector-set! hplayer 0 (vector-ref all-balls 0))
                   (vector-set! hplayer 5 (vector-ref all-balls 5))
                   (set-comp-players))]
        [(= n 3) (begin
                   (vector-set! hplayer 0 (vector-ref all-balls 0))
                   (vector-set! hplayer 3 (vector-ref all-balls 3))
                   (vector-set! hplayer 4 (vector-ref all-balls 4))
                   (set-comp-players))]
        [(= n 4) (begin
                   (vector-set! cplayer 0 (vector-ref all-balls 0))
                   (vector-set! cplayer 5 (vector-ref all-balls 5))
                   (set-human-players))]
        [(= n 5) (begin
                   (vector-set! cplayer 0 (vector-ref all-balls 0))
                   (set-human-players))]
        [(= n 6) (begin
                   (set-human-players))]))
        
        