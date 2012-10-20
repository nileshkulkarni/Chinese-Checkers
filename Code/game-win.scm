(define (game-win j)
  (define evals (evaluate-board (vector-ref all-balls j)))
  (if (> evals 9000) #t
      #f))
