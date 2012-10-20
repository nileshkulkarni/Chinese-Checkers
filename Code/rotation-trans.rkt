

;(define (f x y)
;  (cons
;     (+ (* x (/ 2(sqrt 14))) (* y (/ 3 (sqrt 14))))
;     (- (+ (* y (/ 2 (sqrt 14))) (* x (- (/ 3 (sqrt 14))))))))


(define (r i j) ;red ko red
  (cons
   i 
   j))


(define (g i j) ;green ko red
  (cons
   (modulo (- 16 j) 17) 
   (modulo (- (+ 8 i) j) 17)))


(define (b i j) ;black ko red
  (cons 
   (modulo (- (+ 8 j) i) 17)
   (modulo (- 16 i) 17)))

(define (bl i j) ;blue ko red
  (cons 
   (modulo (- 16 i) 17)
   (modulo (- 16 j) 17)))

(define (y i j)    ;yellow to red
  (cons
   (modulo (- (+ 8 i) j) 17)
   (modulo i 17)))
  
  
(define (o i j)   ;orange  to red
  (cons
   (modulo j 17) 
   (modulo  (- (+ 8 j) i) 17)))
  







