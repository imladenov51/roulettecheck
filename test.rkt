#lang roulette/example/disrupt


(define (choose min max)
    (make-categorical (for/list ([x (in-inclusive-range min max)])
                      (cons x (/ 1 (+ (- max min) 1))))))

(define (vector-gen size)
  (if (<= size 0)
      '()
      (cons (choose 0 1) (vector-gen (- size 1)) )))

(define (list-gen lo hi)
  (vector-gen (choose lo hi)))
 
  
