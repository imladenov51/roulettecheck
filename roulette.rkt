#lang roulette/example/disrupt

(provide choose) 

(define (choose m M)
  (when (> m M)
    (error 'choose "lower bound should be <= upper bound"))
  (λ (_size)
    (make-categorical
    (for/list ([k (in-inclusive-range m M)])
;        (cons k (/ 1.0 (add1 (- M m))))))))
        (cons k (/ 1 (add1 (- M m))))))))
