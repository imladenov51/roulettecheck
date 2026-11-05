#lang roulette/example/disrupt

(require (only-in "racket.rkt" choose-sample flip-sample))

(provide choose-val flip-val
         current-generator-mode)

(define current-generator-mode
  (make-parameter 'symbolic))

(define (choose-symb m M)
  (make-categorical
   (for/list ([k (in-inclusive-range m M)])
     (cons k (/ 1 (add1 (- M m)))))))

(define flip-symb flip)

(define (choose-val m M)
  (if (eq? (current-generator-mode) 'sample)
      (choose-sample m M)
      (choose-symb m M)))

(define (flip-val p)
  (if (eq? (current-generator-mode) 'sample)
      (flip-sample p)
      (flip-symb p)))
