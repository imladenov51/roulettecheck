#lang racket

(provide flip choose define-gen for/all for*/all)

(define (flip p)
  (if (or (< p 0) (> p 1))
      (error "Probability should be in [0, 1]")
      (< (random) p)))

(define (choose m M)
  (λ (_size)
    (if (= m M) m (random m (add1 M)))))

(define-syntax-rule (define-gen (name size arg ...) body ...)
  (define (name size arg ...)
    body ...))

(define-syntax-rule (for/all ([x e opt ...] ...) body ...)
  (let ([x e] ...) body ...))

(define-syntax-rule (for*/all ([x e opt ...] ...) body ...)
  (let* ([x e] ...) body ...))
