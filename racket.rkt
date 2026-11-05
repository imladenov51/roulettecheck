#lang racket

(provide flip-sample choose-sample for/all for*/all)

(define (flip-sample p)
  (if (or (< p 0) (> p 1))
      (error "Probability should be in [0, 1]")
      (< (random) p)))

(define (choose-sample m M)
  (if (= m M) m (random m (add1 M))))

(define-syntax-rule (define-gen (name size arg ...) body ...)
  (define (name size arg ...)
    body ...))

(define-syntax-rule (for/all ([x e opt ...] ...) body ...)
  (let ([x e] ...) body ...))

(define-syntax-rule (for*/all ([x e opt ...] ...) body ...)
  (let* ([x e] ...) body ...))
