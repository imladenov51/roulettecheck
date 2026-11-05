#lang roulettecheck/roulette

(define gen-nat
  (λ (size) (run (choose 0 size) size)))
   
(define gen-list
  (λ (size)
    (if (<= size 0)
        '()
        (run (oneof (list (pure '())
                    (λ (size)
                      (for*/all ([x (run gen-nat size)]
                             [xs (run gen-list (sub1 size))])
                        (cons x xs)))))
             size))))

(define (gen-list-aux ub)
  (λ (size)
    (if (<= size 0)
        '()
        (run
         (oneof
          (list (pure '())
                (λ (size)
                  (let* ([x (run gen-nat ub)]
                         [xs (run (gen-list-aux ub) (sub1 size))])
                  (cons x xs)))))
         size))))

(define gen-list-complete
  (λ (size)
    (run (gen-list-aux size) size)))

(define (completeness reference candidate size)
  (define r (run reference size))
  (define c (run candidate size))
  (define sol
    (parameterize ([current-bitwidth #f])
      (solve
       (assert
        (forall (symbolics c)
                (not (equal? r c)))))))
  (cond
    [(sat? sol)
     (list 'missing
           (evaluate r
                     (complete-solution sol (symbolics r))))]
    [(unsat? sol) 'complete]
    [else 'unknown]))

;(completeness gen-list-complete gen-list 20)

;(completeness gen-list-complete gen-list-complete 2)

(define (prob x)
  (query
    (define target (run gen-list-complete 2))
    (define hit-probability
        ((query
        (equal? target (run gen-list 2)))
        #t))
    (zero? hit-probability)))
