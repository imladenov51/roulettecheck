#lang roulettecheck/roulette

(define (gen-list lo hi) 
  (lambda (size)
    (for/all ([size size #:exhaustive])
        (cond
        [(zero? size) '()]
        [else (cons (run (choose lo hi) size) (run (gen-list lo hi) (sub1 size)))]))))


(define (all-even-list? l)
  (cond
    [(null? l) #t]
    [else (for/all ([h (car l) #:exhaustive])
            (and (even? h)
                 (all-even-list? (cdr l))))]))


(define (test-generator pred? g size)
  (pred? (run g size)))

(struct leaf () #:transparent)
(struct node (v l r) #:transparent)

(define (gen-bst sz lo hi)
  (for*/all ([sz sz #:exhaustive]
             [lo lo #:exhaustive]
             [hi hi #:exhaustive])
    (cond [(<= sz 0) (pure (leaf))]
          [else
           (frequency
            (list (cons 1 (pure (leaf)))
                  (cons (if (< (- hi lo) 2) 0 sz)
                        (λ (size)
                          (let* ([x (run (choose (add1 lo) (sub1 hi)) size)]
                                 [l (run (gen-bst (sub1 sz) lo x) (sub1 size))]
                                 [r (run (gen-bst (sub1 sz) x hi) (sub1 size))])
                            (node x l r))))))])))

(define (all-even-bst? t)
  (cond
    [(leaf? t) #t]
    [else
     (for/all ([v (node-v t) #:exhaustive])
       (and (even? v)
            (all-even-bst? (node-l t))
            (all-even-bst? (node-r t))))]))

(define (gen-tree sz lo hi)
  (for/all ([sz sz #:exhaustive])
    (cond [(<= sz 0) (pure (leaf))]
          [else
           (frequency
            (list (cons 1 (pure (leaf)))
                  (cons sz 
                        (λ (size)
                          (let* ([x (run (choose (add1 lo) (sub1 hi)) (sub1 size))]
                                 [l (run (gen-tree (sub1 sz) lo hi) (sub1 size))]
                                 [r (run (gen-tree (sub1 sz) lo hi) (sub1 size))])
                            (node x l r))))))])))

(define (is-bst? lo hi t)
  (for/all ([t t #:exhaustive])
    (cond [(leaf? t) #t]
          [else
           (for/all ([v (node-v t)])
             (and (and (< lo v) (< v hi))
                  (is-bst? lo v (node-l t))
                  (is-bst? v hi (node-r t))))])))

(query
 (define t (run (gen-bst 3 0 3) 3))
 (is-bst? 0 3 t))

(define (completeness reference candidate lo hi size)
  (define r (run reference size))
  (define c (run candidate size))
  (define sol
    (parameterize ([current-bitwidth #f])
      (solve
       (assert
        (and (is-bst? lo hi r)
             (forall (symbolics c)
               (not (equal? r c))))))))

  (cond
    [(sat? sol)
     (list 'missing
           (evaluate r
                     (complete-solution sol (symbolics r))))]
    [(unsat? sol) 'complete]
    [else 'unknown]))
