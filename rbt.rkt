#lang roulettecheck/roulette

(struct leaf () #:transparent)
(struct node (c v l r) #:transparent)

(define (color? c)
  (or (equal? c 'r) (equal? c 'b)))

(define (tree? t)
  (match t
    [(leaf) #t]
    [(node c _ l r) (and (color? c) (tree? l) (tree? r))]
    [_ #f]))

(define (bst? lo hi t)
  (for/all ([t t #:exhaustive])
   (cond
     [(leaf? t) #t]
     [else
      (for/all ([v (node-v t)])
        (and (and (<= lo v) (<= v hi)) (bst? lo v (node-l t)) (bst? v hi (node-r t))))])))

(define/contract (consistent-black-height? t)
  (-> tree? boolean?)
  (define (is-black? c)
    (if (equal? c 'b) 1 0))
  (define (go t)
    (for/all ([t t #:exhaustive])
      (match t
        [(leaf) (cons #t 1)]
        [(node c _ l r)
          (for*/all ([lbh (go l) #:exhaustive]
                     [rbh (go r) #:exhaustive])
            (cons (and (and (car lbh) (car rbh)) (= (cdr lbh) (cdr rbh)))
                       (+ (cdr lbh) (is-black? c))))])))
  (car (go t)))

(define/contract (no-red-red? t)
  (-> tree? boolean?)
  (define/match (black-root? t)
    [((node 'r _ _ _)) #f]
    [(_) #t])
  (match t
    [(leaf) #t]
    [(node 'b _ l r) (and (no-red-red? l) (no-red-red? r))]
    [(node 'r _ l r) (and (black-root? l) (black-root? r) (no-red-red? l) (no-red-red? r))]))

(define/contract (rbt? lo hi t)
  (-> integer? integer? tree? boolean?)
  (and (bst? lo hi t) (consistent-black-height? t) (no-red-red? t)))

(define/contract (insert x t)
  (-> integer? tree? tree?)
  (define (ins t)
    (match t
      [(leaf) (node 'r x (leaf) (leaf))]
      [(node c y l r)
       (cond
         [(< x y) (balance c y (ins l) r)]
         [(= x y) (node c y l r)]
         [(> x y) (balance c y l (ins r))])]))
  (define/match (make-black t)
    [((node _ y l r)) (node 'b y l r)])
  (make-black (ins t)))

(define/contract (balance c y l r)
  (-> color? any/c tree? tree? tree?)
  (match* (c y l r)
    [('b z (node 'r y (node 'r x a b) c) d) (node 'r y (node 'b x a b) (node 'b z c d))]
    [('b z (node 'r x a (node 'r y b c)) d) (node 'r y (node 'b x a b) (node 'b z c d))]
    [('b x a (node 'r z (node 'r y b c) d)) (node 'r y (node 'b x a b) (node 'b z c d))]
    [('b x a (node 'r y b (node 'r z c d))) (node 'r y (node 'b x a b) (node 'b z c d))]
    [(c y l r) (node c y l r)]))

(define/contract (gen-color)
  (-> gen?)
  (mk-gen (λ (_size) (if (flip 1/2) 'r 'b))))

(define/contract (gen-list lo hi)
  (-> integer? integer? gen?)
  (mk-gen (λ (size)
    (for/all ([size size #:exhaustive])
      (cond [(zero? size) '()]
            [else (cons (run (choose lo hi) size) (run (gen-list lo hi) (sub1 size)))])))))

(define/contract (gen-rbt lo hi)
  (-> integer? integer? gen?)
  (mk-gen (λ (size)
            (let ([xs (run (gen-list lo hi) size)])
              (foldr (λ (x t) (insert x t)) (leaf) xs)))))

(define (insert-arb x t)
  (for/all ([t t #:exhaustive])
    (match t
      [(leaf)
       (node (run (gen-color) size) x (leaf) (leaf))]
      [(node c y l r)
       (if (flip 1/2)
         (node c y (insert-arb x l) r)
         (node c y l (insert-arb x r)))])))

(define/contract (gen-tree lo hi)
  (-> integer? integer? gen?)
  (mk-gen
   (λ (size)
     (let ([xs (run (gen-list lo hi)
                    (run (choose 0 size) size))])
       (foldr insert-arb (leaf) xs)))))

(define/contract (gen-rbt-naive lo hi fuel)
  (-> integer? integer? integer? gen?)
  (mk-gen
   (λ (size)
     (if (= fuel 0)
         (leaf)
         (let ([t (run (gen-tree lo hi))])
           (if (rbt? t)
               t
               (gen-rbt-naive lo hi (sub1 fuel))))))))


      


