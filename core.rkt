;#lang racket
#lang roulette/example/disrupt

;(require "racket.rkt")
(require "roulette.rkt")


;; run : gen a -> int -> a
(define (run g size)
  (g size))

;; pure : a -> gen a
(define (pure x)
  (lambda (_size) x))

;; ap : gen (a -> b) -> gen a -> gen b
(define (ap gf ga)
  (lambda (size)
    ((run gf size) (run ga size))))

;; pick : int -> (int * gen a) list -> gen a
(define (pick n xs)
  (for/all ([n n #:exhaustive])
    (match xs
        ['() (error 'pick "called on an empty list")]
        [(cons (cons w g) rest) (if (<= n w) g (pick (- n w) rest))])))

;; (listof (cons weight gen)) -> gen
(define (frequency xs)
  (cond
    [(empty? xs) (error 'frequency "called on empty list")]
    [(ormap (lambda (x) (< (car x) 0)) xs)
     (error 'frequency "weights must be nonnegative")]
    [(andmap (lambda (x) (= (car x) 0)) xs)
     (error 'frequency "all weights were zero")]
    [else
     (lambda (size)
       (let ([tot (apply + (map car xs))])
         ;(run (pick (choose 1 tot) xs) size)))]))
         (run (pick (run (choose 1 tot) size) xs) size)))]))

(define (gen-list lo hi) 
  (lambda (size)
    (for/all ([size size #:exhaustive])
        (cond
        [(zero? size) '()]
        ;[else (cons (choose lo hi) (run (gen-list lo hi) (sub1 size)))]))))
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

(define (gen-bst lo hi)
  (lambda (size)
    (for*/all ([lo lo #:exhaustive]
               [hi hi #:exhaustive]
               [size size #:exhaustive])
      (cond [(<= size 0) (leaf)]
            [else
             (run (frequency
                   (list (cons 1 (pure (leaf)))
                         (cons (if (< (- hi lo) 2) 0 size)
                               (lambda (size)
                                 (let* (;[x (choose (add1 lo) (sub1 hi))]
                                        [x (run (choose (add1 lo) (sub1 hi)) size)]
                                        [l (run (gen-bst lo x) (sub1 size))]
                                        [r (run (gen-bst x hi) (sub1 size))])
                                   (node x l r))))))
                  size)]))))

(define (all-even-bst? t)
  (cond
    [(leaf? t) #t]
    [else (for/all ([v (node-v t) #:exhaustive])
            (and (even? v)
                (all-even-bst? (node-l t))
                (all-even-bst? (node-r t))))]))

(define (all-even-bst-new? t)
  (cond
    [(leaf? t) #t]
    [else (let* ([ev (for/all ([v (node-v t) #:exhaustive]) (even? v))]
                 [l (all-even-bst-new? (node-l t))]
                 [r (all-even-bst-new? (node-r t))])
            (and ev l r))]))

(define (gen-bst-new lo hi)
  (lambda (size)
    (if (<= size 0)
        (leaf)
        (let* ([nodew (for*/all ([lo lo #:exhaustive] [hi hi #:exhaustive])
                        (if (< (- hi lo) 2)
                           (cons #t 0)
                           ;(cons (flip (/ 1.0 (add1 size)))
                           ;      (choose (add1 lo) (sub1 hi)))))])
                           (cons (flip (/ 1 (add1 size)))
                                 (run (choose (add1 lo) (sub1 hi)) size))))])
          (if (car nodew)
              (leaf)
              (let* ([x (cdr nodew)]
                     [l (run (gen-bst-new lo x) (sub1 size))]
                     [r (run (gen-bst-new x hi) (sub1 size))])
                (node x l r)))))))

