#lang roulette/example/disrupt

(provide (struct-out gen)
         return
         bind
         choose-integer
         generate
         inference)

;; proc : size -> probabilistic value
(struct gen (proc))

(define (run g n)
  ((gen-proc g) n))

(define (fmap f g)
  (gen (lambda (n) (f (run g n)))))

(define (pure x)
  (gen (lambda (_n) x)))

(define (ap g h)
  (gen (lambda (n) ((run g n) (run h n)))))

(define (return x)
  (gen (lambda (_n) x)))

(define (bind g k)
  (gen (lambda (n) (run (k (run g n)) n))))

(define (choose min max)
  (make-categorical (for/list ([x (in-inclusive-range min max)])))
)

(define (choose-integer lo hi)
  (gen (lambda (_n)
         (for*/all ([lo lo] [hi hi])
           (define weight (/ 1 (add1 (- hi lo))))
           (make-categorical (for/list ([x (in-inclusive-range lo hi)])
                   (cons x weight)))))))

(define (pair-gen g h)
  (bind g (lambda (a) (bind h (lambda (b) (return (cons a b)))))))

; frequency :: [(Int, Gen a)] -> Gen a
(define (frequency xs)
  (cond
    [(empty? xs) (error "called on empty list")]
    [(ormap (lambda (x) (< (car x) 0)) xs) (error "weights must be nonnegative")]
    [(andmap (lambda (x) (= (car x) 0)) xs) (error "all weights were zero")]
    [else
     (define tot (apply + (map car xs)))
     (define (pick n xs)
       (for/all ([n n #:exhaustive])
                (match xs
                  ['() (error "called on an empty list")]
                  [(cons (cons k x) xs)
                   (if (<= n k)
                       x
                       (pick (- n k) xs))])))

     (bind (choose-integer 1 tot) (lambda (g) (pick g xs)))]))

; gen a -> io a (jk)
(define (draw-sample g)
  (for ([size (in-inclusive-range 0 20 2)])
    (display (generate g size))
    (newline)))

;; (int -> gen a) -> gen a
(define (sized f)
  (gen (lambda (n) (run (f n) n))))

(struct node (l v r) #:transparent)
(struct leaf () #:transparent)

(define (tree-gen)
  (lambda (size)
    (if (<= size 0)
        (return (leaf))
        (frequency (list (cons 1 (return (leaf)))
                         (cons 7
                               (bind ((tree-gen) (- size 1))
                                     (lambda (l)
                                       (bind ((tree-gen) (- size 1))
                                             (lambda (r) (return (node l '() r))))))))))))

(define (list-gen g)
  (lambda (size)
    (if (<= size 0)
        (return '())
        (bind g (lambda (x)
                  (bind ((list-gen g) (- size 1))
                        (lambda (xs)
                          (return (cons x xs)))))))))
     
(define (inference g [size 3])
  (query (run g size)))

(define (generate g [size 5])
  (sample (run g size)))

(define (sample-one g)
  (define result #f)
  (query (set! result (sample (run g 1))))
  result)

(define (fast-list-gen g length)
  (for/list ([_ (in-range length)])
    (sample-one g)))

