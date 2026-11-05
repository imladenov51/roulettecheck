; core.rktl - generator interface

(provide mk-gen
         gen?
         run
         choose
         flip
         sized
         fmap
         pure
         ap
         oneof
         pick
         frequency
         elements)
;         prop-check
;         prop-check-mc)

(struct gen (f))

(define/contract (mk-gen f)
  (-> (-> integer? any/c) gen?)
  (gen f))

(define/contract (run g size)
  (-> gen? integer? any/c)
  ((gen-f g) size))

(define flip flip-val)

(define/contract (choose m M)
  (-> integer? integer? gen?)
  (when (> m M)
    (error 'choose "lower bound should be <= upper bound"))
  (mk-gen (λ (_size) (choose-val m M))))

(define/contract (sized f)
  (-> (-> integer? gen?) gen?)
  (mk-gen (λ (size) (run (f size) size))))

(define/contract (fmap f g)
  (-> (-> any/c any/c) gen? gen?)
  (mk-gen (λ (size) (f (run g size)))))

(define/contract (pure x)
  (-> any/c gen?)
  (mk-gen (λ (_size) x)))

(define/contract (ap gf ga)
  (-> gen? gen? gen?)
  (mk-gen (λ (size) ((run gf size) (run ga size)))))

(define/contract (oneof gs)
  (-> (listof gen?) gen?)
  (mk-gen (λ (size)
            (for*/all ([gs gs #:exhaustive] [i (run (choose 0 (sub1 (length gs))) size) #:exhaustive])
                      (run (list-ref gs i) size)))))

(define/contract (pick n xs)
  (-> integer? (listof (cons/c integer? gen?)) gen?)
  (for/all ([n n #:exhaustive])
           (match xs
             ['() (error 'pick "called on an empty list")]
             [(cons (cons w g) rest)
              (if (<= n w)
                  g
                  (pick (- n w) rest))])))

(define/contract (frequency xs)
  (-> (listof (cons/c integer? gen?)) gen?)
  (cond
    [(empty? xs) (error 'frequency "called on empty list")]
    [(ormap (lambda (x) (< (car x) 0)) xs) (error 'frequency "weights must be nonnegative")]
    [(andmap (lambda (x) (= (car x) 0)) xs) (error 'frequency "all weights were zero")]
    [else
     (mk-gen (λ (size)
       (let ([tot (apply + (map car xs))]) (run (pick (run (choose 1 tot) size) xs) size))))]))

(define/contract (elements xs)
  (-> (listof any/c) gen?)
  (for/all ([xs xs #:exhaustive])
    (mk-gen (λ (size)
              (for/all ([i (run (choose 0 (sub1 (length xs))) size) #:exhaustive]) (list-ref xs i))))))

;(define (prop-check pred? g size)
;  (pred? (run g size)))

;(define (prop-check-mc pred? g size)
;  (define (go trials ts)
;    (if (<= trials 0)
;        ts
;        (if (prop-check pred? g size)
;            (go (sub1 trials) (add1 ts))
;            (go (sub1 trials) ts))))
;  (let* ([ts (go 10000 0)]
;         [ratio (/ ts 10000.0)])
;    (displayln (format "#t ~a\n#f ~a" (~r ratio #:precision 4) (~r (- 1 ratio) #:precision 4)))))
