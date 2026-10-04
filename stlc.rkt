;#lang racket
#lang roulette/example/disrupt

;(require "racket.rkt")
(require "roulette.rkt")


;; run : gen a -> int -> a
(define (run g size)
  (g size))

;; sized : (int -> gen a) -> gen a
(define (sized f)
  (λ (size)
    (run (f size) size)))

;; fmap : (a -> b) -> gen a -> gen b
(define (fmap f g)
  (λ (size)
    (f (run g size))))

;; pure : a -> gen a
(define (pure x)
  (λ (_size) x))

;; ap : gen (a -> b) -> gen a -> gen b
(define (ap gf ga)
  (λ (size)
    ((run gf size) (run ga size))))

(define (oneof gs)
  (λ (size)
    (for/all ([gs gs #:exhaustive])
      (for/all ([i (run (choose 0 (sub1 (length gs))) size) #:exhaustive])
        (run (list-ref gs i) size)))))

(define (elements xs)
  (for/all ([xs xs #:exhaustive])
    (λ (size)
      (for/all ([i (run (choose 0 (sub1 (length xs))) size) #:exhaustive])
        (list-ref xs i)))))

(define (prop-check pred? g size)
  (pred? (run g size)))

(define (prop-check-mc pred? g size)
  (define (go trials ts)
    (if (<= trials 0)
        ts
        (if (prop-check pred? g size)
            (go (sub1 trials) (add1 ts))
            (go (sub1 trials) ts))))
  (let* ([ts (go 10000 0)]
         [ratio (/ ts 10000.0)])
    (displayln
     (format "#t ~a\n#f ~a" 
       (~r ratio #:precision 4) 
       (~r (- 1 ratio) #:precision 4)))))

;; STLC
(struct tbool () #:transparent)
(struct tfun (t1 t2) #:transparent)

(struct lvar (n) #:transparent)
(struct bool (b) #:transparent)
(struct lam (t e) #:transparent)
(struct app (e1 e2) #:transparent)

(define (typecheck ctx e t)
  (let ([t2 (getTyp ctx e)])
    (if t2
        (equal? t t2)
        #f)))

(define (getTyp ctx e)
  (for/all ([e e #:exhaustive])
    (match e
      [(lvar n) (if (and (>= n 0) (< n (length ctx)))
                    (list-ref ctx n)
                    #f)]
      [(bool _) (tbool)]
      [(lam t e) (let ([t2 (getTyp (cons t ctx) e)])
                   (if t2 (tfun t t2) #f))]
      [(app e1 e2)
       (match (getTyp ctx e1)
         [(tfun t11 t12) (let ([t2 (getTyp ctx e2)])
                           (if (equal? t11 t2) t12 #f))]
         [_ #f])])))

(define (shift d e)
  (define (go c e)
    (match e
      [(lvar n) (if (< n c) (lvar n) (lvar (+ n d)))]
      [(bool b) (bool b)]
      [(lam t e) (lam t (go (add1 c) e))]
      [(app e1 e2) (app (go c e1) (go c e2))]))
  (go 0 e))

;; [n -> s]e
(define (subst n s e)
  (match e
    [(lvar m) (if (equal? m n) s (lvar m))]
    [(bool b) (bool b)]
    [(lam t e) (lam t (subst (add1 n) (shift 1 s) e))]
    [(app e1 e2) (app (subst n s e1) (subst n s e2))]))

(define (subst-top s e)
  (shift -1 (subst 0 (shift 1 s) e)))

(define (pstep e)
  (match e
    [(lam t e) (let ([e2 (pstep e)])
                 (if e2 (lam t e2) #f))]
    [(app (lam _ e1) e2)
     (let ([e1 (let ([me1 (pstep e1)])
                 (if me1 me1 e1))]
           [e2 (let ([me2 (pstep e2)])
                 (if me2 me2 e2))])
       (subst-top e2 e1))]
    [(app e1 e2)
     (let ([me1 (pstep e1)]
           [me2 (pstep e2)])
       (if (or me1 me2)
           (let ([e1 (if me1 me1 e1)]
                 [e2 (if me2 me2 e2)])
             (app e1 e2))
           #f))]
    [_ #f]))

(define (multistep fuel step e)
  (if (equal? fuel 0)
      #f
      (let ([e2 (step e)])
        (if e2 (multistep (sub1 fuel) step e2) e))))

(define (is-nf? e)
  (match e
    [(lvar _) #t]
    [(bool _) #t]
    [(lam _ e) (is-nf? e)]
    [(app (lam _ _) _) #f]
    [(app e1 e2) (and (is-nf? e1) (is-nf? e2))]))

(define (size-stlc e)
  (match e
    [(lam _ e) (add1 (size-stlc e))]
    [(app e1 e2) (add1 (+ (size-stlc e1) (size-stlc e2)))]
    [_ 1]))

(define gen-typ
  (λ (size)
    (for/all ([size size #:exhaustive])
      (define (go n)
        (if (<= n 0)
            (pure (tbool))
            (oneof (list (go 0)
                         (λ (size)
                           (let* ([t1 (run (go (quotient n 2)) size)]
                                  [t2 (run (go (quotient n 2)) size)])
                             (tfun t1 t2)))))))

      (run (go size) size))))

(define gen-bool
  (λ (_size)
    (flip 0.5)))

(define (gen-exact-expr ctx t)
  (define (gen-one ctx t)
    (for/all ([t t #:exhaustive])
      (match t
        [(tbool) (fmap (λ (b) (bool b)) gen-bool)]
        [(tfun t1 t2) (fmap (λ (e) (lam t1 e)) (gen-one (cons t1 ctx) t2))])))

  (define (gen-lam n ctx t1 t2)
    (fmap (λ (e) (lam t1 e)) (go (sub1 n) (cons t1 ctx) t2)))
  
  (define (gen-app n ctx t)
    (λ (size)
      (let* ([tt (run gen-typ size)]
             [e1 (run (go (quotient n 2) ctx (tfun tt t)) size)]
             [e2 (run (go (quotient n 2) ctx tt) size)])
        (app e1 e2))))

  (define (gen-var ctx t)
    (let ([vars (filter (λ (i) (equal? (list-ref ctx i) t))
                        (for/list ([i (in-range (length ctx))]) i))])
      (if (null? vars)
          '()
          (list (fmap lvar (elements vars))))))

  (define (go n ctx t)
    (for/all ([t t #:exhaustive])
      (if (<= n 0)
          (oneof (cons (gen-one ctx t) (gen-var ctx t)))
          (oneof (append (list (gen-one ctx t))
                         (match t
                           [(tfun t1 t2) (list (gen-lam n ctx t1 t2))]
                           [_ '()])
                         (list (gen-app n ctx t))
                         (gen-var ctx t))))))


  (sized (λ (size) (go size ctx t))))

;; gen-stlc-term : gen expr
(define gen-stlc-term
  (λ (size)
    (let ([t (run gen-typ size)])
      (run (gen-exact-expr '() t) size))))

(define (step? e)
  (for/all ([e e #:exhaustive])
    (match e
      [(lam _ b) (step? b)]
      [(app e1 e2)
       (for*/all ([e1 e1 #:exhaustive])
         (or (lam? e1) (step? e1) (step? e2)))]
      [_ #f])))

