#lang roulettecheck/roulette

(require (only-in rosette define-symbolic*))

;; STLC
(struct tbool () #:transparent)
(struct tfun (t1 t2) #:transparent)

(struct lvar (n) #:transparent)
(struct bool (b) #:transparent)
(struct lam (t e) #:transparent)
(struct app (e1 e2) #:transparent)

(define (typecheck ctx e t)
  (let ([t2 (get-typ ctx e)])
    (if t2
        (equal? t t2)
        #f)))


(define (get-typ ctx e)
  (for/all ([e e #:exhaustive])
    (match e
      [(lvar n) (if (and (>= n 0) (< n (length ctx)))
                    (list-ref ctx n)
                    #f)]
      [(bool _) (tbool)]
      [(lam t e) (let ([t2 (get-typ (cons t ctx) e)])
                   (if t2 (tfun t t2) #f))]
      [(app e1 e2)
       (for/all ([ft (get-typ ctx e1) #:exhaustive])
         (match ft
           [(tfun t11 t12)
        (if (equal? t11 (get-typ ctx e2))
                t12 
                #f)]
           [_ #f]))])))

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
      (if (<= size 1)
          (tbool)
          (if (flip 0.5) 
              (tbool)
              (tfun (run gen-typ (quotient size 2))
                    (run gen-typ (quotient size 2))))))))

(define gen-bool
  (λ (_size)
    (flip 1/2)))

(define gen-nat
  (λ (size)
    (run (choose 0 1) size)))

(define (gen-exact-expr ctx t)
  (define (gen-one ctx t)
    (for/all ([t t #:exhaustive])
      (match t
        [(tbool) (fmap bool gen-bool)]
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
      (if (<= n 2)
          (oneof (cons (gen-one ctx t) (gen-var ctx t)))
          (oneof (append (list (gen-one ctx t))
                         (match t
                           [(tfun t1 t2) (list (gen-lam n ctx t1 t2))]
                           [_ '()])
                         (list (gen-app n ctx t))
                         (gen-var ctx t))))))

  (sized (λ (size) (go size ctx t))))

(define gen-stlc-term
  (λ (size)
    (let ([t (run gen-typ size)])
      (run (gen-exact-expr '() t) size))))

(define (gen-naive-expr)
  (define (go sz)
    (for/all ([sz sz #:exhaustive])
      (if (<= sz 0)
          (oneof (list (fmap bool gen-bool) (fmap lvar gen-nat)))
          (frequency
           (list
            (cons 1 (fmap bool gen-bool))
            (cons 1 (fmap lvar gen-nat))
            (cons (sub1 sz)
                  (λ (size)
                    (let* ([e1 (run (go (sub1 sz)) size)]
                           [e2 (run (go (sub1 sz)) size)])
                      (app e1 e2))))
            (cons (sub1 sz)
                  (λ (size)
                    (let* ([t (run gen-typ size)]
                           [e (run (go (sub1 sz)) size)])
                      (lam t e)))))))))
  (sized go)) 

(define gen-naive-term
  (λ (size)
    (run (gen-naive-expr) size)))

(define (step? e)
  (for/all ([e e #:exhaustive])
    (match e
      [(lam _ b) (step? b)]
      [(app e1 e2)
       (for*/all ([e1 e1 #:exhaustive])
         (or (lam? e1) (step? e1) (step? e2)))]
      [_ #f])))

(define (well-typed? e)
  (if (get-typ '() e) #t #f))

(define (completeness reference candidate size size2)
  (define r (run reference size))
  (define c (run candidate size2))

  (define sol
    (parameterize ([current-bitwidth #f])
      (solve
       (assert
        (and (well-typed? r)
             (forall (symbolics c)
                     (not (equal? r c))))))))

  (cond
    [(sat? sol)
     (list 'missing
           (evaluate r
                     (complete-solution sol (symbolics r))))]
    [(unsat? sol) 'complete]
    [else 'unknown]))

