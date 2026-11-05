;(completeness
; gen-naive-term gen-stlc-term 2)

;; Find a single candidate size that covers every well-typed reference output:
;;   exists n >= 0. forall reference choices.
;;     well-typed?(r) => exists candidate choices. r = candidate(n)
;;
;; Size controls recursion, so candidate runs must be built at concrete sizes.
;; Incrementally build sizes 0, 1, ... and use a symbolic integer to select one.
;; Only the generator choices are quantified; candidate-size stays free for solve.
;; Returns the smallest covering size, 'unknown, or '(not-found-up-to limit).
;; Without a limit, the search may run forever if no covering size exists.
(define (completeness2 reference candidate reference-size
                       #:max-size [max-size #f])
  (unless (and (concrete? reference-size)
               (exact-nonnegative-integer? reference-size))
    (raise-argument-error 'completeness2 "concrete natural reference size"
                          reference-size))
  (unless (or (eq? max-size #f)
              (and (concrete? max-size) (exact-nonnegative-integer? max-size)))
    (raise-argument-error 'completeness2 "#f or concrete natural maximum size"
                          max-size))
  (parameterize ([current-bitwidth #f])
    (define r (run reference reference-size))
    (define reference-choices (symbolics r))
    (define typed? (well-typed? r))
    (define-symbolic* candidate-size integer?)
    (define first-candidate (run candidate 0))
    (let search ([bound 0]
                 [c first-candidate]
                 [candidate-choices (symbolics first-candidate)])
      (define sol
        (solve
         (assert
          (and (<= 0 candidate-size bound)
               (forall reference-choices
                       (=> typed?
                           (exists candidate-choices (equal? r c))))))))
      (cond
        [(sat? sol)
         (evaluate candidate-size
                   (complete-solution sol (list candidate-size)))]
        [(unsat? sol)
         (if (and max-size (= bound max-size))
             (list 'not-found-up-to max-size)
             (let* ([next-bound (add1 bound)]
                    [next-candidate (run candidate next-bound)])
               (search next-bound
                       (if (= candidate-size next-bound) next-candidate c)
                       (append candidate-choices (symbolics next-candidate)))))]
        [else 'unknown]))))

;; (completeness2 gen-naive-term gen-stlc-term 2)
;; Limit the search when a covering size might not exist:
;; (completeness2 gen-naive-term gen-stlc-term 2 #:max-size 4)
