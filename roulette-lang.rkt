#lang roulette/example/disrupt

(require racket/include
         "roulette.rkt")

(include "core.rktl")

(define/contract (run/sample g size)
  (-> gen? integer? any/c)
  (parameterize ([current-generator-mode 'sample])
    (run g size)))

(provide run/sample
         (all-from-out roulette/example/disrupt))
