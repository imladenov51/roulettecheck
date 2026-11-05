#lang racket

(require racket/include
         (only-in text-table print-table)
         "racket.rkt")

(define choose-val choose-sample)
(define flip-val flip-sample)

(include "core.rktl")

(define run/sample run)

;; clanker 
(define/contract (sample g size n)
  (-> gen? exact-nonnegative-integer? exact-positive-integer? void?)
  (define counts (make-hash))
  (for ([_ (in-range n)])
    (hash-update! counts (run g size) add1 0))
  (print-table
   #:row-sep? '(#t #f ...)
   #:->string (list ~v ~a)
   (cons (list "Value" "Probability")
         (for/list ([value (in-list
                           (sort (hash-keys counts) string<?
                                 #:key (λ (value) (format "~v" value))))])
           (list value (/ (hash-ref counts value) n))))))

(provide sample run/sample for/all for*/all
         (all-from-out racket))
