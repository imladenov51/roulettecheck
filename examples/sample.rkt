#lang roulettecheck/racket

(define gen-bit
  (oneof (list (pure 0) (pure 1))))

(run gen-bit 0)
