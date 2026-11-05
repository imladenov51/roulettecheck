#lang roulettecheck/roulette

(define gen-bit
  (oneof (list (pure 0) (pure 1))))

(query (run gen-bit 0))
