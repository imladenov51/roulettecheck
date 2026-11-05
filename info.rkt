#lang info

(define collection "roulettecheck")
(define deps '("base" "roulette" "text-table"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Shared generators for Racket sampling and Roulette inference")
(define version "0.1")

;; These existing experiments are not library modules. Leave them untouched
;; and runnable directly without making installation or package tests run them.
(define compile-omit-paths
  '("stlc.rkt" "lists.rkt" "bst.rkt"
    "generators.rkt" "roulettegen.rkt" "test.rkt"))
(define test-omit-paths
  '("stlc.rkt" "lists.rkt" "bst.rkt"
    "generators.rkt" "roulettegen.rkt" "test.rkt"))
