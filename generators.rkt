#lang racket

(require racket/match)

; randomness
(struct random-gen (s1 s2))

(define (make-random-gen s)
  (if (negative? s)
      (make-random-gen (- s))
      (let ((q (quotient s 2147483562))
	    (s1 (remainder s 2147483562)))
	(let ((s2 (remainder q 2147483398)))
	  (random-gen (+ 1 s1) (+ 1 s2))))))

(define (random-gen-next rg)
  (let ((s1 (random-gen-s1 rg))
	(s2 (random-gen-s2 rg)))

    (let ((k (quotient s1 53668))
	  (k* (quotient s2 52774)))
      (let ((s1*  (- (* 40014 (- s1 (* k 53668)))
		     (* k 12211)))
	    (s2* (- (* 40692 (- s2 (* k* 52774)))
		    (* k* 3791))))
	(let ((s1** (if (negative? s1*)
			(+ s1* 2147483563)
			s1*))
	      (s2** (if (negative? s2*)
			(+ s2* 2147483399)
			s2*)))
	  (let* ((z (- s1** s2**))
		 (z* (if (< z 1)
			 (+ z 2147483562)
			 z)))
	    (cons z* (random-gen s1** s2**))))))))

(define (random-split rg)
  (let ((s1 (random-gen-s1 rg))
	(s2 (random-gen-s2 rg)))
    (let ((new-s1 (if (= s1 2147483562)
		      1
		      (+ s1 1)))
	  (new-s2 (if (= s2 1)
		      2147483398
		      (- s2 1))))
      (let ((nrg (cdr (random-gen-next rg))))
	(cons (random-gen new-s1 (random-gen-s2 nrg))
	      (random-gen (random-gen-s1 nrg) new-s2))))))

(define (random-integer rg low high)
  (let ((b 2147483561)
	(k (+ (- high low) 1)))
    (let loop ((n (ilogbase b k))
	       (acc low)
	       (rg rg))
      (if (zero? n)
	  (cons (+ low (modulo acc k))
		rg)
	  (let ((next (random-gen-next rg)))
	    (loop (- n 1)
		  (+ (car next) (* acc b))
		  (cdr next)))))))

(define (ilogbase b i)
  (if (< i b)
      1
      (+ 1 (ilogbase b (quotient i b)))))

; toy generator
(struct gen (proc))

(define (choose-integer lower upper)
  (gen
   (lambda (r n)
     (car (random-integer r lower upper)))))

(define (fmap-gen f g)
  (gen
   (lambda (r n)
     (f ((gen-proc g) r n)))))

(define (ap-gen g h)
  (gen
   (lambda (r n)
     (let ((split (random-split r)))
       (let ((f ((gen-proc g) (car split) n)))
         ((gen-proc h) (cdr split) n))))))
  
(define (return-gen x)
  (gen (lambda (_r _n) x)))

(define (bind-gen g k) 
  (let ((proc1 (gen-proc g)))
    (gen
     (lambda (rgen size)
       (let ((split (random-split rgen)))
         (let ((g (k (proc1 (car split) size))))
           ((gen-proc g) (cdr split) size)))))))


; frequency :: [(Int, Gen a)] -> Gen a
(define (frequency xs)
  (cond
    [(empty? xs) (error "called on empty list")]
    [(ormap (lambda (x) (< x 0))) (error "weights must be nonnegative")]
    [(andmap (lambda (x) (= x 0))) (error "all weights were zero")]
    [else
     (define tot (apply + (map car xs)))
     (define (pick n xs)
       (match xs
         ['() (error "called on an empty list")]
         [(cons (cons k x) xs)
          (if (<= n k)
              x
              (pick (- n k) xs))]))

     (bind-gen (choose-integer 1 tot) (lambda (g) (pick g xs)))]))


(define (pair-gen g h)
  (ap-gen (fmap-gen cons g) h))

(define (int-pair-gen lo hi)
  (pair-gen (choose-integer lo hi) (choose-integer lo hi)))

(define (generate g) 
  ((gen-proc g) (make-random-gen 0) 30))

; (struct empty ())
; (struct node (l v r))

; (define (tree-gen)
