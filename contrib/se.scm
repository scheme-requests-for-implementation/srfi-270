(import (srfi 143) (srfi 144))

; decode-flonum, if called with a finite non-nan flonum, returns 3 values:
; (1) mantissa as a list of 1s and 0s of length [0..precision-bits] 
;    (length is less than precision-bits for subnormals, including 0 which has length 0; nonempty
;     lists start with 1 for both normals and subnormals)
; (2) binary exponent, such that if (1) is taken as an exact integer and multiplied by 2^exp, 
;     we get abs of the original x 
; (3) sign as 1.0 or -1.0

(define (decode-flonum x)
  (define-values (fr exp) (flnormalized-fraction-exponent x)) ; aka frexp
  (let loop ([x (* (flabs fr) 2.0)] [n exp] [up 1.0] [dn (flabs x)] [l '()])
    (if (or (fl=? up (fl+ up 1.0)) (flzero? dn))
        (values (reverse l) n (flcopysign 1.0 fr)) 
        (let* ([tx (fltruncate x)] [rx (fl- x tx)])
          (loop (* rx 2.0) (- n 1) (fl* up 2.0) (fl* dn 0.5) (cons (if (flzero? tx) 0 1) l))))))

; tests

(equal? (call-with-values (lambda () (decode-flonum 1234356789.725)) list)
'((1 0 0 1 0 0 1 1 0 0 1 0 0 1 0 1 1 0 0 1 0 1 0 0 0 1 1 0 1 0 1 1 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0) -22 1.0))
(equal? (call-with-values (lambda () (decode-flonum 12.75)) list)
'((1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) -49 1.0))
(equal? (call-with-values (lambda () (decode-flonum (expt 2.0 -1070))) list)
'((1 0 0 0 0) -1074 1.0))
(equal? (call-with-values (lambda () (decode-flonum (expt 2.0 -1072))) list)
'((1 0 0) -1074 1.0))
(equal? (call-with-values (lambda () (decode-flonum (expt 2.0 -1074))) list)
'((1) -1074 1.0))
(equal? (call-with-values (lambda () (decode-flonum (expt 2.0 -1075))) list)
'(() 0 1.0))
(equal? (call-with-values (lambda () (decode-flonum 0.0)) list)
'(() 0 1.0))
(equal? (call-with-values (lambda () (decode-flonum -0.0)) list)
'(() 0 -1.0))
(equal? (call-with-values (lambda () (decode-flonum (* 1.23 (expt 2.0 -1060)))) list)
'((1 0 0 1 1 1 0 1 0 1 1 1 0 0 0) -1074 1.0))

