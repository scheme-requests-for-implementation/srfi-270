;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT

(import (rnrs) (chezscheme) (srfi :270))
(define (r s) (read (open-string-input-port s)))
(define (test str n)
  (let ((number (r str)))
    (unless (eqv? n (r str))
      (display `(,str (,number) != ,n))
      (newline))))

(test "#e#x1.2p3" 9)
(test "#e#x9p9" 4608)
(test "#e#xFE.FFp1" 65279/128)
(test "#e#x-0.Ap-2" -5/32)
(test "#e#x1.9p1+10p1i" 25/8+32i)
(test "#x1.921fb54442d18pd+1" 3.141592653589793116)
(test "#e#xFE.FF"  65279/256)
(test "#x1p5@1.FEEFp6" 32@130799/1024)
(test "#e#x+3.Fp3i" +63/2i)
(test "#e#x3.Fp3+i" 63/2+i)

(define prng (make-pseudo-random-generator))
(define (roll n)
  (pseudo-random-generator-next! prng n))
(define (random-digit)
  (let ((val (roll 16)))
    (values val
            (case val
              ((0) #\0)
              ((#x1) #\1)
              ((#x2) #\2)
              ((#x3) #\3)
              ((#x4) #\4)
              ((#x5) #\5)
              ((#x6) #\6)
              ((#x7) #\7)
              ((#x8) #\8)
              ((#x9) #\9)
              ((#xA) #\A)
              ((#xB) #\B)
              ((#xC) #\C)
              ((#xD) #\D)
              ((#xE) #\E)
              ((#xF) #\F)))))

(define (generate-pair)
  (let* ((mantissa-hex-length (roll 52/4))
         (exponent (- (roll 2048) 1023))
         (sign (if (zero? (roll 2))
                   #\+
                   #\-))
         (fac (* (case sign
                   ((#\+) 1)
                   ((#\-) -1))
                 (expt 2 exponent))))
    (cond
     ((= exponent 1024)    ; no NaN, inf
      (generate-pair))
     (else
      (let loop ((str '())
                 (n (if (= exponent -1023)
                        0
                        1))
                 (i mantissa-hex-length))
        (cond
          ((and (zero? i) (not (= exponent -1023)))
           (values (string-append "#e#x"
                                  (string sign)
                                  "1."
                                  (list->string str)
                                  "p"
                                  (number->string exponent))
                   (* n fac)))
          ((zero? i)    ; zero and subnormal case
           (values (string-append "#e#x"
                                  (string sign)
                                  "0."
                                  (list->string str)
                                  "p"
                                  (number->string exponent))
                    (* n fac)))
          (else
           (let-values (((val dig) (random-digit)))
             (loop (cons dig str)
                   (+ n (* val (expt 16 (* i -1))))
                   (- i 1))))))))))

(display "randomized tests\n")
(do ((i 0 (+ i 1)))
    ((= i 100000))
  (display i)
  (display " ")
  (let-values (((str num) (generate-pair)))
    (test str num)
    (let ((str2 (call-with-string-output-port
                    (lambda (port)
                      (display "#e#x" port)
                      (write-hexadecimal-float num port)))))
      (unless (eqv? (r str) (r str2))
        (display `(,str != ,str2))
        (newline))))
  (display "\r"))

