(library (srfi :270)
  (export write-hexadecimal-float)
  (import (rnrs))

  (define (to-hex-digit n)
    (case n
     ((#x0) #\0)
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
     ((#xF) #\F)))

  (define (decode-float fv)
    (let ((bv (make-bytevector 8)))
      (bytevector-ieee-double-set! bv 0 fv (endianness big))
      (let ((sign? (fxbit-set? (bytevector-u8-ref bv 0) 7))
            (exponent (fxior
                       (fxarithmetic-shift-left
                        (fxand (bytevector-u8-ref bv 0) #x7F)
                        4)
                       (fxarithmetic-shift-right
                        (bytevector-u8-ref bv 1)
                        4)))
            (mantissa (do ((i 2 (fx+ i 1))
                           (m (fxand (bytevector-u8-ref bv 1) #xF)
                              (fxior
                               (fxarithmetic-shift-left m 8)
                               (bytevector-u8-ref bv i))))
                          ((fx=? i 8) m))))
        (values mantissa (- exponent 1023) (if sign? -1 1)))))

  (define write-hexadecimal-float
    (case-lambda
      ((n) (write-hexadecimal-float n (current-output-port)))
      ((n port)
       (cond
         ((nan? n) (write "+nan.0" port))
         ((equal? n +inf.0) (write "+inf.0" port))
         ((equal? n -inf.0) (write "-inf.0" port))
         ((and (complex? n) (not (real? n)))
          (write-hexadecimal-float (real-part n) port)
          (unless (negative? (imag-part n))
            (display "+" port))
          (write-hexadecimal-float (imag-part n) port)
          (display "i"))
         ((inexact? n)    ; Assuming flonum
          (let-values (((m e sign) (decode-float n)))
            (when (negative? sign)
              (display "-" port))
            (if (<= e -1023)
                (display "0." port)
                (display "1." port))
            (do ((l '() 
                    (let ((n (mod m #x10)))
                      (if (and (null? l) (zero? n))
                          l
                          (cons (to-hex-digit n) l))))
                 (i 0 (fx+ i 1))
                 (m m (div m #x10)))
                ((= i 52/4)
                 (display (list->string l) port)))
            (display "p" port)
            (display e port)))
         ((exact? n) (write-hexadecimal-float (inexact n) port))
         (else (assertion-violation 'write-hexadecimal-float
                                    "not a number"
                                    n)))))))
