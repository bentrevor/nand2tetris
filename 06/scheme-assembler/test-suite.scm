(define (list=? xs ys)
  (cond ((and (null? xs) (null? ys))
         #t)
        ((or (null? xs) (null? ys))
         #f)
        ((= (car xs) (car ys))
         (list=? (cdr xs) (cdr ys)))
        (else #f)))

(define (type-of x)
  (cond ((number? x) "number")
        ((string? x) "string")
        ((char? x)   "char")
        ((list? x)   "list")))

(define (print-success msg)
  (print (string-append subject " " msg) ":.....\x1b[32mok\x1b[0m\n"))

(define (raise-error msg)
  (error (string-append "\x1b[31mfailure: \x1b[0m " subject " " msg)))

(define (assert msg bool)
  (if tests-enabled?
      (if bool
          (print-success msg)
          (raise-error msg))))

(define (assert-eq msg x y)
  (define print-compare (cond ((or (number? x) (string? x) (char? x))
                               (lambda (a b)
                                 (print "\n\nx was: " a "\n")
                                 (print "y was: " b "\n")))
                              (else (lambda (as bs)
                                      (print "\n\n  x was: ( ")
                                      (map (lambda (a) (print a) (print ", "))
                                           as)
                                      (print " )\n  y was: ( ")
                                      (map (lambda (b) (print b) (print ", "))
                                           bs)
                                      (print " )\n\n")))))

  (define eq-type-and-val? (cond ((and (number? x) (number? y)) (lambda (x y) (< (abs (- x y)) 0.0001)))
                                 ((and (string? x) (string? y)) string=?)
                                 ((and (list? x) (list? y))     equal?)
                                 ((and (char? x) (char? y))     eq?)

                                 (else (begin (print "\nmust be the same type:\n")
                                              (print "type of x: " (type-of x) "\n")
                                              (print "type of y: " (type-of y) "\n")
                                              (lambda (a b) #f)))))

  (if tests-enabled?
      (if (eq-type-and-val? x y)
          (print-success msg)
          (begin
            (print-compare x y)
            (raise-error msg)))))

(define (print-alone x)
  (print "\n\n" x "\n\n"))

(define (tests-finished)
  (if tests-enabled?
      (print "\n\ndone with " subject ".\n\n")))

(define tests-enabled? #f)
