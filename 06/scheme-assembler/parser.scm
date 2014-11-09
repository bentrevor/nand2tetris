(load "test-suite")

;; util functions ;;
;; (scheme v4.7 doesn't give you many built-in functions)

(define (str-contains? str char)
  (not (eq? -1 (index-of char str))))

(define (index-of char str)
  (define (index-iter rest-of-chars i)
    (cond ((null? rest-of-chars)             -1)
          ((equal? char (car rest-of-chars)) i)
          (else                              (index-iter (cdr rest-of-chars)(+ 1 i)))))
  (index-iter (string->list str) 0))

(define (strip-leading-whitespace str)
  (define (strip-iter chars stripped-str)
    (if (null? chars)
        (list->string (reverse stripped-str))
        (let ((next-str (if (eq? #\space (car chars))
                            stripped-str
                            (cons (car chars) stripped-str))))
          (strip-iter (cdr chars) next-str))))
  (strip-iter (string->list str) '()))

(define (strip direction index str)
  (let ((sliced-str (if (eq? direction 'before)
                        (substring str (+ 1 index) (string-length str))
                        (substring str 0 index)))
        (char (string-ref str index)))
    (if (str-contains? str char)
        sliced-str
        str)))

(define (substr-after char str)
  (strip 'before (index-of char str) str))

(define (substr-before char str)
  (strip 'after (index-of char str) str))

;; parser functions ;;
(define (parse-dest command)
  (if (str-contains? command #\=)
      (strip-leading-whitespace (substr-before #\= command))
      ""))

(define (parse-jump command)
  (if (str-contains? command #\;)
      (substr-after #\; command)
      ""))

(define (parse-comp command)
  (substr-before #\; (substr-after #\= command)))

(define (parse-type command)
  (let ((stripped-command (strip-leading-whitespace command)))
    (cond ((and (eq? #\( (string-ref stripped-command 0))
                (eq? #\) (string-ref stripped-command
                                     (- (string-length stripped-command) 1))))
           #\l)
          ((eq? #\@ (string-ref stripped-command 0))
           #\a)
          (else
           #\c))))

(define (parse-symbol command)
  (cond ((eq? #\l (parse-type command)) (substr-before #\) (substr-after #\( command)))
        ((eq? #\a (parse-type command)) (substr-after #\@ command))
        (else                           "")))


;; util tests ;;
(define subject "string utils")

(assert "str-contains?"
        (str-contains? "abcde" #\c))

(assert "str-contains?"
        (str-contains? "abcde" #\c))

(assert "str-contains?"
        (not (str-contains? "abcde" #\f)))

(assert-eq "index-of"
           (index-of #\a "asdf")
           0)

(assert-eq "index-of"
           (index-of #\s "asdf")
           1)

(assert-eq "index-of"
           (index-of #\d "asdf")
           2)

(assert-eq "index-of"
           (index-of #\f "adsf")
           3)

(assert-eq "strip-leading-whitespace"
           (strip-leading-whitespace  "    asdf")
           "asdf")

;; parser tests ;;
(define subject "parser")

(let ((a-command "        @symbol")
      (c-command "        dest=comp;jump")
      (l-command "        (LABEL)"))

  (assert-eq "can get the dest from a command"
             (parse-dest c-command)
             "dest")

  (assert-eq "can get the jump from a command"
             (parse-jump c-command)
             "jump")

  (assert-eq "can get the comp from a command"
             (parse-comp c-command)
             "comp")

  (assert-eq "knows the type of an a-command"
             (parse-type a-command)
             #\a)

  (assert-eq "knows the type of an c-command"
             (parse-type c-command)
             #\c)

  (assert-eq "knows the type of an l-command"
             (parse-type l-command)
             #\l)

  (assert-eq "gets the symbol from an l-command"
             (parse-symbol l-command)
             "LABEL")

  (assert-eq "gets the symbol from an a-command"
             (parse-symbol a-command)
             "symbol")

  (assert-eq "gets the symbol from an c-command"
             (parse-symbol c-command)
             "")

  )
