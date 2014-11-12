(load "test-suite")

;; hash
;; (not a real hashtable in terms of time complexity, but it behaves like one)
(define (hash keys values)
  (define (hash-iter keys values hash-so-far)
    (if (empty-hash? keys)
        hash-so-far
        (hash-iter (cdr keys)
                   (cdr values)
                   (cons (cons (car keys)
                               (list (car values)))
                         hash-so-far))))
  (hash-iter keys values '()))

(define (hash-get hash key)
  (cond ((empty-hash? hash)       #f)
        ((equal? key (caar hash)) (car (hash-values hash)))
        (else                     (hash-get (cdr hash) key))))

(define (hash-keys hash)
  (map car hash))

(define (hash-values hash)
  (map cadr hash))

(define (hash-assoc key value old-hash)
  (cond ((hash-get old-hash key) (hash-set old-hash key value))
        (else                    (hash (cons key (hash-keys old-hash))
                                       (cons value (hash-values old-hash))))))

(define (hash-set hash key value)
  (define (set-iter new-hash old-hash)
    (if (empty-hash? old-hash)
        new-hash
        (set-iter (hash-assoc (car (hash-keys old-hash))
                              (if (equal? key (car (hash-keys old-hash)))
                                  value
                                  (car (hash-values old-hash)))
                              new-hash)
                  (cdr old-hash))))

  (set-iter '() hash))

(define empty-hash? null?)

;; string utils
(define (str-contains? str char)
  (not (eq? -1 (index-of char str))))

(define (index-of char str)
  (define (index-iter rest-of-chars i)
    (cond ((null? rest-of-chars)             -1)
          ((equal? char (car rest-of-chars)) i)
          (else                              (index-iter (cdr rest-of-chars)(+ 1 i)))))
  (index-iter (string->list str) 0))

(define (strip-whitespace str)
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

(define (index-of-substr substr str)
  (define (index-iter i str-left)
    (cond ((< (string-length str-left) (string-length substr)) -1)
          ((string=? substr (substring str-left 0 (string-length substr))) i)
          (else (index-iter (+ 1 i) (substring str-left 1 (string-length str-left))))))
  (index-iter 0 str))

(define (substr-after delim str)
  (let ((index (if (char? delim)
                   (index-of delim str)
                   (- (+ (string-length delim)
                         (index-of-substr delim str))
                      1))))
    (if (eq? -1 index)
        str
        (strip 'before index str))))

(define (substr-before delim str)
  (let ((index (if (char? delim)
                   (index-of delim str)
                   (index-of-substr delim str))))
    (if (eq? -1 index)
        str
        (strip 'after index str))))

(define (split delim str)
  (define (split-iter words str-left)
    (if (string=? str-left "")
        words
        (split-iter (cons (substr-before delim str-left) words)
                    (substr-after delim str-left))))
  (reverse (split-iter '() str)))


;; tests ;;

(define subject "hash")

(assert "knows when it is empty"
        (empty-hash? (hash '() '())))

(assert "knows when it is not empty"
        (not (empty-hash? (hash '("k") '("v")))))

(assert-eq "can be built"
           (hash '("k") '("v"))
           '(("k" "v")))

(assert-eq "can have more than one key"
           (hash '("k1" "k2") '("v1" "v2"))
           '(("k2" "v2") ("k1" "v1")))

(assert-eq "can list all keys"
           (hash-keys (hash '("k1" "k2") '("v1" "v2")))
           '("k2" "k1"))

(assert-eq "can list all values"
           (hash-values (hash '("k1" "k2") '("v1" "v2")))
           '("v2" "v1"))

(assert-eq "can get value of a key"
           (hash-get (hash '("k1") '("k2")) "k1")
           "k2")

(assert-eq "can modify values"
           (hash-set '(("k1" "v1")) "k1" "v2")
           '(("k1" "v2")))

(assert-eq "can add key-value pair"
           (hash-assoc "k2" "v2" '(("k1" "v1")))
           '(("k1" "v1") ("k2" "v2")))

(assert-eq "can replace value of a key"
           (hash-assoc "k1" "v2" '(("k1" "v1")))
           '(("k1" "v2")))


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

(assert-eq "strip-whitespace"
           (strip-whitespace  "    asdf    ")
           "asdf")

(assert-eq "substr-after a char"
           (substr-after #\a "asdf")
           "sdf")

(assert-eq "substr-after a char"
           (substr-after #\s "asdf")
           "df")

(assert-eq "substr-after a char"
           (substr-after #\d "asdf")
           "f")

(assert-eq "substr-after a char that's not in the string"
           (substr-after #\x "asdf")
           "asdf")

(assert-eq "substr-after a string"
           (substr-after "a" "asdf")
           "sdf")

(assert-eq "substr-after a string"
           (substr-after "as" "asdf")
           "df")

(assert-eq "substr-after a string"
           (substr-after "sd" "asdf")
           "f")

(assert-eq "substr-after a string"
           (substr-after "df" "asdf")
           "")

(assert-eq "substr-after a non-substring"
           (substr-after "x" "asdf")
           "asdf")

(assert-eq "index-of-substr"
           (index-of-substr "sd" "asdf")
           1)

(assert-eq "split"
           (split "x" "asdfxasdfxx")
           '("asdf" "asdf" ""))

(tests-finished)
