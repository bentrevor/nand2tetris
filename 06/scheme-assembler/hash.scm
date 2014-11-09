(load "test-suite")

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


;; tests ;;

(assert "knows when a hash is empty"
        (empty-hash? (hash '() '())))

(assert "knows when a hash is not empty"
        (not (empty-hash? (hash '("k") '("v")))))

(assert-eq "can build a hash"
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

(assert-eq "can add key-value pairs to a hash"
           (hash-assoc "k2" "v2" '(("k1" "v1")))
           '(("k1" "v1") ("k2" "v2")))

(assert-eq "can replace value of a key in a hash"
           (hash-assoc "k1" "v2" '(("k1" "v1")))
           '(("k1" "v2")))



(newline)
(print "\ndone.\n\n")
