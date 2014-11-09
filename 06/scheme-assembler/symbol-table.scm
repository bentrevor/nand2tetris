(load "test-suite")
(load "hash")

(define (contains? hash key)
  (if (hash-get hash key)
      #t
      #f))

(define get-address hash-get)
(define add-entry hash-assoc)

(define (new-symbol-table keys values)
  (hash keys values))


;; tests ;;

(assert "implements #contains?"
        (contains? (new-symbol-table '("k1" "k2") '("v1" "v2"))
                   "k1"))

(assert "implements #contains?"
        (not (contains? (new-symbol-table '("k1" "k2") '("v1" "v2"))
                        "k3")))

(assert-eq "implements #get-address"
           (get-address (new-symbol-table '("k1" "k2") '("v1" "v2"))
                        "k1")
           "v1")

(assert-eq "implements #get-address"
           (get-address (new-symbol-table '("k1" "k2") '("v1" "v2"))
                        "k2")
           "v2")

(assert-eq "implements #add-entry"
           (add-entry "k1" "v1" (new-symbol-table '() '()))
           '(("k1" "v1")))
