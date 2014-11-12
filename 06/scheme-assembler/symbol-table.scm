(load "test-suite")
(load "utils")

(define (contains? hash key)
  (if (hash-get hash key)
      #t
      #f))

(define get-address hash-get)
(define add-entry hash-assoc)

(define (next-available-ram symbol-table)
  (define (ram-iter ram)
    (if (not (member ram (hash-values symbol-table)))
        ram
        (ram-iter (+ 1 ram))))
  (ram-iter 16))

(define (new-symbol-table keys values)
  (let ((all-keys (append keys     '("SP" "LCL" "ARG" "THIS" "THAT" "SCREEN" "KBD" "R0" "R1" "R2" "R3" "R4" "R5" "R6" "R7" "R8" "R9" "R10" "R11" "R12" "R13" "R14" "R15")))
        (all-values (append values '( 0    1     2     3      4      16384    24576 0    1    2    3    4    5    6    7    8    9    10    11    12    13    14    15))))
    (hash all-keys all-values)))

;; tests ;;

(define subject "symbol-table")

(let ((symbol-table (new-symbol-table '() '())))

  (assert-eq "starts with some special names"
             (get-address symbol-table "SP")
             0)

  (assert-eq "starts with some special names"
             (get-address symbol-table "LCL")
             1)

  (assert-eq "starts with some special names"
             (get-address symbol-table "ARG")
             2)

  (assert-eq "starts with some special names"
             (get-address symbol-table "THIS")
             3)

  (assert-eq "starts with some special names"
             (get-address symbol-table "THAT")
             4)

  (assert-eq "starts with some special names"
             (get-address symbol-table "SCREEN")
             16384)

  (assert-eq "starts with some special names"
             (get-address symbol-table "KBD")
             24576)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R0")
             0)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R1")
             1)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R2")
             2)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R3")
             3)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R4")
             4)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R5")
             5)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R6")
             6)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R7")
             7)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R8")
             8)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R9")
             9)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R10")
             10)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R11")
             11)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R12")
             12)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R13")
             13)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R14")
             14)

  (assert-eq "starts with some special names"
             (get-address symbol-table "R15")
             15)

  )

(let ((symbol-table (new-symbol-table '("k1" "k2") '("v1" "v2"))))

  (assert "implements #contains?"
          (contains? symbol-table
                     "k1"))

  (assert "implements #contains?"
          (not (contains? symbol-table
                          "k3")))

  (assert-eq "implements #get-address"
             (get-address symbol-table
                          "k1")
             "v1")

  (assert-eq "implements #get-address"
             (get-address symbol-table
                          "k2")
             "v2")

  (assert-eq "implements #next-available-ram"
             (next-available-ram symbol-table)
             16)

  (assert-eq "implements #next-available-ram"
             (next-available-ram (add-entry "asdf" 16 symbol-table))
             17)

  (assert-eq "implements #next-available-ram"
             (next-available-ram (add-entry "asdf" 16 (add-entry "jkl;" 17 symbol-table)))
             18)

  (assert "implements #add-entry"
          (contains? (add-entry "k1" "v1" (new-symbol-table '() '()))
                     "k1"))

  (tests-finished))
