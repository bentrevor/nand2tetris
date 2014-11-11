(load "test-suite")
(load "symbol-table")
(load "utils")

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

(define (parse-labels asm-code)
  (let ((lines (split "\r\n" asm-code)))
    (define (parse-iter current-rom-addr lines-left keys values)
      (cond ((null? lines-left) (new-symbol-table keys values))
            ((eq? #\l (parse-type (car lines-left))) (parse-iter current-rom-addr
                                                                 (cdr lines-left)
                                                                 (cons (parse-symbol (car lines-left)) keys)
                                                                 (cons current-rom-addr values)))
            (else (parse-iter (+ 1 current-rom-addr)
                              (cdr lines-left)
                              keys
                              values))))
    (parse-iter 0 lines '() '())))

(define (remove-comments asm-code)
  (let ((lines (split "\r\n" asm-code)))
    (define (comment-iter uncommented-lines lines-left)
      ;; (print-alone (if (not (null? lines-left)) (car lines-left)))
      (if (null? lines-left)
          uncommented-lines
          (comment-iter (if (string=? (substring (strip-leading-whitespace (car lines-left)) 0 2)
                                      "//")
                            uncommented-lines
                            (cons (car lines-left) uncommented-lines))
                        (cdr lines-left))))
    (reverse (comment-iter '() lines))))


;; tests ;;
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

  (let ((asm-code (string-append a-command "\r\n" c-command "\r\n" l-command "\r\n")))
    (assert-eq "creates a new symbol table with default symbols"
               (get-address (parse-labels asm-code)
                            "SP")
               0)

    (assert-eq "adds all labels to the default symbol table (first pass)"
               (get-address (parse-labels asm-code)
                            "LABEL")
               2)

    (assert-eq "strips comments"
               (remove-comments (string-append "// comment\r\n" asm-code))
               (split "\r\n" asm-code)))

    (assert-eq "strips comments"
               (remove-comments "// asdf\r\n// jlkf\r\n// qewr\r\n")
               '()))


(tests-finished)
