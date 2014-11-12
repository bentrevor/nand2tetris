(load "test-suite")
(load "code")
(load "symbol-table")
(load "utils")

(define (parse-dest command)
  (if (str-contains? command #\=)
      (strip-whitespace (substr-before #\= command))
      ""))

(define (parse-jump command)
  (if (str-contains? command #\;)
      (substr-after #\; command)
      ""))

(define (parse-comp command)
  (substr-before #\; (substr-after #\= command)))

(define (parse-dest-binary command)
  (dest-for (parse-dest command)))

(define (parse-jump-binary command)
  (jump-for (parse-jump command)))

(define (parse-comp-binary command)
  (comp-for (parse-comp command)))

(define (parse-type command)
  (let ((stripped-command (strip-whitespace command)))
    (cond ((eq? #\( (string-ref stripped-command 0)) #\l)
          ((eq? #\@ (string-ref stripped-command 0)) #\a)
          (else                                      #\c))))

(define (parse-symbol command)
  (cond ((eq? #\l (parse-type command)) (substr-before #\) (substr-after #\( command)))
        ((eq? #\a (parse-type command)) (substr-after #\@ command))
        (else                           "")))

(define (parse-labels lines)
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
  (parse-iter 0 lines '() '()))

(define (remove-comments lines)
  (define (comment-iter uncommented-lines lines-left)
    (if (null? lines-left)
        uncommented-lines
        (let ((line (strip-whitespace (car lines-left))))
          (comment-iter (if (and (> (string-length line) 1)
                                 (string=? (substring line 0 2)
                                           "//"))
                            uncommented-lines
                            (cons line uncommented-lines))
                        (cdr lines-left)))))
  (define (strip-postfix-comments commands)
    (define (postfix-iter commands-left stripped-commands)
      (if (null? commands-left)
          stripped-commands
          (postfix-iter (cdr commands-left)
                        (cons (strip-trailing-comment (car commands-left))
                              stripped-commands))))
    (postfix-iter commands '()))
  (strip-postfix-comments (comment-iter '() lines)))

(define (strip-trailing-comment command)
  (define (index-of-first-slash-iter chars i)
    (cond ((null? chars)         i)
          ((eq? #\/ (car chars)) i)
          (else                  (index-of-first-slash-iter (cdr chars) (+ 1 i)))))
  (let* ((command-chars (string->list (strip-whitespace command)))
         (first-slash-index (index-of-first-slash-iter command-chars 0)))
    (substring (strip-whitespace command)
               0
               first-slash-index)))

(define (remove-labels lines)
  (define (labels-iter lines-left non-label-lines)
    (if (null? lines-left)
        non-label-lines
        (labels-iter (cdr lines-left)
                     (if (eq? #\l (parse-type (car lines-left)))
                         non-label-lines
                         (cons (car lines-left) non-label-lines)))))
  (reverse (labels-iter lines '())))

(define (remove-blank-lines lines)
  (define (blank-iter lines-left non-blank-lines)
    (if (null? lines-left)
        non-blank-lines
        (blank-iter (cdr lines-left)
                    (if (blank? (car lines-left))
                        non-blank-lines
                        (cons (car lines-left) non-blank-lines)))))
  (reverse (blank-iter lines '())))

(define (blank? str)
  (define (blank-iter chars)
    (if (null? chars)
        #t
        (if (eq? #\space (car chars))
            (blank-iter (cdr chars))
            #f)))
  (blank-iter (string->list str)))


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

  (let ((c-command "AMD=A+1;JMP"))
    (assert-eq "can get the binary dest from a command"
               (parse-dest-binary c-command)
               "111")

    (assert-eq "can get the binary jump from a command"
               (parse-jump-binary c-command)
               "111")

    (assert-eq "can get the binary comp from a command"
               (parse-comp-binary c-command)
               "0110111"))

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

  (let* ((asm-code (string-append "// comment\r\n" a-command "\r\n" c-command "\r\n" l-command "\r\n"))
         (lines (split "\r\n" asm-code)))

    (assert-eq "creates a new symbol table with default symbols"
               (get-address (parse-labels lines)
                            "SP")
               0)

    (assert-eq "adds all labels to the default symbol table (first pass)"
               (get-address (parse-labels lines)
                            "LABEL")
               3)

    (assert "implements blank?"
            (blank? ""))

    (assert "implements blank?"
            (blank? "   "))

    (assert "implements blank?"
            (not (blank? "asdf")))

    (assert-eq "removes labels"
               (remove-labels lines)
               '("// comment" "        @symbol" "        dest=comp;jump"))

    (assert-eq "strips comments"
               (remove-comments lines)
               '("@symbol" "dest=comp;jump" "(LABEL)"))

    (assert-eq "strips comments"
               (remove-comments (split "\r\n" "// asdf\r\n// jlkf\r\n// qewr\r\n\r\n"))
               '(""))

    (assert-eq "strips comments at the end of a line"
               (strip-trailing-comment "        asdf        // jkl;")
               "asdf")

    (assert-eq "strips comments at the end of a line"
               (remove-comments (split "\r\n" "@asdf// comment\r\n// jlkf\r\n\r\n(QWER)// qewr\r\n"))
               '("@asdf" "" "(QWER)"))

    (assert-eq "strips blank lines"
               (remove-blank-lines '("" "asdf" "jkl;" "  "))
               '("asdf" "jkl;"))))



(tests-finished)
