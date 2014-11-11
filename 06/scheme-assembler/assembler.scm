(load "test-suite")
(load "parser")
(load "code")
(load "symbol-table")


(define (translate)
  (let* ((all-chars (call-with-input-file "Add.asm"
                      (lambda (input-port)
                        (let loop ((char (read-char input-port)))
                          (if (eof-object? char)
                              '()
                              (cons char (loop (read-char input-port))))))))
         (asm-code (list->string all-chars))
         (lines (split "\r\n" asm-code))
         ;; (x  (print-alone (cadr lines)))
         (lines-without-comments (remove-comments lines))
         (symbol-table (parse-labels lines-without-comments)))
    (print (asm-to-binary lines symbol-table))))

(define (asm-to-binary lines symbol-table)
  (define (convert-iter lines-left binary-str)
    (if (null? lines-left)
        binary-str
        (let ((new-binary-str (command-to-binary line symbol-table)))
          (convert-iter (cdr lines-left) (string-append binary-str
                                                        new-binary-str)))))
  (convert-iter lines ""))

(define (command-to-binary command symbol-table)
  (let ((type (parse-type command))
        (sym  (parse-symbol command)))
    (if (eq? #\a type)
        (cond ((string->number sym) (dec-to-bin16 sym))
              ((contains? symbol-table sym) (dec-to-bin16 (get-address symbol-table sym)))
              (else (let ((ram (next-available-ram symbol-table)))
                        (begin
                          (add-entry sym ram symbol-table)
                          (dec-to-bin16 ram)))))
        (string-append "111"
                       (comp-for (parse-comp command))
                       (dest-for (parse-dest command))
                       (jump-for (parse-jump command))))))

(define (dec-to-bin16 dec)
  (let ((dec-num (string->number dec)))
    (pad-to-16 (number->string dec-num 2))))

(define (pad-to-16 str)
  (if (equal? 16 (string-length str))
      str
      (pad-to-16 (string-append "0" str))))


;; tests ;;

(define subject "assembler")


(assert-eq "pads strings with 0s at the front"
           (pad-to-16 "11111")
           "0000000000011111")

(assert-eq "converts a number to 16-bit binary"
           (dec-to-bin16 "31")
           "0000000000011111")

(assert-eq "converts a numeric symbol to binary"
           (command-to-binary "        @100"
                              (new-symbol-table '() '()))
           "0000000001100100")


(translate)

(tests-finished)
