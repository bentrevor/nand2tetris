(define (jump-for mnemonic)
  (cond ((string=? mnemonic "JGT") "001")
        ((string=? mnemonic "JEQ") "010")
        ((string=? mnemonic "JGE") "011")
        ((string=? mnemonic "JLT") "100")
        ((string=? mnemonic "JNE") "101")
        ((string=? mnemonic "JLE") "110")
        ((string=? mnemonic "JMP") "111")
        (else                      "000")))


(define (dest-for mnemonic)
  (cond ((string=? mnemonic "M"  ) "001")
        ((string=? mnemonic "D"  ) "010")
        ((string=? mnemonic "MD" ) "011")
        ((string=? mnemonic "A"  ) "100")
        ((string=? mnemonic "AM" ) "101")
        ((string=? mnemonic "AD" ) "110")
        ((string=? mnemonic "AMD") "111")
        (else                      "000")))

(define (comp-for mnemonic)
  (cond ((string=? mnemonic "0"  ) "0101010")
        ((string=? mnemonic "1"  ) "0111111")
        ((string=? mnemonic "-1" ) "0111010")
        ((string=? mnemonic "D"  ) "0001100")
        ((string=? mnemonic "A"  ) "0110000")
        ((string=? mnemonic "!D" ) "0001101")
        ((string=? mnemonic "!A" ) "0110001")
        ((string=? mnemonic "-D" ) "0001111")
        ((string=? mnemonic "-A" ) "0110011")
        ((string=? mnemonic "D+1") "0011111")
        ((string=? mnemonic "A+1") "0110111")
        ((string=? mnemonic "D-1") "0001110")
        ((string=? mnemonic "A-1") "0110010")
        ((string=? mnemonic "D+A") "0000010")
        ((string=? mnemonic "D-A") "0010011")
        ((string=? mnemonic "A-D") "0000111")
        ((string=? mnemonic "D&A") "0000000")
        ((string=? mnemonic "D|A") "0010101")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic "M"  ) "1110000")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic "!M" ) "1110001")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic "-M" ) "1110011")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic "M+1") "1110111")
        ((string=? mnemonic ""   ) "xxxxxxx")
        ((string=? mnemonic "M-1") "1110010")
        ((string=? mnemonic "D+M") "1000010")
        ((string=? mnemonic "D-M") "1010011")
        ((string=? mnemonic "M-D") "1000111")
        ((string=? mnemonic "D&M") "1000000")
        ((string=? mnemonic "D|M") "1010101")))