// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

	@SCREEN
        D=A
        @p      // current pixel
        M=D
(LOOP)
        @24576
        D=M
	@UNFILL
        D;JEQ
(FILL)
        @p
        D=M
	@24576
	D=D-A
	@LOOP
	D;JEQ

        @p
	AD=M
        M=-1 // fill p

        @p
        M=D+1 // inc p

	@LOOP
	0;JMP
(UNFILL)
	@p
	D=M
	@LOOP
	D;JEQ

	@p
	M=D-1 // dec p

	@p
	AD=M
	M=0 // unfill p

	@LOOP
	0;JMP
(END)
        @END
        0;JMP
