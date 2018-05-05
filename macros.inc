;**************************************
; macros
!macro GENCELL .chance, .char, donelbl {
	ldx #.chance
	jsr rnd
	lda #$00
	ldx result
	bne +
	lda #.char
	jmp donelbl
+
}

!macro GENCELL2 .chance, .char, donelbl {
	ldx #.chance
	jsr rnd
	lda result
	php
	jsr rnd
	lda #$00
	plp
	bne +
	ldx result
	bne +
	lda #.char
	jmp donelbl
+
}
