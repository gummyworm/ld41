!to "game.prg",cbm

;**************************************
; zeropage
tmp0=$fa
tmp1=$fb
tmp2=$fc
tmp3=$fd
result=$ff

;**************************************
; constants
SCREEN=$1e00
VP_W=10
VP_H=10

; GC: generation chance (1/(2^GC_XXX))
; CH: character
GC_HEART=5
CH_HEART=83

GC_SPELL=6
CH_SPELL=63

;**************************************
; macros
!macro GENCELL .chance, .char, donelbl {
	ldx #.chance
	jsr rnd
	lda #' '
	ldx result
	bne +
	lda #.char
	jmp donelbl
+	
}

;**************************************
*=$1001
!word $100b
!word 2018
!byte $9e
!text "4109",0
!word 0

;**************************************
start
	jsr genscreen
	jmp *


;**************************************
; parse the player's command
parsecmd
	rts

;**************************************
; generate a new screen of gameplay
genscreen
.y=tmp2
.dst=tmp3
	ldx #<(SCREEN+SCREEN_W+1)
	lda #>(SCREEN+SCREEN_W+1)
	stx .dst
	sta .dst+1

	lda #VP_H-1
	sta .y
.l0
	lda #VP_W
	tay

.l1	
	+GENCELL GC_HEART, CH_HEART, done
	+GENCELL GC_SPELL, CH_SPELL, done
	
done	sta (.dst),y
	dey
	bpl .l1

	rts

;**************************************
; get a random # between 0 and 2^(.X) in result
rnd
	lda #$00
	sta result
-	lsr tmp0
	ror tmp0+1
	bcc +
	lda tmp0
	eor #$aa  ; most significant bit *must* be set
	sta tmp0
	lda tmp0+1
	eor #$2b  ; least significant bit should be set
	sta tmp0+1
+	rol result
	dex
	bne -
	rts

