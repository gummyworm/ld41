!to "game.prg",cbm

;**************************************
; zeropage
rndval=$f0
result=$f2

tmp0=$fa
tmp1=$fb
tmp2=$fc
tmp3=$fd

;**************************************
; constants
SCREEN=$1e00
SCREEN_W=22
VP_W=10
VP_H=10

; GC: generation chance (1/(2^GC_XXX))
; CH: character
GC_HEART=6
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
	;srand
	lda #$ae
	sta rndval
	sta rndval+1

	jsr clear
	jsr drawui
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

	lda #VP_H
	sta .y
.l0
	lda #VP_W-1
	tay
.l1
	+GENCELL GC_HEART, CH_HEART, .next
	+GENCELL GC_SPELL, CH_SPELL, .next

.next   sta (.dst),y
	dey
	bpl .l1

	lda .dst
	clc
	adc #SCREEN_W
	sta .dst

	dec .y
	bne .l0

	rts

;**************************************
; get a random # between 0 and 2^(.X) in result
rnd
	lda #$00
	sta result
-	lsr rndval
	ror rndval+1
	bcc +
	lda rndval
	eor #$aa  ; most significant bit *must* be set
	sta rndval
	lda rndval+1
	eor #$2b  ; least significant bit should be set
	sta rndval+1
+	rol result
	dex
	bne -
	rts

;**************************************
clear
	ldx #$00
-	lda #' '
	sta $1e00,x
	sta $1f00,x
	lda #$00
	sta $9600,x
	sta $9700,x
	dex
	bne -
	rts

;**************************************
drawui
	ldx #($01+VP_W)	; 'A'
-	txa
	sta SCREEN,x
	dex
	bne -

	ldy #$00
	ldx #48
-	txa
	sta SCREEN+SCREEN_W,y
	tya
	adc #SCREEN_W
	tay
	inx
	cpx #48+10
	bcc -
	rts
