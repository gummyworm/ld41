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
SCREEN_H=23
VP_W=10
VP_H=10
VP_X=5
VP_Y=0
STATUS_LINE=12
INPUT_LINE=13

; GC: generation chance (1/(2^GC_XXX))
; CH: character
GC_HEART=7
CH_HEART=83

GC_SPELL=6
CH_SPELL=63

GC_MONEY=6
CH_MONEY=36

GROUND_CHAR=102

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

; .X contains the # of characters matched
!macro STRCMP .src, .other {
	ldx #$ff
-	inx
	lda .other,x
	beq +
	cmp .src,x
	beq -
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
	jsr clear

	; display title
	ldx #titlemsglen-1
-	lda titlemsg,x
	sta SCREEN+(SCREEN_W*(SCREEN_H/2)),x
	dex
	bpl -

	; wait for user to begin
-	jsr $ffe4
	cmp #$00
	beq -

	; srand
	lda $9004
	sta rndval
	jsr rnd
	sta rndval+1

	jsr clear
	jsr drawui
	jsr drawstatus
	jsr genscreen
mainloop
	jsr parsecmd
	jmp mainloop

;**************************************
; get user input and parse the player's command
parsecmd
.input=SCREEN+SCREEN_W*INPUT_LINE
.row=tmp0
.col=tmp1
	ldx #INPUT_LINE
	ldy #0
	jsr $fff0
-	jsr $ffe4
	tay
	beq -

	; clear the input line
	lda #' '
	ldx #10
-	sta .input,x
	dex
	bpl -

	cpy #'T'
	bne +
	ldx #<take
	ldy #>take
	bne .printaction
+	cpy #'H'
	bne +
	ldx #<hit
	ldy #>hit
	bne .printaction
+	jmp parsecmd	; invalid character

.printaction
	stx tmp0
	sty tmp0+1
	ldy #$00
-	lda (tmp0),y
	beq +
	jsr $ffd2
	iny
	bne -
+	lda #' '
	jsr $ffd2
	lda #$80
	eor .input
	sta .input

.getcoord
	jsr $ffe4
	cmp #'A'
	bcc .getcoord
	cmp #'A'+VP_W
	bcs .getcoord
	jsr $ffd2
	sec
	sbc #'A'
	sta .col

-	jsr $ffe4
	cmp #'0'
	bcc -
	cmp #'9'+1
	bcs -
	jsr $ffd2
	sec
	sbc #'0'
	sta .row
	rts

take
!pet "take",0
hit
!pet "hit",0


;**************************************
; generate a new screen of gameplay
genscreen
.y=tmp2
.dst=tmp3
	;draw the ground
	ldx #VP_W
-	lda #GROUND_CHAR
	sta SCREEN+((VP_H+VP_Y)*SCREEN_W)+VP_X,x
	sta SCREEN+((VP_H+VP_Y-1)*SCREEN_W)+VP_X,x
	dex
	bne -

	ldx #<(SCREEN+SCREEN_W+1)+VP_X
	lda #>(SCREEN+SCREEN_W+1)+VP_X
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
	+GENCELL GC_MONEY, CH_MONEY, .next

.next   cmp #$00
	beq +
	sta (.dst),y
+	dey
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
	ldx #(VP_W)	; 'A'-'J'
-	txa
	sta SCREEN+VP_X,x
	dex
	bne -

	ldy #$00
	ldx #48
-	txa
	sta SCREEN+SCREEN_W+VP_X,y
	tya
	adc #SCREEN_W
	tay
	inx
	cpx #48+10
	bcc -
	rts

;**************************************
; draw the player status (health/magic)
drawstatus
	ldy hp
	lda #$00
	jsr $d391
	jsr $dddd

	ldx #$00
-	lda $100,x
	beq +
	sta statusmsg+2,x
	inx
	bne -

+	ldy magick
	lda #$00
	jsr $d391
	jsr $dddd

	ldx #$00
-	lda $100,x
	beq +
	sta statusmsg+7,x
	inx
	bne -

+	ldx #statusmsglen-1
-	lda statusmsg,x
	sta SCREEN+(STATUS_LINE*SCREEN_W),x
	dex
	bpl -
	rts

;**************************************
; data
titlemsg
!scr "press any key to begin"
titlemsglen=*-titlemsg

statusmsg
!scr 83,":   ",88,":    "
statusmsglen=*-statusmsg

hp !byte 10
magick !byte 5
