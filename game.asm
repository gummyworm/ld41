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
COLORMEM=$9600
VP_W=9
VP_H=10
VP_X=5
VP_Y=0
STATUS_LINE=12
INPUT_LINE=13

VIEWPORT=SCREEN+VP_X+SCREEN_W
VIEWPORT_COL=COLORMEM+VP_X+SCREEN_W
VIEWPORT_END=SCREEN+VP_X+VP_W+SCREEN_W*SCREEN_H

; GC: generation chance (1/(2^GC_XXX))
; CH: character
GC_HEART=7
CH_HEART=83

GC_SPELL=6
CH_SPELL=63

GC_MONEY=6
CH_MONEY=36

GC_GEM=1
CH_GEM=$5a

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
	ldx #5
	ldy #0
	clc
	jsr $fff0
	ldx #$00
-	lda titlemsg,x
	jsr $ffd2
	inx
	cpx #titlemsglen
	bne -

	; wait for user to begin
-	jsr $ffe4
	cmp #$00
	beq -

	; srand
	lda $9004
	sta rndval
	jsr rnd
	sta rndval+1

	jsr drawstatus
	jsr genscreen
mainloop
	jsr drawstatus
	jsr parsecmd
	jmp mainloop

;**************************************
; get user input and parse the player's command
parsecmd
.input=SCREEN+SCREEN_W*INPUT_LINE
.cellpos=tmp0
.action=tmp2
	; clear the input line
	lda #' '
	ldx #10
-	sta .input,x
	dex
	bpl -

	ldx #INPUT_LINE
	ldy #0
	clc
	jsr $fff0
-	jsr $ffe4
	tay
	beq -
	sty .action

	cpy #'G'
	bne +
	jsr genscreen
	rts

+	cpy #'T'
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
	cmp #'A'+VP_W+1
	bcs .getcoord
	jsr $ffd2
	sec
	sbc #'A'
	pha

-	jsr $ffe4
	cmp #'0'
	bcc -
	cmp #'9'+1
	bcs -
	jsr $ffd2
	sec
	sbc #'0'
	tay
	pla
	tax

	jsr getcell
	pha
	jsr hicell
	stx .cellpos

	jsr $ffe4
	beq *-3
	ldx .cellpos
	cmp #$0d
	beq +
	pla
	jsr hicell	; unhighlight
	jmp parsecmd	; player cancelled action

+	jsr hicell	; unhighlight
.enemy
	ldx .cellpos
	lda VIEWPORT_COL-SCREEN_W,x
	and #$0f
	cmp #$02
	bne .object

	pla
	lda .action
	cmp #'H'
	bne +
	dec enemy_hp
	bne +
	jsr kill_enemy

+	rts

.object
	pla
	cmp #CH_MONEY
	bne .heart
.money
	lda .action
	cmp #'T'
	bne +
	lda #' '
	ldx .cellpos
	sta VIEWPORT-SCREEN_W,x
	inc money
+	rts

.heart
	cmp #CH_HEART
	bne .spell
	lda #' '
	ldx .cellpos
	sta VIEWPORT-SCREEN_W,x
	inc hp
+	rts

.spell
	cmp #CH_SPELL
	bne .gem
	lda #' '
	ldx .cellpos
	sta VIEWPORT-SCREEN_W,x
	inc magick
+	rts

.gem
	cmp #CH_GEM
	bne .empty
	jmp *
	lda #' '
	ldx .cellpos
	sta VIEWPORT-SCREEN_W,x
	inc gemcnt
+	rts

.empty
	rts

take
!pet "take",0
hit
!pet "hit",0

;**************************************
; setcell sets the cell at the given (row,col) position to the value in .A
setcell
	!byte $2c
;**************************************
; getcell returns the cell under the given (row,col) position- coords in (.X,.Y)
; The screen offset of the char is returned in .X.
getcell
	lda #$00
	sta tmp1
	txa
	clc
-	adc #SCREEN_W
	dey
	bpl -

+	tax
	ldy tmp1
	beq +
+	lda VIEWPORT-SCREEN_W,x
	rts

; highlights the cell whose viewport offset is given in .X
hicell
	lda VIEWPORT-SCREEN_W,x
	eor #$80
	sta VIEWPORT-SCREEN_W,x
	rts

;**************************************
; generate a new screen of gameplay
genscreen
.y=tmp2
.dst=tmp3
	jsr clear
	jsr drawui

	;draw the ground
	ldx #VP_W
-	lda #GROUND_CHAR
	sta SCREEN+((VP_H+VP_Y)*SCREEN_W)+VP_X,x
	sta SCREEN+((VP_H+VP_Y-1)*SCREEN_W)+VP_X,x
	dex
	bpl -

	ldx #<(SCREEN+SCREEN_W)+VP_X
	lda #>(SCREEN+SCREEN_W)+VP_X
	stx .dst
	sta .dst+1

	lda #VP_H-1
	sta .y
.l0
	lda #VP_W
	tay
.l1
	+GENCELL GC_HEART, CH_HEART, .next
	+GENCELL GC_SPELL, CH_SPELL, .next
	+GENCELL GC_MONEY, CH_MONEY, .next
	+GENCELL2 GC_GEM, CH_GEM, .next

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
	bpl .l0

.genenemy
.width=tmp0
.height=tmp1
.src=tmp2
	ldx #<gfx_snake
	lda #>gfx_snake
	stx .src
	sta .src+1
	ldy #$00
	sty .enemydst
	sty .enemycol
	lda (.src),y
	sta .width
	sta enemy_w
	iny
	lda (.src),y
	sta .height
	sta enemy_h
	iny

	ldx #2
	jsr rnd
	lda #<(VIEWPORT+(SCREEN_W*VP_H)-(SCREEN_W*2))
	ldx .height
-	sec
	sbc #SCREEN_W
	dex
	bne -
	clc
	adc result
	sta enemy_pos
	sta .enemydst
	sta .enemycol

--	ldx #$00
-	lda (.src),y
.enemydst=*+1
	sta VIEWPORT,x
	lda #$02
.enemycol=*+1
	sta $9600,x
	iny
	inx
	cpx .width
	bcc -
	lda .enemydst
	clc
	adc #SCREEN_W
	sta .enemydst
	lda .enemycol
	clc
	adc #SCREEN_W
	sta .enemycol
	dec .height
	bne --

	rts

kill_enemy
--	ldx enemy_pos
	ldy enemy_w
-	lda #' '
	sta SCREEN,x
	lda #$00
	sta COLORMEM,x
	inx
	dey
	bne -
	lda enemy_pos
	clc
	adc #SCREEN_W
	sta enemy_pos
	dec enemy_h
	bne --
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
	ldx #VP_W+1	; 'A'-'J'
-	txa
	sta SCREEN+VP_X-1,x
	dex
	bne -

	ldy #$00
	ldx #48
	clc
-	txa
	sta SCREEN+SCREEN_W+VP_X-1,y
	tya
	adc #SCREEN_W
	tay
	inx
	cpx #48+10
	bcc -
	rts

;**************************************
; draw the player status (health/magic/money)
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

+	ldy money
	lda #$00
	jsr $d391
	jsr $dddd
	ldx #$00
-	lda $100,x
	beq +
	sta statusmsg+13,x
	inx
	bne -

	; draw HP, magic, and money
+	ldx #statusmsglen-1
-	lda statusmsg,x
	sta SCREEN+(STATUS_LINE*SCREEN_W),x
	dex
	bpl -

	; draw the gems that the player has
	ldx gemcnt
	beq .done
-	lda #CH_GEM
	sta SCREEN+(STATUS_LINE*SCREEN_W),x
	dex
	bpl -

.done
	rts

;**************************************
; data
titlemsg
!pet $90,"you must find 3 magic gems to restore power to the staff of truth",$0d,$0d,$0d
!pet "press any key to begin"
titlemsglen=*-titlemsg

statusmsg
!scr 83,":   ",88,":    $:    "
statusmsglen=*-statusmsg

hp !byte 10
magick !byte 5
money !byte 100
gemcnt !byte 0

enemy_hp !byte 1
enemy_pos !byte 0
enemy_w !byte 0
enemy_h !byte 0

; graphics
gfx_snake
!byte  6,4	; 6x4
!byte  233,215,208,32,32,32,34,160
!byte  160,32,32,223,32,32,224,227
!byte  227,105,32,32,95,224,105,32

