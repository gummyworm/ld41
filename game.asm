!to "game.prg",cbm

;**************************************
; zeropage
rndval=$f0
result=$f2

tmp0=$fa
tmp1=$fb
tmp2=$fc
tmp3=$fd
tmp4=$fe
tmp5=$22
cellpos=$bb
enemy_idx=$bc

width=tmp0
height=tmp1
src=tmp2

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
STATUS_LINE=11
INPUT_LINE=22
MSG_LINE=16
MSG_H=5

VIEWPORT=SCREEN+VP_X+SCREEN_W
VIEWPORT_COL=COLORMEM+VP_X+SCREEN_W
VIEWPORT_END=SCREEN+VP_X+VP_W+SCREEN_W*SCREEN_H

CH_STAR=42
CH_SWORD=30

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

GEMS_TO_WIN=3
XP_PER_KILL=10
XP_RAND=2
XP_TO_LVLUP=100

MAX_ENEMIES=2

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

;**************************************
*=$1001
basicstub
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
	jsr swordrain
	jsr drawstatus
	jsr parsecmd
	jsr enemymove
	jmp mainloop

;**************************************
;loads the data associated with the enemy in <X/>Y
!zone loadenemy
loadenemy
src=tmp2
	stx src
	sty src+1
	sta .index
	; get random HP/damage for monster
	ldx #2
	jsr rnd
	lda result
	sta enemy_dmg
	ldx #2
	jsr rnd
	lda result

.index=*+1
	ldx #$00
	sta enemy_hp,x
	ldy #$00
	lda (src),y
	sta width
	sta enemy_w,x
	iny
	lda (src),y
	sta height
	sta enemy_h,x
	iny
	lda (src),y
	adc enemy_hp
	sta enemy_hp,x
	iny
	lda (src),y
	adc enemy_dmg
	sta enemy_dmg,x
	iny

	rts

;**************************************
addenemy
	ldx #MAX_ENEMIES
-	lda enemy_dmg,x
	sta enemy_dmg+1,x
	lda enemy_w,x
	sta enemy_w+1,x
	lda enemy_h,x
	sta enemy_h+1,x
	lda enemy_hp,x
	sta enemy_hp+1,x
	rts

;**************************************
enemymove
.i=tmp3
	ldx #0
	stx .i
.l0	lda enemy_hp,x
	bmi +
	beq +
	ldx #2
	jsr rnd
	lda result

	ldx .i
	adc enemy_dmg,x
	jsr harmplayer
+	inc .i
	ldx .i
	cpx #MAX_ENEMIES
	bcc .l0
	rts

;**************************************
; get user input and parse the player's command
parsecmd
.input=SCREEN+SCREEN_W*INPUT_LINE
.action=tmp2
	; clear the input line
	lda #' '
	ldx #SCREEN_W
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

.chkrun
	cpy #'R'	; Run
	bne .chktake
	lda enemy_hp
	beq +
	bpl .run
+	jsr genscreen
	jmp parsecmd

.run	lda $9004
	and #$01
	bne +
	ldx #<runmsg
	ldy #>runmsg
	jsr msgputs
	jsr genscreen
	rts
+	ldx #<runfailmsg
	ldy #>runfailmsg
	jsr msgputs
	rts

.chktake
	cpy #'T'
	bne .chkcast
	ldx #<take
	ldy #>take
	bne .printaction

.chkcast
	cpy #'C'
	bne .chkhit
	ldx #<cast
	ldy #>cast
	jsr puts
	lda #$80
	eor .input
	sta .input
.getspell
	jsr $ffe4
	cmp #'0'
	bcc .getspell
	cmp #'0'+numspells
	bcs .getspell
	sec
	sbc #'0'
	asl
	tax
	lda spellnames,x
	ldy spellnames+1,x
	tax
	jsr puts
	lda #' '
	jsr $ffd2
	jmp .getcoord
.chkhit
	cpy #'H'
	bne +
	ldx #<hit
	ldy #>hit
	bne .printaction
+	jmp parsecmd	; invalid character

.printaction
	stx tmp0
	sty tmp0+1
	jsr puts
	lda #' '
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
	stx cellpos

	jsr $ffe4
	beq *-3
	ldx cellpos
	cmp #$0d
	beq +
	pla
	jsr hicell	; unhighlight
	jmp parsecmd	; player cancelled action

+	jsr hicell	; unhighlight
	lda VIEWPORT-SCREEN_W,x
	cmp #' '
	bne +
	ldx #<nothingmsg
	ldy #>nothingmsg
	jsr msgputs
	pla
	jmp parsecmd

+	pla
	cmp #CH_HEART
	bne +
	ldx #HEART_IDX
+	cmp #CH_GEM
	bne +
	ldx #GEM_IDX
+	cmp #CH_SPELL
	bne +
	ldx #SPELL_IDX
+	cmp #CH_MONEY
	bne +
	ldx #MONEY_IDX
+	lda VIEWPORT_COL-SCREEN_W,x
	and #$0f
	beq +	; black= not enemy, !black= enemy
	ldx #-1
-	inx
	cmp enemy_col,x
	bne -
	stx enemy_idx
	ldx #ENEMY_IDX
	
+	txa
	asl
	tax

	lda .action
	cmp #'H'
	bne +
	lda hittab,x
	ldy hittab+1,x
	bne .exec
+	cmp #'T'
	bne +
	lda taketab,x
	ldy taketab+1,x
	bne .exec
+	cmp #'C'
	bne +
	lda casttab,x
	ldy casttab+1,x
	bne .exec
.exec	sta .target
	sty .target+1
.target=*+1
	jmp $0000

take
!pet "take",0
hit
!pet "hit",0
cast
!pet "cast ",0


HEART_IDX = 0
ENEMY_IDX = 1
SPELL_IDX = 2
GEM_IDX = 3
MONEY_IDX = 4

taketab
!word takeheart
!word takeenemy
!word takespell
!word takegem
!word takemoney

casttab
!word casterr
!word castenemy
!word casterr
!word casterr
!word casterr

hittab
!word hiterr
!word hitenemy
!word hiterr
!word hiterr
!word hiterr

;**************************************
cmderr
	jsr msgputs
	jmp parsecmd

;**************************************
clrcell
	lda #' '
	ldx cellpos
	sta VIEWPORT-SCREEN_W,x
	rts
takeheart
	ldx #4
	jsr rnd
	lda result
	adc #3
	tax
	adc hp
	sta hp
	lda #115
	jsr printtake
	jmp clrcell
takeenemy
	ldx #<canttake
	ldy #>canttake
	jmp cmderr
takegem
	inc gemcnt
	lda gemcnt
	cmp #GEMS_TO_WIN
	bcc +
	jsr win
+	ldx #1
	lda #CH_GEM
	jsr printtake
	jmp clrcell
takespell
	inc magick
	ldx #1
	lda #CH_SPELL
	jsr printtake
	jmp clrcell
takemoney
	inc money
	ldx #1
	lda #CH_MONEY
	jsr printtake
	jmp clrcell

;**************************************
casterr
	ldx #<canttarget
	ldy #>canttarget
	jmp cmderr
castenemy
	ldx #<castmsg
	ldy #>castmsg
	jsr msgputs
	ldx dmg
	jsr rnd
	lda result
	jmp harmenemy

;**************************************
hiterr
	ldx #<canttarget
	ldy #>canttarget
	jmp cmderr
hitenemy
	ldx #<hurtmsg
	ldy #>hurtmsg
	jsr msgputs
	ldx dmg
	jsr rnd
	lda result
	adc basedmg
	jmp harmenemy

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
win
	jsr clear
	lda #$66|8
	sta $900f
	ldy #$03
	ldx #(SCREEN_H/2-4)
	clc
	jsr $fff0
	ldx #<winmsg
	ldy #>winmsg
	jsr puts
	jmp *

;**************************************
; generate a new screen of gameplay
!zone genscreen
genscreen
.y=tmp2
.dst=tmp3
.enemycnt=tmp4
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

	lda #MAX_ENEMIES-1
	sta .enemycnt
.genenemy
	lda .enemycnt
	pha
	asl
	tax
	lda enemiestab,x
	ldy enemiestab+1,x
	tax
	pla
	jsr loadenemy

	; get destination to draw for ground enemy
.drawenemy
	ldx #2
	jsr rnd
	ldx .enemycnt
	lda basepositions,x
	ldx height
-	sec
	sbc #SCREEN_W
	dex
	bne -
	clc
	adc result
	ldx .enemycnt
	sta enemy_pos,x
	sta .enemydst
	sta .enemycol
	lda enemy_col,x
	sta .drawcol

--	ldx #$00
-	lda (src),y
.enemydst=*+1
	sta VIEWPORT,x
.drawcol=*+1
	lda #$02
.enemycol=*+1
	sta $9600,x
	iny
	inx
	cpx width
	bcc -
	lda .enemydst
	clc
	adc #SCREEN_W
	sta .enemydst
	lda .enemycol
	clc
	adc #SCREEN_W
	sta .enemycol
	dec height
	bne --
	dec .enemycnt
	bpl .genenemy
	rts

enemiestab
!word gfx_snake
!word gfx_bat
colors
!byte 2
!byte 4
basepositions
!byte <(VIEWPORT+(SCREEN_W*VP_H)-(SCREEN_W*2))
!byte <(VIEWPORT+(SCREEN_W*VP_H)-(SCREEN_W*6))

;**************************************
kill_enemy
--	ldx enemy_idx
	lda enemy_pos,x
	ldy enemy_w,x
	tax
-	lda #' '
	sta SCREEN,x
	lda #$00
	sta COLORMEM,x
	inx
	dey
	bne -
	ldx enemy_idx
	lda enemy_pos,x
	clc
	adc #SCREEN_W
	sta enemy_pos,x
	dec enemy_h,x
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
scrollmsg
	lda #MSG_H
	sta tmp0
	ldx #$00
--	ldy #SCREEN_W
-	lda SCREEN+SCREEN_W*(MSG_LINE+1),x
	sta SCREEN+SCREEN_W*MSG_LINE,x
	inx
	dey
	bpl -
	dec tmp0
	bne --
	jsr clrmsg
	rts

;**************************************
harmenemy
	pha
	pla
	sta tmp0

	ldx enemy_idx
	lda enemy_hp,x
	sec
	sbc tmp0
	sta enemy_hp,x

	lda tmp0
	jsr putb
	ldx #<damagemsg
	ldy #>damagemsg
	jsr puts

	ldx enemy_idx
	lda enemy_hp,x
	bmi +
	bne ++
+	ldx #<killmsg
	ldy #>killmsg
	jsr msgputs
	ldx #<gainedmsg
	ldy #>gainedmsg
	jsr msgputs
	ldx #XP_RAND
	jsr rnd
	lda result
	adc #XP_PER_KILL
	pha
	adc xp
	sta xp
	cmp #XP_TO_LVLUP
	bcc +
	jsr levelup

+	pla
	jsr putb
	ldx #<xpmsg
	ldy #>xpmsg
	jsr puts

	jsr kill_enemy
++	rts

;**************************************
levelup
	inc lvl
	inc basedmg
	inc hp
	rts

;**************************************
harmplayer
	pha
	sta tmp0
	lda hp
	sec
	sbc tmp0
	sta hp
	ldx #<harmmsg1
	ldy #>harmmsg1
	jsr msgputs
	pla
	jsr putb
	ldx #<damagemsg
	ldy #>damagemsg
	jsr puts
	lda hp
	bmi die
	rts
die
	lda #$22
	sta $900f

	ldx #(SCREEN_H/2)
	ldy #(SCREEN_W/2 - diemsglen/2)
	jsr $fff0

	jsr clear
	ldx #$00
-	lda diemsg,x
	jsr $ffd2
	inx
	cpx #diemsglen
	bne -
	jmp *

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
	ldx #SCREEN_W
	lda #' '
-	sta SCREEN+(SCREEN_W*STATUS_LINE),x
	dex
	bpl -

	ldx #STATUS_LINE
	ldy #0
	clc
	jsr $fff0

	ldx #<statusmsg1
	ldy #>statusmsg1
	jsr puts
	lda hp
	jsr putb
	ldx #<statusmsg2
	ldy #>statusmsg2
	jsr puts
	lda magick
	jsr putb
	ldx #<statusmsg3
	ldy #>statusmsg3
	jsr puts
	lda money
	jsr putb

	ldx #STATUS_LINE+1
	ldy #0
	clc
	jsr $fff0

	; draw XP and level
	ldx #<statuslvl
	ldy #>statuslvl
	jsr puts
	lda lvl
	jsr putb
	ldx #<statusxp
	ldy #>statusxp
	jsr puts
	lda xp
	jsr putb

	; draw the gems that the player has
	ldx gemcnt
	beq .done
-	lda #CH_GEM
	sta SCREEN,x
	dex
	bne -

.done	rts

;**************************************
puts
	stx tmp0
	sty tmp0+1
	ldy #$00
-	lda (tmp0),y
	beq +
	jsr $ffd2
	iny
	bne -
+	rts

;**************************************
; print the take message for the quantity given in .X and the object (char) in
; .A
printtake
	pha
	txa
	pha

	ldx #<takemsg
	ldy #>takemsg
	jsr msgputs
	pla
	jsr putb
	lda #' '
	jsr $ffd2
	pla
	jsr $ffd2
	rts

;**************************************
clrmsg
	lda #' '
	ldx #SCREEN_W-1
-	sta SCREEN+(SCREEN_W*(MSG_LINE+MSG_H)),x
	dex
	bpl -
	rts

;**************************************
putb
	tay
	lda #$00
	jsr $d391
	jsr $dddd
	ldx #<$100
	ldy #>$100
	jsr puts
	rts

;**************************************
msgputs
	stx tmp4
	sty tmp5
	jsr scrollmsg
	ldx #MSG_LINE+MSG_H-1
	ldy #$00
	clc
	jsr $fff0
	ldx tmp4
	ldy tmp5
	jsr puts
	rts

swordrain
	lda #CH_SWORD
	!byte $2c
;**************************************
; do the animation for the starfall spell
!zone starfall
starfall
.line_bak=freebuff
	lda #CH_STAR
	sta .ch
	ldx #VP_W-1
.l0	ldy #VP_W-1
.l1	lda VIEWPORT,x
	sta .line_bak,y
.ch=*+1
	lda #$00
	sta VIEWPORT,x
	dex
	dey
	bpl .l1

	ldy #30
.dly	lda #$03
	cmp $9004
	bne *-3
	dey
	bpl .dly

	txa
	clc
	adc #VP_W
	tax
	ldy #VP_W-1
.restore
	lda .line_bak,y
	sta VIEWPORT,x
	dex
	dey
	bpl .restore

	txa
	clc
	adc #SCREEN_W+VP_W
	tax
	cpx #SCREEN_W*VP_H
	bcc .l0
	rts

;**************************************
; data
!zone data
titlemsg
!pet $90,"you must find 3 magic gems to restore power to the staff of truth",$0d,$0d,$0d
!pet "press any key to begin"
titlemsglen=*-titlemsg

diemsg !pet $90,"you have died!"
diemsglen=*-diemsg

harmmsg1 !pet "you receive ",0
damagemsg !pet " damage!",0

hurtmsg !pet "you hit for ",0
castmsg !pet "you cast for ",0
killmsg !pet "you kill the monster!",0
takemsg !pet "picked up ",0
gainedmsg !pet "gained ",0
xpmsg !pet "xp",0

canttake !pet "you can't take that!",0
canttarget !pet "not a valid target!",0

winmsg
!pet $05,"congratulations!",$0d
!pet "you have collected 3",$0d,"gems. "
!pet "now power may berestored at last to",$0d,"the magic staff",0

nothingmsg
!pet "there is nothing there",0

statusmsg
statusmsg1 !pet "hp:",0
statusmsg2 !pet " mgk:",0
statusmsg3 !pet " $:",0

!scr 83,":    ",88,":    $:    "
statusmsglen=*-statusmsg
statuslvl: !pet "lvl:",0
statusxp: !pet " xp:",0

runmsg !pet "you ran away!",0
runfailmsg !pet "couldn't get away!",0

spellnames
!word starfallname
!word swordrainname
numspells=(*-spellnames)/2
starfallname  !pet "starfall",0
swordrainname !pet "swordrain",0

hp !byte 100
magick !byte 5
money !byte 100
dmg !byte 2 	; max bonus damage (2^n)
basedmg !byte 1 ; min damage
spelldmg !byte 5 ; spell max damage (2^n)
gemcnt !byte 0

; enemy tables: entries contain 1 byte per enemy
enemy_hp !byte 1,0
enemy_pos !byte 0,0
enemy_w !byte 0,0
enemy_h !byte 0,0
enemy_dmg !byte 0,0
enemy_col !byte 2,3

lvl !byte 0
xp !byte 0

; graphics
gfx_snake
!byte  6,4	; 6x4
!byte  5	; max HP
!byte  2	; base damage
!byte  233,215,208,32,32,32,34,160
!byte  160,32,32,223,32,32,224,227
!byte  227,105,32,32,95,224,105,32

gfx_bat
!byte  6,3	; 6x4
!byte  2	; max HP
!byte  2	; base damage
!byte  233,223,223,233,233,223,105,95
!byte  174,174,105,95,32,32,34,34,32,32

freebuff

prg_size=*-basicstub
