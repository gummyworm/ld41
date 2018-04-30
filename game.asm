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
!source "values.inc"

SHORT_DELAY=10
LONG_DELAY=90

SCREEN=$1e00
SCREEN_W=22
SCREEN_H=23
COLORMEM=$9600
VP_W=9
VP_H=10
VP_X=8
VP_Y=0
STATUS_LINE=1
LEVEL_LINE=VP_Y+VP_H+1
INPUT_LINE=22
STORE_LINE=VP_Y+1
STORE_COL=VP_X-1
STORE_W=SCREEN_W-STORE_COL
SALE_VAL_COL=SCREEN_W-8
COST_COL=SCREEN_W-5
MSG_LINE=14
MSG_H=7

VIEWPORT=SCREEN+VP_X+(SCREEN_W*VP_Y)
VIEWPORT_COL=COLORMEM+VP_X+SCREEN_W
VIEWPORT_END=SCREEN+VP_X+VP_W+SCREEN_W*SCREEN_H

CH_STAR=42
CH_SWORD=30

; GC: generation chance (1/(2^GC_XXX))
; CH: character
GC_HEART=7
CH_HEART=83

GC_SPELL=6
CH_SPELL=42

GC_MONEY=6
CH_MONEY=36

GC_GEM=1
CH_GEM=$5a

GC_TRAP=6
CH_TRAP=63

GROUND_CHAR=102

GEMS_TO_WIN=3
XP_PER_KILL=10
XP_RAND=2
XP_TO_LVLUP=100

MAX_ENEMIES=2
MAX_FOR_SALE=6

TRAP_DMG=6

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
	jsr clearall

	; display title
	ldx #5
	ldy #0
	jsr $e50c
	ldx #<titlemsg
	ldy #>titlemsg
	jsr puts

	; wait for user to begin
-	jsr $ffe4
	cmp #$00
	beq -

	; what is your name?
	ldx #13
	ldy #0
	jsr $e50c
	ldx #<enternamemsg
	ldy #>enternamemsg
	jsr puts

	ldx #14
	ldy #0
	jsr $e50c
-	jsr $ffe4
	cmp #$00
	beq -
	cmp #$0d
	beq .start
	jsr $ffd2
	jmp -

.start
	ldx $d3
	dex
-	lda SCREEN+(SCREEN_W*14),x
	clc
	adc #'A'-1
	sta name,x
	dex
	bpl -
	lda #$00
	sta name+7
	jsr clearall

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
	jsr enemymove
	jmp mainloop

;**************************************
exclaim lda #'!'
	!byte $2c
rvson   lda #$12
	!byte $2c
rvsoff  lda #$92
	!byte $2c
space   lda #' '
	jsr $ffd2
	rts

;**************************************
setrow
	ldy #$00
	jmp $e50c

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
removeobj
	ldx #7
	jsr rnd
	ldx result
-	lda COLORMEM,x
	and #$0f
	bne +
	lda SCREEN,x
	cmp #' '
	bne .rem
+	dex
	bne -
	; no target found
	ldx #<staresmsg
	ldy #>staresmsg
	jmp puts

.rem    pha
	ldx #<eatsmsg
	ldy #>eatsmsg
	jsr space
	pla
	ldy $d3
	sta ($d1),y
	rts

;**************************************
enemymove
	ldx #0
	stx enemy_idx
.l0	lda enemy_hp,x
	bmi .nextenemy
	beq .nextenemy
	ldx #2
	jsr rnd
	lda result

	jsr msgputsenemy
	jsr space

	ldx #2
	jsr rnd
	lda result
.idle	bne .eat
	ldx #<staresmsg
	ldy #>staresmsg
	jsr puts
	jmp .nextenemy

.eat    ldx #2
	jsr rnd
	lda result
	bne .attack
	jsr removeobj
	jmp .nextenemy

.attack
	ldx #<attacksmsg
	ldy #>attacksmsg
	jsr puts
	ldx enemy_idx
	adc enemy_dmg,x
	jsr harmplayer

.nextenemy
	inc enemy_idx
	ldx enemy_idx
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
	jsr $e50c
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
+       jsr genlevel
	jmp parsecmd

.run	lda $9004
	and #$01
	bne +
	ldx #<runmsg
	ldy #>runmsg
	jsr msgputs
	jsr genlevel
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
	sta spell
	lda spellnames,x
	ldy spellnames+1,x
	tax
	jsr puts
	jsr space
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
	jsr space
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
	lda VIEWPORT,x
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
+	cmp #CH_TRAP
	bne +
	ldx #TRAP_IDX
+	lda VIEWPORT_COL,x
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
.exec	jmp callya

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
TRAP_IDX = 5

taketab
!word takeheart
!word takeenemy
!word takespell
!word takegem
!word takemoney
!word taketrap

casttab
!word casterr
!word castenemy
!word casterr
!word casterr
!word casterr
!word casterr

hittab
!word hiterr
!word hitenemy
!word hiterr
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
	sta VIEWPORT,x
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
taketrap
	ldx #<openboxmsg
	ldy #>openboxmsg
	jsr msgputs
	ldx #2
	jsr rnd
	lda result
	beq takeheart
	ldx #2
	jsr rnd
	lda result
	beq takemoney
	ldx #2
	jsr rnd
	lda result
	beq takespell
	ldx #<trappedmsg
	ldy #>trappedmsg
	jsr msgputs
	adc #TRAP_DMG
	jsr harmplayer
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

	ldx spell
	lda spelltab,x
	sta .anim
	lda spelltab+1,x
	sta .anim+1
.anim=*+1
	jsr $ffff
	rts

;**************************************
hiterr
	ldx #<canttarget
	ldy #>canttarget
	jmp cmderr
hitenemy
	ldx #<hurtmsg
	ldy #>hurtmsg
	jsr msgputs
	jsr putsenemy
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
+	lda VIEWPORT,x
	rts

; highlights the cell whose viewport offset is given in .X
hicell
	lda VIEWPORT,x
	eor #$80
	sta VIEWPORT,x
	rts

;**************************************
win
	jsr clearall
	lda #$66|8
	sta $900f
	ldy #$03
	ldx #(SCREEN_H/2-4)
	jsr $e50c
	ldx #<winmsg
	ldy #>winmsg
	jsr puts
	jmp *

;**************************************
; generate a new level (combat or shop)
genlevel
	ldx #1
	jsr rnd
	lda result
	beq .shop
	jsr genscreen
	rts
.shop   jsr shop
	rts

;**************************************
; generate a new screen of gameplay
!zone genscreen
genscreen
.y=tmp2
.dst=tmp3
.enemycnt=tmp4
	jsr clear

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
	+GENCELL GC_TRAP, CH_TRAP, .next
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

	jsr drawui
	rts

enemiestab
!word gfx_snake
!word gfx_bat

colors
!byte 2
!byte 4
basepositions
!byte <(VIEWPORT+(SCREEN_W*VP_H)-(SCREEN_W*1))
!byte <(VIEWPORT+(SCREEN_W*VP_H)-(SCREEN_W*5))

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
; call the function in (Y/X)
!zone callyx
callyx
	stx .target
	sty .target+1
.target=*+1
	jmp $ffff


;**************************************
; call the function in (Y/A)
!zone callya
callya
	sta .target
	sty .target+1
.target=*+1
	jmp $ffff

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
msgputsenemy
	jsr getenemyname
	jmp msgputs
putsenemy
	jsr getenemyname
	jmp puts
getenemyname
	ldx enemy_idx
	lda enemy_name,x
	asl
	tax
	lda enemynametab,x
	ldy enemynametab+1,x
	tax
	rts

;**************************************
harmenemy
	pha

	; print <ENEMY> TAKES X DAMAGE
	jsr msgputsenemy

	ldx #<takesmsg
	ldy #>takesmsg
	jsr puts

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
	jsr putsenemy
	jsr exclaim
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
	jsr $e50c

	jsr clearall
	ldx #$00
-	lda diemsg,x
	jsr $ffd2
	inx
	cpx #diemsglen
	bne -
	jmp *

;**************************************
; enter the shop
!zone shop
shop
; item flags
.flg_hp=1
.flg_armor=2
.flg_weapon=3
.flg_spell=4
.flg_magick=5
.i=tmp2
.item=tmp3
.selection=tmp2
	jsr clear
	jsr drawstatus

	ldx #$00
	ldy #STORE_COL
	jsr $e50c
	ldx #<storemsg
	ldy #>storemsg
	jsr puts

	; get an item to sell
.l0     lda #$00
	sta .i

	; determine what item to sell at index .item
-	ldx #3
	jsr rnd
	lda result
	beq -

	cmp #numitems
	bcs -
	sta .item
	ldx .i
	sta .forsale,x
	asl
	tax
	lda items-2,x
	ldy items-1,x
	jsr callya

	lda #STORE_LINE
	clc
	adc .i
	tax
	ldy #STORE_COL
	jsr $e50c

	jsr rvson
	lda .i
	jsr putb
	jsr rvsoff
	jsr space

	; display the item and its cost
	lda .item
	asl
	tax
	lda .itemnames-2,x
	ldy .itemnames-1,x
	tax
	jsr puts
	jsr space
	ldy #SALE_VAL_COL
	jsr $e50e
	ldx .i
	lda .forsale_vals,x
	jsr putb
	ldy #COST_COL
	jsr $e50e
	ldx .i
	lda .forsale_costs,x
	jsr putb
	lda #'$'
	jsr $ffd2

	; next row
	inc .i
	lda .i
	cmp #MAX_FOR_SALE
	bcc -

.getitem
	ldx #<shopmsg
	ldy #>shopmsg
	jsr msgputs
-	jsr $ffe4
	cmp #$00
	beq -
	cmp #'R'
	bne +
	ldx #<byemsg
	ldy #>byemsg
	jsr msgputs
	jmp genscreen
+	sec
	sbc #'0'
	bmi -
	cmp #MAX_FOR_SALE
	bcs -
	sta .selection
.confirm
	ldx #<confirmmsg
	ldy #>confirmmsg
	jsr msgputs

	ldx .selection
	lda .forsale,x
	sta .item
	asl
	tax
	lda .itemnames-2,x
	ldy .itemnames-1,x
	tax
	jsr puts
	lda #'?'
	jsr $ffd2

-	jsr $ffe4
	cmp #$00
	beq -
	cmp #'Y'
	beq .buy
	ldx #<okmsg
	ldy #>okmsg
	jsr msgputs
	jmp .getitem

.buy   	ldx .selection
	lda money
	cmp .forsale_costs,x
	bcs +
	ldx #<notenoughmoneymsg
	ldy #>notenoughmoneymsg
	jsr msgputs
	jmp .getitem

+	lda money
	sec
	sbc .forsale_costs,x
	sta money
	lda #$00
	sta .forsale,x
.clritem
	lda .selection
	clc
	adc #STORE_LINE
	tax
	ldy #STORE_COL
	jsr $e50c
	ldx #STORE_W
-	jsr space
	dex
	bne -
	ldx #<thankyoumsg
	ldy #>thankyoumsg
	jsr msgputs

	ldx .item
	lda .forsale_vals,x
	pha
	lda .item
	asl
	tax
	lda .buytab-2,x
	ldy .buytab-1,x
	tax
	pla
	jsr callyx
	jmp .getitem

.health ldx #2
	jsr rnd
	lda result
	adc lvl
	ldx .i
	sta .forsale_vals,x
	adc #HEALTH_COST
	sta .forsale_costs,x
	rts

.armor  ldx #2
	jsr rnd
	adc result
	lsr
	ldx .i
	sta .forsale_vals,x
	asl
	adc #ARMOR_COST
	asl
	sta .forsale_costs,x
	rts

.weapon ldx #2
	jsr rnd
	lda lvl
	adc result
	ldx .i
	sta .forsale_vals,x
	asl
	adc #WEAPON_COST
	asl
	sta .forsale_costs,x
	rts

.spell  ldx #2
	jsr rnd
	adc result
	ldx .i
	sta .forsale_vals,x
	asl
	asl
	adc #SPELL_COST
	sta .forsale_costs,x
	rts

.buyhealth
	clc
	adc hp
	sta hp
	rts
.buyarmor
	sta armor
	rts
.buysword
	sta dmg
	rts
.buyspell
	rts

items   !word .health, .armor, .weapon, .spell
numitems=(*-items)/2

.forsale=freebuff
.forsale_vals=freebuff+MAX_FOR_SALE
.forsale_costs=freebuff+MAX_FOR_SALE*2

.itemnames
!word .item_health, .item_armor, .item_sword, .item_spell
.buytab
!word .buyhealth, .buyarmor, .buysword, .buyspell
.item_health !pet "health",0
.item_armor !pet "armor",0
.item_sword !pet "sword",0
.item_spell !pet "spell",0

;**************************************
!zone clear
clearall
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

clear
	ldx #SCREEN_W*(VP_H+1)
-	lda #' '
	sta SCREEN-1,x
	lda #$00
	sta $9600,x
	dex
	bne -
	rts

;**************************************
drawui
	ldx #VP_W+1	; 'A'-'J'
-	txa
	sta SCREEN+VP_X-1,x
	lda #$06
	sta COLORMEM+VP_X-1,x
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

	; draw name
	ldx #STATUS_LINE-1
	jsr setrow
	ldx #<name
	ldy #>name
	jsr puts

	; draw HP
	ldx #STATUS_LINE
	jsr setrow
	ldx #<statusmsg1
	ldy #>statusmsg1
	jsr puts
	lda hp
	jsr putb

	; draw magick
	ldx #STATUS_LINE+1
	jsr setrow
	ldx #<statusmsg2
	ldy #>statusmsg2
	jsr puts
	lda magick
	jsr putb

	; draw armor and attack damage
	ldx #STATUS_LINE+2
	jsr setrow
	ldx #<statusarmor
	ldy #>statusarmor
	jsr puts
	lda armor
	jsr putb
	ldx #STATUS_LINE+3
	jsr setrow
	ldx #<statusdmg
	ldy #>statusdmg
	jsr puts
	lda basedmg
	jsr putb

	; draw money
	ldx #STATUS_LINE+4
	jsr setrow
	ldx #<statusmsg3
	ldy #>statusmsg3
	jsr puts
	lda money
	jsr putb

	; draw XP, level
	ldx #LEVEL_LINE
	jsr setrow
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
	beq +
-	lda #CH_GEM
	sta SCREEN,x
	dex
	bne -

	; color the status area
+
--	ldy #VP_X-1
-       lda #$02
	sta COLORMEM,x
	inx
	dey
	bpl -
	txa
	clc
	adc #SCREEN_W-VP_X
	tax
	cpx #SCREEN_W*(VP_Y+VP_H+1)
	bcc --

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
	jsr space
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
	jsr $e50c
	ldx tmp4
	ldy tmp5
	jsr puts
	rts

;**************************************
; swordrain does large damage to all enemies
swordrain
	ldx #SWORDRAIN_DMG
	lda #CH_SWORD
	jmp fallspell
;**************************************
; starfall does massive damage to all enemies
!zone starfall
starfall
.line_bak=freebuff
	ldx #STARFALL_DMG
	lda #CH_STAR
fallspell
	sta .ch
	txa
	pha
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

	jsr shortdelay
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
	pla
	jmp dmgall

;**************************************
; damage all enemies for the amount in .A
!zone dmgall
dmgall
	sta .dmg
	ldx #MAX_ENEMIES-1
	stx enemy_idx
.l0     ldx enemy_idx
	lda enemy_hp,x
	beq +
	bmi +
.dmg=*+1
	lda #$00
	jsr harmenemy
+	dec enemy_idx
	bpl .l0
	rts

;**************************************
; delay functions (clobber .A and .Y)
shortdelay
	ldy #SHORT_DELAY
	!byte $2c
longdelay
	ldy #LONG_DELAY
	lda #10
-	cmp $9004
	bne *-3
	dey
	bne -
	rts

;**************************************
flash
	lda #FLASH_DMG
	jsr dmgall
	jsr flash2
flash2
	lda #VP_H
	sta tmp0
	ldx #SCREEN_W*(VP_H)+VP_W-SCREEN_W
--	ldy #VP_W
-	lda VIEWPORT,x
	eor #$80
	sta VIEWPORT,x
	dex
	dey
	bpl -
	txa
	sec
	sbc #SCREEN_W-VP_W-1
	tax
	dec tmp0
	bpl --
	jsr longdelay
	rts

;**************************************
; data
!zone data
titlemsg
!pet $90,"you must find 3 magic gems to restore power to the staff of truth",$0d,$0d,$0d
!pet "press any key to begin",0

diemsg !pet $90,"you have died!"
diemsglen=*-diemsg

harmmsg1 !pet "you receive ",0
damagemsg !pet " damage!",0

hurtmsg !pet "you hit the ",0
attacksmsg !pet "attacks!",0
castmsg !pet "you cast a spell",0
killmsg !pet "you kill the ",0
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
statusmsg1 !pet 115,":",0
statusmsg2 !pet 120,":",0
statusmsg3 !pet "$:",0

!scr 83,":    ",88,":    $:    "
statusmsglen=*-statusmsg
statuslvl   !pet "lvl:",0
statusxp    !pet " xp:",0
statusarmor !pet 113,":",0
statusdmg   !pet 97,":",0

enternamemsg !pet "what is your name?",0
runmsg !pet "you ran away!",0
runfailmsg !pet "couldn't get away!",0
takesmsg !pet " takes ",0
openboxmsg !pet "you open the box...",0
trappedmsg !pet "it was trapped!",0
armormsg !pet "armor",0
shopmsg !pet "buy somethin'?",0
confirmmsg !pet "buy the ",0
notenoughmoneymsg !pet "not enough $!",0
okmsg !pet "ok",0
thankyoumsg !pet "thank you!",0

spelltab
!word starfall
!word swordrain
!word flash

spellnames
!word starfallname
!word swordrainname
!word flashname
numspells=(*-spellnames)/2

starfallname  !pet "starfall",0
swordrainname !pet "swordrain",0
flashname !pet "flash",0
storemsg !pet "shop",0
byemsg !pet "bye",0
eatsmsg !pet "eats the ",0
staresmsg !pet "stares at you",0

hp !byte 100
magick !byte 5
armor !byte 0
money !byte 10
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
enemy_col !byte 2,6
enemy_name !byte 0,1

enemynametab
!word snakename
!word batname

enemynames
snakename !pet "snake",0
batname !pet "bat",0

lvl !byte 0
xp !byte 0
name !fill 8,' '

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

spell !byte 0

freebuff

prg_size=*-basicstub
remaining_bytes=SCREEN - *
