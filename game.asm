!to "game.prg",cbm
!source "zp.inc"
!source "values.inc"
!source "macros.inc"

;**************************************
*=$1001
basicstub
!word $100b
!word 2018
!byte $9e
!text "4109",0
!word 0
	jmp getname
start
	jsr clearall
	jsr drawstatus
	jsr genscreen
mainloop
	lda armorturns
	beq +
	dec armorturns
	bne +
	lda unbuffedarmor
	sta armor
	ldx #<spellendsmsg
	ldy #>spellendsmsg
	jsr msgputs

+	jsr drawstatus
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
	jmp $ffd2

;**************************************
setrow
	inc $cc
	ldy #$00
	jmp $e50c

;**************************************
getb
	lda #$00
	sta $cc
-	jsr $ffe4
	beq -
	inc $cc
	rts

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
	lda (src),y
	sta enemy_name,x
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
.idle	bne .attack
	ldx #<staresmsg
	ldy #>staresmsg
	jsr puts
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
.row=tmp3
.col=tmp4
	; clear the input line
	lda #' '
	ldx #SCREEN_W
-	sta .input,x
	dex
	bpl -

	ldx #INPUT_LINE
	ldy #0
	sty $cc
	jsr $e50c
-	jsr getb
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
	beq .chkcast
	jmp .printaction

.chkcast
	cpy #'C'
	bne .chkhit
	ldx #<castcmd
	ldy #>castcmd
	jsr puts
	lda #$80
	eor .input
	sta .input
.getspell
	jsr getb
	cmp #'0'
	bcc .getspell
	cmp #'0'+numspells
	bcs .getspell
	sec
	sbc #'0'
	asl
	tax
	tay

	; check if player has learned selected spell
	lda #$00
	sec
-	rol
	dey
	bpl -
	and learnedspells
	beq .getspell

	stx spell
	jsr putsspell
	jsr space
	jmp cast
.chkhit
	cpy #'H'
	beq .confirmhit
	jmp parsecmd

.confirmhit
	; highlight random cells until the player confirms a "HIT"
	ldx #<hit
	ldy #>hit
	jsr puts
--	ldx #3
	jsr rnd
	lda result
	pha
	ldx #3
	jsr rnd
	ldy result
	pla
	tax
	jsr getcell
	stx cellpos
	jsr hicell
	lda #40
	sta tmp0
-	lda #$3f
	lda $9004
	bne *-3
	jsr $ffe4
	cmp #$0d
	beq .doaction2
	dec tmp0
	bne -
	jsr unhicell
	jmp --

.printaction
	stx tmp0
	sty tmp0+1
	jsr puts
	jsr space
	lda #$80
	eor .input
	sta .input

.getcoord
	jsr getb
	cmp #'A'
	bcc .getcoord
	cmp #'A'+VP_W+1
	bcs .getcoord
	jsr $ffd2
	sec
	sbc #'A'
	pha

-	jsr getb
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
	stx .col
	sty .row

	jsr getcell
	stx cellpos
	jsr hicell

.confirmorcancel
	jsr getb

.doaction2
	pha
	jsr unhicell	; unhighlight
	pla

	cmp #$14
	bne +
.parsecmd2
	jmp parsecmd
+	cmp #$0d
	beq .doaction

.checkmove
	ldx .col
	ldy .row
	; check cursor keys
	cmp #$11	; down
	bne +
	iny
	cpy #VP_H
	bcc +
	dey
+	cmp #$91	; up
	bne +
	dey
	bpl +
	iny

+	cmp #$1d	; right
	bne +
	inx
	cpx #VP_W+1
	bcc +
	dex
+	cmp #$9d	; left
	bne .updatepos
	dex
	bpl .updatepos
	inx

.updatepos
	stx .col
	sty .row
	jsr getcell
	stx cellpos
	txa
	jsr hicell	; highlight new position
	jmp .confirmorcancel

.doaction
	ldx cellpos
	lda VIEWPORT,x
	cmp #' '
	beq +
	cmp #GROUND_CHAR
	bne ++
+	ldx #<nothingmsg
	ldy #>nothingmsg
	jmp msgputs

	; get the type of cell that the action applies to (heart, enemy, etc.)
++	cmp #CH_HEART
	bne +
	ldy #HEART_IDX
+	cmp #CH_GEM
	bne +
	ldy #GEM_IDX
+	cmp #CH_SPELL
	bne +
	ldy #SPELL_IDX
+	cmp #CH_MONEY
	bne +
	ldy #MONEY_IDX
+	cmp #CH_TRAP
	bne +
	ldy #TRAP_IDX
+	cmp #CH_SCROLL
	bne +
	ldy #SCROLL_IDX
+	lda VIEWPORT_COL,x
	and #$0f
	beq ++	; black= not enemy, !black= enemy
	ldy #$01
	cpx #SKY_ENEMY_MAXPOS
	bcc +
	dey
+	sty enemy_idx
	ldy #ENEMY_IDX

	; look up the handler for the action and cell type
++	tya
	asl
	tax
	lda .action
	cmp #'H'
	bne ++
	cpy #ENEMY_IDX
	bne +
	jmp hiterr
+	jmp hitenemy

++	cmp #'T'
	bne .exec
	lda taketab,x
	ldy taketab+1,x
.exec	jmp callya

take
!pet "take",0
hit
!pet "hit",0
castcmd
!pet "cast ",0

HEART_IDX = 0
ENEMY_IDX = 1
SPELL_IDX = 2
GEM_IDX = 3
MONEY_IDX = 4
TRAP_IDX = 5
SCROLL_IDX = 6

taketab
!word takeheart
!word takeenemy
!word takespell
!word takegem
!word takemoney
!word taketrap
!word takescroll

;**************************************
cmderr
	jsr msgputs
	jmp parsecmd

;**************************************
clrcell
	lda #' '
	ldx cellpos
	sta VIEWPORT,x
	jmp sfx_take

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

takescroll
	ldx #3
	jsr rnd
	ldx result
	cpx #numspells
	bcs takescroll

	sec
	lda #$00
-	rol
	dex
	bpl -
	ora learnedspells
	sta learnedspells

	ldx #<learnedmsg
	ldy #>learnedmsg
	jsr msgputs

	lda result
	asl
	tax
	jsr putsspell
	jmp clrcell

;**************************************
cast
	ldx #<castmsg
	ldy #>castmsg
	jsr msgputs

	;jsr sfx_spell2

	ldx spell
	lda spelltab,x
	sta .anim
	lda spelltab+1,x
	sta .anim+1
.anim=*+1
	jmp $ffff

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
	jsr sfx_hit
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

;**************************************
; unhighlights the last highlighted cell
!zone highlight
unhicell
.unhi
.lastpos=*+1
	ldx #$00
.lastchar=*+1
	lda #$00
	sta VIEWPORT,x
.lastcol=*+1
	lda #$00
	sta VIEWPORT_COL,x
	rts

;**************************************
; highlights the cell whose viewport offset is given in .X
hicell
	stx .lastpos
	lda VIEWPORT,x
	sta .lastchar
	lda VIEWPORT_COL,x
	and #$0f
	sta .lastcol
	cmp #$01
	bne +
	; target-to-highlight is invis
	lda #$a0
	sta VIEWPORT,x
	lda #$00
	sta VIEWPORT_COL,x
	rts

+	lda VIEWPORT,x
	eor #$80
	sta VIEWPORT,x
	rts

;**************************************
!zone win
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
	jmp genscreen
.shop   jmp shop

;**************************************
gencell
	+GENCELL GC_TRAP, CH_TRAP, .next
gencell_notrap
	+GENCELL GC_HEART, CH_HEART, .next
	+GENCELL GC_SPELL, CH_SPELL, .next
	+GENCELL GC_MONEY, CH_MONEY, .next
	+GENCELL GC_SCROLL, CH_SCROLL, .next
	+GENCELL2 GC_GEM, CH_GEM, .next
.next   rts

;**************************************
; generate a new screen of gameplay
!zone genscreen
genscreen
.y=tmp2
.dst=tmp3
.enemycnt=tmp4
.invis=freebuff
	jsr clear

	lda #$00
	sta .invis

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
.l1     jsr gencell
	cmp #$00
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
	jsr getenemycolor
	ldx .enemycnt
	sta enemy_col,x
	cmp #$01
	bne +
	inc .invis
+	sta .drawcol

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

	lda .invis
	beq +
	ldx #<invisinroommsg
	ldy #>invisinroommsg
	jsr msgputs
+	rts

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
	jmp clrmsg

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
putsspell
	lda spellnames,x
	ldy spellnames+1,x
	tax
	jsr puts
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

	jmp kill_enemy
++	rts

;**************************************
levelup
	inc lvl
	inc basedmg
	inc hp
	rts

;**************************************
harmplayer
	sec
	sbc armor
	bpl +
	lda #$00
+	pha
	sta tmp0
	lda hp
	sec
	sbc tmp0
	sta hp

	lda #$02|$08|(1<<4)
	sta $900f
	jsr sfx_harm
	lda #$03|$08|(1<<4)
	sta $900f

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
-	jsr getb
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

-	jsr getb
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
	sta unbuffedarmor
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
	inc $cc
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
	jmp $ffd2

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
	jmp puts

;**************************************
msgputs
	stx tmp4
	sty tmp5
	jsr scrollmsg
	ldx #MSG_LINE+MSG_H-1
	jsr setrow
	ldx tmp4
	ldy tmp5
	jmp puts

;**************************************
; swordrain does large damage to all enemies
swordrain
.snd=tmp0
	lda #SWORDRAIN_COST
	jsr updatemana
	lda #$c4
	sta .snd
	ldx #SWORDRAIN_DMG
	lda #CH_SWORD
	jmp fallspell
;**************************************
; starfall does massive damage to all enemies
!zone starfall
starfall
.line_bak=freebuff
.snd=tmp0
	lda #STARFALL_COST
	jsr updatemana
	lda #$ff
	sta .snd
	ldx #STARFALL_DMG
	lda #CH_STAR
fallspell
	sta .ch
	txa
	pha
	ldx #$ff
	stx $900e
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

	lda .snd
	sta $900c
	sec
	sbc #5
	sta .snd

	jsr middelay
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
	jsr sfx_clear
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
veryshortdelay
	ldy #$01
	!byte $2c
middelay
	ldy #MID_DELAY
	!byte $2c
shortdelay
	ldy #SHORT_DELAY
	!byte $2c
longdelay
	ldy #LONG_DELAY
	!byte $2c
verylongdelay
	ldy #VLONG_DELAY
-	lda $9004
	bne *-3
	dey
	bne -
	rts

;**************************************
getenemycolor
	ldx #3
	jsr rnd
	lda result
	bne +
	adc #$01
+	rts

;**************************************
; foreachvp calls the function in (Y/X) char in the viewport
foreachvp
	stx .fn
	sty .fn+1
	lda #VP_H
	sta tmp0
	ldx #SCREEN_W*(VP_H)+VP_W-SCREEN_W

--	ldy #VP_W
-	lda VIEWPORT,x
.fn=*+1
	jsr $ffff
	dex
	dey
	bpl -
	txa
	sec
	sbc #SCREEN_W-VP_W-1
	tax
	dec tmp0
	bpl --
	rts

;**************************************
updatemana
	sta tmp0
	lda magick
	sec
	sbc tmp0
	bpl +
	ldx #<nomanamsg
	ldy #>nomanamsg
	pla		; eat return address
	pla
	jmp msgputs
+	sta magick
	rts

;**************************************
!zone flash
flash
	lda #FLASH_COST
	jsr updatemana
	lda #FLASH_DMG
	jsr dmgall
	ldx #<flashfn
	ldy #>flashfn
	jsr foreachvp
	jsr longdelay
	jsr sfx_spell2
	ldx #<flashfn
	ldy #>flashfn
	jmp foreachvp

flashfn
	lda VIEWPORT,x
	eor #$80
	sta VIEWPORT,x
	rts

;**************************************
!zone skin
skin
	lda #SKIN_COST
	lda #SKIN_ARMOR_BONUS
	clc
	adc lvl
	sta armor
	lda #SKIN_TURNS
	sta armorturns
	jmp sfx_spell2

;**************************************
sfx_hit
	ldy #$00
	!byte $2c
sfx_harm
	ldy #$01
	lda #$af
	sta $900d
	!byte $2c
sfx_take
	ldy #$02
	lda #$af
	sta $900a,y
	lda #$09
	sta $900e
	jsr verylongdelay
	jmp sfx_clear

;**************************************
sfx_spell1
	jsr sfx_clear
	ldx #$af
	stx $900e
-	jsr veryshortdelay
	txa
	sta $900c
	sta $900e
	dex
	bmi -
	lda #$00
	jmp sfx_clear

;**************************************
sfx_spell2
	jsr sfx_clear
	ldx #$ff
	stx $900e
-	jsr veryshortdelay
	txa
	sta $900a
	dex
	bmi -
	lda #$00
	jmp sfx_clear

;**************************************
sfx_clear
	lda #$00
	ldx #$0e-$0a
-	sta $900a,x
	dex
	bpl -
	rts

;**************************************
!zone eye
eye
	lda #EYE_COST
	ldx #<eyefn
	ldy #>eyefn
	jmp foreachvp
eyefn
	lda VIEWPORT_COL,x
	and #$0f
	cmp #1
	bne +
	lda #$02
	sta VIEWPORT_COL,x
	rts
+	lda VIEWPORT,x
	cmp #CH_TRAP
	bne .done
	stx tmp1
	jsr gencell_notrap
	cmp #$00
	bne +
	lda #CH_HEART
+	ldx tmp1
	sta VIEWPORT,x
.done	rts

;**************************************
; data
!zone data

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

runmsg !pet "escaped!",0
runfailmsg !pet "couldn't escape!",0
takesmsg !pet " takes ",0
openboxmsg !pet "open the box",0
trappedmsg !pet "trapped!",0
armormsg !pet "armor",0
shopmsg !pet "buy somethin'?",0
confirmmsg !pet "buy the ",0
notenoughmoneymsg !pet "not enough $!",0
okmsg !pet "ok",0
thankyoumsg !pet "thanks!",0

spelltab
!word starfall
!word swordrain
!word flash
!word eye
!word skin

spellnames
!word starfallname
!word swordrainname
!word flashname
!word eyename
!word skinname
numspells=(*-spellnames)/2

starfallname  !pet "starfall",0
swordrainname !pet "swordrain",0
flashname !pet "flash",0
eyename !pet "eye",0
skinname !pet "skin",0

learnedmsg !pet "learned ",0
nomanamsg !pet "no mana",0

storemsg !pet "shop",0
byemsg !pet "bye",0
staresmsg !pet "glares",0
invisinroommsg !pet "uh oh",0
spellendsmsg !pet "spell ends"

hp !byte 100
magick !byte 100
dmg !byte 2 	; max bonus damage (2^n)
basedmg !byte 1 ; min damage
spelldmg !byte 5 ; spell max damage (2^n)

enemynametab
!word snakename
!word batname

enemynames
snakename !pet "snake",0
batname !pet "bat",0

; graphics
gfx_snake
!byte  6,4	; 6x4
!byte  5	; max HP
!byte  2	; base damage
!byte  0	; name index
!byte  233,215,208,32,32,32,34,160
!byte  160,32,32,223,32,32,224,227
!byte  227,105,32,32,95,224,105,32

gfx_bat
!byte  6,3	; 6x4
!byte  2	; max HP
!byte  2	; base damage
!byte  1	; name index
!byte  233,223,223,233,233,223,105,95
!byte  174,174,105,95,32,32,34,34,32,32

prg_size=*-basicstub
remaining_bytes=SCREEN - *

;**************************************
; TITLE
; code beyond here is located in $1e00 and cannot be used after starting the game
*=SCREEN
!source "title.asm"
titleend

WING1=SCREEN_W*2-4
WING2=SCREEN_W*2-2

;**************************************
getname
-	lda #$00
	sta $9600,x
	sta $9700,x
	sta $100,x
	lda #$01
	sta $9700+<titleend-1,x
	dex
	bne -

	ldx #7
	lda #$20
-	sta SCREEN+(SCREEN_W*17),x
	dex
	bpl -

	lda #$90	; black
	jsr $ffd2
	ldx #17
	jsr $e50c
-	lda SCREEN+WING1
	eor #$1f
	sta SCREEN+WING1
	lda SCREEN+WING2
	eor #$02
	sta SCREEN+WING2
	jsr middelay

	ldx #$00
	stx $cc
	jsr $ffe4
	beq -
	cmp #$0d
	beq .storename
	jsr $ffd2

	; validate/update cursor pos
	jsr $e513
	cpy #7
	bne +
	dey
	clc
+	bcc +
	ldy #0
+	ldx #17
	jsr $e50c
	jmp -

.storename
	jsr $e513
	dey
-	lda SCREEN+(SCREEN_W*17),y
	clc
	adc #'A'-1
	sta name,y
	dey
	bpl -
	lda #$00
	sta name+7
	sta enemy_idx

	; srand
	lda $9004
	sta rndval
	jsr rnd
	sta rndval+1
	jmp start

titlecodeend=*
