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

armorturns = $120	; turns before ARMOR spell wears off
unbuffedarmor=$121

;**************************************
; enemy tables: entries contain 1 byte per enemy
enemy_hp = $123
enemy_pos = $125
enemy_w = $127
enemy_h = $129
enemy_dmg = $12b
enemy_col = $12d
enemy_name = $12f

spell = $131

zerodata=$132
lvl = $132
xp = $133
gemcnt = $134
money = $135
armor = $136
learnedspells = $137
zerodatalen=name-zerodata

name = $139

freebuff=$033c
