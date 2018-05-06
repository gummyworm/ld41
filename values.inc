;**************************************
; spell values
FLASH_DMG = 8
SWORDRAIN_DMG = 10
STARFALL_DMG = 20
SKIN_ARMOR_BONUS = 5
SKIN_TURNS = 20

;**************************************
; shop constants
HEALTH_COST = 10
WEAPON_COST = 40
SPELL_COST = 60
ARMOR_COST = 30
MAX_FOR_SALE=6

;**************************************
; delays
SHORT_DELAY=10
MID_DELAY=40
LONG_DELAY=90
VLONG_DELAY=$ff

;**************************************
; memory locations/layout
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
VIEWPORT_COL=COLORMEM+VP_X+(SCREEN_W*VP_Y)
VIEWPORT_END=SCREEN+VP_X+VP_W+SCREEN_W*SCREEN_H

;**************************************
; character/visual constants
CH_STAR=42
CH_SWORD=30
GROUND_CHAR=102

;**************************************
; map generation constants
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

MAX_ENEMIES=2

;**************************************
; misc. gameplay constants
GEMS_TO_WIN=3
XP_PER_KILL=10
XP_RAND=2
XP_TO_LVLUP=100
TRAP_DMG=6
