;**************************************
; spell values
FLASH_DMG = 8
SWORDRAIN_DMG = 10
STARFALL_DMG = 20
SKIN_ARMOR_BONUS = 5
SKIN_TURNS = 20

FLASH_COST = 2
EYE_COST = 3
SKIN_COST = 4
SWORDRAIN_COST = 10
STARFALL_COST = 15

;**************************************
; shop constants
HEALTH_COST = 10
WEAPON_COST = 40
SPELL_COST = 60
ARMOR_COST = 30
MAX_FOR_SALE=6

;**************************************
; delays
SHORT_DELAY=5
MID_DELAY=10
LONG_DELAY=30
VLONG_DELAY=50

;**************************************
; memory locations/layout
SCREEN=$1e00
SCREEN_W=22
SCREEN_H=23
COLORMEM=$9600
VP_W=9
VP_H=10
VP_X=SCREEN_W-VP_W-1
VP_Y=0
STATUS_LINE=1
SPELL_LINE=STATUS_LINE+5
LEVEL_LINE=VP_Y+VP_H+1
INPUT_LINE=22
MSG_LINE=12
MSG_H=9

VIEWPORT=SCREEN+VP_X+(SCREEN_W*VP_Y)
VIEWPORT_COL=COLORMEM+VP_X+(SCREEN_W*VP_Y)
VIEWPORT_END=SCREEN+VP_X+VP_W+SCREEN_W*SCREEN_H

SKY_ENEMY_MAXPOS=<((SCREEN_W*VP_H)-(SCREEN_W*5))

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

GC_GEM=1
CH_GEM=$5a

GC_TRAP=6
CH_TRAP=63

GC_SCROLL=7
CH_SCROLL=88

MAX_ENEMIES=2

ENEMY_SNAKE=0
ENEMY_BAT=1
ENEMY_WARLOCK=2

;**************************************
; misc. gameplay constants
GEMS_TO_WIN=3
XP_PER_KILL=10
XP_RAND=2
XP_TO_LVLUP=100
TRAP_DMG=6
NUM_ENEMIES=3

WARLOCK_DMG=6
