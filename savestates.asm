INCLUDE "constants.asm"


SECTION "Savestates 1", ROMX

Savestates_LoadState:
	di
	ld [MBC1RomBank], a
	call DisableLCD

; load 0:5000-6fff into 0:8000-9fff
	ld de, $5000
	ld hl, $8000
	ld b, $70
.loop_vram0:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_vram0
	inc d
	ld a, d
	cp b
	jr nz, .loop_vram0

IF DEF(_RED) || DEF(_BLUE)
	and a
ELSE ; yellow
	scf
ENDC

; yellow only (vram bank 1)
	jr nc, .done_vram
; load 0:7000-77ff into 1:9800-9fff
	ld a, 1
	ldh [rVBK], a
	ld de, $7000
	ld hl, vBGMap0
	ld b, $78
.loop_vram1:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_vram1
	inc d
	ld a, d
	cp b
	jr nz, .loop_vram1
	xor a ; 0
	ldh [rVBK], a
; end yellow only

.done_vram:
; load 0:7800-0:78ff into ff00-ffff
	ld de, $7800
	ld hl, $ff00
	ld b, $79
.loop_hram:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_hram
	inc d
	ld a, d
	cp b
	jr nz, .loop_hram

IF DEF(_RED) || DEF(_BLUE)
	and a
ELSE ; yellow
	scf
ENDC

; yellow only (CGB palette data)
	jr nc, .done_palettedata
	ld hl, $7900
	ld a, $80 ; index 0 with auto-increment
	ldh [rBGPI], a
	ldh [rOBPI], a
; load 0:7900-0:793f into background palette data
	ld c, LOW(rBGPD)
	ld b, $40
.loop_bgpd:
	ld a, [hli]
	ld [c], a
	dec b
	jr nz, .loop_bgpd
; load 0:7940-0:797f into object palette data
	ld c, LOW(rOBPD)
	ld b, $40
.loop_obpd:
	ld a, [hli]
	ld [c], a
	dec b
	jr nz, .loop_obpd
; end yellow only

.done_palettedata:
	ld a, [wCurrentMenuItem]
	ldh [hCurrentMenuItem], a
	ld a, [wListScrollOffset]
	ldh [hListScrollOffset], a

; load 0:7a68-7fff into 0:a000-0:a597
	ld a, SRAM_ENABLE
	ld [MBC1SRamEnable], a
	xor a ; BANK(sSpriteBuffer0)
	ld [MBC1SRamBank], a
	ld de, $7a68
	ld hl, sSpriteBuffer0
	ld b, $80
.loop_spritebuffers:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_spritebuffers
	inc d
	ld a, d
	cp b
	jr nz, .loop_spritebuffers

; this byte is overwritten in even banks by the patcher program
; it switches to the next bank (2nd bank w/data), where execution continues
	ld a, 0
	ld [MBC1RomBank], a

; load 1:5074-5fff into 1:a598-1:b523
	ld a, BANK(sPlayerName)
	ld [MBC1SRamBank], a
	ld de, $5074
	ld hl, sPlayerName
	ld b, $60
.loop_sav:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_sav
	inc d
	ld a, d
	cp b
	jr nz, .loop_sav
	xor a ; SRAM_DISABLE
	ld [MBC1SRamEnable], a

; load 1:6000-7fff into c000-dfff
	ld de, $6000
	ld hl, $c000
	ld b, $80
.loop_wram:
	ld a, [de]
	ld [hli], a
	inc e
	jr nz, .loop_wram
	inc d
	ld a, d
	cp b
	jr nz, .loop_wram
; tail code is written here by the patcher program here in the odd banks
; code dynamically sets LCD state, rDIV, registers, etc. based on state
; then jumps to return address to complete loading the selected state


SECTION "Savestates 2", ROMX

rLCDC_BG_PRIORITY    EQU 0 ; 0=Off, 1=On
rLCDC_SPRITES_ENABLE EQU 1 ; 0=Off, 1=On
rLCDC_SPRITE_SIZE    EQU 2 ; 0=8x8, 1=8x16
rLCDC_BG_TILEMAP     EQU 3 ; 0=9800-9BFF, 1=9C00-9FFF
rLCDC_TILE_DATA      EQU 4 ; 0=8800-97FF, 1=8000-8FFF
rLCDC_WINDOW_ENABLE  EQU 5 ; 0=Off, 1=On
rLCDC_WINDOW_TILEMAP EQU 6 ; 0=9800-9BFF, 1=9C00-9FFF
;rLCDC_ENABLE         EQU 7 ; 0=Off, 1=On

IF DEF(_RED) || DEF(_BLUE)
SAVESTATES_BASE_BANK EQU $2e
ELSE ; yellow
SAVESTATES_BASE_BANK EQU $40
ENDC

SAVESTATES_NAME_LENGTH EQU 18


Savestates_LoadSavestateMenu::
	ld hl, rIE
	res TIMER, [hl]
	ldh a, [rLCDC]
	bit rLCDC_ENABLE, a
	call nz, DisableLCD
	call Savestates_PrepareOAMData
	call Savestates_InitMenu
	call Savestates_CopyScreenTileBufferToVRAM
	ld a, (1 << rLCDC_WINDOW_TILEMAP) | (1 << rLCDC_WINDOW_ENABLE) | (1 << rLCDC_BG_PRIORITY)
	ldh [rLCDC], a
	ld a, %11100100
	ldh [rBGP], a
	ld a, %11010000
	ldh [rOBP0], a
	ld a, %11100000
	ldh [rOBP1], a

IF DEF(_RED) || DEF(_BLUE)
	and a
ELSE ; yellow
	scf
ENDC
	jr nc, .done_palettes

; yellow only (load default background palette data)
	ld a, $80 ; index 0 with auto-increment
	ldh [rBGPI], a
	ld c, LOW(rBGPD)
	ld b, $10
.loop_palettes:
	ld a, $ff
	ld [c], a
	ld a, $7f
	ld [c], a
	ld a, $63
	ld [c], a
	ld a, $0c
	ld [c], a
	dec b
	jr nz, .loop_palettes
; end yellow only

.done_palettes:
	xor a
	ldh [hWY], a
	inc a
	ldh [hAutoBGTransferEnabled], a
	ld a, $9c
	ldh [hAutoBGTransferDest + 1], a
	call EnableLCD
	ei
	ld hl, wd730
	res 5, [hl]
	xor a
	ld [wJoyIgnore], a
	inc a
	ldh [hJoy6], a
	ldh [hJoy7], a
	ld b, D_DOWN | SELECT | B_BUTTON
	call Savestates_WaitForKeysReleased
	call PlaceMenuCursor
	jr .inputLoop

.handleMenuInput:
	call Savestates_UpdateMenuState
	call Savestates_UpdateVisibleStates
	call Savestates_UpdateScrollState
	call PlaceMenuCursor

.inputLoop:
	call JoypadLowSensitivity
	ldh a, [hJoy5]
	bit BIT_A_BUTTON, a
	jr z, .noA
; handle A
	ld a, SFX_PRESS_AB
	call PlaySound
	ld b, A_BUTTON
	call Savestates_WaitForKeysReleased
	ld a, [wCurrentMenuItem]
	ld c, a
	ld a, [wListScrollOffset]
	add c
	add a ; 2 banks per state
	add SAVESTATES_BASE_BANK
	jp Savestates_LoadState

.noA:
	bit BIT_D_DOWN, a
	jr z, .noDown
; handle down
	ld a, [wCurrentMenuItem]
	inc a
	ld [wCurrentMenuItem], a
	jr .handleMenuInput

.noDown:
	bit BIT_D_UP, a
	jr z, .noUp
; handle up
	ld a, [wCurrentMenuItem]
	dec a
	ld [wCurrentMenuItem], a
	jr .handleMenuInput

.noUp:
	bit BIT_D_LEFT, a
	jr z, .noLeft
; handle left
	ld a, [wListScrollOffset]
	sub 6
	ld [wListScrollOffset], a
	jr .handleMenuInput

.noLeft:
	bit BIT_D_RIGHT, a
	jr z, .inputLoop
; handle right
	ld a, [wListScrollOffset]
	add 6
	ld [wListScrollOffset], a
	jr .handleMenuInput

Savestates_InitMenu:
	call LoadTextBoxTilePatterns
	call LoadFontTilePatterns
	call Savestates_ClearScreen
	ld hl, wd730
	set 6, [hl]
	hlcoord 0, 0
	lb bc, 3, 18
	call TextBoxBorder
	hlcoord 1, 1
	ld de, SavestatesString
	call PlaceString
	ld a, 6
	ld [wTopMenuItemY], a
	ld a, 1
	ld [wTopMenuItemX], a
	ldh a, [hCurrentMenuItem]
	ld [wCurrentMenuItem], a
	ld a, D_UP | D_DOWN | A_BUTTON
	ld [wMenuWatchedKeys], a
	ldh a, [hListScrollOffset]
	ld [wListScrollOffset], a
	ld hl, hUILayoutFlags
	res 1, [hl]
	ld hl, SavestateNames
	ld bc, SAVESTATES_NAME_LENGTH
	ld d, 0

.countNumStatesLoop:
	ld a, [hl]
	cp " " ; first savestate starting with space presumed first empty slot
	jr z, .foundEnd
	add hl, bc
	inc d
	jr .countNumStatesLoop

.foundEnd:
	ld a, d
	dec a
	ld [wListCount], a
	cp 5
	jr c, .gotMaxMenuItem
	ld a, 4

.gotMaxMenuItem:
	ld [wMaxMenuItem], a
	call Savestates_UpdateVisibleStates
	call Savestates_UpdateScrollState
	ret

Savestates_UpdateVisibleStates:
	ld hl, SavestateNames
	ld a, [wListScrollOffset]
	ld bc, SAVESTATES_NAME_LENGTH
	call AddNTimes
	ld e, l
	ld d, h
	hlcoord 2, 6
	ld a, [wListCount]
	cp 5
	jr nc, .maxVisibleStates
	inc a
	ld b, a
	jr .PlaceStateNames

.maxVisibleStates
	ld b, 6

.PlaceStateNames:
.loop
	push bc
	call PlaceString
	inc de
	ld bc, SCREEN_WIDTH * 2
	add hl, bc
	pop bc
	dec b
	jr nz, .loop
	ret

SavestatesString:
	db "Savestates@"

Savestates_UpdateScrollState:
	hlcoord 18, 3
	ld a, [wListCount]
	inc a
	ld b, a
	call Savestates_PlaceNumber
	ld a, "/"
	ld [hld], a
	ld a, [wCurrentMenuItem]
	ld c, a
	ld a, [wListScrollOffset]
	add c
	inc a
	ld b, a
	call Savestates_PlaceNumber
	ld a, " "
	ld [hld], a
	ld [hld], a
	ret

Savestates_UpdateMenuState:
	ld b, 0
	ld a, [wCurrentMenuItem]
	cp -1
	jr nz, .notOob
	xor a
	ld [wCurrentMenuItem], a
	dec b
	jr .gotCurrentMenuItem

.notOob:
	ld c, a
	ld a, [wMaxMenuItem]
	cp c
	jp nc, .gotCurrentMenuItem
	ld [wCurrentMenuItem], a
	inc b

.gotCurrentMenuItem:
	ld a, [wListScrollOffset]
	add b
	cp 250
	jr c, .updateNotOob
; oob
	ld c, a
	ld a, [wCurrentMenuItem]
	add c
	jr c, .updateOob
	xor a
.updateOob:
	ld [wCurrentMenuItem], a
	xor a
	ld [wListScrollOffset], a
	ret

.updateNotOob:
	ld c, a
	ld a, [wMaxMenuItem]
	ld d, a
	ld a, [wListCount]
	sub d
	cp c
	jr nc, .updateListScrollOffset
	ld e, a
	sub c
	ld b, a
	ld a, [wCurrentMenuItem]
	sub b
	ld c, a
	ld a, d
	cp c
	jr nc, .updateCurrentMenuItem
	ld c, a

.updateCurrentMenuItem:
	ld a, c
	ld [wCurrentMenuItem], a
	ld c, e

.updateListScrollOffset:
	ld a, c
	ld [wListScrollOffset], a
	ret

Savestates_PrepareOAMData:
	xor a
	ld [wUpdateSpritesEnabled], a
	ld b, BANK(PrepareOAMData)
	ld hl, PrepareOAMData
	call Bankswitch
	jp hDMARoutine

Savestates_ClearScreen:
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	inc b
	hlcoord 0, 0
	ld a, " "
.loop:
	ld [hli], a
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop
	ret

Savestates_PlaceNumber:
.nextDigit:
	ld a, b
	ld c, 0
.currentDigit:
	sub 10
	inc c
	jr nc, .currentDigit
	add 10
	dec c
	add "0"
	ld [hld], a
	ld a, c
	and c
	ret z
	ld b, c
	jr .nextDigit

Savestates_CopyScreenTileBufferToVRAM:
	ld a, LOW(wTileMap)
	ldh [hVBlankCopyBGSource], a
	ld a, HIGH(wTileMap)
	ldh [hVBlankCopyBGSource + 1], a
	ld a, LOW(vBGMap1)
	ldh [hVBlankCopyBGDest], a
	ld a, HIGH(vBGMap1)
	ldh [hVBlankCopyBGDest + 1], a
	ld a, $12
	ldh [hVBlankCopyBGNumRows], a
	jp VBlankCopyBgMap

Savestates_WaitForKeysReleased:
.loop
	push bc
	call Joypad
	pop bc
	ldh a, [hJoyInput]
	and b
	ret z
	call DelayFrame
	jr .loop


SECTION "Savestates 3", ROMX

SavestateNames:
REPT 100
	db "                 @"
ENDR
