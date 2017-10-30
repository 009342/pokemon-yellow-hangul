AskName:
	call SaveScreenTilesToBuffer1
	call GetPredefRegisters
	push hl
	ld a, [wIsInBattle]
	dec a
	coord hl, 0, 0
	lb bc, 4, 11
	call z, ClearScreenArea ; only if in wild battle
	ld a, [wcf91]
	ld [wd11e], a
	call GetMonName
	ld hl, DoYouWantToNicknameText
	call PrintText
	coord hl, 14, 6
	lb bc, 7, 15
	ld a, TWO_OPTION_MENU
	ld [wTextBoxID], a
	call DisplayTextBoxID
	pop hl
	ld a, [wCurrentMenuItem]
	and a
	jr nz, .declinedNickname
	ld a, [wUpdateSpritesEnabled]
	push af
	xor a
	ld [wUpdateSpritesEnabled], a
	push hl
	ld a, NAME_MON_SCREEN
	ld [wNamingScreenType], a
	call DisplayNamingScreen
	ld a, [wIsInBattle]
	and a
	jr nz, .inBattle
	call ReloadMapSpriteTilePatterns
.inBattle
	call LoadScreenTilesFromBuffer1
	pop hl
	pop af
	ld [wUpdateSpritesEnabled], a
	ld a, [wcf4b]
	cp $50; RAW DATA : 	cp "@"
	ret nz
.declinedNickname
	ld d, h
	ld e, l
	ld hl, wcd6d
	ld bc, NAME_LENGTH
	jp CopyData

DoYouWantToNicknameText:
	TX_FAR _DoYouWantToNicknameText
	db $50; RAW DATA : 	db "@"

DisplayNameRaterScreen:
	ld hl, wBuffer
	xor a
	ld [wUpdateSpritesEnabled], a
	ld a, NAME_MON_SCREEN
	ld [wNamingScreenType], a
	call DisplayNamingScreen
	call GBPalWhiteOutWithDelay3
	call RestoreScreenTilesAndReloadTilePatterns
	call LoadGBPal
	ld a, [wcf4b]
	cp $50; RAW DATA : 	cp "@"
	jr z, .playerCancelled
	ld hl, wPartyMonNicks
	ld bc, NAME_LENGTH
	ld a, [wWhichPokemon]
	call AddNTimes
	ld e, l
	ld d, h
	ld hl, wBuffer
	ld bc, NAME_LENGTH
	call CopyData
	and a
	ret
.playerCancelled
	scf
	ret
ClearHangulKeyboardBuffer:
	push hl
	push af
	xor a
	ld hl,wHangulKeyboard
	ld [hli],a
	ld [hli],a
	ld [hli],a
	ld [hli],a
	ld [hli],a
	ld [hli],a
	ld [hli],a
	pop af
	pop hl
	ret
_DisplayNamingScreen::
	call ClearHangulKeyboardBuffer
	push hl
	ld hl, wd730
	set 6, [hl]
	call GBPalWhiteOutWithDelay3
	call ClearScreen
	call UpdateSprites
	ld b, SET_PAL_GENERIC
	call RunPaletteCommand
	call LoadHpBarAndStatusTilePatterns
	call LoadEDTile
	callba LoadMonPartySpriteGfx
	coord hl, 0, 5
	lb bc, 9, 18
	call TextBoxBorder
	call PrintNamingText
	ld a, 4
	ld [wTopMenuItemY], a
	ld a, 1
	ld [wTopMenuItemX], a
	ld [wLastMenuItem], a
	ld [wCurrentMenuItem], a
	ld a, $ff
	ld [wMenuWatchedKeys], a
	ld a, 7
	ld [wMaxMenuItem], a
	ld a, $50; RAW DATA : 	ld a, "@"
	ld [wcf4b], a
	xor a
	ld hl, wNamingScreenSubmitName
	ld [hli], a
	ld [hli], a
	ld [wAnimCounter], a
.selectReturnPoint
	call PrintAlphabet
	call GBPalNormal
.ABStartReturnPoint
	ld a, [wNamingScreenSubmitName]
	and a
	jr nz, .submitNickname
	call PrintNicknameAndUnderscores
.dPadReturnPoint
	call PlaceMenuCursor
.inputLoop
	ld a, [wCurrentMenuItem]
	push af
	callba AnimatePartyMon_ForceSpeed1
	pop af
	ld [wCurrentMenuItem], a
	call JoypadLowSensitivity
	ld a, [hJoyPressed]
	and a
	jr z, .inputLoop
	ld hl, .namingScreenButtonFunctions
.checkForPressedButton
	sla a
	jr c, .foundPressedButton
	inc hl
	inc hl
	inc hl
	inc hl
	jr .checkForPressedButton
.foundPressedButton
	ld a, [hli]
	ld e, a
	ld a, [hli]
	ld d, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
	push de
	jp hl

.submitNickname
	pop de
	ld hl, wcf4b
	ld bc, NAME_LENGTH
	call CopyData
	call GBPalWhiteOutWithDelay3
	call ClearScreen
	call ClearSprites
	call RunDefaultPaletteCommand
	call GBPalNormal
	xor a
	ld [wAnimCounter], a
	ld hl, wd730
	res 6, [hl]
	ld a, [wIsInBattle]
	and a
	jp z, LoadTextBoxTilePatterns
	jpab LoadHudTilePatterns

.namingScreenButtonFunctions
	dw .dPadReturnPoint
	dw .pressedDown
	dw .dPadReturnPoint
	dw .pressedUp
	dw .dPadReturnPoint
	dw .pressedLeft
	dw .dPadReturnPoint
	dw .pressedRight
	dw .ABStartReturnPoint
	dw .pressedStart
	dw .selectReturnPoint
	dw .pressedSelect
	dw .ABStartReturnPoint
	dw .pressedB
	dw .ABStartReturnPoint
	dw .pressedA

.pressedA_changedCase
	pop de
	ld de, .selectReturnPoint
	push de
.pressedSelect
	ld a, [wAlphabetCase]
	xor $1
	ld [wAlphabetCase], a
	ret

.pressedStart
	ld a, 1
	ld [wNamingScreenSubmitName], a
	ret

.pressedA
	ld a, [wCurrentMenuItem]
	cp $5 ; "ED" row
	jr nz, .didNotPressED
	ld a, [wTopMenuItemX]
	cp $11 ; "ED" column
	jr z, .pressedStart
.didNotPressED
	ld a, [wCurrentMenuItem]
	cp $6 ; case switch row
	jr nz, .didNotPressCaseSwtich
	ld a, [wTopMenuItemX]
	cp $1 ; case switch column
	jr z, .pressedA_changedCase
;스페이스 무력화를 위한 삽질 코드
.didNotPressCaseSwtich
	ld a, [wCurrentMenuItem]
	cp $5 ; case switch row
	jr nz, .didNotPressSpace
	ld a, [wTopMenuItemX]
	cp $F ; case switch column
	jr z, .pressedA_changedCase
	
	ld a, [wCurrentMenuItem]
	cp $5 ; case switch row
	jr nz, .didNotPressSpace
	ld a, [wTopMenuItemX]
	cp $D ; case switch column
	jr z, .pressedA_changedCase

	ld a, [wCurrentMenuItem]
	cp $5 ; case switch row
	jr nz, .didNotPressSpace
	ld a, [wTopMenuItemX]
	cp $B ; case switch column
	jr z, .pressedA_changedCase

	ld a, [wCurrentMenuItem]
	cp $5 ; case switch row
	jr nz, .didNotPressSpace
	ld a, [wTopMenuItemX]
	cp $9 ; case switch column
	jr z, .pressedA_changedCase

.didNotPressSpace
	ld hl, wMenuCursorLocation
	ld a, [hli]
	ld h, [hl]
	ld l, a
	inc hl
	ld a, [hl]
	ld [wNamingScreenLetter], a
	call CalcStringLength
	ld a, [wNamingScreenLetter]
	cp $e5
	ld de, Dakutens
	jr z, .dakutensAndHandakutens
	cp $e4
	ld de, Handakutens
	jr z, .dakutensAndHandakutens
	ld a, [wNamingScreenType]
	cp NAME_MON_SCREEN
	jr nc, .checkMonNameLength
	ld a, [wNamingScreenNameLength]
	cp $c ; max length of player/rival names
	jr .checkNameLength
.checkMonNameLength
	ld a, [wNamingScreenNameLength]
	cp $c ; max length of pokemon nicknames
.checkNameLength
	jr c, .addLetter
	ret

.dakutensAndHandakutens
	push hl
	call DakutensAndHandakutens
	pop hl
	ret nc
	dec hl
.addLetter
	ld a, [wNamingScreenLetter]
	cp a,$A0
	jp c,.NonHangul
	call addLetterHangul
	jr .Hangul
.NonHangul
	ld [hli], a
	ld [hl], $50; RAW DATA : 	ld [hl], "@"
.Hangul
	ld a, SFX_PRESS_AB
	call PlaySound
	ret
.pressedB
	ld a, [wNamingScreenNameLength]
	and a
	ret z
	call CalcStringLength
	dec hl
	ld [hl], $00
	dec hl
	ld [hl], $50; RAW DATA : 	ld [hl], "@"
	push hl
	ld hl,wHangulKeyboard
	ld [hl], $00
	inc hl
	ld [hl], $00
	inc hl
	ld [hl], $00
	inc hl
	ld [hl], $00
	inc hl
	ld [hl], $00
	inc hl
	ld [hl], $00
	inc hl
	ld [hl], $00
	pop hl
	ret
.pressedRight
	ld a, [wCurrentMenuItem]
	cp $6
	ret z ; can't scroll right on bottom row
	ld a, [wTopMenuItemX]
	cp $11 ; max
	jp z, .wrapToFirstColumn
	inc a
	inc a
	jr .done
.wrapToFirstColumn
	ld a, $1
	jr .done
.pressedLeft
	ld a, [wCurrentMenuItem]
	cp $6
	ret z ; can't scroll right on bottom row
	ld a, [wTopMenuItemX]
	dec a
	jp z, .wrapToLastColumn
	dec a
	jr .done
.wrapToLastColumn
	ld a, $11 ; max
	jr .done
.pressedUp
	ld a, [wCurrentMenuItem]
	dec a
	ld [wCurrentMenuItem], a
	and a
	ret nz
	ld a, $5 ; wrap to bottom row
	ld [wCurrentMenuItem], a
	;ld a, $1 ; force left column
	ld a, [wTopMenuItemX]
	jr .done
.pressedDown
	ld a, [wCurrentMenuItem]
	inc a
	ld [wCurrentMenuItem], a
	cp $6
	ret nz
	;jr nz, .wrapToTopRow
	ld a, $1
	ld [wCurrentMenuItem], a
	ld a, [wTopMenuItemX]
	jr .done
;.wrapToTopRow
;	cp $6
;	ret nz
;	ld a, $1
.done
	ld [wTopMenuItemX], a
	jp EraseMenuCursor
addLetterHangul: ;a 글자 
	;wHangulKeyboard 구조
	;0 : 초성 중성 종성 종성2
	;1 : 초성
	;2 : 중성
	;3 : 종성
	;4 : 종성 2
	;5 : 다음글자로
	;6 : 글자백업
	push af
	ld [wHangulKeyboard+6],a
.Main
	ld a,[wHangulKeyboard]
	and a
	jp z,.First
	dec a
	jp z,.Second
	dec a
	jp z,.Last
	dec a
	jp z,.Last2
.First
	;초기화
	ld a,$01
	ld [wHangulKeyboard+5],a ;다음글자로
	pop af
	call JaumOrMoum
	jp c,.Jaum
	;초성에 자음이 아닌경우
.LastToFirstCheck
	ld a,$00
	push hl
	ld hl,wHangulKeyboard+4
	cp [hl]
	pop hl
	jr z,.LastToFirstCheck2
	;없 + ㅓ와 같은 경우, 
	ld a,[wHangulKeyboard+6]
	push af
	ld a,[wHangulKeyboard+4]
	push af
	ld a,[wHangulKeyboard+3]
	push af
	xor a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard+4],a
	ld [wHangulKeyboard+5],a
	ld a,$02
	ld [wHangulKeyboard],a
	pop af
	call addLetterHangul
	pop af
	ld [wHangulKeyboard+1],a
	pop af
	ld [wHangulKeyboard+6],a
	ld [wHangulKeyboard+2],a
	xor a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard+4],a
	ld a,$02
	ld [wHangulKeyboard+5],a
	ld [wHangulKeyboard],a
	jp .Calc
.LastToFirstCheck2
	push hl
	ld hl,wHangulKeyboard+3
	cp [hl]
	pop hl
	jp z,.NotLastToFirst
	;업 + ㅓ 와 같은 경우
	ld a,[wHangulKeyboard+6]
	push af
	ld a,[wHangulKeyboard+3]
	push af
	ld a,[wHangulKeyboard+2]
	push af
	xor a
	ld [wHangulKeyboard+2],a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard+4],a
	ld [wHangulKeyboard+5],a
	ld a,$01
	ld [wHangulKeyboard],a
	pop af
	call addLetterHangul
	pop af
	ld [wHangulKeyboard+1],a
	pop af
	ld [wHangulKeyboard+6],a
	ld [wHangulKeyboard+2],a
	xor a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard+4],a
	ld a,$02
	ld [wHangulKeyboard+5],a
	ld [wHangulKeyboard],a
	jp .Calc
.NotLastToFirst ;따지자면, 어+ㅓ와 같은 경우?
;	ld a,$01
;	ld [wHangulKeyboard+5],a
;	ld a,[wHangulKeyboard+6]
;	ld [wHangulKeyboard+1],a
;	xor a
;	ld [wHangulKeyboard+2],a
;	ld [wHangulKeyboard+3],a
;	ld [wHangulKeyboard+4],a
;	jp .Calc
.Jaum
	;push hl
	;call CalcStringLength
	;cp $A
	;ret z
	;cp $B
	;ret z
	;pop hl
	ld [wHangulKeyboard+1],a
	xor a
	ld [wHangulKeyboard+2],a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard+4],a
	ld a,[wHangulKeyboard]
	inc a
	ld [wHangulKeyboard],a
	jp .Calc
.Second
	pop af
	push af
	call JaumOrMoum
	jr nc,.Moum
	;중성에 모음이 아닐경우
	xor a
	ld [wHangulKeyboard],a
	;ld a,$01
	;ld [wHangulKeyboard+5],a ;다음글자로
	jp .Main
.Moum
	ld [wHangulKeyboard+2],a
	ld a,[wHangulKeyboard]
	inc a
	ld [wHangulKeyboard],a
	pop af
	jp .Calc
.Last
	pop af
	push af
	call JaumOrMoum
	jr c,.LastJaum
    ;종성의 첫번째가 자음이 아닐경우
	xor a
	ld [wHangulKeyboard],a
	;ld a,$01
	;ld [wHangulKeyboard+5],a ;다음글자로
	jp .Main
.LastJaum 
	ld [wHangulKeyboard+3],a
	push bc
	ld b,a
	xor a
	ld c,a
	call CalcFianlConsoTbl
	pop bc
	and a;
	jr nz,.LastJaumInTable
	xor a
	ld [wHangulKeyboard+3],a
	ld [wHangulKeyboard],a
	ld a,[wHangulKeyboard+6]
	ld [wHangulKeyboard+1],a
	jp .Main
.LastJaumInTable
	ld a,[wHangulKeyboard]
	inc a
	ld [wHangulKeyboard],a
	pop af
	jp .Calc
.Last2
	pop af
	push af
	call JaumOrMoum
	jr c,.Last2Jaum
	xor a
	ld [wHangulKeyboard],a
	;ld a,$01
	;ld [wHangulKeyboard+5],a ;다음글자로
	jp .Main
.Last2Jaum
	pop af
	ld [wHangulKeyboard+4],a
	push bc
	ld c,a
	ld a,[wHangulKeyboard+3]
	ld b,a
	call CalcFianlConsoTbl
	pop bc
	and a;a가 1이상이면,(존재하면)
	jr nz,.Calc ;계산!
	;성립하지 않을 경우
	xor a
	ld [wHangulKeyboard+4],a
	ld [wHangulKeyboard],a
	;ld a,$01
	;ld [wHangulKeyboard+5],a ;다음글자로
	ld a,[wHangulKeyboard+6]
	ld [wHangulKeyboard+1],a
	push af
	jp .Main
.Calc
	push bc
	push hl
	ld a,[wHangulKeyboard]
	cp a,1
	jr nz,.NotFirst
	ld a,[wHangulKeyboard+6]
	call FindConsoTbl
	jr .Found
.NotFirst
	call ToUnicode ;bc 유니코드
	call SearchHangulTable ;bc 한글코드
	ld a,b
	cp $FF
	jr nz,.Found
	;유니코드에는 있지만 한글테이블에는 없는 경우
	xor a
	ld [wHangulKeyboard],a
	;ld a,$01
	;ld [wHangulKeyboard+5],a ;다음글자로
	ld a,[wHangulKeyboard+6]
	pop hl
	pop bc
	push af
	jp .Main
.Found
	ld a,[wHangulKeyboard+5]
	and a
	jr nz,.nextLetter
	dec hl
	dec hl
.nextLetter
	xor a
	ld [wHangulKeyboard+5],a
	ld a,b
	ld [hli],a
	ld a,c
	ld [hli],a
	ld a,$50
	ld [hl],a
	pop hl
	pop bc
	ret
FindConsoTbl:
	push hl
	ld hl,ConsoTbl
.loop
	cp a,[hl]
	jr z,.Found
	inc hl
	inc hl
	inc hl
	jr .loop
.Found
	inc hl
	ld a,[hli]
	ld b,a
	ld a,[hl]
	ld c,a
	pop hl
	ret
ToUnicode:
.keep_going
	xor a
	push hl
	ld [H_MULTIPLICAND],a
	ld [H_MULTIPLICAND+1],a
	ld [H_MULTIPLICAND+2],a
	ld a,[wHangulKeyboard+1]
	sub $A0
	ld [H_MULTIPLICAND+2],a
	ld a,21
	ld [H_MULTIPLIER],a
	call Multiply
	ld a,[H_MULTIPLYBUFFER+2]
	ld h,a
	ld a,[H_MULTIPLYBUFFER+3]
	ld l,a
	ld bc,$0000
	ld a,[wHangulKeyboard+2]
	sub $B3
	ld c,a
	add hl,bc
	ld a,h
	ld [H_MULTIPLICAND+1],a
	ld a,l
	ld [H_MULTIPLICAND+2],a
	ld a,28
	ld [H_MULTIPLIER],a
	call Multiply
	ld a,[H_MULTIPLYBUFFER+2]
	ld h,a
	ld a,[H_MULTIPLYBUFFER+3]
	ld l,a
	ld a,[wHangulKeyboard+3]
	ld b,a
	ld a,[wHangulKeyboard+4]
	ld c,a
	call CalcFianlConsoTbl
	ld bc,$0000
	ld c,a
	add hl,bc
	ld bc,$AC00
	add hl,bc
	push hl
	pop bc ; hl->bc
	pop hl
	ret
SearchHangulTable:
	push hl
	ld hl,HangulKeyboardTable
.SearchHangulTableLoop
	inc hl
	inc hl
	ld a,[hl]
	cp b
	jr nz,.SearchHangulTableLoopPrepare
	inc hl
	ld a,[hl]
	cp c
	dec hl
	jr nz,.SearchHangulTableLoopPrepare
	dec hl
	dec hl
	ld a,[hli]
	ld b,a
	ld a,[hl]
	ld c,a
	pop hl
	ret
.SearchHangulTableLoopPrepare
	ld a,[hl]
	cp $FF
	jr nz,.SearchHangulTableLoop
	ld b,a
	ld c,a
	pop hl
	ret
	
CalcFianlConsoTbl:: ;bc 종성 1, 종성 2
	push hl
	ld hl,FinalConsoTbl
	xor a
.loop
	push af
	push hl
	ld a,b
	cp [hl]
	jr nz,.loop_chk
	inc hl
	ld a,c
	cp [hl]
	jr nz,.loop_chk
	;확정
	pop hl
	pop af
	pop hl
	ret
.loop_chk
	pop hl
	inc hl
	inc hl
	push bc
	ld bc,FinalConsoTblEnd
	ld a,h
	cp b
	jr nz,.keep_going
	ld a,l
	cp c
	jr nz,.keep_going
	;테이블에 존재하지 않음
	pop bc
	pop af
	pop hl
	ld a,$00
	ret
.keep_going
	pop bc
	pop af
	inc a
	jr .loop
	
	
JaumOrMoum: ;c : 자음, nc : 모음
	cp a,$B3
	ret c
	ret
LoadEDTile:
; In Red/Blue, the bank for the ED_tile was defined incorrectly as bank0
; Luckily, the MBC3 treats loading $0 into $2000-$2fff range as loading bank1 into $4000-$7fff range
; Because Yellow uses the MBC5, loading $0 into $2000 - $2fff range will load bank0 instead of bank1 and thus incorrectly load the tile
; Instead of defining the correct bank, GameFreak decided to simply copy the ED_Tile in the function during HBlank
	ld de, ED_Tile
	ld hl, vFont + $700
	ld c, $4 ; number of copies needed
.waitForHBlankLoop
	ld a, [rSTAT]
	and %10 ; in HBlank?
	jr nz, .waitForHBlankLoop
	ld a, [de]
	ld [hli], a
	ld [hli], a
	inc de
	ld a, [de]
	ld [hli], a
	ld [hli], a
	inc de
	dec c
	jr nz, .waitForHBlankLoop
	ret

ED_Tile:
	INCBIN "gfx/ED_tile.1bpp"
ED_TileEnd:

PrintKorean:
	ld hl,HangulFontMap
	ld de,$8a00
	ld a,h
	ld [H_VBCOPYSRC+1],a
	ld a,l
	ld [H_VBCOPYSRC],a
	ld a,d
	ld [H_VBCOPYDEST+1],a
	ld a,e
	ld [H_VBCOPYDEST],a
	ld d,8-1
.loop
	ld a,$8
	ld [H_VBCOPYSIZE],a
	call DelayFrame
	dec d
	jr nz,.loop
	ret
PrintAlphabet: ;알파벳 출력
	;2bpp 한글모음표 출력부
	call PrintKorean
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld a, [wAlphabetCase]
	and a
	ld de, LowerCaseAlphabet
	jr nz, .lowercase
	ld de, UpperCaseAlphabet
.lowercase
	coord hl, 2, 6
	lb bc, 5, 9 ; 5 rows, 9 columns
.outerLoop
	push bc
.innerLoop
	ld a, [de]
	ld [hli], a
	inc hl
	inc de
	dec c
	jr nz, .innerLoop
	ld bc, SCREEN_WIDTH + 2
	add hl, bc
	pop bc
	dec b
	jr nz, .outerLoop
	call PlaceString
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	jp Delay3

LowerCaseAlphabet:
UpperCaseAlphabet:
	db "abcdefghijklmnopqrstuvwxyz",$BA,$BB,$BC,$BD,$BE,$BF,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,"    ",$f0,"@,¥UPPER CASE@"
	;db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$10,$11,$12
	;db $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,$30,$31,$32,$33,$34
	;db $7F,$7F,$7F,$7F
	;line ""
	;db $F0,$07,$15,$04,$AE,$07,$A4,$03,$F2,$50; RAW DATA : 	db "¥영문입력@"

	;db "ABCDEFGHIJKLMNOPQRSTUVWXYZ ×():;[]",$e1,$e2,"-?!♂♀/",$f2,
	line "@"
	;db $0B,$6B,$F0,$0A,$71,$01,$AB,$07,$A4,$03,$F2,$50; RAW DATA : 	db ",¥한글입력@"

PrintNicknameAndUnderscores:
	push af
	xor a
	ld [H_AUTOBGTRANSFERENABLED],a
	pop af
	call CalcStringLength
	ld a, c
	ld [wNamingScreenNameLength], a
	coord hl, 12, 2
	lb bc, 1, 12
	call ClearScreenArea
	coord hl, 12, 2
	ld de, wcf4b
	call PlaceString
	coord hl, 12, 3
	ld a, [wNamingScreenType]
	cp NAME_MON_SCREEN
	jr nc, .pokemon1
	;ld b, 10 ; player or rival max name length
	;CalcStringLength2byte
	ld b, 5
	jr .playerOrRival1
.pokemon1
	;ld b, 10 ; pokemon max name length
	ld b,5
.playerOrRival1
	ld a, $76 ; underscore tile id
.placeUnderscoreLoop
	ld [hli], a
	dec b
	;dec b
	jr nz, .placeUnderscoreLoop
	ld a, [wNamingScreenType]
	cp NAME_MON_SCREEN
	ld a, [wNamingScreenNameLength]
	jr nc, .pokemon2
	cp 6 ; player or rival max name length(12 -> 6 수정, 아래도 마찬가지)
	jr .playerOrRival2
.pokemon2
	cp 6 ; pokemon max name length
.playerOrRival2
	jr nz, .emptySpacesRemaining
	; when all spaces are filled, force the cursor onto the ED tile
;임시로 넣은 코드 글자 길이 초과 방지용
	push hl
	call CalcStringLength
	dec hl
	ld [hl], $00
	dec hl
	ld [hl], $50
	pop hl
	call ClearHangulKeyboardBuffer
;임시코드 끝
	call EraseMenuCursor
	ld a, $11 ; "ED" x coord
	ld [wTopMenuItemX], a
	ld a, $5 ; "ED" y corrd
	ld [wCurrentMenuItem], a
	ld a, [wNamingScreenType]
	cp NAME_MON_SCREEN
	ld a, 4 ; keep the last underscore raised
	jr nc, .pokemon3
	ld a, 4 ; keep the last underscore raised
.pokemon3
.emptySpacesRemaining
	call CalcStringLength
	;ld a, c
	;ld c, a
	ld b, $0
	coord hl, 12, 3
	add hl, bc
	ld [hl], $77 ; raised underscore tile id
	;ld de,UnderBar
	;call PlaceString
	ld bc,-SCREEN_WIDTH
	add hl, bc
	ld de,Blankstring
	call PlaceString
	coord hl, 17, 3
	ld [hl], $7F
	push af
	ld a,$01
	ld [H_AUTOBGTRANSFERENABLED],a
	pop af
	ret

DakutensAndHandakutens:
	push de
	call CalcStringLength
	dec hl
	ld a, [hl]
	pop hl
	ld de, $2
	call IsInArray
	ret nc
	inc hl
	ld a, [hl]
	ld [wNamingScreenLetter], a
	ret

Dakutens:
	db "かが", "きぎ", "くぐ", "けげ", "こご"
	db "さざ", "しじ", "すず", "せぜ", "そぞ"
	db "ただ", "ちぢ", "つづ", "てで", "とど"
	db "はば", "ひび", "ふぶ", "へべ", "ほぼ"
	db "カガ", "キギ", "クグ", "ケゲ", "コゴ"
	db "サザ", "シジ", "スズ", "セゼ", "ソゾ"
	db "タダ", "チヂ", "ツヅ", "テデ", "トド"
	db "ハバ", "ヒビ", "フブ", "へべ", "ホボ"
	db $ff

Handakutens:
	db "はぱ", "ひぴ", "ふぷ", "へぺ", "ほぽ"
	db "ハパ", "ヒピ", "フプ", "へぺ", "ホポ"
	db $ff
FinalConsoTbl:
	db $00,$00;X
	db $A0,$00;ㄱ
	db $A1,$00;ㄲ
	db $A0,$A9;ㄳ
	db $A2,$00;ㄴ
	db $A2,$AC;ㄵ
	db $A2,$B2;ㄶ
	db $A3,$00;ㄷ
	db $A5,$00;ㄹ
	db $A5,$A0;ㄺ
	db $A5,$A6;ㄻ
	db $A5,$A7;ㄼ
	db $A5,$A9;ㄽ
	db $A5,$B0;ㄾ
	db $A5,$B1;ㄿ
	db $A5,$B2;ㅀ
	db $A6,$00;ㅁ
	db $A7,$00;ㅂ
	db $A7,$A9;ㅄ
	db $A9,$00;ㅅ
	db $AA,$00;ㅆ
	db $AB,$00;ㅇ
	db $AC,$00;ㅈ
	db $AE,$00;ㅊ
	db $AF,$00;ㅋ
	db $B0,$00;ㅌ
	db $B1,$00;ㅍ
	db $B2,$00;ㅎ
FinalConsoTblEnd:
ConsoTbl:
	db $A0,$0B,$00; RAW DATA : 	db $A0,"ㄱ"
	db $A1,$0B,$0E; RAW DATA : 	db $A1,"ㄲ"
	db $A2,$0B,$01; RAW DATA : 	db $A2,"ㄴ"
	db $A3,$0B,$02; RAW DATA : 	db $A3,"ㄷ"
	db $A4,$0B,$0F; RAW DATA : 	db $A4,"ㄸ"
	db $A5,$0B,$03; RAW DATA : 	db $A5,"ㄹ"
	db $A6,$0B,$04; RAW DATA : 	db $A6,"ㅁ"
	db $A7,$0B,$05; RAW DATA : 	db $A7,"ㅂ"
	db $A8,$0B,$10; RAW DATA : 	db $A8,"ㅃ"
	db $A9,$0B,$06; RAW DATA : 	db $A9,"ㅅ"
	db $AA,$0B,$11; RAW DATA : 	db $AA,"ㅆ"
	db $AB,$0B,$07; RAW DATA : 	db $AB,"ㅇ"
	db $AC,$0B,$08; RAW DATA : 	db $AC,"ㅈ"
	db $AD,$0B,$12; RAW DATA : 	db $AD,"ㅉ"
	db $AE,$0B,$09; RAW DATA : 	db $AE,"ㅊ"
	db $AF,$0B,$0A; RAW DATA : 	db $AF,"ㅋ"
	db $B0,$0B,$0B; RAW DATA : 	db $B0,"ㅌ"
	db $B1,$0B,$0C; RAW DATA : 	db $B1,"ㅍ"
	db $B2,$0B,$0D; RAW DATA : 	db $B2,"ㅎ"
	db $B3,$0B,$20; RAW DATA : 	db $B3,"ㅏ"
	db $B4,$0B,$2A; RAW DATA : 	db $B4,"ㅐ"
	db $B5,$0B,$21; RAW DATA : 	db $B5,"ㅑ"
	db $B6,$0B,$2B; RAW DATA : 	db $B6,"ㅒ"
	db $B7,$0B,$22; RAW DATA : 	db $B7,"ㅓ"
	db $B8,$0B,$2C; RAW DATA : 	db $B8,"ㅔ"
	db $B9,$0B,$23; RAW DATA : 	db $B9,"ㅕ"
	db $BA,$0B,$2D; RAW DATA : 	db $BA,"ㅖ"
	db $BB,$0B,$24; RAW DATA : 	db $BB,"ㅗ"
	db $BC,$0B,$2E; RAW DATA : 	db $BC,"ㅘ"
	db $BD,$0B,$2F; RAW DATA : 	db $BD,"ㅙ"
	db $BE,$0B,$30; RAW DATA : 	db $BE,"ㅚ"
	db $BF,$0B,$25; RAW DATA : 	db $BF,"ㅛ"
	db $C0,$0B,$26; RAW DATA : 	db $C0,"ㅜ"
	db $C1,$0B,$31; RAW DATA : 	db $C1,"ㅝ"
	db $C2,$0B,$32; RAW DATA : 	db $C2,"ㅞ"
	db $C3,$0B,$33; RAW DATA : 	db $C3,"ㅟ"
	db $C4,$0B,$27; RAW DATA : 	db $C4,"ㅠ"
	db $C5,$0B,$28; RAW DATA : 	db $C5,"ㅡ"
	db $C6,$0B,$34; RAW DATA : 	db $C6,"ㅢ"
	db $C7,$0B,$29; RAW DATA : 	db $C7,"ㅣ"
ConsoTblEnd:
; calculates the length of the string at wcf4b and stores it in c

CalcStringLength:
	ld hl, wcf4b
	ld c, $0
.loop
	ld a, [hl]
	cp $50; RAW DATA : 	cp "@"
	ret z
	cp a,$0C ;한글?
	jr nc,.nonHangul
.Hangul
	inc hl
	inc hl
	inc c
	jr .loop
.nonHangul
	inc hl
	inc c
	jr .loop
CalcStringByteLength:
	ld hl, wcf4b
	ld c, $0
.loop
	ld a, [hl]
	cp $50; RAW DATA : 	cp "@"
	ret z
	cp a,$0C ;한글?
	jr nc,.nonHangul
.Hangul
	inc hl
	inc hl
	inc c
	inc c
	jr .loop
.nonHangul
	inc hl
	inc c
	jr .loop


	
PrintNamingText:
	coord hl, 2, 2
	ld a, [wNamingScreenType]
	ld de, YourTextString
	and a
	jr z, .notNickname
	ld de, RivalsTextString
	dec a
	jr z, .notNickname
	ld a, [wcf91]
	ld [wMonPartySpriteSpecies], a
	push af
	callba WriteMonPartySpriteOAMBySpecies
	pop af
	ld [wd11e], a
	call GetMonName
	coord hl, 4, 1
	call PlaceString
	ld hl, $1
	add hl, bc
	ld [hl], $c9
	ld h, b
	ld l, c
	ld de, OFstring
	call PlaceString
	coord hl, 4, 3
	ld de, NicknameTextString
	jr .placeString
.notNickname
	call PlaceString
	ld l, c
	ld h, b
	ld de, NameTextString
.placeString
	jp PlaceString
	
UnderBar:
	db $0B,$3E,$50
	
MidBar:
	db $0B,$3F,$50

Blankstring:
	db $0B,$6D,$50
	
YourTextString:
	db $02,$E7,$06,$65,$07,$97,$50

RivalsTextString:
	db $03,$C3,$07,$9C,$04,$FA,$07,$97,$50

NameTextString:
	db $07,$9C,$04,$37,$07,$8A,$0B,$67,$50

NicknameTextString:
	db $05,$10,$04,$8D,$07,$8A,$0B,$67,$50
OFstring:
	db $07,$97,$50	
HangulKeyboardTable:
	INCLUDE "hangul/keyboard-table.asm"
	
HangulFontMap:
	INCBIN "hangul/naming_screen_hangul.fnt"