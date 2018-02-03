InitializePPU:

;; Load initial sprite data ;;
InitLoadSprites:
  ; start DMA transfer of sprite data starting from $0200 to SPR-RAM
  LDA #$00
  STA $2003		; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014		; set the high byte (02) of the RAM address, start the transfer
  LDX #$00
.loop:
  LDA sprites, x
  STA $0200, x
  INX
  CPX #(NUMSPRITES*4)  ; load all sprites (4 data bytes each)
  BNE .loop
  ;LDA ( sprites+1 ), x  ; 64*4 = 256 bytes, but loop can only run 255 times (max of X).
  ;STA ( $0201+$01 ), x  ; If all 256 sprites are needed, uncomment these lines to run an extra time.

;; Load initial nametable data ;;
 ; Nametable 0
InitLoadNametable0:
  LDA $2002					; read PPU status to reset the high/low latch
  LDA #$20					; write the high byte of $2000 address (nametable 0)
  STA $2006
  LDA #$00					; write the low byte of $2000 address (nametable 0)
  STA $2006
  LDX #$00
  LDY #$00
  LDA #LOW(background_Test0)	; load 2-byte address of background into ppu_ptr for indirect addressing
  STA ppu_ptr
  LDA #HIGH(background_Test0)
  STA ppu_ptr+1
  JSR LoadNametable
 ; Nametable 1
InitLoadNametable1:
  LDA $2002					; read PPU status to reset the high/low latch
  LDA #$24					; write the high byte of $2400 address (nametable 1)
  STA $2006
  LDA #$00					; write the low byte of $2400 address (nametable 1)
  STA $2006
  LDX #$00
  LDY #$00
  LDA #LOW(background_Test1)	; load 2-byte address of background into ppu_ptr for indirect addressing
  STA ppu_ptr
  LDA #HIGH(background_Test1)
  STA ppu_ptr+1
  JSR LoadNametable
 ; ; Nametable 2
; InitLoadNametable2:
  ; LDA $2002					; read PPU status to reset the high/low latch
  ; LDA #$28					; write the high byte of $2800 address (nametable 2)
  ; STA $2006
  ; LDA #$00					; write the low byte of $2800 address (nametable 2)
  ; STA $2006
  ; LDX #$00
  ; LDY #$00
  ; LDA #LOW(background_Test2)	; load 2-byte address of background into ppu_ptr for indirect addressing
  ; STA ppu_ptr
  ; LDA #HIGH(background_Test2)
  ; STA ppu_ptr+1
  ; JSR LoadNametable
 ; ; Nametable 3
; InitLoadNametable3:
  ; LDA $2002					; read PPU status to reset the high/low latch
  ; LDA #$2C					; write the high byte of $2C00 address (nametable 3)
  ; STA $2006
  ; LDA #$00					; write the low byte of $2C00 address (nametable 3)
  ; STA $2006
  ; LDX #$00
  ; LDY #$00
  ; LDA #LOW(background_Test3)	; load 2-byte address of background into ppu_ptr for indirect addressing
  ; STA ppu_ptr
  ; LDA #HIGH(background_Test3)
  ; STA ppu_ptr+1
  ; JSR LoadNametable
  
;; Load initial palette data ;;
InitLoadPalette:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address (palette area)
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address (palette area)
  LDX #$00              ; start out at 0
.loop:
  LDA palette, x
  STA $2007             ; write to PPU
  INX
  CPX #32			    ; run 32 times for 16 bg colors and 16 sprite colors
  BNE .loop