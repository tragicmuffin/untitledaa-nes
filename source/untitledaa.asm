;;;; | Untitled Diablo Clone [WIP]
;;;; | Jesse Williams

;; Game state flowchart:

;; RAM Layout:
 ; Page 0 ($0000-$00FF) - Main game variables
 ; Page 1 ($0100-$01FF) - Stack
 ; Page 2 ($0200-$02FF) - Sprite data
 ; Page 3 ($0300-$03FF) - Sound engine variables and data
 ; Page 4 ($0400-$04FF) - Rendering engine variables and data

;; PRG-ROM Layout:
 ; $8000-$9FFF (bank 0): Sound Engine
 ; $A000-$BFFF (bank 1): Text and UI Rendering Engines
 ; $C000-$DFFF (bank 2): Main program code
 ; $E000-$FFFA (bank 3): Palettes, sprite data, background name/attr tables
 ; $FFFA-$FFFF (bank 3): Interrupt vectors


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iNES Header Directives ;;
  .inesprg 2   ; 2x 16KB PRG code banks (banks 0, 1, 2, 3)
  .ineschr 1   ; 1x  8KB CHR data banks (bank 4)
  .inesmap 0   ; mapper (0 = NROM), no bank swapping
  .inesmir 1   ; 0 = horizontal mirroring (vertical scrolling), 1 = vertical mirroring (horizontal scrolling)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Declare variables ;;
 ; Start variables at ram location $0000.
 ; This is within zero-page ($0000-$00FF) and so should be used only for heavy-use variables.
  .rsset $0000
  
gamestate		.rs 1
rng				.rs 2	; 16-bit seed for 8-bit random number (in rng+0)
ppu_cr1			.rs 1	; state of PPU Control Register 1 ($2000, PPUCTRL)
ppu_cr2			.rs 1	; state of PPU Control Register 2 ($2001, PPUMASK)
ppu_sprite0		.rs 1	; state of sprite 0 collision flag from previous read (flag is in bit 6)
inputOne		.rs 1	; player 1: gamepad buttons pressed on current frame, one bit per button
inputOne_Last	.rs 1	; player 1: buttons pressed during last frame
inputOne_Pressed .rs 1	; player 1: buttons pressed since last frame (off-to-on transitions)
inputTwo		.rs 1	; player 2: gamepad buttons pressed on current frame, one bit per button
inputTwo_Last	.rs 1	; player 2: buttons pressed during last frame
inputTwo_Pressed .rs 1	; player 2: buttons pressed since last frame (off-to-on transitions)

sleeping		.rs 1	; starts game logic, triggered by end of NMI
temp1			.rs 1	; general-purpose temp variable
temp2			.rs 1	; general-purpose temp variable
ppu_ptr			.rs 2	; pointer used for indirect addressing by nametable loader

ui_redrawflag	.rs 1	; flag to indicate that UI should be redrawn

; Sound engine variables
sound_ptr		.rs 2	; address pointer used by sound engine for indirect addressing (must be on zero-page)

; Text engine variables
text_ptr		.rs 2	; address pointer used by text engine for indirect addressing (must be on zero-page)

;; Declare constants ;;
 ; These are not stored in ROM, thus they can be declared anywhere.
STATETITLE		= $00  ; displaying title screen
STATEPLAYING	= $01  ; playing game

NUMSPRITES		= 1




  .bank 2
  .org $C000  ; start of main game ROM
  
;; Reset script ;;
vblankwait:
  BIT $2002
  BPL vblankwait
  RTS
  
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankwait  ; First wait for vblank to make sure PPU is ready
  
clrmem:
  LDA #$00
  STA $0000, x  ; initializes all variables to 0
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
  JSR vblankwait  ; Second wait for vblank, PPU is ready after this

  
;; Set initial values (on RESET) ;;
  LDA $00
  STA gamestate
  
  ; Set default 16-bit rng seed
  LDA #$12
  STA rng		; store high 8 bits in rng address
  LDA #$34
  STA rng+1		; store low 8 bits in rng address
  
  ; Set initial PPU state
  LDA #%10010000  	; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA ppu_cr1		; port $2000
  LDA #%00011110  	; enable sprites, enable background, no clipping on left side
  STA ppu_cr2		; port $2001

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
InitLoadNametables:
  LDA $2002					; read PPU status to reset the high/low latch
  LDA #$20					; write the high byte of $2000 address (nametable 0)
  STA $2006
  LDA #$00					; write the low byte of $2000 address (nametable 0)
  STA $2006
  LDX #$00
  LDY #$00
  LDA #LOW(background)		; load 2-byte address of background into ppu_ptr for indirect addressing
  STA ppu_ptr
  LDA #HIGH(background)
  STA ppu_ptr+1
  JSR LoadNametable
  
;; Load initial palette data ;;
InitLoadPalettes:
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
  
; Write initial PPU state (must be done after data is loaded)
  LDA ppu_cr1
  STA $2000
  LDA ppu_cr2
  STA $2001
  
; Initialize sound engine
  JSR sound_init
  
  LDA #sng_Silence		; load and play song
  JSR sound_load
  
;; OTHER INITIALIZATION STUFF HERE ;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Forever:
;; NMI has passed, and we've done the post-NMI handling
  INC sleeping		; set sleeping to 1 and loop until NMI sets it back to 0
  
; Cycle counting
; HBlank lasts 21 cycles
; Worst case scenario: Collision flag is flipped on right after LDA $2002
; In this case, it will take 2+2+3+2+4+4+3 = 20 cycles to get to next check
; Afterwards, it will take 4+2+2+2+4+2+4+2+4 = 26 cycles to update $2000 register
; Thus a total of 20+26 = 46 cycles will be needed


.loop:
  LDA $2002			; read current PPU status												; 4 cycles
  AND #%01000000	; check for sprite 0 collision											; 2 cycles
  TAY				; save new flag value in Y so we can update ppu_sprite0 later			; 2 cycles
  BEQ .noupdate		; if new flag is 0, no need to check for transition						; 2/3 cycles (3 on branch)
  CMP ppu_sprite0 	; compare to last state of sprite 0 collision flag from previous read	; 4 cycles
  BEQ .noupdate		; if flag was the same last time we checked, do nothing					; 2/3 cycles (3 on branch)
  ; if set (0->1 transition), update background pattern table
  LDA ppu_cr1		; get current state of PPUCTRL											; 4 cycles
  EOR #%00010000	; flip background pattern table bit (to sprite pattern table)			; 2 cycles
  STA $2000			; set new background pattern table (this is changed back at next NMI)	; 4 cycles
.noupdate:
  TYA				; restore new flag value from Y											; 2 cycles
  STA ppu_sprite0	; save new flag value for next loop										; 4 cycles

  LDA sleeping		; wait for NMI to clear sleep flag										; 4 cycles
  BNE .loop																					; 2/3 cycles (3 on branch)
  
;; NMI is done. The following code will run once after NMI.
  JSR ReadControllerOne
  ;JSR ReadControllerTwo
  JSR HandleInputOne
  ;JSR HandleInputTwo
  
  JSR sound_play_frame	; play next frame of loaded sound file
  
  JMP Forever

  
;;; Start of VBlank ;;;
  ; VBlank is the only safe time to write to PPU.
  ; Thus, all PPU-writing code should be near the start of NMI.
  ; Code towards the end of NMI can potentially run past VBlank into next screen drawing cycle.
  ; This is fine for all code that does not write to PPU (unless it runs long enough to get to next NMI).
NMI:
  PHA  ; Backup register state
  TXA
  PHA
  TYA
  PHA
  
  LDA #tx_TestText	; get text stream index from alias
  JSR text_draw		; render text stream using drawing engine
  
  LDA ui_redrawflag
  BEQ .uidone
  ;JSR RedrawUI		; redraw UI if updates were made
.uidone:
  
;; PPU clean up section, so rendering the next frame starts properly.
  LDA $2002		; reset PPU write flipflop
  LDA #$00
  STA $2006		; reinitialize VRAM pointer because it will be written to scroll reload bits
  STA $2006

  LDA ppu_cr1		; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA ppu_cr2		; enable sprites, enable background, no clipping on left side
  STA $2001
;; Set background scroll (none)
  LDA #$00
  STA $2005		; write horizontal scroll
  STA $2005		; write vertical scroll
  
  
;; Initiate sprite update
  LDA #$00
  STA $2003			; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014			; set the high byte (02) of the RAM address, start the transfer


  
  LDA #$00
  STA sleeping
  
  PLA  ; Restore register state
  TAY
  PLA
  TAX
  PLA
  RTI  ; End of NMI



;; Game Subroutines ;;

; Read 8 bits of controller 1 state and store new button presses in 'inputOne_Pressed'
ReadControllerOne:
  LDA inputOne
  STA inputOne_Last		; save buttons pressed last frame
  
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
.loop:
  LDA $4016
  LSR A			; bit0 -> Carry
  ROL inputOne	; bit0 <- Carry
  DEX
  BNE .loop
  
  LDA inputOne_Last		; check buttons pressed last frame
  EOR #%11111111		; get what was *not* pressed last frame
  AND inputOne			; AND to find only buttons that were pressed since last frame (if inputOne_Last==inputOne, we get $00 here.)
  STA inputOne_Pressed	; save these new button presses (off-to-on transitions)
  RTS
  
; Read 8 bits of controller 2 state and store new button presses in 'inputTwo_Pressed'
ReadControllerTwo:
  LDA inputTwo
  STA inputTwo_Last		; save buttons pressed last frame
  
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
.loop:
  LDA $4017
  LSR A				; bit0 -> Carry
  ROL inputTwo		; bit0 <- Carry
  DEX
  BNE .loop
  
  LDA inputOne_Last		; check buttons pressed last frame
  EOR #%11111111		; get what was *not* pressed last frame
  AND inputOne			; AND to find only buttons that were pressed since last frame (if inputTwo_Last==inputTwo, we get $00 here.)
  STA inputOne_Pressed	; save these new button presses (off-to-on transitions)
  RTS

; Loads buttons for controller 1 and handles pressed buttons.
;   Note: ALL buttons will be processed. Multiple button presses will cause multiple subroutines to run.
HandleInputOne:
  LDA inputOne_Pressed
  BEQ .done			; if nothing is pressed, skip checks
.a:
  LDA inputOne_Pressed
  AND #%10000000	; check bit 7 (A button)
  BEQ .b			; if bit 7 wasn't set, check next button
  ;JSR DOSOMETHING	; if bit 7 was set, register A button press
.b:
  LDA inputOne_Pressed
  AND #%01000000
  BEQ .select
  ;JSR DOSOMETHING
.select:
  LDA inputOne_Pressed
  AND #%00100000
  BEQ .start
  ;JSR DOSOMETHING
.start:
  LDA inputOne_Pressed
  AND #%00010000
  BEQ .up
  ;JSR DOSOMETHING
.up:
  LDA inputOne_Pressed
  AND #%00001000
  BEQ .down
  ;JSR DOSOMETHING
.down:
  LDA inputOne_Pressed
  AND #%00000100
  BEQ .left
  ;JSR DOSOMETHING
.left:
  LDA inputOne_Pressed
  AND #%00000010
  BEQ .right
  ;JSR DOSOMETHING
.right:
  LDA inputOne_Pressed
  AND #%00000001
  BEQ .done
  ;JSR DOSOMETHING
.done:
  RTS
  
; Loads buttons for controller 2 and handles pressed buttons.
;   Note: ALL buttons will be processed. Multiple button presses will cause multiple subroutines to run.
HandleInputTwo:
  LDA inputTwo_Pressed
  BEQ .done			; if nothing is pressed, skip checks
.a:
  LDA inputTwo_Pressed
  AND #%10000000	; check bit 7 (A button)
  BEQ .b			; if bit 7 wasn't set, check next button
  ;JSR DOSOMETHING	; if bit 7 was set, register A button press
.b:
  LDA inputTwo_Pressed
  AND #%01000000
  BEQ .select
  ;JSR DOSOMETHING
.select:
  LDA inputTwo_Pressed
  AND #%00100000
  BEQ .start
  ;JSR DOSOMETHING
.start:
  LDA inputTwo_Pressed
  AND #%00010000
  BEQ .up
  ;JSR DOSOMETHING
.up:
  LDA inputTwo_Pressed
  AND #%00001000
  BEQ .down
  ;JSR DOSOMETHING
.down:
  LDA inputTwo_Pressed
  AND #%00000100
  BEQ .left
  ;JSR DOSOMETHING
.left:
  LDA inputTwo_Pressed
  AND #%00000010
  BEQ .right
  ;JSR DOSOMETHING
.right:
  LDA inputTwo_Pressed
  AND #%00000001
  BEQ .done
  ;JSR DOSOMETHING
.done:
  RTS
  
;; Load a nametable file into PPU RAM
 ; Loop using indirect indexed addressing mode to load a large amount of data (1KB of nametable data)
LoadNametable:
  LDA [ppu_ptr], Y					; load data using indirect indexed addressing (Y must be used in this mode)
  STA $2007							; write to PPU
  INY
  CPY #$FF
  BNE LoadNametable  ; branch when Y reaches $FF = 255 (255 bytes have been loaded).
  LDA [ppu_ptr], Y					; since the loop ends before Y=$FF is used, run one more time to get to 256 bytes.
  STA $2007							; write to PPU
  INY								; increment Y to overflow back to $00 and prepare for next round
  INX								; increment X now that the first of four blocks of 256 bytes is done
  INC ppu_ptr+1						; move offset to next 256-byte block of memory
  CPX #$04
  BNE LoadNametable	; when X=$04, 4 rounds of 256 are complete for a full 1024 bytes read.
  RTS


;; External Subroutines ;;
  .include "subroutines.asm"

;; Debugging subroutines ;;

; Increments RAM address $00D0 when called. Indicates how many times (if any) a line is being reached.
Debug_Probe:
  PHA		; save A to stack
  LDA $00D0
  CLC
  ADC #1
  STA $00D0
  PLA		; restore A from stack
  RTS
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .bank 3
  .org $E000
;; Palettes ;;
 ; First color in each 4-color block is used as the transparency color, usually left as $0F.
 ; Any sprite pixel assigned the transparency color will let background pass through.
palette:
  .db $0F,$00,$30,$01,  $0F,$17,$28,$38,  $0F,$3F,$3D,$2D,  $0F,$27,$37,$17			; background palette
  .db $0F,$07,$01,$08,  $0F,$07,$3E,$00,  $0F,$38,$16,$06,  $0F,$27,$3F,$17			; sprite palette

  
;; Sprites ;;
sprites:
  .db $CF, $01, %00000011, $F8  ; sprite 0: used for pattern table switching

;; Background nametables ;;
background:
  .incbin "untitledaa_Test.nam"
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define interrupt vectors ;;
  .bank 3
  .org $FFFA     ; first of the three vectors starts here
  
  .dw NMI        ; when an NMI happens (once per frame if enabled) the processor will jump to the label NMI:
  .dw RESET      ; when the processor first turns on or is reset, it will jump to the label RESET:
  .dw 0          ; external interrupt IRQ unused
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Import sound engine code (starting at $8000) ;;
  .bank 0
  .org $8000
  .include "soundengine.asm"
  
;; Import text engine code (starting at $A000) ;;
  .bank 1
  .org $A000
  .include "textengine.asm"
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load graphics file into CHR-ROM ;;
  .bank 4  ; store sprite/bg data in bank 4 (CHR-ROM)
  
  ; Pattern table 0 - Sprites ($0000-$0FFF), Pattern table 1 - Background ($1000-$1FFF)
  .org $0000
  .incbin "untitledaa_Test.chr"  ; include 8KB graphics file
  
  
  