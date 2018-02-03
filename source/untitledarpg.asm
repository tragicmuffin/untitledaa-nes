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
hscroll			.rs 1	; current horizontal scroll position
vscroll			.rs 1	; current vertical scroll position
ppu_cr1_nt		.rs 1	; current state of nametable bits (0-1) of ppu_cr1

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

ui_flag			.rs 1	; flag to indicate that sprite 0 has been hit and we're drawing the UI for the remaining frames

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

  ; Run external PPU initialization script
  .include "untitledarpg_initppu.asm"
  
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
  LDA #tx_TestText_NT0	; get text stream index from alias
  JSR text_draw			; render text stream using drawing engine
  ;LDA #tx_TestText_NT1	; get text stream index from alias
  ;JSR text_draw			; render text stream using drawing engine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Forever:
;; NMI has passed, and we've done the post-NMI handling
  LDA #$00
  STA ui_flag		; reset UI drawing flag
  INC sleeping		; set sleeping to 1 and loop until NMI sets it back to 0

; When we come back from NMI, we may still be in VBlank, which means the sprite 0 hit flag has not been reset.
; Wait here for it to be reset.
.loopWaitForClear:
  LDA $2002			; read current PPU status
  AND #%01000000	; check for sprite 0 collision
  BNE .loopWaitForClear ; loop until flag is clear
  
; Loop here until sprite 0 hit flag is set.
.loop:
  LDA $2002			; read current PPU status
  AND #%01000000	; check for sprite 0 collision
  BEQ .loop			; loop until a collision occurs
  
  LDA ppu_cr1		; get current state of PPUCTRL     *(could save 2 cycles here by using zero-page addressing)
  EOR #%00010000	; flip background pattern table bit (to sprite pattern table)
  STA $2000			; set new background pattern table (this is changed back at next NMI)
  
  LDA #$01
  STA ui_flag
  
  

; We can be sure that the sprite 0 collision above will happen before NMI, 
;   so we don't need to check for NMI until the collision handling is finished.
.loopNMI:
  LDA sleeping
  BNE .loopNMI


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
  
  
  LDA $00DF
  BNE .testdone
  LDA #tx_TestText_NT0	; get text stream index from alias
  JSR text_draw			; render text stream using drawing engine
  INC $00DF
.testdone
  
  
;; PPU clean up section, so rendering the next frame starts properly.
  LDA $2002		; reset PPU write flipflop
  LDA #$00
  STA $2006		; reinitialize VRAM pointer because it will be written to scroll reload bits
  STA $2006
  
  ;;;; TEMP: This should be put in game logic
  ;INC vscroll
  INC hscroll
  LDA #$01
  ;STA hscroll
    
;; Scrolling
HorizontalWrapCheck:
  LDA hscroll			; check if the horizontal scroll hit 256 = 0
  CMP #0
  BNE .h_wrapdone
  
.h_wrapswap:
  LDA ppu_cr1_nt		; load current Nametable number (%00 or %01)
  EOR #%00000001		; exclusive OR of bit 0 will flip that bit
  STA ppu_cr1_nt

.h_wrapdone:
  LDA ui_flag			; check if we're drawing the UI
  BEQ .h_notui
  LDA #$00				; if so, set scroll to 0
  JMP .h_ui
.h_notui:
  LDA hscroll			; if not, set scroll to current position
.h_ui:
  STA $2005				; first write: update horizontal scroll position
  
  
VerticalWrapCheck:
  LDA vscroll			; check if the vertical scroll hit 240 (wrap to 0)
  CMP #240
  BNE .v_wrapdone
  
.v_wrapswap:
  LDA #0				; reset vscroll to 0
  STA vscroll
  
  ; - This doesn't work as intended due to vertical mirroring
  ;LDA ppu_cr1_nt		; load current Nametable number (%00 or %10)
  ;EOR #%00000010		; exclusive OR of bit 1 will flip that bit
  ;STA ppu_cr1_nt

.v_wrapdone:
  LDA ui_flag			; check if we're drawing the UI
  BEQ .v_notui
  LDA #$00				; if so, set scroll to 0
  JMP .v_ui
.v_notui:
  LDA vscroll			; if not, set scroll to current position
.v_ui:
  STA $2005				; second write: update vertical scroll position
  

  ; Write state of PPUCRTL and PPUMASK
  LDA ppu_cr1			; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ORA ppu_cr1_nt		; replace nametable bits with scroll data
  STA $2000
  LDA ppu_cr2			; enable sprites, enable background, no clipping on left side
  STA $2001
  
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
  .db $0F,$00,$20,$01,  $0F,$17,$28,$38,  $0F,$3F,$3D,$2D,  $0F,$27,$37,$17			; background palette
  .db $0F,$07,$01,$08,  $0F,$07,$3E,$00,  $0F,$38,$16,$06,  $0F,$27,$3F,$17			; sprite palette

  
;; Sprites ;;
sprites:
  .db $CF, $C0, %00000011, $D8  ; sprite 0: used for pattern table switching

;; Background nametables ;;
background_Test0:
  .incbin "untitledarpg_Test0.nam"
background_Test1:
  .incbin "untitledarpg_Test1.nam"
background_Test2:
  .incbin "untitledarpg_Test2.nam"
background_Test3:
  .incbin "untitledarpg_Test3.nam"
  
  
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
  .incbin "untitledarpg_Test.chr"  ; include 8KB graphics file
  
  
  