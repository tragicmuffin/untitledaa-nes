;; Text Engine
;; Renders tiles from a stream onto background name table.

;; Notes:
 ; entrance for rendering text, text_draw
 ; entrances for rendering custom images and layouts
 ;
 ; text will be read from a ram location(?) in a stream, terminated with $FF
 ; opcodes for text starting position and newline (newline = add $20)
 ; $FD will indicate the start of an (optional) attribute table stream
 ; we may need to clone all text onto all nametables to handle scrolling

;;; Text stream format ;;;
  ; Text stream will always start with the starting location for the text, followed by an opcode.
  ; If the opcode is $F0, the next byte will give another number of bytes to read.
  ; After we finish reading a string of text bytes, another opcode will follow.
  ; Read bytes -> Opcode(s) -> ... cycle will continue until $FF opcode is reached.
  
  ; -- Opcodes --
  ; $FF:	End of stream
  ; $FE:	New line
  ; $F0:	Read a text string (followed by number of bytes to read)
  
; Store text engine variables starting at $0400
  .rsset $0400
  
;; Text stream
txstream_startaddr_LO 		.rs 1	; low byte of text starting position
txstream_startaddr_HI 		.rs 1	; high byte of text starting position

;;; Entrances (text engine accessor functions) ;;;

; text_draw reads a text stream and renders the text on screen.
;   input A: index of text stream
text_draw:
  ASL A					; multiply by 2 to index into a table of pointers (words)
  TAY
  LDA text_streams, y
  STA text_ptr			; initialize ptr variables (on zero-page) with start of text stream
  LDA text_streams+1, y
  STA text_ptr+1
  
  JSR te_read_stream	; process text stream at txstream_ptr

.done:
  RTS			; go back to main program
  
  
;;; Internal Subroutines ;;;
; te_read_stream reads through the text stream, getting tiles and handling opcodes
; It is assumed that text will not wrap, otherwise we will have possibly unexpected behavior.
te_read_stream:
  LDY #$00
  ; Read first two bytes to get drawing location
  LDA [text_ptr], y
  STA txstream_startaddr_HI
  JSR .update_pointer
  LDA [text_ptr], y
  STA txstream_startaddr_LO
  JSR .update_pointer
  
  JSR te_set_draw_location	; set PPU with initial drawing location
  
.opcode:
  LDA [text_ptr], y			; check next opcode
  JSR .update_pointer
.opcode_endofstream:
  CMP #$FF  ; end of stream
  BNE .opcode_newline
  JMP .end
.opcode_newline:
  CMP #$FE  ; new line
  BNE .opcode_readtext
  JMP .newline
.opcode_readtext:
  CMP #$F0  ; set length of text string
  BNE .opcode_undefined
  JMP .textstart
.opcode_undefined:
  JSR Debug_Probe
  JMP .end	; unrecognized opcode
  
  
.textstart
  ; Read next byte to get length of text string, store in X
  LDA [text_ptr], y
  TAX
  JSR .update_pointer
  
.textloop:
  LDA [text_ptr], y			; get next tile number in text string  
  STA $2007					; write tile number into name table
  JSR .update_pointer
  
  DEX
  BNE .textloop				; continue reading bytes until X reaches 0
  JMP .opcode				; text string is finished, read next opcode
  
.newline:
  LDA txstream_startaddr_LO
  CLC
  ADC #$20					; add $0020 to address to go to next row of name table
  BCC .newline_nowrap		; if we didn't wrap past $00, just store new LO byte
  INC txstream_startaddr_HI	; if we did wrap past $00, increment HI byte as well
.newline_nowrap:
  STA txstream_startaddr_LO
  
  JSR te_set_draw_location	; set PPU with next drawing location
  JMP .opcode
  
.end:
  RTS
; Careful: JSR here only!
.update_pointer:
  INY						; go to next byte in stream
  BNE .update_pointer_done	; if Y (LO byte) didn't wrap, we're done
  INC text_ptr+1			; if LO byte wrapped, increment HI byte
.update_pointer_done:
  RTS
  
; te_set_draw_location sets the PPU address of the starting tile for subsequent text bytes
te_set_draw_location:
  LDA $2002		; reset PPU write flipflop
  LDA txstream_startaddr_HI
  STA $2006		; write MSB of stream address
  LDA txstream_startaddr_LO
  STA $2006		; write LSB of stream address
  RTS
  
  
;;; Tables and Includes ;;;

;; Text streams
  .include "tx_text_streams.i"

;; Text streams (from included file)
text_streams:
  .dw txhead_Test0
  .dw txhead_Test1
  .dw txhead_Test2
  .dw txhead_Test3
  .dw txhead_TestText
  
  
;; Text stream aliases
tx_Test0	= $00
tx_Test1	= $01
tx_Test2	= $02
tx_Test3	= $03
tx_TestText	= $04
