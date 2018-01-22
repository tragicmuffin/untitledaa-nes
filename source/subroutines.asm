;;; 6502 Subroutine Library ;;;

; Modulo operator (https://gist.github.com/hausdorff/5993556)
; Load A and Y prior to calling. Returns A%Y in A.
mod:
  SEC			; set carry (C=1) to clear borrow
  STY temp1 	; store Y in a temporary variable
.modloop:
  SBC temp1		; subtract A - Y
  BCS .modloop	; loops if subtraction DID NOT produce a borrow (C=1)
  ADC temp1		; add Y back to A to get last positive modulus
  RTS

  
; Division operator (https://gist.github.com/hausdorff/5993556)
; Load A and Y prior to calling. Returns floor(A/Y) = quotient(A/Y) in A.
divide:
  LDX #$FF		; start X at $FF so subtraction count will begin at 0
  SEC
  STY temp1 	; store Y in a temporary variable
.divideloop:
  INX			; count number of loops with X (starting at 0)
  SBC temp1		; subtract A - Y
  BCS .divideloop
  TXA			; transfer quotient to A
  RTS
  
  
; prng (https://wiki.nesdev.com/w/index.php/Random_number_generator)
;
; 16-bit Galois linear feedback shift register with polynomial $002D.
; Returns a random 8-bit number in A/rng (0-255).
; Period: 65535
; Execution time: ~125 cycles
prng:
  LDX #8     ; iteration count (generates 8 bits)
  LDA rng+0
prng_step1:
  ASL A       ; shift the register
  ROL rng+1
  BCC prng_step2
  EOR #$2D   ; apply XOR feedback whenever a 1 bit is shifted out
prng_step2:
  DEX
  BNE prng_step1
  STA rng+0
  CMP #0     ; reload flags
  RTS

; Convert a 2-byte (16-bit) binary/hex number to a 5-byte decimal number (1 byte per decimal digit)
; Requires two variables: temp_dec (5 bytes), temp_bin (2 bytes).
;   temp_bin must be set before call. Result will be returned in temp_dec.
;   Note: Both variables are little endian. Pass temp_bin = LSB, MSB. Returns temp_dec = d5, d4, d3, d2, d1 (digits)
; BinaryToDecimal:
  ; LDA #$00 
  ; STA temp_dec+0
  ; STA temp_dec+1
  ; STA temp_dec+2
  ; STA temp_dec+3
  ; STA temp_dec+4
  ; LDX #$10 
; BitLoop:
  ; ASL temp_bin+0 
  ; ROL temp_bin+1
  ; LDY temp_dec+0
  ; LDA BinTable, y 
  ; ROL a
  ; STA temp_dec+0
  ; LDY temp_dec+1
  ; LDA BinTable, y 
  ; ROL a
  ; STA temp_dec+1
  ; LDY temp_dec+2
  ; LDA BinTable, y 
  ; ROL a
  ; STA temp_dec+2
  ; LDY temp_dec+3
  ; LDA BinTable, y 
  ; ROL a
  ; STA temp_dec+3
  ; ROL temp_dec+4
  ; DEX 
  ; BNE BitLoop 
  ; RTS 
; BinTable:
  ; .db $00, $01, $02, $03, $04, $80, $81, $82, $83, $84
  
  