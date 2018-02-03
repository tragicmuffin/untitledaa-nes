; Text streams

  ; Test 1234 / 56
txhead_Test0:  ; prints TEST starting at tile $214D = (10, 13)
  .db $22, $4D
  .db $F0, 4, $63, $64, $65, $66, $FE
  .db $F0, 2, $67, $68, $FF
  
txhead_Test1:
  .db $20, $CA
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 4, $60, $60, $60, $60, $FE
  .db $F0, 7, $E3, $E4, $E3, $E4, $E3, $E4, $C3, $FF ; scrolls (after 47 bytes, 5 newlines)
  
txhead_Test2:
  .db $20, $C6
  .db $F0, 8, $60, $60, $60, $60, $60, $60, $60, $60
  .db $F0, 8, $60, $60, $60, $60, $60, $60, $60, $60
  .db $F0, 8, $60, $60, $60, $60, $60, $60, $60, $60
  .db $F0, 8, $60, $60, $60, $60, $60, $60, $60, $60
  .db $F0, 8, $60, $60, $60, $60, $60, $60, $60, $60
  .db $F0, 1, $60 ; scrolls after 41 tiles, 51 instructions (+2 for address)
  .db $FF
  
txhead_Test3:
  .db $20, $C6
  .db $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE
  .db $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE
  .db $FE, $FE, $FE, $FE ; scrolls after 20 newlines
  .db $FF
  
txhead_TestText_NT0:
  .db $23, $96
  .db $F0, 4, $ED, $DE, $EC, $ED
  .db $FF
  
txhead_TestText_NT1:
  .db $27, $96
  .db $F0, 4, $ED, $DE, $EC, $ED
  .db $FF
  