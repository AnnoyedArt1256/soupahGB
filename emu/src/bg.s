.include "snes.inc"
.include "global.inc"
.smart
.segment "ZEROPAGE"
bg_frame: .res 2
bg_frame_obj: .res 1

.segment "CODE5"
.proc load_bg_pal
  phb  ; Save old program bank and push new bank
  phk
  plb

  ; Copy background palette to the S-PPU.
  ; We perform the copy using DMA (direct memory access), which has
  ; four steps:
  ; 1. Set the destination address in the desired area of memory,
  ;    be it CGRAM (palette), OAM (sprites), or VRAM (tile data and
  ;    background maps).
  ; 2. Tell the DMA controller which area of memory to copy to.
  ; 3. Tell the DMA controller the starting address to copy from.
  ; 4. Tell the DMA controller how big the data is in bytes.
  ; ppu_copy uses the current data bank as the source bank
  ; for the copy, so set the source bank.
  seta8
  stz CGADDR  ; Seek to the start of CGRAM
  setaxy16
  lda #DMAMODE_CGDATA
  ldx #palette & $FFFF
  ldy #palette_size
  jsl ppu_copy

  ; This demo uses background mode 0, in which each of four 2bpp
  ; layers gets its own palette, at word offsets $00-$1F, $20-$3F,
  ; $40-$5F, and $60-$7F in CGRAM.  (The other modes with 2bpp
  ; backgrounds use only $00-$1F.)  We just finished copying the
  ; main palette to $00; also copy it to $20 for the second layer
  ; in demo modes that use it.  The palettes could be different,
  ; but in this demo they're the same.
  seta8
  lda #$80
  sta CGADDR
  setaxy16
  lda #DMAMODE_CGDATA
  ldx #palette & $FFFF
  ldy #palette_size
  jsl ppu_copy

  plb
  rtl
.endproc

.proc load_bg_chr
  phb  ; Save old program bank and push new bank
  phk
  plb

  setaxy8

  lda z:bg_frame
  and #1
  eor #1
  sta z:bg_frame

  lda #$80  ; +1 on PPUDATA high byte write
  sta PPUCTRL

  lda a:IO_SPACE+$40
  and #1<<4
  beq @block_wrap

  setaxy16
  lda #$0000
  ldx z:bg_frame
  beq :+
  lda #$0400
:
  sta PPUADDR  ; we will start video memory at $0000
  ldx #VRAM & $FFFF
  ldy z:bg_frame
  beq :+
  ldx #(VRAM+$800) & $FFFF
:
  lda #DMAMODE_PPUDATA
  ldy #2048
  jsl ppu_copy_WRAM

  plb
  rtl
 @block_wrap:

  setaxy16
  lda #$0000
  ldx z:bg_frame
  beq :+
  lda #$0400
:
  sta PPUADDR  ; we will start video memory at $0000
  ldx #(VRAM+$1000) & $FFFF
  ldy z:bg_frame
  beq :+
  ldx #(VRAM+$800) & $FFFF
:
  lda #DMAMODE_PPUDATA
  ldy #2048
  jsl ppu_copy_WRAM

  plb
  rtl
.endproc

.proc load_obj_chr
  phb  ; Save old program bank and push new bank
  phk
  plb

  setaxy8

  lda #0  ; +1 on PPUDATA high byte write
  sta PPUCTRL

  lda z:bg_frame_obj
  inc a
  and #3
  sta z:bg_frame_obj

  setaxy16
  lda z:bg_frame_obj-1
  and #$0300
  asl
  asl
  tax
  clc
  adc #$4000
  sta PPUADDR  ; we will start video memory at $0000

  txa
  clc
  adc #$8000
  tax

  lda #(<PPUDATA << 8)
  ldy #8192>>3
  jsl ppu_copy_OAM

  plb
  rtl
.endproc


.proc draw_bg
  phb  ; Save old program bank and push new bank
  phk
  plb

  setaxy8

  lda #0  ; +1 on PPUDATA high byte write
  sta PPUCTRL

  lda a:IO_SPACE+$40
  and #1<<3
  beq @block_wrap

  setaxy16
  lda #$1000
  sta PPUADDR  ; we will start video memory at $0000
  lda #(<PPUDATA << 8)
  ldy #1024
  ldx #(VRAM + $1C00) & $FFFF
  jsl ppu_copy_WRAM

  plb
  rtl
 @block_wrap:

  setaxy16
  lda #$1000
  sta PPUADDR  ; we will start video memory at $0000
  lda #(<PPUDATA << 8)
  ldy #1024
  ldx #(VRAM + $1800) & $FFFF
  jsl ppu_copy_WRAM

  plb
  rtl
.endproc

.proc draw_win
  phb  ; Save old program bank and push new bank
  phk
  plb

  setaxy8

  lda #0  ; +1 on PPUDATA high byte write
  sta PPUCTRL

  lda a:IO_SPACE+$40
  and #1<<5
  bne :+
  ;plb
  ;rtl
:

  lda a:IO_SPACE+$40
  and #1<<6
  beq @block_wrap

  setaxy16
  lda #$1400
  sta PPUADDR  ; we will start video memory at $0000
  lda #(<PPUDATA << 8)
  ldy #1024
  ldx #(VRAM + $1C00) & $FFFF
  jsl ppu_copy_WRAM

  plb
  rtl
 @block_wrap:

  setaxy16
  lda #$1400
  sta PPUADDR  ; we will start video memory at $0000
  lda #(<PPUDATA << 8)
  ldy #1024
  ldx #(VRAM + $1800) & $FFFF
  jsl ppu_copy_WRAM

  plb
  rtl
.endproc

.proc ppu_copy_WRAM
  php
  setaxy16
  sta DMAMODE
  stx DMAADDR
  sty DMALEN
  seta8
  lda #$7e
  sta DMAADDRBANK
  lda #%00000001
  sta COPYSTART
  plp
  rtl
.endproc


.proc ppu_copy_OAM
  php
  setaxy16
  sta DMAMODE
  stx DMAADDR
  sty DMALEN
  seta8
  lda #$7f
  sta DMAADDRBANK
  lda #%00000001
  sta COPYSTART
  plp
  rtl
.endproc

.segment "RODATA5"

; The background palette
palette:
  ;.word RGB(31,31,31)
  ;.word RGB(31,31,31),RGB(24,24,24),RGB(16,16,16),RGB(0,0,0)
  ;.word $2ca8, $3175, $1a5a, $6f67
  ;.word RGB($f0>>3,$e0>>3,$a0>>3), $1a5a, $3175, $2ca8
  .word RGB($f8>>3,$e8>>3,$a8>>3), $1a5a, $3175, $2ca8
  ;.word RGB(31,0,0),RGB(0,31,0),RGB(0,0,31),RGB(31,28,0)
  ;.word RGB(31,15,28),RGB(8,28,31),RGB(15,9,0),RGB(30,24,16)
  ;.word RGB(31,15,0),RGB(16,0,12),RGB(26,26,4)
palette_size = * - palette

; The background palette
layer1_palette:
    .word RGB(0,0,0), RGB(0,0,0), RGB(31,31,31), RGB(5,5,5)

layer1_palette_size = * - layer1_palette
