.include "snes.inc"
.include "global.inc"
.smart
.export main, nmi_handler

USE_AUDIO = 1

.segment "ZEROPAGE"

nmis: .res 1
triggered: .res 2
tick: .res 2
IS_PAL: .res 2
framecount: .res 1

.segment "BSS"
.align 256
OAM:   .res 512
OAMHI: .res 32
; OAMHI contains bit 8 of X (the horizontal position) and the size
; bit for each sprite.  It's a bit wasteful of memory, as the
; 512-byte OAMHI needs to be packed by software into 32 bytes before
; being sent to the PPU, but it makes sprite drawing code much
; simpler.  The OBC1 coprocessor used in the game Metal Combat:
; Falcon's Revenge performs the same packing function in hardware,
; possibly as a copy protection method.

.segment "CODE"
;;
; Minimalist NMI handler that only acknowledges NMI and signals
; to the main thread that NMI has occurred.
.proc nmi_handler
  ; Because the INC and BIT instructions can't use 24-bit (f:)
  ; addresses, set the data bank to one that can access low RAM
  ; ($0000-$1FFF) and the PPU ($2100-$213F) with a 16-bit address.
  ; Only banks $00-$3F and $80-$BF can do this, not $40-$7D or
  ; $C0-$FF.  ($7E can access low RAM but not the PPU.)  But in a
  ; LoROM program no larger than 16 Mbit, the CODE segment is in a
  ; bank that can, so copy the data bank to the program bank.

  pha
  phx
  phy

  php
  phb

  phk
  plb

  seta8
  inc a:nmis       ; Increase NMI count to notify main thread
  bit a:NMISTATUS  ; Acknowledge NMI

  lda a:PPUSTATUS2
  and #$10
  lsr
  lsr
  lsr
  lsr
  sta z:IS_PAL
  stz z:IS_PAL+1

  setaxy8
  stz a:TM

  setaxy8
  lda LCDC_mirror
  and #$80
  beq @skip_NMI

  lda #%00010001
  sta a:TM

  jsl do_hdma

  lda z:do_vblank
  beq :+
  stz z:do_vblank
  jsl load_bg_chr
  jsl draw_bg
  bra :++
:
  jsl load_obj_chr
:
@skip_NMI:
  jsl update_pal

  ; And restore the previous data bank value.
  plb
  plp

  ply
  plx
  pla
  rti
.endproc

;;
; This program doesn't use IRQs either.
.proc irq_handler
  pha
  phx
  phy

  php
  phb

  phk
  plb

  setaxy8
  lda a:IO_SPACE+$40
  sta z:LCDC_mirror
  and #$80
  beq @skip_IRQ

  setaxy8
  lda #$80
  sta PPUBRIGHT

  jsl ppu_update
  jsl draw_win
  setaxy8
  lda a:IO_SPACE+$40
  and #1<<1
  beq :+
  setaxy16
  jsl ppu_copy_oam
:

@skip_IRQ:
  setaxy8
  lda #$0f
  sta PPUBRIGHT
  lda a:TIMEUP

  ; And restore the previous data bank value.
  plb
  plp

  ply
  plx
  pla
  rti
.endproc

.segment "CODE"
; init.s sends us here

.proc main

  ; In the same way that the CPU of the Commodore 64 computer can
  ; interact with a floppy disk only through the CPU in the 1541 disk
  ; drive, the main CPU of the Super NES can interact with the audio
  ; hardware only through the sound CPU.  When the system turns on,
  ; the sound CPU is running the IPL (initial program load), which is
  ; designed to receive data from the main CPU through communication
  ; ports at $2140-$2143.  Load a program and start it running.
  .if ::USE_AUDIO
    jsl spc_boot_apu
  .endif

  seta8
  setxy16
  lda #$0
  ldx #$0
:
  sta $0, x
  inx
  cpx #$100
  bne :-

  ldx #$200
:
  stz $0000, x
  inx
  cpx #$2000
  bne :-

  setaxy8
  stz tick
  stz triggered
  nop

  ; In LoROM no larger than 16 Mbit, all program banks can reach
  ; the system area (low RAM, PPU ports, and DMA ports).
  ; This isn't true of larger LoROM or of HiROM (without tricks).
  phk
  plb

  ; Program the PPU for the display mode
  seta8
  lda #0
  sta BGMODE     ; mode 0 (four 2-bit BGs) with 8x8 tiles
  stz BGCHRADDR  ; bg planes 0-1 CHR at $0000

  ; OBSEL needs the start of the sprite pattern table in $2000-word
  ; units.  In other words, bits 14 and 13 of the address go in bits
  ; 1 and 0 of OBSEL.
  lda #$4000 >> 13
  sta OBSEL      ; sprite CHR at $4000, sprites are 8x8 and 16x16
  lda #>$6000
  sta NTADDR+0   ; plane 0 nametable at $6000
  lda #>$6800
  sta NTADDR+1   ; plane 0 nametable at $6000
  ; set up plane 0's scroll
  stz BGSCROLLX+0
  stz BGSCROLLX+0
  lda #$FF
  sta BGSCROLLY+0  ; The PPU displays lines 1-224, so set scroll to
  sta BGSCROLLY+0 ; $FF so that the first displayed line is line 0

  lda #0
  sta PPURES
  lda #%00010001  ; enable sprites and plane 0
  sta BLENDMAIN
  lda #VBLANK_NMI|AUTOREAD  ; but disable htime/vtime IRQ
  sta PPUNMI

  jsl ppu_vsync
  jsl load_bg_pal

  seta8
  ; This is where you'd usually update the scroll position.
  ; The scrolling registers are write-twice: first write bits 7-0
  ; then write bits 9-8 to the same address.
  stz BGSCROLLX+0
  stz BGSCROLLX+0
  stz z:framecount

  setaxy16
  ldx #0
  jsl ppu_clear_oam

  jmp emu_start

forever:
  setaxy16
  ldx #0

  ; Backgrounds and OAM can be modified only during vertical blanking.
  ; Wait for vertical blanking and copy prepared data to OAM.
  jsl ppu_vsync

  jsl ppu_copy_oam

  lda z:IS_PAL
  beq :+
  inc z:framecount
  lda z:framecount
  cmp #6
  bne :+
  stz z:framecount

:

  seta8
  lda #$0F
  sta PPUBRIGHT  ; turn on rendering
  setaxy8

  seta8
  ; wait for control reading to finish
  lda #$01
:
  bit VBLSTATUS
  bne :-

  inc tick

  jmp forever
.endproc


.feature c_comments
