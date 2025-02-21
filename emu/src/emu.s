.include "snes.inc"
.include "global.inc"
.feature c_comments

.smart
.export emu_start, VRAM, IO_SPACE, ppu_update, VRAM_4bpp, update_pal
.exportzp do_vblank
.exportzp LCDC_mirror
.import __ZEROPAGE_RUN__

CARTRAM = $7e0000 ; + $a000
VRAM_4bpp = $7f0000
WRAM = $7ec000
VRAM = $7e8000

; NOTE: i can't implement mode 1 and mode 2 bc of SPEED REASONS

.define INCLUDE_HALF_CARRY 0
;.define INCLUDE_STAT_VBLANK 0
.define INCLUDE_ACCURATE_HALT 3
.define INCLUDE_ECHO_RAM 0
.define INCLUDE_JOYPAD_IRQ 0
.define AUDIO_EMULATION 1
.define INCLUDE_LY_LYC_STATUS 0

; LY_WAITLOOP_OPTIMIZATION = 0: 
;   - no waitloop optimizations whatsoever
; LY_WAITLOOP_OPTIMIZATION = 1: 
;   - some basic LY waitloop optimizations (should be faster in most cases)
; LY_WAITLOOP_OPTIMIZATION = 2:
;   - more waitloop optimizations are enabled (faster but less accurate)
; LY_WAITLOOP_OPTIMIZATION = 3 
;   NOTE: !!!!!!!!!!!!!! USE AS A LAST RESORT !!!!!!!!!!!!!!
;   - even more optimizations are enabled through discarding checks
;   - some games and demos WILL hang with this mode
.define LY_WAITLOOP_OPTIMIZATION 2

.define SCROLL_PER_SCANLINE 0
.define INCLUDE_FAKE_TIMER 1

.segment "BSS"
OAM_GB: .res 256
; WY, going_to, $ff
WIN_HDMA: .res 16
.if SCROLL_PER_SCANLINE = 1
SCX_HDMA: .res 144*2
SCY_HDMA: .res 144*2
SCX_temp: .res 144
SCY_temp: .res 144
.endif

.align 128
IO_SPACE: .res 128
.segment "ZEROPAGE"
mem_p: .res 5
mem_w: .res 5
inst: .res 2
opcode: .res 1
inst_executed: .res 2
write_val16: .res 1
write_val: .res 1
temp: .res 4
sp: .res 2
flags: .res 4
jmp_ptr: .res 2
regs: .res 8
int_temp: .res 1
ppu_mode: .res 1
joypad_byte: .res 3
read_prg: .res 8
do_vblank: .res 1
ppu_temp: .res 2
LCDC_mirror: .res 1
pc := $7e
halt_irq: .res 2
apuio_vbl: .res 2
waitloop_read: .res 1
waitloop_cmp: .res 1
waitloop_addr:.res 1
waitloop_pc: .res 2
waitloop_and: .res 1

; 76543210
; 7 = bitwise and
; 6-0: N/A
waitloop_modes: .res 1

IME := $70
REG_A := regs
REG_B := regs+1
REG_C := regs+2
REG_D := regs+3
REG_E := regs+4
REG_H := regs+5
REG_L := regs+6

F_Z := flags
F_N := flags+1
F_H := flags+2
F_C := flags+3

hram := $80

.macro makelabel arg1, arg2
  .ident (.concat (arg1, arg2)):
.endmacro

.macro incw lo, hi
	.local poo
	inc lo
	bne poo
	inc hi
poo:
.endmacro


.macro decw lo, hi
	.local poo
	lda lo
	bne poo
	dec hi
poo:
	dec lo
.endmacro

.segment "CODE1"
PROGRAM:
;.incbin "gb_bootrom.bin"
;.incbin "roms/11-op a,(hl).gb", 256
;.res 32768-256, $ff
;.incbin "roms/Tetris (World) (Rev 1).gb"
;.incbin "roms/daa.gb"
;.incbin "roms/bgbtest.gb"

.segment "CODE"
empty_rom:
.res 1, 0
.byte "EMV1.00"
.byte "GB EMU BY AART1256"
emu_start:
  setaxy8
  lda empty_rom
  beq :+
  jmp @goto_startup
:
  ; here's a SMALL 65c816 program (~160 bytes!)
  ; that tells the user that they didn't import
  ; a ROM to the emulator using the python converter
  ; provided in the github repo as well as play a
  ; HUUGH sound effect (coutersy of roostersox's keyboard)
  ; to warn the user yet again

  ; write tile data to VRAM
  jsl ppu_vsync
  seta8
  stz a:NMITIMEN
  lda #$80
  sta PPUBRIGHT

  lda #(($1000>>10)<<2)|%00
  sta BG1SC
  stz BG12NBA
  stz W12SEL
  lda #1
  sta TM
  stz TMW

  setaxy16
  stz PPUADDR
  ldx #0
:
  lda a:mono_font, x
  and #$00ff
  sta PPUDATA
  inx
  cpx #$0300
  bne :-

  setaxy8
  stz BGMODE
  stz CGADDR
  stz CGDATA
  stz CGDATA
  lda #$ff
  sta CGDATA
  lda #$7f
  sta CGDATA

    setaxy16
    ldx #$1000
    stx PPUADDR
    ldx #0
:
    lda a:no_roms_text, x
    and #$00ff
    sec
    sbc #$20
    sta PPUDATA
    inx
    cpx #no_roms_text_end-no_roms_text
    bne :-
:
    stz PPUDATA
    inx
    cpx #$2000
    bne :-

  setaxy8
  ; send HUUGH to SPC
  lda #$cc
  sta a:APUIO0
:
  cmp a:APUIO0
  bne :-

  lda #$de
  ;sta a:APUIO1 ; not needed for HUUGH
  sta a:APUIO0

  lda #$0f
  sta PPUBRIGHT

  bra *
@goto_startup:

lda #$af
sta mem_p
lda #$00
sta mem_p+1
sta mem_p+2
lda #^PROGRAM
sta mem_p+3
lda #$60
sta mem_p+4

lda #$8f
sta mem_w
lda #$00
sta mem_w+1
sta mem_w+2
lda #$7e
sta mem_w+3
lda #$60
sta mem_w+4

; initialize the DMG registers
lda #1
sta REG_A
sta F_Z
sta F_H
sta F_C

lda #0
sta F_N
sta REG_B
sta REG_D

lda #$13
sta REG_C

lda #$d8
sta REG_E

lda #$01
sta REG_H

lda #$4d
sta REG_L

lda #$fe
sta sp
lda #$ff
sta sp+1

lda #0
sta pc
lda #1
sta pc+1

jsr ppu_init

;lda #0
;ldx #7
;init_reg:
;sta regs, x
;dex
;bpl init_reg

jmp next

no_roms_text:
    .byte "There is no ROM imported!       "
    .byte "Use the included ROM importer to"
    .byte "build the emulator with a ROM!  "
    .byte "Thanks for reading this text :3 "
    .byte "                                "
    .byte "                      - AArt1256"
no_roms_text_end:
    .byte "tf you looking at?! :skull:     "

; $CB prefixed opcodes
opcb:
  jsr read_mem
  bmi do_cb_80_ff
  incw pc, pc+1
  asl
  tax
  jmp (cb_prefix_lut&$ffff, x)

do_cb_80_ff:
  incw pc, pc+1
  asl
  tax
  jmp ((cb_prefix_lut+256)&$ffff, x)

; rlc r8
.repeat 8, I
  OP .set I|(0<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    cmp #$80
    rol a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

  jmp next
.endrepeat

; rrc r8
.repeat 8, I
  OP .set I|(1<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    tax
    ror a
    txa
    ror a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

  jmp next
.endrepeat

; rl r8
.repeat 8, I
  OP .set I|(2<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    tax
    ldy F_C
    cpy #1

    pha
    rol a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    pla
    and #128
    sta F_C

  jmp next
.endrepeat


; rr r8
.repeat 8, I
  OP .set I|(3<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    tax
    ldy F_C
    cpy #1

    pha
    ror a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    pla
    and #1
    sta F_C

  jmp next
.endrepeat

; sla r8
.repeat 8, I
  OP .set I|(4<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    asl a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

  jmp next
.endrepeat

; sra r8
.repeat 8, I
  OP .set I|(5<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif
    stz F_Z

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    cmp #$80
    ror a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000001 ; get "carry" flag
    sta F_C

    txa
    and #%00000010 ; get "carry" flag
    sta F_Z

  jmp next
.endrepeat

; swap r8
.repeat 8, I
  OP .set I|(6<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif
    stz F_C

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    tax
    lda swap_nibble, x
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

  jmp next
.endrepeat


; srl r8
.repeat 8, I
  OP .set I|(7<<3)
  ld_src .set I
  ld_dst .set I
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    .if INCLUDE_HALF_CARRY = 1
        stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    clc
    lsr a
    php

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

  jmp next
.endrepeat


; bit b3, r8
.repeat $40, I
  OP .set I|(%01<<6)
  ld_src .set I&7
  ld_dst .set I&7
  makelabel "cb", .sprintf("%02x",OP)
    stz F_N
    stz F_Z
    .if INCLUDE_HALF_CARRY = 1
        lda #1
        sta F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

      and #1<<((OP>>3)&7)
      bne :+
      inc F_Z
     :

  jmp next
.endrepeat

; res b3, r8
.repeat $40, I
  OP .set I|(%10<<6)
  ld_src .set I&7
  ld_dst .set I&7
  makelabel "cb", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

      and #$ff^(1<<((OP>>3)&7))

      .if ld_dst = 6
        sta write_val
        lda REG_H
        sta mem_w+2
        lda REG_L
        sta mem_w+1
        jsr write_mem
      .elseif ld_dst = 7
        sta regs+0
      .else
        sta regs+(ld_dst+1)
      .endif

  jmp next
.endrepeat

; set b3, r8
.repeat $40, I
  OP .set I|(%11<<6)
  ld_src .set I&7
  ld_dst .set I&7
  makelabel "cb", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

      ora #1<<((OP>>3)&7)

      .if ld_dst = 6
        sta write_val
        lda REG_H
        sta mem_w+2
        lda REG_L
        sta mem_w+1
        jsr write_mem
      .elseif ld_dst = 7
        sta regs+0
      .else
        sta regs+(ld_dst+1)
      .endif

  jmp next
.endrepeat

; UN-IMPLEMENETED/ILLEGAL OPCODE TRAP
cbUNK:
  sei
  txa
  ldx pc
  ldy pc+1
:
  sta $100 ; outputs opcode byte at $100
  stx $103 ; outputs PCL at $103
  sty $102 ; outputs PCH at $102
  inc $101
  jmp :-

.align 256

cb_prefix_lut:
.repeat 256, I
  .ifdef .ident(.concat ("cb", .sprintf("%02x",I)))
    .word .ident(.concat ("cb", .sprintf("%02x",I)))&$ffff
  .else
    .word cbUNK&$ffff
  .endif
.endrepeat

swap_nibble:
    .repeat 16, J
        .repeat 16, I
            .byte (I<<4)|J
        .endrepeat
    .endrepeat

.macro lsr32 addr ; big endian
	clc
	lsr addr
	ror addr+1
	ror addr+2
	ror addr+3
.endmacro

.macro lsr24 addr ; big endian
	clc
	lsr addr
	ror addr+1
	ror addr+2
.endmacro

.macro lsr16 addr ; big endian
	clc
	lsr addr
	ror addr+1
.endmacro

.proc do_hdma
    phb
    php

.if SCROLL_PER_SCANLINE = 1
    lda #0
    sta a:DMAMODE|(1<<4)
    lda #<W12SEL
    sta a:DMAPPUREG|(1<<4)
    seta16
    lda #(WIN_HDMA)&$ffff
    sta a:DMAADDR|(1<<4)
    seta8
    stz a:DMAADDRBANK|(1<<4)

    lda #0
    sta a:DMAMODE|(2<<4)
    lda #<TM
    sta a:DMAPPUREG|(2<<4)
    seta16
    lda #(WIN_HDMA+8)&$ffff
    sta a:DMAADDR|(2<<4)
    seta8
    stz a:DMAADDRBANK|(2<<4)

    lda #$42
    sta a:DMAMODE|(3<<4)
    lda #<BG1HOFS
    sta a:DMAPPUREG|(3<<4)
    seta16
    lda #(scrollx_hdma)&$ffff
    sta a:DMAADDR|(3<<4)
    seta8
    lda #$80
    sta a:DMAADDRBANK|(3<<4)
    stz a:HDMAINDBANK|(3<<4)

    lda #$42
    sta a:DMAMODE|(4<<4)
    lda #<BG1VOFS
    sta a:DMAPPUREG|(4<<4)
    seta16
    lda #(scrolly_hdma)&$ffff
    sta a:DMAADDR|(4<<4)
    seta8
    lda #$80
    sta a:DMAADDRBANK|(4<<4)
    stz a:HDMAINDBANK|(4<<4)

    lda #1<<1|1<<2|1<<3|1<<4
    sta a:HDMASTART

.else
    lda #0
    sta a:DMAMODE|(1<<4)
    lda #<W12SEL
    sta a:DMAPPUREG|(1<<4)
    seta16
    lda #(WIN_HDMA)&$ffff
    sta a:DMAADDR|(1<<4)
    seta8
    stz a:DMAADDRBANK|(1<<4)

    lda #0
    sta a:DMAMODE|(2<<4)
    lda #<TM
    sta a:DMAPPUREG|(2<<4)
    seta16
    lda #(WIN_HDMA+8)&$ffff
    sta a:DMAADDR|(2<<4)
    seta8
    stz a:DMAADDRBANK|(2<<4)

    lda #1<<1|1<<2
    sta a:HDMASTART
.endif

    plp
    plb
    rtl
.endproc

.if SCROLL_PER_SCANLINE = 1
scrollx_hdma:
    .byte $80 | 72
    .word SCX_HDMA
    .byte $80 | 72
    .word SCX_HDMA+72*2
    .byte 0
scrolly_hdma:
    .byte $80 | 72
    .word SCY_HDMA
    .byte $80 | 72
    .word SCY_HDMA+72*2
    .byte 0
.endif

; 00 00 00 00 00 00 00 00
ppu_update:
    php
    setaxy8
    ldx #7
:
    stz a:WIN_HDMA, x
    stz a:WIN_HDMA+8, x
    dex
    bpl :-

.if SCROLL_PER_SCANLINE = 1
    rep #$30
    lda #SCX_HDMA
    tad              ; temporarily move direct page to SCX temporary area
    setaxy8
    .repeat 72, I
        lda a:SCX_temp+I
        sta z:I*2
    .endrepeat
    rep #$30
    lda #SCX_HDMA+72*2
    tad              ; temporarily move direct page to SCX temporary area
    setaxy8
    .repeat 72, I
        lda a:SCX_temp+I+72
        sta z:I*2
    .endrepeat


    rep #$30
    lda #SCY_HDMA
    tad              ; temporarily move direct page to SCX temporary area
    setaxy8
    .repeat 72, I
        lda a:SCY_temp+I
        sta z:I*2
    .endrepeat
    rep #$30
    lda #SCY_HDMA+72*2
    tad              ; temporarily move direct page to SCX temporary area
    setaxy8
    .repeat 72, I
        lda a:SCY_temp+I+72
        sta z:I*2
    .endrepeat

    rep #$20
    lda #__ZEROPAGE_RUN__ & $FF00
    tad                 ; return direct page to real zero page
    setaxy8
.else
    lda a:IO_SPACE+$43
    sta BG1HOFS
    stz BG1HOFS

    lda a:IO_SPACE+$42
    sta BG1VOFS
    stz BG1VOFS
.endif

    lda a:IO_SPACE+$4b
    sec
    sbc #7
    sta WH0
    lda #160
    sta WH1

    lda a:IO_SPACE+$4b
    sta ppu_temp+0
    stz ppu_temp+1
    seta16
    lda #%1111111111+8
    sec
    sbc ppu_temp
    sta ppu_temp
    seta8
    lda ppu_temp
    sta BG2HOFS
    lda ppu_temp+1
    sta BG2HOFS

    lda a:IO_SPACE+$4a
    sta ppu_temp+0
    stz ppu_temp+1
    seta16
    lda #%1111111111
    sec
    sbc ppu_temp
    sta ppu_temp
    seta8
    lda ppu_temp
    sta BG2VOFS
    lda ppu_temp+1
    sta BG2VOFS

    ldy #0
    ldx #0
    lda a:IO_SPACE+$40
    and #1<<7
    beq @skip_LCDC
    txa
    ora #1<<0
    tax

    lda a:IO_SPACE+$40
    and #1<<5
    beq :+
    ldy #1<<1|1<<0
    lda a:IO_SPACE+$4a
    bne :+
    txa
    ora #1<<1
    tax
:
    lda a:IO_SPACE+$40
    and #1<<1
    beq :+
    txa
    ora #1<<4
    tax
:
@skip_LCDC:
    stx TM
    sty TMW

    ;lda #%00010001
    stx WIN_HDMA+1+8 ; TM_HDMA

    lda a:IO_SPACE+$40
    and #1<<5
    beq :+
    txa
    ora #1<<1
    tax
:
    stx WIN_HDMA+3+8
    stx WIN_HDMA+5+8

    lda #1
    sta WIN_HDMA+2
    sta WIN_HDMA+2+8
    sta WIN_HDMA+4
    sta WIN_HDMA+4+8

    ldx #0
    lda a:IO_SPACE+$40; LCDC
    and #1<<5
    beq :+
    ldx #%00110010
:
    stx WIN_HDMA+3 ;W12SEL_going_to
    stx WIN_HDMA+5 ;W12SEL_going_to

    lda a:IO_SPACE+$4a
    sta WIN_HDMA
    sta WIN_HDMA+8

    lda a:IO_SPACE+$4a
    cmp #128
    bcc :+
    sec
    sbc #127
    sta WIN_HDMA+2
    sta WIN_HDMA+2+8
    stz WIN_HDMA+3
    lda #127
    sta WIN_HDMA
    sta WIN_HDMA+8
    lda #$11
    sta WIN_HDMA+3+8
:

    lda a:IO_SPACE+$4a
    bne :+
    lda #1
    sta WIN_HDMA
    lda WIN_HDMA+5
    sta WIN_HDMA+1
    stz WIN_HDMA+2
    lda #1
    sta WIN_HDMA+8
    lda WIN_HDMA+5+8
    sta WIN_HDMA+1+8
    stz WIN_HDMA+2+8
:

    lda a:IO_SPACE+$40
    and #1<<5
    bne :+
    lda #8
    sta WIN_HDMA
    stz WIN_HDMA+1
    stz WIN_HDMA+2
    sta WIN_HDMA+8
    lda #%00010001
    sta WIN_HDMA+1+8
    stz WIN_HDMA+2+8
:

    lda JOY1CUR_HI
    sta z:joypad_byte+1

    jsr update_OAM

    plp
    rtl

write_pal:
    .repeat 4, I
      .if I <> 0
        pla
      .endif
      .if I <> 0
        lsr
        lsr
      .endif
      .if I <> 3
        pha
      .endif
      and #3
      asl
      tax
      lda a:gb_pal, x
      sta a:CGDATA
      inx
      lda a:gb_pal, x
      sta a:CGDATA
    .endrepeat
    rts

write_pal_obj:
    sty a:CGADDR
    .repeat 4, I
      .if I <> 0
        tya
        ora #(I&1)|((I>>1)<<2)
        sta a:CGADDR
        pla
      .endif
      .if I <> 0
        lsr
        lsr
      .endif
      .if I <> 3
        pha
      .endif
      and #3
      asl
      tax
      lda a:gb_pal, x
      sta a:CGDATA
      inx
      lda a:gb_pal, x
      sta a:CGDATA
    .endrepeat
    rts

update_pal:
    ; FF47: BGP
    setaxy8
    stz a:CGADDR
    setaxy8
    lda a:IO_SPACE+$47 ; BGP
    jsr write_pal

    setaxy8
    lda #$20
    sta a:CGADDR
    setaxy8
    lda a:IO_SPACE+$47 ; BGP
    jsr write_pal


    ; FF48: OBP0
    setaxy8
    ldy #$80
    lda a:IO_SPACE+$48 ; OBP0
    jsr write_pal_obj

    ; FF49: OBP1
    setaxy8
    ldy #$90
    lda a:IO_SPACE+$49 ; OBP1
    jsr write_pal_obj

    rtl

gb_pal:
  .word RGB($f8>>3,$e8>>3,$a8>>3), $1a5a, $3175, $2ca8

.align 256
OAM_LUT:
.repeat 256, I
    Xflip .set ((I>>5)&1)
    Yflip .set ((I>>6)&1)
    Spri .set ((I>>7)&1)
    OGBP .set ((I>>4)&1)
    Spri_SNES .set 0
    .if Spri = 0
        Spri_SNES .set 3<<4
    .endif
    .byte (Xflip<<6)|(Yflip<<7)|(Spri_SNES)|(OGBP<<1)
.endrepeat

update_OAM:
    lda a:IO_SPACE+$40
    and #1<<2
    beq :+
    jmp update_OAM_8x16
:
    .repeat 40, I
        lda a:OAM_GB+4*I+0
        sec
        sbc #17
        sta a:OAM+4*I+1
        lda a:OAM_GB+4*I+1
        sec
        sbc #8
        sta a:OAM+4*I+0
        lda a:OAM_GB+4*I+2
        sta a:OAM+4*I+2
        ldx a:OAM_GB+4*I+3
        lda a:OAM_LUT, x
        sta a:OAM+4*I+3
    .endrepeat

    setaxy16
    ldx #0
    lda #$e180
:
    sta a:OAM+(40*4), x
    inx
    inx
    inx
    inx
    cpx #40*4
    bne :-
    setaxy8

    rts

update_OAM_8x16:
    .repeat 40, I
        lda a:OAM_GB+4*I+0
        sec
        sbc #17
        sta a:OAM+4*I+1
        clc
        adc #8
        sta a:OAM+4*I+1+(40*4)

        lda a:OAM_GB+4*I+1
        sec
        sbc #8
        sta a:OAM+4*I+0
        sta a:OAM+4*I+0+(40*4)

        lda a:OAM_GB+4*I+2
        and #$fe
        sta a:OAM+4*I+2
        inc a
        sta a:OAM+4*I+2+(40*4)

        ldx a:OAM_GB+4*I+3
        lda a:OAM_LUT, x
        sta a:OAM+4*I+3
        sta a:OAM+4*I+3+(40*4)
    .endrepeat
    rts

; A: joypad byte (at $FF00)
do_joypad:
    ldy z:joypad_byte+1
    lsr
    lsr
    lsr
    and #3<<1
    tax
    ;jmp (joypad_cases, x)
    .byte $7c, <joypad_cases, >joypad_cases
end_joypad:
    sta a:IO_SPACE+$00 ; FF00: joypad
    and #$0f
    eor #$0f
    sta z:joypad_byte+0 ; for joypad IRQ
    rts

joypad_cases:
  .word joypad_case0&$ffff, joypad_case1&$ffff, joypad_case2&$ffff, joypad_case3&$ffff

joypad_dpad_lut:
  .repeat 256, I
    joypad_SR .set (((I>>4)&1)<<3)|(((I>>5)&1)<<2)
    joypad_BA .set (((I>>6)&1)<<1)|(((I>>7)&1)<<0)
    .byte (joypad_SR|joypad_BA)
  .endrepeat


joypad_button_lut:
  .repeat 256, I
    joypad_U .set (((I>>3)&1)<<2)
    joypad_D .set (((I>>2)&1)<<3)
    joypad_LR .set (I&3) ; yay :3
    .byte (joypad_U|joypad_D|joypad_LR)
  .endrepeat


joypad_case0:
    lda a:joypad_button_lut, y
    ora a:joypad_dpad_lut, y
    eor #$0f
    ora #0<<4
    jmp end_joypad

joypad_case1:
    lda a:joypad_dpad_lut, y
    eor #$0f
    ora #1<<4
    jmp end_joypad

joypad_case2:
    lda a:joypad_button_lut, y
    eor #$0f
    ora #0<<4
    jmp end_joypad

joypad_case3:
    lda #$3f
    jmp end_joypad


ppu_init:
    setaxy8
    lda #$bf
    sta z:read_prg
    stz z:read_prg+1
    lda #$80
    sta z:read_prg+2
    lda #$88
    sta z:read_prg+3
    lda #$e2
    sta z:read_prg+4
    lda #$10
    sta z:read_prg+5
    lda #$60
    sta z:read_prg+6

    stz z:LCDC_mirror
    stz z:halt_irq

    setxy16
    ldx #0
    lda #0
:
    sta f:VRAM, x
    inx
    cpx #$2000
    bne :-

    /*
    ldx #0
    lda #0
:
    sta f:VRAM_4bpp, x
    inx
    cpx #$4000
    bne :-
    */
    setxy8

    lda #256-114
    sta z:inst_executed
    stz a:IO_SPACE+$44

    jsl ppu_vsync

    lda #$80
    sta PPUBRIGHT

    lda #(($1000>>10)<<2)|%00
    sta BG1SC
    lda #(($1400>>10)<<2)|%00
    sta BG2SC
    stz BG12NBA
    stz W12SEL
    stz W34SEL

    setaxy16
    ldx #$0fff
    stx PPUADDR
    ldx #0
:
    stz PPUDATA
    inx
    cpx #$1000
    bne :-


    setaxy16
    ldx #$1400
    stx PPUADDR
    lda #1<<13
    ldx #0
:
    sta PPUDATA
    inx
    cpx #$400
    bne :-

    setaxy16
    ldx #$3fff
    stx PPUADDR
    ldx #0
:
    stz PPUDATA
    inx
    cpx #$1000
    bne :-

    ldx #0
    jsl ppu_clear_oam

    jsl ppu_vsync
    jsl ppu_copy_oam

    setaxy8

    lda #$0f
    sta PPUBRIGHT
    stz z:IME
    stz z:ppu_mode
    stz z:write_val16
    lda #$4000 >> 13
    sta a:OBSEL
    stz z:do_vblank

    seta16
    lda #160
    sta a:VTIMEL
    stz a:VTIMEH
    seta8
    lda #%10100001
    sta a:NMITIMEN
    cli
    rts

LY_lut:
    .repeat 160, I
        .byte (I+1) .mod 154
    .endrepeat

invoke_irq_waitloop:
  inc z:halt_irq
  pha
  lda z:IME
  bne :+
  pla
  bra waitloop_jmp
:

  setaxy16
  dec sp
  lda sp
  sta mem_w+1
  setaxy8

  lda pc+1
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+0
  sta write_val
  jsr write_mem

  pla
  sta pc
  stz pc+1
  jmp next


waitloop:
  lda z:waitloop_modes
  and #$80
  bne :+
  lda #$ff
  sta z:waitloop_and
:

waitloop_jmp:
  lda #256-114
  sta z:inst_executed

  inc a:IO_SPACE+$44
  ldy a:IO_SPACE+$44 ; i loaded LY into Y for retrieving it later
  cpy #154
  bne @skip_ly
  stz a:IO_SPACE+$44
  lda #0
  sta z:ppu_mode
@skip_ly:

  .if SCROLL_PER_SCANLINE = 1
  cpy #145
  bcs @skip_LY_after_vbl
  lda a:IO_SPACE+$43
  sta a:SCX_temp-1, y
  lda a:IO_SPACE+$42
  sta a:SCY_temp-1, y
@skip_LY_after_vbl:
  .endif

  cpy #144
  bne @skip_LY_144

  lda #1
  sta z:ppu_mode
  sta z:do_vblank
  .if AUDIO_EMULATION = 1
  inc z:apuio_vbl
  lda z:apuio_vbl
  sta a:APUIO3
  .endif

  lda a:IO_SPACE+$40
  bpl @skip_vblank
  lda z:hram+$7f
  and #1
  beq @skip_vblank
  tsb a:IO_SPACE+$0f
  lda #$40
  jmp invoke_irq_waitloop
@skip_vblank:

  ;.if INCLUDE_STAT_VBLANK = 1
  ;lda a:IO_SPACE+$41
  ;and #1<<4
  ;beq skip_LY_144
  ;lda z:hram+$7f
  ;and #1<<1
  ;beq skip_LY_144
  ;tsb a:IO_SPACE+$0f
  ;lda #$48
  ;bra invoke_irq_waitloop
  ;.endif

@skip_LY_144:

.if INCLUDE_FAKE_TIMER = 1
  cpy #72
  bne @skip_fake_timer
  lda z:hram+$7f
  and #1<<2
  beq @skip_fake_timer
  tsb a:IO_SPACE+$0f
  lda #$50
  jmp invoke_irq_waitloop
@skip_fake_timer:
.endif

  lda z:hram+$7f
  and #1<<1
  beq @skip_STAT_int

  cpy #144
  bcs @skip_per_line_STAT

  lda a:IO_SPACE+$41
  and #1<<3|1<<5
  beq @skip_per_line_STAT
  lda #1<<1
  tsb a:IO_SPACE+$0f
  lda #$48
  jmp invoke_irq_waitloop
@skip_per_line_STAT:

  lda a:IO_SPACE+$41
  and #1<<6
  beq @skip_LY_LYC_STAT

  ; LY == LYC
  cpy a:IO_SPACE+$45
  bne @skip_LY_LYC_STAT

  lda #1<<1
  tsb a:IO_SPACE+$0f

  lda #$48
  jmp invoke_irq_waitloop
@skip_LY_LYC_STAT:

@skip_STAT_int:

  .if INCLUDE_JOYPAD_IRQ = 1
  lda z:joypad_byte+0
  beq @skip_JOYPAD_IRQ
  lda z:hram+$7f
  and #1<<4
  beq @skip_JOYPAD_IRQ
  tsb a:IO_SPACE+$0f
  lda #$60
  bra invoke_irq_waitloop
@skip_JOYPAD_IRQ:
  .endif

  lda z:waitloop_addr
  sta mem_p+1
  jsr read_HRAM
  and z:waitloop_and
  cmp z:waitloop_cmp
  bne :+
  jmp execute_opcode
:
  jmp waitloop_jmp

invoke_irq:
  inc z:halt_irq
  pha
  lda z:IME
  bne :+
  pla
  bra next
:

  setaxy16
  dec sp
  lda sp
  sta mem_w+1
  setaxy8

  lda pc+1
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+0
  sta write_val
  jsr write_mem

  pla
  sta pc
  stz pc+1
  ;jmp next

next:
  lda z:inst_executed
  bpl :+
  jmp :++
:
  sec
  sbc #114
  sta z:inst_executed

advance_ly:
  inc a:IO_SPACE+$44
advance_ly_no_inc:
  ldy a:IO_SPACE+$44 ; i loaded LY into Y for retrieving it later
  cpy #154
  bne @skip_ly
  stz a:IO_SPACE+$44
  lda #0
  sta z:ppu_mode
@skip_ly:

  .if SCROLL_PER_SCANLINE = 1
  cpy #145
  bcs skip_LY_after_vbl
  lda a:IO_SPACE+$43
  sta a:SCX_temp-1, y
  lda a:IO_SPACE+$42
  sta a:SCY_temp-1, y
skip_LY_after_vbl:
  .endif

  .if 0 ;.if AUDIO_EMULATION = 1
  cpy #144>>1
  bne skip_LY_halfway
  inc z:apuio_vbl
  lda z:apuio_vbl
  sta a:APUIO3
skip_LY_halfway:
  .endif

  cpy #144
  bne skip_LY_144

  lda #1
  sta z:ppu_mode
  sta z:do_vblank
  .if AUDIO_EMULATION = 1
  inc z:apuio_vbl
  lda z:apuio_vbl
  sta a:APUIO3
  .endif

  lda a:IO_SPACE+$40
  bpl skip_vblank
  lda z:hram+$7f
  and #1
  beq skip_vblank
  tsb a:IO_SPACE+$0f
  lda #$40
  jmp invoke_irq
skip_vblank:

  ;.if INCLUDE_STAT_VBLANK = 1
  ;lda a:IO_SPACE+$41
  ;and #1<<4
  ;beq skip_LY_144
  ;lda z:hram+$7f
  ;and #1<<1
  ;beq skip_LY_144
  ;tsb a:IO_SPACE+$0f
  ;lda #$48
  ;bra invoke_irq
  ;.endif

skip_LY_144:

.if INCLUDE_FAKE_TIMER = 1
  cpy #72
  bne skip_fake_timer
  lda z:hram+$7f
  and #1<<2
  beq skip_fake_timer
  tsb a:IO_SPACE+$0f
  lda #$50
  jmp invoke_irq
skip_fake_timer:
.endif

  lda z:hram+$7f
  and #1<<1
  beq skip_STAT_int

  cpy #144
  bcs skip_per_line_STAT

  lda a:IO_SPACE+$41
  and #1<<3|1<<5
  beq skip_per_line_STAT
  lda #1<<1
  tsb a:IO_SPACE+$0f
  lda #$48
  jmp invoke_irq
skip_per_line_STAT:

  lda a:IO_SPACE+$41
  and #1<<6
  beq skip_LY_LYC_STAT

  ; LY == LYC
  cpy a:IO_SPACE+$45
  bne skip_LY_LYC_STAT

  lda #1<<1
  tsb a:IO_SPACE+$0f

  lda #$48
  jmp invoke_irq
skip_LY_LYC_STAT:

skip_STAT_int:

  .if INCLUDE_JOYPAD_IRQ = 1
  lda z:joypad_byte+0
  beq skip_JOYPAD_IRQ
  lda z:hram+$7f
  and #1<<4
  beq skip_JOYPAD_IRQ
  tsb a:IO_SPACE+$0f
  lda #$60
  jmp invoke_irq
skip_JOYPAD_IRQ:
  .endif
:

execute_opcode:
  jsr read_mem
  bmi do_inst_80_ff
  incw pc, pc+1
  asl
  tax
  lda z:inst_executed
  clc
  adc a:cycle_lut, x
  sta z:inst_executed
  jmp (inst_lut&$ffff, x)

do_inst_80_ff:
  incw pc, pc+1
  asl
  tax
  lda z:inst_executed
  clc
  adc a:cycle_lut+256, x
  sta z:inst_executed
  jmp ((inst_lut+256)&$ffff, x)


read_pc_byte:
	jsr read_mem
	sta inst
    tax
	incw pc, pc+1
	rts

read_mem:
  ;lda #0
  bit pc+1
  bmi @skip_rom
  setxy16
  ldx pc
  jmp read_prg
  .i8
@skip_rom:

  lda pc+1
  cmp #$ff
  bne @skip_IO

  bit pc
  bmi read_HRAM_2

@skip_IO:
  cmp #$fe
  bne :+
  rts
:

.if INCLUDE_ECHO_RAM = 1
  ;lda mem_p+2
  cmp #$a0
  bcs @skip_VRAM

  ; read from VRAM
  setaxy16
  lda pc+0
  and #$1fff
  tax
  seta8
  lda VRAM, x
  setxy8
  rts
@skip_VRAM:
  cmp #$c0
  bcc do_CARTRAM_2

  setaxy16
  lda pc+0
  and #$1fff
  tax
  seta8
  lda WRAM, x
  setxy8
  rts

read_HRAM_2:
  bit pc
  bmi @skip_HRAM

  ldx pc
  lda a:IO_SPACE, x
  rts
@skip_HRAM:
  ldx pc
  lda z:$00, x
  rts

do_CARTRAM_2:
  seta8
  setxy16
  ldx pc
  lda CARTRAM, x
  setxy8
  rts
.else
  ;seta8
  setxy16
  ldx pc
  lda CARTRAM, x
  setxy8
  rts

read_HRAM_2:
  bit pc
  bmi @skip_HRAM

  ldx pc
  lda a:IO_SPACE, x
  rts
@skip_HRAM:
  ldx pc
  lda z:$00, x
  rts
.endif

read_mem_no_pc:
  lda #0
  bit mem_p+2
  bmi @skip_rom
  setxy16
  ldx mem_p+1
  jmp read_prg
  .i8
@skip_rom:

  lda mem_p+2
  cmp #$ff
  bne @skip_IO

@read_HRAM:
  bit mem_p+1
  bmi @skip_HRAM

  ldx mem_p+1
  cpx #$41
  bne @skip_STAT
  lda a:IO_SPACE+$41
  and #$ff^$07
  ora z:ppu_mode
  .if INCLUDE_LY_LYC_STATUS = 1
  ldx a:IO_SPACE+$44
  cpx a:IO_SPACE+$45
  bne :+
  ora #1<<2
:
  .endif
  rts
@skip_STAT:
  .if LY_WAITLOOP_OPTIMIZATION >= 2
  ;stz z:waitloop_read
  ;cpx #$44
  ;bne :+
  stz z:waitloop_modes
  lda #1
  sta z:waitloop_read
  stx z:waitloop_addr
  .if LY_WAITLOOP_OPTIMIZATION = 2
    lda z:pc+0
    sta z:waitloop_pc
    lda z:pc+1
    sta z:waitloop_pc+1
  .endif
;:
  .elseif LY_WAITLOOP_OPTIMIZATION = 1
  stz z:waitloop_read
  cpx #$44
  bne :+
  inc z:waitloop_read
:
  .endif
  lda a:IO_SPACE, x
  rts
@skip_HRAM:
  .if LY_WAITLOOP_OPTIMIZATION >= 2
  ;stz z:waitloop_read
  ;cpx #$44
  ;bne :+
  stz z:waitloop_modes
  lda #1
  sta z:waitloop_read
  stx z:waitloop_addr
  lda z:pc+0
  sta z:waitloop_pc
  lda z:pc+1
  sta z:waitloop_pc+1
  .endif
  ldx mem_p+1
  lda z:$00, x
  rts

@skip_IO:
  cmp #$fe
  bne :+
  rts
:

.if INCLUDE_ECHO_RAM = 1
  ;lda mem_p+2
  cmp #$a0
  bcs @skip_VRAM

  ; read from VRAM
  setaxy16
  lda mem_p+1
  and #$1fff
  tax
  seta8
  lda VRAM, x
  setxy8
  rts

@skip_VRAM:
  cmp #$c0
  bcc @do_CARTRAM

  setaxy16
  lda mem_p+1
  and #$1fff
  tax
  seta8
  lda WRAM, x
  setxy8
  rts

@do_CARTRAM:
  seta8
  setxy16
  ldx mem_p+1
  lda CARTRAM, x
  setxy8
  rts
.else
  ;seta8
  setxy16
  ldx mem_p+1
  lda CARTRAM, x
  setxy8
  rts
.endif

; workaround to local labels
read_HRAM = @read_HRAM


write_mem:
  bit mem_w+2
  bmi @skip_rom

  lda mem_w+2
  lsr
  lsr
  lsr
  lsr
  lsr
  cmp #1
  bne @skip_MBC_bank

  lda z:write_val
  bne :+
  inc a
:
  and #63
  dec a
  clc
  adc #$88
  sta z:read_prg+3

@skip_MBC_bank:

  rts

@skip_rom:
  lda mem_w+2
  cmp #$ff
  bne @skip_IO

@write_HRAM:
  ldx mem_w+1
  bmi @skip_HRAM

  ;ldx mem_w+1
  bne :+
  lda write_val
  jmp do_joypad
:
  cpx #$46
  bne :+
  lda write_val
  sta z:mem_p+2
  stz z:mem_p+1
  jmp OAM_DMA
:

  .if AUDIO_EMULATION = 1
  lda a:is_apu_reg, x
  bne :+
  ; non-apu regs
  lda write_val
  sta a:IO_SPACE, x
  rts
:

  ; apu regs (write register to SPC)

  lda #$cc
  sta a:APUIO0
:
  cmp a:APUIO0
  bne :-

  lda write_val
  sta a:IO_SPACE, x
  sta a:APUIO1
  stx a:APUIO0

:
  cpx a:APUIO0
  bne :-
  .else
  lda write_val
  sta a:IO_SPACE, x
  .endif
  rts

@skip_HRAM:
  ;ldx mem_w+1
  lda write_val
  sta $00, x
  rts

@skip_IO:
  cmp #$fe
  bne :+
  ldx z:mem_p+1
  lda z:write_val
  sta a:OAM_GB, x
  rts
:

.if INCLUDE_ECHO_RAM = 1
  ;lda mem_p+2
  cmp #$a0
  bcs @skip_VRAM

  ; write to VRAM
  setaxy16
  lda mem_w+1
  and #$1fff
  tax
  asl
  pha
  seta8
  lda write_val
  sta VRAM, x
  plx
  seta16
  lda f:VRAM_tile_table, x
  tax
  seta8
  lda write_val
  sta VRAM_4bpp, x
  setxy8
  rts
@skip_VRAM:
  cmp #$c0
  bcc @do_CARTRAM

  setaxy16
  lda mem_w+1
  and #$1fff
  tax
  seta8
  lda write_val
  sta WRAM, x
  setxy8
  rts

@do_CARTRAM:
  seta8
  setxy16
  ldx mem_w+1
  lda write_val
  sta CARTRAM, x
  setxy8
  rts
.else
  cmp #$a0
  bcc @skip_VRAM

  ;seta8
  setxy16
  ldx mem_w+1
  lda write_val
  sta CARTRAM, x
  setxy8
  rts

@skip_VRAM:
  setaxy16
  lda mem_w+1
  tax
  seta8
  lda write_val
  sta CARTRAM, x
  setxy8
  lda a:VRAM_tile_table, x
  setxy16
  tax
  lda write_val
  sta f:VRAM_4bpp, x
  setxy8
  rts

.endif
; workaround to local labels
write_HRAM = @write_HRAM

OAM_DMA:
    ldy #0
    .repeat 40*4, I
        jsr read_mem_no_pc
        sta a:OAM_GB, y
        iny
        inc z:mem_p+1
    .endrepeat
    rts

; =========== THE INSTRUCTIONS ===========

; NOP
op00:
  jmp next

; UN-IMPLEMENETED/ILLEGAL OPCODE TRAP
opUNK:
  sei
  txa
  ldx pc
  ldy pc+1
:
  sta $100 ; outputs opcode byte at $100
  stx $103 ; outputs PCL at $103
  sty $102 ; outputs PCH at $102
  jmp :-

; LD r16, u16
op01:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx REG_B
  pla
  sta REG_C
  jmp next

op11:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx REG_D
  pla
  sta REG_E
  jmp next

op21:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx REG_H
  pla
  sta REG_L
  jmp next

op31:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx sp+1
  pla
  sta sp
  jmp next

; JP imm16
opc3:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx pc+1
  pla
  sta pc
  jmp next

; ld r8, r8
.repeat $40, I
  OP .set I+$40
  ld_src .set I&7
  ld_dst .set (I>>3)&7
  makelabel "op", .sprintf("%02x",OP)
    .if OP = $76
      ; halt
      .if INCLUDE_ACCURATE_HALT = 3
        ldx #200
        lda a:IO_SPACE+$40
        bpl :+
        lda z:hram+$7f
        and #1
        beq :+
        ldx #144
:

        lda a:hram+$7f
        and #1<<1
        bpl @skip_stat

        lda a:IO_SPACE+$41
        and #1<<3|1<<5
        beq @skip_STAT_blank
        lda a:IO_SPACE+$44
        cmp #144
        bcs @skip_STAT_blank
        jmp advance_ly_no_inc
@skip_STAT_blank:

        lda a:IO_SPACE+$41
        and #1<<6
        beq @skip_LYC_comp
        cpx a:IO_SPACE+$45
        bcs @skip_LYC_comp
        lda a:IO_SPACE+$45
        dec a
        sta a:IO_SPACE+$44
        lda #256-114
        sta z:inst_executed
        jmp advance_ly
@skip_LYC_comp:

@skip_stat:

    .if INCLUDE_FAKE_TIMER = 1
        lda z:hram+$7f
        and #1<<2
        beq @skip_tim
        lda #256-114
        sta z:inst_executed
        jmp advance_ly
@skip_tim:
    .endif

        lda a:IO_SPACE+$40
        bpl @skip_vbl
        lda z:hram+$7f
        and #1
        beq @skip_vbl
        lda #256-114
        sta z:inst_executed
        lda #144-1
        sta a:IO_SPACE+$44
        jmp advance_ly
@skip_vbl:

        lda z:halt_irq
        bne :+
        decw pc, pc+1
        lda #256-114
        sta z:inst_executed
        jmp advance_ly
      :
        stz z:halt_irq
    .elseif INCLUDE_ACCURATE_HALT = 2
        lda z:halt_irq
        bne :+
        decw pc, pc+1
        lda #256-114
        sta z:inst_executed
        jmp advance_ly
      :
        stz z:halt_irq
      .elseif INCLUDE_ACCURATE_HALT = 1
        lda z:halt_irq
        bne :+
        decw pc, pc+1
        jmp next
      :
        stz z:halt_irq
      .endif
    .elseif ld_src = ld_dst
    .else
      .if ld_src = 6
        lda REG_H
        sta mem_p+2
        lda REG_L
        sta mem_p+1
        jsr read_mem_no_pc
      .elseif ld_src = 7
        lda regs+0
      .else
        lda regs+(ld_src+1)
      .endif

      .if ld_dst = 6
        sta write_val
        lda REG_H
        sta mem_w+2
        lda REG_L
        sta mem_w+1
        jsr write_mem
      .elseif ld_dst = 7
        sta regs+0
      .else
        sta regs+(ld_dst+1)
      .endif
    .endif
  jmp next
.endrepeat

; ld r8, u8
.repeat 8, I
  OP .set (I<<3)|%110
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    jsr read_pc_byte

    .if ld_dst = 6
      stx write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      stx regs+0
    .else
      stx regs+(ld_dst+1)
    .endif
    jmp next
.endrepeat

; ld a, [hli]
op2a:
  lda REG_H
  sta mem_p+2
  lda REG_L
  sta mem_p+1
  incw REG_L, REG_H
  jsr read_mem_no_pc
  sta REG_A
  jmp next

; inc r8
.repeat 8, I
  OP .set (I<<3)|%100
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    clc
    adc #1
    php
    .if INCLUDE_HALF_CARRY = 1
        pha
    .endif

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    .if INCLUDE_HALF_CARRY = 1
        pla
        and #$0f
        tax
        lda cmp_half_carry, x
        sta F_H
    .endif

    pla ; get flag from php
    and #%00000010 ; get "zero" flag
    sta F_Z
    stz F_N
  jmp next
.endrepeat

; dec r8
.repeat 8, I
  OP .set (I<<3)|%101
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    sec
    sbc #1
    php
    .if INCLUDE_HALF_CARRY = 1
        pha
    .endif

    .if ld_dst = 6
      sta write_val
      lda REG_H
      sta mem_w+2
      lda REG_L
      sta mem_w+1
      jsr write_mem
    .elseif ld_dst = 7
      sta regs+0
    .else
      sta regs+(ld_dst+1)
    .endif

    .if INCLUDE_HALF_CARRY = 1
        pla
        and #$0f
        tax
        lda cmp_half_carry_inv, x
        sta F_H
    .endif

    pla ; get flag from php
    and #%00000010 ; get "zero" flag
    sta F_Z
    lda #$ff
    sta F_N
  jmp next
.endrepeat

; ld [bc], a
op02:
  lda REG_A
  sta write_val
  lda REG_B
  sta mem_w+2
  lda REG_C
  sta mem_w+1
  jsr write_mem
  jmp next

; ld [de], a
op12:
  lda REG_A
  sta write_val
  lda REG_D
  sta mem_w+2
  lda REG_E
  sta mem_w+1
  jsr write_mem
  jmp next

.macro halt_check
        lda a:hram+$7f
        and #1<<1
        bpl @skip_stat

        lda a:IO_SPACE+$41
        and #1<<3|1<<5
        beq @skip_STAT_blank
        jmp advance_ly
@skip_STAT_blank:

        lda a:IO_SPACE+$41
        and #1<<6
        beq @skip_LYC_comp
        cpy a:IO_SPACE+$45
        bcc @skip_LYC_comp
        lda a:IO_SPACE+$45
        dec a
        sta a:IO_SPACE+$44
        jmp advance_ly
@skip_LYC_comp:

        jmp advance_ly
@skip_stat:

        lda a:IO_SPACE+$40
        bpl @skip_vbl
        lda z:hram+$7f
        and #1
        beq @skip_vbl
        cpy #144
        bcs @skip_vbl
        lda #144-1
        sta a:IO_SPACE+$44
        jmp advance_ly
@skip_vbl:
.endmacro

; jr cond, imm8
.repeat 4, I
  OP .set (I<<3)|(1<<5)
  makelabel "op", .sprintf("%02x",OP)
    jsr read_pc_byte

    .if I > 1
      lda F_C
    .else
      lda F_Z
    .endif
    .if (I&1) = 0
      bne :+
    .else
      beq :+
    .endif
      txa
      ; http://forum.6502.org/viewtopic.php?p=75716
      sta temp
      lda #$7f
      cmp temp
      sbc #$7f
      sta temp+1

      txa
      clc
      adc pc
      sta pc
      lda temp+1
      adc pc+1
      sta pc+1
        
    .if LY_WAITLOOP_OPTIMIZATION >= 2
      lda z:waitloop_read
      cmp #2
      bne @s
      .if LY_WAITLOOP_OPTIMIZATION = 2
      lda pc
      cmp waitloop_pc
      bne @s
      lda pc+1
      cmp waitloop_pc+1
      bne @s
      .endif
      stz z:waitloop_read
      setaxy8
      lda #256-114
      sta z:inst_executed
      jmp waitloop
    @s:
    .elseif LY_WAITLOOP_OPTIMIZATION = 1
      lda z:waitloop_read
      cmp #2
      bne @s
      lda #256-114
      sta z:inst_executed
      stz z:waitloop_read
      jmp advance_ly
    @s:
    .endif

    :
    jmp next
.endrepeat

; di
opf3:
  stz z:IME
  jmp next

; ei
opfb:
  lda #$ff
  sta z:IME
  jmp next

; reti
opd9:
  lda #$ff
  sta z:IME

  jmp opc9

; ld [imm16], a
opea:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx mem_w+2
  pla
  sta mem_w+1

  lda REG_A
  sta write_val
  jsr write_mem
  jmp next

; ldh [imm8], a
ope0:
  jsr read_pc_byte
  sta mem_w+1

  lda REG_A
  sta write_val

  jsr write_HRAM
  jmp next

; call imm16
opcd:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+0
  clc
  adc #2
  lda pc+1
  adc #0
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+0
  clc
  adc #2
  sta write_val
  jsr write_mem

  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx pc+1
  pla
  sta pc

  jmp next

; jr i8
op18:
  jsr read_pc_byte

  ; http://forum.6502.org/viewtopic.php?p=75716
  sta temp
  lda #$7f
  cmp temp
  sbc #$7f
  sta temp+1

  txa
  clc
  adc pc
  sta pc
  lda temp+1
  adc pc+1
  sta pc+1
  jmp next

; ret
opc9:
  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta pc
  incw sp, sp+1

  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta pc+1
  incw sp, sp+1

  jmp next


; sub a, r8
.repeat 8, I
  OP .set $90|I
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda REG_A
    .else
      lda regs+(ld_src+1)
    .endif
      sta temp

    .if INCLUDE_HALF_CARRY = 1
      lda REG_A
      sec
      sbc temp
      tay
      php

      eor REG_A
      eor temp
      and #$10
      sta F_H
      sty REG_A
    .else
      lda REG_A
      sec
      sbc temp
      sta REG_A
      php
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    eor #%00000001
    sta F_C

    lda #$ff
    sta F_N

  jmp next
.endrepeat

; ret cond
.repeat 4, I
  OP .set (I<<3)|%11000000
  makelabel "op", .sprintf("%02x",OP)
    .if I > 1
      lda F_C
    .else
      lda F_Z
    .endif
    .if (I&1) = 0
      bne :+
    .else
      beq :+
    .endif
      jmp opc9
    :
    jmp next
.endrepeat

; jp hl
ope9:
  lda REG_H
  sta pc+1
  lda REG_L
  sta pc+0
  jmp next

; push r16mem

; - push af [$f5]
opf5:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_A
  sta write_val
  jsr write_mem

  lda #0
  ldx F_Z
  beq :+
  ora #1<<7
:

  ldx F_N
  beq :+
  ora #1<<6
:
.if INCLUDE_HALF_CARRY = 1
  ldx F_H
  beq :+
  ora #1<<5
:
.endif
  ldx F_C
  beq :+
  ora #1<<4
:
  sta write_val

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  jsr write_mem
  jmp next

; - push hl [$e5]
ope5:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_H
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_L
  sta write_val
  jsr write_mem
  jmp next

; - push de [$d5]
opd5:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_D
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_E
  sta write_val
  jsr write_mem
  jmp next

; - push bc [$c5]
opc5:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_B
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda REG_C
  sta write_val
  jsr write_mem
  jmp next


; pop r16mem

; - pop af [$f1]
opf1:
  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  pha
  and #1<<7
  sta F_Z
  pla

  pha
  and #1<<6
  sta F_N
  pla

.if INCLUDE_HALF_CARRY = 1
  pha
  and #1<<5
  sta F_H
  pla
.endif

  pha
  and #1<<4
  sta F_C
  pla

  incw sp, sp+1

  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_A

  incw sp, sp+1

  jmp next

; - pop hl [$e1]
ope1:
  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_L
  incw sp, sp+1

  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_H

  incw sp, sp+1

  jmp next

; - pop de [$d1]
opd1:
  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_E
  incw sp, sp+1

  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_D

  incw sp, sp+1

  jmp next

; - pop bc [$c1]
opc1:
  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_C
  incw sp, sp+1

  lda sp
  sta mem_p+1
  lda sp+1
  sta mem_p+2

  jsr read_mem_no_pc
  sta REG_B

  incw sp, sp+1

  jmp next

; inc bc [$03]
op03:
  incw REG_C, REG_B
  jmp next

; inc de [$13]
op13:
  incw REG_E, REG_D
  jmp next

; inc hl [$23]
op23:
  incw REG_L, REG_H
  jmp next

; inc sp [$33]
op33:
  incw sp, sp+1
  jmp next

; dec bc [$0b]
op0b:
  decw REG_C, REG_B
  jmp next

; dec de [$1b]
op1b:
  decw REG_E, REG_D
  jmp next

; dec hl [$2b]
op2b:
  decw REG_L, REG_H
  jmp next

; dec sp [$3b]
op3b:
  decw sp, sp+1
  jmp next




; or a, r8
.repeat 8, I
  OP .set I|$B0
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 7
      .if LY_WAITLOOP_OPTIMIZATION >= 1
        lda z:waitloop_read
        cmp #1
        bne :+
        inc z:waitloop_read
        stz z:waitloop_cmp        
      :
      .endif
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif
    .if ld_src <> 7
      ora REG_A
    .endif
    php
    sta REG_A
    pla ; get flag from php
    and #%00000010 ; get "zero" flag
    sta F_Z

    stz F_N
    .if INCLUDE_HALF_CARRY = 1
      stz F_H
    .endif
    stz F_C
    jmp next
.endrepeat

; cp a, r8
.repeat 8, I
  OP .set I|%10111000
  ld_src .set I&7
  ld_dst .set (I>>3)&7
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

      sta temp

      .if INCLUDE_HALF_CARRY = 1
        and #$0f
        sta temp+1

        ldx #0
        lda REG_A
        and #$0f
        cmp temp+1
        bcs :+
        ldx #$ff
      :
        stx F_H
        lda #1
        sta F_N
      .endif

      .if LY_WAITLOOP_OPTIMIZATION >= 1
        ldx z:waitloop_read
        cpx #1
        bne :+
        inc z:waitloop_read
        sta z:waitloop_cmp        
      :
      .endif

      lda REG_A
      cmp temp
      php

      pla ; get flag from php
      tax
      and #%00000010 ; get "zero" flag
      sta F_Z

      txa
      and #%00000001 ; get "carry" flag
      eor #%00000001
      sta F_C

  jmp next
.endrepeat

; cp a, u8
opfe:
      jsr read_pc_byte

      sta temp

      .if INCLUDE_HALF_CARRY = 1
        and #$0f
        sta temp+1

        ldx #0
        lda REG_A
        and #$0f
        cmp temp+1
        bcs :+
        ldx #$ff
      :
        stx F_H
        lda #1
        sta F_N
      .endif

      .if LY_WAITLOOP_OPTIMIZATION >= 1
        ldx z:waitloop_read
        cpx #1
        bne :+
        inc z:waitloop_read
        sta z:waitloop_cmp        
      :
      .endif

      lda REG_A
      cmp temp
      php

      pla ; get flag from php
      tax
      and #%00000010 ; get "zero" flag
      sta F_Z

      txa
      and #%00000001 ; get "carry" flag
      eor #%00000001
      sta F_C
  jmp next

; ldh a, [imm8]
opf0:
  jsr read_pc_byte
  sta mem_p+1

  jsr read_HRAM
  sta REG_A
  jmp next

; ld a, u16
opfa:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  pla
  stx mem_p+2
  sta mem_p+1
  jsr read_mem_no_pc
  sta REG_A
  jmp next


; and a, imm8
ope6:
  stz F_N
  stz F_C
  .if INCLUDE_HALF_CARRY = 1
    ldx #1
    stx F_H
  .endif
  .if LY_WAITLOOP_OPTIMIZATION >= 2
    lda #$80
    tsb z:waitloop_modes
  .endif

  jsr read_pc_byte

  .if LY_WAITLOOP_OPTIMIZATION >= 2
    sta z:waitloop_and
  .endif

  and REG_A
  php
  sta REG_A
  pla
  and #1<<1
  sta F_Z
  jmp next

; or a, imm8
opf6:
  stz F_N
  stz F_C
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  jsr read_pc_byte
  ora REG_A
  php
  sta REG_A
  pla
  and #1<<1
  sta F_Z
  jmp next

; xor a, imm8
opee:
  stz F_N
  stz F_C
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  jsr read_pc_byte
  eor REG_A
  php
  sta REG_A
  pla
  and #1<<1
  sta F_Z
  jmp next

; call cond, imm16
.repeat 4, I
  OP .set (I<<3)|%11000100
  makelabel "op", .sprintf("%02x",OP)
    jsr read_pc_byte
    pha
    jsr read_pc_byte
    stx temp+3
    pla
    sta temp+2

    .if I > 1
      lda F_C
    .else
      lda F_Z
    .endif
    .if (I&1) = 0
      bne :+
    .else
      beq :+
    .endif
      jmp call_cond
    :
    jmp next
.endrepeat

call_cond:
  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+1
  sta write_val
  jsr write_mem

  decw sp, sp+1
  lda sp
  sta mem_w+1
  lda sp+1
  sta mem_w+2

  lda pc+0
  sta write_val
  jsr write_mem

  lda temp+2
  sta pc
  lda temp+3
  sta pc+1
  jmp next


; ld a, [bc]
op0a:
  lda REG_B
  sta mem_p+2
  lda REG_C
  sta mem_p+1
  jsr read_mem_no_pc
  sta REG_A
  jmp next

; ld a, [de]
op1a:
  lda REG_D
  sta mem_p+2
  lda REG_E
  sta mem_p+1
  jsr read_mem_no_pc
  sta REG_A
  jmp next

; ld a, [hld]
op3a:
  lda REG_H
  sta mem_p+2
  lda REG_L
  sta mem_p+1
  jsr read_mem_no_pc
  sta REG_A
  decw REG_L, REG_H
  jmp next

; and a, r8
.repeat 8, I
  OP .set I|%10100000
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    stz F_N
    stz F_C
    .if INCLUDE_HALF_CARRY = 1
      ldx #1
      stx F_H
    .endif

    .if LY_WAITLOOP_OPTIMIZATION >= 2
      lda #$80
      tsb z:waitloop_modes
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda regs+0
    .else
      lda regs+(ld_src+1)
    .endif

    .if ld_src = 7
      .if LY_WAITLOOP_OPTIMIZATION >= 1
        pha
        lda z:waitloop_read
        cmp #1
        bne :+
        inc z:waitloop_read
        stz z:waitloop_cmp        
      :
        pla
      .endif
    .elseif LY_WAITLOOP_OPTIMIZATION >= 2
      sta z:waitloop_and
    .endif

    .if ld_src <> 7
      and REG_A
    .endif
    php
    sta REG_A
    pla
    and #1<<1
    sta F_Z
    jmp next
.endrepeat


; xor a, r8
.repeat 8, I
  OP .set I|%10101000
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    stz F_N
    stz F_C
    .if INCLUDE_HALF_CARRY = 1
      stz F_H
    .endif

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda #0
    .else
      lda regs+(ld_src+1)
    .endif

    .if ld_src <> 7
      eor REG_A
    .endif

    php
    sta REG_A
    pla
    and #1<<1
    sta F_Z
    jmp next
.endrepeat

; ld [hli], a
op22:
  lda REG_H
  sta mem_w+2
  lda REG_L
  sta mem_w+1
  incw REG_L, REG_H
  lda REG_A
  sta write_val
  jsr write_mem
  jmp next


; add a, imm8
opc6:
  stz F_N

  jsr read_pc_byte

  .if INCLUDE_HALF_CARRY = 1
    ldx REG_A
  .endif
  clc
  adc REG_A
  php
  sta REG_A

  pla ; get flag from php
  tay
  and #%00000010 ; get "zero" flag
  sta F_Z

  tya
  and #%00000001 ; get "carry" flag
  sta F_C

  .if INCLUDE_HALF_CARRY = 1
    txa
    eor REG_A
    eor inst
    and #$10
    sta F_H
  .endif

  jmp next

; ld [hld], a
op32:
  lda REG_H
  sta mem_w+2
  lda REG_L
  sta mem_w+1
  decw REG_L, REG_H
  lda REG_A
  sta write_val
  jsr write_mem
  jmp next

; sub a, imm8
opd6:
  jsr read_pc_byte

  .if INCLUDE_HALF_CARRY = 1
    lda REG_A
    sec
    sbc inst
    pha
    php

    eor REG_A
    eor inst
    and #$10
    sta F_H

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    eor #%00000001
    sta F_C

    pla
    sta REG_A
  .else
    lda REG_A
    sec
    sbc inst
    sta REG_A
    php

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    eor #%00000001
    sta F_C

  .endif

  lda #$ff
  sta F_N
  jmp next

; rlca
op07:
  stz F_N
  stz F_Z
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif

  lda REG_A
  cmp #$80
  rol a
  sta REG_A
  php

  pla
  and #%00000001 ; get "carry" flag
  sta F_C
  jmp next

; rrca
op0f:
  stz F_N
  stz F_Z
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif

  lda REG_A
  tax
  ror a
  txa
  ror a
  sta REG_A
  php

  pla
  and #%00000001 ; get "carry" flag
  sta F_C
  jmp next

; rla
op17:
  ldy F_C

  lda REG_A
  and #128
  sta F_C

  stz F_N
  stz F_Z
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif

  lda REG_A
  cpy #1
  rol a
  sta REG_A
  jmp next

; rra
op1f:
  ldy F_C

  lda REG_A
  and #1
  sta F_C

  stz F_N
  stz F_Z
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif

  lda REG_A
  cpy #1
  ror a
  sta REG_A
  jmp next

; adc a, r8
.repeat 8, I
  OP .set $88|I
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda REG_A
    .else
      lda regs+(ld_src+1)
    .endif
      sta temp

    ldx F_C
    cpx #$01
    adc REG_A
    .if INCLUDE_HALF_CARRY = 1
      pha
    .else
      sta REG_A
    .endif
    php

    .if INCLUDE_HALF_CARRY = 1
      eor REG_A
      eor temp
      and #$10
      sta F_H
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

    .if INCLUDE_HALF_CARRY = 1
      pla
      sta REG_A
    .endif

    stz F_N

  jmp next
.endrepeat

; adc a, imm8
opce:
    jsr read_pc_byte

    ldx F_C
    cpx #$01
    adc REG_A
   .if INCLUDE_HALF_CARRY = 1
      pha
    .else
      sta REG_A
    .endif
    php

    .if INCLUDE_HALF_CARRY = 1
      eor REG_A
      eor inst
      and #$10
      sta F_H
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    sta F_C

    .if INCLUDE_HALF_CARRY = 1
      pla
      sta REG_A
    .endif

    stz F_N
    jmp next

; add hl, r16

; - add hl, bc [$09]
op09:
  .if INCLUDE_HALF_CARRY = 1
    lda #0
    tax
    sta F_N
    clc
    lda REG_L
    adc REG_C
    sta REG_L
    lda REG_H
    tay
    adc REG_B
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C

    tya
    eor REG_H
    eor REG_B
    and #$10
    sta F_H
  .else
    ldx #0
    stz F_N
    clc
    lda REG_L
    adc REG_C
    sta REG_L
    lda REG_H
    adc REG_B
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C
  .endif
  jmp next

; - add hl, de [$19]
op19:
  .if INCLUDE_HALF_CARRY = 1
    lda #0
    tax
    sta F_N
    clc
    lda REG_L
    adc REG_E
    sta REG_L
    lda REG_H
    tay
    adc REG_D
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C

    tya
    eor REG_H
    eor REG_D
    and #$10
    sta F_H
  .else
    ldx #0
    stz F_N
    clc
    lda REG_L
    adc REG_E
    sta REG_L
    lda REG_H
    adc REG_D
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C
  .endif
  jmp next

; - add hl, hl [$29]
; - NOTE: [add hl, hl] is OPTIMIZED
op29:
  ldx #0
  stz F_N
  asl REG_L
  rol REG_H
  bcc :+
  ldx #$ff
:
  stx F_C

  .if INCLUDE_HALF_CARRY = 1
    lda REG_H
    and #$10
    sta F_H
  .endif
  jmp next

; - add hl, sp [$39]
op39:
  .if INCLUDE_HALF_CARRY = 1
    lda #0
    tax
    sta F_N
    clc
    lda REG_L
    adc sp
    sta REG_L
    lda REG_H
    tay
    adc sp+1
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C

    tya
    eor REG_H
    eor sp+1
    and #$10
    sta F_H
  .else
    ldx #0
    stz F_N
    clc
    lda REG_L
    adc sp
    sta REG_L
    lda REG_H
    adc sp+1
    sta REG_H
    bcc :+
    ldx #$ff
  :
    stx F_C
  .endif
  jmp next

; jp cond, imm16
.repeat 4, I
  OP .set (I<<3)|%11000010
  makelabel "op", .sprintf("%02x",OP)
    jsr read_pc_byte
    pha
    jsr read_pc_byte
    stx temp+1
    pla
    sta temp

    .if I > 1
      lda F_C
    .else
      lda F_Z
    .endif
    .if (I&1) = 0
      bne :+
    .else
      beq :+
    .endif
      lda temp
      sta pc
      lda temp+1
      sta pc+1
    :
    jmp next
.endrepeat

; daa (implemented if assembler flag is enabled)
op27:
  .if INCLUDE_HALF_CARRY = 1
    stz z:F_Z

    lda z:REG_A
    ldx z:F_N
    beq :+

    ; flags.n == 1
    ldx z:F_C
    beq @skip_c1
    sec
    sbc #$60
@skip_c1:

    ldx z:F_H
    beq @skip_h1
    sec
    sbc #$06
@skip_h1:

    bra :++
:
    ; flags.n == 0
    ldx z:F_C
    bne @do_c2
    cmp #$9a
    bcc @skip_c2
@do_c2:
    ldx #1
    stx z:F_C

    clc
    adc #$60
@skip_c2:

    ldx z:F_H
    bne @do_h2

    tax
    ldy four_bit_ident, x
    cpy #$0a
    bcc @skip_h2
@do_h2:
    clc
    adc #$06
@skip_h2:

:

    cmp #0
    bne :+
    inc z:F_Z
:

    stz z:F_H
  .endif
  jmp next

; ld [imm16], sp
op08:
  jsr read_pc_byte
  pha
  jsr read_pc_byte
  stx temp+3
  pla
  sta temp+2

  lda temp+2
  sta mem_w+1
  lda temp+3
  sta mem_w+2

  lda sp
  sta write_val
  jsr write_mem

  clc
  lda temp+2
  adc #1
  sta mem_w+1
  lda temp+3
  adc #0
  sta mem_w+2

  lda sp+1
  sta write_val
  jsr write_mem
  jmp next

; ld sp, hl
opf9:
  lda REG_H
  sta sp+1
  lda REG_L
  sta sp+0
  jmp next

; add a, r8
.repeat 8, I
  OP .set $80|I
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    stz F_N

    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda REG_A
    .else
      lda regs+(ld_src+1)
    .endif
      sta temp

      .if INCLUDE_HALF_CARRY = 1
        ldx REG_A
      .endif
      clc
      adc REG_A
      php
      sta REG_A

      pla ; get flag from php
      tay
      and #%00000010 ; get "zero" flag
      sta F_Z

      tya
      and #%00000001 ; get "carry" flag
      sta F_C

      .if INCLUDE_HALF_CARRY = 1
        txa
        eor REG_A
        eor temp
        and #$10
        sta F_H
      .endif

  jmp next
.endrepeat

; sbc a, r8
.repeat 8, I
  OP .set $98|I
  ld_src .set I
  ld_dst .set I
  makelabel "op", .sprintf("%02x",OP)
    .if ld_src = 6
      lda REG_H
      sta mem_p+2
      lda REG_L
      sta mem_p+1
      jsr read_mem_no_pc
    .elseif ld_src = 7
      lda REG_A
    .else
      lda regs+(ld_src+1)
    .endif
      sta temp

    ; i love carry yayaayayay
    ldx F_C
    cpx #0
    sec
    beq :+
    clc
  :

    lda REG_A
    sbc temp
    .if INCLUDE_HALF_CARRY = 1
      tay
    .endif
    php

    .if INCLUDE_HALF_CARRY = 1
      eor REG_A
      eor temp
      and #$10
      sta F_H
      sty REG_A
    .else
      sta REG_A
    .endif

    pla ; get flag from php
    tax
    and #%00000010 ; get "zero" flag
    sta F_Z

    txa
    and #%00000001 ; get "carry" flag
    eor #%00000001
    sta F_C

    lda #$ff
    sta F_N

  jmp next
.endrepeat

; cpl
op2f:
  lda REG_A
  eor #$ff
  sta REG_A

  lda #1
  sta F_N
  .if INCLUDE_HALF_CARRY = 1
    sta F_H
  .endif
  jmp next

; scf
op37:
  lda #1
  sta F_C
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  stz F_N
  jmp next

; ccf
op3f:
  lda F_C
  beq :+
  lda #1
:
  eor #1
  sta F_C
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  stz F_N
  jmp next

; add sp, i8
ope8:
  stz F_Z
  stz F_N
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  stz F_C

  jsr read_pc_byte
  tax

  clc
  adc sp+0
  bcc :+
  inc F_C
:

  .if INCLUDE_HALF_CARRY = 1
  txa
  and #$0f
  sta temp+3

  clc
  lda sp
  ora #$f0
  adc temp+3
  bcc :+
  inc F_H
:
  .endif

  txa
  ; http://forum.6502.org/viewtopic.php?p=75716
  sta temp
  lda #$7f
  cmp temp
  sbc #$7f
  sta temp+1

  txa
  clc
  adc sp
  sta sp
  lda temp+1
  adc sp+1
  sta sp+1

  jmp next

; ld hl, sp + i8
opf8:
  stz F_Z
  stz F_N
  .if INCLUDE_HALF_CARRY = 1
    stz F_H
  .endif
  stz F_C

  jsr read_pc_byte
  tax

  clc
  adc sp+0
  bcc :+
  inc F_C
:

  .if INCLUDE_HALF_CARRY = 1
  txa
  and #$0f
  sta temp+3

  clc
  lda sp
  ora #$f0
  adc temp+3
  bcc :+
  inc F_H
:
  .endif

  txa
  ; http://forum.6502.org/viewtopic.php?p=75716
  sta temp
  lda #$7f
  cmp temp
  sbc #$7f
  sta temp+1

  txa
  clc
  adc sp
  sta REG_L
  lda temp+1
  adc sp+1
  sta REG_H

  jmp next

; sbc a, u8
opde:
  jsr read_pc_byte

  ; i love carry yayaayayay
  ldx F_C
  cpx #0
  sec
  beq :+
  clc
:

  lda REG_A
  sbc inst
  .if INCLUDE_HALF_CARRY = 1
    tay
  .endif
  php

  .if INCLUDE_HALF_CARRY = 1
    eor REG_A
    eor inst
    and #$10
    sta F_H
    sty REG_A
  .else
    sta REG_A
  .endif

  pla ; get flag from php
  tax
  and #%00000010 ; get "zero" flag
  sta F_Z

  txa
  and #%00000001 ; get "carry" flag
  eor #%00000001
  sta F_C

  lda #$ff
  sta F_N

  jmp next

; rst tgt3
.repeat 8, I
  OP .set %11000111|(I<<3)
  makelabel "op", .sprintf("%02x",OP)
    decw sp, sp+1
    lda sp
    sta mem_w+1
    lda sp+1
    sta mem_w+2

    lda pc+1
    sta write_val
    jsr write_mem

    decw sp, sp+1
    lda sp
    sta mem_w+1
    lda sp+1
    sta mem_w+2

    lda pc+0
    sta write_val
    jsr write_mem

    lda #(OP&$38)
    sta pc
    stz pc+1
  jmp next
.endrepeat

; ldh [c], a
ope2:
  lda REG_C
  sta mem_w+1

  lda REG_A
  sta write_val

  jsr write_HRAM
  jmp next

; ldh a, [c]
opf2:
  lda REG_C
  sta mem_p+1

  jsr read_HRAM
  sta REG_A
  jmp next

; TODO: STOP

; for memory-saving reasons the two tables are "combined"...
cmp_half_carry_inv:
  .res 15, $00
cmp_half_carry:
  .byte $ff
  .res 15, $00

.align 256

/*
inst_lut:
.repeat 256, I
  .ifdef .ident(.concat ("op", .sprintf("%02x",I)))
    .byte <.ident(.concat ("op", .sprintf("%02x",I)))
  .else
    .byte <opUNK
  .endif
.endrepeat

.repeat 256, I
  .ifdef .ident(.concat ("op", .sprintf("%02x",I)))
    .byte >.ident(.concat ("op", .sprintf("%02x",I)))
  .else
    .byte >opUNK
  .endif
.endrepeat
*/

inst_lut:
.repeat 256, I
  .ifdef .ident(.concat ("op", .sprintf("%02x",I)))
    .word .ident(.concat ("op", .sprintf("%02x",I)))&$ffff
  .else
    .word opUNK&$ffff
  .endif
.endrepeat

cycle_lut:
.word 1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, 1, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1, 2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 2, 3, 6, 2, 4, 2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4, 3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, 3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4

four_bit_ident:
  .repeat 16, I
    .byte I&$0f
  .endrepeat

.if INCLUDE_ECHO_RAM = 1
.segment "CODE4"
VRAM_tile_table:
.repeat $2000, I
    .word (I&$0f)|((I>>4)<<5)
.endrepeat
.else
VRAM_tile_table:
.repeat $100, I
    J .set (I&$0f)
    .byte (J>>1)|((J&1)<<3)|((I>>4)<<4)
.endrepeat
.endif

is_apu_reg:
.res $10, 0
.res $30, $ff
.res $80-$40, 0

mono_font:
.incbin "1bpp_font.bin"

.segment "GBROM"
; nothing! :3
