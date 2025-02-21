.setcpu "none"
.include "spc-65c02.inc"
.feature c_comments

TIMER_HZ = 64

.define chnum 4

.macro decx addr
 pha
 lda addr, x
 dec a
 sta addr, x
 pla
.endmacro

.macro incx addr
 pha
 lda addr, x
 inc a
 sta addr, x
 pla
.endmacro

.macro ldya addr
 lda (<addr), y
.endmacro


.macro stya addr
.local addr1
addr1 = addr
  .assert addr <= $00FE, error, "stya works only in zero page"
  movw <addr, ya
.endmacro

; MMIO at $00F0-$00FF
TIMEREN     := $00F1  ; 0-2: enable timer; 7: enable ROM in $FFC0-$FFFF
DSPADDR     := $00F2
DSPDATA     := $00F3
TIMERPERIOD := $00FA  ; Divisors for timers (0, 1: 8 kHz base; 2: 64 kHz base)
TIMERVAL    := $00FD  ; Number of times timer incremented (bits 3-0; cleared on read)

DSP_CLVOL    = $00
DSP_CRVOL    = $01
DSP_CFREQLO  = $02  ; Playback frequency in 7.8125 Hz units
DSP_CFREQHI  = $03  ; (ignored
DSP_CSAMPNUM = $04
DSP_CATTACK  = $05  ; 7: set; 6-4: decay rate; 3-0: attack rate
DSP_CSUSTAIN = $06  ; 7-5: sustain level; 4-0: sustain decay rate
DSP_CGAIN    = $07  ; Used only when attack is disabled

DSP_LVOL     = $0C
DSP_RVOL     = $1C
DSP_LECHOVOL = $2C
DSP_RECHOVOL = $3C
DSP_KEYON    = $4C
DSP_KEYOFF   = $5C
DSP_FLAGS    = $6C  ; 5: disable echo; 4-0: set LFSR rate
DSP_FMCH     = $2D  ; Modulate these channels' frequency by the amplitude before it
DSP_NOISECH  = $3D  ; Replace these channels with LFSR noise
DSP_ECHOCH   = $4D  ; Echo comes from these channels
DSP_SAMPDIR  = $5D  ; High byte of base address of sample table

.export sample_dir, spc_entry

.segment "SPCZEROPAGE"
IO_SPACE: .res 128

chan_enable: .res 4
chan_enable2: .res 4
length_timer: .res 4
env: .res 4
shadow_sweep: .res 2

vol: .res 4
vol_frac: .res 4
pitch_lo: .res 3
pitch_hi: .res 3
temp: .res 4
duty: .res 2
tick: .res 2
trigger: .res 4
T1: .res 2
T2: .res 2
PRODUCT: .res 4
sweep_frac: .res 2
sweep_timer: .res 2
spc_vbl: .res 1
len_enable: .res 4

.segment "SPCIMAGE"
.align 256
sample_dir:
  ; each directory entry is 4 bytes:
  ; a start address then a loop address
  .addr pulse_8_brr, pulse_8_brr
  .addr pulse_4_brr, pulse_4_brr
  .addr pulse_2_brr, pulse_2_brr
  .addr pulse_4_brr, pulse_4_brr
  .addr wavetable_brr, wavetable_brr
  .addr huugh_brr, huugh_brr
  nop  ; resync debugger's disassembly
.align 256
spc_entry:
  jsr initaddr
  lda $f7
  sta spc_vbl
nexttick:
  jsr check_reg
  lda $f7
  cmp spc_vbl
  beq nexttick
  sta spc_vbl

  jsr playaddr
/*
nexttick2:
  jsr check_reg
  lda $f7
  cmp spc_vbl
  beq nexttick2
  sta spc_vbl
*/
  jsr playaddr_2
  jmp nexttick

.proc initaddr
  ldy #$7F
  lda #DSP_LVOL  ; overall output volume left
  stya DSPADDR
  lda #DSP_RVOL  ; overall output volume right
  stya DSPADDR

  ; Disable the APU features we're not using
  ldy #%00100000  ; mute off, echo write off, LFSR noise stop
  lda #DSP_FLAGS
  stya DSPADDR
  ldy #$00
  lda #DSP_KEYON  ; Clear key on
  stya DSPADDR
  lda #DSP_FMCH   ; Clear frequency modulation
  stya DSPADDR

  dey
  lda #DSP_KEYOFF  ; Key off everything
  stya DSPADDR
  iny

  ldy #%00001000
  lda #DSP_NOISECH  ; LFSR noise on no channels
  stya DSPADDR
  ldy #0
  lda #DSP_ECHOCH  ; Echo on no channels
  stya DSPADDR
  lda #DSP_LECHOVOL  ; Left echo volume = 0
  stya DSPADDR
  lda #DSP_RECHOVOL  ; Right echo volume = 0
  stya DSPADDR

  lda #DSP_KEYOFF
  ldy #0
  stya DSPADDR
  lda #DSP_KEYON
  sta DSPADDR

  lda #DSP_SAMPDIR  ; set sample directory start address
  ldy #>sample_dir
  stya DSPADDR

  lda #8000/TIMER_HZ  ; S-Pently will use 125 Hz
  sta TIMERPERIOD
  lda #%10000001
  sta TIMEREN
  lda TIMERVAL

  ldx #chnum-1
:
  sta pitch_lo, x
  sta pitch_hi, x
  sta vol, x
  dex
  bpl :-

  lda #%00001111
  ldx #$4c
  stx DSPADDR
  sta DSPDATA

  lda #$fe
  sta $f6 ; CPUIO2
  lda #0
  sta tick
  sta tick+1

  rts
.endproc

.macro get_patzp
  .local skipW
  inc patzp
  bne skipW
  inc patzp+1
skipW:
  ldya patzp
.endmacro

.macro cmp16 val1, val2
    lda val1
    sec
    sbc val2
    php
    lda val1+1
    sbc val2+1
    php
    pla
    sta macroIns
    pla
    and #%00000010
    ora #%11111101
    and macroIns
    pha
    plp
.endmacro

.macro cmp16a val1, val2
    lda val1
    sec
    sbc #<val2
    php
    lda val1+1
    sbc #>val2
    php
    pla
    sta macroIns
    pla
    and #%00000010
    ora #%11111101
    and macroIns
    pha
    plp
.endmacro

doFinepitch:
  lda pitch_lo, x
  sta temp
  lda pitch_hi, x
  sta temp+1
  rts

goto_huugh:
  ;lda $f5
  ;cmp #$de
  ;bne skip_huugh

  lda #$04
  sta DSPADDR
  lda #5
  sta DSPDATA

  lda #15
  xcn a
  lsr a
  ldx #0
  stx DSPADDR
  sta DSPDATA
  inx
  stx DSPADDR
  sta DSPDATA
  lda #$60
  inx
  stx DSPADDR
  sta DSPDATA
  lda #05
  inx
  stx DSPADDR
  sta DSPDATA
  inx
  inx
  stx DSPADDR
  lda #$ff
  sta DSPDATA
  inx
  stx DSPADDR
  lda #$e0
  sta DSPDATA

huugh_inf:
  jmp huugh_inf

.proc check_reg
;  lda $f4
;  cmp #$cc
;  bne :+
;  sta $f4
;:
;:

  lda $f4 ; CPUIO0
  cmp #$cc
  beq write_cc
  rts
write_cc:
  sta $f4

end_wait:
  lda $f4
  cmp #$de
  beq goto_huugh
  bmi end_wait

skip_huugh:
  sta $f4
  tax
  lda $f5 ; CPUIO1
  sta IO_SPACE, x

  cpx #$11
  bne :+
  and #63
  eor #$3f
  sta length_timer+0
:

  cpx #$16
  bne :+
  and #63
  eor #$3f
  sta length_timer+1
:

  cpx #$1b
  bne :+
  eor #$ff
  sta length_timer+2
:

  cpx #$20
  bne :+
  and #63
  eor #$3f
  sta length_timer+3
:

  cpx #$14
  bne @ch1_trig
  sta len_enable+0
  and #$80
  sta trigger
  beq @skip_env1
  lda chan_enable+0
  beq @skip_env1
  sta chan_enable2+0
  lda IO_SPACE+$12
  sta env+0
  xcn a
  and #$0f
  sta vol+0
  lda IO_SPACE+$13
  sta shadow_sweep
  lda IO_SPACE+$14
  and #7
  sta shadow_sweep+1
@skip_env1:
  rts
@ch1_trig:

  cpx #$19
  bne @ch2_trig
  sta len_enable+1
  and #$80
  sta trigger+1
  beq @skip_env2
  lda chan_enable+1
  beq @skip_env2
  sta chan_enable2+1
  lda IO_SPACE+$17
  sta env+1
  xcn a
  and #$0f
  sta vol+1
@skip_env2:
  rts
@ch2_trig:

  cpx #$1e
  bne @ch3_trig
  sta len_enable+2
  and #$80
  sta trigger+2
  beq @skip_env3
  lda chan_enable+1
  beq @skip_env3
  sta chan_enable2+1
@skip_env3:
  rts
@ch3_trig:

  cpx #$23
  bne @ch4_trig
  sta len_enable+3
  and #$80
  sta trigger+3
  beq @skip_env4
  lda chan_enable+3
  beq @skip_env4
  sta chan_enable2+3
  lda IO_SPACE+$21
  sta env+3
  xcn a
  and #$0f
  sta vol+3
@skip_env4:
  rts
@ch4_trig:

  cpx #$12
  bne :+
  tax
  lda chan_enable_lut, x
  sta chan_enable+0
  bne :+
  sta chan_enable2+0  
:

  cpx #$17
  bne :+
  tax
  lda chan_enable_lut, x
  sta chan_enable+1
  bne :+
  sta chan_enable2+1
:

  cpx #$1a
  bne :+
  and #128
  sta chan_enable+2
  sta chan_enable2+2
:

  cpx #$21
  bne :+
  tax
  lda chan_enable_lut, x
  sta chan_enable+3
  bne :+
  sta chan_enable2+3
:

@skip_reg_write:
  rts
.endproc

.proc playaddr
  jsr whar
  jsr update_sweep
  jsr length_cnt
  jsr length_cnt
  rts
.endproc

.proc playaddr_2
  jsr update_sweep
  jsr length_cnt
  jsr length_cnt
  rts
.endproc

length_cnt:
  ldx #0
loop_length_cnt:
  lda len_enable, x
  and #$40
  beq @skip_len_cnt
  lda chan_enable, x
  beq @skip_len_cnt

  lda length_timer, x
  beq @skip_len_cnt
  decx length_timer

  lda length_timer, x
  bne @skip_len_cnt
  sta vol, x
  sta env, x
  sta chan_enable2, x
@skip_len_cnt:
  inx
  cpx #4
  bne loop_length_cnt

update_volume_env:
    tya
    and #8
    beq @vol_down

@vol_inc:
    ; increase
    tya
    and #7
    tay

    lda vol_frac, x
    clc
    adc vol_table_lo, y
    sta vol_frac, x
    lda vol, x
    adc vol_table_hi, y
    cmp #$0f
    bcs :+
    sta vol, x
:
    rts

@vol_down:
    ; decrease
    lda vol_frac, x
    sec
    sbc vol_table_lo, y
    sta vol_frac, x
    lda vol, x
    sbc vol_table_hi, y
    cmp #$80
    bcs :+
    sta vol, x
:
    rts

vol_table_lo:
    .lobytes $00,$111,$88,$5b,$44,$36,$2d,$27

vol_table_hi:
    .hibytes $00,$111,$88,$5b,$44,$36,$2d,$27

sweep_table_lo:
    .lobytes $00,$111>>1,$88>>1,$5b>>1,$44>>1,$36>>1,$2d>>1,$27>>1

whar:
  .repeat 4, I
    .if I <> 2
      ldx #I
      lda env, x
      and #$0f
      tay
      jsr update_volume_env
    .else
      lda #0
      sta vol+I
    .endif
  .endrepeat

  .repeat 3, I
    lda IO_SPACE+$13+(I*5)
    sta pitch_lo+I
    lda IO_SPACE+$14+(I*5)
    and #7
    sta pitch_hi+I
  .endrepeat

  ldx #0
  ldy #0
set_duty:
  lda IO_SPACE+$11, y
  xcn a
  lsr
  lsr
  and #3
  sta duty, x

  tya
  clc
  adc #5
  tay

  inx
  cpx #2
  bne set_duty


.repeat 2, I
  ldx #I
  jsr doFinepitch

  lda temp+1
  clc
  adc #>snestabl_lo
  sta temp+1

  ldy #0
  ldya temp
  sta temp+2

  lda temp+1
  clc
  adc #>(snestabl_hi-snestabl_lo)
  sta temp+1

  ldy #0
  ldya temp
  sta temp+3

  clc
  asl temp+2
  rol temp+3

  lda #$04|(I<<4)
  sta DSPADDR
  lda duty+I
  and #3
  sta DSPDATA

  ldx #0
  lda chan_enable+I
  beq :+
  lda chan_enable2+I
  beq :+
  lda vol+I
  xcn a
  lsr a
  tax
:
  txa

  ldx #I<<4
  stx DSPADDR
  sta DSPDATA
  inx
  stx DSPADDR
  sta DSPDATA
  lda temp+2
  inx
  stx DSPADDR
  sta DSPDATA
  lda temp+3
  inx
  stx DSPADDR
  sta DSPDATA
  inx
  inx
  stx DSPADDR
  lda #$ff
  sta DSPDATA
  inx
  stx DSPADDR
  lda #$e0
  sta DSPDATA
.endrepeat

  ldx #7
:
  lda IO_SPACE+$30, x
  tay
  lda sign_change, y
  sta wavetable_brr+1, x
  lda IO_SPACE+$38, x
  tay
  lda sign_change, y
  sta wavetable_brr+1+9, x
  dex
  bpl :-

  ldx #2
  jsr doFinepitch

  lda temp+1
  clc
  adc #>snestabl_lo
  sta temp+1

  ldy #0
  ldya temp
  sta temp+2

  lda temp+1
  clc
  adc #>(snestabl_hi-snestabl_lo)
  sta temp+1

  ldy #0
  ldya temp
  sta temp+3

  lda #$04|(2<<4)
  sta DSPADDR
  lda #4
  sta DSPDATA


  ldx #0
  lda chan_enable+2
  beq :+
  lda chan_enable2+2
  beq :+
  lda IO_SPACE+$1c ; NR32: ch3 output level
  xcn a
  lsr
  and #3
  tax
  lda wavetable_volume, x
  tax
:
  txa

  ldx #2<<4
  stx DSPADDR
  sta DSPDATA
  inx
  stx DSPADDR
  sta DSPDATA
  lda temp+2
  inx
  stx DSPADDR
  sta DSPDATA
  lda temp+3
  inx
  stx DSPADDR
  sta DSPDATA
  inx
  inx
  stx DSPADDR
  lda #$ff
  sta DSPDATA
  inx
  stx DSPADDR
  lda #$e0
  sta DSPDATA


  ldx #0
  lda chan_enable+3
  beq :+
  lda chan_enable2+3
  beq :+
  lda vol+3
  asl
  tax
:
  txa

  ldx #3<<4
  stx DSPADDR
  sta DSPDATA
  inx
  stx DSPADDR
  sta DSPDATA

  lda IO_SPACE+$22
  tax
  lda noise_freq_table, x
  ora #%00100000  ; mute off, echo write off, LFSR noise stop
  tay
  lda #DSP_FLAGS
  stya DSPADDR

  ldx #$35
  stx DSPADDR
  lda #$ff
  sta DSPDATA
  inx
  stx DSPADDR
  lda #$e0
  sta DSPDATA

  rts

.proc multiply_16bit_unsigned
                ; <T1 * <T2 = AAaa
                ; <T1 * >T2 = BBbb
                ; >T1 * <T2 = CCcc
                ; >T1 * >T2 = DDdd
                ;
                ;       AAaa
                ;     BBbb
                ;     CCcc
                ; + DDdd
                ; ----------
                ;   PRODUCT!

                ; Perform <T1 * <T2 = AAaa
                lda T2+0
                ldy T1+0
                mul ya
                sta PRODUCT+0
                sty _AA+1

                ; Perform >T1_hi * <T2 = CCcc
                lda T2+0
                ldy T1+1
                mul ya
                sta _cc+1
                sty _CC+1

                lda T2+1
                ldy T1+0
                mul ya
                sta _bb+1
                sty _BB+1


                lda T2+1
                ldy T1+1
                mul ya
                sta _dd+1
                sty PRODUCT+3

                ; Add the separate multiplications together
                clc
_AA:            lda #0
_bb:            adc #0
                sta PRODUCT+1
_BB:            lda #0
_CC:            adc #0
                sta PRODUCT+2
                bcc :+
                    inc PRODUCT+3
                    clc
                :
_cc:            lda #0
                adc PRODUCT+1
                sta PRODUCT+1
_dd:            lda #0
                adc PRODUCT+2
                sta PRODUCT+2
                bcc :+
                    inc PRODUCT+3
                :

                rts
.endproc

update_sweep:
  lda IO_SPACE+$10
  and #8
  beq inc_sweep

dec_sweep:
  lda IO_SPACE+$10
  xcn a
  and #7
  bne :+
  lda #$ff
  sta sweep_frac
  rts
:
  tax

  sta temp
  inc sweep_timer
  lda sweep_timer
  cmp temp
  bcc @skip_sweep

  lda #0
  sta sweep_timer

/*
  lda sweep_frac
  clc
  adc sweep_table_lo, x
  sta sweep_frac
  bcc @skip_sweep
*/

  lda shadow_sweep+1
  sta T1
  lda shadow_sweep+1
  sta T1+1

  lda IO_SPACE+$10
  and #7
  tax
  beq @skip_sweep_step
  dex
@sweep_step:
  clc
  lsr T1+1
  ror T1+0
  dex
  bpl @sweep_step
@skip_sweep_step:

  lda IO_SPACE+$13
  sec
  sbc T1
  sta IO_SPACE+$13
  sta shadow_sweep
  lda IO_SPACE+$14
  and #7
  sbc T1+1
  and #7
  sta IO_SPACE+$14
  sta shadow_sweep+1

@skip_sweep:

  rts

inc_sweep:
  lda IO_SPACE+$10
  xcn a
  and #7
  bne :+
  lda #$ff
  sta sweep_frac
  rts
:
  tax

  sta temp
  inc sweep_timer
  lda sweep_timer
  cmp temp
  bcc @skip_sweep

  lda #0
  sta sweep_timer

/*
  lda sweep_frac
  clc
  adc sweep_table_lo, x
  sta sweep_frac
  bcc @skip_sweep
*/

  lda shadow_sweep+1
  sta T1
  lda shadow_sweep+1
  sta T1+1

  lda IO_SPACE+$10
  and #7
  tax
  beq @skip_sweep_step
  dex
@sweep_step:
  clc
  lsr T1+1
  ror T1+0
  dex
  bpl @sweep_step
@skip_sweep_step:

  lda IO_SPACE+$13
  clc
  adc T1
  sta IO_SPACE+$13
  sta shadow_sweep
  lda IO_SPACE+$14
  and #7
  adc T1+1
  and #7
  sta IO_SPACE+$14
  sta shadow_sweep+1

@skip_sweep:

  rts


pulse_2_brr:
  .byte $B0,$9B,$BB,$BB,$BB,$BB,$BB,$BB,$B9
  .byte $B3,$75,$55,$55,$55,$55,$55,$55,$57
pulse_4_brr:
  .byte $B0,$9B,$BB,$BB,$B9,$75,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
pulse_8_brr:
  .byte $B0,$9B,$B9,$75,$55,$55,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
wavetable_brr:
  .byte $B0,$00,$B9,$75,$55,$55,$55,$55,$55
  .byte $B3,$00,$55,$55,$55,$55,$55,$55,$57

huugh_brr:
  .incbin "obj/snes/huugh.brr"

wavetable_volume:
  .byte 0, $f0*10/54, $80*10/54, $40*10/54

; taken from project nested
.align 256
.include "snestabl.s"

sign_change:
.repeat 16, J
  .repeat 16, I
    .byte ((I-8)&15)|(((J-8)&15)<<4)
  .endrepeat
.endrepeat

chan_enable_lut:
.res 8, 0
.res 256-8, $ff
