; main.s
.global main, nmi_handler, irq_handler, cop_handler
.globalzp oam_used
.global chr_bin, chr_bin_size

; ppuclear.s
.global ppu_copy_oam, ppu_pack_oamhi, ppu_clear_oam
.global ppu_copy, ppu_clear_nt, ppu_vsync
.global OAM, OAMHI

; bg.s
.global load_bg_tiles, draw_bg, load_bg_pal, load_bg_chr, draw_win
.global load_obj_chr

; blarggapu.s
.global spc_boot_apu

; draw.s
.global draw_pixel_cursor
.globalzp do_update, update_chunk

; emu.s
.global emu_start, VRAM, IO_SPACE, ppu_update, VRAM_4bpp, update_pal
.globalzp do_vblank
.global do_hdma
.globalzp LCDC_mirror
