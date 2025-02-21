#!/usr/bin/make -f
#
# Makefile for LoROM template
# Copyright 2014-2015 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the SFC program and the zip file.
title = test
version = 0.1

# Space-separated list of asm files without .s extension
# (use a backslash to continue on the next line)
objlist = \
  snesheader init emu main \
  ppuclear blarggapu spcimage bg
objlistspc = \
  spcheader spcimage
brrlist = huugh

AS65 := ca65
LD65 := ld65
CFLAGS65 := -g
objdir := obj/snes
srcdir := src
imgdir := tilesets

ifdef COMSPEC
PY := py.exe
else
PY :=
endif

# Calculate the current directory as Wine applications would see it.
# yep, that's 8 backslashes.  Apparently, there are 3 layers of escaping:
# one for the shell that executes sed, one for sed, and one for the shell
# that executes wine
# TODO: convert to use winepath -w
wincwd := $(shell pwd | sed -e "s'/'\\\\\\\\'g")

# .PHONY means these targets aren't actual filenames
.PHONY: all dist clean

all: $(title).sfc

clean:
	-rm $(objdir)/*.o $(objdir)/*.chrsfc $(objdir)/*.chrgb
	-rm $(objdir)/*.wav $(objdir)/*.brr $(objdir)/*.s

dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in all README.md $(objdir)/index.txt
	$(PY) tools/zipup.py $< $(title)-$(version) -o $@
	-advzip -z3 $@

# Build zip.in from the list of files in the Git tree
zip.in:
	git ls-files | grep -e "^[^.]" > $@
	echo zip.in >> $@
	echo $(title).sfc >> $@
	echo $(title).spc >> $@

$(objdir)/index.txt: makefile
	echo "Files produced by build tools go here. (This file's existence forces the unzip tool to create this folder.)" > $@

# Rules for ROM

objlisto = $(foreach o,$(objlist),$(objdir)/$(o).o)
objlistospc = $(foreach o,$(objlistspc),$(objdir)/$(o).o)
brrlisto = $(foreach o,$(brrlist),$(objdir)/$(o).brr)

map.txt $(title).sfc: lorom256k.cfg $(objlisto)
	$(LD65) -o $(title).sfc --dbgfile $(title).dbg -m map.txt -C $^
	$(PY) tools/fixchecksum.py $(title).sfc

spcmap.txt $(title).spc: spc.cfg $(objlistospc)
	$(LD65) -o $(title).spc -m spcmap.txt -C $^

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/snes.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/mktables.s: tools/mktables.py
	$< > $@

$(objdir)/spcimage.o: $(brrlisto)

$(objdir)/bg.o:

# Rules for CHR data

# .chrgb (CHR data for Game Boy) denotes the 2-bit tile format
# used by Game Boy and Game Boy Color, as well as Super NES
# mode 0 (all planes), mode 1 (third plane), and modes 4 and 5
# (second plane).
$(objdir)/%.chrgb: tilesets/%.png
	$(PY) tools/pilbmp2nes.py --planes=0,1 $< $@

$(objdir)/%.chrsfc: tilesets/%.png
	$(PY) tools/pilbmp2nes.py "--planes=0,1;2,3" $< $@


# Rules for audio
$(objdir)/%.brr: audio/%.wav
	$(PY) tools/wav2brr.py $< $@
$(objdir)/%.brr: $(objdir)/%.wav
	$(PY) tools/wav2brr.py $< $@
$(objdir)/%loop.brr: audio/%.wav
	$(PY) tools/wav2brr.py --loop $< $@
$(objdir)/%loop.brr: $(objdir)/%.wav
	$(PY) tools/wav2brr.py --loop $< $@
$(objdir)/karplusbass.wav: tools/karplus.py
	$(PY) $< -o $@ -n 1024 -p 64 -r 4186 -e square:1:4 -a 30000 -g 1.0 -f .5
$(objdir)/hat.wav: tools/makehat.py
	$(PY) $< $@
