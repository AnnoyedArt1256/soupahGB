#!/usr/bin/env python3
# soupahGB_converter: a simple converter program for the soupahGB emulator
# made by AArt1256 in 23/01/2025
#
# Copyright (c) 2025 AnnoyedArt1256
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
import sys, math
import fixchecksum
from pathlib import Path

def chunks(lst, start, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(start, len(lst), n):
        yield lst[i:i + n]

if len(sys.argv) < 2:
    print("not enough arguments supplied")
    print("example arguments (outputs converted file to soupahGB_test.sfc):")
    print("soupahGB_converter.py test.gb")
else:
    emu_rom = list(open("soupahGB_stub.sfc","rb").read())
    input_rom = list(open(sys.argv[1],"rb").read())
    prebanked_rom = []
    for data in chunks(input_rom, 16384, 16384):
        prebanked_rom.extend(input_rom[0:16384])
        prebanked_rom.extend(data)

    emu_rom[0x100] = 0xff
    emu_rom[0x40000:0x40000+len(prebanked_rom)] = prebanked_rom;
    del input_rom
    write_name = "soupahGB_"+str(Path(sys.argv[1]).stem)+".sfc"
    write_file = open(write_name,"wb")
    write_file.write(bytearray(emu_rom))
    write_file.close()
    # from fixchecksum.py by Pinobatch
    fixchecksum.main('fixchecksum.py -v --mode 20 '.split()+[write_name])
