the SoupahGB emulator source code
==============

This is the source code to the SoupahGB Game Boy emulator for the SNES.
The emulator is made in 100% 65c816 assembly. In the `emu.s` file in the `src` directory, you can find a bunch of define flags like this 
```
.define INCLUDE_ACCURATE_HALT 3
...
```

Here are what these flags do
  - `INCLUDE_HALF_CARRY` (default value set to 0)
    - if set to 0 (unset), any half-carry calculations and the DAA instruction are omitted from the CPU emulator.
    - if set to 1 (set), half-carry and DAA gets emulated
  - `INCLUDE_ACCURATE_HALT` (default value set to 3)
    | Value | Description
    | :---: | --------------------------------------------------------
    | `0`   | Treats the HALT opcode like a NOP
    | `1`   | Advances per-instruction until an interrupt occurs 
    | `2`   | Advances per-scanline until an interrupt occurs (faster)
    | `3`   | Uses simple calculations to estimate how long to wait until an interrupt occurs (fastest, selected by default)
  - `INCLUDE_ECHO_RAM` (default value set to 0)
    - if set to 1, "Echo RAM" (mirror of $C000–$DDFF in $E000-$FDFF) is implemented, which may slow down emulation
  - `INCLUDE_JOYPAD_IRQ` (default value set to 0)
    - if set to 1, the joypad IRQ is implemented, which may slow down emulation
  - `AUDIO_EMULATION` (default value set to 1)
    - if set to 1, audio emulation is implemented (through a custom SPC program interfacing with reg writes), THIS IS RECOMMENDED TO BE SET TO 1, although this does slow down emulation by a tiny bit.
  - `INCLUDE_LY_LYC_STATUS` (default value set to 0)
    - if set to 1, LSB bit 2 of the STAT register ($FF41) is implemented, which may slow down emulation
  - `SCROLL_PER_SCANLINE` (default value set to 0)
    - if set to 1, per-scanline scroll register writes are implemented via HDMA, this makes some games and demos look more accurate, but it does slow down emulation.
  - `INCLUDE_FAKE_TIMER` (default value set to 1)
    - if set to 1, the timer IRQ is "faked" by only firing it every 60hz (to allow some games like Super Mario Land to have audio)

