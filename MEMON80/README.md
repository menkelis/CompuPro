The BIN file is built for CompuPro SS1
 2K RAM chip@F000h
 2K ROM chip@F800h
 I/O ports@50h


Assemble A:MEMON80@F800h, Size=800h with HEX file saved to A:
A:> ASM MEMMON80.AAZ
A:> DDT
- F100,9FF,0
- IMEMON80.HEX
- R0900
- G0000
A:> SAVE 7 MEMON80.BIN

Now ready to burn a 2516 or other single voltage 2K EPROM.
