MDRIVE -Drive Hex_address

Drive:			A to P
Hex address:	Freee space ABOVE CP/M

Example:	Assign M-DRIVE/H as drive 'M'
			Loaded above 48K CP/M
	MDRIVE -M BC00

Example:	Assign M-DRIVE/H as drive 'H'
			Loaded above 52K CP/M
	MDRIVE -H CC00

Memory	Size 	Hex address
------	----	-----------
64K		63K		FC00
60K		59K		EC00
56K		55K		DCOO
52K		51K		CCOO
48K		47K		BC00
44K		43K		AC00
40K		39K		9C00
36K		35K		8C00
32K		31K		7C00
28K		27K		6CO0
24K		23K		3COO

------------------------------

MFORM Drive[!]
Drive:			A to P. Only use 'drive' specified with MDRIVE command.
!:				Format Drive without asking for confirmation.
				Checks drive is already formatted, and will exit
				with message.

If drive is corrupted, 'MFORM Drive' will ask for confirmation
and will over-write old contents.

Example:	Format drive 'M' on BOOT
			Won't format if contents are valid.
	MFORM M!

Example:	Format drive 'M'
			Confirm that you want to erase drive, Force format operation.
	MFORM M

------------------------------

MTEST [-P -Q] [ -B[num] -F[pat] | -R | -V[pat] ]

-F=Fixed pattern. Default pattern is random unless [pat]
-R=Random pattern.
-V=Verify pattern. Default pattern is random unless [pat]
-P=Non-paged, Checks for pause/control-C
-Q=Quick, Skip F000 blocks
-B=Number of boards, 1 to 8
No args runs Random, then Fixed tests

Example:	Test one M-DRIVE/H
			Using RANDOM patterns
	MTEST

Example:	Test one M-DRIVE/H
			Using FIXED pattern 'E5'
	MTEST -F E5

Example:	Test two M-DRIVE/H cards
			Using random patterns
	MTEST -B 2 -R
