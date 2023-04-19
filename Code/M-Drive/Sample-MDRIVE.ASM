;	Sample READ/WRITE routines
;Memory	Size 	Hex address
;------	----	-----------
;64K	63K		FC00
;60K	59K		EC00
;56K	55K		DCOO
;52K	51K		CCOO
;48K	47K		BC00
;44K	43K		AC00
;40K	39K		9C00
;36K	35K		8C00
;32K	31K		7C00
;28K	27K		6CO0
;24K	23K		3COO

;	ORG	3C00h

;
;	M - D R I V E / H
;
;	To quickly add this to your exlstlng bios, just replace the "JMP
;	N0THING"s at the end of each routine with a jump to your BIOS,
;	and assemble lt all together. The DPB is set for one M-DRIVE/H
;	board, lf you have more boards you will have to adjust it
;	accordingly.
;	Information such as sector, track, and DMA address are saved even
;	lf they are not intended for the memory drive.	Thls is intended
;	as an example and as a way for someone to use the M-DRIVE/H board
;	with a minimum amount of trouble and work.	The performance of
;	any CP/M BIOS can be greatly enhanced by lntegrating thls code
;	directly into an exlstlng BIOS, as well as save some memory space.
;
;	This code expects the M-DRIVE/H board to be addressed at the
;	following I/0 ports:
;
;	0C6H - Data
;	0C7H - Address
;
;		Written L. Ott	12/17/82
;		1982 by CompuPro div. Godbout Electronics

HBASE:	EQU	0C6h
HDATA:	EQU	HBASE
HADDR:	EQU	HBASE+1

MEMDRV:	EQU	'M'				;SET AS DRIVE "M"
F$RDAT:	EQU	6				;READ DATA
F$WRT:	EQU	5				;WRITE DATA
MRTRY:	EQU	10				;MAX RETRY COUNT

HMDSIB:	EQU	2048/128		;SECTORS IN MEMORY DRIVE DATA BLOCK
HMDDSM:	EQU	((512-4)*8)/HMDSIB
;
MDRIVE:	JMP		NOTHING		;COLD START
		JMP		NOTHING		;WARM START
		JMP		NOTHING		;CONSTAT
		JMP		NOTHING		;C0NIN
		JMP		NOTHING		;CONOUT
		JMP		NOTHING		;LIST
		JMP		NOTHING		;PUNCH
		JMP		NOTHING		;READER
		JMP		HOME		;HOME
		JMP		SELDSK		;SELECT DISK
		JMP		SETTRK		;SET TRACK
		JMP		SETSEC		;SET SECTOR
		JMP		SETDMA		;SET DMA
		JMP		READ		;READ
		JMP		WRITE		;WRITE
		JMP		NOTHING		;LIST STATUS
		JMP		SECTRN		;SECT0R TRANSLATE
;
;DUMMY ROUTINE FOR NON IMPLEMENTED ROUTINES
;
NOTHING:
		RET
;
;
;	S E L E C T   D I S K   D RI V E

;	Select the disk drive for subsequent disk transfers and return
;	the address of the DPH for the memory drive.
;	ENTRY -- C = disk selection value
;	  DE and l = 0 must determine disk type
;	           = 1 type already determined (only needed for floppies)
;
;	EXIT -- HL = DPH address of the memory drive.
;	        If memory drive not selected, control is passed to BIOS
;
SELDSK:	XRA		A			;CLEAR A
		STA		SELFLG		;CLEAR MEMORY DRIVE FLAG
		MOV		A,C			;
		CPI		MEMDRV-'A'	;IS MEMORY DRIVE SELECTED
BIOSSEL:JNZ		NOTHING		;IF NOT GO BACK TO BIOS
		MVI		A,0FFH
		STA		SELFLG		;SET MEMORY DRIVE AS SELECTED
		LXI		H,HMD$DPH	;GET DPH ADDRESS
		RET					;AND DONE
;
;	H O M E
;	Set current track to 0
;
HOME:	XRA		A
		STA		CIOPB+2		;SET TRACK AS 0
		STA		CIOPB+3
BI0SH0M:JMP		NOTHING		;ON TO BIOS
;
;	S  E  T   T R A C K
;
;	Set track number.  The track number 1s save for later use during
;	a disk transfer.  The track is save whether or not the memory disk
;	is currently selected.
;
;	ENTRY -- BC = Track number.
;
SETTRK:	MOV		L,C
		MOV		H,B
		SHLD	CIOPB+2
BIOSTRK:JMP		NOTHING
;
;	S E T	S E C T 0 R
;
;	Set	sector number.  The sector number is save for later use during
;	a disk transfer.  The sector is saved whether or not the memory
;	disk 1s current1y selected.
;
;	ENTRY -- BC = Sector number.

SETSEC:	MOV		L,C
		MOV		H,B
		INX		H			;+1 TO MAKE LIKE FLOPPIES (1-26)
		SHLD	CIOPB+4
BIOSSEC:JMP		NOTHING
;
;	S E T   D M A   A D D R E S S
;
;	Set the address for the next DMA transfer.
;	The DMA address is saved for later use during a disk transfer.
;	The address is saved whether or not the memory disk ia
;	currently selected.
;
;	ENTRY -- BC = Disk memory address.

SETDMA:	MOV		L,C
		MOV		H,B
		SHLD	DMAADR
BIOSDHA:JMP		NOTHING
;
;	S  E  C  T O R   T R A N S L A T E
;
;	Translate the sector number from logical to physical.
;
;	ENTRY	DE = 0, no translatlon required.
;           DE = Translation table address.
;           BC = Number to translate.
;
SECTRN:	MOV		A,D
		ORA		E
		RZ					;No translation needed.
		JMP		NOTHING		;Let BIOS translate.
;
;	R E A D
;
;	Read the currently selected track,sector from the currently
;	selected drive.
;
READ:	LDA		SELFLG
		ORA		A
BIOSRED:JZ		NOTHING
		MVI		A,F$RDAT	;MUST WANT THR HEMORY DRIVE
		STA		CIOPB		;TO COMMAND BUFFER
		JMP		HMDFNL		;AND GO PROCESS
;
;	W R I T E
;
;	Write to the current1y selected drive at the current track
;	and sector.
;
WRITE:	LDA		SELFLG
		ORA		A			;IS MEMORY DRIVE SELECTED
BIOSWRT:JZ		NOTHING
		MVI		A,F$WRT		;SET COMMAND AS WRITE
		STA		CIOPB
;		JMP		HMDFNL		;AND GO PROCESS
;
;	HMDFNL - Hard memory disk final command processing
;	ENTRY -- CIOPB+0 = COMMAND
;			 CIOPB+2 = TRACK
;			 CIOPB+4 = SECTOR
;
HMDFNL:	MVI		A,MRTRY		;Set retry count
		STA		RTRY
HMDFNL1:LHLD	CIOPB+2		;GET TRACK
		DAD		H			;*2
		DAD		H			;*4
		DAD		H			;*8
		DAD		H			;*l6
		DAD		H			;*32
		DAD		H			;*64
		DAD		H			;*128
		LDA		CIOPB+4		;GET SECTOR
		DCR		A			;STARTS AT 0
		CALL	SETMDRV		;SET HDRIVE BOARD VIA <A>,<HL>
		MVI		C, 128		;<C> = 128 BYTE'S PER SECTOR
		MOV		B,C			; also into <B> for parity check
		LHLD	DMAADR		;DIRECTLY TO CPM BUFFER
		LDA		CIOPB		;GET COMMAND
		CPI		F$RDAT		;
		JNZ		M$WRITE		;IF WRITE COMMAND JUMP
;
;		JMP		M$READ
;
;	GET A 128 BYTE BLOCK FROM M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C> IS NUMBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO PUT DATA
;
;	EXIT -- <A> = 0 No errors, 1 READ error.
M$READ:	IN		HDATA		;GET DATA BYTE
		MOV		M,A			;PUT TO DMA BLOCK
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		INX		H			;UP POINTER
		DCR		C			;C0UNT THIS MOVE
		JNZ		M$READ		;AND DO AGAIN
		CALL	PARITY		;D0 PARITY CHECK
		IN		HDATA		;GET PARITY BYTE
		SUB		B			; 0 IF NO ERROR
		RZ					;	THEN RETURN
		LDA		RTRY		;GET RETRY COUNT
		DCR		A			;
		STA		RTRY		;REPLACE WITH NEW COUNT
		JNZ		HMDFNL1		;
		ORI		1			;Set ERROR flag
		RET					;RETURN WHEN DONE
;
;	PUT A BLOCK OF DATA INTO M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C>  IS NUNBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO GET DATA FROM
;
;	EXIT -- <A> = 0 No errors.
M$WRITE:MOV		A,M			;GET BYTE FROH BUFFER
		OUT		HDATA		;TO MDRIVE BOARD
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		INX		H
		DCR		C			;COUNT THIS MOVE
		JNZ		M$WRITE		;IF DONE WITH 128 OR <C> COUNT
		CALL	PARITY		;SELECT PARITY BYTE LOCATION
		MOV		A,B
		OUT		HDATA		;WRITE PARITY BYTE TO HEMORY DISK
		XRA		A			;AND SHOW NO ERROR
		RET					;RETURN
;
;	FIND LOCATION IF PARITY BYTE FOR TRACK IN CIOPB+2
;	AND THE SECTOR IN CIOPB+4
PARITY:	LDA		CIOPB+4		;Get sector
		DCR		A			;Bias
		LHLD	CIOPB+2		;Get track

;	Set	M-DRIVE/H board to address In <A>,<HL>
SETMDRV:OUT		HADDR		;<A>: to M-DRIVE/H board
		MOV		A,H			;<H>: is always 0 or 1
		OUT		HADDR
		MOV		A,L			;<L>: Upper bit's picks track 0-3
		OUT		HADDR		; remainder picks sector and byte
		RET
;
;	Disk Parameter Block
;
HMD$DPB:					;SET FOR 1 BOARD, 8 SPT
		DB	128				;HDRIVE TYPE FOR MFORM, DO NOT CHANGE
		DW	8				;SECT0RS PER TRACK, THIS AND DSM ...
							;...HUST BE INCREASED FOR MORE BOARDS
		DB	4,151			;BSH, BLH, EXM
		DW	HMDDSM-1,128-1	;DSM , NUMBER OF DIRECTORY ENTRIES
		DB	11000000B,00000000B	;RESERVED DIR BLOCKS, MUST MATCH ...
							; ... NUMBER OF DIRECTORY ENTRIES
		DW	0				;CHECK SUM
		DW	4				;NUMBER OF RESERVED TRAGKS FOR PARITY CHECK
							;	THIS IS ALWAYS 4
;
;	Disk Parameter Header
;
HMD$DPH:
		DW	0				;SECT0R TRANSLATE TABLE
		DW	0,0,0			;SCRATCH
		DW	BUFDIR			;DIRECT0RY BUFFER
		DW	HMD$DPB+1		;DPB BLOCK
		DW	0				;CHECKSUM VECTOR
		DW	ALV				;ALL0CATI0N VECTOR
;
MDRLNG:	EQU		$-MDRIVE	;LENGTH OF MDRIVE BIOS
;
;	Flags and Buffers
;
SELFLG:	DS		1			;DISK SELECT FLAG
RTRY:	DS		1			;RETRY COUNTER
CIOPB:	DS		8			;COMMAND BUFFER,TRACK,SECTOR
DMAADR:	DS		2			;ADDRESS OF DHA BUFFER
BUFDIR:	DS		128			;DIRECTORY BUFFER FOR CPM
ALV:	DS		(((HMDDSM*8)+7)/8)+1
	END
