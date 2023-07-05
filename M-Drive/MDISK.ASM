;	MDISK  Relocatable M-Drive/H Driver module
;
; ASM MDISK (ORG 0000h)
; DDT
; IMDISK.HEX
; R100
; G0
; ERA MDISK000.COM
; SAVE 1 MDISK000.COM
; ASM MDISK (ORG 0100h)
; LOAD MDISK
; ERA MDISK100.COM
; REN MDISK100.COM=MDISK.COM


	ORG	0000h
;	ORG	0100h

;
;	M - D R I V E / H
;
;	The DPB is set for one M-DRIVE/H board, and will be adjusted by the
;	installer progam accordingly.
;	Information such as sector, track, and DMA address are saved even
;	if they are not intended for the memory drive.	The performance of
;	any CP/M BIOS can be greatly enhanced by integrating this code
;	directly into an existing BIOS, as well as save some memory space.
;
;	This code expects the M-DRIVE/H board to be addressed at the
;	following I/O ports:
;
;	0C6H - Data
;	0C7H - Address
;
;		Written L. Ott	12/17/82
;		1982 by CompuPro div. Godbout Electronics
;
;	Updated for use by self-installer by Michael Enkelis 2023
;	Self-installer written by Michael Enkelis with some code
;	written by Mike Douglas 2017.

HBASE	EQU	0C6h
HDATA	EQU	HBASE
HADDR	EQU	HBASE+1

MEMDRV	EQU	'M'				;SET AS DRIVE "M"
SPT		equ	8				;Sectors per track
secsize	equ	128				;Size of sector in bytes
NOTHING	equ	0

SIB:	EQU	2048/secsize	;SECTORS IN MEMORY DRIVE DATA BLOCK
DSM:	EQU	((512-4)*SPT)/SIB
;
MDRIVE:	JMP		HOME		;HOME
		JMP		SELDSK		;SELECT DISK
		JMP		SETTRK		;SET TRACK
		JMP		SETSEC		;SET SECTOR
		JMP		SETDMA		;SET DMA
		JMP		READ		;READ
		JMP		WRITE		;WRITE
		JMP		SECTRN		;SECT0R TRANSLATE
;
; Following filled in by installer.
BIOSHOM:JMP		NOTHING		;HOME
BIOSSEL:JMP		NOTHING		;SELECT DISK
BIOSTRK:JMP		NOTHING		;SET TRACK
BIOSSEC:JMP		NOTHING		;SET SECTOR
BIOSDMA:JMP		NOTHING		;SET DMA
BIOSRED:JMP		NOTHING		;READ
BIOSWRT:JMP		NOTHING		;WRITE
BIOSTRN:JMP		NOTHING		;SECT0R TRANSLATE


;
;	Driver variables used (set) by loader.
;
DRIVE:	DW		MDISKL		;Read by loader. DO NOT CHANGE VALUE OR LOCATION
							;Filled in by loader with DRIVE (MEMDRV-'A') code
							;Filled in by loader with M-DRIVE/H TYPE flag

;
;	Disk Parameter Block
;
MD$DPB:						;SET FOR 1 BOARD, 8 SPT
		DW	SPT				;SPT (See NOTE)
		DB	4,15			;BSH, BLH
		DB	1				;EXM (See NOTE)
		DW	DSM-1			;DSM (See NOTE)
		DW	128-1			;DRM
		DB	0C0h,00h		;AL0, AL1 (RESERVED DIR BLOCKS, MUST MATCH ...
							; ... NUMBER OF DIRECTORY ENTRIES)
		DW	0				;CKS (CHECK SUM), This is always zero.
		DW	4				;OFF (NUMBER OF RESERVED TRAGKS FOR PARITY CHECK
							;	THIS IS ALWAYS 4)
;
; NOTE: SPT, EXM, DSM --- WILL BE CHANGED FOR MORE BOARDS BY LOADER.

SELFLG:	DB		0			;M-DRIVE/H selected flag. DO NOT CHANGE LOCATION
							;0 = Not selected, -1 Selected
;
;	16 byte Disk Parameter Header
;
MD$DPH:	DW	0				;SECT0R TRANSLATE TABLE (NONE)
		DW	0,0,0			;SCRATCH AREA
		DW	BUFDIR			;DIRECT0RY BUFFER
		DW	MD$DPB			;DPB BLOCK
		DW	0				;CHECKSUM VECTOR
		DW	ALV				;ALL0CATI0N VECTOR

;
;	S E L E C T   D I S K   D RI V E

;	Select the disk drive for subsequent disk transfers and return
;	the address of the DPH for the memory drive.
;	ENTRY -- <C> = disk selection value.
;
;	EXIT -- <HL> = DPH address of the memory drive.
;	        If memory drive not selected, control is passed to BIOS
;			Return <HL> = 0 if invalid drive selected.

SELDSK:	LXI		H,SELFLG	;<HL> = Selected flag
		MVI		M,0			;M=0, SET MEMORY DRIVE AS NOT SELECTED
		LDA		DRIVE		;<A>=DRIVE code
		CMP		C			;IS MEMORY DRIVE SELECTED?
		JNZ		BIOSSEL		;IF NOT GO BACK TO BIOS
		DCR		M			;M=-1, SET MEMORY DRIVE AS SELECTED
		INX		H			;<HL> = DPH ADDRESS
		RET					;AND DONE
;
;	H O M E
;	Set current track to 0

HOME:	LXI		H,0
		SHLD	TRACK		;SET TRACK AS 0
		JMP		BIOSHOM		;ON TO BIOS
;
;	S  E  T   T R A C K
;
;	Set track number.  The track number 1s save for later use during
;	a disk transfer.  The track is save whether or not the memory disk
;	is currently selected.
;
;	ENTRY -- <BC> = Track number, 0 to 65535.

SETTRK:	MOV		L,C
		MOV		H,B
		SHLD	TRACK
		JMP		BIOSTRK		;ON TO BIOS
;
;	S E T	S E C T 0 R
;
;	Set	sector number.  The sector number is save for later use during
;	a disk transfer.  The sector is saved whether or not the memory
;	disk is current1y selected.
;
;	ENTRY -- <BC> = Sector number, 0 to 255.

SETSEC:	MOV		A,C
		STA		SECTOR		;Set sector
		JMP		BIOSSEC		;ON TO BIOS
;
;	S E T   D M A   A D D R E S S
;
;	Set the address for the next DMA transfer.
;	The DMA address is saved for later use during a disk transfer.
;	The address is saved whether or not the memory disk ia
;	currently selected.
;
;	ENTRY -- <BC> = Disk memory address.

SETDMA:	MOV		L,C
		MOV		H,B
		SHLD	DMAADR		;Set DMA
		JMP		BIOSDMA		;ON TO BIOS
;
;	S  E  C  T O R   T R A N S L A T E
;
;	Translate the sector number from logical to physical.
;
;	ENTRY	<DE> = 0, no translatlon required.
;           <DE> = Translation table address.
;           <BC> = Number to translate.
;
;	EXIT	<HL> = Physical sector number.

SECTRN:	MOV		H,B
		MOV		L,C			;HL=Physical sector number
		MOV		A,D			;DE=Translation table address
		ORA		E			;Are we using translation?
		RZ					;No, All done
		JMP		BIOSTRN		;Yes, Let BIOS translate

;
;	R E A D
;
;	Read the currently selected track,sector from the currently
;	selected drive.

READ:	LDA		SELFLG		;SELFLG= -1 if selected
		ANA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSRED		;ON TO BIOS
		JMP		MDFNL		;A= -1 for READ
;
;	W R I T E
;
;	Write to the current1y selected drive at the current track
;	and sector.

WRITE:	LDA		SELFLG		;SELFLG= -1 if selected
		ANA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSWRT		;ON TO BIOS
		XRA		A			;A= 0 for WRITE

;	Fall into MDFNL
;
;	MDFNL - Hard memory disk final command processing
;	ENTRY: <A> = Command. -1 = READ, 0 = WRITE

MDFNL:	MOV		B,A			;Save command
		LHLD	TRACK		;GET TRACK
		MOV		D,H			;
		MOV		E,L			;Save untranslated TRACK
		DAD		H			;*2
		DAD		H			;*4
		DAD		H			;*8
		DAD		H			;*l6
		DAD		H			;*32
		DAD		H			;*64
		DAD		H			;*128
		CALL	SETMDRV		;SET M-DRIVE/H BOARD VIA <HL>
		LHLD	DMAADR		;DIRECTLY TO CPM BUFFER
		MVI		C,secsize	;<C> = 128 BYTE'S PER SECTOR
		INR		B			;<B>=0 READ [Z=1], 1 WRITE [Z=0]
		MOV		B,C			;Set <B> for parity check
		JNZ		M$WRITE		;IF WRITE COMMAND JUMP

;		Fall into M$READ
;
;	GET A 128 BYTE BLOCK FROM M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C> IS NUMBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO PUT DATA
;
;	EXIT -- <A> = 0 No errors, > 0 READ error.

M$READ:	IN		HDATA		;GET DATA BYTE
		MOV		M,A			;PUT TO DMA BLOCK
		INX		H			;UP POINTER
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		DCR		C			;C0UNT THIS MOVE
		JNZ		M$READ		;AND DO AGAIN
		XCHG				;<HL> = untranslated TRACK
		CALL	SETMDRV		;SELECT PARITY BYTE LOCATION VIA <HL>
		IN		HDATA		;GET PARITY BYTE
		SUB		B			; 0 IF NO ERROR
		RET					;	THEN RETURN
;
;	PUT A BLOCK OF DATA INTO M-DRIVE/H BOARD
;	ADDRESS IS ALREADY SELECTED.
;	ENTRY -- <C>  IS NUNBER OF BYTES TO MOVE
;			 <HL> IS DMA BUFFER TO GET DATA FROM
;
;	EXIT -- <A> = 0 No errors.

M$WRITE:MOV		A,M			;GET BYTE FROH BUFFER
		INX		H			;UP POINTER
		OUT		HDATA		;TO MDRIVE BOARD
		ADD		B			;ADD UP PARITY BYTE
		MOV		B,A			; AND SAVE
		DCR		C			;COUNT THIS MOVE
		JNZ		M$WRITE		;AND DO AGAIN
		XCHG				;<HL> = untranslated TRACK
		CALL	SETMDRV		;SELECT PARITY BYTE LOCATION
		MOV		A,B
		OUT		HDATA		;WRITE PARITY BYTE TO HEMORY DISK
		XRA		A			;AND SHOW NO ERROR
		RET					;RETURN
;
;	Set	M-DRIVE/H board to address in <HL>
;	The combined values of <AHL> is a 24 bit number
;	which is three more bits for a 2 Mbyte card, and
;	five more bits than a 512 Kbyte card.

SETMDRV:LDA		SECTOR		;Get sector 0-31 (0x1F)
		OUT		HADDR		;<A>: to M-DRIVE/H board
		MOV		A,H			;<H>: is always 0 or 1
		OUT		HADDR
		MOV		A,L			;<L>: Upper bit's picks track 0-3
		OUT		HADDR		; remainder picks sector and byte
		RET

MDISKL:	EQU		$-MDRIVE	;LENGTH OF MDRIVE BIOS
;	End of code

;
;	Flags and Buffers
;

SECTOR:	DS		1			;SECTOR: 0-31
TRACK:	DS		2			;TRACK:  0-65535
DMAADR:	DS		2			;ADDRESS OF DMA BUFFER
BUFDIR:	DS		128			;DIRECTORY BUFFER
ALV:	DS		(((DSM*SPT)+7)/SPT)+1

	END
