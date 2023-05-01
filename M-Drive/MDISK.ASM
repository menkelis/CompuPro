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


;	ORG	0000h
	ORG	0100h

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
;	written by Mike Douglas 2017

HBASE	EQU	0C6h
HDATA	EQU	HBASE
HADDR	EQU	HBASE+1

MEMDRV	EQU	'M'				;SET AS DRIVE "M"
MRTRY	EQU	10				;MAX RETRY COUNT
BUFDIR	EQU	80H				;DIRECTORY BUFFER
spt		equ	8				;Sectors per track
secsize	equ	128				;Size of sector in bytes
NOTHING	equ	0

SIB:	EQU	2048/secsize	;SECTORS IN MEMORY DRIVE DATA BLOCK
DSM:	EQU	((512-4)*spt)/SIB
;
MDRIVE:	JMP		HOME		;HOME
		JMP		SELDSK		;SELECT DISK
		JMP		SETTRK		;SET TRACK
		JMP		SETSEC		;SET SECTOR
		JMP		SETDMA		;SET DMA
		JMP		READ		;READ
		JMP		WRITE		;WRITE
		JMP		SECTRN		;SECT0R TRANSLATE

BIOSHOM:JMP		NOTHING		;HOME
BIOSSEL:JMP		NOTHING		;SELECT DISK
BIOSTRK:JMP		NOTHING		;SET TRACK
BIOSSEC:JMP		NOTHING		;SET SECTOR
BIOSDMA:JMP		NOTHING		;SET DMA
BIOSRED:JMP		NOTHING		;READ
BIOSWRT:JMP		NOTHING		;WRITE
BIOSTRN:JMP		NOTHING		;SECT0R TRANSLATE

DRIVE:	DW		MDISKL		;Filled in by loader with MEMDRV-'A', M-DRIVE/H TYPE
							;DO NOT CHANGE VALUE OR LOCATION
;
;	Disk Parameter Block
;
MD$DPB:						;SET FOR 1 BOARD, 8 SPT
		DW	spt				;SPT* (SECTORS PER TRACK, THIS AND DSM ...
							;...MUST BE INCREASED FOR MORE BOARDS)
		DB	4,15,1			;BSH, BLH, EXM
		DW	DSM-1			;DSM*
		DW	128-1			;DRM
		DB	0C0h,00h		;AL0, AL1 (RESERVED DIR BLOCKS, MUST MATCH ...
							; ... NUMBER OF DIRECTORY ENTRIES)
		DW	0				;CKS (CHECK SUM), This is always zero.
		DW	4				;OFF (NUMBER OF RESERVED TRAGKS FOR PARITY CHECK
							;	THIS IS ALWAYS 4)
;
;	Disk Parameter Header
;
MD$DPH:
		DW	0				;SECT0R TRANSLATE TABLE
		DW	0,0,0			;SCRATCH
		DW	BUFDIR			;DIRECT0RY BUFFER
		DW	MD$DPB			;DPB BLOCK
		DW	0				;CHECKSUM VECTOR
		DW	ALV				;ALL0CATI0N VECTOR
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
SELDSK:	LXI		H,CIOPB+0
		XRA		A			;CLEAR A
		MOV		M,A			;CLEAR MEMORY DRIVE FLAG
		LDA		DRIVE		;
		SUB		C			;IS MEMORY DRIVE SELECTED
		JNZ		BIOSSEL		;IF NOT GO BACK TO BIOS
		CMA					;A=-1
		MOV		M,A			;SET MEMORY DRIVE AS SELECTED
		LXI		H,MD$DPH	;GET DPH ADDRESS
		RET					;AND DONE
;
;	H O M E
;	Set current track to 0
;
HOME:	LXI		H,0
		SHLD	CIOPB+2		;SET TRACK AS 0
		JMP		BIOSHOM		;ON TO BIOS
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
		JMP		BIOSTRK		;ON TO BIOS
;
;	S E T	S E C T 0 R
;
;	Set	sector number.  The sector number is save for later use during
;	a disk transfer.  The sector is saved whether or not the memory
;	disk 1s current1y selected.
;
;	ENTRY -- BC = Sector number.

SETSEC:	MOV		A,C
		STA		CIOPB+4		;Set sector
		JMP		BIOSSEC		;ON TO BIOS
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
		SHLD	DMAADR		;Set DMA
		JMP		BIOSDMA		;ON TO BIOS
;
;	S  E  C  T O R   T R A N S L A T E
;
;	Translate the sector number from logical to physical.
;
;	ENTRY	DE = 0, no translatlon required.
;           DE = Translation table address.
;           BC = Number to translate.
;
;	EXIT	HL = Physical sector number.
;
SECTRN:	MOV		A,D			;DE=Translation table address
		ORA		E			;Are we using translation?
		JNZ		BIOSTRN		;Yes, Let BIOS translate
		MOV		H,B			;No translation required
		MOV		L,C			;HL=Physical sector number
		RET					;And done
;
;	R E A D
;
;	Read the currently selected track,sector from the currently
;	selected drive.
;
READ:	LDA		CIOPB+0		;SELFLG
		ORA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSRED		;ON TO BIOS
		CMA					;A=0 for READ
		STA		CIOPB+1		;TO COMMAND BUFFER
		JMP		MDFNL		;AND GO PROCESS
;
;	W R I T E
;
;	Write to the current1y selected drive at the current track
;	and sector.
;
WRITE:	LDA		CIOPB+0		;SELFLG
		ORA		A			;IS MEMORY DRIVE SELECTED
		JZ		BIOSWRT		;ON TO BIOS
		STA		CIOPB+1		;A=-1 for WRITE
;	Fall into MDFNL
;
;	MDFNL - Hard memory disk final command processing
;	ENTRY --
;			 CIOPB+0 = SELFLG
;			 CIOPB+1 = COMMAND
;			 CIOPB+2 = TRACK
;			 CIOPB+4 = SECTOR
;			 CIOPB+5 = MRETRY
;
MDFNL:	MVI		A,MRTRY		;Set retry count
MDFNL1:	STA		CIOPB+5
		LHLD	CIOPB+2		;GET TRACK
		DAD		H			;*2
		DAD		H			;*4
		DAD		H			;*8
		DAD		H			;*l6
		DAD		H			;*32
		DAD		H			;*64
		DAD		H			;*128
		CALL	SETMDRV		;SET HDRIVE BOARD VIA <A>,<HL>
		MVI		C,secsize	;<C> = 128 BYTE'S PER SECTOR
		MOV		B,C			; also into <B> for parity check
		LHLD	DMAADR		;DIRECTLY TO CPM BUFFER
		LDA		CIOPB+1		;GET COMMAND
		ANA		A			;A=0 for READ or -1 for WRITE
		JNZ		M$WRITE		;IF WRITE COMMAND JUMP
;
;		Fall into M$READ
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
		LDA		CIOPB+5		;GET RETRY COUNT
		DCR		A			;REPLACE WITH NEW COUNT
		JNZ		MDFNL1		;
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
PARITY:	LHLD	CIOPB+2		;Get track

;	Set	M-DRIVE/H board to address In <HL>
SETMDRV:
		LDA		CIOPB+4		;Get sector, Starts at 0
		OUT		HADDR		;<A>: to M-DRIVE/H board
		MOV		A,H			;<H>: is always 0 or 1
		OUT		HADDR
		MOV		A,L			;<L>: Upper bit's picks track 0-3
		OUT		HADDR		; remainder picks sector and byte
		RET
;
MDISKL:	EQU		$-MDRIVE	;LENGTH OF MDRIVE BIOS
;
;	Flags and Buffers
;
;	CIOPB+0 = SELFLG
;	CIOPB+1 = COMMAND
;	CIOPB+2 = TRACK
;	CIOPB+4 = SECTOR
;	CIOPB+5 = MRETRY

CIOPB:	DS		8			;COMMAND BUFFER,TRACK,SECTOR
DMAADR:	DS		2			;ADDRESS OF DHA BUFFER
ALV:	DS		(((DSM*spt)+7)/spt)+1

	END
