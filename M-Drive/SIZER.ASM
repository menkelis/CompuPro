PAGE	0

HM$DATA	EQU		0C6H
HM$CNTL	EQU		0C7H
FD$RDAT	EQU		0FFH

FALSE	EQU	0
TRUE	EQU	NOT FALSE
BOOT	EQU	0
BDOS	EQU	5				;BDOS entry point
TPA		EQU	100H
FCB		EQU	5CH				;file control block
FCB1	EQU	5CH				;first file name
FCB2	EQU	6CH				;second file name
DBUFF	EQU	80H				;default buffer
ABUFF	EQU	DBUFF			;actual buffer
UNUSED	EQU	0E5H			;dir entry
LMAX	EQU	24				;max lines/screen

;Set flags in main program so only one
;copy of certain subroutines will be generated.
;Place set lines before MACLIB call.

BNFLAG	SET FALSE		;binary to ASCII bin
COFLAG	SET FALSE		;output console char
CRFLAG	SET FALSE		;carr-ret/line-feed
CXFLAG	SET FALSE		;binary to hex
DEFLAG	SET FALSE		;binary to decimal
DVFLAG	SET FALSE		; 16-bit divide
MLFLAG	SET FALSE		; 16-bit multiply in HL
MVFLAG	SET FALSE		;block move
PRFLAG	SET FALSE		;print console
;end of flags

MACLIB	CPMMAC

		ORG		TPA

		CALL	HMDSIZE
		PUSH	PSW
		LXI		D,HM$MSG
		CALL	MYPRINT
		POP		PSW
		ANA		A			;Test <A> = 0
		RNZ					;No boards found
		CALL	GETDPB
		CALL	XAMINE
		RET
;
;block move disk parameters to end of program
;
GETDPB:
		LXI		H,SPT
		MOVE	,DPARM,15	;copy to end
		LHLD	BLKMAX		;maximum # blocks
		INX		H
		SHLD	BLKMAX		;starts at zero
		LHLD	DIRENT		;# of directory entries
		INX		H			;starts at zero
		DIVIDE	4			;convert to # sectors
;Save number of directory sectors as 16 bits
		SHLD	DIRMAX		;and save
		SHLD	DIRMX2		;count
;
;Directory block allocation is stored as
;1000 0000 for 1 block , 1100 0CXX) for 2, etc.
;But we want left byte in H.
;
		LHLD	ALLOC		;reverse bytes
		MOV		A,L
		MOV		L,H
		MOV		H,A
		SHLD	ALLOC
;Get number of directory blocks as ASCII
		XRA		A			;zero A
XAM3:	DAD		H			;shift left
		JNC		XAM4
		INR		A
		JMP		XAM3
XAM4:	MOV		E,A			;# dir blocks
		MVI		D,0
		LHLD	BLKMAX		;blocks
		SBC		HL,DE		;deduct for directory
		SHLD	NETBL		;net data blocks
		MOV		A,E
		ORI		'0'			;ASCII bias
		STA		ALLOCA		;save
		RET
;
;Display disk parameters
;
XAMINE:	PRINT	<CR,LF,'Sectors/track        :'>
		LHLD	NUMSEC
		HLDEC				;decimal
		PCHAR	BLANK
		PCHAR	'('
		OUTHL
		PRINT	<' hex)',CR,LF,'Sectors/block        :'>
		LDA		BLM
		INR		A
		MOV		L,A
		MVI		H,0
		HLDEC
		PCHAR	BLANK
		PCHAR	'('
		OUTHEX	L
		PRINT	' hex)'
		PRINT	<CR,LF,'Block size           :'>
		DIVIDE	8
		MOV		B,L			;save block size
		HLDEC
		PRINT	'K bytes'
		LHLD	NETBL		;# data blocks
		MOV		A,B			;block size
		MULT
		PRINT	<CR,LF,'Disk size            :'>
		HLDEC
		PRINT	'K bytes'
		PRINT	<CR,LF,'Extents/entry        :'>
		LDA		EMASK
		INR		A
		MOV		L,A
		MVI		H,0
		HLDEC
		PRINT	<CR,LF,'Number of blocks     :'>
		LHLD	BLKMAX
		HLDEC
		PCHAR	BLANK
		PCHAR	'('
		OUTHL
		PRINT	<' hex)',CR,LF,'Max directory entries:'>
		LHLD	DIRENT
		INX		H
		HLDEC
		PCHAR	BLANK
		PCHAR	'('
		OUTHL
		PRINT	<' hex)',CR,LF,'Directory blocks     :'>
		LDA		ALLOCA
		PCHAR
		PCHAR	BLANK
		PCHAR	'('
		LDA		ALLOC+1
		BINBIN				;alloc in binary
		LDA		ALLOC
		ORA		A
		JZ		XAM2
		BINBIN				;2nd if needed
XAM2:	PRINT	<')',CR,LF,'Track offset         :'>
		LHLD	TRKOFF
		HLDEC
		MOV		A,H
		ORA		A
		JZ		XAM5		;skip hex
		PCHAR	BLANK
		PCHAR	'('
		OUTHL
		PRINT	' hex)'
XAM5:	CRLF
		RET


;
; "Size" the M-Drive/H by counting active boards in use.
HMDSIZE:
		LXI		H,-((512-4)/2)	;Init the disk size (blocks 2K)
		LXI		B,30F8h		;"B" = "0", "C" = board count-1
HMDTEST:
		MOV		A,B			;ASCII "0"
		STA		HM$MSG		;Put ASCII board count in Sign-on
		INR		B			;Bump count for next loop
		LXI		D,(512-4)/2	;Size of each board in 2K blocks
		DAD		D			;Add in more blocks for new board
		MOV		A,C			;<C> = board count
		ADI		8			;Bump sector count by 8 (1 board)
		STA		SPT			;Save sector count in Disk Parameter Block
		MOV		C,A			;Update <C>
		CPI		64			;See if maximum count reached
		JZ		HMDEND		;Done if so
		PUSH	H			;Save the current disk size in blocks
		CALL	HMDTRD		;Read a sector into the directory buffer
		XRA		A			;Clear command (for write)
		CALL	HMDTWR		;Write it back out again
		CALL	HMDTRD		;Read back, should now report no errors
		POP		H			;Recover current disk size
		JZ		HMDTEST		;Test for next board if no errors
		MOV		A,C			;Board not found
		CPI		0			;See if no boards present
		JZ		HMDZERO		;If no boards found
		CPI		31			;See if 2 megabytes or more
		JNC		HMD4K		;Set DSM only if so, rest of DPB is set
		CPI		8			;See if only 512K
		JNZ		HMD1 		;If not 512k, don't change EXM
		XRA		A
		STA		EXM			;Set EXM to 0 if only one board
; Set DIRS = 128, Reserving two blocks
HMD1:	XCHG				;Save DSM in "D,E"
		LXI		H,128-1 	;
		SHLD	DIRS		;Set directory count to 128
		MVI 	A,0C0H		;
		STA		AL0			;Set number of reserved blocks to match
		XCHG				;Put DSM back in "H,L"
HMD4K:	DCX		H
		SHLD	DSM			;Save corrected disk size allocation
HMDEND:	XRA		A			;A= 0
		RET
; No M-DRIVE/H boards found
HMDZERO:
		MVI		A,0FFh		;A= -1
		RET
;
; Routine to size the number of boards belonging to an M-Drive/H.

HMDTRD:	MVI		A,FD$RDAT	;Floppy "read data" command works for M-Drive
HMDTWR:	STA		CIOPB		;Store command for Read/Write process
		LXI		H,DIRBUF	;Use the directory buffer area for storage
		PUSH	B			;Save sector, message value
		MOV		A,C			;Sector to test
		LXI		B,4			;Actual track number for parity
		LXI		D,4*128		;"Translated" track number 4 (directory)
		CALL	HMD$FNL		;Perform Read/Write operation
		POP		B			;Recover sector and the ASCII board number
		RET					;Return with parity error status

; ----- For M-DRIVE/H -----
;Entry:	D,E = Translated "Track" in range of (4-511)*128,
;	A = "Sector" in range of 0-"8n-1", where "n" is # of M-DRIVE/H boards.
;	B,C = Actual track
;
;  Rearranged so that "sector" info is actually the high order address byte -
; "D" becomes the low order track address byte, and since "D" may hold
; only a "1" or "0" in it's MSB, use it to identify the "high or low"
; sector being addressed by rotating that bit to the high order position.
;
HMD$FNL:
		PUSH	B			;Save "track" for parity
		PUSH	PSW
		OUT		HM$CNTL		;Set "sector" for parity¬ high "track" byte
		MOV		A,D
		OUT		HM$CNTL		;Low order byte of "track"
		MOV		A,E
		OUT		HM$CNTL		;Set "record" 1 or 0 ID in final byte setup
		LXI		B,8080H
		LDA		CIOPB		;See if read operation requested
		CPI		FD$RDAT
		JNZ		HMD$WRT		;Do write if not
HMD$RD:
		IN		HM$DATA		;Get data byte from "drive"
		MOV		M,A
		INX		H			;In memory, point to next location
		ADD		B
		MOV		B,A			;Accumulate parity byte
		DCR		C			;Until all 128 bytes done
		JNZ		HMD$RD		;If done, point to stored disk parity byte
		POP		PSW
		OUT		HM$CNTL		;Set High order byte of track
		POP		D
		MOV		A,D
		OUT		HM$CNTL		;Set Low order byte of track (single bit)
		MOV		A,E
		OUT		HM$CNTL		;Use old byte of "track" to find single byte
		IN		HM$DATA
		SUB		B			;Get it and see if parity matches
		RET					;Return with error status
;
; Write operation to MDRIVE/H.
HMD$WRT:
		MOV		A,M
		INX		H			;Get byte from memory, point to next
		OUT		HM$DATA		;Xmit to "drive"
		ADD		B
		MOV		B,A			;Accumulate parity byte
		DCR		C			;Until 128 bytes done
		JNZ		HMD$WRT		;If done, point to storage for parity byte
		POP		PSW
		OUT		HM$CNTL		;Set High order byte of track
		POP		D			;Recover selected track
		MOV		A,D
		OUT		HM$CNTL		;Set Low order byte of track (single bit)
		MOV		A,E
		OUT		HM$CNTL
		MOV		A,B
		OUT		HM$DATA		;Save it to memory "drive" reserved area
		XRA		A
		RET					;Show valid write operation
;
; DPB for 8 boards
SPT:	DW		0040H		;SPT
		DB		4,15		;BSH, BLH - set BLS=2,048
EXM:	DB		1			;EXM      - set 0 if DSM>255, 1 if DSM<256
DSM:	DW		03EFH		;DSM-1
DIRS:	DW		512-1		;DIRS-1
AL0:	DB		0FFH,000H	;AL0, AL1
		DW		000H		;CKS
		DW		004H		;OFF

HM$MSG:	DB		'0'
		DB		' M-Drive/H$' ;Show active M-DRIVE/H boards
CIOPB:	DS		1

;
; Print termenited message pointed by DE
;
myprint:
		push	psw
		push	b
		push	d
		push	h
		mvi		c,009H		;BDOS function PRINT$STRING
		call	bdos
		pop		h
		pop		d
		pop		b
		pop		psw
		ret

DPARM:			;copy of disk parameters
NUMSEC:	DS	2	;sectors per track
BSHIFT:	DS	1	;block shift
BLM:	DS	1	;block mask
EMASK:	DS	1	;extent mask
BLKMAX:	DS	2	;max # blocks on disk
DIRENT:	DS	2	;max # dir entries
ALLOC:	DS	2	;AL 1, ALO reversed
CKS:	DS	2	;check size
TRKOFF:	DS	2	;track offset
PSH:	DS	1	;physical record shift factor
PSM:	DS	1	;physical record mask
;
DIRMAX:	DS	2	;max # directory sectors
NETBL:	DS	2	;number of data blocks
ALLOCA:	DS	1	;directory blocks (ASCII)
DIRMX2:	DS	2	;remaining dir sectors

DIRBUF	DS	80H

	END
