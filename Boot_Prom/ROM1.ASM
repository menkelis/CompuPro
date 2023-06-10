; LAST UPDATED:		14 AUGUST 84
; REASON FOR UPDATE:	The boot ROM will now load all of track 0, head 0,
;		26 single density 128 byte sector format sectors, if a floppy
;		disk is present.  If not, it will attempt to boot from a hard
;		disk drive, loading in the first 9 1024-byte sectors,
;		i.e. one full track.				        rrs
;		
;
; PROGRAM NAME:	PROM1A1.ASM.
;
; PURPOSE:	1st routine for Disk 1A boot rom:  8 inch Floppy Disk or Hard
;		Disk initial program loader (IPL), which consequently will load
;		the remainder of the operating system upon execution entry.
;
;	==========================	Copyright 1984, CompuPro Corporation,
;	|| 8-INCH FLOPPY OR	||	A division of Godbout Electronics.
;	||  HARD DISK PROM	||	3506 Breakwater Court
;	||   POWER UP LOADER	||      Hayward, CA  94545
;	||			||
;	==========================
;
;	This product is a copyright program product of CompuPro and is
;	supplied for use with the CompuPro Disk controllers.
;
; Disk layout Definition.
;
;  Cylinder 0,  Head 0,  8 inch disk = 26 X 128 byte single density sectors.
;
;	The code is loaded starting XBOOT and may be
; any type of secondary loader or the runtime operating system itself.
;
;
; LIBRARY CONSTANTS:
	MACLIB	COMPUPRO	;Disk and Serial/Parallel interface constants
	MACLIB	ACTIVE		;Flags directing construction for the various
;
; EQUATED CONSTANTS:
;
ENTRY	EQU	100h			;Org address (= 0, for ROM)
HIRAM	EQU	8000h			;RAM offset of ROM so loader doesn't overlay 2nd page
RBOOT	EQU	ENTRY+200h+HIRAM	;end of PROM in RAM
XBOOT	EQU	100h			;Base Location of second phase loader in RAM
MAP	EQU	XBOOT+400h		;allocation map address for bad DISK3 sectors
; Drive specific constants. (Set to handle nearly any drive)
SRT	EQU	16 - 10		;10 millisecond step rate for controller value
HUT	EQU	240 / 16	;Head unload delay time in milliseconds (240)
HDLT	EQU	(35 + 1) / 2	;Head load time in milliseconds (35)
;
; controller port for Disk 1A at 0C0h
FDPORT	EQU	0C0h
NS$128	EQU	26	; 128 byte sectors / track	-- 3.25K
NS$256	EQU	26	; 256 byte sectors / track	-- 6.5K
NS$512	EQU	15	; 512 byte sectors / track	-- 7.5K
NS1024	EQU	 8	;1024 byte sectors / track	-- 8K bytes total
;
;	DISK3 equates follow:
D3PORT	EQU 	90h	; DISK3 IO port
D3NOP	EQU	0	; DISK3 no-op command
D3RDFL	EQU 	1	; DISK3 read flag
D3GLBAL	EQU	2	; DISK3 global command
D3SPEC	EQU	3	; DISK3 specify command
D3MAP	EQU	4	; DISK3 bad map command
D3HOME	EQU	5	; DISK3 home command
D3RWCMD	EQU	8	; DISK3 read/write command
;
;	ROM Bootstrap loader.

;
    ASEG
	ORG	ENTRY		;Base address of bootstrap PROM
	NOP! NOP! NOP! NOP	;8080 power up delay opcodes
INIT:	MVI	A,0FFh		;Turn on the disk motors, leave PROM active
	OUT	FDPORT+FDON
;
; Loop on total error to this point.
	LXI	B,4000h		;Init delay count
DELAY1	EQU $-ENTRY
	XTHL!	XTHL		;Harmless time consuming instructions
	DCX B!	MOV A,B! ORA C	;Bump count, test if done
	JNZ	DELAY1		;Loop until the time passes
; read ROM and write to RAM
ROMOVE:	LXI	H,RBOOT-HIRAM-ENTRY	;point to end of ROM
	LXI	D,RBOOT-ENTRY		;point to end of RAM
ROMOV1	EQU	$-ENTRY
	MOV	A,M		;read ROM byte
	STAX	D		;write to RAM
	DCX	D		;point to next byte
	DCX H!	MOV A,L! ORA H	;bump count (also pointer)
	JNZ ROMOV1		;loop if more bytes left to move
;
; load DISK3 IOPB address vector with first IOPB address
	LXI	D,5Dh		;DISK3 IOPB address vector
	MVI	A,low IOPB1	;LOB of first IOPB
	STAX	D		;write to vector
	INX	D		;point to next byte
	MVI	A,high IOPB1	;HOB of first IOPB
	STAX	D		;write to vector
	INX	D		;point to next byte
	MVI	A,0		;extended address offset
	STAX	D		;write to vector
	JMP	START		;jump to code located in high RAM
;
DELAY	EQU	$+HIRAM-ENTRY	; delay subroutine
	LXI	B,0C000h	;Init delay count
DELAY2	EQU	$+HIRAM-ENTRY
	XTHL!	XTHL		;Let command settle
	DCX B!	MOV A,B! ORA C	;Bump count, test if done
	JNZ	DELAY2		;Loop until the time passes
	RET
;
START	EQU	$+HIRAM-ENTRY
	MVI	A,0FEh		;disable ROM, enable RAM
	OUT	FDPORT+FDON
; Load Specify Command.
	LXI	D,SPEC		;Point to specify command sequence
	MVI	B,LSPEC		;Length in "B"
SPEC1	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;See if ready to accept next byte
	ORA	A
	JP	SPEC1		;Wait if not ready
	LDAX	D		;Load command byte
	OUT 	FDPORT+FDCD	;to controller command port
	INX	D		;Point to next byte
	DCR	B		;Bump count until all loaded
	JNZ	SPEC1		;Loop if more to do
;
; Recalibrate drive.
	MVI	B,LRECAL	;Length of command in "B"
RCAL1	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;See if ready to accept next byte
	ORA	A
	JP	RCAL1		;Wait if not ready
	LDAX	D		;Load command byte
	OUT 	FDPORT+FDCD	;to controller command port
	INX	D		;Point to next byte
	DCR	B		;Bump count until all loaded
	JNZ	RCAL1		;Loop if more to command load
;
RCAL2	EQU	$+HIRAM-ENTRY
	IN	FDPORT+INTS	;See if command execution completed
	ORA	A
	JP	RCAL2		;Wait if not
;
; Verify successful completion of recalibrate command.
	MVI	A,FD$RSTS	;"Read Status" command
	OUT	FDPORT+FDCD	; to controller
	CALL	DELAY		; blink drive light
RCAL3	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;See if command accepted
	ORA	A
	JP	RCAL3		;Loop until ready
	IN	FDPORT+FDCD	;Get first resulting "Status Byte"
	XRI	20h		;Flip "drive ready" status bit
	MOV	C,A		;Put result in "C"
RCAL4	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;Get command execution status again
	ORA	A
	JP	RCAL4		;See if seek complete
	IN	FDPORT+FDCD	;Get second "execution" result byte
	ORA	C		;Combine the two result status bytes
	JNZ	D3BOOT		;Loop to DISK3 boot if error
;
; Execute read operation sequence until loaded successfully.
	MVI	B,LDMA		;Length of DMA data
;
; Output beginning DMA address.
ADDR	EQU	$+HIRAM-ENTRY
	LDAX	D		;Get byte of extended address
	OUT	FDPORT+FDMA	;Send to DMA port of DISK 1
	INX	D		;Next byte to xfer
	DCR	B		;Bump count
	JNZ	ADDR		;Loop until all 3 bytes loaded
;
; Read all the data on track 0 in to get the cold boot loader.
	MVI	B,LREAD		;Load "B" with command lenght
READ1	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;Get controller status
	ORA	A
	JP	READ1		;Wait until controller ready for another byte
	LDAX	D		;load command byte
	OUT 	FDPORT+FDCD	; to controller
	INX	D		;Point to next byte to load
	DCR	B		;Bump command load count
	JNZ	READ1		;Loop if more bytes to load
;
READ2	EQU	$+HIRAM-ENTRY
	IN	FDPORT+INTS	;See if interrupt active (command complete)
	ORA	A
	JP	READ2		;Loop until so
;
READ3	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;See if ready to read status
	ORA	A
	JP	READ3		;Wait if not
	IN	FDPORT+FDCD	;Get resulting status of read operation
	SUI	40h		;Remove "abnormal ending" status bit
	MOV	H,A		;Put result in "E"
READ4	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;Get second status byte
	ORA	A
	JP	READ4		;Loop until ready
	IN	FDPORT+FDCD	;Second status byte in "A"
	XRI	FD$EOC		;Remove "End of Cylinder" bit
	MOV	L,A		;Put result in "D"
;
	MVI	B,7-2		;Count of remaining status bytes (ignored)
READ5	EQU	$+HIRAM-ENTRY
	IN	FDPORT+FDCS	;See if controller ready
	ORA	A
	JP	READ5		;Loop if not
	IN	FDPORT+FDCD	;Read status byte, ignore it
	DCR	B		;Bump remaining count
	JNZ	READ5		;Wait until all done
; If "Abnormal Ending" caused by "End of Cylinder" error, then read is valid.
	MOV A,L!  ORA	H	;Combine the two significant status bytes
	JZ	BOOTVEC		;Exit from boot PROM if successful
				;otherwise, fall through to DISK3 boot
D3BOOT	EQU	$+HIRAM-ENTRY
	MVI	A,1		;DISK3 board reset byte
	OUT	D3PORT		;send it
	XRA	A		;ATTENTION byte
	OUT	D3PORT		;send it
	CALL	DELAY		; turn off disk drive light
	LDA 	IOPB1S		;get STATUS
	ORA	A		;check if DISK3 present
	JM	DOHBT		;successful no-op, continue with hard boot
	JMP	START		;otherwise error, try floppy again
; execute DISK3 command subroutine
EXECUT	EQU	$+HIRAM-ENTRY
	XRA	A		;null byte
	OUT	D3PORT		;nudge DISK3
XX1	EQU	$+HIRAM-ENTRY
	LDAX	D		;get STATUS
	ORA	A		;check if busy
	JZ	XX1		;if so, try again
	RLC			;return if
	RM			;no error
	POP	B		;otherwise, fix stack
	JMP	START		;and try floppy again
;
DOHBT	EQU	$+HIRAM-ENTRY
	LXI 	D,IOPB2S	;point to global status byte
	CALL 	EXECUT		;do global command
;
	LXI	D,IOPB3S	;point to home status byte
	CALL	EXECUT		;do home head command
;
	LXI	D,IOPB4S	;point to read marker status byte
	LHLD	IOPB4+10	;point to data address for format check
	CALL	EXECUT		;do read command
;
	LXI	D,COMP		;point to 'COMPUPRO'
	MVI	B,4		;compare 4 characters
TEST	EQU	$+HIRAM-ENTRY
	LDAX	D		;'COMP' character in A
	CMP	M		;compare header character
	JNZ	START		;if not equal, then DISK3 not formatted
	INX	H		;point to next header byte
	INX	D		;point to next 'COMPUPRO' byte
	DCR 	B		;bump count
	JNZ	TEST		;more characters?
;
	LXI	D,IOPB5S	;point to specify status byte
	CALL	EXECUT		;do specify parameter command
;
	LXI	D,IOPB6S	;point to bad map status byte
	CALL	EXECUT		;do bad map command
;
	LXI	D,IOPB7S	;point to read sectors status byte
	LHLD	IOPB7+10	;point to data address for loader check
	CALL	EXECUT		;do read command
;
	MOV	A,M		;possible loader byte
	CPI	0E5h		;is loader there, or merely formatted?
	JZ	START		;no--try floppy again
;
; successful hard boot!
;
;************************************************
;*	EXIT ON SUCCESSFUL READ OPERATION	*
;************************************************
;
BOOTVEC	EQU	$+HIRAM-ENTRY
	IN	0C2h		;sense switch value
				;if bit 4 on then Interfacer 3/4
				;if bit 4 off then SS
	RRC!	RRC		;divide by 4
	ANI	1		;mask 4
	ORI	2		;add 2 for boot switch value
	MOV	C,A		;Boot switch value passed to loader
	JMP	XBOOT		;second phase loader
;	org	XBOOT		;Cold boot code starts in RAM here
	DB	'RR1'
;
	PAGE
;************************************************
;*	FIXED STORAGE FOR DISK PARAMETERS	*
;************************************************
	ORG	($ and 0FFF0h) + 16	;Next Segment boundary
;
SPEC	EQU	$+HIRAM-ENTRY
	DB	FD$SPEC		;Floppy disk specification command
	DB	0DFh
	DB	HDLT SHL 1
LSPEC	EQU	HIRAM+$-ENTRY-SPEC	;Length of specification sequence
;
RECAL	EQU	$+HIRAM-ENTRY
	DB	FD$RECA,0	;Recalibrate command (home to track 0)
LRECAL	EQU	HIRAM+$-ENTRY-RECAL	;Command length
;
; Function data for controller to boot
DATA	EQU	$+HIRAM-ENTRY
	DB	0		;Extended Address
	DB	high XBOOT	;Base address of BOOT loader
	DB	low XBOOT
LDMA	EQU	HIRAM+$-ENTRY-DATA	;Length of DMA to load
;
; Try to read disk as single density 128 byte sectors.
READ	EQU	$+HIRAM-ENTRY
	DB	FD$RDAT		;Read sector(s) command for 8272 controller
	DB	0		;Head select, Drive select = 0
	DB	0		;Cylinder #0
	DB	0		;Head #0
	DB	1		;Starting Record (sector)
	DB	0		;"N" parameter (128 byte sectors)
	DB	NS$128		;Read to end of track = 3.25K bytes
	DB	07h		;GPL (Gap length)
	DB	128		;DTL (Data length)
LREAD	EQU	HIRAM+$-ENTRY-READ	;Length of parameter block to xmit
;
COMP	EQU	$+HIRAM-ENTRY
	DB	'Comp'
;
IOPB1	EQU	$+HIRAM-ENTRY
	DB	D3NOP		;DISK3 present check
IOPB1S	EQU	$+HIRAM-ENTRY
	DB	0,0,0,0,0,0,0,0,0,0,0,0,low IOPB2,high IOPB2,0
IOPB2	EQU	$+HIRAM-ENTRY
	DB	D3GLBAL		;global information
IOPB2S	EQU	$+HIRAM-ENTRY
	DB	0,0,0,8,1,0,0,0,0,0,0,0,low IOPB3,high IOPB3,0
IOPB3	EQU	$+HIRAM-ENTRY
	DB	D3HOME		;home head
IOPB3S	EQU	$+HIRAM-ENTRY
	DB	0,0,0,0,0,0,0,0,0,0,0,0,low IOPB4,high IOPB4,0
IOPB4	EQU	$+HIRAM-ENTRY
	DB	D3RWCMD		;read marker into XBOOT
IOPB4S	EQU	$+HIRAM-ENTRY
	DB	0,0,D3RDFL,0,0,0,0,2,0,low XBOOT,high XBOOT
	DB	0,low IOPB5,high IOPB5,0
IOPB5	EQU	$+HIRAM-ENTRY
	DB	D3SPEC		;specify parameters
IOPB5S	EQU	$+HIRAM-ENTRY
 	DB	0,0,0,0,0,0,0,0,0,low XBOOT+10h,high XBOOT
	DB	0,low IOPB6,high IOPB6,0
IOPB6	EQU	$+HIRAM-ENTRY
	DB	D3MAP		;allocation map for bad sectors
IOPB6S	EQU	$+HIRAM-ENTRY
	DB	0,0,0,0,0,0,0,0,0,low MAP,high MAP
	DB	0,low IOPB7,high IOPB7,0
IOPB7	EQU	$+HIRAM-ENTRY
	DB	D3RWCMD		;read 9 1024-B sectors into XBOOT
IOPB7S	EQU	$+HIRAM-ENTRY
	DB	0,0,D3RDFL,0,0,0,0,9,0,low XBOOT,high XBOOT,0,50h,0,0
;
	END
