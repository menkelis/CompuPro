; G-MDRIVE
; V1.0 2023 Michael Enkelis
; Some code was derived from MAKEMOV written by Mike Douglas 

;
; Create MDRIVE.COM

; ASM L-MDRIVE
; LOAD L-MDRIVE
; ASM G-MDRIVE
; LOAD G-MDRIVE
; G-MDRIVE MDISK000.COM MDISK100.COM
;
; BDOS equates

BDOS	equ	5		;BDOS entry point
PRINT	equ	9		;BDOS write string to console
RDLINE	equ	10		;BDOS read a line from the console
OPENF	equ	15		;BDOS open file
CLOSEF	equ	16		;BDOS close file
DELETEF	equ	19		;BDOS delete file
READF	equ	20		;BDOS read file
WRITEF	equ	21		;BDOS write file
MAKEF	equ	22		;BDOS make file
SETDMA	equ	26		;BDOS set DMA address

; File control block

FCB		equ	5Ch			;location of default CP/M FCB
FCBFN	equ	FCB+1		;location of file name
FCBEXT	equ	FCB+9		;location of file extension
FCBCR	equ	FCB+32		;current record

FCB2	equ	6Ch			;location of 2nd FCB from command line
FCB2FN	equ	FCB2+1		;location of file name

; Misc equates

MODOFF	equ	0600h		;offset of module base in the image file
CR	equ	13				;ascii for carriage return
LF	equ	10				;ascii for line feed

;-----------------------------------------------------------------------------
; Start of program
;-----------------------------------------------------------------------------
	org		0100h		;CP/M load and entry address

	lxi		h,0			;save stack pointer by HL=SP+0
	dad		sp			;HL=current stack pointer from CP/M
	shld	saveStk		;save for restore later

	lxi		sp,stack	;setup local stack

	call	clrRam		;Clear module memory

	call	rdMdrive	;read L-MDRIVE code into the image
	call	rdFiles		;read the two image files into memory

	call	mkBmap		;make the bitmap
	call	setParm		;Set relocation parameters
	call	dspData		;display the data we've collected
	call	wrtImg		;write the image to MDRIVE.COM

	lxi		d,mDone		;DE->creation complete message
	jmp		exitMsg

;-----------------------------------------------------------------------------
; clrRam - Clear module memory
;-----------------------------------------------------------------------------
clrRam:
	lxi		h,MDRIVE
	lxi		b,EOM
clrRamlp
	mov		a,b
	ora		c
	rz
	mvi		m,0
	inx		h
	dcx		b
	jmp		clrRamlp

;-----------------------------------------------------------------------------
; rdFiles - Read the two image files into memory and verify they are the
;     same length. Save the length in imgLen.
;-----------------------------------------------------------------------------
rdFiles:
	lxi		h,img0		;HL->where to read image ORG 000 file
	call	readImg		;read image from file #1
	shld	imgLen		;save length read

	lxi		h,img1		;HL->where to read image ORG 100 file
	mvi		a,'1'
	sta		dskFcb+6
	call	readImg		;read image 2 from file #2
	
	lxi		d,mLenErr	;DE->length error message
	lda		imgLen+1	;compare length MSBs
	cmp		h
	jnz		exitMsg		;exit with length error message

	lda		imgLen		;compare length LSBs 
	cmp		l
	jnz		exitMsg		;exit with length error message

	shld	modLen		;save MDISK module length
	ret

;-----------------------------------------------------------------------------
; readImg - Read image file in FCB (DE) into RAM pointed to by HL.
;     Number of bytes read returned in HL.
;----------------------------------------------------------------------------
readImg:
	push	h			;save starting address on stack
	lxi		d,-80h		;pre-decrement address by one sector
	dad		d			;HL->image-80h
	push	h			;save on stack

	xra		a			;zero current record in FCB
	sta		dskFCB+32
	lxi		d,dskFcb	;DE->file control block
	mvi		c,OPENF		;C=open file command
	call	BDOS		;open the image file
	lxi		d,mNoFile	;DE->file not found message
	inr		a			;test for FFh error
	jz		exitMsg		;file not found, exit
rdLoop:
	pop		h			;HL->last address read
	lxi		d,80h		;increment by one sector
	dad		d			;HL->next sector
	push	h			;save on stack
	xchg				;DE=buffer address
	mvi		c,SETDMA	;C=set buffer address command
	call	BDOS
	lxi		d,dskFCB	;DE->file control block
	mvi		c,READF		;C=read file sequential
	call	BDOS		;read the file
	ora		a			;end of file?
	jz		rdLoop		;no, read more
	pop		h			;HL->end of last sector read + 1
	pop		d			;DE->start of image file
	call	hlSubDe		;HL=end-start = length read
	ret					;exit

;-----------------------------------------------------------------------------
; rdMdrive - Read installer L-MDRIVE.COM into RAM.
;     Number of bytes read returned in HL.
;----------------------------------------------------------------------------
rdMdrive:
	xra		a			;zero current record in FCB
	sta		ldrFCB+32
	lxi		d,ldrFCB	;DE->FCB for L-MDRIVE.COM
	mvi		c,OPENF		;C=open file command
	call	BDOS		;open the image file
	inr		a			;test for FFh error
	lxi		d,IntErr
	jz		exitMsg		;Internal Error if file not found, exit
	lxi		h,MDRIVE	;HL->where to read image file
	push	h			;save starting address on stack
	lxi		d,-80h		;pre-decrement address by one sector
	dad		d			;HL->HL-80h
	push	h			;save on stack
rdMLoop
	pop		h			;HL->last address read
	lxi		d,80h		;increment by one sector
	dad		d			;HL->next sector
	push	h			;save on stack
	xchg				;DE=buffer address
	mvi		c,SETDMA	;C=set buffer address command
	call	BDOS
	lxi		d,ldrFCB	;DE->file control block
	mvi		c,READF		;C=read file sequential
	call	BDOS		;read the file
	ora		a			;end of file?
	jz		rdMLoop		;no, read more
	pop		h			;HL->end of last sector read + 1
	pop		d			;DE->start of image file
	call	hlSubDe		;HL=end-start = length read, exit
	shld	ldrLen
	ret

;-----------------------------------------------------------------------------
; mkBmap - Make relocation bitmap and put at the end of image 1. The
;    total length of the image (imgLen) is updated to include the
;    addition of the bitmap.
;-----------------------------------------------------------------------------
mkBmap:
	xra		a			;initialize 1st bitmap byte
	sta		bmByte

	lxi		d,img0		;DE->start of image ORG 000
	lhld	modLen		;HL=length of module: MDISK
	mov		b,h			;BC=length of module
	mov		c,l
	dad		d			;HL->end of module, start of bit map
	shld	bmapPtr		;save as the bitmap pointer
	shld	bmapBeg		;save as start of bitmap in image

	lxi		h,img1		;HL->start of image ORG 100

; mbLoop - Loop through all bytes of the module and create bitmap

mbLoop:
	lda		bmByte		;rotate bitmap byte
	rlc	
	sta		bmByte

	ldax	d			;A=byte from image 1
	cmp		m			;same as from image 2?
	push	h			;save HL
	jz		mbMod8		;yes, nothing to do

	lda		bmByte		;A=current bitmap byte
	ori		1			;set bit to flag adjustment is required
	sta		bmByte

mbMod8:
	inx		d			;increment image 1 pointer
	mov		a,e			;see if LSB is multiple of 8
	ani		07h		
	jnz		mbNext		;no, go on

	lda		bmByte		;A=completed bitmap byte
	lhld	bmapPtr		;HL->bit map for addresses just finished
	mov		m,a			;store the eight bits just done	
	inx		h			;HL->next bit map location
	shld	bmapPtr		;save it

	xra		a			;clear bitmap byte to start another 8
	sta		bmByte

mbNext:
	pop		h			;restore HL
	inx		h			;increment image 2 pointer

	dcx		b			;decrement total byte counter
	mov		a,b
	ora		c
	jnz		mbLoop		;loop if not zero

; Compute new length of image with the bitmap at the end and round up to
;     128 byte boundary, then subtract one. Later, when writing the file
;     and imgLen is decremented by 128 for each record (using DAD -80),
;     then we're assured to cross zero when the last sector is written.

	lhld	bmapPtr		;HL->end of image
	shld	bmapEnd		;Base of bitmap
	lxi		d,img0		;DE->start of image ORG 000
	call	hlSubDe		;Compute end-start in HL

; Caclulate total image size to write
	lxi		d,EOL		;DE=loader reserved space
	dad		d			;HL=loader+module+bitmap

; Round up to next boundary
	mov		a,l			;A=LSB of length
	adi		7Fh			;round up to next 80h boundary
	jnc		haveLen		;didn't wrap to MSB

	inr		h			;LSB wrapped, increment MSB
 
haveLen:
	ani		80h			;for LSB of 00h or 80h
	mov		l,a			;HL=image length rounded up to 128 bytes
	dcx		h			;decrement by one for DAD -80 to cross zero
	shld	imgLen		;update image length

	ret	

;-----------------------------------------------------------------------------
; Set parameters for MDRIVE to use when it runs
setParm:
	lhld	bmapBeg		;HL=Base of bitmap
	lxi		d,modoff	;Relocate to load address
	call	hlSubDe		;HL = HL - DE
	shld	RELBAS		;Put into location MDRIVE expects
	lhld	modLen		;HL=length of module
	shld	MDSIZE		;store in image where MDRIVE expects it
	ret


;-----------------------------------------------------------------------------
; dspData - Display information about the parameters found and computed
;-----------------------------------------------------------------------------
dspData:

; L-MDRIVE Module size
	lhld	ldrLen		;HL=data to display
	lxi		d,dLdrSz	;DE->where to put ascii result
	lxi		b,mLdrSz	;BC->message to display
	call	dataMsg		;display message with data

; Location of MDISK000 module in G-MDRIVE
	lxi		h,img0		;HL=data to display
	lxi		d,dModAt	;DE->where to put ascii result
	lxi		b,mModAt	;BC->message to display
	call	dataMsg		;display message with data

; Module MDISK000 size
	lhld	modLen		;HL=data to display
	lxi		d,dModSz	;DE->where to put ascii result
	lxi		b,mModSz	;BC->message to display
	call	dataMsg		;display message with data

; Location BITMAP in G-MDRIVE
	lhld	bmapBeg		;HL=pointer to bitmap
	lxi		d,dMapAt	;DE->where to put ascii result
	lxi		b,mMapAt	;BC->message to display
	call	dataMsg		;display message with data

; Size of bitmap
	lhld	bmapBeg		;pointer to bitmap
	xchg
	lhld	bmapEnd		;pointer to bitmap end
	call	hlSubDe		;Compute end-start in HL
	lxi		d,dMapSz	;DE->where to put ascii result
	lxi		b,mMapSz	;BC->message to display
	call	dataMsg		;display message with data

; Final image size MDRIVE.COM (L-MDRIVE + MDISK000 + BITMAP)

	lhld	imgLen		;HL=data to display
	inx		h			;restore actual value
	lxi		d,dImgSz	;DE->where to put ascii result
	lxi		b,mImgSz	;BC->message to display
	call	dataMsg		;display message with data and exit

; Passed VARS
; Show RELBASE (Offset to bitmap in MDRIVE.COM)
	lhld	RELBAS		;HL=Offset to bitmap in MDRIVE
	lxi		d,dRelba	;DE->where to put ascii result
	lxi		b,mRelba	;BC->message to display
	call	dataMsg		;display message with data
; Show MDSIZE
	lhld	MDSIZE		;HL=Offset to bitmap in MDRIVE
	lxi		d,dMDsiz	;DE->where to put ascii result
	lxi		b,mMDsiz	;BC->message to display
	call	dataMsg		;display message with data

; Show location of MDISK000 in MDRIVE.COM
	lxi		h,EOL 		;HL=Offset to image in MDRIVE.COM
	lxi		d,100h
	dad		d
	lxi		d,dImg0		;DE->where to put ascii result
	lxi		b,mImg0		;BC->message to display
	call	dataMsg		;display message with data

	ret

;-----------------------------------------------------------------------------
; dataMsg - Display a data message given:
;    HL = 16 bit data to display
;    DE->where to put ASCII hex of the value in HL
;    BC->message to display after inserting the data
;-----------------------------------------------------------------------------
dataMsg:
	xchg				;DE=data,HL->ascii location

	mov		a,d			;A=MSB of data
	call	bin2hex		;store as ASCII at (HL)

	mov		a,e			;A=LSB of data
	call	bin2hex		;store as ASCII at (HL)

	mov		d,b			;DE->message to display
	mov		e,c

	mvi		c,PRINT		;C=CP/M print message command
	jmp		BDOS		;display the message and exit

;-----------------------------------------------------------------------------
; bin2hex - Convert byte in A into two ascii hex bytes and store where
;    pointed to by HL. HL=HL+2 on exit.
;-----------------------------------------------------------------------------
bin2hex:
	push	a			;save copy of value

	rrc					;put msn into lsn
	rrc
	rrc
	rrc
	ani		0Fh			;get nibble alone
	adi		'0'			;convert to printable ascii
	cpi		'9'+1		;see if > '9'
	jc		msnDone

	adi		7			;make A to F printable

msnDone:
	mov		m,a			;store msn at (HL)
	inx		h

	pop		a			;get original byte back

	ani		0Fh			;get lsb alone
	adi		'0'			;convert to printable ascii
	cpi		'9'+1		;see if > '9'
	jc		lsnDone		;no, lsn is done

	adi		7			;make A-F printable

lsnDone:
	mov		m,a			;store at (HL)
	inx		h

	ret

;-----------------------------------------------------------------------------
; wrtImg - Write the final image to MDRIVE.COM
;----------------------------------------------------------------------------
wrtImg:
	lxi		d,outFCB	;DE->output file FCB
	mvi		c,DELETEF	;C=delete file command
	call	BDOS		;delete MDRIVE.COM
	lxi		d,outFCB	;DE->output file FCB
	mvi		c,MAKEF		;C=make file command
	call	BDOS		;create the file
	lxi		d,mMakErr	;DE->can't create file error message
	inr		a			;test for FF (make file fail)
	jz		exitMsg		;create file failed, exit
	xra		a			;zero current record
	sta		outFCB+32
	lxi		h,MDRIVE	;HL->image to write to disk
wrLoop:
	push	h			;save pointer on stack
	xchg				;DE=buffer address
	mvi		c,SETDMA	;C=set buffer address command
	call	BDOS
	lxi		d,outFCB	;DE->file control block
	mvi		c,WRITEF	;C=write file sequential
	call	BDOS
	lxi		d,mWrtErr	;DE->write error message
	ora		a			;test for write error
	jnz		exitMsg		;write failed, exit
	lhld	imgLen		;decrement bytes left to write
	lxi		d,-80h
	dad		d
	shld	imgLen		;save the bytes remaining
	jnc		wrExit		;all done, exit
	pop		h			;restore HL
	lxi		d,80h		;increment by 80h bytes
	dad		d
	jmp		wrLoop	
wrExit:
	pop		h			;restore stack
	lxi		d,outFCB	;DE->output file FCB
	mvi		c,CLOSEF	;C=close file command
	jmp		BDOS		;do close and exit	

;-----------------------------------------------------------------------------
; movFCB - Move 16 byte FCB from (HL) to (DE)
;-----------------------------------------------------------------------------
movFcb:
	mvi		b,16		;move 16 bytes

mfLoop:
	mov		a,m			;move from (HL) to (DE)
	stax	d

	inx		h			;increment pointers
	inx		d

	dcr		b			;decrement and test count
	jnz		mfLoop

	ret

;-----------------------------------------------------------------------------
; hlSubDe - HL = HL - DE, sets Zero flag for result = 0
;-----------------------------------------------------------------------------
hlSubDe:
	mov		a,l			;subtract LSBs
	sub		e
	mov		l,a			;result in L

	mov		a,h			;subtract MSBs
	sbb		d
	mov		h,a			;result in H

	ora		l			;set zero flag for 16 bit result
	
	ret

;-----------------------------------------------------------------------------
;  exitMsg
;-----------------------------------------------------------------------------
exitMsg:
	mvi		c,PRINT		;display message passed in DE
	call	BDOS

	lhld	saveStk		;restore stack pointer
	sphl

	ret					;return to CCP

;-----------------------------------------------------------------------------
;  String Constants
;-----------------------------------------------------------------------------
mHelp:	equ	$		;banner/help display
 db CR,LF
 db 'G-MDRIVE v1.0 - Make MDRIVE.COM',CR,LF
 db LF
 db 'Creates the program MDRIVE.COM',CR,LF
 db LF
 db 'Usage: G-MDRIVE',CR,LF,'$' 

mLdrSz	db	'MDRIVE module size: '
dLdrSz	db	'xxxxh',CR,LF,'$'

mModAt	db	'MDISK  at: '
dModAt	db	'xxxxh',CR,LF,'$'
mModSz	db	'MDISK  module size: '
dModSz	db	'xxxxh',CR,LF,'$'

mMapAt	db	'BITMAP at: '
dMapAt	db	'xxxxh',CR,LF,'$'
mMapSz	db	'BITMAP size: '
dMapSz	db	'xxxxh',CR,LF,'$'
mImgSz	db	'Final image size: '
dImgSz	db	'xxxxh',CR,LF,LF
		db	'MDRIVE.COM',CR,LF,'$'
; Passed VARS
mRelba	db	'RELBAS '
dRelba	db	'xxxxh',CR,LF,'$'
mMDsiz	db	'MDSIZE '
dMDsiz	db	'xxxxh',CR,LF,'$'
mImg0	db	'MDISK000 at '
dImg0	db	'xxxxh',CR,LF,'$'

mDone	db	LF,'MDRIVE.COM Created',CR,LF,'$'

mLenErr	db	CR,LF,'File sizes do not match',CR,LF,'$'
mNoFile	db	CR,LF,'File MDISKXXX.COM not found',CR,LF,'$'
mRdErr	db	CR,LF,'File read error',CR,LF,'$'
mNotDif	db	CR,LF,'The two files are the same MDISK memory size',CR,LF,'$'
mMakErr	db	CR,LF,'Can''t create output file',CR,LF,'$'
mWrtErr	db	CR,LF,'File write error, disk full?',CR,LF,'$'
IntErr	db	CR,LF,'Internal error',CR,LF,'$'

;-----------------------------------------------------------------------------
;  Data Area
;-----------------------------------------------------------------------------
; output FCB
outFcb	db	0							;+0 User#/Drive
		db	'MDRIVE  COM'				;+1 filename
		db	0							;+12 Extent
		db	0,0							;+13 Reserved
		db	0							;15 Records Used
		ds	15							;Allocation Blocks
		db	0							;CR
		db	0,0,0						;RR

; loader FCB
ldrFcb	db	0							;User#/Drive
		db	'L-MDRIVECOM'				;filename
		db	0							;+12 Extent
		db	0,0							;+13 Reserved
		db	0							;15 Records Used
		ds	15							;Allocation Blocks
		db	0							;CR
		db	0,0,0						;RR

; MDISK FCB
dskFcb	db	0							;User#/Drive
		db	'MDISK000COM'				;filename
		db	0							;+12 Extent
		db	0,0							;+13 Reserved
		db	0							;15 Records Used
		ds	15							;Allocation Blocks
		db	0							;CR
		db	0,0,0						;RR

saveStk	ds	2			;save area for CP/M stack pointer 

bmapPtr	ds	2			;pointer to current bitmap location
bmapBeg	ds	2			;pointer to start of bitmap
bmapEnd	ds	2			;pointer to end of bitmap
bmByte	ds	1			;single byte of bits for bitmap
ldrLen	ds	2			;Length of MDRIVE file
modLen	ds	2			;module length
imgLen	ds	2			;length of MDRIVE+MDISK+BITMAP to be saved

	ds	32				;local stack space
stack	equ	$

;-----------------------------------------------------------------------------
;  MDRIVE.COM is patched here for copying into the generated MDRIVE
;-----------------------------------------------------------------------------
		org	($+255) AND 0FF00h	;force page boundary
;-----------------------------------------------------------------------------
; The installer program is read into MDRIVE and can be up to 2K.
;-----------------------------------------------------------------------------
MDRIVE		equ	$				;parameters are in last page
			ds	800h			;LOADER: UP TO 2K
RELBAS:		equ	MDRIVE+3		;RELOCATION BASE
MDSIZE:		equ	RELBAS+2		;SIZE OF MODULE
EOL			equ	$-MDRIVE
;-----------------------------------------------------------------------------
; The image files (MDISK000,MDISK100) read from disk are saved here in
; img0 and img1. 1K is reserved for each image.
;-----------------------------------------------------------------------------
		org	($+255) AND 0FF00h	;force page boundary
img0		equ	$				;Base of ORG 000 module (1K).
img1		equ	img0+400h		;Base of ORG 100 module (1K).
EOM			equ	($-MDRIVE)+800h	;End of modules
	end
