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
MDRIVEL	equ	0400h		;length of MDRIVE module

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

	call	clrRam		;Clear unused memory
	call	chkParm		;check input parameters
	call	rdFiles		;read the two image files into memory
	call	rdMdrive	;move MDRIVE code into the image
	call	mkBmap		;make the bitmap
	call	setParm		;Set relocation parameters
	call	dspData		;display the data we've collected
	call	wrtImg		;write the image to MDRIVE.COM

	lxi		d,mDone		;DE->creation complete message
	jmp		exitMsg

;-----------------------------------------------------------------------------
; clrRam - Clear unused memory
;-----------------------------------------------------------------------------
clrRam:
	lxi		h,img0
	lxi		b,img2-img0
clrRamlp
	mov		a,b
	ora		c
	rz
	mvi		m,0
	inx		h
	dcx		b
	jmp		clrRamlp

;-----------------------------------------------------------------------------
; chkParm - Verify the file name parameters are supplied. If not, display
;    help banner and exit. Otherwise, save the 2nd file name supplied in
;    fcbSave.
;-----------------------------------------------------------------------------
chkParm:
	lxi		d,mHelp		;DE->help message

	lda		FCBFN		;look at 1st character of 1st file name
	cpi		' '			;anything there?
	jz		exitMsg		;no, exit with message from DE

	lda		FCB2FN		;look at 1st character of 2nd file name
	cpi		' '			;anything there?
	jz		exitMsg		;no, exit with message from DE

	lxi		h,FCB2		;HL->source FCB (2nd file name)
	lxi		d,fcbSave	;DE->destination (FCB save area)
	jmp		movFcb		;save 2nd FCB and exit

;-----------------------------------------------------------------------------
; rdFiles - Read the two image files into memory and verify they are the
;     same length. Save the length in imgLen.
;-----------------------------------------------------------------------------
rdFiles:
	lxi		h,img1		;HL->where to read image file
	call	readImg		;read image from file #1
	shld	imgLen		;save length read

	lxi		h,fcbSave	;HL->source FCB (2nd file name)
	lxi		d,FCB		;DE->destination (default FCB)
	call	movFcb		;put 2nd file name into default FCB

	lxi		h,img2		;HL->where to read image file
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
; readImg - Read image file in FCB into RAM pointed to by HL. Number
;     of bytes read returned in HL.
;----------------------------------------------------------------------------
rdMdrive:
	lxi		h,ldrFcb	;HL->FCB for L-MDRIVE.COM
	lxi		d,FCB		;DE->destination (default FCB)
	call	movFcb		;put output file name into default FCB
	lxi		h,img0		;HL->where to read image file
	call	readImg		;read image from file #1
						;Return HL=length
	shld	ldrLen
	ret

;-----------------------------------------------------------------------------
; readImg - Read image file in FCB into RAM pointed to by HL. Number
;     of bytes read returned in HL.
;----------------------------------------------------------------------------
readImg:
	push	h			;save starting address on stack

	lxi		d,-80h		;pre-decrement address by one sector
	dad		d			;HL->image-80h
	push	h			;save on stack
	
; Insert .COM extension if not supplied. If just memory sizes entered
;    as parameters, create the default 'MDISKxx' file name.

	lxi		h,FCBEXT	;HL->file extension in FCB
	mvi		a,' '		;compare to space to see if ext present
	cmp		m
	jnz		haveExt		;yes, extension is present

	mvi		m,'C'		;else, insert 'COM' as extension
	inx		h
	mvi		m,'O'
	inx		h
	mvi		m,'M'

haveExt:
	lda		FCBFN		;A=first character of file name
	sui		'0'			;see if a digit 0-5
	cpi		5
	jnc		haveFn		;no, a file name must already be there

	lhld	FCBFN		;move xx memory size to 6,7 in file name
	shld	FCBFN+5
	
	lxi		h,'MD'		;put 'MDISK' in 0-5 of file name
	shld	FCBFN+0
	lxi		h,'IS'
	shld	FCBFN+2
	mvi		a,'K'
	sta		FCBFN+4

; Read the file image into memory

haveFn:
	xra		a			;zero current record in FCB
	sta		FCBCR

	lxi		d,FCB		;DE->file control block
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

	lxi		d,FCB		;DE->file control block
	mvi		c,READF		;C=read file sequential
	call	BDOS		;read the file
	ora		a			;end of file?
	jz		rdLoop		;no, read more

	pop		h			;HL->end of last sector read + 1
	pop		d			;DE->start of image file
	jmp		hlSubDe		;HL=end-start = length read, exit

;-----------------------------------------------------------------------------
; mkBmap - Make relocation bitmap and put at the end of image 1. The
;    total length of the image (imgLen) is updated to include the
;    addition of the bitmap.
;-----------------------------------------------------------------------------
mkBmap:
	xra		a			;initialize 1st bitmap byte
	sta		bmByte

	lxi		d,img1		;DE->start of image 1
	lhld	modLen		;HL=length of module: MDISK
	mov		b,h			;BC=length of module
	mov		c,l
	dad		d			;HL->end of module, start of bit map
	shld	bmapPtr		;save as the bitmap pointer
	shld	bmapBeg		;save as start of bitmap in image

	lxi		h,img2		;HL->start of image 2

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
	shld	bmapEnd
	lxi		d,img1		;DE->start of image
	call	hlSubDe		;Compute end-start in HL

; Caclulate total image size to write
	lxi		d,MDRIVEL	;DE=loader reserved space
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

; MDRIVE Module size

	lhld	ldrLen		;HL=data to display
	lxi		d,dLdrSz	;DE->where to put ascii result
	lxi		b,mLdrSz	;BC->message to display
	call	dataMsg		;display message with data

; MDISK Module size

	lhld	modLen		;HL=data to display
	lxi		d,dModSz	;DE->where to put ascii result
	lxi		b,mModSz	;BC->message to display
	call	dataMsg		;display message with data

; Size of bitmap
	lhld	bmapBeg		;pointer to bitmap
	xchg
	lhld	bmapEnd		;pointer to bitmap end
	call	hlSubDe		;Compute end-start in HL
	lxi		d,dMapSz	;DE->where to put ascii result
	lxi		b,mMapSz	;;BC->message to display
	call	dataMsg		;display message with data

; Final image size

	lhld	imgLen		;HL=data to display
	inx		h			;restore actual value
	lxi		d,dImgSz	;DE->where to put ascii result
	lxi		b,mImgSz	;BC->message to display
	jmp		dataMsg		;display message with data and exit

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
; wrtImg - Write the final image to BMOVCPM.COM
;----------------------------------------------------------------------------
wrtImg:
	lxi		h,outFcb	;HL->FCB for MDRIVE.COM
	lxi		d,FCB		;DE->destination (default FCB)
	call	movFcb		;put output file name into default FCB

	lxi		d,FCB		;DE->output file FCB
	mvi		c,DELETEF	;C=delete file command
	call	BDOS		;delete MDRIVE.COM

	lxi		d,FCB		;DE->output file FCB
	mvi		c,MAKEF		;C=make file command
	call	BDOS		;create the file
	
	lxi		d,mMakErr	;DE->can't create file error message
	inr		a			;test for FF (make file fail)
	jz		exitMsg		;create file failed, exit

	xra		a			;zero current record
	sta		FCBCR

	lxi		h,img0		;HL->image to write to disk

wrLoop:
	push	h			;save pointer on stack
	xchg				;DE=buffer address
	mvi		c,SETDMA	;C=set buffer address command
	call	BDOS

	lxi		d,FCB		;DE->file control block
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

	lxi		d,FCB		;DE->output file FCB
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
 db 'Usage: G-MDRIVE param1 param2',CR,LF
 db '  Where param1 & param2 = [filename]XX[.ext]',CR,LF
 db LF
 db 'param1 and param2 identify two MDISK.COM image files',CR,LF
 db 'of two different memory sizes',CR,LF
 db LF
 db 'Examples:',CR,LF
 db '  G-MDRIVE MDISK000.COM MDISK100.COM',CR,LF
 db '  G-MDRIVE MDISK0 MDISK1  (assumes .COM extension)',CR,LF
 db '  G-MDRIVE 000 100        (assumes filename like MDISKxx.COM)',CR,LF,'$' 

mLdrSz	db	'MDRIVE module size: '
dLdrSz	db	'xxxxh',CR,LF,'$'

mModBg	db	'MDISK  module start: '
dModBg	db	'xxxxh',CR,LF,'$'

mModSz	db	'MDISK  module size: '
dModSz	db	'xxxxh',CR,LF,'$'

mMapSz	db	'BITMAP size: '
dMapSz	db	'xxxxh',CR,LF,'$'

mImgSz	db	'Final image size: '
dImgSz	db	'xxxxh',CR,LF,'$'

mDone	db	LF,'MDRIVE.COM Created',CR,LF,'$'

mLenErr	db	CR,LF,'File sizes do not match',CR,LF,'$'
mNoFile	db	CR,LF,'File not found',CR,LF,'$'
mRdErr	db	CR,LF,'File read error',CR,LF,'$'
mNotDif	db	CR,LF,'The two files are the same MDISK memory size',CR,LF,'$'
mMakErr	db	CR,LF,'Can''t create output file',CR,LF,'$'
mWrtErr	db	CR,LF,'File write error, disk full?',CR,LF,'$'

;-----------------------------------------------------------------------------
;  Data Area
;-----------------------------------------------------------------------------
outFcb	db	0,'MDRIVE  COM',0,0,0,0		;output FCB
ldrFcb	db	0,'L-MDRIVECOM',0,0,0,0		;loader FCB

saveStk	ds	2			;save area for CP/M stack pointer 
fcbSave	ds	16			;save area for FCB prepared by the CCP
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
; The image files read from disk are saved here in img1 and img2.
;    1K is reserved for each image. Images are on a page boundary
;-----------------------------------------------------------------------------
img0		equ	$
MDRIVE:		ds	MDRIVEL			;parameters are in last page
RELBAS:		equ	MDRIVE+3		;RELOCATION BASE
MDSIZE:		equ	RELBAS+2		;SIZE OF MODULE

		org	($+255) AND 0FF00h	;force page boundary
img1		equ	$
img2		equ	img1+1000h		;leaves 4K for 1st image

	end
