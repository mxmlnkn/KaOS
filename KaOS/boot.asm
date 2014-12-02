org 7C00h	; Unsere Startadresse
Bits 16
; -----------------------------------------
; Unser Bootloader
; -----------------------------------------
	jmp word start				;JmpBoot		Sprung zu Bootsector (1Byte=Jmp + 2Byte=WORD Start	->3		3
	OS_NAME	DB	"KaOS    "		;OSName		Name des Betriebssystems							8
	BPS		DW	512				;BytesPerSec	Bytes pro Sektor, bei Diskette 512						2
	SPC		DB	1				;SecPerClus		Sektoren pro Cluster bei Disktte 1						1
	RESSEC	DW	1				;RsvdSecCnt	reservierte Sektoren, nur Boot-Sektor					2
	NFATS	DB	2				;NumFATs		Anz. der FAT-Tabellen, aus Sicherheit immer 2				1
	ROOTE	DW	224				;RootEntCnt		Anz. der Einträge ins Root-Verzeichnis, 224 reichen voll und ganz	2
	TOTSEC	DW	2880			;TotSec		gesamte Anzahl der Sektoren, bei Diskette (1.44M) 2880		2
	MEDTYP	DB	0F0H			;MediaType		Medien Typ, Diskette = 0Fh							1
	FATSIZE	DW	9				;FATSize		Anzahl der Sektoren, die eine FAT-Tabelle belegt ist 9			2
	SPT		DW	18				;SecPerTrack	Sektoren pro Cylinder(Track) gibt es auf einer 1.44M Diskette 18	2
	NH		DW	2				;NumHeads		Anzahl der Seiten, ist bei 1.44M Diskette 2					2
	HS		DD	0				;HiddenSec		Anzahl der versteckten Sektoren ?						4
	TOTS	DD	0				;TotSec32		nur für FAT32 -> deshalb 0							4
	DRVNUM	DB	0				;DrvNum		Laufwerks-Nummer bei Diskette (A:) = 0					1
	RES		DB	0				;Reserved		sollte 0 enthalten									1
	BOOTS	DB	29H				;														1
	VID		DD	0				;VolumeID		wird meist mit der Zeit belegt							4
	VLABEL	DB	"NO NAME    "	;														11
	FSTP1	DB	"FAT12   "		;FileSysType	FAT-Dateisystem									8
; Speicherung der Zeit in ein 16Bit Register
; Bit 0 - 4 (5 Bits) = Tag des Monats; Werte : 0 - 31
; Bit 5 - 8 (4 Bits) = Monat; Werte : 1 - 12
; Bit 9 - 15 (7 Bits) = Jahr; Werte : 0 - 127	(+ 1980)

start:
; Erst brauchen wir einen Stack.
cli				; Keine Interrupts!
mov ax,9000h	; Stackadresse
mov ss,ax		; SS = 9000 (unser Stack)
mov sp,0FFFFh	; SP = 0  (der Stackpointer)
sti				; Interrupts zulassen

;Soweit ich das mitbekommen hab, besteht ein Cylinder aus 2 Teilen
;Head0 und Head1. Head0 sind die ersten 18 Bytes (0-17) und Head1 sind
;die zweiten 18 Bytes (18-36). Da Root Entry auf Sektor 19 liegt,
;brauchen wir head1.
mov [cs:bootdrive],dl

push word 1000H
pop es
mov bp,19
load_root:								;Lade FAT
	mov ax,bp
	CALL LBA_CHS
	xor dl,dl
	mov bx,[memory]
	try_again:
		mov ax,0201H
		int 13h
	jc try_again
	add word[memory],512
	inc bp
	cmp bp,33
	jne load_root

lea di,[datn]
push cs
pop es
call ReadFileName			;AX=Erster cLUSTER
or ax,ax
jnz load_kernel
	lea si, [FEHLER]
	push cs
	pop ds
	call write
	int 16h
	int 19h

load_kernel:
	push word 1000h
	pop es
	mov word[memory],0
	add ax,31			;Datenbreich + Cluster
	mov cx,63			;64 Sektoren dazuaddieren, um dann vergleichen zu können
	mov bp,ax

	WEITER:
		push cx
		mov ax,bp
		CALL LBA_CHS
	
		xor dl,dl
		mov bx,[memory]
		try_read_kernel:
			mov ax,0201H
			int 13h
		jc try_read_kernel
		add word[memory],512

		inc bp
		pop cx
	loop WEITER

push cs
pop ds
lea si,[loadmsg]
call write				; Meldung ausgeben

;Springe zu diesem Kernel
mov ax,1000h		;Die Adresse des Programms
mov es,ax			;Segmentregister updaten
mov ds,ax

xor eax,eax			;Register auf 0 setzen
xor ebx,ebx			;	~
xor ecx,ecx			;	~
xor edx,edx			;	~
xor edi,edi			;	~
xor esi,esi			;	~

mov dl,[bootdrive]
jmp 1000h:0h

loadmsg db 'Loading...OK',13,10,0
datn db "KRNLFAT    "
memory dw 0
FEHLER db "Kernel.com nicht gefunden",0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ReadFileName: Suche in Root Dateiname
;; Parameter: ES:DI Dateiname
;; Back:	AX=0 Fehler(Nicht gefunden)
;;		AX<>0 Nummer des Filetable Eintrages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReadFileName:
	mov cx,224
	push word 1000H
	pop ds
	xor si,si	;zeige auf Root Dir
	cld
	READ_FN:
		push di
		push si
		push cx
		mov cx,10
		repe cmpsb
		or cx,cx
		pop cx
		pop si
		pop di
		je DAT_OK
		add si,32
		loop READ_FN
			xor ax,ax
			ret
	DAT_OK:
		mov ax,224
		sub ax,cx
		mov bx,32
		mul bx
		mov si,ax			;SI zeigt auf den benötigten Filetable Eintrag
		add si,26			;SI Zeigt auf den Cluster
		lodsw
	ret

; Von LBA zu CHS: 
; Sektor = LBA_Sektor % SectorsPerTrack
; Temp = LBA_Sektor / SectorsPerTrack
; Cylinder = Temp / Head_anzahl
; Head = Temp % Head_anzahl
;***************************************************************************************************
;******************* lba_chs ax:lba dh:headanzahl -> cl=cylinder ch=sector dh=head *****************
;***************************************************************************************************
LBA_CHS:
	xor	dx,dx			;clear out DX reg
	mov bx,18
	div	bx				;divide AX by SPT
	inc dl				;increment dl to get sector address
	mov	cl,dl			;get sector value
	xor	dx,dx			;clear DX reg
	mov bx,2
	div	bx
	mov	ch,al			;get cylinder value
	mov	dh,dl			;get head value
ret

;***********************************************************************
;******************* write ds:si String Ausgabe:String *****************
;***********************************************************************
write:
	pusha
	mov ah,0Eh		; BIOS teletype
	mov bx,2		; Textfarbe grün (geht nur im Grafik-Modus)
write2:	
	lodsb			; laden des nächsten Zeichens
	or al,al		; testen ob Zeichen (al) 0
	jz writed		; falls das Charzeichen 0 ist, dann writed
	int 10h			; BIOS-INterrupt
	jmp write2		; Nächstes Byte
writed:
	popa
	ret

bootdrive db 224
	
times 510-($-$$) db 0	; Dateil?e: 512 Bytes
dw 0AA55h				; Bootsignatur

FAT_TAB1:
	times 9*512-($-FAT_TAB1) db 0
FAT_TAB2:
	times 9*512-($-FAT_TAB2) db 0
ROOT_DIR:
	; DB	"KERNEL  BIN"		;DIR_Name			Dateiname
	; DB	0					;DIR_Attribute		ATTR_READ_ONLY: 0x01	-> Bit0
	;;										ATTR_HIDDEN: 0x02		-> Bit1
	;;										ATTR_SYSTEM: 0x04 		-> Bit2
	;;										ATTR_VOLUME_ID: 0x08	-> Bit3
	;;										ATTR_DIRECTORY: 0x10	-> Bit4
	;;										ATTR_ARCHIVE: 0x20 		-> Bit5
	; DB	0					;DIR_NTRes		Dieses Byte ist für Windows NT reserviert und sollte immer mit 0 initalisiert werden.
	; DW	0					;
	; DB	0					;DIR_CrtTimeTenth	Milisekunden / 10
	; DW	0					;DIR_LstAccDate		Datum der Erstellung der Datei / des Verzeichnisses. Siehe oben f?icherweise von Datum in 16 BITs.
	; DW	0					;
	; DW	0					;DIR_FstClusH		Dieser Eintrag ist f?12 nicht relevant und sollte immer 0 sein.
	; DW	0					;DIR_WrtTime		Zeit, an der die Datei das letzte Mal geschrieben wurde. Also wann sie das letzte Mal ver?ert wurde.
	; DW	0					;DIR_WrtDate		Datum, an dem die Datei das letzte Mal ver?ert wurde.
	; DW	2					;DIR_FstClusLO		Die Nummer des Startsektors der Datei / des Verzeichnisses.
	; DD	32768				;DIR_FileSize		Größe der Datei in Bytes
	times 14*512-($-ROOT_DIR) db 0