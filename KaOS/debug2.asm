;mit NASM compiliert
org 100h

nop
pushf
pushf
pop ax
mov ebx,2
call zahlausgabe
call break
popf
call install_debug
xor eax,eax
xor ebx,ebx
xor ecx,ecx
xor edx,edx
xor esi,esi
xor edi,edi
xor ebp,ebp

lea si,[msg]
call writeln
xor ax,ax
int 16h
retf

;***********************************************************************
;************* Zahlausgabe eax:zahl ebx:basis Ausgabe:zahl *************
;***********************************************************************
zahlausgabe:
	pusha
	XOR ECX,ECX
zahlausgabe1:
	XOR EDX,EDX
	DIV EBX
	PUSH EDX
	INC ECX
	OR EAX,EAX
	JNE zahlausgabe1

	MOV AH,0Eh
	MOV BX,2
	zahlausgabe2:
	POP EDX
	CMP EDX,10
	JB Zahl
		ADD DL,7
		Zahl:
			ADD DL,48
			MOV AL,DL
			INT 10h
	LOOP zahlausgabe2
	popa
	ret

;***********************************************************************
;************* writeln ds:si String bx:farbe Ausgabe:String ************
;***********************************************************************
writeln:
	pusha
	mov ah,0Eh		; Funktion 0x0E	
writeln2:	
	lodsb			; Byte laden
	or al,al
	jz writelnd		; 0-Byte? -> Ende!
	int 10h			; schreiben
	jmp writeln2		; N?stes Byte
writelnd:
	mov al,13
	int 10h
	mov al,10
	int 10h
	popa
	ret	

;***********************************************************************
;*************************** install_debug *****************************
;***********************************************************************
install_debug:
	pushf
	push ax
	push es
	cli
	xor ax,ax
	mov es,ax
	mov word[es:4*1h],debug
	mov word[es:4*1h+2],cs
	sti
	pop es
	pop ax
	popf
	ret
	
;***********************************************************************
;*********************** debug zeigt Register **************************
;***********************************************************************
debug:
;Register speichern
;OFFSET	00	02	04	06	08	10	12		16		20	24	28	32	36	40	44	48	52	56	58		62
;BITS	16	16	16	16	16	16	32		32		32	32	32	32	32	32	32	32	32	16	32
;REG		cs	ds	es	fs	gs	ss	[ds:si]	[es:di]	Flags	eax	ebx	ecx	edx	edi	esi	ebp	esp	ip	[cs:ip]
pushad
push ds
push es

mov [cs:reg_eax],eax
mov [cs:reg_ebx],ebx
mov [cs:reg_ecx],ecx
mov [cs:reg_edx],edx

mov [cs:reg_ds],ds
mov [cs:reg_es],es
mov [cs:reg_fs],fs
mov [cs:reg_gs],gs
mov [cs:reg_ss],ss

mov ax,cs
mov ds,ax
mov es,ax

pushfd
pop eax
mov [reg_flags],eax
mov [reg_edi],edi
mov [reg_esi],esi
mov [reg_ebp],ebp
mov [reg_esp],esp

mov eax,[ds:esi]
mov [reg_dssi],eax
; push es
; pop ds
; mov esi,edi
; mov eax,[ds:esi]
mov [reg_esdi],eax

mov [reg_cs],cs
mov [reg_ip],cs
mov [reg_csip],cs

;Register speichern
;Ausgabe der Ergebnisse
	mov ax,cs
	mov es,ax
	mov ds,ax
	lea di,[screen]
	mov ax,0B800h
	mov ds,ax
	xor si,si
	mov cx,160
	rep movsd
	
	mov ah,03h
	xor bh,bh
	int 10h			;wherexy
	lea di,[xy]
	mov ax,dx
	stosw
	mov ah,02h
	xor dx,dx
	xor bh,bh
	int 10h			;gotoxy
	mov ax,cs
	mov es,ax
	mov ds,ax
	lea si,[debug1]
	call writeln
	lea si,[debug2]
	call writeln
	lea si,[debug3]
	call writeln	;Bildschirm einrichten

	push cs
	pop ds
	xor eax,eax
	mov cx,6
	mov dx,0121h
	mov ebx,16
	lea si,[regs]
	segment_reg:
		lodsw
		push ax
		push cx
		call zahl_length
		sub dl,cl
		mov ah,02h
		int 10h
		add dl,cl
		pop cx
		pop ax
		add dl,8
		call zahlausgabe
		loop segment_reg

	mov cx,2
	mov dx,020Eh
	mov ax,15
	mov fs,ax
	call repeat_show_regs
	mov dx,0344h
	mov ebx,2
	mov cx,1
	call repeat_show_regs

	mov cx,4
	mov dx,000Ch
	mov ebx,16
	mov ax,13
	mov fs,ax
	call repeat_show_regs
	mov cx,2
	mov dx,010Ch
	call repeat_show_regs
	mov cx,2
	mov dx,0040h
	call repeat_show_regs
	
	mov cx,1
	mov dx,024Ch
	call repeat_show_regs

	mov dl,[xy]
	mov ah,02h
	int 10h
	xor ax,ax
	int 16h

	lea si,[screen]
	mov ax,0B800h
	mov es,ax
	xor di,di
	mov cx,160
	rep movsd
 ;Ausgabe der Ergebnisse
;Register wiederherstellen eax,ebx,ecx,edx,edi,esi,ds,es,flags
	lea si,[regs]
	add si,2
	lodsw
	push ax
	lodsw
	push ax
	lodsw
	push ax
	add si,12
	mov cx,7
	load_regs:
		lodsd
		push eax
		loop load_regs
	pop esi
	pop edi
	pop edx
	pop ecx
	pop ebx
	pop eax
	popfd
	pop fs
	pop es
	pop ds
	
	pop es
	pop ds
	popad
	iret
;Register wiederherstellen eax,ebx,ecx,edx,edi,esi,ds,es,flags
repeat_show_regs:
	lodsd
	push ax
	push cx
	call zahl_length
	sub dl,cl
	mov ah,02h
	int 10h
	add dl,cl
	pop cx
	mov ax,fs
	add dl,al
	pop ax
	call zahlausgabe
	loop repeat_show_regs
	ret
;EAX:00000000 EBX:00000000 ECX:00000000 EDX:00000000 EBP:00000000 ESP:00000000
;EDI:00000000 ESI:00000000 CS:0000 DS:0000 ES:0000 FS:0000 GS:0000 SS:0000
;DS:SI:00000000 ES:DI:00000000 CS:IP:00000000 Flags: ----ODITSZ-A-P-C
;Format Register:x+var,y+var (wenn Zahl 0Stellen besitzt -> ansonsten um Anz.d.St. weniger addieren)
;	-> EAX:+12+0 EBX:+25+0 ECX:+38+0 EDX:+51+0 EBP:+64+0 ESP:+77+0
;	-> EDI:+12+1 ESI:+25+1 CS:+33+1 DS:+41+1 ES:+49+1 FS:+57+1 GS:+65+1 SS:73+1
;	-> DS:SI:+14+2 ES:DI:+29+2 CS:IP:+44+2 FLAGS:+68+3
debug1	db 'EAX:00000000 EBX:00000000 ECX:00000000 EDX:00000000 EBP:00000000 ESP:00000000',0
debug2	db 'EDI:00000000 ESI:00000000 CS:0000 DS:0000 ES:0000 FS:0000 GS:0000 SS:0000',0
debug3	db 'DS:SI:00000000 ES:DI:00000000 CS:IP:00000000 Flags: ----ODITSZ-A-P-C IP:0000',0
regs:
	reg_cs dw 0
	reg_ds dw 0
	reg_es dw 0
	reg_fs dw 0
	reg_gs dw 0
	reg_ss dw 0
	reg_dssi dd 0
	reg_esdi dd 0
	reg_flags dd 0
	reg_eax dd 0
	reg_ebx dd 0
	reg_ecx dd 0
	reg_edx dd 0
	reg_edi dd 0
	reg_esi dd 0
	reg_ebp dd 0
	reg_esp dd 0
	reg_ip dw 0
	reg_csip dd 0

xy		dw 0
screen	times 640 db 0
;OFFSET	00	02	04	06	08	10	12		16		20	24	28	32	36	40	44	48	52	56	58		62
;BITS	16	16	16	16	16	16	32		32		32	32	32	32	32	32	32	32	32	16	32
;REG		cs	ds	es	fs	gs	ss	[ds:si]	[es:di]	Flags	eax	ebx	ecx	edx	edi	esi	ebp	esp	ip	[cs:ip]

;***********************************************************************
;************* Zahl_length eax:zahl ebx:basis cx:zahllänge *************
;***********************************************************************
zahl_length:
	PUSH EAX
	PUSH EBX
	PUSH EDX
	XOR CX,CX
zahl_length_start:
	XOR EDX,EDX
	DIV EBX
	INC CX
	OR EAX,EAX
	JNE zahl_length_start
	POP EDX
	POP EBX
	POP EAX
	RET

;***********************************************************************
;********************** Break Erg:Zeilenumbruch ************************
;***********************************************************************
break:
	push ax
	push bx
	mov ah,0eh
	mov bx,2
	mov al,13
	int 10h
	mov al,10
	int 10h
	pop bx
	pop ax
	ret
	
msg db 'max hat potential!',0