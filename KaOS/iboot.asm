;Allgemeine Informationen:
; -ein CD-Sektor ist 2048Bytes groß
; -das Bios lädt den Bootsekor immer an 0h:7c00h
; -der Stack ist nur ein Breich im Memory der durch SS:SP bestimmt wird
; -das Bios lädt das Laufwerk von dem es gebootet hat in DL
org 0h	; Unsere Startadresse
bits 16
start:
;Stack einrichten
cli
mov sp,0FFFFh	;Sackpointer = 0
mov ax,9000h	;Stacksegment = 9000h
mov ss,ax		;Stack Segment setzen
sti

push cs			;CS contain the segment of this program
pop ds			; ...so it contains also the segment for dap
mov ax,1000h	;set ES to 1000h in the indirect way via AX
mov es,ax		;ES segment of loaded block from cd

call $+3
pop bp
sub bp,($-$$)-1
mov ax,cs
mov bx,16
mov cx,bp
add cx,zahlausgabe
call cx
mov ax,0e3ah
int 10h
mov ax,bp
mov cx,bp
add cx,zahlausgabe
call cx
mov ax,0e0dh
int 10h
mov al,0ah
int 10h
lea si,[bp+msg_loading]
mov cx,bp
add cx,write
call cx

;load volume descriptor to 1000h:0h
load_vol_desc:
	lea si,[bp+dap]			;Zeiger SI auf "Disk Acces Paket"
	mov ah,42h				;advanced read out of disk to memory
	int 13h					;load the lba which stands in the dap into the location which stands also in the dap
	jnc .go					;if an error occurs go to write error message
		mov cx,bp
		add cx,errors.err_read
		jmp cx
	.go:
	cmp byte[es:0],1		;1 signs primary volume - see also iso9660
	je .end_vol_desc		;is this the primary volume descriptor, if yes jump
	cmp byte[es:0],0ffh
	jne .go2				;is this the last volume descriptor, if yes jump
		mov cx,bp
		add cx,errors.err_read
		jmp cx
	.go2:
	
	inc dword[bp+dap.lba]		;increase var for lba in dap
	mov cx,bp
	add cx,load_vol_desc
	jmp cx						;go to loop sothat the next lba will be searched for primary volume
.end_vol_desc:

mov eax,dword[es:iso9660vol_desc.root_directory_record+iso9660vol_desc.root_directory_record.location_of_data]	;the start lba of the root directory
mov ecx,dword[es:iso9660vol_desc.root_directory_record+iso9660vol_desc.root_directory_record.data_length]		;the length of the root directory
shr ecx,11				;length of root dir / 2048, because we need the number of blocks
inc cx
mov [cs:bp+dap.blocks],cx		;save number of blocks 2 load
mov [cs:bp+dap.lba],eax		;save lba to load
lea si,[bp+dap]
mov ah,42h
int 13h
	jc errors.err_read

;===============================================================================================
load_loader:
	xor si,si
	shl ecx,11					;in ecx is the number of blocks of the root dir, but we need the size

.loop_search:
	cmp byte[es:si],0
		je errors.err_no_loader					;is this no root dir entry, if yes jump
	cmp si,cx
		je errors.err_no_loader					;is si equ size of root dir, if yes jump
	mov di,si
	add di,iso9660vol_desc.root_directory_record.file_id				;move the addr of the act entry into di and let it point to the file name

	push cx
	push si
	movzx cx,byte[es:si+iso9660vol_desc.root_directory_record.length_file_id]			;get the size of the file indentifier
	lea si,[bp+osloader_img]
	rep cmpsb
	mov al,[cs:si]
	pop si
	pop cx

	test al,al
		jz .load
	movzx ax,byte[es:si+iso9660vol_desc.root_directory_record.length_directory_record]
	add si,ax					;get size of the act entry and add it to si, so that si points to the next entry
	mov cx,bp
	add cx,.loop_search
	jmp cx

.load:
	mov eax,dword[es:si+iso9660vol_desc.root_directory_record.location_of_data]	;start lba of the file in EAX
	mov ecx,dword[es:si+iso9660vol_desc.root_directory_record.data_length]		;length of the file in ECX
	shr ecx,11					;because the length of file is in byte, ecx is divided with 2048, sothat it is Kilobytes
	inc cx
	mov [cs:bp+dap.blocks],cx			;save number of blocks 2 load
	mov [cs:bp+dap.lba],eax			;save lba to load
	lea si,[bp+dap]
	mov ah,42h
	int 13h

	lea si,[bp+msg_loading_ok]		;Offset der Loading-Nachricht
	mov cx,bp
	add cx,write
	call cx					;Nachricht an Offset SI ausgeben
	
	mov ax,1000h				;set AX to 1000h
	mov ds,ax					;update DS with AX
	jmp 1000h:0000h				;Jump to program in memory

;==========================================================================================================================

errors:
;----------------------------
.err_read:
	mov ax,cs
	mov ds,ax
	mov es,ax
	inc dl
	lea si,[msg_err_read]
	jmp near .print_err_msg
;----------------------------
.err_no_loader:
	mov ax,cs
	mov ds,ax
	mov es,ax
	lea si,[msg_err_loader]
	.print_err_msg
		call write
		lea si,[error_code]
		call write
		movzx eax,ah
		mov bx,16
		call zahlausgabe
		lea si,[msg_drive]
		call write
		movzx eax,dl
		call zahlausgabe
		xor ax,ax
		int 16h
		int 19h

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	pusha
	mov ah,0Eh		; Funktion 0x0E	
	mov bx,2
	mov cx,bp
	add cx,.loop
.loop:	
	lodsb			; Byte laden
	or al,al
	jz .end			; 0-Byte? -> Ende!
	int 10h			; schreiben
	jmp cx			; Nästes Byte
.end:
	popa
	ret

;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pushad
	xor ecx,ecx
	or ebx,ebx
	jnz .loop
		mov ebx,16
.loop:
	xor edx,edx
	div ebx
	push dx
	inc cx
	or eax,eax
	JNE .loop

	mov ah,0eh
	mov bx,2
	.loop_print:
		pop dx
		cmp dx,10
		jb .zahl
			add dl,7
			.zahl:
				add dl,48
				mov al,dl
				int 10h
		dec cx
	jnz .loop_print
	popad
	ret

;================================================vars================================================
dap:
	.size		db 10h
	.reserved	db 0
	.blocks		dw 1			;amount of transfering block
	.offset		dw 0			;Transfer Buffer
	.segment	dw 1000h		;	~
	.lba		dd 10h,0

;================================================consts================================================
osloader_img	db 'kernel',0

;================================================messages================================================
msg_err_read	db 'diskreading error',13,10,0
msg_err_loader	db 'OS-Loader not found',13,10,0
msg_loading		db 'Booting...',0
msg_loading_ok	db 'OK',13,10,0
error_code		db 'Error Code:',0
msg_drive		db ' Drive:',0
;================================================references================================================
iso9660vol_desc:
	.root_directory_record			equ	156
		.root_directory_record.length_directory_record			equ	0
		.root_directory_record.location_of_data					equ	2
		.root_directory_record.data_length						equ	10
		.root_directory_record.length_file_id					equ	32
		.root_directory_record.file_id							equ	33

; times 2048-2-($-$$) db 0
; dw 0aa55h