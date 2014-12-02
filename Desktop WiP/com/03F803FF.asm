[org 100h]
mov dx,[COM1]
call init

mov dx,011Eh
call gotoxy
lea si,[msg_key1]
call write
mov dx,021Eh
call gotoxy
lea si,[msg_key2]
call write
mov dx,031Eh
call gotoxy
lea si,[msg_key3]
call write

com_control:
 call eingaenge
 call ausgaenge
 xor ax,ax
 int 16h

 cmp al,49
 jne not_1
  mov bl,[com_rts]
  or bl,bl
  jnz com_rts_1
   mov bl,2
  com_rts_1:
   dec bl
  mov [com_rts],bl
  mov dx,[COM1]
  call rts
 
 not_1:
 cmp al,50
 jne not_2
  mov bl,[com_dtr]
  or bl,bl
  jnz com_dtr_1
   mov bl,2
  com_dtr_1:
   dec bl
  mov [com_dtr],bl
  mov dx,[COM1]
  call dtr
 
 not_2:
 cmp al,51
 jne not_3
  mov bl,[com_txd]
  or bl,bl
  jnz com_txd_1
   mov bl,2
  com_txd_1:
   dec bl
  mov [com_txd],bl
  mov dx,[COM1]
  call txd
 
 not_3:
 cmp al,27
jne com_control

mov ah,4ch
int 21h

com_rts db 0
com_dtr db 0
com_txd db 0

COM1 dw 03F8h
COM2 dw 02F8h
COM3 dw 03E8h
COM4 dw 02E8h

;***********************************************************************
;****************** init dx:basisadresse des com-port ******************
;***********************************************************************
init:
 pusha
 mov bx,dx
 
 add dx,3
 mov al,128
 out dx,al
 
 mov dx,bx
 mov al,6
 out dx,al		;19200 Baud
 
 inc dx
 xor ax,ax
 out dx,al
 
 mov dx,bx
 add dx,3
 mov al,7
 out dx,al		;8-Bit, n-Parity, 2 Stopbits
 
 mov dx,bx
 inc dx
 xor ax,ax
 out dx,al		;keine Interrupts
 
 add dx,3
 out dx,al 		;DTR = 0, RTS = 0

 mov dx,bx
 mov cx,3
 dummy:			;UART leeren
  in al,dx
 loop dummy
 
 popa
ret

;***********************************************************************
;****************** sende bx : Byte dx : COM-adresse *******************
;***********************************************************************

sende:
 pusha
 mov ax,bx
 out dx,al		;Port[BA]:=Zeichen;
 
 add dx,5
 in al,dx
 sub dx,5
 and al,00100000b	;Sende-Halteregister leer?
 jz sende			;while (Port[BA+5] AND 32) = 0 do
 popa
ret

;***********************************************************************
;******************** dcd al : dcd dx : COM-adresse ********************
;***********************************************************************
dcd:
 push bx
 push cx
 push dx
 
 add dx,6
 in al,dx

 mov cx,7
 mov bl,al
 xor bh,bh
 xor ax,ax

 bt bx,cx
 setc al
 
 pop dx
 pop cx
 pop bx
ret

;***********************************************************************
;********************* ri al : ri dx : COM-adresse *********************
;***********************************************************************
ri:
 push bx
 push cx
 push dx
 
 add dx,6
 in al,dx

 mov cx,6
 mov bl,al
 xor bh,bh
 xor ax,ax

 bt bx,cx
 setc al
 
 pop dx
 pop cx
 pop bx
ret

;***********************************************************************
;******************** dsr al : dsr dx : COM-adresse ********************
;***********************************************************************
dsr:
 push bx
 push cx
 push dx
 
 add dx,6
 in al,dx

 mov cx,5
 mov bl,al
 xor bh,bh
 xor ax,ax

 bt bx,cx
 setc al
 
 pop dx
 pop cx
 pop bx
ret

;***********************************************************************
;******************** cts al : cts dx : COM-adresse ********************
;***********************************************************************
cts:
 push bx
 push cx
 push dx
 
 add dx,6
 in al,dx

 mov cx,4
 mov bl,al
 xor bh,bh
 xor ax,ax

 bt bx,cx
 setc al
 
 pop dx
 pop cx
 pop bx
ret

;***********************************************************************
;******************** rxd al : rxd dx : COM-adresse ********************
;***********************************************************************
rxd:
 push bx
 push cx
 push dx
 
 add dx,5
 in al,dx

 mov cx,4
 mov bl,al
 xor bh,bh
 xor ax,ax

 bt bx,cx
 setc al
 
 pop dx
 pop cx
 pop bx
ret

;***********************************************************************
;****************** rts bl : rts; dx : COM-adresse *********************
;***********************************************************************
rts:
 cmp bl,3
 jne rts_write
  push cx
  push ax

  add dx,4
  in al,dx
  mov cx,1
  mov bl,al
  xor bh,bh
  xor ax,ax
  bt bx,cx
  setc al
  mov bl,al

  pop ax
  pop cx
  ret

 rts_write:
 pusha
 add dx,4
 in al,dx 
 
 or bl,bl
 jz loesch
  or al,00000010b
  jmp fertig
 loesch:
  and al,11111101b
 fertig:
 
 out dx,al
 popa
ret

;***********************************************************************
;****************** dtr bl : dtr; dx : COM-adresse *********************
;***********************************************************************
dtr:
 cmp bl,3
 jne dtr_write
  push cx
  push ax

  add dx,4
  in al,dx
  xor cx,cx
  mov bl,al
  xor bh,bh
  xor ax,ax
  bt bx,cx   
  setc al
  mov bl,al
  
  pop ax
  pop cx
  ret
  
 dtr_write: 
 pusha
 add dx,4
 in al,dx

 or bl,bl
 jz loesch_dtr
  or al,00000001b
  jmp fertig_dtr
 loesch_dtr:
  and al,11111110b
 fertig_dtr:
 
 out dx,al
 popa
ret

;***********************************************************************
;****************** txd bl : txd; dx : COM-adresse *********************
;***********************************************************************
txd:
 cmp bl,3
 jne txd_write
  push cx
  push ax

  add dx,3
  in al,dx
  mov cx,6
  mov bl,al
  xor bh,bh
  xor ax,ax
  bt bx,cx   
  setc al
  mov bl,al
  
  pop ax
  pop cx
  ret 
 
 txd_write:
 pusha
 add dx,3
 in al,dx

 or bl,bl
 jz loesch_txd
  or al,01000000b
  jmp fertig_txd
 loesch_txd:
  and al,10111111b
 fertig_txd:
 
 out dx,al
 popa
ret

;***********************************************************************
;************** Zahlausgabe ax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pusha
	XOR CX,CX
zahlausgabe1:
	XOR DX,DX
	DIV BX
	PUSH DX
	INC CX
	OR AX,AX
	JNE zahlausgabe1

	MOV AH,0Eh
	MOV BX,2
	zahlausgabe2:
	POP DX
	CMP DX,10
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
	push ax
	push si
	mov ah,0Eh		; Funktion 0x0E	

writeln2:	
	lodsb			; Byte laden
	or al,al
	jz writelnd		; 0-Byte? -> Ende!

	
	int 10h			; schreiben
	jmp writeln2		; N?stes Byte
writelnd:
	
	mov ah,0eh
	mov bx,2
	mov al,13
	int 10h
	mov al,10
	int 10h
	
	pop si
	pop ax
	ret	

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	push ax
	push si
	mov ah,0Eh		; Funktion 0x0E	

write2:	
	lodsb			; Byte laden
	or al,al
	jz writed			; 0-Byte? -> Ende!

	
	int 10h			; schreiben
	jmp write2		; N?stes Byte
writed:
	pop si
	pop ax
	ret

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

;***********************************************************************
;************************* gotoxy x:dl y:dh ****************************
;***********************************************************************	
gotoxy:
	push ax
	push bx
	mov ah,02h
	xor bh,bh
	int 10h
	pop bx
	pop ax
ret

;***********************************************************************
;************************ wherexy x:dl y:dh ****************************
;***********************************************************************	
wherexy:
	push ax
	push bx
	mov ah,03h
	xor bh,bh
	int 10h
	pop bx
	pop ax
ret

;***********************************************************************
;**************************** Eingaenge ********************************
;***********************************************************************
eingaenge:
 pusha
 call wherexy
 push dx
 
 mov dx,0723h
 call gotoxy
 lea si,[msg_ri]
 call write
 mov dx,[COM1]
 call ri
 or al,al
 jz ri_aus
  lea si,[msg1]
  jmp ri_ende
 ri_aus:
  lea si,[msg2]
 ri_ende:
 call write 

 mov dx,0823h
 call gotoxy
 lea si,[msg_cts]
 call write
 mov dx,[COM1]
 call cts
 or al,al
 jz cts_aus
  lea si,[msg1]
  jmp cts_ende
 cts_aus:
  lea si,[msg2]
 cts_ende:
 call write

 mov dx,0923h
 call gotoxy
 lea si,[msg_dsr]
 call write
 mov dx,[COM1]
 call dsr
 or al,al
 jz dsr_aus
  lea si,[msg1]
  jmp dsr_ende
 dsr_aus:
  lea si,[msg2]
 dsr_ende:
 call write
 
 mov dx,0A23h
 call gotoxy
 lea si,[msg_rxd]
 call write
 mov dx,[COM1]
 call rxd
 or al,al
 jz rxd_aus
  lea si,[msg1]
  jmp rxd_ende
 rxd_aus:
  lea si,[msg2]
 rxd_ende:
 call write
 
 mov dx,0B23h
 call gotoxy
 lea si,[msg_dcd]
 call write
 mov dx,[COM1]
 call dcd
 or al,al
 jz dcd_aus
  lea si,[msg1]
  jmp dcd_ende
 dcd_aus:
  lea si,[msg2]
 dcd_ende:
 call write
 
 pop dx
 call gotoxy
 popa
ret

;***********************************************************************
;**************************** Ausgaenge ********************************
;***********************************************************************
ausgaenge:
 pusha
 call wherexy
 push dx
 
 mov dx,0D23h
 call gotoxy
 lea si,[msg_rts]
 call write
 mov dx,[COM1]
 mov bl,3
 call rts
 or bl,bl
 jz rts_aus
  lea si,[msg1]
  jmp rts_ende
 rts_aus:
  lea si,[msg2]
 rts_ende:
 call write 

 mov dx,0E23h
 call gotoxy
 lea si,[msg_dtr]
 call write
 mov dx,[COM1]
 mov bl,3
 call dtr
 or bl,bl
 jz dtr_aus
  lea si,[msg1]
  jmp dtr_ende
 dtr_aus:
  lea si,[msg2]
 dtr_ende:
 call write
 
 mov dx,0F23h
 call gotoxy
 lea si,[msg_txd]
 call write
 mov dx,[COM1]
 mov bl,3
 call txd
 or bl,bl
 jz txd_aus
  lea si,[msg1]
  jmp txd_ende
 txd_aus:
  lea si,[msg2]
 txd_ende:
 call write

 pop dx
 call gotoxy
 popa
ret

msg1    db 'AN ',0
msg2    db 'AUS',0
msg_ri  db 'ri  :',0
msg_cts db 'cts :',0
msg_dsr db 'dsr :',0
msg_rxd db 'rxd :',0
msg_dcd db 'dcd :',0

msg_rts db 'rts :',0
msg_dtr db 'dtr :',0
msg_txd db 'txd :',0

msg_key1 db 'Negiere rts : <1>',0
msg_key2 db 'Negiere dtr : <2>',0
msg_key3 db 'Negiere txd : <3>',0
