;mit NASM compiliert
org 100h			;Segmentregister updaten
mov ax,0002h
int 10h

call install_timer	;installiert Timer an Int8h mit 100Hz
;------------------program timer start-----------------------
repeat: 
 xor ax,ax			;Funktion 0
 int 16h			;	...lese Tastendruck al=Ascii ah=scancode
 cmp al,13			;wenn der Ascii-code nicht 13(Enter) ist
 jne repeat			;	...dann warte auf nächsten Tastendruck
;------------------program timer end-------------------------  
 call clean_up		;stellt alles auf normal
 mov dx,1800h		;x=0	y=18h
 mov ah,02h			;Funktion 2h
 xor bh,bh			;Seite 0
 int 10h			; Gehe zu xy
 
 mov eax,[timer]	;lade in eax die timer-variable
 mov bx,10			;setze Zahlensystem auf 10
 call zahlausgabe	;gib zahl in eax aus
 
 xor ax,ax			;warte auf tastendruck
 int 16h			;	...dann mache weiter

 retf

;***********************************************************************
;**************************** install_timer ****************************
;***********************************************************************
install_timer:
 pushad						;Register speichern
 push es					;	~

 xor ax,ax					;in ax Segment des IVT speichern
 mov es,ax					;in es ax kopieren
 mov di,32					;Offset 32 ist int 8h
 mov eax,[es:di]			;Int-Vector von 8h laden
 mov [old_int_8],eax		;	...und temporär speichern

 lea si,[msg1]				;si offset Nachricht1 speichern
 call writeln				;String an Offset si bis 0 ausgeben
 lea si,[msg2]				;si offset Nachricht2 speichern
 call writeln				;String an Offset si bis 0 ausgeben
 lea si,[msg3]				;si offset Nachricht3 speichern
 call writeln				;String an Offset si bis 0 ausgeben

 CLI						;Interrupts blockieren (zum umstellen)
 xor ax,ax					;in ax Segment des IVT speichern
 mov es,ax					;in es ax kopieren
 mov di,32					;Offset 32 ist int 8h
 mov ax,cs					;auf ax Segment des eigenen Int-Handlers verschieben
 shl eax,16					;ax in den high-teil von eax verschieben
 lea ax,[int_handler]		;auf ax offset des eigenen Int-Handlers verschieben
 mov [es:di],eax			;eax in IVT anstelle Int8h's eintragen (eax=Segment:Offset	32Bit)
 STI						;Interrupts wieder freigeben
 mov bx,16					;Zahlensystem auf Hexadecimal-Zahlen umstellen
 call zahlausgabe			;Int-Vector von eigenen Int-Handler ausgeben
 
 mov al,34h					;auf al Byte verschieben um es an einen Port zu senden
 out 43h,al					;Byte wählt Zähler 0 im PIT-Port an
 mov al,0h					;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al					;Byte gibt den low-teil des teilers an, der benötigt wird um die Frequenz einzustellen
 mov al,0h					;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al					;Byte sendet high-teil des teilers

 pop es						;Register wiederherstellen
 popad						;	~
 ret

;***********************************************************************
;***************************** int_handler *****************************
;***********************************************************************
int_handler:
	 ; insert interrupt code here
	 pushad
	 push es
	 push ds
	 
	 mov ax,cs
	 mov es,ax
	 mov ds,ax
	 
	 pusha
	 call sound_on
	 call sound_off
	 popa
	 
	 inc dword[timer]
	 mov ebx,10
	 
	 mov eax,[timer]
	 mov cx,0B800h
	 mov es,cx
	 mov di,158
	 std
	 mov ebx,10
	 
	 mov ecx,100
	 call zahlausgabe_timer
	 mov ecx,60
	 call zahlausgabe_timer
	 mov ecx,60
	 call zahlausgabe_timer
	 mov ecx,24
	 mov dl,1
	 call zahlausgabe_timer
	 add di,2
	 mov ax,0700h
	 stosw

	 pop ds
	 pop es
	 ; insert interrupt code here

	 dec word[count]			;call original interrupt as required(on Windows use)
	 jz oldint					;call original interrupt as required(on Windows use)
eoi:	 
	 mov al,20h					;if not, send EOI to PIC
	 out 20h,al
	 popad
	 iret
timer dd 0
old_int_8 dd 0
count dw 2E9Ch
oldint:
	 push ds
	 push si

	 mov word[count],2E9Ch			;reset counter
	 mov ax,cs
	 mov ds,ax
	 lea si,[old_int_8]
	 mov es,[ds:si]
	 add si,2
	 mov di,[ds:si]

	 pop si
	 pop ds
	 popad
	 jmp [es:di]					;original vector will send EOI to PIC

zahlausgabe_timer:
	xor edx,edx
	div ecx
	push eax
	mov eax,edx
	mov ecx,2
	zahlausgabe_timer_handler:
		xor edx,edx
		div ebx
		push ax
		mov ah,2
		mov al,dl
		add al,48
		stosw
		pop ax
		loop zahlausgabe_timer_handler
	mov ax,023ah
	stosw
	xor edx,edx
	pop eax
	ret

;***********************************************************************
;****************************** Clean up *******************************
;***********************************************************************
clean_up:
 pushad					;Register speichern
 push es				;	~

 mov al,34h				;Byte auf al kopieren um es danach an Port zu senden
 out 43h,al				;Byte an PIT senden; wählt Timer0 an
 xor al,al				;Byte löschen
 out 40h,al				;Byte an PIT senden; setzt low-teil des teilers auf 0
 out 40h,al				;Byte an PIT senden; setzt high-teil des teilers auf 0
						;da der Teiler 0 ist und die Frequenz eigentlich unendlich wäre heißt 0 in diesem Fall (1)0000h, was eine Frequenz von 18,20648193359375
						;	...ergibt
 xor ax,ax				;ax löschen (IVT Segment = 0)
 mov es,ax				;es ax, welches das Segment des IVT beinhaltet, zuweisen
 mov di,32				;Offset von Int8h in IVT
 mov eax,[old_int_8]	;alte Adresse des Interrupt 8h's laden
 mov [es:di],eax		;Int-Vector in IVT eintragen

 pop es					;Register wiederherstellen
 popad					;	~
 ret

msg1 db 'Alten Interrupt Vector (Int 08h) gespeichert...',0
msg3 db 'Interrupt Vector auf Programm verbogen...',0
msg2 db 'PIT auf 100Hz bei IRQ0 gestellt (timer-Interrupt)...',0

;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pushad
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
 
	popad
	ret

;***********************************************************************
;************* writeln ds:si String bx:farbe Ausgabe:String ************
;***********************************************************************
writeln:
	push ax			;Register speichern
	push bx			;	~
	push si			;	~
	mov ah,0Eh		;Funktion 0x0E	
writeln2:	
	lodsb			;Byte laden
	or al,al		;wenn al=0
	jz writelnd		;	...dann gehe zu writelnd
	int 10h			;schreiben
	jmp writeln2	;Nästes Byte
writelnd:
	mov ah,0eh		;Teletype-Output (BIOS)
	mov bx,2		;Farbe grün (geht nur im Video-Modus -> angewohnheit)
	mov al,13		;Zeichen mit Ascii-code 13
	int 10h			;	...ausgeben
	mov al,10		;Zeichen mit Ascii-code 10
	int 10h			;	...ausgeben
					;beide Zeichen hintereinander erzeugen einen Zeilenumbruch
	pop si			;Register wiederherstellen
	pop bx			;	~
	pop ax			;	~
	ret


sound_off:
IN AL,61h
AND AL,11111100b
OUT 61h,AL
RET

sound_on:
MOV AX,34DDH
MOV DX,0012H
CMP DX,BX
JNC DONE1
DIV BX
MOV BX,AX
IN AL,61H
TEST AL,3
JNZ A99
OR AL,3
OUT 61H,AL
MOV AL,0B6H
OUT 43H,AL
A99:
MOV AL,BL
OUT 42H,AL
MOV AL,BH
OUT 42H,AL
DONE1:
ret
