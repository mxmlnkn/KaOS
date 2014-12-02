org 100h
jmp word after_timer
timer dd 0
timer_installed db 0
after_timer:
	cmp byte[cs:timer_installed],0
	jz install_timer
		jmp clean_up
;***********************************************************************
;**************************** install_timer ****************************
;***********************************************************************
install_timer:
 pushad							;Register speichern
 push es						;	~
 push ds						;	~

 push cs						;Segmentregister aktualisieren
 pop ds							;	~
 lea si,[msg_start]				;Adresse der Bootmessage speichern
 call writeln					; Schicke Message :)
 mov dword[timer],0				;Timer-Variable löschen

 xor al,al						;al löschen, als zeichen, das alles ok ist
 cmp ebp,0FFh					;prüfen, ob ebp in der möglichen spanne von FFh interrupts liegt
	seta al						;wenn ebp außerhalb des möglichen bereiches ist, setze al, als zeichen für fehler
	ja inst_timer_end			;und springe zum Ende der Install-Prozedure
 
 CLI							;Interrupts blockieren (zum umstellen)
 xor ax,ax						;in ax Segment des IVT speichern
 mov es,ax						;in es ax kopieren
 mov eax,[es:4*ebp]				;Int-Vector von 8h laden
 mov [old_int_8],eax			;	...und temporär speichern
 mov word[es:4*ebp+2],cs			;auf low-word des IVT(8h) Segment des eigenen Int-Handlers verschieben
 mov word[es:4*ebp],int_handler	;auf high-word des IVT(8h) Offset des eigenen Int-Handlers verschieben
 STI							;Interrupts wieder freigeben

 mov al,34h						;auf al Byte verschieben um es an einen Port zu senden
 out 43h,al						;Byte wählt Zähler 0 im PIT-Port an
 mov al,9Ch						;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al						;Byte gibt den low-teil des teilers an, der benötigt wird um die Frequenz einzustellen
 mov al,2Eh						;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al						;Byte sendet high-teil des teilers

 mov byte[cs:timer_installed],1	;vermerke, dass dieser Timer instaliert wurde
 
 inst_timer_end:
 pop ds							;Register wiederherstellen
 pop es							;	~
 popad							;	~
 retf
times 512-($$-$) db 'i'

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
 mov eax,[cs:old_int_8]	;alte Adresse des Interrupt 8h's laden
 mov [es:4*8h],eax		;Int-Vector in IVT eintragen

 mov byte[cs:timer_installed],0
 
 pop es					;Register wiederherstellen
 popad					;	~
 ret

; 1193180/1000000=1,193180
; EDX:EAX div EBX
; EDX:EAX=timer2*1000000
; EBX=1193180
;***********************************************************************
;***************************** int_handler *****************************
;***********************************************************************
int_handler:
	 ; insert interrupt code here
	 pushad
	 push es
	 push ds
	 
	 push cs
	 pop ds
	 
	 inc dword[timer]
	 mov eax,[timer]
	 push word 0B800h
	 pop es
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
	 
	 mov al,20h					;if not, send EOI to PIC
	 out 20h,al
	 popad
	 iret
old_int_8 dd 0			;original vector will send EOI to PIC
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
;************* writeln ds:si String bx:farbe Ausgabe:String ************
;***********************************************************************
writeln:
	pusha
	pushf
	cld
	mov ah,0Eh		; Funktion 0x0E	
writeln2:	
	lodsb			; Byte laden
	or al,al
	jz writelnd		; 0-Byte? -> Ende!
	int 10h			; schreiben
	jmp writeln2	; N?stes Byte
writelnd:
	mov al,13
	int 10h
	mov al,10
	int 10h
	popf
	popa
	ret	
	
msg_start db 'PIT auf 100Hz bei IRQ0 gestellt (timer-Interrupt)...OK',0
