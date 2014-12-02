;######################################
;### in bp wird von install_keyboard die interruptnr. von ####
;### dem INT erwartet, an den der KeyboardIRQ hängt #####
;######################################
org 100h
call install_keyboard
retf

; push ds
; push es
; pushad

; call install_keyboard

; push cs
; pop ds
; push cs
; pop es

; mov ah,3
; mov bx,2
; int 10h
; mov [x],dl
; mov [y],dh

; write_buffer_loop:
	; lea si,[key_buffer]
	; mov cx,[buffer_pointer]
	
	; call clrscr
	; mov ah,2
	; mov dl,[x]
	; mov dh,[y]
	; int 10h
	
	; write_buffer:
		; xor eax,eax
		; lodsb
		; mov bx,10
		; call zahlausgabe
		; mov ax,0e3ah
		; int 10h
		; loop write_buffer
		
	; call break
	; call break
	; lea si,[last2keys]
	; xor eax,eax
	; lodsb
	; call zahlausgabe
	; call break
	; lodsb
	; call zahlausgabe

	; xor ax,ax
	; int 16h
	; cmp ah,1
	; jne write_buffer_loop
	
; popad
; pop es
; pop ds
; retf

; x db 0
; y db 0

	; ***********************************************************************
	; ********************** Break Erg:Zeilenumbruch ************************
	; ***********************************************************************
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

	; ***********************************************************************
	; ************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
	; ***********************************************************************
	zahlausgabe:
		pushad
		XOR ECX,ECX
		or ebx,ebx
		jnz zahlausgabe1
			mov ebx,16
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
		
	; ***********************************************************************
	; *********************** clrscr löscht Bildschirm **********************
	; ***********************************************************************
	; clrscr:
		; pushad
		; push es
		; pushf

		; cld
		; mov ax,0B800h
		; mov es,ax
		; xor di,di
		; mov cx,1000
		; mov eax,07000700h
		; rep stosd
		; mov ah,02h
		; xor bh,bh
		; xor dx,dx
		; int 10h

		; popf
		; pop es
		; popad
		; ret

;***********************************************************************
;********************* install_keyboard *********************
;***********************************************************************
install_keyboard:
	push word 0					;0 auf Stack speichern
	pop ds						;	...um es in DS zu speichern
	CLI							;Interrupt deaktivieren
	mov al,0f0h					;Befehl für Scancode-Set wählen
	out 60h,al					;Befehl senden
	call kb.checkcmd			;prüfe ob Befehel ordnungsgemäß verarbeitet wurde
	call kb.read				;hole Bestätigung (FAh)
	mov al,02h					;Scancode-Set2 (default)
	out 60h,al					;Wahl senden
	call kb.checkcmd			;prüfe ob Befehel ordnungsgemäß verarbeitet wurde
	call kb.read				;hole Bestätigung (FAh)
	mov al,0aeh					;kopiere auf al befehl zum aktivieren des Keyboards
	out 64h,al					;sende den Befehl an den KC
	call kb.checkcmd			;prüfe ob Befehel ordnungsgemäß verarbeitet wurde

	; mov eax,ebp
	; mov ebx,16
	; call zahlausgabe
	
	xor al,al					;al löschen, als zeichen, das alles ok ist
	cmp ebp,0FFh				;prüfen, ob ebp in der möglichen spanne von FFh interrupts liegt
		seta al					;wenn ebp außerhalb des möglichen bereiches ist, setze al, als zeichen für fehler
		ja inst_kb_end			;und springe zum Ende der Install-Prozedure
	mov word[ds:4*ebp],int9h	;in IVT den Offset von int9h bei dem Platz für den IRQ1 eintragen 
	mov word[ds:4*ebp+2],cs		;in IVT das Segment von int9h bei dem Platz für den IRQ1 eintragen
	mov word[ds:4*16h],int16h	;in IVT den Offset von int16h bei dem Platz für den Int16h eintragen
	mov word[ds:4*16h+2],cs		;in IVT das Segment von int16h bei dem Platz für den Int16h eintragen
	STI							;Interrupts aktivieren
	
	push cs						;cs auf stack legen
	pop ds						;um es nach ds zu kopieren
	lea si,[msg_start]			;Offset der Message auf SI
	call write					;Prozedure Ausgabe
inst_kb_end:
	ret							;Prozedure verlassen

;***************************************************************************************************************
;dies sind Prozeduren die speziell Befehle an das Keyboard schicken oder Daten entgegennehmen, da man immer warten muss, bis
;welche da sind. diese Prozeduren werden überwiegend für die Mausinitialisierung und für den IRQ12 benötigt, der die Pakete holt
;***************************************************************************************************************
kb:
.read:					;wartet, bis im Output-Buffer des Port60h Daten bereitliegen und holt diese dann ab
	push ecx			;ecx sichern, da das für loop benötigt wird
	xor ecx,ecx			;auf ecx, ffffh schieben, damit hier nicht unendlich lang auf Daten gewartet wird
	.wait_for_output:
		in al,64h					;liest das Statusregister vom Keyboard-Controller aus
		test al,00000001b			;testen ob im Outputbuffer Daten sind
			jnz .output_is_there	;wenn ja, dann springe aus der Schleife raus und lese die Daten ein
		loop .wait_for_output		;wenn nicht, also Bit0 nicht gesetzt ist, wiederhole schleife und warte weiter auf Daten
			mov ah,1				;wenn loop durchgelaufen ist und noch immer keine Daten da sind, schiebe auf ah errorcode
			or ah,ah				;und setze ggf. das zeroflag, damit der user gleich danach jz o.ä. schreiben kann
			pop ecx					;stelle register wieder her
			ret						;und springe zum aufrufsort zurück
	.output_is_there:
	in al,60h			;hole Daten aus dem Output-Buffer
	xor ah,ah			;und lösche ah, da bei fehler auf ah der errorcode ist; setze ggf. zeroflag (macht xor...)
	pop ecx				;stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück
;************************* Variablen des IRQ12 *************************
.checkcmd:					;prüft, ob der Befehl auch angenommen wurde, oder noch immer im Buffer auf seine Verarbeitung wartet
	push ax					;ax sichern, da es beim einlesen des Statusbytes zerstört wird
	.loop_chkcmd:
		in al,64h			;nach al das Statusregister holen
		test al,00000010b	;und dort prüfen, ob Bit2, dass anzeigt, das etwas im out-buffer, gesetzt ist
		jnz .loop_chkcmd	;wenn nicht, wiederhole den vorgang, bis das byte verarbeitet ist
	pop ax					;stelle ax nun wieder her
	ret
;************************* wartet bis alle Befehle des KC verarbeitet sind *************************

;***********************************************************************
;********************* Interrupt 9h : Keyboard IRQ *********************
;***********************************************************************
	int9h:
		pusha										;Register speichern
		push es										;	~
		push ds										;	~
		
		mov ax,cs									;Auf ax,cs kopieren um segmentregister zu aktualisieren
		mov ds,ax									;segmentregister aktualisieren
		mov es,ax									;	~
		in al,64h									;statusbyte des keyboards abfragen
		test al,00000001b							;prüfen ob im kb-buffer ein zeichen liegt
		jnz save_scancode							;	...wenn ja hole und speichere es
			jmp int9h_ende							;	...ansonsten leite ende-sequenz ein
		save_scancode:
			in al,60h								;	...ansonsten hole dieses Zeichen
			cmp al,224
				je int9h_ende
			mov [scan],al							;und speichere es
		; lea esi,[kb_startled]
		; call write
		; movzx eax,al
		; mov ebx,16
		; call zahlausgabe

		cld											;bei lodsb bzw. stosb si,di erhöhen
		test al,10000000b							;wenn Taste losgelassen wurde
		jz taste_true								;	...speichere Taste
		taste_false:								;	...ansonsten lösche, dass Taste gedrückt wurde
			cmp word[buffer_pointer],1				;wenn buffer pointer 0 ist
				jbe	int9h_ende						;	...dann beende und warte das Taste eingegeben wird
			lea di,[key_buffer]						;Tasten Buffer merken (zum laden)
			and al,01111111b						;lösche diesen Vermerk um zu vergleichen
			mov cx,bufptr_limit						;auf den Counter die Anzahl der Zeichen, die im Buffer liegen könnten verschieben
			repne scasb								;solange suchen bis das Zeichen in AL gefunden wurde oder CX 0 ist
			dec di									;di decrementieren um auf das letzte überprüfte Zeichen zu springen
			cmp al,[es:di]							;testen ob Zeichen gleich sind oder CX doch bloß 0 ist
				jne int9h_ende						;	...wenn Zeichen nicht gleich sind, dann beende
			or cx,cx								;ist CX=0
			jz loesch_letzte_eingabe				;	...dann kann ich mir das löschendes Zeichens sparen. da es eh überschrieben wird
				mov si,di							;Tasten Buffer merken (zum speichern)
				inc si								;erhöhe SI um das Zeichen beim verschieben des Strings zu löschen
				rep movsb							;String verschieben
			loesch_letzte_eingabe:
				dec word[buffer_pointer]			;decrementiere buffer pointer, da ein Zeichen gelöscht wurde
				lea di,[key_buffer-1]				;kopiere auf DI den Anfang des Buffers
				add di,[buffer_pointer]				;	...addiere den Buffer Pointer dazu
				mov byte[es:di],0					;lösche das letzte Zeichen (ist noch übrig vom verschieben des Strings)
				jmp int9h_ende

		taste_true:
			cmp word[buffer_pointer],bufptr_limit	;wenn buffer pointer den Limit erreicht hat
				jae int9h_ende						;	...dann beende und warte bis Taste losgelassen wird
			lea di,[key_buffer]						;Tasten Buffer merken (zum laden)
			mov cx,bufptr_limit						;auf den Counter die Anzahl der Zeichen, die im Buffer liegen könnten verschieben
			repne scasb								;solange suchen bis das Zeichen in AL gefunden wurde oder CX 0 ist
			dec di									;di decrementieren um auf das letzte überprüfte Zeichen zu springen
			cmp al,[es:di]							;testen ob Zeichen gleich sind oder CX doch bloß 0 ist
				je int9h_ende						;	...wenn Zeichen nicht gleich sind, dann beende
			inc word[buffer_pointer]				;incrementiere buffer pointer, da ein Zeichen hinzugefügt wurde
			lea di,[key_buffer-2]					;kopiere auf DI den Anfang des Buffers
			add di,[buffer_pointer]					;	...addiere den Buffer Pointer dazu
			stosb									;speicher das Zeichen am Ende des Buffers

		int9h_ende:
			mov bl,[last2keys+1]					;letzte taste merken (in bl da in al jetzige Taste ist)
			mov [last2keys],bl						;bl als vorletzte taste speichern
			mov [last2keys+1],al					;	...und al als letzte Taste speichern
			
			mov al,20h								;kopiere auf al das byte für den EOI
			out 20h,al								;sende EOI
		pop ds										;Register wiederherstellen
		pop es										;	~
		popa										;	~
		iret										;springe zu adresse des aufrufs

	scan db 0
	bufptr_limit equ 101
	key_buffer times bufptr_limit db 0
	buffer_pointer dw 1
	last2keys dw 0

;***********************************************************************
;************ Interrupt 16h : Keyboard Interrupt bp:funktion ***********
;***********************************************************************
	int16h:
		pusha						;Register sichern
		cmp ah,2					;teste ob ax die nummer einer der funktionen enthält
		jb int16_funktion_vorhanden	;	...wenn ja mache weiter
			popa					;register wiederherstellen
			iret					;	...ansonsten springe zurück
		int16_funktion_vorhanden:
		
		STI							;Wichtig: Setze das Interrupt Flag wieder!!!!
		lea si,[int16h_procs]		;lade offset der offsetdatenbank der prozeduren
		movzx ax,ah					;auf ganz ax ah schieben, damit das mit si addiert werden kann
		add si,ax					;addiere den offset der datenbank mit der funktionsnummer(x2)
		add si,ax					;addiere den offset der datenbank mit der funktionsnummer(x2)
		mov ax,[cs:si]				;auf ax den Offset der Funktion kopieren, da kein direkter Speicher zu Speicher Transfer möglich ist
		mov [cs:funktion_jump],ax	;Offset der zu rufenden Funktion temporär speichern um Register wiederherzustellen
		
		popa						;Register wiederherstellen
		jmp [cs:funktion_jump]		;gehe zu der funktion (indirekter sprung)
	
	int16h_procs:					;offset  der offset-tafel der einzelnen prozeduren (hier ist platz für 2 unterprozeduren)
		dw	read_scancode			;Int 16/AH=00h - KEYBOARD - GET KEYSTROKE
		dw	check_scancode			;Int 16/AH=01h - KEYBOARD - CHECK FOR KEYSTROKE
		dw	get_shift_flags			;Int 16/AH=02h - KEYBOARD - GET SHIFT FLAGS
		dw	set_typematic_and_delay	;Int 16/AH=03h - KEYBOARD - SET TYPEMATIC RATE AND DELAY
		dw	0						;Int 16/AH=04h - KEYBOARD - SET KEYCLICK (PCjr only)
		dw	0						;Int 16/AH=09h - KEYBOARD - GET KEYBOARD FUNCTIONALITY
		dw	0						;Int 16/AH=0Ah - KEYBOARD - GET KEYBOARD ID
		dw	0						;Int 16/AH=10h - KEYBOARD - GET ENHANCED KEYSTROKE (enhanced kbd support only)
		dw	0						;Int 16/AH=11h - KEYBOARD - CHECK FOR ENHANCED KEYSTROKE (enh kbd support only)
		dw	0						;Int 16/AH=12h - KEYBOARD - GET EXTENDED SHIFT STATES (enh kbd support only)
	funktion_jump dw 0
;***********************************************************************
;************** read_scancode **************
;Eingabe:	AH=0
;Ausgabe:	AH=scancode
;		AL=Asciicode
;Veränderungen:	Flags,ax
;************** read_scancode **************
read_scancode:
	push ds							;Register speichern
	push cs							;	~
	pop ds							;	~
	read_scancode_loop:
		cmp byte[scan],0			;testen ob Scancode <> als 0 ist
		je read_scancode_loop		;	...wenn nicht, dann probiere es nocheinmal
			mov ah,[scan]			;	...wenn ja, dann speichere den scancode in ah
			mov byte[scan],0		;lösche Scancode
			test ah,10000000b		;testen ob Taste gedrückt wird oder losgelasen wurde
			jnz read_scancode_loop	;	... wenn Taste losgelassen wurde dann warte auf nächsten Scancode
				xchg bh,ah			;vertausche bh und ah
				call scan2ascii		;rufe Prozedure, die zu dem scancode den passenden Asciicode findet
				xchg bh,ah			;tausche ah und bh zurück
	pop ds							;Register wiederherstellen
	iret							;Interrupt beenden
;************** read_scancode_ende **************
;***********************************************************************

;***********************************************************************
;************** check_scancode **************
;Eingabe:	AH=01
;Ausgabe:	ZF=0 wenn Zeichen vorhanden
;		ZF=1 wenn kein  Zeichen vorhanden
;		AH=scancode
;		AL=Asciicode
;Veränderungen:	Flags,ax
;************** check_scancode **************
check_scancode:
	mov ah,[cs:scan]		;	...wenn ja, dann speichere den scancode in ah
	test ah,10000000b		;testen ob Taste gedrückt wird oder losgelasen wurde
	jz key_pressed			;	... wenn Taste losgelassen wurde dann nimm Scancode nicht an -> lösche ax
		xor ax,ax			;da nur eine Taste losgelassen wurde, oder nichtmal eine gedrückt wurde, setze ah und al auf 0
		iret				;Interrupt beenden
	key_pressed:
		xchg bh,ah			;vertausche bh und ah
		call scan2ascii		;rufe Prozedure, die zu dem scancode den passenden Asciicode findet
		xchg bh,ah			;tausche ah und bh zurück
	iret					;Interrupt beenden
;************** check_scancode_ende **************

;***********************************************************************
;************** get_shift_flags **************
;Eingabe:	AH=02
;Ausgabe:	AL=Shift Flags
;Veränderungen:	Flags,ax
;************** get_shift_flags **************
get_shift_flags:

	iret							;Interrupt beenden
;************** get_shift_flags_ende **************

;***********************************************************************

;***********************************************************************
;************** set_typematic_and_delay **************
;Eingabe:	AH = 03h
;		AL = subfunction
;			00h set default delay and rate (PCjr and some PS/2)
;			01h increase delay before repeat (PCjr)
;			02h decrease repeat rate by factor of 2 (PCjr)
;			03h increase delay and decrease repeat rate (PCjr)
;			04h turn off typematic repeat (PCjr and some PS/2)
;			05h set repeat rate and delay (AT,PS)
;				BH = delay value (00h = 250ms to 03h = 1000ms)
;				BL = repeat rate (00h=30/sec to 0Ch=10/sec [def] to 1Fh=2/sec)
;			06h get current typematic rate and delay (newer PS/2s)
;Ausgabe	BL = repeat rate (al=06h)
;		BH = delay (al=06h)
;************** set_typematic_and_delay **************
set_typematic_and_delay:
	pusha					;Register speichern
	push es					;	~
	push ds					;	~
	push cs					;Auf Stack pushen kopieren um segmentregister zu aktualisieren
	pop ds					;segmentregister aktualisieren
		
	xor ah,ah					;ah löschen, damit später zu SI, AX addiert werden kann
	cmp ax,2					;teste ob ax die nummer einer der funktionen enthält
	jb ah03h_funktion_vorhanden	;	...wenn ja mache weiter
		pop ds					;Register wiederherstellen
		pop es					;	~
		popa					;	~
		iret					;Interrupt beenden
	ah03h_funktion_vorhanden:
		
	lea si,[ah03h_procs]	;lade offset der offsetdatenbank der prozeduren
	add si,ax				;addiere den offset der datenbank mit der funktionsnummer(x2)
	add si,ax				;	~
	call [ds:si]			;gehe zu der funktion (indirekter sprung)
	
	mov [temp],bx			;bx temporär speichern, damit es beim wiederherstellen der Register nicht verloren geht
	pop ds					;Register wiederherstellen
	pop es					;	~
	popa					;	~
	mov bx,[temp]			;bx wiederherstellen
	iret					;Interrupt beenden

temp db 0
ah03h_procs:		;offset  der offset-tafel der einzelnen prozeduren (hier ist platz für 6 unterprozeduren)
	dw	set_default_rate_delay	;00h set default delay and rate (PCjr and some PS/2)
	dw	increase_delay			;01h increase delay before repeat (PCjr)
	dw	decrase_repeat_rate		;02h decrease repeat rate by factor of 2 (PCjr)
	dw	increase_delay_repeate	;03h increase delay and decrease repeat rate (PCjr)
	dw	turn_off_typematic_rate	;04h turn off typematic repeat (PCjr and some PS/2)
	dw	set_repeat_delay		;05h set repeat rate and delay (AT,PS)
	dw	get_repeat_delay	;06h get current typematic rate and delay (newer PS/2s)

	;***********************************************************************
	;************** set_default_rate_delay **************
	;Eingabe:	AL=00h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** set_default_rate_delay **************
	set_default_rate_delay:
		
		
		ret
	;************** set_default_rate_delay **************
	;***********************************************************************

	;***********************************************************************
	;************** increase_delay **************
	;Eingabe:	AL=01h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** increase_delay **************
	increase_delay:
		
		
		ret
	;************** increase_delay **************
	;***********************************************************************

	;***********************************************************************
	;************** decrase_repeat_rate **************
	;Eingabe:	AL=02h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** decrase_repeat_rate **************
	decrase_repeat_rate:
		
		
		ret
	;************** decrase_repeat_rate **************
	;***********************************************************************

	;***********************************************************************
	;************** increase_delay_repeate **************
	;Eingabe:	AL=03h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** increase_delay_repeate **************
	increase_delay_repeate:
		
		
		ret
	;************** increase_delay_repeate **************
	;***********************************************************************
	
	;***********************************************************************
	;************** turn_off_typematic_rate **************
	;Eingabe:	AL=04h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** turn_off_typematic_rate **************
	turn_off_typematic_rate:
		
		
		ret
	;************** turn_off_typematic_rate **************
	;***********************************************************************

	;***********************************************************************
	;************** set_repeat_delay **************
	;Eingabe:	AL=05h
	;Ausgabe:	-
	;Veränderungen:	-
	;************** set_repeat_delay **************
	set_repeat_delay:
		
		
		ret
	;************** set_repeat_delay **************
	;***********************************************************************
	
	;***********************************************************************
	;************** get_repeat_delay **************
	;Eingabe:	AL=06h
	;Ausgabe:	BH = delay value (00h = 250ms to 03h = 1000ms)
	;		BL = repeat rate (00h=30/sec to 0Ch=10/sec [def] to 1Fh=2/sec)
	;Veränderungen:	Flags,BX
	;************** get_repeat_delay **************
	get_repeat_delay:
		
		
		ret
	;************** get_repeat_delay **************
	;***********************************************************************
;************** set_typematic_and_delay_ende **************
;***********************************************************************

;***********************************************************************
;************** scan2ascii **************
;Eingabe:	bh=scancode
;		bl=224 falls Vorzeichen des Scancodes 224 ist
;Ausgabe:	al=asciicode
;Veränderungen:	Flags,al
;************** scan2ascii **************
scan2ascii:
	push cx							;Register sichern (pusha geht nicht, da AX wiedergegeben werden soll)
	push si							;	~
	push ds							;	~
	push dx							;	~
	
	std								;bei losdb, stosb u.a. si bzw di decrementieren
	push cs							;Auf Stack pushen kopieren um segmentregister zu aktualisieren
	pop ds							;segmentregister aktualisieren

	xor dx,dx						;dx löschen
	lea si,[key_buffer]				;zum laden Offset von Buffer auf SI
	mov cx,[buffer_pointer]			;Anz. d. zurzeit gespeicherten Tasten auf cx, um nicht alle abzufragen und somit Zeit zu sparen
	add si,cx						;zu si,cx addieren da std (dies ist um NUR die letzte Sondertaste abzufragen)
	inc cx							;cx incrementieren
	special_test_loop:
		lodsb						;Tasten-Scan in ax laden
		begin_to_test:
		cmp al,29					;Scan mit 29(STRG) vergleichen
		jne strg_weiter				;wenn nicht dasselbe mache weiter
			mov dx,4				;	...ansonsten auf dx,4 schieben, da die STRG-Asciis in der 4.Spalte nach normal liegen
			jmp special_test_end	;mache weiter
		strg_weiter:
		cmp al,42					;Scan mit 42(Shift) vergleichen
		jne shift_weiter			;wenn nicht dasselbe mache weiter
		shift:
			mov dx,2				;	...ansonsten auf dx,2 schieben, da die STRG-Asciis in der 2.Spalte nach normal liegen
			jmp special_test_end	;mache weiter
		shift_weiter:
		cmp al,54					;Scan mit 54(R Shift) vergleichen
			je shift				;wenn das gleich gehe zu L Shift, da gleiche Bedeutung bei Ascii-Codes
		cmp al,56					;Scan mit 56(ALT GR) vergleichen
		jne altgr_weiter			;wenn nicht dasselbe mache weiter
			mov dx,6				;	...ansonsten auf dx,6 schieben, da die STRG-Asciis in der 6.Spalte nach normal liegen
			jmp special_test_end	;mache weiter
		altgr_weiter:
		loop special_test_loop		;solange alles wiederholen bis eine Übereinstimmung gefunden wurde oder cx 0 ist
	special_test_end:
	cld								;;bei losdb, stosb u.a. si bzw di incrementieren
	
	cmp byte[last2keys],224			;wenn E0h vorletzter Scan ist, dann besteht der letzte Scan aus 2 Bytes
	jne kein_extra_key				;wenn kein Doppel-Scan, dann gehe zu 'kein_Extra_key'
		lea si,[sonderzeichen+1]	;	... ansonsten schiebe auf Si den Offset von den Sonderzeichen
		mov cx,34					;	... und gib in cx an, in der Tabelle insgesamt 34 Sonderzeichen liegen
		jmp search_in_bibo			;mache weiter
	kein_extra_key:
		lea si,[keyb_bibliothek+1]	;	... ansonsten schiebe auf Si den Offset von den normalen Zeichen
		mov cx,87					;	... und gib in cx an, in der Tabelle insgesamt 87 Zeichen liegen
	
	search_in_bibo:
		cmp bh,[ds:si]				;vergleiche bh mit Zeichen in ds:si
			je found_in_bibo		;	... wenn gleich, dann mache weiter
		add si,10					;	... ansonsten verschiebe den Zeiger si auf das nächste Zeichen (+10, da pro Zeichen 10 Bytes abgelegt wurden)
		loop search_in_bibo			;wiederhole den Vorgang, bis alle Zeichen getest wurden
	found_in_bibo:
	
	inc si							;incremetiere SI, um auf die Spalte der normalen Zeichen zu kommen, um es zu laden, falls keine Ausnahme vorliegt
	add si,dx						;addiere zu dx, das anders als 0 ist, falls vorher eine Sondertaste wie STRG geedrückt wurde
	lodsb							;lade das Zeichen

	pop dx							;Stelle die Register wieder her
	pop ds							;	~
	pop si							;	~
	pop cx							;	~
	ret								;Rücksprung an Aufrufstelle
;************** scan2ascii_ende **************
numlock db 0
capslock db 0
scrolllock db 0
;***********************************************************************
;Tastatur-treiber Bibliothek, enthält alle scancodes mit dem dazu gehörigen ascii-code
;	scancode		normal		shift			STRG		ALT Gr
keyb_bibliothek:
db	00,00,		000,00,		00,00,		00,00,		00,00	;nothing
db	00,01,		027,00,		27,00,		27,00,		00,00	;ESC
db	00,02,		049,00,		33,00,		00,02,		00,00	;1
db	00,03,		050,00,		34,00,		00,03,		253,00	;2
db	00,04,		051,00,		21,00,		00,04,		00,00	;3
db	00,05,		052,00,		36,00,		00,05,		00,00	;4
db	00,06,		053,00,		37,00,		00,06,		00,00	;5
db	00,07,		054,00,		38,00,		00,07,		00,00	;6
db	00,08,		055,00,		47,00,		00,08,		123,00	;7
db	00,09,		056,00,		40,00,		00,09,		91,00	;8
db	00,10,		057,00,		41,00,		00,10,		93,00	;9
db	00,11,		048,00,		61,00,		00,11,		125,00	;0
db	00,12,		225,00,		63,00,		00,12,		92,00	;ß
db	00,13,		39,00,		96,00,		00,00,		00,00	;´
db	00,14,		08,00,		08,00,		00,08,		08,00	;BKSP
db	00,15,		09,00,		09,00,		00,00,		00,00	;TAB
db	00,16,		113,00,		81,00,		17,00,		64,00	;Q
db	00,17,		119,00,		87,00,		23,00,		00,00	;W
db	00,18,		101,00,		69,00,		5,00,		238,00	;E
db	00,19,		114,00,		82,00,		18,00,		00,00	;R
db	00,20,		116,00,		84,00,		20,00,		00,00	;T
db	00,21,		122,00,		90,00,		00,00,		236,00	;Z
db	00,22,		117,00,		85,00,		21,00,		240,00	;U
db	00,23,		105,00,		73,00,		9,00,		232,00	;I
db	00,24,		111,00,		79,00,		15,00,		238,00	;O
db	00,25,		112,00,		80,00,		16,00,		239,00	;P
db	00,26,		129,00,		154,00,		00,26,		00,00	;ü
db	00,27,		43,00,		42,00,		00,27,		126,00	;+
db	00,28,		13,00,		13,00,		10,00,		00,00	;ENTER
db	00,29,		00,00,		00,00,		00,00,		00,00	;L\R CTRL
db	00,30,		097,00,		65,00,		1,00,		224,00	;A
db	00,31,		115,00,		83,00,		19,00,		228,00	;S
db	00,32,		100,00,		68,00,		4,00,		227,00	;D
db	00,33,		102,00,		70,00,		6,00,		229,00	;F
db	00,34,		103,00,		71,00,		7,00,		230,00	;G
db	00,35,		104,00,		72,00,		8,00,		231,00	;H
db	00,36,		106,00,		74,00,		10,00,		233,00	;J
db	00,37,		107,00,		75,00,		11,00,		234,00	;K
db	00,38,		108,00,		76,00,		12,00,		235,00	;L
db	00,39,		148,00,		153,00,		39,00,		148,00	;ö
db	00,40,		132,00,		142,00,		40,00,		132,00	;ä
db	00,41,		94,00,		248,00,		00,41,		00,00	;^
db	00,42,		00,00,		00,00,		00,00,		00,00	;L SHFT
db	00,43,		035,00,		39,00,		28,00,		00,00	;#
db	00,44,		121,00,		89,00,		25,00,		00,00	;Y
db	00,45,		120,00,		88,00,		24,00,		00,00	;X
db	00,46,		099,00,		67,00,		3,00,		226,00	;C
db	00,47,		118,00,		86,00,		22,00,		00,00	;V
db	00,48,		098,00,		66,00,		2,00,		225,00	;B
db	00,49,		110,00,		78,00,		14,00,		237,00	;N
db	00,50,		109,00,		77,00,		13,00,		230,00	;M
db	00,51,		44,00,		59,00,		00,51,		00,00	;,
db	00,52,		46,00,		58,00,		00,52,		00,00	;.
db	00,53,		45,00,		95,00,		00,45,		00,00	;-
db	00,54,		00,00,		00,00,		00,00,		00,00	;R SHFT
db	00,55,		42,00,		42,00,		00,150,		00,00	;KP *
db	00,56,		00,00,		00,00,		00,00,		00,00	;L\R ALT
db	00,57,		32,00,		00,32,		00,32,		00,00	;SPACE
db	00,58,		00,00,		00,00,		00,00,		00,00	;CAPS
db	00,59,		00,59,		00,84,		00,94,		00,00	;F1
db	00,60,		00,60,		00,85,		00,95,		00,00	;F2
db	00,61,		00,61,		00,86,		00,96,		00,00	;F3
db	00,62,		00,62,		00,87,		00,97,		00,00	;F4
db	00,63,		00,63,		00,88,		00,98,		00,00	;F5
db	00,64,		00,64,		00,89,		00,99,		00,00	;F6
db	00,65,		00,65,		00,90,		00,100,		00,00	;F7
db	00,66,		00,66,		00,91,		00,101,		00,00	;F8
db	00,67,		00,67,		00,92,		00,102,		00,00	;F9
db	00,68,		00,68,		00,93,		00,103,		00,00	;F10
db	00,69,		00,00,		00,00,		00,00,		00,00	;NUM
db	00,70,		00,00,		00,00,		00,00,		00,00	;SCROLL
db	00,71,		055,00,		00,00,		00,00,		00,00	;KP 7
db	00,72,		056,00,		00,00,		00,00,		00,00	;KP 8
db	00,73,		057,00,		00,00,		00,00,		00,00	;KP 9
db	00,74,		045,00,		45,00,		00,142,		00,00	;KP -
db	00,75,		052,00,		00,00,		00,00,		00,00	;KP 4
db	00,76,		053,00,		00,00,		00,00,		00,00	;KP 5
db	00,77,		054,00,		00,00,		00,00,		00,00	;KP 6
db	00,78,		043,00,		00,00,		00,78,		00,00	;KP +
db	00,79,		049,00,		00,79,		00,117,		00,00	;KP 1
db	00,80,		050,00,		00,80,		00,145,		00,00	;KP 2
db	00,81,		051,00,		00,00,		00,00,		00,00	;KP 3
db	00,82,		048,00,		00,82,		00,146,		00,00	;KP 0
db	00,83,		046,00,		83,00,		00,147,		00,00	;KP .
db	00,84,		84,84,		84,84,		84,84,		00,00
db	00,85,		85,85,		85,85,		85,85,		00,00
db	00,86,		60,00,		62,00,		00,86,		124,00	;<
db	00,87,		00,133,		00,135,		00,137,		00,00	;F11
db	00,88,		00,133,		00,136,		00,138,		00,00	;F12
db	00,113,		00,113,		00,113,		00,113,		00,00	;HOME
sonderzeichen:
db	224,101,	00,101,		00,101,		00,101,		00,00	;WWW Search
db	224,102,	00,102,		00,102,		00,102,		00,00	;WWW Favorites
db	224,103,	00,103,		00,103,		00,103,		00,00	;WWW Refresh
db	224,104,	00,104,		00,104,		00,104,		00,00	;WWW Stop
db	224,105,	00,105,		00,105,		00,105,		00,00	;WWW Forward
db	224,106,	00,106,		00,106,		00,106,		00,00	;WWW Back
db	224,107,	00,107,		00,107,		00,107,		00,00	;My Computer
db	224,108,	00,108,		00,108,		00,108,		00,00	;E-Mail
db	224,109,	00,109,		00,109,		00,109,		00,00	;Media Select
db	224,16,		00,16,		00,16,		00,16,		00,00	;Previous Track
db	224,25,		00,25,		00,25,		00,25,		00,00	;Next Track
db	224,28,		13,00,		13,00,		00,28,		00,00	;KP EN
db	224,32,		00,32,		00,32,		00,32,		00,00	;Mute
db	224,33,		00,33,		00,33,		00,33,		00,00	;Calculator
db	224,34,		00,34,		00,34,		00,34,		00,00	;Play/Pause
db	224,36,		00,36,		00,36,		00,36,		00,00	;Stop
db	224,46,		00,46,		00,46,		00,46,		00,00	;Volume Down
db	224,48,		00,48,		00,48,		00,48,		00,00	;Volume Up
db	224,50,		00,50,		00,50,		00,50,		00,00	;WWW Home
db	224,53,		47,00,		00,47,		00,149,		00,00	;KP /
db	224,72,		00,72,		00,72,		00,141,		00,00	;U ARROW
db	224,73,		00,73,		00,73,		00,132,		00,00	;PG UP
db	224,75,		00,75,		00,75,		00,115,		00,00	;L ARROW
db	224,77,		00,77,		00,77,		00,116,		00,00	;R ARROW
db	224,79,		00,79,		00,79,		00,117,		00,00	;END
db	224,80,		00,80,		00,80,		00,145,		00,00	;D ARROW
db	224,81,		00,81,		00,81,		00,118,		00,00	;PG DN
db	224,82,		00,82,		00,82,		00,146,		00,00	;INSERT
db	224,83,		00,83,		00,83,		00,147,		00,00	;DELETE
db	224,91,		00,81,		00,91,		00,91,		00,00	;L GUI
db	224,92,		00,92,		00,92,		00,92,		00,00	;R GUI
db	224,93,		00,93,		00,93,		00,93,		00,00	;APPS
db	224,94,		00,94,		00,94,		00,94,		00,00	;Power
db	224,95,		00,95,		00,95,		00,95,		00,00	;Sleep
db	224,99,		00,96,		00,96,		00,96,		00,00	;Wake
;db	0E0h,02Ah, 0E0h,037h	0E0h,0B7h, 0E0h,0AAh	;PRNT SCRN
;db	0E1h,01Dh,045h	0E1h,09Dh,0C5h	-NONE-	;PAUSE

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	pusha			;Word-Register speichern
	pushf			;Flagregister speichern
	cld				;direction-flag löschen
	mov ah,0Eh		;Funktion 0x0E	
.loop:	
	lodsb			;Byte laden
	or al,al		;Byte prüfen, sodass zeroflag gesetzt wird oder nicht
	jz .end			;0-Byte? -> Ende!
	int 10h			;schreiben
	jmp .loop		;Nächstes Byte
.end:
	popf			;flagregister wiederherstellen
	popa			;Word-Register wiederherstellen
	ret				;zum Aufrufsort zurückspringen

msg_start db 'Tastatur-Treiber laden...OK',13,10,0
kb_startled db 'Tasten gedrückt... ansonsten falschen code erhalten...',0