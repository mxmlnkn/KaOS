org 100h		;Nasm sagen, dass das Programm am Offset 100h geladen wird, damit er zu allen Offsets dies addiert
mov ax,cs		;auf ax cs schieben
mov ds,ax		;um damit ds zu updaten
mov es,ax		;und auch es zu updaten

main:
	in al,0A1h			;hole von dem PIC2 das IMR
	or al,00010000b		;um darin den IRQ12 zu maskieren, damit er deaktiviert wird
	out 0A1h,al			;und sende das modifizierte IMR an den Slave

	CLI							;Wir deaktivieren w�hrend des setzens des neuen IRVs, die IRQs
	xor ax,ax					;ax l�schen um es mit dem IVT-Segment zu initialiseren
	mov es,ax					;auf es ax kopieren
	mov [es:74h*4+2],cs			;und dann an den Eintrag f�r den Int74h/IRQ12 das Segment
	mov [es:74h*4],word irq12	;und den Offset des Int-Handlers speichern
	STI

	mov al,0a8h			;schiebe auf al den Befehl zur Aktivierung der Maus
	out 64h,al			;schicke den Befehl an den Keyboard-Controller
	call kb.checkcmd	;und warte bis der Befehl angenommen und verarbeitet wurde

	mov al,0f4h			;danach an die Maus den Befehl zum Eintritt in den Streaming-Mode auf al kopieren
	call kb.write.mouse	;um diesen an die Maus �ber den Input-Buffer-Port zu senden
	
	;CLI					;alle Interrupts auschalten, damit nicht unser Keyboard-Int das gesendete Byte erh�lt xD
	mov al,20h			;Befehl zum auslesen des Kommando-Bytes auf al schieben
	out 64h,al			;um es an den Keyboard-Controller zu senden
	call kb.checkcmd	;nun noch warten, bis dieser angenommen wurde
	call kb.read		;und wir k�nnen das command-Byte abholen
	;STI					;nun da wir das Byte entgegengenommen haben, k�nnen wir die IRQs wieder aktivieren
		push ax			;spiechere al, da dies f�r out gebraucht wird
	mov al,60h			;und kopiere auf al den Befehl zum einlesen eines neuen Command-Bytes
	out 64h,al			;und schicke diesen nun an den Keyboard-Controller
	call kb.checkcmd	;warte bis es angenommen wurde -> nun wartet der KC auf das Command-Byte am Port 60h(Datenport des 64h)
		pop ax			;stelle nun das mit 20h erhaltene Byte wieder her um es zu modifizieren und neu zu setzen
	or al,00000011b		;setze das Bit 1, bei dessem setzen der IRQ12 immer angesprungen wird, sobald Bit5 im 64h-Status-Byte gesetzt ist
	out 60h,al			;und schicke das Command-Byte an den Daten-Port 60h, durch den Befehl 60h wird dieses neu eingelesen
	call kb.checkcmd	;und warte, bis das Byte vollst�ndig angenommen wurde

	; CLI	;(is schon oben gemacht)	;alle Interrupts auschalten, damit nicht unser Keyboard-Int das gesendete Byte erh�lt xD
	; mov al,20h			;Befehl zum auslesen des Kommando-Bytes auf al schieben
	; out 64h,al			;um es an den Keyboard-Controller zu senden
	; call kb.checkcmd	;nun noch warten, bis dieser angenommen wurde
	; call kb.read		;und wir k�nnen das command-Byte abholen
	; STI	;(wir warten lieber noch, kann ja nicht schaden)	;nun da wir das Byte entgegengenommen haben, k�nnen wir die IRQs wieder aktivieren
		; push ax
	; mov al,60h
	; out 64h,al
	; call kb.checkcmd
		; pop ax
	; or al,00000011b
	; out 60h,al
	; call kb.checkcmd
						movzx eax,al
						mov ebx,16	
						call zahlausgabe

	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,0C8h			;dann das zweite Byte des Befehls, die Sample-Rate (200 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,64h			;dann das zweite Byte des Befehls, die Sample-Rate (100 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,50h			;dann das zweite Byte des Befehls, die Sample-Rate (80 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	;	CLI
	mov al,0F2h			;sende Befehl F2h f�r GetDeviceID an Maus, sodass wenn die Maus ein Mausrad besitzt mit 03h antwortet,
	call kb.write.mouse	;ansonsten erwidert sie 00h -> Diese Funktion mit 03h wird durch die reihe setsamplerate 200,100,80 aktiviert.
	call kb.read		;die ID der Maus auslesen. kb.write.mouse liest nur die Best�tigung FAh aus
	;	STI
						movzx eax,al
						mov ebx, 16
						call zahlausgabe

	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,0C8h			;dann das zweite Byte des Befehls, die Sample-Rate (200 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,0C8h			;dann das zweite Byte des Befehls, die Sample-Rate (200 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	mov al,0F3h			;Befehl zum senden und neu setzen der SampleRate auf al schieben
	call kb.write.mouse	;und per vordefinierter Prozedur an die Maus senden
	mov al,50h			;dann das zweite Byte des Befehls, die Sample-Rate (80 reports/s) auf al kopieren
	call kb.write.mouse	;und auch dies per vordefinierter Prozedur an die Maus senden
	;	CLI
	mov al,0F2h			;sende Befehl F2h f�r GetDeviceID an Maus, sodass wenn die Maus ein Mausrad besitzt mit 04h antwortet,
	call kb.write.mouse	;ansonsten erwidert sie 00h -> Diese Funktion mit 04h wird durch die reihe setsamplerate 200,200,80 aktiviert.
	call kb.read		;die ID der Maus auslesen. kb.write.mouse liest nur die Best�tigung FAh aus
	;	STI
	mov [mouseid],al	;speichere die bekommene MausID in einer Variable, die dann der IRQ12 auslesen kann, damit dieser wei�, ob die Maus ein Rad hat
						movzx eax,al
						mov ebx, 16
						call zahlausgabe
	
	in al,0A1h			;hole von dem PIC2 das IMR
	and al,11101111b	;um darin den IRQ12 zu maskieren, damit er aktiviert wird
	out 0A1h,al			;und sende das modifizierte IMR an den Slave

	wait_esc:
		mov ah,1		;Funktion 1 des Int16h w�hlen, um abzufragen, ob ein Tastendruck vorhanden ist
		int 16h			;diese Funktion ausf�hren, zeroflag gesetzt, wenn nichts vorhanden ist
		or ah,ah		;teste ob ein Scancode <> 0 erhalten wurde und somit eine Taste gedr�ckt wurde...
			jz wait_esc	;wenn keine Taste gedr�ckt wurde, f�hre die Prozedur so lange aus, bis die geschieht
		xor ax,ax			;l�sche ax f�r funktion 00h
		int 16h				;und hole die Taste ab, die bei ah=1 int 16 den Tastendruck ausgel�st hat, da diese ja nicht abgeholt wird
		cmp ah,1		;vergleicht den asciicode mit dem von ESC
	jne wait_esc		;wenn ESC nicht gedr�ckt wurde, warte auf n�cshten Tastendruck

	;CLI					;alle Interrupts auschalten, damit nicht unser Keyboard-Int das gesendete Byte erh�lt xD
	mov al,20h			;Befehl zum auslesen des Kommando-Bytes auf al schieben
	out 64h,al			;um es an den Keyboard-Controller zu senden
	call kb.checkcmd	;nun noch warten, bis dieser angenommen wurde
	call kb.read		;und wir k�nnen das command-Byte abholen
	;STI					;nun da wir das Byte entgegengenommen haben, k�nnen wir die IRQs wieder aktivieren
		push ax			;spiechere al, da dies f�r out gebraucht wird
	mov al,60h			;und kopiere auf al den Befehl zum einlesen eines neuen Command-Bytes
	out 64h,al			;und schicke diesen nun an den Keyboard-Controller
	call kb.checkcmd	;warte bis es angenommen wurde -> nun wartet der KC auf das Command-Byte am Port 60h(Datenport des 64h)
		pop ax			;stelle nun das mit 20h erhaltene Byte wieder her um es zu modifizieren und neu zu setzen
	and al,11111101b	;l�sche das Bit 1, bei dessem setzen der IRQ12 immer angesprungen wird, sobald Bit5 im 64h-Status-Byte gesetzt ist
	out 60h,al			;und schicke das Command-Byte an den Daten-Port 60h, durch den Befehl 60h wird dieses neu eingelesen
	call kb.checkcmd	;und warte, bis das Byte vollst�ndig angenommen wurde
	; movzx eax,al
	; mov ebx,16			;als basis des zahlensystems 16 f�e Hexadezimal-zahlen w�hlen
	; call zahlausgabe	;Zahl im Format "xxyy" ausgeben, wobei xx der gesendete SpecialCode und yy das Fehlerregister ist

	mov al,0F5h			;auf al den Befehl zum deaktivieren der Maus selbst schieben
	call kb.write.mouse	;und an die Maus senden
	
	mov al,0a7h			;schiebe auf al den Befehl zur Deaktivierung der Maus und 
	out 64h,al			;schicke dies dem Keyboard-Controller, sodass die Maus auch auf diesem deaktiviert ist
	call kb.checkcmd	;warte bis der Befehl angenommen wurde

	in al,0A1h			;hole von dem PIC2 das IMR
	or al,00010000b		;um darin den IRQ12 zu maskieren, damit er deaktiviert wird
	out 0A1h,al			;und sende das modifizierte IMR an den Slave

	retf				;springe zum Kernel zur�ck

;***************************************************************************************************************
;dies ist der IRQ12, der immer angesprungen wird, wenn sich die Daten der Maus ver�ndert habe (sofern der Stream-Mode benutzt wird)
;da der IRQ12 immer nur aufgerufen wird, wenn ein Byte im Mouse-Buffer liegt, holen wie auch immer nur ein Byte ab, z�hlen jedoch mit,
;welches Byte des Mouse-data-packets das ist. Unter VirtualBox ging es zwar auch, wenn man alle 3 Byte hintereinander beim Aufrufen
;abgeholt hat, doch bei meinem alten PC kam es dabei zu disinformation, sodass ich das so �ndern musste.
;***************************************************************************************************************
irq12:
	CLI			;wir wollen uns ja nicht von anderen IRQs dazwischen funken lassen ;)
	pusha		;Register sichern
	push ds		;Datensegment sichern
	push es		;es-Segment sichern
	mov ax,cs	;dann auf ax das codesegment, also das segment, wo das Programm ist, holen
	mov ds,ax	;um dies auf ds zu kopieren
	mov es,ax	;und um auch es zu updaten
	
	; lea si,[irq12_startled]
	; call write

.start:
inc byte[actualb]		;erh�he den Counter f�r das aktuelle Byte um 1
cmp byte[actualb],1		;�berpr�fe, ob der Counter 1 ist
jne .not_first_byte		;wenn nicht, hole schoneinmal Byte 1 nicht ab und springe somit dar�ber

	call kb.read.mouse			;Byte #1 nach al holen 
		jnz .irq12_end_err		;wenn alles klar ging und das Byte geholt wurde, �berspr
	mov [status],al				;das Byte in Status speichern, damit es die Prozeduren von int33h nutzen k�nnen
	mov bl,al					;und nocheinmal f�r die Berechnung der Position in bl zwischenspeichern
	jmp .irq12_end				;beende den Interrupt, da beim n�chsten Byte dieser IRQ sowieso nocheinmal aufgerufen wird

.not_first_byte:	
cmp byte[actualb],2		;teste ob der Counter 2 ist
jne .not_second_byte		;wenn nicht, dann ist es auch nicht das zweite Byte, was wir jetzt holen m�ssen

	call kb.read.mouse			;Byte #2 aus dem Output-Buffer der Maus holen
		jnz .irq12_end_err		;wenn dabei was schief ging, beende den irq
	mov [xcoord],al				;ansonsten zwischenspeichere den x-movement
	xor ah,ah					;l�sche den HighTeil von ax, damit al der Position(word) addiert werden kann
	test byte[status],00010000b	;dann teste ob das SignBit von x gesetzt ist, wenn ja, ist das movement negativ
	jnz .sign_bit_x				;und springe zu dem Teil, der subtrahiert statt addiert
		add [x],ax				;ansonsten addiere zur aktuellen Position die Bewegung
		jmp .sign_x				;und �berspringe das Abziehen
	.sign_bit_x:
		not al					;negiere x-coord, damit der overflow aufgehoben wird, der ensteht, wenn man 0-1 rechnet
		inc al					;da aber -1 = FFh was genoted 0 w�re, addiere nocheinmal 1
		sub [x],ax				;und ziehe den Betrag des Movements der aktuellen Position ab
	.sign_x:
	jmp .irq12_end		;beende den Interrupt, da beim n�chsten Byte dieser IRQ sowieso nocheinmal aufgerufen wird

.not_second_byte:
cmp byte[actualb],3		;teste ob der Byte-Counter 3 ist
jne .not_third_byte		;wenn nicht, dann muss es das 4. Byte sein, was wir abholen m�ssen also springe dahin

	call kb.read.mouse			;Byte #3 aus dem Output-Buffer der Maus holen
		jnz .irq12_end_err		;wenn dabei was schief ging, beende den irq
	mov [ycoord],al				;ansonsten zwischenspeichere den y-movement
	xor ah,ah					;l�sche den HighTeil von ax, damit al der Position(word) addiert werden kann
	test byte[status],00100000b	;dann teste ob das SignBit von y gesetzt ist, wenn ja, ist das movement negativ
	jnz .sign_bit_y				;und springe zu dem Teil, der subtrahiert statt addiert
		add [y],ax				;ansonsten addiere zur aktuellen Position die Bewegung
		jmp .sign_y				;und �berspringe das Abziehen
	.sign_bit_y:
		not al					;negiere y-coord, damit der overflow aufgehoben wird, der ensteht, wenn man 0-1 rechnet
		inc al					;da aber -1 = FFh was genoted 0 w�re, addiere nocheinmal 1
		sub [y],ax				;und ziehe den Betrag des Movements der aktuellen Position ab
	.sign_y:
	jmp .irq12_end		;beende den Interrupt, da beim n�chsten Byte dieser IRQ sowieso nocheinmal aufgerufen wird

.not_third_byte:

	cmp byte[mouseid],0			;pr�fe ob die MouseID 0 ist, denn wenn nciht, dann werden 4 Bytes gesendet, egal ob mouseid 3 o. 4
	jz .no_4th_byte				;falls nun kein 4. Byte ben�tigt wird �berspringe das abholen desgleichen
		call kb.read.mouse		;ansonsten hol Byte #4 aus dem Output-Buffer der Maus
			jnz .irq12_end_err	;wenn dabei was schief ging, beende den irq
		mov [zbtn45],al			;sonst speichere es f�r sp�tere Benutzung
	.no_4th_byte:
	mov byte[actualb],0			;setze den Byte-Counter wieder auf 0, da nun alle Bytes des Paketes abgeholt wurden und es von vorne beginnt

	call display_mouse			;dies ist eine sehr umfangreiche Prozedur zur Veranschaulichung der Daten
	cmp byte[mouseid],0			;pr�fe ob die MouseID 0 ist, denn wenn nciht, dann werden 4 Bytes gesendet, egal ob mouseid 3 o. 4
		jz .start				;und wenn doch, dann wurde das Byte, weswwegen dieses mal der IRQ ausgel�st wurde ja nicht abgeholt, also hole dies nach..
	jmp .irq12_end				;�berspringe den Fehlerteil, der gerufen wird, wenn kein MausByte vorhanden war, was eig. NIE vorkommen d�rfte,
								;au�er der IRQ12 wird manuell angesprungen
.irq12_end_err:
	dec byte[actualb]		;dekrementiere den Bytecounter, da ja das aktuelle Byte nicht abgeholt wurde

.irq12_end:
	mov al,20h		;schiebe das OCW1/EOI auf al
	out 20h,al		;und sende den EOI an den Master
	out 0A0h,al		;und den Slave, da beide ihn ben�tigen, da Slave am IRQ2 h�ngt
	pop es			;es wiederherstellen
	pop ds			;stelle ds wieder her
	popa			;stelle auch die anderen Register wieder her
	STI				;so, nun ist unser Handler zu Ende..
	iret			;und verlasse den Interrupt

;************************* Variablen des IRQ12 *************************
status	db 0	;hier wird das Byte1 des Mouse-data-packes gespeichert
xcoord	db 0	;hier das zweite
ycoord	db 0	;und hier das 3.
zbtn45	db 0	;und hier das 4.
x		dw 0	;hier ist die aktuelle Position des Mauszeigers gespeichert, die entsprechend
y		dw 0	; ... der Movement-Bytes ver�ndert werden
z		dw 0	;hier die aktuelle z-position des rades(nicht die bewegung)
mouseid	db 0	;hier wird die MouseID gespeichert 0 f�r normal, 3 f�r Rad und 4 f�r Rad und 5 Buttons
actualb	db 0	;hier wird gespeichert, welche bytes des Maus-Daten-Pakets schon abgeholt wurden (f�r jedes Byte wird der IRQ12 einzeln angesprungen)

;***************************************************************************************************************
;dies sind Prozeduren die speziell Befehle an das Keyboard schicken oder Daten entgegennehmen, da man immer warten muss, bis
;welche da sind. diese Prozeduren werden �berwiegend f�r die Mausinitialisierung und f�r den IRQ12 ben�tigt, der die Pakete holt
;***************************************************************************************************************
kb:
.read:					;wartet, bis im Output-Buffer des Port60h Daten bereitliegen und holt diese dann ab
	CLI				;Interrupts deaktivieren, damit das Byte nicht vom IRQ12 abgefangen wird
	push ecx			;ecx sichern, da das f�r loop ben�tigt wird
	xor ecx,ecx			;auf ecx, ffffh schieben, damit hier nicht unendlich lang auf Daten gewartet wird
	.wait_for_output:
		in al,64h					;liest das Statusregister vom Keyboard-Controller aus
		test al,00000001b			;testen ob im Outputbuffer Daten sind
			jnz .output_is_there	;wenn ja, dann springe aus der Schleife raus und lese die Daten ein
		loop .wait_for_output		;wenn nicht, also Bit0 nicht gesetzt ist, wiederhole schleife und warte weiter auf Daten
			mov ah,1				;wenn loop durchgelaufen ist und noch immer keine Daten da sind, schiebe auf ah errorcode
			or ah,ah				;und setze ggf. das zeroflag, damit der user gleich danach jz o.�. schreiben kann
			pop ecx					;stelle register wieder her
			ret						;und springe zum aufrufsort zur�ck
	.output_is_there:
	in al,60h			;hole Daten aus dem Output-Buffer
	xor ah,ah			;und l�sche ah, da bei fehler auf ah der errorcode ist; setze ggf. zeroflag (macht xor...)
	pop ecx				;stelle Register wieder her
	STI				;Interrupts wieder aktivieren, sonst bleibt sicher der PC h�ngen xD
	ret					;und springe zum Aufrufsort zur�ck
;************************* Variablen des IRQ12 *************************
.checkcmd:					;pr�ft, ob der Befehl auch angenommen wurde, oder noch immer im Buffer auf seine Verarbeitung wartet
	push ax					;ax sichern, da es beim einlesen des Statusbytes zerst�rt wird
	.loop_chkcmd:
		in al,64h			;nach al das Statusregister holen
		test al,00000010b	;und dort pr�fen, ob Bit2, dass anzeigt, das etwas im out-buffer, gesetzt ist
		jnz .loop_chkcmd	;wenn nicht, wiederhole den vorgang, bis das byte verarbeitet ist
	pop ax					;stelle ax nun wieder her
	ret
;************************* wartet bis alle Befehle des KC verarbeitet sind *************************
.read.mouse:
	CLI				;Interrupts deaktivieren, damit das Byte nicht vom IRQ12 abgefangen wird
	push cx				;cx sichern
	mov cx,0FFFFh		;auf cx die H�chstm�gliche Zahl schieben, damit getmb nicht alles aufh�lt, nur da es auf ein Byte wartet
	.waitforbytes:		;und da loop zuerst dekremntiert und dann vergleicht, obs 0 ist, ist 0 die h�chste Zahl f�r cx
		in al,64h		;nach al das Statusregister vom Keyboard-Contoller holen
		test al,20h		;und schauen, ob Bit5 gesetzt ist, ob im Mouse-Output-Buffer Daten warten
		jnz .byte_ready	;wenn ja, dann springe aus dem loop heruas
	loop .waitforbytes	;ansonsten f�hre loop-schleife fort, sp�testens bis cx 0 ist
		pop cx			;stelle cx wieder her
		mov ah,1		;zeroflag l�schen, indem 1 auf ah gemoved wird, damit dies dem anwender gegeben werden kann
		or ah,ah		;und setze ggf. das zeroflag, damit der user gleich danach jz o.�. schreiben kann
		ret				;und springe zum Aufrufsort zur�ck
	.byte_ready:		;wenn IP hier ist, dann wartet ein Byte darauf abgeholt zu werden
	in al,60h			;und holen dann vom Mouse-Output-Buffer das lang ersehnte Byte :)
	pop cx				;und cx wiederherstellen
	xor ah,ah			;und l�sche ah, da bei fehler auf ah der errorcode ist; setze ggf. zeroflag (macht xor...)
	STI				;Interrupts wieder aktivieren, sonst bleibt sicher der PC h�ngen xD
	ret					;zum Abschluss springen wir zum Aufrufsort zur�ck; das Byte liegt dabei in al und ZF zeigt an, ob funktionert hat
;************************* wartet eine Weile ob Daten kommen und speichert diese ggf. in al ab *************************
.write.mouse:
	push ax				;al sichern, da dort der zu sendende Befehl gesichert ist
	mov al,0d4h			;auf al d4h kopieren, das als Befehl an den KC anzeigt, dass das n�chste Byte an die Maus statt der Tastatur geschickt
	out 64h,al			;... werden soll -> dann schicke das Byte an den KC
	call kb.checkcmd	;und warte, bis es angenommen und verarbeitet wurde
	pop ax				;hole den eig. zu sendenden Befehl nach al
	CLI				;Interrupts deaktivieren, damit das Byte nicht vom IRQ12 abgefangen wird
	out 60h,al			;und sende ihn nu, da D4h gesendet wurde an den Maus-Controller
	call kb.checkcmd	;und warte, bis auch dieser Befehl vollst�ndig angenommen unf abgearbeitet wurde
	call kb.read		;hole die response. die ist bei succes FAh. bei manchen anderen gibt es da besondere codes, wie 00h bei GetID
	STI				;Interrupts wieder aktivieren, sonst bleibt sicher der PC h�ngen xD
	ret					;verlasse Prozedur und springe zumi Aufruf zur�ck
;******* Prozedur schickt den Befehl per D4h an die Maus und gibt error von kb.read aus (prc succesfull if ah=0 und al=FAh sein) *********

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	pusha			;Word-Register speichern
	pushf			;Flagregister speichern
	cld				;direction-flag l�schen
	mov ah,0Eh		;Funktion 0x0E	
	mov bx,7		;auf bl die auszugebende Farbe senden
.loop:	
	lodsb			;Byte laden
	or al,al		;Byte pr�fen, sodass zeroflag gesetzt wird oder nicht
	jz .end			;0-Byte? -> Ende!
	int 10h			;schreiben
	jmp .loop		;N�chstes Byte
.end:
	popf			;flagregister wiederherstellen
	popa			;Word-Register wiederherstellen
	ret				;zum Aufrufsort zur�ckspringen

;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pushad				;Register speichern
	pushf				;Flags speichern

	push ax				;speichere ax, da dort die auszugebende zahl gestored ist
	mov ax,0e0dh		;und schiebe auf ax die funktion 0eh zum ausgeben des zeichens in al
	int 10h				;und gib #13 aus
	mov ax,0e0ah		;das selbe mache nun noch einmal nur mit zeichen #10
	int 10h				;unf gib aus. beizusammen erzeugen einen Zeilenumbruch
	pop ax				;stelle ax wieder her um mit der eig. zahlausgabe zu beginnen

	xor ecx,ecx			;ecx l�schen, damit von 0 an hochgez�hlt werden kann
	or ebx,ebx			;testen ob ebx 0 ist, denn w�re dem so, k�me es zu einer division durch 0
	jnz zahlausgabe1	;wenns nciht 0 ist, fahre wie gewohnt fort
		mov ebx,16		;falls aber doch, ersetze ebx mit der basis f�r das hesadezimale Zahlensystem
zahlausgabe1:
	xor edx,edx			;l�sche edx, damit das nicht mit dividiert wird(hier landt der modulo der division)
	div ebx				;mit der basis dividieren um den rest zu erhalten
	push dx				;und zu speichern... der rest ist bei 16 beo 0-15 und somit sp�ter umgewandelt bei 0-9 und a-f
	inc cx				;erh�he cx damit man wei�, wie viele ziffern im stack sind
	or eax,eax			;teste ob eax 0 ist und somit schon alle ziffern gepusht worden sind
	jne zahlausgabe1	;wenn nicht, hole n�chste ziffern durch modulo

	mov ah,0eh			;schiebe auf ah 0eh f�r teletype-output
	mov bx,2			;auf bx farbe
	zahlausgabe2:
	pop dx				;hole das zuletzt erhaltene zeichen/ziffer vom stack
	cmp dx,10			;vergleiche ob sie noch im bereich der zahlen ist
	jb zahl				;wenn ja �berspringe folgendes
		add dl,7		;ansonsten wechsle in den Bereich der gro�en Buchstaben
		zahl:
			add dl,48	;addiere 48 ("0") um ins Zahlsystem bzw. Gro�e Alphabet zu kommen
			mov al,dl	;auf al das berechnete zeichen verschieben (pop ax deswegen nicht, weil dann ah schrott w�re)
			int 10h		;zeichen ausgeben
	loop zahlausgabe2	;wiederhole dies f�r jedes im Stack gespeicherte Zeichen
	
	popf				;stelle Flag wieder her
	popad				;stelle Register wieder her
	ret					;und springe zum Aufrufsort zur�ck

;***************************************************************************************
;Display-Mouse -> eine ganz spezifische Prozedur, die die Daten des Mouse-Packets vernaschaulicht
;***************************************************************************************
mouse_pic	db	"  ___________  "	;16 Bytes per line
			db	" /   |   |   \ "
			db	"|    |   |    |"
			db	"|    |   |    |"
			db	"|-------------|"
			db  "|             |"
			db  "| xmov=       |"
			db  "| ymov=       |"
			db  "|  x=         |"
			db  "|  y=         |"
			db  " \___________/ "

display_mouse:
	pusha			;Register sichern
	mov ax,0B800h	;auf ax das Anfangsegment des Grafikspeichers von Modus 3h schieben
	mov es,ax		;um es auf es zu storen
	
	;mov [es:100h],word 1234h			;w�rde gr�ne 4 auf blauem Grund ausgeben -> 1:Hintergrund 2->Vordergrund 34->Ascii-Char   ->eine Zeile umfasst 160Bytes
	cmp byte[es:1184+4*2],'_'	;vergleicht das Zeichen an x=7 und y=36 mit "_", also dem ersten sichtbaren Zeichen unseres Maus-Bildes
	je mouse_pic_installed		;wenn das wirklich dort ist, dann ist unser Mausskellet schon im Grafikspeicher und wir m�ssen nur noch z.b.  x&y �ndern
		lea si,[mouse_pic]		;auf si zum laden den Offset unsere Bildes schieben
		lea di,[1184]			;und mit di an die Stelle gehen, wo die obere linke Ecke unseres Bildes sein wird
		mov cx,11				;auf cx 11 schieben, da es 11 zeilen sind und diese schliefe f�r jede ausgef�hrt werden muss
		copy_pic:
			mov ah,0Fh			;auf ah die Attriubte schieben, sodass die schrift wei� ist
			push cx				;cx speichern, damit sozusagen loop zweimal gleichzeitig laufen kann
			mov cx,15			;auf cx,15 schieben, damit nur eine Zeile kopiert wird, schieben
			copy_row:
				lodsb			;das erste Char laden
				stosw			;und zusammen mit dem Attribut in den Grafikspeicher schieben
				loop copy_row	;das ganze f�r jedes Char in der Zeile wiederholen(15x)
			pop cx				;cx wiederherstellen
			add di,65*2			;di 65 Zeichen weiter verschieben (80 Zeichen/Zeile und wir haben gerade 15 geschrieben)
		loop copy_pic			;und diese Schleife wiederholen wir f�r die Zeile in unserem AsciiArt-Bild
	mouse_pic_installed:
	
	xor al,al						;l�sche al, damit man sieht, ob es gesetzt wurde oder nicht
	mov bl,[status]					;nach bl das Status-Byte des Pakets holen
	test bl,00000001b				;pr�fen ob in StatusByte Bit2 f�r linke-Taste gesetzt ist, oder nicht,
	setnz al						;sodass al bei gesetztem Byte 1 und bei gel�schten Byte 0 ist
	mov cl,4Fh						;4Fh mit dem Bit multipliezieren, sodass bei gesetztem Bit die Hintergrundfarbe rot und die schriftfarbe wei� ist
	mul cl							;und ansonsten alles schwar ist, da 1*Zeichen = Zeichen und 0*Zeichen=kein Zeichen ist
	mov [es:1184+1*160+2*2+1],al	;setzt die Attribute der  Zeichen in der linken Taste im Bild so, dass diese rot wird. Ich habe sogar die Speicheraddressierung
	mov [es:1184+1*160+2*3+1],al	;   ...als Rechnung stehen lassen, damit diese besser nachvollziehbar ist, siehe auch nochmal den Befehl: mov [es:100h],word 1234h
	mov [es:1184+1*160+2*4+1],al	;	~
	mov [es:1184+2*160+2*1+1],al	;	~
	mov [es:1184+2*160+2*2+1],al	;	~
	mov [es:1184+2*160+2*3+1],al	;	~
	mov [es:1184+2*160+2*4+1],al	;	~
	mov [es:1184+3*160+2*1+1],al	;	~
	mov [es:1184+3*160+2*2+1],al	;	~
	mov [es:1184+3*160+2*3+1],al	;	~
	mov [es:1184+3*160+2*4+1],al	;	~
	xor al,al						;l�sche al, damit man sieht, ob es gesetzt wurde oder nicht
	test bl,00000010b				;pr�fen ob in StatusByte Bit2 f�r rechte-Taste gesetzt ist, oder nicht,
	setnz al						;sodass al bei gesetztem Byte 1 und bei gel�schten Byte 0 ist
	mov cl,4Fh						;4Fh mit dem Bit multipliezieren, sodass bei gesetztem Bit die Hintergrundfarbe rot und die schriftfarbe wei� ist
	mul cl							;und ansonsten alles schwar ist, da 1*Zeichen = Zeichen und 0*Zeichen=kein Zeichen ist
	mov [es:1184+1*160+2*10+1],al	;setzt die Attribute der  Zeichen in der rechten Taste im Bild so, dass diese rot wird. Ich habe sogar die Speicheraddressierung
	mov [es:1184+1*160+2*11+1],al	;   ...als Rechnung stehen lassen, damit diese besser nachvollziehbar ist, siehe auch nochmal den Befehl: mov [es:100h],word 1234h
	mov [es:1184+1*160+2*12+1],al	;	~
	mov [es:1184+2*160+2*10+1],al	;	~
	mov [es:1184+2*160+2*11+1],al	;	~
	mov [es:1184+2*160+2*12+1],al	;	~
	mov [es:1184+2*160+2*13+1],al	;	~
	mov [es:1184+3*160+2*10+1],al	;	~
	mov [es:1184+3*160+2*11+1],al	;	~
	mov [es:1184+3*160+2*12+1],al	;	~
	mov [es:1184+3*160+2*13+1],al	;	~
	xor al,al						;l�sche al, damit man sieht, ob es gesetzt wurde oder nicht
	test bl,00000100b				;pr�fen ob in StatusByte Bit2 f�r mittlere-Taste gesetzt ist, oder nicht,
	setnz al						;sodass al bei gesetztem Byte 1 und bei gel�schten Byte 0 ist
	mov cl,4Fh						;4Fh mit dem Bit multipliezieren, sodass bei gesetztem Bit die Hintergrundfarbe rot und die schriftfarbe wei� ist
	mul cl							;und ansonsten alles schwar ist, da 1*Zeichen = Zeichen und 0*Zeichen=kein Zeichen ist
	mov [es:1184+1*160+2*6+1],al	;setzt die Attribute der  Zeichen in der mittleren Taste im Bild so, dass diese rot wird. Ich habe sogar die Speicheraddressierung
	mov [es:1184+1*160+2*7+1],al	;   ...als Rechnung stehen lassen, damit diese besser nachvollziehbar ist, siehe auch nochmal den Befehl: mov [es:100h],word 1234h
	mov [es:1184+1*160+2*8+1],al	;	~
	mov [es:1184+2*160+2*6+1],al	;	~
	mov [es:1184+2*160+2*7+1],al	;	~
	mov [es:1184+2*160+2*8+1],al	;	~
	mov [es:1184+3*160+2*6+1],al	;	~
	mov [es:1184+3*160+2*7+1],al	;	~
	mov [es:1184+3*160+2*8+1],al	;	~

	xor al,al						;l�sche al, damit man sieht, ob es gesetzt wurde oder nicht
	test bl,00010000b				;testen ob Bit4 f�r negatives Movement gesetzt ist
	setnz al						;wenn ja, setze al auf 1
	mov cl,'-'-' '					;schiebe auf al das Monuszeichen-Space, damit es bei mul mit 0 und addition von space nur space
	mul cl							;und bei mul mit 1 und addition mit space nur minus wird -> multipliziere al(0 oder 1) mit dem "pre"-zeichen
	add al,' '						;und addiere zu al space, damit aus dem pre- ein vollst�ndiges zeichen wird
	mov [es:1184+6*160+2*7],al		;das Vorzeichen an der Stelle vor dem Movement platzieren
	movzx eax,byte[xcoord]			;nach eax die Anzahl der x-Bewegungen seit dem letzten Ping holen
	test bl,00010000b				;testen ob das SignBit von x-movment gesetzt ist, wenn nicht, ist das movement positiv
	jz .sign_x						;und �berspringe den Teil, der aus dem Overflowten, negativen Wert den Betrag erstellt
		not al						;negiere x-coord, damit der overflow aufgehoben wird, der ensteht, wenn man 0-1 rechnet
		inc al						;da aber -1 = FFh was genoted 0 w�re, addiere nocheinmal 1
	.sign_x:
	mov ebx,10						;nun kann f�r die Spezial-Zahlausgabe als Basis 10 f�r das Dezimale System festgelgt werden
	lea di,[1184+6*160+2*11]		;auf di die Adresse im SPeicher, die am Ende der Zahl liegt geschoben werden
	mov cx,3						;und auf cx 3 geschoben werden
	call ddispdec					;nun kann die Prozedur aufgerufen werden, die die Zahl gleich in den Video-Speicher bei es:di speichert, statt in10h zu nutzen
	
	xor al,al						;l�sche al, damit man sieht, ob es gesetzt wurde oder nicht
	test byte[status],00100000b		;testen ob Bit4 f�r negatives Movement gesetzt ist (deswegen hier pl�tzlich [status], weil bl vorhin durch ebx mit 10 gel�scht wurde)
	setnz al						;wenn ja, setze al auf 1
	mov cl,'-'-' '					;schiebe auf al das Monuszeichen-Space, damit es bei mul mit 0 und addition von space nur space
	mul cl							;und bei mul mit 1 und addition mit space nur minus wird -> multipliziere al(0 oder 1) mit dem "pre"-zeichen
	add al,' '						;und addiere zu al space, damit aus dem pre- ein vollst�ndiges zeichen wird
	mov [es:1184+7*160+2*7],al		;das Vorzeichen an der Stelle vor dem Movement platzieren
	movzx eax,byte[ycoord]			;nach eax die Anzahl der y-Bewegungen seit dem letzten Ping holen
	test byte[status],00100000b		;testen ob das SignBit vom y-movment gesetzt ist, wenn nicht, ist das movement positiv ([status], weil bl zerst�rt wurde(durch dispdec))
	jz .sign_y						;und �berspringe den Teil, der aus dem Overflowten, negativen Wert den Betrag erstellt
		not al						;negiere x-coord, damit der overflow aufgehoben wird, der ensteht, wenn man 0-1 rechnet
		inc al						;da aber -1 = FFh was genoted 0 w�re, addiere nocheinmal 1
	.sign_y:
	lea di,[1184+7*160+2*11]		;auf di die Adresse im Video-Speicher, die am Ende der Zahl liegt, schieben
	mov cx,3						;und auf cx 3 schieben
	call ddispdec					;nun kann die Prozedur aufgerufen werden, die die Zahl gleich in den Video-Speicher bei es:di speichert, statt in10h zu nutzen

	movzx eax,word[x]				;auf eax die auszugebende Zahl, die aktuelle x-Position des MausZeigers, schieben
	lea di,[1184+8*160+2*10]		;auf di die Adresse im Video-Speicher, die am Ende der Zahl liegt, schieben
	mov cx,5						;auf cx 5 schieben, da die auszugebende Zahl maximal 5 Stellen lang sein wird/werden kann
	call ddispdec					;nun kann die Prozedur aufgerufen werden, die die Zahl gleich in den Video-Speicher bei es:di speichert, statt in10h zu nutzen
	
	movzx eax,word[y]				;auf eax die auszugebende Zahl, die aktuelle y-Position des MausZeigers, schieben
	lea di,[1184+9*160+2*10]		;auf di die Adresse im Video-Speicher, die am Ende der Zahl liegt, schieben
	mov cx,5						;auf cx 5 schieben, da die auszugebende Zahl maximal 5 Stellen lang sein wird/werden kann
	call ddispdec					;nun kann die Prozedur aufgerufen werden, die die Zahl gleich in den Video-Speicher bei es:di speichert, statt in10h zu nutzen
	
	movzx eax,byte[zbtn45]
	lea di,[1184+5*160+2*10]
	mov cx,8
	mov ebx,2
	call ddispdec
	
	popa		;Register wiederherstellen
	ret			;und Prozedur beenden und zum Aufrufsort zur�ckkehren

ddispdec:	;dies ist die SpezialProzedur, die an die Adresse es:di r�ckw�rtsschreibend (Anfang der Zahl wird es:di-n sein) die Zahl, die in eax liegt schreibt und 
			;den Rest des Platzes, der in cx angegeben ist, aber vlt. von der Zahl nicht ganz ausgenutzt worden ist mit Leezeichen/Space f�llt
	pushad	;Register sichern
	STD		;Direction-Flag setzen, sodass di nach stosx dekrementiert, statt inkrementiert wird
	.loop:
		xor edx,edx			;edx l�schen, da hier nach der Division der Modulo ist und der sonst mit dividier werden w�rde (Dividend w�re edx:eax)
		div ebx				;Zahl in eax (eig. edx:eax, aber da edx ja gel�scht ist...) durch ebx teilen
		xchg al,dl			;den Modulo in dx nach al holen um al so auch gleich in dl zu speichern
		add al,'0'			;zu dem Modulo 48 addieren, damit aus der Zahl ein Ascii-Zeichen wird
		stosb				;und das Zeichen in den Video-Speicher schreiben (die Zahl 123 w�rde nacheinander 3,2,1 in den Speicher geschrieben werden)
		dec di				;di noch einmal extra dekrementieren, da nur ein Zeichen geschrieben wurde und di jetzt auf das Attribut-Byte zeigt
		xchg al,dl			;al und dl wieder tauschen, sodass in eax wie vorher der ganzteilige Quotient der Division liegt
		or eax,eax			;testen ob die Zahl vollst�ndig ausgegeben wurde
			jz .loop_end	;wenn ja, verlasse den loop -> in cx ist dann die Anzahl der Zeichen, die die Zahl einnehmen sollte, die werden dann gel�scht
		loop .loop			;ansonsten f�hre die schleife so oft aus, bis cx 0 ist
	.loop_end:
	mov ax,0F20h	;nach Ende des Ausgebens der Zahl, schiebe auf ax ein Zeichen(Space/20h) und das Attribut-Byte
	rep stosw		;um mit der schnellen Variante mit rep die �brigen in cx angegeben Felder mit Space zu f�llen
	CLD				;danach l�sche das Direction-Flag, da es standardm��g gel�scht sein sollte
	popad			;dann stelle die Register wieder her
	ret				;und kehre zum Aufrufsort zur�ck
