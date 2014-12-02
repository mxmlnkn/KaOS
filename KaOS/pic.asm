;**********************************************************************
;******* PIC -die Programmierung des programmable Interrupt-Controllers *******
;**********************************************************************
org 100h						;üblicher Header bei Com-Dateien und auch bei meinem OS(symbolisiert, dass IP jetzt 100h ist)

mov ax,cs						;hole aktives Segment nach ax
mov ds,ax						;um es nach ds
mov es,ax						;und es zu kopieren

start:

lea si,[welcome_msg]			;lade auf si offset de nachricht
call write						;um diese mit write auszugeben
lea si,[chars_for_readln_1]		;lade nach si den offset der zeichenkette mit den möglichen Zeichen für die Eingabe
lea di,[temp_readln_str]		;lade nach di den offset, wo das eingegeben gespeichert wird
mov cx,1						;schiebe nach cx, 1, da der user nur eine ein-zeichen.lange nachricht eintippen soll
call readln_special				;rufe die spezial-read-prozedure auf
mov al,[temp_readln_str]		;hole nach al das Zeichen, was der User eingegeben hat
sub al,48						;ziehe davon 48 ab -> 48 = "0"
jz is_0							;falls er 0 eingegeben hat springe nach INIT
	jmp not_0					;ansonsten überspringe das und teste auf andere Eingaben
is_0:
	lea si,[init_msg]			;auf si offset der Initiliations-Message schieben
	call write					;um diese auszugeben
	lea si,[chars_for_readln_2]	;auf si string mit allen HEX-Ziffern schieben, um nur diese als Einagbe zu erlauben
	lea di,[temp_readln_str]	;auf die offset des strings, wo die Eingabe gespeichert wird speichern
	mov cx,2					;auf cx 2 schieben, da die HEX-Zahl entsprechend der INT-Anz. 2 Stellen lang sein kann
	call readln_special_mul8	;ruft die Abwandlung der Special-Readln-Prozedur auf, die als zweite Ziffer nur 8 oder 0 zulässt
	
	mov cx,[temp_readln_str]	;hole nach cl das erste eingegebene zeichen und nach ch das entsprechend zweite
	sub cx,3030h				;ziehe von beiden zeichen 48 ab um eventuelle zahl-zeichen in die entsprechende zahl umzuwandeln
	cmp cl,10					;testen ob cl eine zahl ist
	jb cl_is_zahl				;wenn ja, überspringe folgenden schritt und teste ch
		sub cl,39				;wenn das ein Buchstabe war, ziehe nocheinmal 39 ab, um auch diesen zu konvertieren
	cl_is_zahl:
	cmp ch,10					;testen ob ch eine zahl ist
	jb ch_is_zahl				;wenn ja, überspringe folgenden schritt
		sub ch,39				;wenn das ein Buchstabe war, ziehe nocheinmal 39 ab, um auch diesen zu konvertieren
	ch_is_zahl:
	shl cl,4					;mit 16 (2^4), also der Basis des hexadezimalen Systems multiplizieren
	or cl,ch					;und das zweite zeichen hinzuaddieren, sollte statt add auch mit or funktionieren
	
	push cx						;cx sichern, da das für readln_special benötogt wird, aber darin auch der startint vom Master liegt
	lea si,[init_quest_3]		;auf si offset der dritten, optionalen Frage schieben
	call write					;um auch diese dem anwender vorzuführen
	lea si,[chars_for_readln_2]	;auf si string mit allen HEX-Ziffern schieben, um nur diese als Einagbe zu erlauben
	lea di,[temp_readln_str]	;auf die offset des strings, wo die Eingabe gespeichert wird speichern
	mov cx,2					;auf cx 2 schieben, da die HEX-Zahl entsprechend der INT-Anz. 2 Stellen lang sein kann
	call readln_special_mul8	;ruft die Abwandlung der Special-Readln-Prozedur auf, die als zweite Ziffer nur 8 oder 0 zulässt
	pop cx						;cx wiederherstellen
	
	mov bx,[temp_readln_str]	;hole nach bl das erste eingegebene zeichen und nach bh das entsprechend zweite
	sub bx,3030h				;ziehe von beiden zeichen 48 ab um eventuelle zahl-zeichen in die entsprechende zahl umzuwandeln
	cmp bl,10					;testen ob bl eine zahl ist
	jb bl_is_zahl				;wenn ja, überspringe folgenden schritt und teste ch
		sub bl,39				;wenn das ein Buchstabe war, ziehe nocheinmal 39 ab, um auch diesen zu konvertieren
	bl_is_zahl:
	cmp bh,10					;testen ob bh eine zahl ist
	jb bh_is_zahl				;wenn ja, überspringe folgenden schritt
		sub bh,39				;wenn das ein Buchstabe war, ziehe nocheinmal 39 ab, um auch diesen zu konvertieren
	bh_is_zahl:
	shl bl,4					;mit 16 (2^4), also der Basis des hexadezimalen Systems multiplizieren
	or bl,bh					;und das zweite zeichen hinzuaddieren, sollte statt add auch mit or funktionieren

	mov al,00010001b			;dies ist das ICW1 und wird angeben, dass das Slave-PIC genutzt wird, als auch dass ein ICW4 gesendet werden wird
	out 20h,al					;ICW1 nun an den PIC1 senden
	out 0A0h,al					;ICW1 auch an den PIC2 senden
	mov al,cl					;auf al ICW2 kopieren, was den vom user gewünschten Startinterrupt enthält
	out 21h,al					;und an den Master-PIC senden
	mov al,bl					;nun auf al das ICW2 für den Slave-PIC senden, was auch den Startint enthält, aber hoffentlich anders wie der vom master ist :)
	out 0A1h,al					;ICW2 an den Slave senden
	mov al,00000010b			;das Bit was in diesem ICW3 gesetzt ist, signalisiert dem Master, dass er über den IRQ2 mit dem PIC verbunden ist
	out 21h,al					;ICW3 an Master senden
	out 0A1h,al					;das ICW3 an den Slave senden
	mov al,00000001b			;ICW4 auf al so initialisieren, dass ein manueller EOI erwartet wird, die anderen Bits werden alle nicht gebraucht
	out 21h,al					;ICW4 an PIC1 senden
	out 0A1h,al					;ICW4 auch an Slave senden
	
	lea si,[init_succes]		;Nachricht, dass alles ordnungsgemäß durchgeführt wurde nach si laden
	call write					;und ausgeben
	jmp start					;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen

not_0:
dec al							;decrementiere um 1, sodass bei eingabe von 1 al nun 0 wäre
jnz not_1						;falls dem jedoch nicht so ist, überspringe auch ACTIVATE

	mov cx,10						;auf cx 10 schieben, da in dem ersten loop die ziffern 0-9 in die tabelle an das statusbyte geschoben werden
	mov al,48						;initialisiere al mit "0"
	lea di,[show_irq_msg_table+58]	;in der IRQ-Tabelle auf die stelle springen, die sonst status anzeigt. die wird jetzt entsprechenden KEY anzeigen :)
	.loop_sign_digits:
		stosb						;das zeichen in al an entsprechender statusbyte-stelle speichern
		inc al						;al erhöhen (aus "0" wird "1")
		add di,60					;den di-zeiger auf nächstes Status-Byte stellen
		loop .loop_sign_digits		;das für alle 10 ziffern ausführen
	mov cx,6						;auf cx 6 schieben, da wir "a" nur 6x bis "f" erhöhen wollen um IRQ 11-15 zu signieren
	add al,39						;nun da al 58 ist 39 addieren um auf 97 ("a") zu kommen um fortführend die StatusBytes mit Buchstaben zu füllen
	.loop_sign_alpha:
		stosb						;das zeichen in al an entsprechender statusbyte-stelle speichern
		inc al						;al erhöhen (aus "a" wird "b")
		add di,60					;den di-zeiger auf nächstes Status-Byte stellen
		loop .loop_sign_alpha		;das für die Buchstaben A-F ausführen und dann haben wir jedem IRQ ein Zeichen zugewiesen, was der Anwender eingeben kann
	
	lea si,[activate_msg]			;auf si offset für msg zur aktivierung eines IRQ kopieren
	call write						;diese message ausgeben
	lea si,[show_irq_msg_table]		;auf si offset der irq-auswahl/tabelle schieben, die wir davor so schön formatiert haben
	call write						;um auch diese auszugeben
	lea si,[wait_for_input]			;zuletzt auf sie offset der msg zum warten auf eingabe kopieren
	call write						;um auch diese auszugeben
	lea si,[chars_for_readln_2]		;lade nach si den offset der zeichenkette mit den möglichen Zeichen für die Eingabe
	lea di,[temp_readln_str]		;lade nach di den offset, wo das eingegeben gespeichert wird
	mov cx,1						;schiebe nach cx, 1, da der user nur eine ein-zeichen.lange nachricht eintippen soll
	call readln_special				;rufe die spezial-read-prozedure auf
	mov al,[temp_readln_str]		;hole nach al das Zeichen, was der User eingegeben hat
	sub al,48						;ziehe davon 48 ab -> 48 = "0"
	cmp al,10						;al mit 10 vergleichen um zu prüfen, ob die eingabe eine zahl oder ein zeichen war
	jb .zahl						;wenn es keine zahl war,
		sub al,39					;subtrahiere von al 39 um sozusagenaus dem alpha-bereich in den num-bereich zu kommen
	.zahl:							;hier sollte nun in al eine zahl von 0-15 vorliegen
	mov cl,al						;auf cl al verschieben, da nur cl für rol angenommen wird
	mov bl,11111110b				;auf bl diese vordefinierte Bitmaske verschieben, die dann später noch für "0" gerolet und and-verknüpft wird
	cmp cl,8						;testen ob der gewählte IRQ 8 oder höher ist,
	jae	remask_slave				;wenn ja, gehe zu Prozedur, die die OCWs an den Slave sendet
		rol bl,cl					;rotiere al so, dass "0" an der Stelle des zu aktivierenden IRQs (siehe IMR)
		in al,21h					;hole vom Master das IMR, worin die aktuell unterdrückten IRQs als gesetztes Bit gespeichert sind
		and al,bl					;bl und al and-verknüpfen, sodass alle so bleiben wie vorher, außer das 0-Bit, was wir mit rol verschoben habe, das wird im IMR gelöscht
		out 21h,al					;das IMR als OCW1 an Port 21h senden
		jmp start					;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen
	remask_slave:
		sub cl,8					;von cl 8 abziehen, damit aus IRQ8-15 die Bits 0-7 werden
		rol bl,cl					;rotiere al so, dass "0" an der Stelle des zu aktivierenden IRQs (siehe IMR)
		in al,0A1h					;hole vom Slave das IMR, worin die aktuell unterdrückten IRQs als gesetztes Bit gespeichert sind
		and al,bl					;bl und al and-verknüpfen, sodass alle so bleiben wie vorher, außer das 0-Bit, was wir mit rol verschoben habe, das wird im IMR gelöscht
		out 0A1h,al					;das IMR als OCW1 an Port A1h senden
	jmp start						;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen

not_1:
dec al							;decrementiere um 1, sodass bei eingabe von 2 al nun 0 wäre
jnz not_2						;falls dem jedoch nicht so ist, überspringe auch SUPPRESS

;NOTE: DER PROGRAMMTEIL ZUM DEAKTIVIEREN DER IRQS IST BIS AUF DEN BITVERKNÜPFUNGSTEIL IDENTISCH MIT DEM PROGRAMMTEIL ZU AKTIVIEREN DER IRQS
	mov cx,10						;auf cx 10 schieben, da in dem ersten loop die ziffern 0-9 in die tabelle an das statusbyte geschoben werden
	mov al,48						;initialisiere al mit "0"
	lea di,[show_irq_msg_table+58]	;in der IRQ-Tabelle auf die stelle springen, die sonst status anzeigt. die wird jetzt entsprechenden KEY anzeigen :)
	.loop_sign_digits:
		stosb						;das zeichen in al an entsprechender statusbyte-stelle speichern
		inc al						;al erhöhen (aus "0" wird "1")
		add di,60					;den di-zeiger auf nächstes Status-Byte stellen
		loop .loop_sign_digits		;das für alle 10 ziffern ausführen
	mov cx,6						;auf cx 6 schieben, da wir "a" nur 6x bis "f" erhöhen wollen um IRQ 11-15 zu signieren
	add al,39						;nun da al 58 ist 39 addieren um auf 97 ("a") zu kommen um fortführend die StatusBytes mit Buchstaben zu füllen
	.loop_sign_alpha:
		stosb						;das zeichen in al an entsprechender statusbyte-stelle speichern
		inc al						;al erhöhen (aus "a" wird "b")
		add di,60					;den di-zeiger auf nächstes Status-Byte stellen
		loop .loop_sign_alpha		;das für die Buchstaben A-F ausführen und dann haben wir jedem IRQ ein Zeichen zugewiesen, was der Anwender eingeben kann
	
	lea si,[deactivate_msg]			;auf si offset für msg zur deaktivierung eines IRQ kopieren
	call write						;diese message ausgeben
	lea si,[show_irq_msg_table]		;auf si offset der irq-auswahl/tabelle schieben, die wir davor so schön formatiert haben
	call write						;um auch diese auszugeben
	lea si,[wait_for_input]			;zuletzt auf sie offset der msg zum warten auf eingabe kopieren
	call write						;um auch diese auszugeben
	lea si,[chars_for_readln_2]		;lade nach si den offset der zeichenkette mit den möglichen Zeichen für die Eingabe
	lea di,[temp_readln_str]		;lade nach di den offset, wo das eingegeben gespeichert wird
	mov cx,1						;schiebe nach cx, 1, da der user nur eine ein-zeichen.lange nachricht eintippen soll
	call readln_special				;rufe die spezial-read-prozedure auf
	mov al,[temp_readln_str]		;hole nach al das Zeichen, was der User eingegeben hat
	sub al,48						;ziehe davon 48 ab -> 48 = "0"
	cmp al,10						;al mit 10 vergleichen um zu prüfen, ob die eingabe eine zahl oder ein zeichen war
	jb .zahl						;wenn es keine zahl war,
		sub al,39					;subtrahiere von al 39 um sozusagenaus dem alpha-bereich in den num-bereich zu kommen
	.zahl:							;hier sollte nun in al eine zahl von 0-15 vorliegen
	mov cl,al						;auf cl al verschieben, da nur cl für rol angenommen wird
	mov bl,00000001b				;auf bl diese vordefinierte Bitmaske verschieben, die dann später noch für "0" gerolet und or-verknüpft wird
	cmp cl,8						;testen ob der gewählte IRQ 8 oder höher ist,
	jae	remask_slave_2				;wenn ja, gehe zu Prozedur, die die OCWs an den Slave sendet
		rol bl,cl					;rotiere al so, dass "0" an der Stelle des zu aktivierenden IRQs (siehe IMR)
		in al,21h					;hole vom Master das IMR, worin die aktuell unterdrückten IRQs als gesetztes Bit gespeichert sind
		or al,bl					;bl und al or-verknüpfen, sodass alle so bleiben wie vorher, außer das 1-Bit, was wir mit rol verschoben habe, das wird im IMR gesetzt
		out 21h,al					;das IMR als OCW1 an Port 21h senden
		jmp start					;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen
	remask_slave_2:
		sub cl,8					;von cl 8 abziehen, damit aus IRQ8-15 die Bits 0-7 werden
		rol bl,cl					;rotiere al so, dass "0" an der Stelle des zu aktivierenden IRQs (siehe IMR)
		in al,0A1h					;hole vom Slave das IMR, worin die aktuell unterdrückten IRQs als gesetztes Bit gespeichert sind
		or al,bl					;bl und al or-verknüpfen, sodass alle so bleiben wie vorher, außer das 1-Bit, was wir mit rol verschoben habe, das wird im IMR gesetzt
		out 0A1h,al					;das IMR als OCW1 an Port A1h senden
	jmp start						;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen

not_2:
dec al							;decrementiere um 1, sodass bei eingabe von 3 al nun 0 wäre
jnz not_3						;falls dem jedoch nicht so ist, überspringe auch SHOW

	in al,0A1h						;holt IMR des Slave-PICs nach al
	mov ah,al						;und zischenspeichert es in ah
	in al,21h						;um das IMR des PIC1 zu holen
	
	mov bx,ax						;auf bx ax zwischenspeichern, da ax für setx und stosx beötigt wird
	mov cx,16						;auf cx 16 schieben, damit die schleife 16x ausgeführt wird
	lea di,[show_irq_msg_table+58]	;lade für stosx nach edi offset von string bitmask_imr
	load_bitmask_imr:
		xor al,al					;al löschen, damit es NUR bei gesetztem carry-flag 1 ist, und SONST 0 ist
		rcr	bx,1					;über carry-flag richtung bit 15 rotieren um 1 rotieren
		setc al						;wenn das carryflag 1 ist, also wenn das bit, was zuvor an stelle 15 von ax war, gesetzt war, setze auch al
		add al,48					;addiere zu al 48, damit als Ascii-char 0 bei nichtgesetztem bit und 1 bei gesetztem bit ausgegeben/gespeichert wird
		stosb						;speichere al nach es:di und erhöhe edi um 1
		add di,60					;erhöhe di extra nocheinmal um 60, damit di vor dem nächsten "?" platziert wird
	loop load_bitmask_imr			;decrementiere cx und wiederhole schleife -> schleife wird 16x wiederholt, da in ax 16bits sind
	lea si,[show_irq_msg]			;lade für write nach si den Offset der Nachricht, in der wir gerade die Status-anzeigen entsprechend den Bits modifiziert haben
	call write						;gebe Nachricht aus
	
	xor ax,ax						;lösche ax für Funktion 0h bei int 16h
	int 16h							;rufe int16h auf -> warten auf usertastaturdruck
	jmp start						;springe zum Anfang zurück um den Nutzer nocheinmal die Auswahl zu zeigen

not_3:
	retf							;zu meinem os zurückspringen, bei DOS und WINx wäre der Befehl mov ah,4ch -> int 21h!!!

init_succes			db 13,10,"PIC with your typed Inputs initialized.",0
j_n_digits			db 2,0,"yn"
init_msg			db 13,10,"This Tool will initiliziate the PIC with your values."
					db 13,10,"You will asked for the startint and either slave is off/on."
					db 13,10,13,10,"Define INT for IRQ0 (Master)! Must be a multiplier of 8 in HEX!  -> ",0
init_quest_3		db 13,10,"Define INT for IRQ8 (Slave)! Must be a multiplier of 8 in HEX!   -> ",0
chars_for_readln_2	db 16,0,"1234567890abcdef"
deactivate_msg		db 13,10,13,10,"Which IRQ do you want to suppres? Type the specific key for that IRQ!",13,10,0
activate_msg		db 13,10,13,10,"Which IRQ do you want to activate? Type the specific key for that IRQ!",13,10,0
wait_for_input		db "Input: ",0
show_irq_msg		db 13,10,13,10
					db "The next 15 Lines will be the names of the 15 IRQs and its status",13,10," 1 -> suppressed",13,10
					db " 0 -> activated",13,10
show_irq_msg_table	db "  IRQ 0  - PIT - Programmable Intervall Timer          -> ?",13,10		;Die Status-Bytes, die später über das Programm in 1 oder 0
					db "  IRQ 1  - PS/2-Mouse or -Keyboard                     -> ?",13,10		;geändert werden sollen, sind jetzt noch als "?"
					db "  IRQ 2  - Slave-PIC-Bridge (former Video-Interrupt)   -> ?",13,10		;symbolisiert und haben einen Abstand zwischeinander
					db "  IRQ 3  - COM2                                        -> ?",13,10		;von 60 Bytes.
					db "  IRQ 4  - COM1                                        -> ?",13,10
					db "  IRQ 5  - Parallel Printer Port - LPT2                -> ?",13,10
					db "  IRQ 6  - Floppy-Controller                           -> ?",13,10
					db "  IRQ 7  - Parallel Printer Port - LPT1                -> ?",13,10
					db "  IRQ 8  - Realtimeclock                               -> ?",13,10
					db "  IRQ 9  - Slave-PIC-Bridge                            -> ?",13,10
					db "  IRQ 10 - reserved                                    -> ?",13,10
					db "  IRQ 11 - reserved                                    -> ?",13,10
					db "  IRQ 12 - Mouse-Interrupt                             -> ?",13,10
					db "  IRQ 13 - Coprocessor-Exception-Handler               -> ?",13,10		;Über die Prozedur, die die Bitfelder ausliest,
					db "  IRQ 14 - Harddisk-Controller - Primary Channel       -> ?",13,10		;werden dann durch stosb und erhöhung von di
					db "  IRQ 15 - Harddisk-Controller - Secondary Channel     -> ?",13,10,0	;um 60 die einzelnen Fragezeichen ersetzt.
chars_for_readln_1	db 5,0,"01234",0
temp_readln_str		db 0,0,0
welcome_msg			db 13,10,13,10,"Welcome to the PIC-Manager-Tool! What do you want to do?",13,10,"  [0] Init - Sets the PIC to the"
					db "Standard-Settings",13,10,"  [1] Activate IRQ",13,10,"  [2] Suppres IRQ",13,10,"  [3] Show current active IRQs"
					db 13,10,"  [4] EXIT",13,10,"Input: ",0

;***********************************************************************
;********* readln es:di=string ecx:laenge ds:esi allowed signs...  *********
;********* length of string defined in first word of string *********
;********* exp.: allowed_chars db 4,0,"0123" -> will only allow digits 0-3 *********
;***********************************************************************
readln_special:
	pushad							;Register sichern
	mov ebp, ecx					;zum Abgleich ecx auf ebp zwischensichern
	xor ecx, ecx					;ecx löschen, damit es von 0 an hochzählen kann ... bis zu ebp
	.readln_loop:
		xor ax,ax					;ax löschen, damit bei int16h funtion 0h gestartet wird
		int 16h						;int 16h aufrufen -> auf Tastendruck warten :) -> das ist die zentral funktion von readln
		or al,al					;al auf asciizeichen prüfen
			jz .readln_loop			;wenn al=0, dann warte auf nächstes zeichen
		cmp al,8					;al auf Backspace prüfen
		jne .nicht_zurueck			;wenn es keiner ist, überspringe die Backspace-Prozedure
			or cx,cx				;wenn cx jedoch schon 0 ist, also wenn kein zeichen mehr zum backspacen da ist
				jz .readln_loop		;dann warte auf das nächste zeichen
			
			dec cx					;ansonsten dekrementiere cx, da ja in zeichen gelöscht wird
			mov ah,0eh				;verschiebe auf ah 0eh - telerype output
			mov bx,2				;auf bx die farbe
			int 10h					;und gib den backspace aus -> bios geht automatisch ein schritt zurück
			xor al,al				;löscht das letzte zeichen jedoch nicht, weswegen auf al 0 kommt
			int 10h					;und das dann auf dem platz des letzten zeichens ausgegeben wird
			mov al,08h				;auf al Backspace schieben
			int 10h					;um es auszugeben und somit an stelle des gelöschten zeichens zu gelangen
			
			dec edi					;den pointer für stosx auf das letzte zeichen stellen(das zeichen wird entweder überschrieben oder als End of string mit 0 gelöscht)
			jmp .readln_loop		;warte auf nächstes zeichen
		.nicht_zurueck:
		cmp al,13					;testen, ob das zeichen enter ist
		jne .not_enter				;wenn nicht, dann überspringe folgendes
			cmp ecx, ebp			;teste ob die anz. der eingegeben zeichen gleich der ist, die man eingeben muss
				je .readln_end		;wenn der nutzer genug zeichen eingegeben hat, leite end-sequenz ein
				jmp .readln_loop	;ansonsten warte auf nächstes zeichen
		.not_enter:
		cmp ecx, ebp				;vergleiche, ob der nutzer die per cx bestimmte anzahl vonzeichen schon eingegeben hat
			jz .readln_loop			;wenn ja, dann nehme das zeichen nicht an und warte auf nächstes zeichen, was irgendwann backspace oder enter sein sollte
		
		push cx						;cx sichern, da das für rep benötigt wird
		push edi					;edi sichern, da das für scas beötigt wird
		mov edi,esi					;auf edi esi schieben, da scasb al (das aktuelle zeichen) mit dem wert an es:di abgleicht
		mov cx,[ds:edi]				;schiebe auf cx die länge des strings der für den user möglichen zeichen
		inc cx						;erhöhe cx um 1
		add edi,2					;da edi auf dem längenbezeichner nich steht, erhöhe es um 2, da die längenangabe als word vorliegen muss
		repne scasb					;vergleiche al mit den werten an di, bis es etwas übereinstimmendes gefunden hat, oder cx 0 ist
		or cx,cx					;teste ob cx 0 ist, denn wenn dem so wäre, dann wäre di schon auf einem zeichen nach dem string...deswegen vorhin auch inc cx
		pop edi						;edi wiederherstellen
		pop cx						;cx wiederherstellen
			jz .readln_loop			;wenn das cx 3 befehle weiter oben nun 0 ist, warte auf nächste eingabe, da das zeichen keins der erlaubten ist(pop beeinflust flags nicht)
		
		stosb						;wenn das zeichen nicht irgendwie vorher abgefangen wurden ist und hier gelandet ist, speichere es im Input-Stream-String
		mov ah,0eh					;auf ah 0eh für teletype-output
		mov bx,2					;auf bx farbe
		int 10h						;und nun gib das zeichen aus, was alle test bestanden hat
	inc ecx							;erhöhe ecx, da ja nun ein zeichen mehr in input-stream ist
	jmp .readln_loop				;und warte auf nächstes zeichen
	.readln_end:
		xor al,al					;lösche ax
		stosb						;und setze in string zeichen für stringende
	popad							;stelle alle register wieder her
	ret								;und springe zurück zum aufrufsort

	;***********************************************************************
	;************** write ds:si String bx:farbe Ausgabe:String *************
	;***********************************************************************
	write:
		pusha			;Word-Register speichern
		pushf			;Flagregister speichern
		cld				;direction-flag löschen
		mov ah,0Eh		;Funktion 0x0E	
		mov bx,7		;auf bx die Farbe des Textes laden... standard white-grey
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

;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pushad				;Register speichern
	pushf				;Flags speichern
	xor ecx,ecx			;ecx löschen, damit von 0 an hochgezählt werden kann
	or ebx,ebx			;testen ob ebx 0 ist, denn wäre dem so, käme es zu einer division durch 0
	jnz zahlausgabe1	;wenns nciht 0 ist, fahre wie gewohnt fort
		mov ebx,16		;falls aber doch, ersetze ebx mit der basis für das hesadezimale Zahlensystem
zahlausgabe1:
	xor edx,edx			;lösche edx, damit das nicht mit dividiert wird(hier landt der modulo der division)
	div ebx				;mit der basis dividieren um den rest zu erhalten
	push dx				;und zu speichern... der rest ist bei 16 beo 0-15 und somit später umgewandelt bei 0-9 und a-f
	inc cx				;erhöhe cx damit man weiß, wie viele ziffern im stack sind
	or eax,eax			;teste ob eax 0 ist und somit schon alle ziffern gepusht worden sind
	jne zahlausgabe1	;wenn nicht, hole nächste ziffern durch modulo

	mov ah,0eh			;schiebe auf ah 0eh für teletype-output
	mov bx,2			;auf bx farbe
	zahlausgabe2:
	pop dx				;hole das zuletzt erhaltene zeichen/ziffer vom stack
	cmp dx,10			;vergleiche ob sie noch im bereich der zahlen ist
	jb zahl				;wenn ja überspringe folgendes
		add dl,7		;ansonsten wechsle in den Bereich der großen Buchstaben
		zahl:
			add dl,48	;addiere 48 ("0") um ins Zahlsystem bzw. Große Alphabet zu kommen
			mov al,dl	;auf al das berechnete zeichen verschieben (pop ax deswegen nicht, weil dann ah schrott wäre)
			int 10h		;zeichen ausgeben
	loop zahlausgabe2	;wiederhole dies für jedes im Stack gespeicherte Zeichen
	popf				;stelle Flag wieder her
	popad				;stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück
	
;***********************************************************************
;********************** Break Erg:Zeilenumbruch ************************
;***********************************************************************
break:
	push ax			;ax speichern
	push bx			;bx speichern
	pushf			;die flags speichern
	mov ah,0eh		;auf ah 0eh für teletype-output
	mov bx,2		;auf bx farbe(eig. egal)
	mov al,13		;auf al char #13
	int 10h			;ausgeben
	mov al,10		;das selbe mit 10
	int 10h			;und auch ausgeben(13 und 10 = break)
	popf			;flags wiederherstellen
	pop bx			;bx wiederherstellen
	pop ax			;ax wiederherstellen
	ret				;zurück zu Aufrufsort springen

;***********************************************************************
;********* readln es:di=string ecx:laenge ds:esi allowed signs...  *********
;********* length of string defined in first word of string *********
;********** Diese Prozedur erlaubt als zweites Zeichen nur 8 oder 0 *******
;********* und ist eig. nur für cx=2 vorgesehen *********
;********* exp.: allowed_chars db 4,0,"0123" -> will only allow digits 0-3 *********
;***********************************************************************
readln_special_mul8:
		;DER JETZT EINGERÜCKTE TEIL IST NUR EINE ABWANDLUNG DER READLN-SPECIAL-PROZEDURE, DA ES SO SEIN SOLL, DASS DER USER ALS
		;ZWEITES ZEICHEN NUR "8" ODER "0" EINGEBEN KÖNNEN SOLL, SODASS DIE EINGABE(IN HEXADEZIMALER FORM) NUR DURCH 8 TEILBAR SEIN KANN
		pushad							;Register sichern
		mov ebp, ecx					;zum Abgleich ecx auf ebp zwischensichern
		xor ecx, ecx					;ecx löschen, damit es von 0 an hochzählen kann ... bis zu ebp
		.readln_loop_80:
			xor ax,ax					;ax löschen, damit bei int16h funtion 0h gestartet wird
			int 16h						;int 16h aufrufen -> auf Tastendruck warten :) -> das ist die zentral funktion von readln
			or al,al					;al auf asciizeichen prüfen
				jz .readln_loop_80		;wenn al=0, dann warte auf nächstes zeichen
			cmp al,8					;al auf Backspace prüfen
			jne .nicht_zurueck_80		;wenn es keiner ist, überspringe die Backspace-Prozedure
				or cx,cx				;wenn cx jedoch schon 0 ist, also wenn kein zeichen mehr zum backspacen da ist
					jz .readln_loop_80	;dann warte auf das nächste zeichen
				
				dec cx					;ansonsten dekrementiere cx, da ja in zeichen gelöscht wird
				mov ah,0eh				;verschiebe auf ah 0eh - telerype output
				mov bx,2				;auf bx die farbe
				int 10h					;und gib den backspace aus -> bios geht automatisch ein schritt zurück
				xor al,al				;löscht das letzte zeichen jedoch nicht, weswegen auf al 0 kommt
				int 10h					;und das dann auf dem platz des letzten zeichens ausgegeben wird
				mov al,08h				;auf al Backspace schieben
				int 10h					;um es auszugeben und somit an stelle des gelöschten zeichens zu gelangen
				
				dec edi					;den pointer für stosx auf das letzte zeichen stellen(das zeichen wird entweder überschrieben oder als End of string mit 0 gelöscht)
				jmp .readln_loop_80		;warte auf nächstes zeichen
			.nicht_zurueck_80:
			cmp al,13					;testen, ob das zeichen enter ist
			jne .not_enter_80			;wenn nicht, dann überspringe folgendes
				cmp ecx, ebp			;teste ob die anz. der eingegeben zeichen gleich der ist, die man eingeben muss
					je .readln_end_80	;wenn der nutzer genug zeichen eingegeben hat, leite end-sequenz ein
					jmp .readln_loop_80	;ansonsten warte auf nächstes zeichen
			.not_enter_80:
			cmp ecx, ebp				;vergleiche, ob der nutzer die per cx bestimmte anzahl vonzeichen schon eingegeben hat
				jz .readln_loop_80		;wenn ja, dann nehme das zeichen nicht an und warte auf nächstes zeichen, was irgendwann backspace oder enter sein sollte
			
			push cx						;cx sichern, da das für rep benötigt wird
			push edi					;edi sichern, da das für scas beötigt wird
			mov edi,esi					;auf edi esi schieben, da scasb al (das aktuelle zeichen) mit dem wert an es:di abgleicht
			mov cx,[ds:edi]				;schiebe auf cx die länge des strings der für den user möglichen zeichen
			inc cx						;erhöhe cx um 1
			add edi,2					;da edi auf dem längenbezeichner nich steht, erhöhe es um 2, da die längenangabe als word vorliegen muss
			repne scasb					;vergleiche al mit den werten an di, bis es etwas übereinstimmendes gefunden hat, oder cx 0 ist
			or cx,cx					;teste ob cx 0 ist, denn wenn dem so wäre, dann wäre di schon auf einem zeichen nach dem string...deswegen vorhin auch inc cx
			pop edi						;edi wiederherstellen
			pop cx						;cx wiederherstellen
				jz .readln_loop_80		;wenn das cx 3 befehle weiter oben nun 0 ist, warte auf nächste eingabe, da das zeichen keins der erlaubten ist(pop beeinflust flags nicht)
			
			;NEW !!! -DIESER TEIL UNTERSCHEIDET SICH VON DER NORMALEN READLN_SPECIAL
			cmp cx,1						;überprüfen ob schon das erste Zeichen eingegeben wurden ist
			jne .isnt_second_digit			;wenn nicht, dann mache wie gewohnt weiter
				cmp al,"8"					;wenn aber doch, prüfe ob diese zweite Ziffer eine 8 ist oder nicht
					je .isnt_second_digit	;wenn dem so ist, mach wie gewohnt weiter, da die HEX-Zahl mit 8 als zweiter Ziffer durch 8 teilbar sein muss, wie bei der Eingabe gefordert worde
				cmp al,"0"					;ist dem jedoch nicht so, prüfe ob die zweite Ziffer eine 0 ist, da diese Zahl bei einer 0 an letzter Stelle auch durch 8 teilbar sein muss
					jne .readln_loop_80		;ist dies jedoch nicht so, warte auf nächstes zeichen, da die Zahl, die versucht wird einzugeben nicht durch 8 teilbar wäre
			.isnt_second_digit:				;hier gelangt das zeichen nur hin, wenn die zahl, die versucht wird einzugeben durch 8 teilbar sein wird
			;NEW !!! -DIESER TEIL UNTERSCHEIDET SICH VON DER NORMALEN READLN_SPECIAL
			
			stosb						;wenn das zeichen nicht irgendwie vorher abgefangen wurden ist und hier gelandet ist, speichere es im Input-Stream-String
			mov ah,0eh					;auf ah 0eh für teletype-output
			mov bx,2					;auf bx farbe
			int 10h						;und nun gib das zeichen aus, was alle test bestanden hat
		inc ecx							;erhöhe ecx, da ja nun ein zeichen mehr in input-stream ist
		jmp .readln_loop_80				;und warte auf nächstes zeichen
		.readln_end_80:
			xor al,al					;lösche ax
			stosb						;und setze in string zeichen für stringende
		popad							;stelle alle register wieder her
		ret
		;HIER ENDET DIESE ABWANDLUNG DER READLN-SPECIAL-PRZEDURE