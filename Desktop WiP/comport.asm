;ver. 0.7
org 100h

COM1 equ 03F8h		;Der Port des Com1, alle weitere Ports werden durch das Berechnen des Abstands zu diesen Port berechnet
COM2 equ 02F8h		;Der Port des Com2, alle weitere Ports werden durch das Berechnen des Abstands zu diesen Port berechnet
COM3 equ 03E8h		;Der Port des Com3, alle weitere Ports werden durch das Berechnen des Abstands zu diesen Port berechnet
COM4 equ 02E8h		;Der Port des Com4, alle weitere Ports werden durch das Berechnen des Abstands zu diesen Port berechnet

mov ax,0003h		;funktion 0h und MOdus 3h für 80x25Zeilen mit Anfangsadresse 0B800h im Speicher
int 10h				;und ausführen (mit dem Mode-Switch wird auch der Bildschirm gelöscht)

mov dx,COM1			;Auf dx die Anfangsadresse des COM1-Ports schieben um es init als Parameter zu übergeben
call init			;rufe die Prozedur Init auf, die in dx den Basisport des Com-Ports erwartet um diesen zu initialiseren

mov ax,0b800h		;auf ax b800h, die startadresse des Bildschirmspeichers schieben
mov es,ax			;um sie auf es zu kopieren um dann mit stosb die zeichen zu transferieren
push cs				;cs sichern
pop ds				;um cs auf ds zu kopieren, um die message aus dem speicher zu laden
lea di,[y_top*80*2+x_top_left*2];schiebe auf di offset des zeichen der stelle y=y_top x=x_top_left in screenspeicher
lea si,[com_pic]	;auf si den offset der zu schreibenden nachricht speichern
mov ah,0Fh			;auf ah die Attriubte schieben, sodass die schrift weiß ist
xor cx,cx			;cx löschen um von 0 an hochzuzählen
repeat_write:
	inc cx				;cx pro schritt um 1 erhöhen
	copy_row:
		lodsb			;das erste Char laden
		stosw			;und zusammen mit dem Attribut in den Grafikspeicher schieben
		or al,al		;testen ob das aus dem String ausgelesen Char dessen Ende anzeigt
		jnz copy_row	;wenn nicht, dann verfahre weiter mit dem Kopierend dieser Zeile
	push ax				;ax sichern, da das jetzt für Rechenoperation benötigt wird
	mov ax,cx			;auf ax, cx, also sozusagen die aktuell geschrieben Zeile(x-Achse), kopieren
	mov bx,160			;auf bx 160 verschieben, da mul nicht mit immediaten funzt,
	mul bx				;um ax mit 160, der Anzahl der Bytes, die eine Zeile einnimmt, multiplizieren
	add ax,4*160		;und die Bytes, der von uns beim Beginn des SChreibens übersprungenen 4 Zeilen addieren
	mov di,ax			;und di sozusagen an den anfang der nächsten Zeile stellen
	pop ax				;und ax wiederherstellen, der rest wird mit di gerechnet
	add di,28*2			;mit di 28 Zeichen im Screen weitergehen, sodass wir nun an x,28 sind
	
	cmp cx,17			;testen, ob schon alle 17 Zeilen kopiert wurden
	jne repeat_write	;wenn nicht, dann mache mit der nächsten Zeile weiter

com_control:
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call dtr.read		;dtr-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls dtr gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [dtr_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call txd.read		;txd-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls txd gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [txd_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call rxd			;rxd-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls rxd gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [rxd_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call dcd			;dcd-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls dcd gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [dcd_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call ri				;ri-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls ri gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [ri_var],al		;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call cts			;cts-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls cts gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [cts_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call rts.write		;rts-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls rts gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [rts_var],al	;schreibe nun unser schönes Zeichen in das Bild
	
	xor al,al			;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call dsr			;dsr-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setnz al			;wenn ZF gelöscht ist, also es NICHT 0 ist, dann setze al
	mov bl,'X'-'O'		;schiebe auf bl den Ascii-wert von X-O, welcher per mul angenommen wird, falls dsr gesetzt ist
	mul bl				;multipliziere den Zustand mit X-O, sodass dann bei addieren von O aus 0 O und aus 1(nun X-O) X wird
	add al,'O'			;O addieren... al ist nun bei gesetztem dtr X und bei gelöschtem O
	mov [dsr_var],al	;schreibe nun unser schönes Zeichen in das Bild

 xor ax,ax			;auf ah 0 schieben-> tastendruck abwarten
 mov ah,01
 int 16h			;ausführen-> warte auf key

 cmp al,'1'			;test ob die Taste "1" ist
 jne not_1			;wenn nicht überspringe folgendes und gehe weitere möglichkeiten durch
	xor bl,bl		;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call rts.read	;rts-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setz bl			;wenn ZF gesetzt ist, also es 0 ist, dann setze al (negieren)
	call rts.write	;setze das rts-register je nach zustand von bl, also umgekehrt des gelesenen
 not_1:
 cmp al,'2'			;test ob die Taste "2" ist
 jne not_2			;wenn nicht überspringe folgendes und gehe weitere möglichkeiten durch
	xor bl,bl		;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call dtr.read	;dtr-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setz bl			;wenn ZF gesetzt ist, also es 0 ist, dann setze al (negieren)
	call dtr.write	;setze das dtr-register je nach zustand von bl, also umgekehrt des gelesenen
 not_2:
 cmp al,'3'			;test ob die Taste "3" ist
 jne not_3			;wenn nicht überspringe folgendes und gehe weitere möglichkeiten durch
	xor bl,bl		;lösche al, um es per setxx zu setzen, falls das ZF-Flag nicht gesetzt ist
	call txd.read	;txd-Status auslesen, je nachdem wird ZF gesetzt/gelöscht
	setz bl			;wenn ZF gesetzt ist, also es 0 ist, dann setze al (negieren)
	call txd.write	;setze das txd-register je nach zustand von bl, also umgekehrt des gelesenen
 not_3:
 cmp al,27			;test ob die Taste "ESC" ist
jne com_control		;wenn ja, verlasse schleife

retf

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
;******************** dcd ZF : dcd dx : COM-adresse ********************
;***********************************************************************
dcd:
	pusha				;Register sichern
	add dx,6			;zu der Comport-Basisadresse 6 dazuaddieren, um in dx den port des Modem-Statusregisters zu haben
	in al,dx			;dann hole aus dem Port das MSR-Modem-Status-Register
	test al,10000000b	;testen ob Bit7 (DCD) gesetzt ist, sodass je nach dessen Zustand das Zeroflag gesetzt oder gelöscht ist
	popa				;Stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück

;***********************************************************************
;********************* ri ZF : ri dx : COM-adresse *********************
;***********************************************************************
ri:
	pusha				;Register sichern
	add dx,6			;zu der Comport-Basisadresse 6 dazuaddieren, um in dx den port des Modem-Statusregisters zu haben
	in al,dx			;dann hole aus dem Port das MSR-Modem-Status-Register
	test al,01000000b	;testen ob Bit6 (RI) gesetzt ist, sodass je nach dessen Zustand das Zeroflag gesetzt oder gelöscht ist
	popa				;Stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück

;***********************************************************************
;******************** dsr ZF : dsr dx : COM-adresse ********************
;***********************************************************************
dsr:
	pusha				;Register sichern
	add dx,6			;zu der Comport-Basisadresse 6 dazuaddieren, um in dx den port des Modem-Statusregisters zu haben
	in al,dx			;dann hole aus dem Port das MSR-Modem-Status-Register
	test al,00100000b	;testen ob Bit5 (DSR gesetzt ist, sodass je nach dessen Zustand das Zeroflag gesetzt oder gelöscht ist
	popa				;Stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück

;***********************************************************************
;******************** cts ZF : cts dx : COM-adresse ********************
;***********************************************************************
cts:
	pusha				;Register sichern
	add dx,6			;zu der Comport-Basisadresse 6 dazuaddieren, um in dx den port des Modem-Statusregisters zu haben
	in al,dx			;dann hole aus dem Port das MSR-Modem-Status-Register
	test al,00010000b	;testen ob Bit4 (CTS) gesetzt ist, sodass je nach dessen Zustand das Zeroflag gesetzt oder gelöscht ist
	popa				;Stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück

;***********************************************************************
;******************** rxd ZF : rxd dx : COM-adresse ********************
;***********************************************************************
rxd:
	pusha				;Register sichern
	add dx,5			;zu der Comport-Basisadresse 5 dazuaddieren, um in dx den port des Leitungs-Statusregisters zu haben
	in al,dx			;dann hole aus dem Port das LSR-Leitungs-Status-Register
	test al,00010000b	;testen ob Bit4 (RXD) gesetzt ist, sodass je nach dessen Zustand das Zeroflag gesetzt oder gelöscht ist
	popa				;Stelle Register wieder her
	ret					;und springe zum Aufrufsort zurück

;***********************************************************************
;************************* dx: com-basis-Adresse **************************
;******************** .write->schreibe BL-Zustand auf RTS *******************
;********************** .read->lese RTS-Zustand nach ZF *********************
;***********************************************************************
rts:
	.read
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		in al,dx			;und hole das MCR ab
		test al,00000010b	;teste ob im MCR das Bit1, der Zustand von RTS, gesetzt ist
		popa				;stelle Register wieder her
		ret					;und beende Prozedur
	.write:
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		shl bl,1			;Bit0(RTS-zu schreibender Status) auf Bit1 rotieren, damit das an gleicehr Position wie im MCR ist
		in al,dx			;das aktuelle MCR nach al zu holen um Bit1 zu verändern, aber die anderen nicht
		and al,11111101b	;Bit1 in al(RTS im MCR) löschen, damit dieses 0 ist, falls Bit1 in bl 0 ist(bei or wäre Bit1 nur 0, wenn es
		or al,bl			;... im MCR UND in bl 0 wäre) -> al mit bl or-verknüpfen, sodass Bit1 wieder gesetzt wird, wenn Bit1 in bl=1 ist
		out dx,al			;nun sende das aktualisierte MCR wieder and das Modem-Control-Registers des Com-Ports
		popa				;Register wiederherstellen
		ret					;Zu Aufrufsort zurückspringen

;***********************************************************************
;************************* dx: com-basis-Adresse **************************
;******************** .write->schreibe BL-Zustand auf DTR *******************
;********************** .read->lese DTR-Zustand nach ZF *********************
;***********************************************************************
dtr:
	.read
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		in al,dx			;und hole das MCR ab
		test al,00000001b	;teste ob im MCR das Bit0, der Zustand von DTR, gesetzt ist
		popa				;stelle Register wieder her
		ret					;und beende Prozedur
	.write:
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		in al,dx			;das aktuelle MCR nach al zu holen um Bit1 zu verändern, aber die anderen nicht
		and al,11111110b	;Bit0 in al(DTR im MCR) löschen, damit dieses 0 ist, falls Bit0 in bl 0 ist(bei or wäre Bit0 nur 0, wenn es
		or al,bl			;... im MCR UND in bl 0 wäre) -> al mit bl or-verknüpfen, sodass Bit1 wieder gesetzt wird, wenn Bit0 in bl=1 ist
		out dx,al			;nun sende das aktualisierte MCR wieder and das Modem-Control-Registers des Com-Ports
		popa				;Register wiederherstellen
		ret					;Zu Aufrufsort zurückspringen

;***********************************************************************
;************************* dx: com-basis-Adresse **************************
;******************** .write->schreibe BL-Zustand auf TXD *******************
;********************** .read->lese TXD-Zustand nach ZF *********************
;***********************************************************************
txd:
	.read
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		in al,dx			;und hole das LCR ab
		test al,01000000b	;teste ob im LCR das Bit6, der Zustand von TXD, gesetzt ist
		popa				;stelle Register wieder her
		ret					;und beende Prozedur
	.write:
		pusha				;Register sichern
		add dx,4			;zu der Basis-Port-adresse 4 addieren um in dx den Port des LCR's (LineControlRegister's) zu haben
		shl bl,6			;Bit0(TXD-zu schreibender Status) auf Bit6 rotieren, damit das an gleicher Position wie im LCR ist
		in al,dx			;das aktuelle LCR nach al zu holen um Bit1 zu verändern, aber die anderen nicht
		and al,11111110b	;Bit6 in al(TXD im LCR) löschen, damit dieses 0 ist, falls Bit1 in bl 0 ist(bei or wäre Bit1 nur 0, wenn es
		or al,bl			;... im MCR UND in bl 0 wäre) -> al mit bl or-verknüpfen, sodass Bit1 wieder gesetzt wird, wenn Bit1 in bl=1 ist
		out dx,al			;nun sende das aktualisierte MCR wieder and das Modem-Control-Registers des Com-Ports
		popa				;Register wiederherstellen
		ret					;Zu Aufrufsort zurückspringen

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

x_top_left	equ 28
y_top		equ 4
com_pic db 'Switch COM-Port : COM1',0
		db 0
		db '   Negate rts : <1>',0
		db '   Negate dtr : <2>',0
		db '   Negate txd : <3>',0
		db 0
		db 0
		db '          TXD       ',0
		db '           |        ',0
		db '     DTR-| | |-RXD  ',0
		db '   GND-| | | | |-DCD',0
		db '      ___________   ',0
		db '      \X X X X X/   ',0		;GND ist an Offset6 von upper, die anderen immer 2 weiter
		db '       \X X X X/    ',0		;RI ist an Offset7 von lower, die anderen immer 2 weiter
		db '        ',248,248,248,248,248,248,248,'     ',0	;248=°
		db '     RI-| | | |-DSR ',0
		db '      CTS-| |-RTS   ',0

;folgende Konstanten können wie eine eigenständige Variable(1Byte 'X' oder 'O') gehandelt werden, da sie den Offset
;an der entsprechenden Bildstelle entsprechen... -> mov[compic+193 (das, was im videospeicher liegt)] = mov[txd_var]
;NOTE: bei upper*2 kam bei NASM ein Fehler, deswegen upper+upper
;          (12*80*2), deswegen, weil die Zeile mit den 'X X X X X' 12 Zeilen vom Bild-oberen Anfang entfernt ist
gnd_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(12*80*2)+(6*2)
dtr_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(12*80*2)+(8*2)
txd_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(12*80*2)+(10*2)
rxd_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(12*80*2)+(12*2)
dcd_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(12*80*2)+(14*2)

ri_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(13*80*2)+(7*2)
cts_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(13*80*2)+(9*2)
rts_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(13*80*2)+(11*2)
dsr_var equ 0b800h+(y_top*80*2)+(x_top_left*2)+(13*80*2)+(13*2)
