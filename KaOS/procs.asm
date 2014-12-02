;cmp_str_cs DS:SI String1 ES:DI String2 -> wenn gleich(casesensitive), al=1
;cmp_str DS:SI=String1 ES:DI=String2 wenn gleich, al=1 ("a"="A")
;upcase_string ändert alle kleinen Buchstaben in große um string auf es:di
;readln es:di=string ecx:laenge
;readln es:di=string ecx:laenge ds:esi allowed signs... length of string defined in first word of string -> exp.: allowed_chars db 4,0,"0123" -> will only allow digits 0-3
;Zahlausgabe ax:zahl bx:basis Ausgabe:zahl
;writeln ds:si String bx:farbe Ausgabe:String
;write ds:si String bx:farbe Ausgabe:String
;Line ax x1:bx y1 cx x2:dx y2 ds:farbe
;fill bx:farbe
;set_int_vector ds:Segment si:offset interrupt al:interrupt
;get_int_vector ds:Segment si:offset interrupt al:interrupt
;Bitset bx:Zahl cx:welches(Bit) dann al:Bit
;deconvert ax:Zahl bl:Basis es:di string
;convert_to_dec ax:Zahl bl:Basis ds:si string
;Break Erg:Zeilenumbruch
;Find DS:SI gesuchtes_Wort ES:DI String
;gotoxy x:dl y:dh
;wherexy x:dl y:dh
;clrscr

;Prim eax:zahl al:0 keine Prim; 1 Prim
;Primlast eax:zahl eax:letzte Primzahl
;Primnext eax:zahl eax:nächste Primzahl
;potenz ax:basis cx:exponent eax:erg
;fakultaet cx:zahl eax:erg

;init_ps2 dx:basisadresse des com-port
;sende bx : Byte dx : COM-adresse
;dcd al : dcd dx : COM-adresse
;ri al : ri dx : COM-adresse
;dsr al : dsr dx : COM-adresse
;cts al : cts dx : COM-adresse
;rxd al : rxd dx : COM-adresse
;rts bl : rts; dx : COM-adresse
;dtr bl : dtr; dx : COM-adresse
;txd bl : txd; dx : COM-adresse

;install_timer
;int_handler
;Clean_up
;delay eax:Wartezeit im hs
;*********************mus ich noch schreiben****************************
;procedure dauer(h,m,s,hs,h1,m1,s1,hs1:word;var e1,e2,e3,e4:word);
;function all_in_hsec(h,m,s,hs:word):longint;
;*********************mus ich noch schreiben****************************

;show_intvectors: zeigt Inerrupt Vectoren auf Bildshirm an
;debug
;zahl_length eax:zahl cx:laenge
;getscreen es:di Speicher-Adresse für Bildschirm (Bytes nach Modi)
;setscreen ds:si Speicher-Adresse für Bildschirm (Bytes nach Modi)
;get_video_ram eax:segment im ram (0=Fehler)

; biginit es:di=anfangsadresse string eax=füllt string
; bigmul ds:esi=1.Faktor ds:edx=2.Faktor ds:edi=Produkt
; bigadd ds:esi=1.Summand ds:edx=2.Summand ds:edi=Summe
; bigsub ds:esi=Minuend ds:edx=Subtrahend ds:edi=Differenz
; bigdiv ds:esi=Dividend ebx=Divisor ds:edi=Quotient
; bigread es:di=String al=letztes Zeichen
; bigshr es:di=String ecx:shr x
; bigprint ds:si=String
; readzahl eax:eingegeben Zahl

stellen equ 1024
basis equ 10

;***********************************************************************
;**** cmp_str_cs DS:SI String1 ES:DI String2 -> wenn gleich(casesensitive), al=1 ****
;***********************************************************************
cmp_str_cs:
	pusha					;Register sichern
	.such:
		cmpsb				;vergleiche Bytes/Zeichen der beiden Strings
			jne .endef		;wenn sie nicht gleich sind gehe zu endef
		cmp byte[es:di-1],0	;ansonsten teste, ob beide(ds:si muss ja gleich es:di sein) 0/zuende sind
		jne .such			;wenn nicht, gehe weiter im String
	popa					;stelle Register wieder her
	mov al,1				;schiebe auf al 1, als Zeichen, das Strings gleich sind
	ret						;und springe zum Aufrufsort zurück
.endef:
	popa					;ansonste leite endef ein und stelle Register wieder her
	xor al,al				;belege al jedoch mit 0 als Zeichen für ungleich
	ret						;und Springe zu Aufrufsort zurück

;**************************************************************************
;****** cmp_str DS:SI=String1 ES:DI=String2 wenn gleich, al=1 ("a"="A") **********
;**************************************************************************
cmp_str:
	pusha					;Register sichern
	.such:
		xor ax,ax			;ax löschen, damit man bei setxx auch sieht, ob da was gesetzt wurde
		cmp byte[ds:si],0	;teste, ob ende von String1  erreicht ist
			setz al			;wenn dem so ist, setze al von 0 auf 1
		cmp byte[es:di],0	;ansonsten, teste, ob auch String2 zu ende ist
			setz ah			;wenn dem so ist, setze ah von 0 auf 1
		test ah,al			;testet, ob die beiden gleich gesetzt sind (1+1=1; 1+0=0; 0+0=0)
			jnz .endew		;wenn beide 1 sind, springe zu endew
		cmpsb				;vergleiche Bytes/Zeichen der beiden Strings
			je .such		;wenn sie gleich sind gehe im String weiter
		
		dec si					;wenn sie nicht übereinstimmen, gehe in String1 ein zurück
		lodsb					;um von diesem das letzte Zeichen nach al zu holen
		cmp al,64				;teste ob es unterhalb des Bereiches der großbuchstaben ist
		jna .no_high_alpha		;wenn ja, gehe zum testen nach kleinen Buchstaben
			cmp al,91			;ansonsten, teste ob es an der oberen Grenze der Großbuchstaben ist
			jnb .no_high_alpha	;wenn es nicht im Bereich von 64-97 ist, teste obs ein Kleinbuchstabe ist
				add al,32		;ansonsten isses ein Großbuchstabe und er wird in ein kleinen umgewandelt
				jmp .no_letter	;und dann nocheinmal abgeglichen, sodass das AntiCaseSensitive entsteht
		.no_high_alpha:
		cmp al,96				;teste ob es unterhalb des Bereiches der Kleinbuchstaben ist
		jna .no_letter			;wenn nicht, ist es kein Buchstabe und mache normal weiter
			cmp al,123			;teste ob al im Bereich der kleinen Buchstaben ist
			jnb .no_letter		;wenn nicht, ist es kein Buchstabe und mache normal weiter
				sub al,32		;ansonsten subtrahiere 32 um den entsprechenden Großbuchstaben zu erhalten
		.no_letter:
		cmp al,[es:di-1]	;und vergleiche das nocheinmal, sodass nun bei "a" und "A" kein Unterschied ist
		je .such			;wenn sie gleich sind, gehe im String weiter
.endef:
	popa					;ansonste leite endef ein und stelle Register wieder her
	mov al,0				;belge al jedoch mit 0 als Zeichen für ungleich
	ret						;und Springe zu Aufrufsort zurück
.endew:
	popa					;stelle Register wieder her
	mov al,1				;schiebe auf al 1, als Zeichen, das Strings gleich sind
	ret						;und springe zum Aufrufsort zurück

;********************************************************************************************
;******************* upcase_string ändert alle kleinen Buchstaben in große um string auf es:di ****************
;********************************************************************************************
upcase_string:
	pusha				;Register sicher
	push ds				;DS sichern
	mov si,di			;auf si di kopieren
	push es				;es pushen
	pop ds				;und auf ds kopieren, sodass nun ds:si und es:di auf den selben string zeigen
	upcase_letters:
		lodsb							;Zeichen von String laden
		or al,al						;prüfen, ob das Zeichen 0 ist
			jz upcase_letters_ende		;wenn ja, beende, da 0 das ende des strings signiert
		cmp al,96						;teste al mit 96 (97=a)
		jna upcase_letters_weiter		;wenn es nicht im Bereich der LowerAlpha ist, überspringe dies
			cmp al,123					;teste al mit 123 (122=z)
			jnb upcase_letters_weiter	;ist das Zeichen au0erhalb des LowerAlpha, mach weiter
				sub al,32				;ist das Zeichen nun zwischen 96 und 123, dann ziehe 32 ab um es groß zu machen
		upcase_letters_weiter:
		stosb							;speichere das evtl. modifzierte Zeichen an der Stelle, von wo es geladen wurde
		jmp upcase_letters				;und wiederhole die Schleife
	upcase_letters_ende:
	stosb				;da hier nur das Zeichen 0 hinkommt, wird dieses als EOS im String gespeichert
	pop ds				;stelle DS wieder her
	popa				;stelle Register wieder her
	ret					;springe zum Aufrufsort zurück

;***********************************************************************
;************** readln es:di=string ecx:laenge **************
;***********************************************************************
readln:
	pushad						;Register sichern
	mov ebp, ecx				;zum Abgleich ecx auf ebp zwischensichern
	readln_loop:
		xor ax,ax				;ax löschen, damit bei int16h funtion 0h gestartet wird
		int 16h					;int 16h aufrufen -> auf Tastendruck warten :) -> das ist die zentral funktion von readln
		or al,al				;al auf asciizeichen prüfen
			jz readln_loop		;wenn al=0, dann warte auf nächstes zeichen
		cmp al,8				;al auf Backspace prüfen
		jne nicht_zurueck		;wenn es keiner ist, überspringe die Backspace-Prozedure
			cmp ebp,ecx			;wenn cx jedoch immer noch gleich dem eingabewert ist, also wenn kein zeichen mehr zum backspacen da ist
				je readln_loop	;dann warte auf das nächste zeichen
			inc ecx				;inkrementiere cx, da ja in zeichen gelöscht wird und umso weniger zeichen im string sind umso höher muss cx an der eingabe sein
			
			mov ah,0eh			;verschiebe auf ah 0eh - telerype output
			mov bx,2			;auf bx die farbe
			int 10h				;und gib den backspace aus -> bios geht automatisch ein schritt zurück
			xor al,al			;löscht das letzte zeichen jedoch nicht, weswegen auf al 0 kommt
			int 10h				;und das dann auf dem platz des letzten zeichens ausgegeben wird
			mov al,08h			;auf al Backspace schieben
			int 10h				;um es auszugeben und somit an stelle des gelöschten zeichens zu gelangen
			
			dec edi				;den pointer für stosx auf das letzte zeichen stellen(das zeichen wird entweder überschrieben oder als End of string mit 0 gelöscht)
				jmp readln_loop	;warte auf nächstes zeichen
		nicht_zurueck:
		cmp al,13				;testen, ob das zeichen enter ist
			je readln_end		;wenn Eingabe gleich Return ist, beende die readln-prozedure
		or cx,cx				;vergleiche, ob der nutzer die per cx bestimmte anzahl vonzeichen schon eingegeben hat
			jz readln_loop		;wenn ja, dann nehme das zeichen nicht an und warte auf nächstes zeichen, was irgendwann backspace oder enter sein sollte
		
		stosb					;wenn das zeichen nicht irgendwie vorher abgefangen wurden ist und hier gelandet ist, speichere es im Input-Stream-String
		mov ah,0eh				;auf ah 0eh für teletype-output
		mov bx,2				;auf bx farbe
		int 10h					;und nun gib das zeichen aus, was alle test bestanden hat
	dec ecx						;erniedrige ecx, da ja nun ein zeichen mehr in input-stream ist
	jmp readln_loop				;und warte auf nächstes zeichen
readln_end:
	xor al,al					;lösche ax
	stosb						;und setze in string zeichen für stringende
popad							;stelle alle register wieder her
ret								;und springe zurück zum aufrufsort

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
;************* writeln ds:si String bx:farbe Ausgabe:String ************
;***********************************************************************
writeln:
	pusha			;Word-Register speichern
	pushf			;Flagregister speichern
	cld				;direction-flag löschen
	mov ah,0Eh		;Funktion 0x0E	
writeln2:	
	lodsb			;Byte laden
	or al,al		;Byte prüfen, sodass zeroflag gesetzt wird oder nicht
	jz writelnd		;0-Byte? -> Ende!
	int 10h			;schreiben
	jmp writeln2	;Nächstes Byte
writelnd:
	mov al,13		;Char-zeichen 13 auf al
	int 10h			;und ausgegeben
	mov al,10		;char-zeichen 10 auf al
	int 10h			;und ausgegeben -> 13 und 10 = break
	popf			;flagregister wiederherstellen
	popa			;Word-Register wiederherstellen
	ret				;zum Aufrufsort zurückspringen

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

;***********************************************************************
;**************** Line ax x1:bx y1 cx x2:dx y2 ds:farbe ****************
;***********************************************************************
Line:
	 jmp linestart
	 X dw 0
	 Y dw 0
	 X2 dw 0
	 Y2 dw 0
	 FARBE dw 0
	 BXY dw 0
	 BYX dw 0
	 PX dw 0
	 PY dw 0
	 
	 linestart:
	 PUSHA
	 MOV BX,[X]
	 CMP [X2],BX
	 JG GROESER
		 SUB BX,[X2]
		 MOV [BXY],BX
		 JMP WEITER
	 GROESER:
		 MOV CX,[X2]
		 MOV [BXY],CX
		 SUB [BXY],BX
	 WEITER:
	 
	 MOV BX,[Y]
	 CMP [Y2],BX
	 JG BIGGER
		 SUB BX,[Y2]
		 MOV [BYX],BX
		 JMP WEITER2
	 BIGGER:
		 MOV CX,[Y2]
		 MOV [BXY],CX
		 SUB [BXY],BX
	 WEITER2:
	 
	 XOR CX,CX
	 XOR DX,DX
	 MOV BX,[BYX]
	 CMP [BXY],BX
	 JG BIGGERXY
		 CMP word [BXY],0
		 JE ENDE
		 CMP BX,0
		 JE ENDE
		 
		 ;PX:=bxy/byx
		 MOV AX,[BXY]
		 DIV word [BYX]
		 XOR AH,AH
		 MOV [PX],AX
		 ;PY:=byx/bxy
		 XOR DX,DX
		 MOV AX,[BYX]
		 DIV word [BXY]
		 XOR AH,AH
		 MOV [PY],AX
		 JMP ENDE
	 
	 BIGGERXY:
		 ;PX:=byx/bxy
		 MOV AX,[BYX]
		 DIV word [BXY]
		 XOR AH,AH
		 MOV [PX],AX
		 ;PY:=bxy/byx
		 XOR DX,DX
		 MOV AX,[BXY]
		 DIV word [BYX]
		 XOR AH,AH
		 MOV [PY],AX
		 JMP ENDE
	 ENDE:
	 
	 MOV AH,0Ch
	 XOR BX,BX
	 MOV DX,[Y]
	 MOV CX,[X]
	 
	 SCHLEIFE:
		 ADD DX,[PY]
		 INC CX
		 INT 10h
		 MOV AX,[X]
		 CMP AX,[X2]
		 JNE SCHLEIFE
	 
	 POPA
	 Ret					;Sprung an Aufrufstelle

;***********************************************************************
;**************************** fill bx:farbe ****************************
;***********************************************************************
  fill:
	 push ax
	 push cx
	 push es
	 push di
	 
	 MOV AX,0A000h
	 MOV ES,AX
	 MOV CX,16000
	 XOR DI,DI
	 MOV AX,BX
	 CLD
	 REP STOSD
	 
	 pop di
	 pop es
	 pop cx
	 pop ax
  ret

;***********************************************************************
;*******set_int_vector ds:Segment si:offset interrupt bp:interrupt******
;***********************************************************************
 set_int_vector:			;Label install
	pusha
	push es
	pushf
	
	xor ax,ax
	mov es,ax
	CLI					;Interrupts sperren
	mov [es:4*ebp+2],ds
	mov [es:4*ebp],si
	STI					;Interrupts zulassen
	
	popf
	pop es
	popa
	ret

;***********************************************************************
;*******get_int_vector ds:Segment si:offset interrupt bp:interrupt******
;***********************************************************************
 get_int_vector:			;Label install
	 pusha
	 push es
	 pushf
	 
	 xor ax,ax
	 mov es,ax
	 mov ax,[4*ebp+2]
	 mov si,[4*ebp]
	 mov ds,ax
	 
	 popf
	 pop es
	 popa
	 ret

;***********************************************************************
;************** Bitset bx:Zahl cx:welches(Bit) dann al:Bit *************
;***********************************************************************
 Bitset:
	 xor ax,ax
	 bt bx,cx
	 setc al
	 
	 ret

;***********************************************************************
;*************** deconvert ax:Zahl bl:Basis es:di string ***************
;***********************************************************************
deconvert:
	pusha
	xor cx,cx
deconvert1:
	div bl
	push ax
	xor ah,ah
	inc cx
	or ax,ax
	jnz deconvert1

	deconvert2:
		pop ax
		xchg al,ah
		add al,48
		stosb
	loop deconvert2
	
	xor ax,ax
	stosb
	popa
ret

;***********************************************************************
;************* convert_to_dec ax:Zahl bl:Basis ds:si string ************
;***********************************************************************
 convert_to_dec:
	 push bx
	 push cx
	 push dx
	 pushf
	 
	 cld
	 xor cx,cx
	 schleife_to_dec:
		 lodsb
		 or al,al
		 jz ende_to_dec
		 
		 xor dx,dx
		 xchg ax,cx
		 mul bx
		 sub cl,48
		 xor ch,ch
		 add cx,ax
		 
		 jmp schleife_to_dec
 ende_to_dec:
	 mov ax,cx
	 
	 popf
	 pop dx
	 pop cx
	 pop bx	 
	 ret

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
;************** Find DS:SI gesuchtes_Wort ES:DI String *****************
;***********************************************************************
find:
	push bx
	push cx
	push dx
	pushf
	
	push si
	push di
	
	cld
	mov cx,0ffffh
	repnz scasb
	not cx
	mov dx,cx
	
	xchg si,di
	mov cx,0ffffh
	repnz scasb
	not cx
	xchg cx,dx
	
	pop di
	pop ax
	
	such:
		mov si,ax
		mov bx,cx
		repz cmpsb
		sub bx,cx
		
		cmp bx,dx
		je endew
		or cx,cx
		jz endef
		jmp such
		
	endew:
		mov ax,87
		pop dx
		pop cx
		pop bx
		ret
	endef:
		mov ax,70
		popf
		pop dx
		pop cx
		pop bx
		ret

;***********************************************************************
;************************* gotoxy x:dl y:dh ****************************
;***********************************************************************	
gotoxy:
	push ax
	push bx
	pushf
	mov ah,02h
	xor bh,bh
	int 10h
	popf
	pop bx
	pop ax
ret

;***********************************************************************
;************************ wherexy x:dl y:dh ****************************
;***********************************************************************	
wherexy:
	push ax
	push bx
	pushf
	mov ah,03h
	xor bh,bh
	int 10h
	popf
	pop bx
	pop ax
ret

;***********************************************************************
;*********************** clrscr löscht Bildschirm **********************
;***********************************************************************
clrscr:
	pushad
	push es
	mov ax,0B800h
	mov es,ax
	xor di,di
	mov cx,1000
	mov eax,07000700h
	rep stosd
	mov ah,02h
	xor bh,bh
	xor dx,dx
	int 10h
	pop es
	popad
	ret














;***********************************************************************
;**************** Prim eax:zahl al:0 keine Prim; 1 Prim ****************
;***********************************************************************
prim:
	 PUSHA
	 MOV EBX,EAX
	 MOV ECX,2
	 FORSCHLEIFE:
		 XOR EDX,EDX
		 MOV EAX,EBX
		 DIV ECX
		 
		 OR EDX,EDX
		 JZ NOTPRIM
		 
		 INC ECX
		 CMP ECX,EBX
	 JB FORSCHLEIFE
	 POPA
	 MOV EAX,1
	 RET
 NOTPRIM:
	 POPA
	 XOR EAX,EAX
 RET

;***********************************************************************
;*************** Primlast eax:zahl eax:letzte Primzahl *****************
;***********************************************************************
primlast:
	 PUSH EBX
	 PUSH ECX
	 PUSH EDX

	 MOV EBX,EAX
	 BEFORE_FORSCHLEIFEL:
	 MOV ECX,2
	 FORSCHLEIFEL:
		 XOR EDX,EDX
		 MOV EAX,EBX
		 DIV ECX
		 
		 OR EDX,EDX
		 JZ NOTPRIML
		 
		 INC ECX
		 CMP ECX,EBX
	 JB FORSCHLEIFEL
	 MOV EAX,EBX
	 POP EDX
	 POP ECX
	 POP EBX
	 RET
 NOTPRIML:
	 DEC EBX
	 JMP BEFORE_FORSCHLEIFEL
 
;***********************************************************************
;*************** Primnext eax:zahl eax:nächste Primzahl ****************
;***********************************************************************
primnext:
	 PUSH EBX
	 PUSH ECX
	 PUSH EDX

	 MOV EBX,EAX
	 BEFORE_FORSCHLEIFEN:
	 MOV ECX,2
	 FORSCHLEIFEN:
		 XOR EDX,EDX
		 MOV EAX,EBX
		 DIV ECX
		 
		 OR EDX,EDX
		 JZ NOTPRIMN
		 
		 INC ECX
		 CMP ECX,EBX
	 JB FORSCHLEIFEN
	 MOV EAX,EBX
	 POP EDX
	 POP ECX
	 POP EBX
	 RET
 NOTPRIMN:
	 INC EBX
	 JMP BEFORE_FORSCHLEIFEN

;***********************************************************************
;***************** potenz ax:basis cx:exponent eax:erg******************
;***********************************************************************
 potenz:
	 push bx
	 push cx
	 mov bx,ax
	 dec cx
 potenz2:
	 mul bx
	 loop potenz2
	 pop cx
	 pop bx
 ret

;***********************************************************************
;********************** fakultaet cx:zahl eax:erg ***********************
;***********************************************************************

fakultaet:
	mov eax,1
	fakschleife:
		mul ecx
		loop fakschleife
	ret
















;***********************************************************************
;**************** init_ps2 dx:basisadresse des com-port ****************
;***********************************************************************
init_ps2:
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
 mov al,9Ch					;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al					;Byte gibt den low-teil des teilers an, der benötigt wird um die Frequenz einzustellen
 mov al,2Eh					;auf al Byte verschieben um es an einen Port zu senden
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
;************** delay eax:Wartezeit im hs **************
;***********************************************************************
delay:
	push es					;speichere es
	push word 2000h			;pushe 2000h (anfangsadresse von TimerINT)
	pop es					;und beschreibe es damit
	add eax,[es:100h+3]		;addiere zu der eingestellten Wartezeit die aktuelle Zeit
	.loop:
		cmp eax,[es:100h+3]	;und vergleiche fortweg
		jae .loop			;ob die in eax definierte Zeit um ist, wenn nicht, wiederhole
	pop es					;ansonsten stelle es wieder her
	ret						;und springe zum Aufrufsort zurück











;***********************************************************************
;****** show_intvectors: zeigt Inerrupt Vectoren auf Bildshirm an ******
;***********************************************************************
show_intvectors:
	pushad
	push ds
	pushf
	xor ax,ax
	mov ds,ax
	mov si,ax
	mov cx,256
	sti
	intvecs:
		mov ax,256
		sub ax,cx
		mov bx,16
		call zahlausgabe
		mov ah,0eh
		mov bx,2
		mov al,32
		int 10h
	
		lodsw
		mov bx,16
		call zahlausgabe
		mov ah,0eh
		mov bx,2
		mov al,58
		int 10h
		lodsw
		mov bx,16
		call zahlausgabe
		call break

		mov ax,cx
		mov bl,16
		div bl
		or ah,ah
		jnz weiter
			xor ax,ax
			int 16h
		weiter:
	loop intvecs
	popf
	pop ds
	popad
	ret

;***********************************************************************
;*********************** debug zeigt Register **************************
;***********************************************************************
debug:
;Register speichern
	push esp
	push ebp
	push esi
	push edi
	push edx
	push ecx
	push ebx
	push eax
	pushfd
	push dword[es:di]
	push dword[ds:si]
	push ss
	push gs
	push fs
	push es
	push ds
	push cs
;Register speichern
;Ausgabe der Ergebnisse
	cld
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

	cld
	mov ax,cs
	mov es,ax
	mov ds,ax
	lea di,[regs]
	mov cx,6
	stosb_loop:
		pop ax
		stosw
		loop stosb_loop
	mov cx,11
	stosd_loop:
		pop eax
		stosd
		loop stosd_loop

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

	lea si,[xy]
	lodsw
	mov dx,ax
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
	ret
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
debug3	db 'DS:SI:00000000 ES:DI:00000000 CS:IP:00000000 Flags: ----ODITSZ-A-P-C',0
regs	times 56 db 0
xy		dw 0
screen	times 640 db 0
;OFFSET	00	02	04	06	08	10	12		16		20	24	28	32	36	40	44	48	52
;BITS	16	16	16	16	16	16	32		32		32	32	32	32	32	32	32	32	32
;REG		cs	ds	es	fs	gs	ss	[ds:si]	[es:di]	Flags	eax	ebx	ecx	edx	edi	esi	ebp	esp

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
;** getscreen es:di Speicher-Adresse für Bildschirm (Bytes nach Modi)***
;***********************************************************************
getscreen:
	pusha
	cld
	call get_video_ram
	mov ds,ax
	xor si,si
	mov cx,1000
	rep movsd
	popa
	ret

;***********************************************************************
;** setscreen ds:si Speicher-Adresse für Bildschirm (Bytes nach Modi)***
;***********************************************************************
setscreen:
	pusha
	cld
	call get_video_ram
	mov es,ax
	xor di,di
	mov cx,1000
	rep movsd
	popa
	ret

;***********************************************************************
;************* get_video_ram eax:segment im ram (0=Fehler) *************
;***********************************************************************
get_video_ram:
	pushf
	mov ah,0Fh
	int 10h
	cmp al,06
	jnb video_weiter_0
		mov ax,0B800h
		jmp video_ram_end
	video_weiter_0:
	cmp al,7
	jne video_weiter_1
		mov ax,0B000h
		jmp video_ram_end
	video_weiter_1:
	cmp al,9
	jnb video_weiter_2
		mov ax,0B800h
		jmp video_ram_end
	video_weiter_2:
	cmp al,10
	jnb fehler
		mov ax,0B000h
		jmp video_ram_end
	fehler:
		xor ax,ax
		jmp video_ram_end
	video_ram_end:
	popf
	ret



















;***********************************************************************
;******** biginit es:di=anfangsadresse string eax=füllt string**********
;***********************************************************************
biginit:
	pusha
	pushfd
	mov cx,stellen/4
	rep stosd
	popfd
	popa
	ret

;***********************************************************************
;******** bigmul ds:esi=1.Faktor ds:edx=2.Faktor ds:edi=Produkt ********
;***********************************************************************
bigmul:
	pushad
	pushfd
	std							;bei lodsb und stosb u.a. esi bzw. edi decrementiert
	add edx,stellen-1			;gehe an letzte Position des Strings
	add esi,stellen-1			;		~
	add edi,stellen-1			;		~
	mov[temp_esi],esi			;esi temporär speichern
	mov[temp_edi],edi			;edi temporär speichern
	
	xor eax,eax					;eax löschen, um mp mit 0 zu füllen (initialisieren)
	mov ecx,stellen/4			;auf ecx, die Anzahl der Zeichern die der String enthält speichern
	rep stosd					;string mit 0 befüllen
	
	mov ebp,stellen				;anzahl der ziffern der mps auf ebp kopieren
	bigmul_loop:
		mov bl,[ds:edx]			;Ziffer aus 1.Faktor laden in bl zwischenspeichern
		dec edx					;eine Ziffer im 1.String weitergehen
		mov edi,[temp_edi]		;edi temporär laden
		mov esi,[temp_esi]		;esi temporär laden
		mov ecx,stellen			;auf ecx die Ziffernanzahl der Strings kopieren
		bigmul_loop_fak2:
			lodsb						;Byte aus 2.Faktor holen
			mul bl						;Byte aus 1.Faktor mit Byte aus 2.Faktor multiplizieren
			add al,[ds:edi]				;al derzeitige Ziffer in Ergebnis String dazuaddieren (wegen Übertrag)
			test_uebertrag_mul:
				cmp al,basis			;teste ob ergebnis der ziffern noch in den ziffern des gewählten zahlensystems vorhanden ist
				jb bigmul_end			;	...wenn ja speichere Zeichen
					sub al,10				;	...wenn nicht dann ziehe vom ergeebnis 10 ab
					inc byte[ds:edi-1]		;erhöhe das Ergebnis
					jmp test_uebertrag_mul	;mache mit dem multiplizieren der strings weiter
				bigmul_end:
					stosb					;	...und im erg-string speichern
			loop bigmul_loop_fak2
			
		dec dword[temp_edi]
		dec ebp
		jnz bigmul_loop
	popfd
	popad
	ret
temp_esi dd 0
temp_edi dd 0
	
;***********************************************************************
;******** bigadd ds:esi=1.Summand ds:edx=2.Summand ds:edi=Summe ********
;***********************************************************************
bigadd:
	pushad
	pushfd
	std							;bei lodsb und stosb u.a. esi bzw. edi decrementiert
	add edx,stellen-1			;gehe an letzte Position des Strings
	add esi,stellen-1			;		~
	add edi,stellen-1			;		~

	mov ecx,stellen				;anzahl der ziffern der mps auf ecx kopieren
	xor al,al					;lösche byte, welches Übertrag enthält
	bigadd_loop:
		add al,[ds:esi]			;Ziffer aus 1.Summanden laden in bl zwischenspeichern
		add al,[ds:edx]			;beide Ziffern addieren
		dec edx					;gehe auf nachstes Byte im String
		dec esi					;gehe auf nachstes Byte im String
		cmp al,basis				;teste ob ergebnis der ziffern noch in den ziffern des gewählten zahlensystems vorhanden ist
		jb kein_uebertrag_bigadd	;	...wenn ja gehe zu...
			sub al,10				;	...wenn nicht dann ziehe vom ergeebnis 10 ab (kann ja nie mehr als 19 sein wenn beide ziffern 9 sind und bl,1)
			stosb					;	...und im erg-string speichern
			mov al,1				;setze bx, dass hier als übertragsbyte dient, auf 1 (beim näachstem mal addieren wird es mit addiert)
			jmp bigadd_end			;mache mit dem addieren der strings weiter
		kein_uebertrag_bigadd:
			stosb					;	...und im erg-string speichern
			xor al,al				;lösche bx, dass hier als übertragsbyte dient
		bigadd_end:
		loop bigadd_loop
	popfd
	popad
	ret

;***********************************************************************
;****** bigsub ds:esi=Minuend ds:edx=Subtrahend ds:edi=Differenz *******
;***********************************************************************
bigsub:
	pushad
	pushfd
	std							;bei lodsb und stosb u.a. esi bzw. edi decrementiert
	add edx,stellen-1			;gehe an letzte Position des Strings
	add esi,stellen-1			;		~
	add edi,stellen-1			;		~

	mov ecx,stellen				;anzahl der ziffern der mps auf ecx kopieren
	xor al,al					;;lösche byte, welches Übertrag enthält
	bigsub_loop:
		add al,[ds:esi]				;	...in bl zwischenspeichern
		sub al,[ds:edx]				;2.Ziffer von 1.Ziffer subtrahieren
		dec edx						;gehe auf nachstes Byte im String
		dec esi						;gehe auf nachstes Byte im String
		cmp al,basis				;teste ob ergebnis der ziffern noch in den ziffern des gewählten zahlensystems vorhanden ist
		jb kein_uebertrag_bigsub	;	...wenn ja gehe zu...
			add al,10				;	...wenn nicht dann addiere zum ergeebnis 10(kann nie weniger als -10 sein wenn 0-9 und bl=1)
			stosb					;	...und im erg-string speichern
			mov al,-1				;setze bx, dass hier als übertragsbyte dient, auf 1 (beim näachstem mal subtrahieren wird es mit subtrahiert)
			jmp bigsub_end			;mache mit dem addieren der strings weiter
		kein_uebertrag_bigsub:
			stosb					;	...und im erg-string speichern
			xor al,al				;lösche bx, dass hier als übertragsbyte dient
		bigsub_end:
		loop bigsub_loop			;decrementiere cx; wenn cx=0 ist dann beende
	popfd
	popad
	ret

;***********************************************************************
;********* bigdiv ds:esi=Dividend ebx=Divisor ds:edi=Quotient **********
;***********************************************************************
bigdiv:
	pushad						;Register speichern
	pushfd
	push ds						;	~
	push es						;	~
	or ebx,ebx					;prüfen ob Teiler 0 ist und das ergebnis so unendlich wäre
	jnz teiler0_jump_far 
		jmp bigdiv_end 			;	...wenn ja beende
	teiler0_jump_far:

	cld							;bei lodsb und stosb u.a. esi bzw. edi erhöhen
	mov byte[komma],0
	mov dword[laenge],0

	xor ecx,ecx
	dividend_length:			;da zahl am ende des String gespeichert ist zur 1.Zahl vom anfang an gehen
		cmp ecx,stellen			;wenn keine Ziffer außer 0 gefunden wurde
		jne ziffern0_jump_far 
			jmp bigdiv_end 		;	...dann beende
		ziffern0_jump_far:
		lodsb					;Ziffer laden
		inc ecx
		or al,al				;Ziffer prüfen
		jz dividend_length
	dec esi
	dec ecx
	
	xor edx,edx
	dividiere:
		mov eax,edx				;vorigen rest mit der Zahlenbasis(10) multiplizieren
		mov edx,basis			;		~
		mul edx					;		~

		mov dl,[komma]
		or dl,dl
		jnz ziffer_laden_komma
			cmp ecx,stellen			;prüfen ob schon ende des strings erreicht wurde
			je dividiere_end		;	...wenn ja beende
			inc ecx

			mov edx,eax
			xor eax,eax
			lodsb					;Ziffer in al laden
			add eax,edx				;voriger rest(am anfang 0) + nächste Ziffer
			jmp ziffer_laden_end
		ziffer_laden_komma:
			or eax,eax
			jz dividiere_end
		ziffer_laden_end:
		
		xor edx,edx
		div ebx					;rest:edx	ergebnis:eax
		stosb					;ganzteil speichern (mit modulo(rest) weiterarbeiten)

		inc_test_laenge:
			inc dword[laenge]
			mov eax,[laenge]
			cmp eax,stellen
			je dividiere_end

		mov al,[komma]						;prüfen ob schon nachkomma berechnet wird
		or al,al
		jnz dividiere						;	...wenn ja gehe zu anfang
			cmp ecx,stellen					;	...ansonsten prüfe ob schon ganzer dividend eingelsen wurde
			jne dividiere					;	...wenn ja gehe zu anfang
				or edx,edx					;	...ansonsten prüfe ob der modulo 0 ist
				jz dividiere				;	...wenn ja gehe zu anfang und beende somit die divisionsschleife
					mov al,-2				;	...wenn ein rest vorhanden ist speichere das Zeichen für den Punkt (chr(.)-48)
					stosb					;hier wird das zeichen dem quotienten-string hinzugefügt
					mov byte[komma],1		;merke, dass jetzt nachkomma stellen berechnet werden
					jmp inc_test_laenge		;erhöhe stellen-anzahl und mache mit nachkommateil weiter
		jmp dividiere						;gehe zu anfang
	dividiere_end:
	mov ecx,[laenge]			;Anzahl d. Stellen auf ecx
	sub edi,ecx					;auf stringanfang vom erg-string setzen
	call bigshr
	bigdiv_end:
	pop es						;Register wiederherstellen
	pop ds						;	~
	popfd
	popad						;	~
	ret
komma db 0
laenge dd 0

;***********************************************************************
;*************** bigread es:di=String al=letztes Zeichen****************
;***********************************************************************
bigread:
	pushfd
	pushad						;Register speichern
	cld
	xor ecx,ecx
	mov bx,2
	bigread_loop:
		xor ax,ax				;auf tastendruck warten
		int 16h					;ah:scan-code	al:ascii-code
		mov ah,0eh				;teletype output al:ascii-code
		int 10h					;zeichen wird jetzt ausgegeben
		
		sub al,48				;aus zeichen eine zahl machen (für bessere lesbarkeit bei den Funktionen)
		cmp al,basis			;prüfen ob zeichen eine zahl des angegeben Zahlensystems war
		jnb bigread_loop_end	;	...wenn nein gehe zu ende
		stosb					;zahl in string speichern
		inc ecx					;anz. d. Stellen erhöhen
		jmp bigread_loop
	bigread_loop_end:
	add al,48
	mov [bigread_zeichen],al
	sub edi,ecx					;auf stringanfang vom erg-string setzen
	call bigshr
	popad						;Register wiederherstellen
	mov al,[bigread_zeichen]
	popfd
	ret
bigread_zeichen db 0

;***********************************************************************
;******************* bigshr es:di=String ecx:shr x *********************
;***********************************************************************
bigshr:
	pushad						;Register wiederherstellen
	pushfd
	cld
	push edi
	mov esi,edi					;		~
	add edi,stellen				;stringende - anzahl der stellen
	sub edi,ecx					;		~
	push ecx
	rep movsb					;Ergebnis an hinteren ende des mp(string) verschieben
	pop edx						;Anz. d. Ziffern retten

	pop edi						;Anfangsadresse der Ergebnisstrings wiederherstellen
	mov ecx,stellen				;stellen-anz.d.ziffern ergibt di zu löschende menge
	sub ecx,edx					;		~
	xor al,al					;löschbyte auf 0 setzen
	rep stosb					;mit 0 stellen-ziffern Bytes löschen
	popfd
	popad						;Register wiederherstellen
	ret

;***********************************************************************
;*********************** bigprint ds:si=String *************************
;***********************************************************************
bigprint:
	pusha
	pushfd
	cld
	xor cx,cx
	bigprint_test:
		cmp cx,stellen
		je bigprint_test_end
		lodsb
		inc cx
		or al,al
		jz bigprint_test
	bigprint_test_end:
	dec cx
	dec si
	mov ah,0eh
	mov bx,2

	cmp al,-2
	jne bigprint_not_komma
		mov al,48
		int 10h
	bigprint_not_komma

	bigprint_loop:
		cmp cx,stellen
		je bigprint_loop_end
		lodsb
		inc cx
		add al,48
		int 10h
		jmp bigprint_loop
	bigprint_loop_end:
	popfd
	popa
	ret

;***********************************************************************
;******************** readzahl eax:eingegeben Zahl *********************
;***********************************************************************
readzahl:
	push bx
	push ecx
	push edx
	pushfd
	mov bx,2
	xor edx,edx
	mov ecx,basis
	readzahl_loop:
		xor eax,eax				;auf tastendruck warten
		int 16h					;ah:scan-code	al:ascii-code
		mov ah,0eh				;teletype output al:ascii-code
		int 10h					;zeichen wird jetzt ausgegeben
		
		xor ah,ah
		sub al,48				;aus zeichen eine zahl machen (für bessere lesbarkeit bei den Funktionen)
		cmp al,basis			;prüfen ob zeichen eine zahl des angegeben Zahlensystems war
		jnb readzahl_loop_end	;	...wenn nein gehe zu ende
		
		push eax
		mov eax,edx				;edx retten
		mul ecx					;mit basis multiplizieren
		mov edx,eax				;edx retten
		pop eax
		add edx,eax
		jmp readzahl_loop
	readzahl_loop_end:
	mov eax,edx
	popfd
	pop edx
	pop ecx
	pop bx
	ret
