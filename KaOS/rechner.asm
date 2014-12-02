;Ver. 0.4.16
;######################################################################
;**********************************************************************
;**********                 RECHNER                          **********
;**********************************************************************
;######################################################################

stellen equ 1024
basis equ 10
org 100h

taschenrechner_loop_readln:
	xor eax,eax
	lea di,[big1]
	call biginit
	lea di,[big2]
	call biginit

	call break
	lea di,[big1]
	call bigread
	call break
	
	cmp al,178
	jne taschenrechner_not_bigpot
		
	taschenrechner_not_bigpot:
	
	; push eax
	; call bigtest
	; movzx eax,al
	; mov ebx,10
	; call zahlausgabe
	; call break
	; pop eax
	
	cmp al,47
	jne taschenrechner_not_bigdiv
		lea si,[big1]
		lea di,[big3]
		call readzahl
		call break
		mov ebx,eax
		call bigdiv
		lea si,[big3]
		call bigprint
		xor ax,ax
		int 16h
		call break
		cmp al,27
		jne taschenrechner_loop_readln
	taschenrechner_not_bigdiv:
	
	push ax
	lea di,[big2]
	call bigread
	call break
	lea si,[big1]
	lea dx,[big2]
	lea di,[big3]
	pop ax

	cmp al,45
	jne taschenrechner_not_bigsub
		call bigsub
		jmp taschenrechner_test_end
	taschenrechner_not_bigsub:
	cmp al,43
	jne taschenrechner_not_bigadd
		call bigadd
		jmp taschenrechner_test_end
	taschenrechner_not_bigadd:
	cmp al,42
	jne taschenrechner_test_end
		call bigmul
		jmp taschenrechner_test_end
	taschenrechner_test_end:

	lea si,[big3]
	call bigprint

	xor ax,ax
	int 16h
	call break
	
	cmp al,27
	jne taschenrechner_loop_readln

retf

showflags:
	pushfd
	pop eax
	mov ebx,2
	call break
	call zahlausgabe
	call break
	xor ax,ax
	int 16h
	push eax
	popfd
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
	mov ecx,stellen				;auf ecx, die Anzahl der Zeichern die der String enthält speichern
	rep stosb					;string mit 0 befüllen
	
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
	cld							;bei lodsb und stosb u.a. esi bzw. edi erhöhen
	mov byte[komma],0
	mov dword[laenge],stellen
	or ebx,ebx					;prüfen ob Teiler 0 ist und das ergebnis so unendlich wäre
	jz bigdiv_end

	xor ecx,ecx
	dec ecx
	dividend_length:			;da zahl am ende des String gespeichert ist zur 1.Zahl vom anfang an gehen
		cmp ecx,stellen			;wenn keine Ziffer außer 0 gefunden wurde
		je bigdiv_end
		inc ecx
		lodsb					;Ziffer laden
		or al,al				;Ziffer prüfen
		jz dividend_length
	dec esi
	
	xor edx,edx
	mov ebp,10
	dividiere:
		mov eax,edx				;vorigen rest mit der Zahlenbasis(10) multiplizieren
		cmp byte[komma],0
		jnz ziffer_laden_komma
			cmp ecx,stellen			;prüfen ob schon ende des strings erreicht wurde
			je test_komma		;	...wenn ja beende
			inc ecx
			mul ebp
			
			mov edx,eax
			xor eax,eax
			lodsb					;Ziffer in al laden
			add eax,edx				;voriger rest(am anfang 0) + nächste Ziffer
			jmp ziffer_laden_end
		ziffer_laden_komma:
			mul ebp
			or eax,eax
			jz dividiere_end
		ziffer_laden_end:
		xor edx,edx
		div ebx						;rest:edx	ergebnis:eax
		stosb						;ganzteil speichern (mit modulo(rest) weiterarbeiten)
		inc_test_laenge:
			dec dword[laenge]
			jz dividiere_end
			jmp dividiere			;gehe zu anfang
		
		test_komma:
		or edx,edx					;	...ansonsten prüfe ob der modulo 0 ist
		jz dividiere_end			;	...wenn ja gehe zu anfang und beende somit die divisionsschleife
			mov al,-2				;	...wenn ein rest vorhanden ist speichere das Zeichen für den Punkt (chr(.)-48)
			stosb					;hier wird das zeichen dem quotienten-string hinzugefügt
			mov byte[komma],1		;merke, dass jetzt nachkomma stellen berechnet werden
			jmp inc_test_laenge		;erhöhe stellen-anzahl und mache mit nachkommateil weiter
	dividiere_end:
	mov ecx,stellen			;Anzahl d. Stellen auf ecx
	sub ecx,[laenge]
	sub edi,ecx					;auf stringanfang vom erg-string setzen
	call bigshr
bigdiv_end:
	popfd						;Register wiederherstellen
	popad						;	~
	ret
komma db 0
laenge dd 0
bigdiv_basis dd 10

;***********************************************************************
;********* bigrad ds:esi=potenz ebx=exponent ds:edi=Quotient **********
;***********************************************************************
bigrad:
	pushad						;Register speichern
	pushfd
	cld							;bei lodsb und stosb u.a. esi bzw. edi erhöhen
	mov byte[rad_komma],0
	mov dword[rad_laenge],stellen
	or ebx,ebx					;prüfen ob Teiler 0 ist und das ergebnis so unendlich wäre
	jz radiziere_end

	xor ecx,ecx
	dec ecx
	radiziere_length:			;da zahl am ende des String gespeichert ist zur 1.Zahl vom anfang an gehen
		cmp ecx,stellen			;wenn keine Ziffer außer 0 gefunden wurde
		je bigrad_end
		inc ecx
		lodsb					;Ziffer laden
		or al,al				;Ziffer prüfen
		jz radiziere_length
	dec esi
	
	xor edx,edx
	mov ebp,10
	radiziere:
		mov eax,edx				;vorigen rest mit der Zahlenbasis(10) multiplizieren
		cmp byte[rad_komma],0
		jnz ziffer_rad_komma
			cmp ecx,stellen			;prüfen ob schon ende des strings erreicht wurde
			je test_rad_komma		;	...wenn ja beende
			inc ecx
			mul ebp
			
			mov edx,eax
			xor eax,eax
			lodsb					;Ziffer in al laden
			add eax,edx				;voriger rest(am anfang 0) + nächste Ziffer
			jmp ziffer_laden_end
		ziffer_rad_komma:
			mul ebp
			or eax,eax
			jz dividiere_end
		ziffer_rad_end:
		xor edx,edx
		div ebx						;rest:edx	ergebnis:eax
		stosb						;ganzteil speichern (mit modulo(rest) weiterarbeiten)
		inc_rad_laenge:
			dec dword[laenge]
			jz radiziere_end
			jmp radiziere			;gehe zu anfang
		
		test_rad_komma:
		or edx,edx					;	...ansonsten prüfe ob der modulo 0 ist
		jz radiziere_end			;	...wenn ja gehe zu anfang und beende somit die divisionsschleife
			mov al,-2				;	...wenn ein rest vorhanden ist speichere das Zeichen für den Punkt (chr(.)-48)
			stosb					;hier wird das zeichen dem quotienten-string hinzugefügt
			mov byte[rad_komma],1		;merke, dass jetzt nachkomma stellen berechnet werden
			jmp inc_rad_laenge		;erhöhe stellen-anzahl und mache mit nachkommateil weiter
	radiziere_end:
	mov ecx,stellen			;Anzahl d. Stellen auf ecx
	sub ecx,[laenge]
	sub edi,ecx					;auf stringanfang vom erg-string setzen
	call bigshr
bigrad_end:
	popfd						;Register wiederherstellen
	popad						;	~
	ret
rad_komma db 0
rad_laenge db 0

;***********************************************************************
;******** bigpot ds:esi=Basis ds:edx=Exponent ds:edi=Potenz ********
;***********************************************************************
bigpot:
	pushad
	pushfd
	std							;bei lodsb und stosb u.a. esi bzw. edi decrementiert
	add edx,stellen-1			;gehe an letzte Position des Strings
	add esi,stellen-1			;		~
	add edi,stellen-1			;		~
	mov[pot_esi],esi			;esi temporär speichern
	mov[pot_edi],edi			;edi temporär speichern
	
	xor eax,eax					;eax löschen, um mp mit 0 zu füllen (initialisieren)
	mov ecx,stellen				;auf ecx, die Anzahl der Zeichern die der String enthält speichern
	rep stosb					;string mit 0 befüllen
	
	mov ebp,stellen				;anzahl der ziffern der mps auf ebp kopieren
	bigpot_loop:
		mov bl,[ds:edx]			;Ziffer aus 1.Faktor laden in bl zwischenspeichern
		dec edx					;eine Ziffer im 1.String weitergehen
		mov edi,[pot_edi]		;edi temporär laden
		mov esi,[pot_esi]		;esi temporär laden
		mov ecx,stellen			;auf ecx die Ziffernanzahl der Strings kopieren
		bigpot_loop_fak2:
			lodsb						;Byte aus 2.Faktor holen
			mul bl						;Byte aus 1.Faktor mit Byte aus 2.Faktor multiplizieren
			add al,[ds:edi]				;al derzeitige Ziffer in Ergebnis String dazuaddieren (wegen Übertrag)
			test_uebertrag_pot:
				cmp al,basis			;teste ob ergebnis der ziffern noch in den ziffern des gewählten zahlensystems vorhanden ist
				jb bigpot_end			;	...wenn ja speichere Zeichen
					sub al,10				;	...wenn nicht dann ziehe vom ergeebnis 10 ab
					inc byte[ds:edi-1]		;erhöhe das Ergebnis
					jmp test_uebertrag_pot	;mache mit dem multiplizieren der strings weiter
				bigpot_end:
					stosb					;	...und im erg-string speichern
			loop bigpot_loop_fak2
		dec dword[pot_edi]
		dec ebp
		jnz bigpot_loop
	popfd
	popad
	ret
pot_esi dd 0
pot_edi dd 0

;***********************************************************************
;*************** bigtest ds:esi=String al=0 wenn String 0 ist****************
;***********************************************************************
bigtest:
	push ecx
	pushfd
	cld
	mov ecx,stellen
	xor al,al
	repe scasb
	jz string_null
		mov al,1
	string_null:
	popfd
	pop ecx
	ret

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
	mov edx,ecx					;Anz. d. Ziffern retten
	rep movsb					;Ergebnis an hinteren ende des mp(string) verschieben

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
	
;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
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

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	pusha
	pushf
	cld
	mov ah,0Eh		; Funktion 0x0E	
write2:	
	lodsb			; Byte laden
	or al,al
	jz writed			; 0-Byte? -> Ende!
	int 10h			; schreiben
	jmp write2		; N?stes Byte
writed:
	popf
	popa
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

big1 times stellen db 0
big2 times stellen db 0
big3 times stellen db 0