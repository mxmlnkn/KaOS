;######################################################################
;**********************************************************************
;**********                 SEG_OFF                          **********
;**********************************************************************
;######################################################################
org 100h

cld								;l�sche direction flag, damit die bei gesetztem d-flag nicht decrementiert wird
mov ax,3333h					;auf ax ein unbenutztes segment schieben
mov es,ax						;und auf es kopieren
inc ax							;dann zum n�cshten segment springen
mov ds,ax						;und das auf ds sichern
xor edi,edi						;den zeiger di l�schen, damit er ganz am anfang des Segments positioniert ist
write_to_next_seg:
 cmp edi,0FFFFFFFFh				;vergleichen, ob edi sein ultimum erreicht hat und somit am ende des segments positioniert ist
	je too_much					;ist dem so, w�rde in der n�chsten runde di wieder 0 sein und eine endlose scleife enstehen, deswegen beende
 stosd							;das dword in eax nach es:di schreiben
 cmp dword[ds:0],3334h			;und vergleichen, ob damit schon in des n�chste segment geschrieben wurde
	jne write_to_next_seg		;wenn nicht, mache weiter
		push cs						;ansonsten pushe cs
		pop ds						;um es in ds zu kopieren
		lea esi,[segment_size_equal];schiebe auf si den offset der Nachricht
		call write					;und gib diese aus
		mov eax,edi					;schiebe auf eax den zeiger edi
		sub eax,4					;und ziehe davon ein dword ab, da dieses ja schon im n�chsten seg w�re
		mov ebx,10					;w�hle als basis 10
		call zahlausgabe			;und gib edi aus, was ja dann auch die segmentgr��e in Bytes entspricht
		lea esi,[bytes_msg]			;verschiebe auf esi den offset der Nachricht
		call write					;gib diese aus
		retf						;und kehre zum OS zur�ck
too_much:
	push cs						;w�rde das n�chste segment per edi nicht erreicht werden k�nnen, pushe cs
	pop ds						;um es nach ds zu kopieren (f�r write)
	lea si,[too_much_msg]		;schiebe auf sie den offset der fehler-message
	call write					;gib diese aus
	retf						;und kehre zum Programm zur�ck

;***********************************************************************
;************** Zahlausgabe eax:zahl bx:basis Ausgabe:zahl **************
;***********************************************************************
zahlausgabe:
	pushad				;Register speichern
	pushf				;Flags speichern
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

;***********************************************************************
;************** write ds:si String bx:farbe Ausgabe:String *************
;***********************************************************************
write:
	pusha			;Word-Register speichern
	pushf			;Flagregister speichern
	cld				;direction-flag l�schen
	mov ah,0Eh		;Funktion 0x0E	
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

segment_size_equal db " is -> ",0
bytes_msg db " Bytes",0
too_much_msg db ' must be 4GB, but i dont belive it. "Maxinator 2008"',0