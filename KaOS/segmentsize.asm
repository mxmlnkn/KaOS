;######################################################################
;**********************************************************************
;**********                 SEG_OFF                          **********
;**********************************************************************
;######################################################################
org 100h

cld								;lösche direction flag, damit die bei gesetztem d-flag nicht decrementiert wird
mov ax,3333h					;auf ax ein unbenutztes segment schieben
mov es,ax						;und auf es kopieren
inc ax							;dann zum näcshten segment springen
mov ds,ax						;und das auf ds sichern
xor edi,edi						;den zeiger di löschen, damit er ganz am anfang des Segments positioniert ist
write_to_next_seg:
 cmp edi,0FFFFFFFFh				;vergleichen, ob edi sein ultimum erreicht hat und somit am ende des segments positioniert ist
	je too_much					;ist dem so, würde in der nächsten runde di wieder 0 sein und eine endlose scleife enstehen, deswegen beende
 stosd							;das dword in eax nach es:di schreiben
 cmp dword[ds:0],3334h			;und vergleichen, ob damit schon in des nächste segment geschrieben wurde
	jne write_to_next_seg		;wenn nicht, mache weiter
		push cs						;ansonsten pushe cs
		pop ds						;um es in ds zu kopieren
		lea esi,[segment_size_equal];schiebe auf si den offset der Nachricht
		call write					;und gib diese aus
		mov eax,edi					;schiebe auf eax den zeiger edi
		sub eax,4					;und ziehe davon ein dword ab, da dieses ja schon im nächsten seg wäre
		mov ebx,10					;wähle als basis 10
		call zahlausgabe			;und gib edi aus, was ja dann auch die segmentgröße in Bytes entspricht
		lea esi,[bytes_msg]			;verschiebe auf esi den offset der Nachricht
		call write					;gib diese aus
		retf						;und kehre zum OS zurück
too_much:
	push cs						;würde das nächste segment per edi nicht erreicht werden können, pushe cs
	pop ds						;um es nach ds zu kopieren (für write)
	lea si,[too_much_msg]		;schiebe auf sie den offset der fehler-message
	call write					;gib diese aus
	retf						;und kehre zum Programm zurück

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

segment_size_equal db " is -> ",0
bytes_msg db " Bytes",0
too_much_msg db ' must be 4GB, but i dont belive it. "Maxinator 2008"',0