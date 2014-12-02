;######################################################################
;**********************************************************************
;**********                 KERNEL                           **********
;**********************************************************************
;######################################################################
;mit NASM compiliert
stellen equ 1999
basis equ 10
use16

kernel:
;installiere Keyboard und Timer
mov ax,cs
mov ds,ax
mov es,ax

movzx eax,dl
mov bx,10
call zahlausgabe
call break

mov ebp,08h
lea di,[timer_string]
mov ax,2000h
call comc

mov ebp,09h
lea di,[keyb_string]
mov ax,2100h
call comc

call installos
mov ah,03h
xor bh,bh
int 10h
mov eax,100
xor bp,bp	;call ladebalken
int 21h		;call ladebalken

mov ax,cs
mov ds,ax
mov es,ax

call break
lea si,[welcome]
call write
warte_auf_eingabe:
	call break
	push cs
	pop ds
	lea si,[pfad]
	call write
	mov ax,0e3eh
	mov bx,2
	int 10h

	lea di,[program_string]
	mov cx,64
	call readln
	lea si,[program_string]

	lea di,[befehl]
	erstes_wort2befehl:
		lodsb
		or al,al
			jz erstes_wort2befehl_ende
		cmp al,32
			je erstes_wort2befehl_ende
		stosb
		jmp erstes_wort2befehl
	erstes_wort2befehl_ende:
	xor al,al
	stosb
	lea di,[befehl]
	call upcase_string
;#################################################################
;########################Befehl testen############################
;#################################################################
	lea di,[befehl]
	
	lea si,[cls]
	call cmp_str_cs
	or al,al
	jz not_cls
		call clrscr
		jmp warte_auf_eingabe
	not_cls:

	lea si,[copy]
	call cmp_str_cs
	or al,al
	jz not_copy
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_copy:
	
	lea si,[credits]
	call cmp_str_cs
	or al,al
	jz not_credits
		lea si,[show_credits]
		call write
		jmp warte_auf_eingabe
	not_credits:
	
	lea si,[delete]
	call cmp_str_cs
	or al,al
	jz not_delete
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_delete:
	
	lea si,[dir]
	call cmp_str_cs
	or al,al
	jz not_dir
		xor cx,cx
		xor ax,ax
		mov al,[cs:anf_cluster_root]
		mov cl,[cs:anz_clusters_root]
		call show_dir
		jmp warte_auf_eingabe
	not_dir:
	
	lea si,[edit]
	call cmp_str_cs
	or al,al
	jz not_edit
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_edit:

	lea si,[help]
	call cmp_str_cs
	or al,al
	jz not_help
		call break
		lea si,[writeln_help]
		call write
		jmp warte_auf_eingabe
	not_help:
	
	lea si,[make]
	call cmp_str_cs
	or al,al
	jz not_make
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_make:
	
	lea si,[move]
	call cmp_str_cs
	or al,al
	jz not_move
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_move:
	
	lea si,[reboot]
	call cmp_str_cs
	or al,al
	jz not_reboot
		int 19h
	not_reboot:

	lea si,[rename]
	call cmp_str_cs
	or al,al
	jz not_rename
		lea si,[funktion_nicht_da]
		call write
		jmp warte_auf_eingabe
	not_rename:
	
	lea si,[run]
	call cmp_str_cs
	or al,al
	jz not_run
		lea si,[befehl]
		call write
		call break
	
		mov cx,16
		xor eax,eax
		lea di,[befehl]
		rep stosd
		
		lea si,[program_string]
		suche_space:
			lodsb
			or al,al
				jz zweites_wort
			cmp al,32
				je zweites_wort
			jmp suche_space
		zweites_wort:	;dieser Einstiegspunkt wird auch dafür verwendet, falls der Benutzer anscheinend das "Run" vor dem Programmnamen vergessen hat, wie es bei windows eig. üblich ist
		
		lea di,[befehl]
		save_datn:
			lodsb
			stosb
			or al,al
			jnz save_datn
		lea si,[befehl]
		call string283
		
		pushad
		call break
		call write
		call break
		mov si,di
		call write
		call break
		popad
		
		lea di,[befehl]
		mov ax,2500h
		call comc
		jmp warte_auf_eingabe
	not_run:

	lea si,[shutdown]
	call cmp_str_cs
	or al,al
	jz not_shutdown
		mov bp,1
		mov al,3
		int 21h
	not_shutdown:
	
	lea si,[version]
	call cmp_str_cs
	or al,al
	jz not_version
		call break
		lea si,[show_version]
		call write
		jmp warte_auf_eingabe
	not_version:
	
	lea si,[fat]
	call cmp_str_cs
	or al,al
	jnz not_fat_far
		jmp not_fat
	not_fat_far:
		mov ax,0201h
		lea bx,[boot_sektor]
		mov cx,0001h
		xor dx,dx
		int 13h

		call break
		lea si,[OS_NAME]
		mov cx,8
		mov ah,0eh
		mov bx,2
		write_os_name:
			lodsb
			int 10h
			loop write_os_name
		call break
		mov bx,10
		lodsw
		call zahlausgabe	;BPS
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;SPC
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;RESSEC
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;NFATS
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;ROOTE
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;TOTSEC
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;MEDTYP
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;FATSIZE
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;SPT
		call break
		xor eax,eax
		lodsw
		call zahlausgabe	;NH
		call break
		xor eax,eax
		lodsd
		call zahlausgabe	;HS
		call break
		xor eax,eax
		lodsd
		call zahlausgabe	;TOTS
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;DRVNUM
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;RES
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;BOOTS
		call break
		xor eax,eax
		lodsb
		call zahlausgabe	;VID
		call break
		xor eax,eax
		mov cx,11
		mov ah,0eh
		mov bx,2
		write_vlabel:
			lodsb
			int 10h
			loop write_vlabel
		mov cx,8
		call break
		write_FTSP1:
			lodsb
			int 10h
			loop write_FTSP1
		xor ax,ax
		int 16h
		jmp warte_auf_eingabe
	not_fat:

	lea si,[program_string]
	jmp zweites_wort

timer_string		db 'TIMER      '
keyb_string			db 'KEYBOARD   '
pfad				db 'A:\',0
welcome				db 'Welcome to KaOS a OS, programmed in much freetime by maxinator -> maxinator.tk',13,10
					db 'To show all available commands by typing "help" and pressing return!',13,10,0
falsche_eingabe		db 'Wrong input or unkown command!',0
funktion_nicht_da	db 'This command/function will be programmed in future!',0
show_credits		db 'KaOS:',13,10
					db 'Programmer : Maximilian K. <Maxinator>',13,10
					db 'Thanks and Greets to : T0ast3r, SPTH, joachim_neu, TeeJay and the Lowlevel-Magazin',13,10,0
show_version		db 'KaOS Version 0.0.5 Alpha',0

anf_daten_cluster	db 31		;FATSIZE*NFATS+ROOTE/16+RESSEC
anf_cluster_root	db 19		;FATSIZE*NFATS+RESSEC
anz_clusters_root	db 14		;ROOTE/16

befehl				times 64 db 0
program_string		times 64 db 0
name				times 63 db 0

cls			db 'CLS',0
copy		db 'COPY',0
credits		db 'CREDITS',0
delete		db 'DELETE',0
dir			db 'DIR',0
edit		db 'EDIT',0
help		db 'HELP',0
list		db 'LIST',0
make		db 'MAKE',0
move		db 'MOVE',0
reboot		db 'REBOOT',0
rename		db 'RENAME',0
run			db 'RUN',0
shutdown	db 'SHUTDOWN',0
version		db 'VERSION',0
fat			db 'FAT',0

writeln_help    db	'cls                   = clears the actual Screen',13,10
				db	'copy <source><target> = copy a file in <source> to <target>',13,10
				db	'credits               = shows the credits',13,10
				db	'delete <file>         = deletes <file>',13,10
				db	'dir                   = shows all files and directorys in the current destination',13,10
				db	'edit <file>           = opens a file for editing',13,10
				db	'fat                   = shows the fat-information in the mbr',13,10			
				db	'help                  = shows this text/help',13,10
				db	'make <file>           = creates a empty <file>-named file',13,10
				db	'move <source><target> = moves the <source>-file to the <target>-file',13,10
				db	'reboot                = rebootes the PC',13,10
				db	'rename <file><name>   = renames a <file> to <name>',13,10
				db	'run <file>            = runs a com-structur-like <file>',13,10
				db	'shutdown              = shutdowns the PC',13,10
				db	'version               = shows the current version of this OperatingSystem',13,10,0

;***********************************************************************
;********************* installos *********************
;***********************************************************************
installos:
	push ds						;Register speichern
	push word 0					;0 auf Stack speichern
	pop ds						;	...um es in DS zu speichern
	CLI							;Interrupt deaktivieren
	mov word[ds:4*21h],int21h	;in IVT den Offset von int21h bei dem Platz für den Int21h eintragen
	mov word[ds:4*21h+2],cs		;in IVT das Segment von int21h bei dem Platz für den Int21h eintragen
	STI							;Interrupts aktivieren
	pop ds						;Register wiederherstellen
	ret							;Prozedure verlassen

;***********************************************************************
;************ Interrupt 21h : OS Interrupt bp:funktion ***********
;***********************************************************************
int21h:
	push ds						;Register speichern
	pusha						;	~
	push cs						;Auf Stack pushen kopieren um segmentregister zu aktualisieren
	pop ds						;segmentregister aktualisieren
	
	mov ax,bp
	cmp ax,60h					;teste ob ax die nummer einer der funktionen enthält
	jb int21_funktion_vorhanden	;	...wenn ja mache weiter
		popa					;register wiederherstellen
		pop ds					;	~
		iret					;	...ansonsten springe zurück
	int21_funktion_vorhanden:
	
	STI							;Wichtig: Setze das Interrupt Flag wieder!!!!
	lea si,[int21h_procs]		;lade offset der offsetdatenbank der prozeduren
	add si,ax					;addiere den offset der datenbank mit der funktionsnummer(x2)
	add si,ax					;addiere den offset der datenbank mit der funktionsnummer(x2)
	lodsw						;auf ax den Offset der Funktion kopieren, da kein direkter Speicher zu Speicher Transfer möglich ist
	mov [funktion_jump],ax		;Offset der zu rufenden Funktion temporär speichern um Register wiederherzustellen
	
	popa						;register wiederherstellen
	pop ds						;	~
	call near[cs:funktion_jump]	;gehe zu der funktion (indirekter sprung)
	iret

int21h_procs:					;offset  der offset-tafel der einzelnen prozeduren (hier ist platz für 2 unterprozeduren)
	dw	ladebalken				;BP=0h
	dw	quit_os					;BP=1h
	dw	delay					;BP=2h
	dw	zahlausgabe				;BP=3h
	dw	readln					;BP=4h
	dw	write					;BP=5h
	dw	break					;BP=6h
	dw	clrscr					;BP=7h
	dw	cmp_str					;BP=8h
	dw	0						;BP=9h
	dw	0						;BP=Ah
	dw	0						;BP=Bh
	dw	0						;BP=Ch
	dw	0						;BP=Dh
	dw	0						;BP=Eh
	dw	0						;BP=Fh
	dw	0						;BP=10h
	dw	0						;BP=11h
	dw	0						;BP=12h
	dw	0						;BP=13h
	dw	0						;BP=14h
	dw	0						;BP=15h
	dw	0						;BP=16h
	dw	0						;BP=17h
	dw	0						;BP=18h
	dw	0						;BP=19h
	dw	0						;BP=1Ah
	dw	0						;BP=1Bh
	dw	0						;BP=1Ch
	dw	0						;BP=1Dh
	dw	0						;BP=1Eh
	dw	0						;BP=1Fh
	dw	0						;BP=20h
	dw	0						;BP=21h
	dw	0						;BP=22h
	dw	0						;BP=23h
	dw	0						;BP=24h
	dw	set_int_vector			;BP=25h
	dw	0						;BP=26h
	dw	0						;BP=27h
	dw	0						;BP=28h
	dw	0						;BP=29h
	dw	0						;BP=2Ah
	dw	0						;BP=2Bh
	dw	0						;BP=2Ch
	dw	0						;BP=2Dh
	dw	0						;BP=2Eh
	dw	0						;BP=2Fh
	dw	0						;BP=30h
	dw	0						;BP=31h
	dw	0						;BP=32h
	dw	0						;BP=33h
	dw	0						;BP=34h
	dw	get_int_vector			;BP=35h
	dw	0						;BP=36h
	dw	0						;BP=37h
	dw	0						;BP=38h
	dw	0						;BP=39h
	dw	0						;BP=3Ah
	dw	0						;BP=3Bh
	dw	0						;BP=3Ch
	dw	0						;BP=3Dh
	dw	0						;BP=3Eh
	dw	0						;BP=3Fh
	dw	0						;BP=40h
	dw	0						;BP=41h
	dw	0						;BP=42h
	dw	0						;BP=43h
	dw	0						;BP=44h
	dw	0						;BP=45h
	dw	0						;BP=46h
	dw	0						;BP=47h
	dw	0						;BP=48h
	dw	0						;BP=49h
	dw	0						;BP=4Ah
	dw	0						;BP=4Bh
	dw	exit_program			;BP=4Ch
	dw	0						;BP=4Dh
	dw	0						;BP=4Eh
	dw	0						;BP=4Fh
	dw	0						;BP=50h
	dw	0						;BP=51h
	dw	0						;BP=52h
	dw	0						;BP=53h
	dw	0						;BP=54h
	dw	0						;BP=55h
	dw	0						;BP=56h
	dw	0						;BP=57h
	dw	0						;BP=58h
	dw	0						;BP=59h
	dw	0						;BP=5Ah
	dw	0						;BP=5Bh
	dw	0						;BP=5Ch
	dw	0						;BP=5Dh
	dw	0						;BP=5Eh
	dw	0						;BP=5Fh
funktion_jump dw 0
	;***********************************************************************
	;************** ladebalken **************
	;Eingabe:	BP=0
	;		EAX=Wartezeit in hs
	;		dh=zeile
	;		dl=spalte
	;Veränderungen:	Flags,ax
	;************** ladebalken **************
	ladebalken:
		push es
		push ds
		push 2000h
		pop ds
		
		push word 0b800h
		pop es
		
		push eax
		movzx ax,dh
		mov bx,160
		mul bx
		movzx edi,ax
		mov al,dl
		mov bl,2
		mul bl
		add di,ax
		pop eax
		
		mov ebx,eax
		add eax,[ds:100h+3]
		ladebalken_loop:
			push eax
			mov ecx,[ds:100h+3]
			sub eax,ebx
			sub ecx,eax
			mov eax,ecx
			mov ecx,100
			mul ecx
			div ebx
			call zahlausgabe_ladebalken
			pop eax
			
			cmp eax,[ds:100h+3]
			jae ladebalken_loop
		pop ds
		pop es
		ret

	zahlausgabe_ladebalken:
		pusha
		xor cx,cx
		mov bx,10
		zahlausgabe_ladebalken1:
			xor dx,dx
			div bx
			push dx
			inc cx
			or ax,ax
			jne zahlausgabe_ladebalken1
		mov bx,2
		zahlausgabe_ladebalken2:
			pop ax
			add al,48
			mov ah,0eh
			stosw
		LOOP zahlausgabe_ladebalken2
		mov ax,0e25h
		stosw
		popa
		ret
	
	;************** quit_os **************
	;Eingabe:	BP=1
	;		al =Quit-Methode
	;			0=reboot
	;			1=coldboot
	;			2=warmboot
	;			3=shutdown
	;Veränderungen:	all, bei falschen Parameter al
	;************** quit_os **************
	quit_os:
		or al,al
		jnz not_0_reboot
			int 19h
		not_0_reboot:
		dec al
		or al,al
		jnz not_1_cooldboot
			mov ax,40h
			mov es,ax
			mov word[es:00072h],0
			jmp 0ffffh:0000h
		not_1_cooldboot:
		dec al
		or al,al
		jnz not_2_warmboot
			mov ax,40h
			mov es,ax
			mov word[es:00072h],01234h
			jmp 0ffffh:0000h
		not_2_warmboot:
		dec al
		or al,al
		jnz not_3_shutdown
			mov ax,5300h	;shuts down apm-machines.
			xor bx,bx		;newer machines automatically
			int 15h			;shut down
			mov ax,5304h
			xor bx,bx
			int 15h			;interrupt 15h originally was
			mov ax,5301h	;used for cartridges (cassettes)
			xor bx,bx		;but is still in use for
			int 15h			;diverse things
			mov ax,5307h
			mov bx,1
			mov cx,3
			int 15h
		not_3_shutdown:
		ret

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
	;*********************** clrscr löscht Bildschirm **********************
	;***********************************************************************
	clrscr:
		pushad
		push es
		pushf

		cld
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

		popf
		pop es
		popad
		ret

;***********************************************************************
;**** cmp_str_cs DS:SI String1 ES:DI String2 -> wenn gleich(casesensitive), al=1 ****
;***********************************************************************
cmp_str_cs:
	pusha					;Register sichern
	.such:
		xor ax,ax			;ax löschen, damit man bei setxx auch sieht, ob da was gesetzt wurde
		cmp byte[ds:si],0	;teste, ob ende von String1  erreicht ist
			setz al			;wenn dem so ist, setze al von 0 auf 1
		cmp byte[es:di],0	;ansonsten, teste, ob auch String2 zu ende ist
			setz ah			;wenn dem so ist, setze ah von 0 auf 1
		test ah,al			;testet, ob die beiden gleich gesetzt sind (1+1=1; 1+0=0; 0+0=0)
			jnz .endew		;wenn beide 1 sind, springe zu endew
		cmp al,ah			;ansonsten teste ob al=ah ist
			jne	.endef		;wenn ungleich, also sozusagen ein String zuende ist, der andere nicht, gehe zu endef
		cmpsb				;vergleiche Bytes/Zeichen der beiden Strings
			je .such		;wenn sie gleich sind gehe im String weiter
.endef:
	popa					;ansonste leite endef ein und stelle Register wieder her
	mov al,0				;belge al jedoch mit 0 als Zeichen für ungleich
	ret						;und Springe zu Aufrufsort zurück
.endew:
	popa					;stelle Register wieder her
	mov al,1				;schiebe auf al 1, als Zeichen, das Strings gleich sind
	ret						;und springe zum Aufrufsort zurück

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
		cmp al,ah			;ansonsten teste ob al=ah ist
			jne	.endef		;wenn ungleich, also sozusagen ein String zuende ist, der andere nicht, gehe zu endef
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
.endef:
	popa					;ansonste leite endef ein und stelle Register wieder her
	mov al,0				;belge al jedoch mit 0 als Zeichen für ungleich
	ret						;und Springe zu Aufrufsort zurück
.endew:
	popa					;stelle Register wieder her
	mov al,1				;schiebe auf al 1, als Zeichen, das Strings gleich sind
	ret						;und springe zum Aufrufsort zurück

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
	;*******comc ax:Segment f���Programm******
	;***********************************************************************
	comc:
		push ds
		push es
		pushad
		push ebp
		
		mov[cs:programme_segment],ax
		;lade des Root an 2000h
		push es
		mov word[cs:memory],0
		mov es,ax
		mov bp,19
		load_root:								;Lade FAT
			mov ax,bp
			call lba_chs
			xor dl,dl
			mov bx,[cs:memory]
			try_again:
				mov ax,0201H
				int 13h
			jc try_again
			add word[cs:memory],512
			inc bp
			cmp bp,33
			jne load_root
		pop es
		
		xor si,si
		mov ax,[cs:programme_segment]
		mov ds,ax
		call readfilename			;AX=Erster cLUSTER
		cmp ax,-1
		jne load_program
			jmp ende
		
		load_program:
			add ax,26
			mov si,ax
			lodsw
			mov ecx,[ds:si]
			add ax,31			;Datenbreich + Cluster
			mov bp,ax
			
			mov ax,[cs:programme_segment]
			mov es,ax
			mov ds,ax
			mov word[cs:memory],100h
			
			mov eax,ecx
			xor edx,edx
			mov ebx,512
			div ebx
			or edx,edx
			jz gerundet_weniger
				inc eax
			gerundet_weniger:
			mov ecx,eax
			
			read_program:
				push cx
				mov ax,bp
				call lba_chs
				xor dl,dl
				mov bx,[cs:memory]
				try_read_program:
					mov ax,0201h
					int 13h
				jc try_read_program
				add word[cs:memory],512
				inc bp
				pop cx
			loop read_program
		;Springe zu diesem Programm
		popad
		pushad
		
		pop ebp
		call far[cs:farcall]
	ende:
		popad
		pop es
		pop ds
		ret
memory dw 0
farcall dw 100h
programme_segment dw 0

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; readfilename: Suche in Root Dateiname
	;; Parameter: ES:DI Dateiname
	;;		 DS:SI Root
	;; Back:	AX=-1 Fehler(Nicht gefunden)
	;;		AX=Nummer des Filetable Eintrages
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	readfilename:
		push bx
		push cx
		push si
		mov cx,224
		cld
		READ_FN:
			pusha
			mov cx,11
			repe cmpsb
			or cx,cx
			popa
				je DAT_OK
			add si,32
			loop READ_FN
				mov ax,-1
				jmp end_on_error
		DAT_OK:
			mov ax,si			;AX zeigt auf den Filetable Eintrag
		end_on_error:
		pop si
		pop cx
		pop bx
		ret

	;Von LBA zu CHS: 
	;Sektor = LBA_Sektor % SectorsPerTrack
	;Temp = LBA_Sektor / SectorsPerTrack
	;Cylinder = Temp / Head_anzahl
	;Head = Temp % Head_anzahl
	;***************************************************************************************************
	;******************* lba_chs ax:lba dh:headanzahl -> cl=cylinder ch=sector dh=head *****************
	;***************************************************************************************************
	lba_chs:
		xor	dx,dx			;clear out DX reg
		mov bx,18
		div	bx				;divide AX by SPT
		inc dl				;increment dl to get sector address
		mov	cl,dl			;get sector value
		xor	dx,dx			;clear DX reg
		mov bx,2
		div	bx
		mov	ch,al			;get cylinder value
		mov	dh,dl			;get head value
	ret
	
	;************************************************************************************
	;******************* exit_program (retf)nur wegen DOS-kompatiblität *****************
	;************************************************************************************
	exit_program:
		pop eax
		pop eax
		mov eax, esp
		mov ebx, 16
		call break
		call zahlausgabe
		call break
		xor ax,ax
		int 16h
		retf
	ret
	
	;************************************************************************************
	;******************* show_dir listet die verzeichnisse des Pfades auf ****************
	;************************************************************************************
	show_dir:
		push ds
		push es
		pushad
		
		;lade des Root an 3000h
		push ax
		mov bp,ax
		mov word[cs:memory2],0
		add cx,ax
		push word 3000h
		pop es
		load_root_dir:								;Lade FAT
			push cx
			mov ax,bp
			call lba_chs
			xor dl,dl
			mov bx,[cs:memory2]
			try_again_dir:
				mov ax,0201H
				int 13h
			jc try_again_dir
			add word[cs:memory2],512
			inc bp
			pop cx
			cmp bp,cx
			jne load_root_dir
		pop ax
		
		mov bx,16
		mul bx
		mov cx,ax
		cld
		xor si,si
		push 3000h
		pop ds
		read_filename_dir:
			mov al,[ds:si]
			or al,al
				jz next_entry
			cmp al,20h
				jb next_entry
			cmp al,22h
				je next_entry
			cmp al,2ah
			jnae not_2a2f
				cmp al,2fh
				jbe next_entry
			not_2a2f:
			cmp al,3ah
			jnae not_3a3f
				cmp al,3fh
				jbe next_entry
			not_3a3f:
			cmp al,5bh
			jnae not_5b5d
				cmp al,5dh
				jbe next_entry
			not_5b5d:
			cmp al,7ch
			je next_entry
			
			pusha
			mov ah,0Eh			;Funktion 0x0E
			mov bx,2			
			mov cx,11
			writeln2_dir:
				lodsb				;Byte laden
				int 10h				;schreiben
				loop writeln2_dir
			writelnd_dir:
			mov al,13
			int 10h
			mov al,10
			int 10h
			popa
		next_entry:
			add si,32
			loop read_filename_dir
		popad
		pop es
		pop ds
		ret
	ret
memory2 dw 0
	
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
	
;***************************************************************************
;******************* string283 ändert string ins 8.3 Format ****************
;***************************************************************************
string283:
	pusha							;Register speichern
	push si							;Si speichern
	mov di,si						;für upcase-String si auf di kopieren
	call upcase_string				;aus loweralpha higheralpha machen
	
	lea di,[temp_string283]			;di auf den Zwischenspeicher dieser Prozedur ausrichten
	mov cx,8						;auf cx 8 schmeißen, da nur 8 Zeichen für den Namen erlaubt sind
	string283_loop:
		lodsb						;laden des Zeichens aus dem vom user mitgegeben String
		cmp al,46					;mit 46 vergleichen (.)
			je string283_loop_ende	;wenn das ein Punkt ist, dann beende loop um die fileext auszulesen
		or al,al					;teste ob ende des Strings erreicht ist
			jz string283_loop_ende	;wenn ja beende loop um file-ext auszlesen
		stosb						;speichern des gelesenen Zeichens in den Buffer
		loop string283_loop			;8 Zeichen einlesen, dann abbruch des loop erzwingen
			lodsb					;wenn das Kopieren des Strings verfrüht abgebochen werden musste, lade noch schnell das nächste Zeichen
	string283_loop_ende:			;... zum vergleichen in al, da das, was derzeit in al liegt allefälle kein Punkt oder EOS sein kann
	push ax					;al für spätere abfragen speichern
	mov al,32				;ansonsten schiebe auf al space um damit den string zu füllen
	rep stosb				;füllen des string bis zu 8 zeichen mit space
	pop ax					;al wiederherstellen
	
	mov cx,3								;auf cx für rep 3 schieben, da ext auf 3 zeichen begrenzt ist
	cmp al,46								;mit 46 vergleichen (.)
	jne mb_no_extension						;wenn das ein Punkt ist, dann beginne extension zu kopieren
	extension_available:
		rep movsb							;die nächsten 3 Zeichen aus dem String als ext kopieren
		jmp ext_copy_end					;die Befehle von no_ext überspringen um dem User das Resultat zu übergeben
	mb_no_extension:
		or al,al							;testen ob der String wirklich zu ende ist, oder nur wegen der Begrenzung von 8 Zeichen abgebrochen wurde
		jz no_extension						;wenn ja, dann fülle die extension mit space
			mov word[es:di-2],"~1"			;ansonsten ersetze die letzten beiden filenamezeichen mit "~1"
			search_eos_or_dot:
				lodsb						;weiteres Zeichen von angegebenen String holen, um das ende oder den punkt zu finden
				cmp al,46					;testen ob al punkt ist
					je extension_available	;wenn ja gehe zu den Schritten, die die Ext in den Buffer kopieren
				or al,al					;ansonsten teste, ob der String zu ende ist
			jnz search_eos_or_dot			;und lade das nächsten zeichen, falls dem nicht so ist, oder fall zu no_extension durch, falls dem so ist
		no_extension:
		mov al,32							;auf al space schieben um damit ext zu füllen
		rep stosb							;füllen der ext im Buffer mit space
	ext_copy_end:
	
	pop di							;di(si) wiederherstellen
	lea si,[temp_string283]			;und auf si offset des buffers schieben
	mov cx,11						;und auf cx die länge des buffers schieben
	rep movsb						;um den buffer auf den ursprungsstring des Users zu kopieren
	xor al,al
	stosb
	popa							;Register wiederherstellen
ret									;zu Aufrufsort zurückspringen
temp_string283 times 11 db 0

bootdrive db 224

big1 times stellen db 49
big2 times stellen db 50
big3 times stellen db 51

boot_sektor:
	jmp word boot_sektor		;JmpBoot		Sprung zu Bootsector (1Byte=Jmp + 2Byte=WORD Start	->3		3
	OS_NAME	DB	"KaOS    "		;OSName		Name des Betriebssystems							8
	BPS		DW	512				;BytesPerSec	Bytes pro Sektor, bei Diskette 512						2
	SPC		DB	1				;SecPerClus		Sektoren pro Cluster bei Disktte 1						1
	RESSEC	DW	1				;RsvdSecCnt	reservierte Sektoren, nur Boot-Sektor					2
	NFATS	DB	2				;NumFATs		Anz. der FAT-Tabellen, aus Sicherheit immer 2				1
	ROOTE	DW	224				;RootEntCnt		Anz. der Einträge ins Root-Verzeichnis, 224 reichen voll und ganz	2
	TOTSEC	DW	2880			;TotSec		gesamte Anzahl der Sektoren, bei Diskette (1.44M) 2880		2
	MEDTYP	DB	0F0H			;MediaType		Medien Typ, Diskette = 0Fh							1
	FATSIZE	DW	9				;FATSize		Anzahl der Sektoren, die eine FAT-Tabelle belegt ist 9			2
	SPT		DW	18				;SecPerTrack	Sektoren pro Cylinder(Track) gibt es auf einer 1.44M Diskette 18	2
	NH		DW	2				;NumHeads		Anzahl der Seiten, ist bei 1.44M Diskette 2					2
	HS		DD	0				;HiddenSec		Anzahl der versteckten Sektoren ?						4
	TOTS	DD	0				;TotSec32		nur für FAT32 -> deshalb 0							4
	DRVNUM	DB	0				;DrvNum		Laufwerks-Nummer bei Diskette (A:) = 0					1
	RES		DB	0				;Reserved		sollte 0 enthalten									1
	BOOTS	DB	29H				;														1
	VID		DD	0				;VolumeID		wird meist mit der Zeit belegt							4
	VLABEL	DB	"NO NAME    "	;														11
	FSTP1	DB	"FAT12   "		;FileSysType	FAT-Dateisystem									8
	times 512-($-OS_NAME-3) db 0
