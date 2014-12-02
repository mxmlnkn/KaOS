;######################################################################
;**********************************************************************
;**********                 KERNEL                           **********
;**********************************************************************
;######################################################################
;mit NASM compiliert
stellen equ 1999
basis equ 10
bits 16

kernel:
;installiere Keyboard und Timer
mov ax,cs
mov ds,ax
mov es,ax

mov [bootdrive],dl
movzx eax,dl
mov bx,10
call zahlausgabe
call break

mov ebp,8h
lea si,[timer_string]
mov ax,2000h
call comc

mov ebp,09h
lea si,[keyb_string]
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
lea si,[einleitung]
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
	mov cx,24
	call readln				;bei leerem String, also nur enter hängt der pc sich auf!! -> liegt an dem iso-laden-proc, da es den ersten eintrag lädt, der anscheinend "" ist... -> zeiger muss irgendwie auf ersten echten Eintrag geschiben werden
	call upcase_string		;es:di = String

	pusha
	mov si,di
	lea di,[befehl]
	erstes_wort2befehl:
		lodsb
		cmp al,64
		jna erstes_wort2befehl_ende
			cmp al,91
			jnb erstes_wort2befehl_ende
		stosb
		jmp erstes_wort2befehl
	erstes_wort2befehl_ende:
	xor al,al
	stosb
	popa
	lea di,[befehl]

;#################################################################
;########################Befehl testen############################
;#################################################################
	
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
		lea si,[funktion_nicht_da]
		call write
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
		mov cx,25
		xor al,al
		lea di,[befehl]
		rep stosb
		
		lea si,[program_string]
		suche_space:
			lodsb
			cmp al,64
			jna zweites_wort
				cmp al,91
				jnb zweites_wort
			jmp suche_space
		zweites_wort:	;dieser Einstiegspunkt wird auch dafür verwendet, falls der Benutzer anscheinend das "Run" vor dem Programmnamen vergessen hat, wie es bei windows eig. üblich ist
			
		lea di,[befehl]
		save_datn:
			lodsb
			stosb
			or al,al
			jnz save_datn
		lea si,[befehl]
		mov ax,2500h
		cmp byte[befehl],0
			jz warte_auf_eingabe
		
		call comc
		test al,al
			jz warte_auf_eingabe
		
		call break
		lea si,[falsche_eingabe]
		call write
		lea si,[einleitung]
		call write
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
		lea si,[no_fat]
		call write
		jmp warte_auf_eingabe
	not_fat:

	lea si,[program_string]
	jmp zweites_wort

timer_string		db 'TIMER',0
keyb_string			db 'KEYBOARD',0
pfad				db 'D:\',0
einleitung			db 'Welcome to KaOS a OS, programmed in much freetime by maxinator -> maxinator.tk',13,10,13,10
					db 'To show all available commands by typing "help" and pressing return!',0
falsche_eingabe		db 'Wrong input or unkown command!',0
funktion_nicht_da	db 'This command/function will be programmed in future!',0
show_credits		db 'KaOS:',13,10
					db 'Programmer : Maximilian K. <Maxinator>',13,10
					db 'Thanks and Greets to : T0ast3r, SPTH, joachim_neu, TeeJay and the Lowlevel-Magazin',13,10,0
show_version		db 'KaOS Version 0.0.5 Alpha',0
no_fat				db 'You are booting from CD, which dont use fat.',0

anf_daten_cluster	db 31		;FATSIZE*NFATS+ROOTE/16+RESSEC
anf_cluster_root	db 19		;FATSIZE*NFATS+RESSEC
anz_clusters_root	db 14		;ROOTE/16

befehl				times 1023 db 0
program_string		times 221 db 0
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
		pushad
		push es
		push ds
		push 2000h
		pop ds
		
		push eax
		call get_video_ram
		mov es,ax
		
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
			cmp eax,100
			jna not_higher_then_100_percent
				mov eax,100
			not_higher_then_100_percent
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
			
			pop eax
			cmp eax,[ds:100h+3]
			jae ladebalken_loop
		pop ds
		pop es
		popad
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
	xor al,al				;belge al jedoch mit 0 als Zeichen für ungleich
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
	
	;********************************************************************************************
	;******************* comc - laedt datei nach ax:100h und gibt bei fehler in al eine 1 aus ****************
	;********************************************************************************************
	comc:
		push ds
		push es
		pushad
		
		mov byte[cs:fehler_al_comc],1
		mov [farjump+2],ax
		mov bx,100h
		mov dl,[bootdrive]
		call iso9660.load_file
		test al,al
		jnz .no_program_found
			mov ax,[farjump+2]
			mov ds,ax
			mov es,ax
			popad
			pushad
			call far[cs:farjump]
			mov byte[cs:fehler_al_comc],0
		.no_program_found:
		
		popad
		pop es
		pop ds
		mov al,[cs:fehler_al_comc]
		ret
	
	fehler_al_comc db 0
	ss_save dw 0
	sp_save dw 0
	farjump dw 100h,0

;********************************************************************************************
;******************* upcase_string ändert alle kleinen Buchstaben in große um ****************
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
	;************* get_video_ram ax:segment im ram (0=Fehler) *************
	;***********************************************************************
	get_video_ram:
		pushf
		mov ah,0Fh
		int 10h
		cmp al,6
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

bootdrive db 224

;######################################################################
;**********************************************************************
;**********                 ISO9660-Filesystem                           **********
;**********************************************************************
;######################################################################

;Allgemeine Informationen:
; -ein CD-Sektor ist 2048Bytes groß

iso9660:
;***********************************************************************
;******* load_file ax:bx = Segment:Offset dl=drive ds:si=Name cx=sektoren *******
;***********************************************************************
.load_file:
	pushad
	push ds
	push es

	mov [cs:file.offset],si
	mov [cs:file.segment],ds
	mov [cs:dap.segment],ax
	mov [cs:dap.offset],bx
	mov dword[cs:dap.lba],10h
	mov [bootdrv],dl			;save bootdrive
	mov es,ax					;set ES segment of loaded block from cd
	push cs						;CS contain the segment of this program
	pop ds						; ...so it contains also the segment for dap
	
.load_vol_desc:
	lea si,[dap]			;Zeiger SI auf "Disk Acces Paket"
	mov ah,42h				;advanced read out of disk to memory
	int 13h					;load the lba which stands in the dap into the location which stands also in the dap
	jnc .go					;if an error occurs go to write error message
		jmp errors.err_read
	.go:
	cmp byte[es:bx],1		;1 signs primary volume - see also iso9660
	je .end_vol_desc		;is this the primary volume descriptor, if yes jump
	cmp byte[es:bx],0ffh
	jne .go2				;is this the last volume descriptor, if yes jump
		jmp errors.err_read
	.go2:

	inc dword[dap.lba]		;increase var for lba in dap
	jmp .load_vol_desc		;go to loop sothat the next lba will be searched for primary volume
.end_vol_desc:

mov eax,dword[es:bx+iso9660vol_desc.root_directory_record+iso9660vol_desc.root_directory_record.location_of_data]	;the start lba of the root directory
mov ecx,dword[es:bx+iso9660vol_desc.root_directory_record+iso9660vol_desc.root_directory_record.data_length]		;the length of the root directory
shr ecx,11					;length of root dir / 2048, because we need the number of blocks

mov [dap.blocks],cx			;save number of blocks 2 load
mov [dap.lba],eax			;save lba to load
lea si,[dap]
mov ah,42h
int 13h
jnc .pass
	jmp errors.err_read
.pass:
;===============================================================================================
load_loader:
	mov si,bx
	shl ecx,11					;in ecx is the # of blocks of the root dir, but we need the size
	add ecx,ebx
.loop_search:
	cmp byte[es:si],0
	jne .go
		jmp errors.err_no_loader				;is this no root dir entry, if yes jump
	.go:
	cmp si,cx
	jne .go2
		jmp errors.err_no_loader				;is si equ size of root dir, if yes jump
	.go2:
	mov di,si
	add di,iso9660vol_desc.root_directory_record.file_id				;move the addr of the act entry into di and let it point to the file name
	
	pusha						;alle Register sichern, da hier viele stark verändert werden
	movzx cx,byte[es:si+iso9660vol_desc.root_directory_record.length_file_id]	;länge des Dateinames nach ax holen
	mov ax,[cs:file.segment]	;auf ax die segment-adresse des gesuchten namen-strings der datei holen
	mov ds,ax					;und in ds speichern
	mov si,[cs:file.offset]		;dann den offset des angegebenen strings fr den gesuchten dateinamen nach si holen
	add di,cx					;auf di die länge des namens addieren, damit hinter den namen ein eos für cmp_str platziert werden kann
	mov ah,[es:di]				;das byte, was am end of string steht speichern, da es noch benötogt wird und zu iso-spec gehört
	mov [es:di],byte 0			;den eos platzieren
	sub di,cx					;di wieder auf die ursprüngliche länge bringen, sodass es am anfang des strings steht
	call cmp_str				;damit der dateinamen-string aus der iso-table und der des users verglichen werden kann
	add di,cx					;platziere di nocheinmal auf den eos
	mov [es:di],ah				;und stelle das vorher gespeicherte zeichen wieder her
	test al,al					;prüfe, ob al 0 ist
	popa						;und stelle erstmal register wieder her, da das die flags nicht beeinflusst und dann evtl. weggesprungen wird
		jnz .load				;wenn al=1, dann stimmen strings überein und die datei kann in den ram gelesen werden

	movzx ax,byte[es:si+iso9660vol_desc.root_directory_record.length_directory_record]
	add si,ax					;get size of the act entry and add it to si, so that si points to the next entry
	jmp .loop_search

.load:
	mov eax,dword[es:si+iso9660vol_desc.root_directory_record.data_length]		;length of the file in ECX
	xor edx,edx
	mov ecx,2048
	div ecx						;because the length of file is in byte, ecx is divided with 2048, sothat it is Kilobytes
	mov cx,ax
	or dx,dx
	jz .dividable_with_2048
		inc cx					;if the filesize not divideable with 2048 then it will round down, but these lines let it round up
	.dividable_with_2048
	mov al,[es:si+iso9660vol_desc.root_directory_record.file_flags]
	mov [file.flags],al
	mov eax,dword[es:si+iso9660vol_desc.root_directory_record.location_of_data]	;start lba of the file in EAX	

	mov [dap.blocks],cx			;save number of blocks 2 load
	mov [dap.lba],eax			;save lba to load
	lea si,[dap]
	mov ah,42h
	mov dl,byte[bootdrv]		;;move bootdrive into dl for the loader
	int 13h
	
	pop es
	pop ds
	popad
	mov cx,[dap.blocks]
	mov dl,[file.flags]
	ret
;==========================================================================================================================

errors:
.err_read:
	pop es
	pop ds
	popad
	mov al,1
ret
.err_no_loader:
	pop es
	pop ds
	popad
	mov al,2
ret

;================================================vars================================================
dap:
	.size		db 10h
	.reserved	db 0
	.blocks		dw 1			;amount of transfering block
	.offset		dw 0			;Transfer Buffer
	.segment	dw 0			;	~
	.lba		dd 10h,0
bootdrv			db 0

file:
 .offset	dw 0
 .segment	dw 0
 .flags		db 0

;================================================references================================================
iso9660vol_desc:
	.volume_type_descriptor			equ	0
	.standard_id					equ	1
	.volume_descriptor_version		equ	6
	.volume_flags					equ	7
	.system_id						equ	8
	.vol_id							equ	40
	.reserved1						equ	72
	.volume_space_size				equ	80
	.reserved2						equ	88
	.volume_set_size				equ	120
	.volume_sequenz_number			equ	124
	.locical_block_size				equ	128
	.path_table_size				equ	132
	.first_sect_little_1st_table	equ	140
	.first_sect_little_2nd_table	equ	144
	.first_sect_big_1st_table		equ	148
	.first_sect_big_2nd_table		equ	152
	.root_directory_record			equ	156
		.root_directory_record.length_directory_record			equ	0
		.root_directory_record.extended_attribut_record_length	equ	1
		.root_directory_record.location_of_data					equ	2
		.root_directory_record.data_length						equ	10
		.root_directory_record.recording_date_time				equ	18
		.root_directory_record.file_flags						equ	25
		.root_directory_record.file_unit_size					equ	26
		.root_directory_record.interleave_gap_size				equ	27
		.root_directory_record.volume_sequenz_number			equ	28
		.root_directory_record.length_file_id					equ	32
		.root_directory_record.file_id							equ	33
	.volume_set_id					equ	190
	.publisher_id					equ	318
	.data_preparer_id				equ	446
	.application_id					equ	574
	.copyright_file_id				equ	702
	.abstract_file_id				equ	739
	.bibliographica_file_id			equ	776
	.volume_creation_date_time		equ	813
	.volume_modification_date_time	equ	830
	.volume_expiration_date_time	equ	847
	.volume_effective_date_time		equ	864
	.file_structure_version			equ	881
	.reserved4						equ	882
	.reserved_for_application		equ	883
	.reserved5						equ	1395
