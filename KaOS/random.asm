org 100h

mov bp,7
int 21h

push cs
pop es
lea edi,[primfaktoren_array]
call readzahl
	push ax
	mov ax,0e0ah
	mov bx,2
	int 10h
	mov al,13
	int 10h
	pop ax
call primfaktoren

push cs
pop ds
lea esi,[primfaktoren_array]
write_faks:
	lodsd
	push ax
	
	mov ebx,10
	call zahlausgabe
	mov ax,0e0ah
	mov bx,2
	int 10h
	mov al,13
	int 10h
	
	pop ax
	or eax,eax
	jnz write_faks

	mov ax,0e0ah
	mov bx,2
	int 10h
	mov al,13
	int 10h

call randomize
schleife:
	mov dword[m],23
	call random
	mov bx,10
	call zahlausgabe
	mov ax,0e0ah
	mov bx,2
	int 10h
	mov al,13
	int 10h
	xor ax,ax
	int 16h
	cmp al,27
	jne schleife


mov ah,4ch
int 21h

randomize:
	mov ax,40h
	mov es,ax
	mov eax,[es:6ch]
	mov [saat],eax		;temporär speichern
	ret

; xn = (a * xn-1 + b) mod m
; nk+1 = (ank + c) mod m.
; Die maximale Periode einer solchen Folge ist offenbar m, und es gibt keine k¨urzere, wenn
; - c und m teilerfremd sind,
; - a - 1 ein Vielfaches von jedem Primfaktor von m ist,
; - a - 1 ein Vielfaches von 4 ist, falls vier Teiler von m ist.
; Eine erfahrungsgem¨aß gute Wahl ist
; a = 7^5,m = 2^31 - 1(~2.1 · 109), c = 0.
random:
	mov eax,[saat]
	mov ebx,[a]
	mul ebx
	mov ebx,[c]
	add eax,ebx
	mov ebx,[m]
	div ebx
	mov [saat],edx
	mov eax,edx
	ret

a dd 1
m dd 2147483647
saat dd 3145189
c dd 3

;***********************************************************************
;************* Zahlausgabe eax:zahl ebx:basis Ausgabe:zahl *************
;***********************************************************************
zahlausgabe:
	pushad
	XOR ECX,ECX
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
;**************** Prim eax:zahl al:0 keine Prim; 1 Prim ****************
;***********************************************************************
prim:
	 pusha
	 mov ebx,eax
	 mov ecx,2
	 FORSCHLEIFE:
		 xor edx,edx
		 mov eax,ebx
		 div ecx
		 
		 or edx,edx
		 jz NOTPRIM
		 
		 inc ecx
		 cmp ecx,ebx
	 jb FORSCHLEIFE
	 popa
	 mov eax,1
	 ret
 NOTPRIM:
	 popa
	 xor eax,eax
	 ret

;***********************************************************************
;*************** Primlast eax:zahl eax:letzte Primzahl *****************
;***********************************************************************
primlast:
	 push ebx
	 push ecx
	 push edx
	 mov ebx,eax

	 BEFORE_FORSCHLEIFEL:
	 mov ecx,2
	 FORSCHLEIFEL:
		 xor edx,edx
		 mov eax,ebx
		 div ecx
		 
		 or edx,edx
		 jnz ist_priml
			dec ebx
			jmp BEFORE_FORSCHLEIFEL
		 ist_priml:
		 
		 inc ecx
		 cmp ecx,ebx
	 jb FORSCHLEIFEL

	 mov eax,ebx
	 pop edx
	 pop ecx
	 pop ebx
	 ret
 
;***********************************************************************
;*************** Primnext eax:zahl eax:nächste Primzahl ****************
;***********************************************************************
primnext:
	 push ebx
	 push ecx
	 push edx
	 mov ebx,eax

	 BEFORE_FORSCHLEIFEN:
	 mov ecx,2
	 FORSCHLEIFEN:
		 xor edx,edx
		 mov eax,ebx
		 div ecx
		 
		 or edx,edx
		 jnz ist_prim
			inc ebx
			jmp BEFORE_FORSCHLEIFEN
		 ist_prim:
		 
		 inc ecx
		 cmp ecx,ebx
	 jb FORSCHLEIFEN

	 mov eax,ebx
	 pop edx
	 pop ecx
	 pop ebx
	 ret

;***********************************************************************
;*************** Primfaktoren eax=zahl -> es:edi=string wo Faktoren drin sind(ca. 32 Bytes) ****************
;***********************************************************************
primfaktoren:
	pushad
	cld
	mov ecx,2
	mov ebx,eax
	schleife_faks:
		mov eax,ebx
		xor edx,edx
		div ecx
		
		or edx,edx
		jnz kein_faktor
			mov ebx,eax
			mov eax,ecx
			stosd
			jmp schleife_faks
		kein_faktor:
		
		cmp ecx,ebx
		jae primfak_ende
			inc ecx
		jmp schleife_faks
	primfak_ende:
	popad
	ret
primfaktoren_array times 32 dd 0

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
	mov ecx,10
	readzahl_loop:
		xor eax,eax				;auf tastendruck warten
		int 16h					;ah:scan-code	al:ascii-code
		mov ah,0eh				;teletype output al:ascii-code
		int 10h					;zeichen wird jetzt ausgegeben
		
		xor ah,ah
		sub al,48				;aus zeichen eine zahl machen (für bessere lesbarkeit bei den Funktionen)
		cmp al,10				;prüfen ob zeichen eine zahl des angegeben Zahlensystems war
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
