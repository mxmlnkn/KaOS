;######################################################################
;**********************************************************************
;**********                 MD5-Modul                           **********
;**********************************************************************
;######################################################################
;mit NASM compiliert
org 100h
h0 equ 67452301h
h1 equ 0EFCDAB89h
h2 equ 98BADCFEh
h3 equ 10325476h

;########################################## Initilization ##########################################
mov ax, cs
mov es, ax
mov ds, ax

;########################################## MD5 What to do - Choice ##########################################

read_choice_cracker_hasher:
	lea esi, [welcome]
	call write

	lea edi,[message]
	mov ecx,1
	lea esi,[only_choice_012]
	call readln_special

	mov al,[message]
	sub al, 48
	jz md5_hasher
		dec al
		jnz not_md5_cracker_nor_hasher
			jmp md5_cracker
		not_md5_cracker_nor_hasher:
			dec al
			jnz read_choice_cracker_hasher
				retf
;########################################## MD5 What to do - Choice ##########################################

;########################################## MD5 Hasher ##########################################

md5_hasher:
;*************** Ask for String ***************
 lea esi,[hasher]
 call write
 
 lea edi, [message]
 xor eax,eax
 mov ecx, 16
 rep stosd
 
 lea edi,[message]
 mov ecx, 55
 call readln
 call break

 mov eax,[timer]
 xor edx,edx
 mov ebx,20
 div ebx
 mov [start_time], eax
;*************** Ask for String ***************
 mov eax, h0
 mov ebx, h1
 mov ebp, h2
 mov edx, h3

 xor ecx,ecx
 round1:
	mov edi, edx
	mov edx, ebp
	mov ebp, ebx

	mov ebx, edi         ;################################
	xor ebx, edx         ;################################
	and ebx, ebp         ;### d xor (b and (c xor d)); ###
	xor ebx, edi         ;################################

	add ebx, eax
	movzx eax, byte [k+ecx]     ;###### add Message[k[cx]] ######
	add ebx, [message+eax*4]    ;###### add Message[k[cx]] ######
	add ebx, [T+ecx*4]          ;########## add T[cx] ###########
	xchg al, cl                 ;################################
	mov cl, [r+ecx]             ;######## rol x, r[cx] ##########
	rol ebx, cl                 ;################################
	xchg cl, al                 ;################################

	add ebx, ebp
	mov eax,edi

	inc ecx              ;################################
	cmp ecx,15           ;### repeat round1; if cx<=15 ###
	jle round1           ;################################	
 round2:
	mov edi, edx
	mov edx, ebp
	mov ebp, ebx

	mov ebx, edx         ;################################
	xor ebx, ebp         ;################################
	and ebx, edi         ;### c xor (d and (b xor c)); ###
	xor ebx, edx         ;################################

	add ebx, eax
	movzx eax, byte [k+ecx]     ;###### add Message[k[cx]] ######
	add ebx, [message+eax*4]    ;###### add Message[k[cx]] ######
	add ebx, [T+ecx*4]          ;########## add T[cx] ###########
	push cx                     ;################################
	mov cl, [r+ecx]             ;######## rol x, r[cx] ##########
	rol ebx, cl                 ;################################
	pop cx                      ;################################

	add ebx, ebp
	mov eax,edi

	inc ecx              ;################################
	cmp ecx,31           ;### repeat round2; if cx<=31 ###
	jle round2           ;################################	
 round3:
	mov edi, edx
	mov edx, ebp
	mov ebp, ebx
	xor ebx, edx         ;################################
	xor ebx, edi         ;######## b xor c xor d; ########

	add ebx, eax
	movzx eax, byte [k+ecx]     ;###### add Message[k[cx]] ######
	add ebx, [message+eax*4]    ;###### add Message[k[cx]] ######
	add ebx, [T+ecx*4]          ;########## add T[cx] ###########
	push cx                     ;################################
	mov cl, [r+ecx]             ;######## rol x, r[cx] ##########
	rol ebx, cl                 ;################################
	pop cx                      ;################################

	add ebx, ebp
	mov eax,edi

	inc ecx              ;################################
	cmp ecx,47           ;### repeat round3; if cx<=47 ###
	jle round3           ;################################	
 round4:
	mov edi, edx
	mov edx, ebp
	mov ebp, ebx

	mov ebx, edi         ;################################
	not ebx              ;################################
	or  ebx, ebp         ;##### c xor (b or (not d)); ####
	xor ebx, edx         ;################################

	add ebx, eax
	movzx eax, byte [k+ecx]     ;###### add Message[k[cx]] ######
	add ebx, [message+eax*4]    ;###### add Message[k[cx]] ######
	add ebx, [T+ecx*4]          ;########## add T[cx] ###########
	push cx                     ;################################
	mov cl, [r+ecx]             ;######## rol x, r[cx] ##########
	rol ebx, cl                 ;################################
	pop cx                      ;################################

	add ebx, ebp
	mov eax,edi

	inc ecx              ;################################
	cmp ecx,63           ;### repeat round4; if cx<=63 ###
	jle round4           ;################################
;%include "md5_algorithmus.asm"

 add eax, h0
 add ebx, h1
 add ebp, h2
 add edx, h3
 
 mov [md5_hash_128bit], eax
 mov [md5_hash_128bit+4], ebx
 mov [md5_hash_128bit+8], ebp
 mov [md5_hash_128bit+12], edx

;*************** Show Results ***************
mov eax, dword[timer]
xor edx,edx
mov ebx,20
div ebx
sub eax, dword[start_time]

lea esi,[cycles_msg]
call write
mov ebx, 10
call zahlausgabe
call break

lea si, [result]
call write
lea si, [md5_hash_128bit]
mov ecx, 16
mov ebx, 16
xor eax, eax
write_byte_128:
  lodsb
  call zahlausgabe_byte
  loop write_byte_128
;*************** Show Results ***************

jmp read_choice_cracker_hasher 

;########################################## MD5 Hasher ##########################################

;########################################## MD5 Cracker ##########################################

md5_cracker:
;*************** Ask for Hash ***************
 lea si, [cracker]
 call write
 lea edi,[message]
 lea esi,[only_hex_digits]
 mov ecx,32
 call readln_special
 lea esi, [message]

 lea edi, [md5_hash_128bit]
 xor eax,eax
 mov ecx, 4
 rep stosd
 lea edi,[md5_hash_128bit]
; *************** Ask for Hash ***************

; *************** Save Hash as digit ***************
 mov cl, 2
 hex_to_dec:
	lodsb
	xchg al, dl
	shl al, 4
	cmp dl, 58
	jb normal_number
		and dl, 11011111b
		sub dl, 7
	normal_number:
	sub dl, 48
	add dl, al
	loop hex_to_dec
		mov cl, 2
		mov al, dl
		stosb
		xor eax, eax
		cmp edi, md5_hash_128bit+17
		jne hex_to_dec
;*************** Save Hash as digit ***************

lea esi,[charset_choice_msg]
call write
lea edi,[message]
mov ecx,1
lea esi,[only_choice_1_12]
call readln_special
mov al,[message]
sub al, 48

or al,al
jnz choice_not_0
	lea esi,[higher_case]
choice_not_0:
dec al
jnz choice_not_1
	lea esi,[higher_case_num]
choice_not_1:
dec al
jnz choice_not_2
	lea esi,[higher_case_num_special_chars]
choice_not_2:
dec al
jnz choice_not_3
	lea esi,[higher_case_all]
choice_not_3:
dec al
jnz choice_not_4
	lea esi,[lower_case]
choice_not_4:
dec al
jnz choice_not_5
	lea esi,[lower_case_num]
choice_not_5:
dec al
jnz choice_not_6
	lea esi,[lower_case_num_special_chars]
choice_not_6:
dec al
jnz choice_not_7
	lea esi,[lower_case_all]
choice_not_7:
dec al
jnz choice_not_8
	lea esi,[alphabet]
choice_not_8:
dec al
jnz choice_not_9
	lea esi,[alphabet_num]
choice_not_9:
sub al,40
jnz choice_not_a
	lea esi,[alphabet_num_special_chars]
choice_not_a:
dec al
jnz choice_not_b
	lea esi,[all_chars]
choice_not_b:

rdtsc
mov [cpu_clock_cycles+4],edx
mov [cpu_clock_cycles],eax
mov eax,[timer]
mov [start_time], eax

;###########################################################
;###########################################################
;###########################################################
;#################### dual-core-Support by letting core0 ###################
;################### calculate everey first messagfe and ###################
;##################### core1 every second message ######################
;###########################################################
;###########################################################
;###########################################################

;*************** Cracking-main-loop ***************
push ds									;DS sichern
xor ax,ax								;0 in ax speichern
mov ds,ax								;	...um es in DS zu speichern
CLI										;Interrupt deaktivieren
mov eax,[ds:4*8h]						;Interrupt-Vektor in eax holen
mov [int08h_save],eax					;um ihn zu speichern
mov eax,[ds:4*9h]						;Interrupt-Vektor in eax holen
mov [int09h_save],eax					;um ihn zu speichern

mov word[ds:4*9h],md5_crack_keyb_drv	;in IVT den Offset vom neuen Keyboard-Driver bei dem Platz für den Int9h eintragen
mov word[ds:4*9h+2],cs					;in IVT das Segment vom neuen Keyboard-Driver bei dem Platz für den Int9h eintragen
mov word[ds:4*8h],timer_handler			;auf high-word des IVT(8h) Offset des eigenen Int-Handlers verschieben
mov word[ds:4*8h+2],cs					;auf low-word des IVT(8h) Segment des eigenen Int-Handlers verschieben
STI										;Interrupts aktivieren
pop ds									;DS wiederherstellen

 mov al,34h						;auf al Byte verschieben um es an einen Port zu senden
 out 43h,al						;Byte wählt Zähler 0 im PIT-Port an
 mov al,0Bh						;auf al Byte verschieben um es an einen Port zu senden
 out 40h,al						;Byte gibt den low-teil des teilers an, der benötigt wird um die Frequenz einzustellen
 mov al,0E9h
 out 40h,al						;Byte sendet high-teil des teilers
mov dword[timer],1				;timer, falls schon benutzt wieder zurücksetzen
 
lea edi, [message]
xor eax,eax
mov ecx, 16
rep stosd

mov dword[cracks_tested],0

mov dword[message+56], 8
mov al,[esi+1]
mov byte[message],al
mov byte[message+1], 128
mov word[crack_msg_pointer__length],1

sub dword[md5_hash_128bit],h0
sub dword[md5_hash_128bit+4],h1
sub dword[md5_hash_128bit+8],h2
sub dword[md5_hash_128bit+12],h3

push esi
lea esi,[start_cracking_msg]
call write
pop esi

hash_md5_inc_msg:
		;######################################################################
		;**********************************************************************
		;**********                 MD5-Algorithmus                           **********
		;**********************************************************************
		;######################################################################
		;+++++++++++++crack_round1:+++++++++++++++
		;***************0***************
				mov eax, h0
				mov ebx, h1
				mov ecx, h2
				mov edx, h3
			
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, ebx         ;### d xor (b and (c xor d)); ###
				xor edi, edx         ;################################
				add eax, edi

				add eax, [message+0*4]	;###### add Message[k[cx]] ######
				add eax, 3614090360		;########## add T[cx] ###########
				rol eax, 7                 ;################################
				add eax, ebx
		;***************0***************
		;***************1***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, eax         ;### d xor (b and (c xor d)); ###
				xor edi, ecx         ;################################
				add edx, edi

				add edx, [message+1*4]	;###### add Message[k[cx]] ######
				add edx, 3905402710		;########## add T[cx] ###########
				rol edx, 12                 ;################################
				add edx, eax
		;***************1***************
		;***************2***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, edx         ;### d xor (b and (c xor d)); ###
				xor edi, ebx         ;################################
				add ecx, edi

				add ecx, [message+2*4]	;###### add Message[k[cx]] ######
				add ecx, 606105819		;########## add T[cx] ###########
				rol ecx, 17                 ;################################
				add ecx, edx
		;***************2***************
		;***************3***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ecx         ;### d xor (b and (c xor d)); ###
				xor edi, eax         ;################################
				add ebx, edi

				add ebx, [message+3*4]	;###### add Message[k[cx]] ######
				add ebx, 3250441966		;########## add T[cx] ###########
				rol ebx, 22                 ;################################
				add ebx, ecx
		;***************3***************
		;***************4***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, ebx         ;### d xor (b and (c xor d)); ###
				xor edi, edx         ;################################
				add eax, edi

				add eax, [message+4*4]	;###### add Message[k[cx]] ######
				add eax, 4118548399		;########## add T[cx] ###########
				rol eax, 7                 ;################################
				add eax, ebx
		;***************4***************
		;***************5***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, eax         ;### d xor (b and (c xor d)); ###
				xor edi, ecx         ;################################
				add edx, edi

				add edx, [message+5*4]	;###### add Message[k[cx]] ######
				add edx, 1200080426		;########## add T[cx] ###########
				rol edx, 12                 ;################################
				add edx, eax
		;***************5***************
		;***************6***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, edx         ;### d xor (b and (c xor d)); ###
				xor edi, ebx         ;################################
				add ecx, edi

				add ecx, [message+6*4]	;###### add Message[k[cx]] ######
				add ecx, 2821735955		;########## add T[cx] ###########
				rol ecx, 17                 ;################################
				add ecx, edx
		;***************6***************
		;***************7***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ecx         ;### d xor (b and (c xor d)); ###
				xor edi, eax         ;################################
				add ebx, edi

				add ebx, [message+7*4]	;###### add Message[k[cx]] ######
				add ebx, 4249261313		;########## add T[cx] ###########
				rol ebx, 22                 ;################################
				add ebx, ecx
		;***************8***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, ebx         ;### d xor (b and (c xor d)); ###
				xor edi, edx         ;################################
				add eax, edi

				add eax, [message+8*4]	;###### add Message[k[cx]] ######
				add eax, 1770035416		;########## add T[cx] ###########
				rol eax, 7                 ;################################
				add eax, ebx
		;***************8***************
		;***************9***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, eax         ;### d xor (b and (c xor d)); ###
				xor edi, ecx         ;################################
				add edx, edi

				add edx, [message+9*4]	;###### add Message[k[cx]] ######
				add edx, 2336552879		;########## add T[cx] ###########
				rol edx, 12                 ;################################
				add edx, eax
		;***************9***************
		;***************10***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, edx         ;### d xor (b and (c xor d)); ###
				xor edi, ebx         ;################################
				add ecx, edi

				add ecx, [message+10*4]	;###### add Message[k[cx]] ######
				add ecx, 4294925233		;########## add T[cx] ###########
				rol ecx, 17                 ;################################
				add ecx, edx
		;***************10***************
		;***************11***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ecx         ;### d xor (b and (c xor d)); ###
				xor edi, eax         ;################################
				add ebx, edi

				add ebx, [message+11*4]	;###### add Message[k[cx]] ######
				add ebx, 2304563134		;########## add T[cx] ###########
				rol ebx, 22                 ;################################
				add ebx, ecx
		;***************11***************
		;***************12***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, ebx         ;### d xor (b and (c xor d)); ###
				xor edi, edx         ;################################
				add eax, edi

				add eax, [message+12*4]	;###### add Message[k[cx]] ######
				add eax, 1804603682		;########## add T[cx] ###########
				rol eax, 7                 ;################################
				add eax, ebx
		;***************12***************
		;***************13***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, eax         ;### d xor (b and (c xor d)); ###
				xor edi, ecx         ;################################
				add edx, edi

				add edx, [message+13*4]	;###### add Message[k[cx]] ######
				add edx, 4254626195		;########## add T[cx] ###########
				rol edx, 12                 ;################################
				add edx, eax
		;***************13***************
		;***************14***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, edx         ;### d xor (b and (c xor d)); ###
				xor edi, ebx         ;################################
				add ecx, edi

				add ecx, [message+14*4]	;###### add Message[k[cx]] ######
				add ecx, 2792965006		;########## add T[cx] ###########
				rol ecx, 17                 ;################################
				add ecx, edx
		;***************14***************
		;***************15***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ecx         ;### d xor (b and (c xor d)); ###
				xor edi, eax         ;################################
				add ebx, edi

				add ebx, [message+15*4]	;###### add Message[k[cx]] ######
				add ebx, 1236535329		;########## add T[cx] ###########
				rol ebx, 22                 ;################################
				add ebx, ecx
		;***************15***************
		;+++++++++++++crack_round2:+++++++++++++++
		;***************16***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, edx         ;### c xor (d and (b xor c)); ###
				xor edi, ecx         ;################################
				add eax, edi

				add eax, [message+1*4]	;###### add Message[k[cx]] ######
				add eax, 4129170786		;########## add T[cx] ###########
				rol eax, 5                 ;################################
				add eax, ebx
		;***************16***************
		;***************17***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, ecx         ;### c xor (d and (b xor c)); ###
				xor edi, ebx         ;################################
				add edx, edi

				add edx, [message+6*4]	;###### add Message[k[cx]] ######
				add edx, 3225465664		;########## add T[cx] ###########
				rol edx, 9                 ;################################
				add edx, eax
		;***************17***************
		;***************18***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ebx         ;### c xor (d and (b xor c)); ###
				xor edi, eax         ;################################
				add ecx, edi

				add ecx, [message+11*4]	;###### add Message[k[cx]] ######
				add ecx, 643717713		;########## add T[cx] ###########
				rol ecx, 14                 ;################################
				add ecx, edx
		;***************18***************
		;***************19***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, eax         ;### c xor (d and (b xor c)); ###
				xor edi, edx         ;################################
				add ebx, edi

				add ebx, [message+0*4]	;###### add Message[k[cx]] ######
				add ebx, 3921069994		;########## add T[cx] ###########
				rol ebx, 20                 ;################################
				add ebx, ecx
		;***************19***************
		;***************20***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, edx         ;### c xor (d and (b xor c)); ###
				xor edi, ecx         ;################################
				add eax, edi

				add eax, [message+5*4]	;###### add Message[k[cx]] ######
				add eax, 3593408605		;########## add T[cx] ###########
				rol eax, 5                 ;################################
				add eax, ebx
		;***************20***************
		;***************21***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, ecx         ;### c xor (d and (b xor c)); ###
				xor edi, ebx         ;################################
				add edx, edi

				add edx, [message+10*4]	;###### add Message[k[cx]] ######
				add edx, 38016083		;########## add T[cx] ###########
				rol edx, 9                 ;################################
				add edx, eax
		;***************21***************
		;***************22***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ebx         ;### c xor (d and (b xor c)); ###
				xor edi, eax         ;################################
				add ecx, edi

				add ecx, [message+15*4]	;###### add Message[k[cx]] ######
				add ecx, 3634488961		;########## add T[cx] ###########
				rol ecx, 14                 ;################################
				add ecx, edx
		;***************22***************
		;***************23***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, eax         ;### c xor (d and (b xor c)); ###
				xor edi, edx         ;################################
				add ebx, edi

				add ebx, [message+4*4]	;###### add Message[k[cx]] ######
				add ebx, 3889429448		;########## add T[cx] ###########
				rol ebx, 20                 ;################################
				add ebx, ecx
		;***************23***************
		;***************24***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, edx         ;### c xor (d and (b xor c)); ###
				xor edi, ecx         ;################################
				add eax, edi

				add eax, [message+9*4]	;###### add Message[k[cx]] ######
				add eax, 568446438		;########## add T[cx] ###########
				rol eax, 5                 ;################################
				add eax, ebx
		;***************24***************
		;***************25***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, ecx         ;### c xor (d and (b xor c)); ###
				xor edi, ebx         ;################################
				add edx, edi

				add edx, [message+14*4]	;###### add Message[k[cx]] ######
				add edx, 3275163606		;########## add T[cx] ###########
				rol edx, 9                 ;################################
				add edx, eax
		;***************25***************
		;***************26***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ebx         ;### c xor (d and (b xor c)); ###
				xor edi, eax         ;################################
				add ecx, edi

				add ecx, [message+3*4]	;###### add Message[k[cx]] ######
				add ecx, 4107603335		;########## add T[cx] ###########
				rol ecx, 14                 ;################################
				add ecx, edx
		;***************26***************
		;***************27***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, eax         ;### c xor (d and (b xor c)); ###
				xor edi, edx         ;################################
				add ebx, edi

				add ebx, [message+8*4]	;###### add Message[k[cx]] ######
				add ebx, 1163531501		;########## add T[cx] ###########
				rol ebx, 20                 ;################################
				add ebx, ecx
		;***************27***************
		;***************28***************
				mov edi, ebx
				xor edi, ecx         ;################################
				and edi, edx         ;### c xor (d and (b xor c)); ###
				xor edi, ecx         ;################################
				add eax, edi

				add eax, [message+13*4]	;###### add Message[k[cx]] ######
				add eax, 2850285829		;########## add T[cx] ###########
				rol eax, 5                 ;################################
				add eax, ebx
		;***************28***************
		;***************29***************
				mov edi, eax
				xor edi, ebx         ;################################
				and edi, ecx         ;### c xor (d and (b xor c)); ###
				xor edi, ebx         ;################################
				add edx, edi

				add edx, [message+2*4]	;###### add Message[k[cx]] ######
				add edx, 4243563512		;########## add T[cx] ###########
				rol edx, 9                 ;################################
				add edx, eax
		;***************29***************
		;***************30***************
				mov edi, edx
				xor edi, eax         ;################################
				and edi, ebx         ;### c xor (d and (b xor c)); ###
				xor edi, eax         ;################################
				add ecx, edi

				add ecx, [message+7*4]	;###### add Message[k[cx]] ######
				add ecx, 1735328473		;########## add T[cx] ###########
				rol ecx, 14                 ;################################
				add ecx, edx
		;***************30***************
		;***************31***************
				mov edi, ecx
				xor edi, edx         ;################################
				and edi, eax         ;### c xor (d and (b xor c)); ###
				xor edi, edx         ;################################
				add ebx, edi

				add ebx, [message+12*4]	;###### add Message[k[cx]] ######
				add ebx, 2368359562		;########## add T[cx] ###########
				rol ebx, 20                 ;################################
				add ebx, ecx
		;***************31***************
		;+++++++++++++crack_round2:+++++++++++++++
		;***************32***************
				mov edi, ebx		;################################
				xor edi, ecx		;######## b xor c xor d; ########
				xor edi, edx		;################################
				add eax, edi

				add eax, [message+5*4]	;###### add Message[k[cx]] ######
				add eax, 4294588738		;########## add T[cx] ###########
				rol eax, 4                 ;################################
				add eax, ebx
		;***************32***************
		;***************33***************
				mov edi, eax		;################################
				xor edi, ebx		;######## b xor c xor d; ########
				xor edi, ecx		;################################
				add edx, edi

				add edx, [message+8*4]	;###### add Message[k[cx]] ######
				add edx, 2272392833		;########## add T[cx] ###########
				rol edx, 11                 ;################################
				add edx, eax
		;***************33***************
		;***************34***************
				mov edi, edx		;################################
				xor edi, eax		;######## b xor c xor d; ########
				xor edi, ebx		;################################
				add ecx, edi

				add ecx, [message+11*4]	;###### add Message[k[cx]] ######
				add ecx, 1839030562		;########## add T[cx] ###########
				rol ecx, 16                 ;################################
				add ecx, edx
		;***************34***************
		;***************35***************
				mov edi, ecx		;################################
				xor edi, edx		;######## b xor c xor d; ########
				xor edi, eax		;################################
				add ebx, edi

				add ebx, [message+14*4]	;###### add Message[k[cx]] ######
				add ebx, 4259657740		;########## add T[cx] ###########
				rol ebx, 23                 ;################################
				add ebx, ecx
		;***************35***************
		;***************36***************
				mov edi, ebx		;################################
				xor edi, ecx		;######## b xor c xor d; ########
				xor edi, edx		;################################
				add eax, edi

				add eax, [message+1*4]	;###### add Message[k[cx]] ######
				add eax, 2763975236		;########## add T[cx] ###########
				rol eax, 4                 ;################################
				add eax, ebx
		;***************36***************
		;***************37***************
				mov edi, eax		;################################
				xor edi, ebx		;######## b xor c xor d; ########
				xor edi, ecx		;################################
				add edx, edi

				add edx, [message+4*4]	;###### add Message[k[cx]] ######
				add edx, 1272893353		;########## add T[cx] ###########
				rol edx, 11                 ;################################
				add edx, eax
		;***************37***************
		;***************38***************
				mov edi, edx		;################################
				xor edi, eax		;######## b xor c xor d; ########
				xor edi, ebx		;################################
				add ecx, edi

				add ecx, [message+7*4]	;###### add Message[k[cx]] ######
				add ecx, 4139469664		;########## add T[cx] ###########
				rol ecx, 16                 ;################################
				add ecx, edx
		;***************38***************
		;***************39***************
				mov edi, ecx		;################################
				xor edi, edx		;######## b xor c xor d; ########
				xor edi, eax		;################################
				add ebx, edi

				add ebx, [message+10*4]	;###### add Message[k[cx]] ######
				add ebx, 3200236656		;########## add T[cx] ###########
				rol ebx, 23                 ;################################
				add ebx, ecx
		;***************39***************
		;***************40***************
				mov edi, ebx		;################################
				xor edi, ecx		;######## b xor c xor d; ########
				xor edi, edx		;################################
				add eax, edi

				add eax, [message+13*4]	;###### add Message[k[cx]] ######
				add eax, 681279174		;########## add T[cx] ###########
				rol eax, 4                 ;################################
				add eax, ebx
		;***************40***************
		;***************41***************
				mov edi, eax		;################################
				xor edi, ebx		;######## b xor c xor d; ########
				xor edi, ecx		;################################
				add edx, edi

				add edx, [message+0*4]	;###### add Message[k[cx]] ######
				add edx, 3936430074		;########## add T[cx] ###########
				rol edx, 11                 ;################################
				add edx, eax
		;***************41***************
		;***************42***************
				mov edi, edx		;################################
				xor edi, eax		;######## b xor c xor d; ########
				xor edi, ebx		;################################
				add ecx, edi

				add ecx, [message+3*4]	;###### add Message[k[cx]] ######
				add ecx, 3572445317		;########## add T[cx] ###########
				rol ecx, 16                 ;################################
				add ecx, edx
		;***************42***************
		;***************43***************
				mov edi, ecx		;################################
				xor edi, edx		;######## b xor c xor d; ########
				xor edi, eax		;################################
				add ebx, edi

				add ebx, [message+6*4]	;###### add Message[k[cx]] ######
				add ebx, 76029189		;########## add T[cx] ###########
				rol ebx, 23                 ;################################
				add ebx, ecx
		;***************43***************
		;***************44***************
				mov edi, ebx		;################################
				xor edi, ecx		;######## b xor c xor d; ########
				xor edi, edx		;################################
				add eax, edi

				add eax, [message+9*4]	;###### add Message[k[cx]] ######
				add eax, 3654602809		;########## add T[cx] ###########
				rol eax, 4                 ;################################
				add eax, ebx
		;***************44***************
		;***************45***************
				mov edi, eax		;################################
				xor edi, ebx		;######## b xor c xor d; ########
				xor edi, ecx		;################################
				add edx, edi

				add edx, [message+12*4]	;###### add Message[k[cx]] ######
				add edx, 3873151461		;########## add T[cx] ###########
				rol edx, 11                 ;################################
				add edx, eax
		;***************45***************
		;***************46***************
				mov edi, edx		;################################
				xor edi, eax		;######## b xor c xor d; ########
				xor edi, ebx		;################################
				add ecx, edi

				add ecx, [message+15*4]	;###### add Message[k[cx]] ######
				add ecx, 530742520		;########## add T[cx] ###########
				rol ecx, 16                 ;################################
				add ecx, edx
		;***************46***************
		;***************47***************
				mov edi, ecx		;################################
				xor edi, edx		;######## b xor c xor d; ########
				xor edi, eax		;################################
				add ebx, edi

				add ebx, [message+2*4]	;###### add Message[k[cx]] ######
				add ebx, 3299628645		;########## add T[cx] ###########
				rol ebx, 23                 ;################################
				add ebx, ecx
		;***************47***************
		;+++++++++++++crack_round2:+++++++++++++++
		;***************48***************
				mov edi, edx		;################################
				not edi				;################################
				or  edi, ebx		;##### c xor (b or (not d)); ####
				xor edi, ecx		;################################
				add eax, edi

				add eax, [message+0*4]	;###### add Message[k[cx]] ######
				add eax, 4096336452		;########## add T[cx] ###########
				rol eax, 6                 ;################################
				add eax, ebx
		;***************48***************
		;***************49***************
				mov edi, ecx		;################################
				not edi				;################################
				or  edi, eax		;##### c xor (b or (not d)); ####
				xor edi, ebx		;################################
				add edx, edi

				add edx, [message+7*4]	;###### add Message[k[cx]] ######
				add edx, 1126891415		;########## add T[cx] ###########
				rol edx, 10                 ;################################
				add edx, eax
		;***************49***************
		;***************50***************
				mov edi, ebx		;################################
				not edi				;################################
				or  edi, edx		;##### c xor (b or (not d)); ####
				xor edi, eax		;################################
				add ecx, edi

				add ecx, [message+14*4]	;###### add Message[k[cx]] ######
				add ecx, 2878612391		;########## add T[cx] ###########
				rol ecx, 15                 ;################################
				add ecx, edx
		;***************50***************
		;***************51***************
				mov edi, eax		;################################
				not edi				;################################
				or  edi, ecx		;##### c xor (b or (not d)); ####
				xor edi, edx		;################################
				add ebx, edi

				add ebx, [message+5*4]	;###### add Message[k[cx]] ######
				add ebx, 4237533241		;########## add T[cx] ###########
				rol ebx, 21                 ;################################
				add ebx, ecx
		;***************51***************
		;***************52***************
				mov edi, edx		;################################
				not edi				;################################
				or  edi, ebx		;##### c xor (b or (not d)); ####
				xor edi, ecx		;################################
				add eax, edi

				add eax, [message+12*4]	;###### add Message[k[cx]] ######
				add eax, 1700485571		;########## add T[cx] ###########
				rol eax, 6                 ;################################
				add eax, ebx
		;***************52***************
		;***************53***************
				mov edi, ecx		;################################
				not edi				;################################
				or  edi, eax		;##### c xor (b or (not d)); ####
				xor edi, ebx		;################################
				add edx, edi

				add edx, [message+3*4]	;###### add Message[k[cx]] ######
				add edx, 2399980690		;########## add T[cx] ###########
				rol edx, 10                 ;################################
				add edx, eax
		;***************53***************
		;***************54***************
				mov edi, ebx		;################################
				not edi				;################################
				or  edi, edx		;##### c xor (b or (not d)); ####
				xor edi, eax		;################################
				add ecx, edi

				add ecx, [message+10*4]	;###### add Message[k[cx]] ######
				add ecx, 4293915773		;########## add T[cx] ###########
				rol ecx, 15                 ;################################
				add ecx, edx
		;***************54***************
		;***************55***************
				mov edi, eax		;################################
				not edi				;################################
				or  edi, ecx		;##### c xor (b or (not d)); ####
				xor edi, edx		;################################
				add ebx, edi

				add ebx, [message+1*4]	;###### add Message[k[cx]] ######
				add ebx, 2240044497		;########## add T[cx] ###########
				rol ebx, 21                 ;################################
				add ebx, ecx
		;***************55***************
		;***************56***************
				mov edi, edx		;################################
				not edi				;################################
				or  edi, ebx		;##### c xor (b or (not d)); ####
				xor edi, ecx		;################################
				add eax, edi

				add eax, [message+8*4]	;###### add Message[k[cx]] ######
				add eax, 1873313359		;########## add T[cx] ###########
				rol eax, 6                 ;################################
				add eax, ebx
		;***************56***************
		;***************57***************
				mov edi, ecx		;################################
				not edi				;################################
				or  edi, eax		;##### c xor (b or (not d)); ####
				xor edi, ebx		;################################
				add edx, edi

				add edx, [message+15*4]	;###### add Message[k[cx]] ######
				add edx, 4264355552		;########## add T[cx] ###########
				rol edx, 10                 ;################################
				add edx, eax
		;***************57***************
		;***************58***************
				mov edi, ebx		;################################
				not edi				;################################
				or  edi, edx		;##### c xor (b or (not d)); ####
				xor edi, eax		;################################
				add ecx, edi

				add ecx, [message+6*4]	;###### add Message[k[cx]] ######
				add ecx, 2734768916		;########## add T[cx] ###########
				rol ecx, 15                 ;################################
				add ecx, edx
		;***************58***************
		;***************59***************
				mov edi, eax		;################################
				not edi				;################################
				or  edi, ecx		;##### c xor (b or (not d)); ####
				xor edi, edx		;################################
				add ebx, edi

				add ebx, [message+13*4]	;###### add Message[k[cx]] ######
				add ebx, 1309151649		;########## add T[cx] ###########
				rol ebx, 21                 ;################################
				add ebx, ecx
		;***************59***************
		;***************60***************
				mov edi, edx		;################################
				not edi				;################################
				or  edi, ebx		;##### c xor (b or (not d)); ####
				xor edi, ecx		;################################
				add eax, edi

				add eax, [message+4*4]	;###### add Message[k[cx]] ######
				add eax, 4149444226		;########## add T[cx] ###########
				rol eax, 6                 ;################################
				add eax, ebx
		;***************60***************
		;***************61***************
				mov edi, ecx		;################################
				not edi				;################################
				or  edi, eax		;##### c xor (b or (not d)); ####
				xor edi, ebx		;################################
				add edx, edi

				add edx, [message+11*4]	;###### add Message[k[cx]] ######
				add edx, 3174756917		;########## add T[cx] ###########
				rol edx, 10                 ;################################
				add edx, eax
		;***************61***************
		;***************62***************
				mov edi, ebx		;################################
				not edi				;################################
				or  edi, edx		;##### c xor (b or (not d)); ####
				xor edi, eax		;################################
				add ecx, edi

				add ecx, [message+2*4]	;###### add Message[k[cx]] ######
				add ecx, 718787259		;########## add T[cx] ###########
				rol ecx, 15                 ;################################
				add ecx, edx
		;***************62***************
		;***************63***************
				mov edi, eax		;################################
				not edi				;################################
				or  edi, ecx		;##### c xor (b or (not d)); ####
				xor edi, edx		;################################
				add ebx, edi

				add ebx, [message+9*4]	;###### add Message[k[cx]] ######
				add ebx, 3951481745		;########## add T[cx] ###########
				rol ebx, 21                 ;################################
				add ebx, ecx
		;***************63***************
		;######################################################################
		;**********************************************************************
		;**********                 MD5-Algorithmus                           **********
		;**********************************************************************
		;######################################################################

;+++++ Compare the hashes +++++
	cmp eax, [md5_hash_128bit]
		jne hash_furher
	cmp ebx, [md5_hash_128bit+4]
		jne hash_furher
	cmp ecx, [md5_hash_128bit+8]
		jne hash_furher
	cmp edx, [md5_hash_128bit+12]
		jne hash_furher
;+++++ Compare the hashes +++++
;+++++ Only Hashed hashes come to this code +++++
		lea esi, [correct_hash]
		call write
		lea esi, [message]
		call write
		call break
		jmp end_hashing
;+++++ Only Hashed hashes come to this code +++++

;+++++ Increment Message +++++
hash_furher:
	inc dword[cracks_tested]
	mov bx,[crack_msg_pointer__length]
	inc_msg:
		lea edi,[esi+1]
		mov al,[message+bx-1]
		movzx ecx,byte[esi]
		repne scasb
		mov al,[edi]
		mov [message+bx-1],al
		or al,al
			jnz hash_md5_inc_msg
		mov al,[esi+1]
		mov [message+bx-1],al
		dec bx
		jnz inc_msg

	inc word[crack_msg_pointer__length]
	mov bx,[crack_msg_pointer__length]
	mov al,[esi+1]
	mov [message+bx-1],al
	mov byte[message+bx],80h
	add dword[message+56], 8

	jmp hash_md5_inc_msg
;+++++ Increment Message +++++
;*************** Cracking-main-loop ***************

;*************** Show Results ***************
end_hashing:
 push ds							;DS speichern
 xor ax,ax							;0 in ax speichern
 mov ds,ax							;	...um es in DS zu speichern
 CLI								;Interrupt deaktivieren
 mov eax,[int09h_save]				;gespeicherten Interrupt-Vektor nach eax holen
 mov [ds:4*9h],eax				;und dann auf stelle in der IVT zurückspeichern
 mov eax,[int08h_save]				;gespeicherten Interrupt-Vektor nach eax holen
 mov [es:4*8h],eax				;und dann auf stelle in der IVT zurückspeichern
 STI								;Interrupts aktivieren
 pop ds								;DS wiederherstellen

 call space_pressed
;*************** Show Results ***************

jmp read_choice_cracker_hasher
;########################################## MD5 Cracker ##########################################

;########################################## Vars ##########################################
r: db 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
   db 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
   db 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
   db 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21

k: db 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
   db 1, 6, 11, 0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12,
   db 5, 8, 11, 14, 1, 4, 7, 10, 13, 0, 3, 6, 9, 12, 15, 2,
   db 0, 7, 14, 5, 12, 3, 10, 1, 8, 15, 6, 13, 4, 11, 2, 9 

T: dd 3614090360, 3905402710, 606105819, 3250441966, 
   dd 4118548399, 1200080426, 2821735955, 4249261313, 
   dd 1770035416, 2336552879, 4294925233, 2304563134, 
   dd 1804603682, 4254626195, 2792965006, 1236535329, 
   dd 4129170786, 3225465664, 643717713, 3921069994, 
   dd 3593408605, 38016083, 3634488961, 3889429448, 
   dd 568446438, 3275163606, 4107603335, 1163531501, 
   dd 2850285829, 4243563512, 1735328473, 2368359562, 
   dd 4294588738, 2272392833, 1839030562, 4259657740, 
   dd 2763975236, 1272893353, 4139469664, 3200236656, 
   dd 681279174, 3936430074, 3572445317, 76029189, 
   dd 3654602809, 3873151461, 530742520, 3299628645, 
   dd 4096336452, 1126891415, 2878612391, 4237533241, 
   dd 1700485571, 2399980690, 4293915773, 2240044497, 
   dd 1873313359, 4264355552, 2734768916, 1309151649, 
   dd 4149444226, 3174756917, 718787259, 3951481745

int08h_save dd 0
int09h_save dd 0
higher_case db 26,"ABCDEFGHIJKLMNOPQRSTUVWXYZ",0,0
higher_case_num db 36,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",0,0
higher_case_num_special_chars db 51,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+= ",0,0
higher_case_all db 69,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+=~`[]{}|\:;",34,"'<>,.?/ ",0,0
lower_case db 26,"abcdefghijklmnopqrstuvwxyz",0,0
lower_case_num db 36,"abcdefghijklmnopqrstuvwxyz0123456789",0,0
lower_case_num_special_chars db 51,"abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+= ",0,0
lower_case_all db 69,"abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+=~`[]{}|\:;",34,"'<>,.?/ ",0,0
alphabet db 52,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",0,0
alphabet_num db 62,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",0,0
alphabet_num_special_chars db 77,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+= ",0,0
all_chars db 95,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+=~`[]{}|\:;",34,"'<>,.?/ ",0,0
charset_choice_msg db 13,10,"Please choose one of the following charsets, so that only the digits in this charsets will be tested; to save time.",13,10," As longer as the sets are, as much time will be needed!",13,10,13,10," [0] ABCDEFGHIJKLMNOPQRSTUVWXYZ",13,10," [1] ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",13,10," [2] ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+= ",13,10," [3] ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+=~`[]{}|\:;",34,"'<>,.?/ (",13,10," [4] abcdefghijklmnopqrstuvwxyz",13,10," [5] abcdefghijklmnopqrstuvwxyz0123456789",13,10," [6] abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+= ",13,10," [7] abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+=~`[]{}|\:;",34,"'<>,.?/ (",13,10," [8] ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",13,10," [9] ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",13,10," [a] ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+= ",13,10," [b] ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_+=~`[]{}|\:;",'"',"'<>,.?/ (",13,10,"CHOICE: ",0
only_choice_1_12 db 12,0,"0123456789ab",0
only_choice_012 db 3,0,"012",0
only_hex_digits db 22,0,"0123456789abcdefABCDEF",0
timer dd 0
cpu_clock_cycles_tmp dd 0,0
cpu_clock_cycles dd 0,0
start_cracking_msg db 13,10,13,10,"Cracking started.",13,10," [space] To show some information ",13,10," [ESC] To cancel cracking-process",0
timer_tmp dd 0
cpu_cycles_per_hash db 13,10,"The CPU-clocks per calculating 1 hash should be -> ",0
hashes_per_sec db 13,10,"If the timer works correct, the hashes-per-second-rate should be -> ",0
cpu_speed_by_timer db 13,10,"If the timer works correct, your CPU-speed should be -> ",0
time_gone_by_now db 13,10,"Time gone by now (Source = Timer-Cycles) -> ",0
cpu_cycles_msg db 13,10,"CPU-Steps needed until now -> ",0
current_message db 13,10,"current testing message -> ",0
cycles_msg db 13,10,"Timer-cycles(20Hz) needed until now to calculate md5-hashes -> ",0
end_cracking db "User canceled or no hash found...",13,10,0
tested_cracks_msg db 13,10,"Amount of tested strings -> ",0
cracks_tested dd 0
start_time dd 0
crack_msg_pointer__length dw 0
message times 64 db 0
md5_hash_128bit dd 0,0,0,0
welcome db 13,10,13,10,"Thx for using my Assembler-MD5-Hash-Tools! :)",13,10,"Please visit also maxinator.tk or maxinator.meugster.net for more information.",13,10,"Unfortunately my homepage is just now only available in german :(",13,10,"What do you want to do?",13,10,"   [0] Encrypt String",13,10,"   [1] Crack MD5-Hash",13,10,"   [2] EXIT",13,10,"CHOICE: ",0
hasher db 13,10,"Please type the string you want to encrypt: ",0
cracker db 13,10,"Please type the 128 Bit long Hash in Hex: ",0 
result db "MD5-Hash -> ",0
correct_hash db 13,10,"HASH CRACKED !!! -> ",0
;########################################## Vars ##########################################

;***********************************************************************
;********* readln es:di=string ecx:laenge ds:esi allowed signs...  *********
;********* length of string defined in first word of string *********
;********* exp.: allowed_chars db 4,0,"0123" -> will only allow digits 0-3 *********
;***********************************************************************
readln_special:
	pushad							;Register sichern
	push edi						;edi sichern um es am ende der Prozedur wiederherstellen zu können
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
			
			dec edi					;den pointer für stosx auf das letzte zeichen stellen
			mov byte[es:edi],0		;und das Byte löschen ohne den Pointer zu bewegen
			jmp .readln_loop		;warte auf nächstes zeichen
		.nicht_zurueck:
		cmp al,13					;testen, ob das zeichen enter ist
		jne .not_enter				;wenn nicht, dann überspringe folgendes
			cmp ecx,ebp				;teste ob die anz. der eingegeben zeichen gleich der ist, die man eingeben muss
				je .readln_end		;wenn der nutzer genug zeichen eingegeben hat, leite end-sequenz ein
				jmp .readln_loop	;ansonsten warte auf nächstes zeichen
		.not_enter:
		cmp ecx,ebp					;vergleiche, ob der nutzer die per cx bestimmte anzahl vonzeichen schon eingegeben hat
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
		mov al,80h					;setze auf Bit nach dem letzten Byte 1, da das MD5 so verlangt
		stosb						;und speichere dies im string
		shl ecx,3					;multipliziere die Anz. der Zeichen mit 8 um die Anzahl der Bits zu bekommen
		pop edi						;stelle edi, also der anfang des Strings wieder her
		mov [edi+56],ecx			;um dann in das QWord am Ende des Strings die Länge diesen in Bits abzulegen
	popad							;stelle alle register wieder her
	ret								;und springe zurück zum aufrufsort

;***********************************************************************
;************** readln es:di=string ecx:laenge **************
;***********************************************************************
readln:
	pushad						;Register sichern
	push edi					;edi sichern um es am ende der Prozedur wiederherstellen zu können
	mov ebp, ecx				;zum Abgleich ecx auf ebp zwischensichern
	xor ecx, ecx				;ecx löschen, damit es von 0 an hochzählen kann ... bis zu ebp
	readln_loop:
		xor ax,ax				;ax löschen, damit bei int16h funtion 0h gestartet wird
		int 16h					;int 16h aufrufen -> auf Tastendruck warten :) -> das ist die zentral funktion von readln
		or al,al				;al auf asciizeichen prüfen
			jz readln_loop		;wenn al=0, dann warte auf nächstes zeichen
		cmp al,8				;al auf Backspace prüfen
		jne nicht_zurueck		;wenn es keiner ist, überspringe die Backspace-Prozedure
			cmp ebp,ecx			;wenn cx jedoch immer noch gleich dem eingabewert ist, also wenn kein zeichen mehr zum backspacen da ist
				je readln_loop	;dann warte auf das nächste zeichen
			dec ecx				;dekrementiere cx, da ja in zeichen gelöscht wird
			
			mov ah,0eh			;verschiebe auf ah 0eh - telerype output
			mov bx,2			;auf bx die farbe
			int 10h				;und gib den backspace aus -> bios geht automatisch ein schritt zurück
			xor al,al			;löscht das letzte zeichen jedoch nicht, weswegen auf al 0 kommt
			int 10h				;und das dann auf dem platz des letzten zeichens ausgegeben wird
			mov al,08h			;auf al Backspace schieben
			int 10h				;um es auszugeben und somit an stelle des gelöschten zeichens zu gelangen
			
			dec edi				;den pointer für stosx auf das letzte zeichen stellen
			mov byte[es:edi],0	;und das Byte löschen ohne den Pointer zu bewegen
				jmp readln_loop	;warte auf nächstes zeichen
		nicht_zurueck:
		cmp al,13				;testen, ob das zeichen enter ist
			je readln_end		;wenn Eingabe gleich Return ist, beende die readln-prozedure
		cmp ecx,ebp				;vergleiche, ob der nutzer die per cx bestimmte anzahl vonzeichen schon eingegeben hat
			jz readln_loop		;wenn ja, dann nehme das zeichen nicht an und warte auf nächstes zeichen, was irgendwann backspace oder enter sein sollte
		
		stosb					;wenn das zeichen nicht irgendwie vorher abgefangen wurden ist und hier gelandet ist, speichere es im Input-Stream-String
		mov ah,0eh				;auf ah 0eh für teletype-output
		mov bx,2				;auf bx farbe
		int 10h					;und nun gib das zeichen aus, was alle test bestanden hat
	inc ecx						;inkrementiere ecx, da ja nun ein zeichen mehr in input-stream ist
	jmp readln_loop				;und warte auf nächstes zeichen
readln_end:
	mov al,80h					;setze auf Bit nach dem letzten Byte 1, da das MD5 so verlangt
	stosb						;und speichere dies im string
	shl ecx,3					;multipliziere die Anz. der Zeichen mit 8 um die Anzahl der Bits zu bekommen
	pop edi						;stelle edi, also der anfang des Strings wieder her
	mov [edi+56],ecx			;um dann in das QWord am Ende des Strings die Länge diesen in Bits abzulegen
popad							;stelle alle register wieder her
ret								;und springe zurück zum aufrufsort

	;***********************************************************************
	;*** Zahlausgabe speziell für MD5-Hash eax:zahl bx:basis Ausgabe:zahl ***
	;***********************************************************************
	zahlausgabe_byte:
		pushad
		mov ecx,2
		mov ebx,16
	.loop_byte:
		xor edx,edx
		div ebx
		push dx
		loop .loop_byte

		mov ax,0900h
		mov cx,2
		mov bx,4
		int 10h

		mov ax,0e00h
		xor bx,bx
		mov ecx,2
	.loop_ausgabe_byte:
		pop dx
		cmp dx,10
		jb .zahl_byte
			add dl,39
		.zahl_byte:
			add dl,48
			mov al,dl
			int 10h
		loop .loop_ausgabe_byte
		popad
		ret

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
;***************************** timer-int_handler *****************************
;***********************************************************************
timer_handler:
	push ax				;speichere ax
	inc dword[timer]	;erhöhe timer um einen Takt
	mov al,20h			;schiebe auf al OCW2
	out 20h,al			;und sende das als EOI dem PIC1
	pop ax				;stelle ax wieder her
	iret				;und beende Interrupt

;***********************************************************************
;**************** speed-trimmed md5-crack-own keyboard-drv ****************
;***********************************************************************
md5_crack_keyb_drv:
	pushad										;Register speichern
	push es										;	~
	push ds										;	~
	
	mov ax,cs									;Auf ax,cs kopieren um segmentregister zu aktualisieren
	mov ds,ax									;segmentregister aktualisieren
	mov es,ax									;	~
	in al,64h									;statusbyte des keyboards abfragen
	test al,00000001b							;prüfen ob im kb-buffer ein zeichen liegt
	jnz .save_scancode							;	...wenn ja hole und speichere es
		jmp no_key							;	...ansonsten leite ende-sequenz ein
	.save_scancode:
		in al,60h								;	...ansonsten hole dieses Zeichen

	cmp al,1
		je esc_pressed
	cmp al,57
		jne jmp_no_key
		call space_pressed
	jmp_no_key:
	jmp no_key
	
		esc_pressed:
			pop ds							;Register wiederherstellen
			pop es							;	~
			mov al,20h						;kopiere auf al das byte für den EOI
			out 20h,al						;sende EOI
			popad							;	~
			add esp,6						;bewege stackpointer auf die stelle, wo er wäre, wenn iret ausgeführt worden wäre
			push cs				;dann hole word 2 vor
			push end_hashing				;und hole word 3 vor, sodass die von iret gepushten cs und ip für retf vorne sind, die gepushten flags jedoch nicht, da die nicht benötigt werden
			retf		
			
		space_pressed:
			rdtsc
			sub edx,[cpu_clock_cycles+4]
			sub eax,[cpu_clock_cycles]
			mov [cpu_clock_cycles_tmp+4],edx
			mov [cpu_clock_cycles_tmp],eax
			
			mov eax,[timer]
			mov [timer_tmp],eax
			
			call break
			lea esi,[current_message]
			call write
			lea esi,[message]
			mov ecx,[crack_msg_pointer__length]
			mov ax,0920h
			mov bx,0002h
			int 10h
			mov ah,0Eh			; Funktion 0x0E	
			.loop:	
				lodsb			; Byte laden
				int 10h			; schreiben
				loop .loop		; Nästes Byte
			.end:
			
			mov eax,[timer_tmp]
			sub eax,[start_time]
			lea esi,[cycles_msg]
			call write
			mov ebx, 10
			call zahlausgabe
			
			lea esi,[time_gone_by_now]	;schiebe auf esi den Offset der Nachricht der vergangenen Zeit
			call write					;gib diese aus
			xor edx,edx			;lösche edx, da nur in eax die cycle, die bisher vergangen sind gespeichert sind
			mov ebx,20			;schiebe auf ebx 20
				div ebx			;und dividiere eax damit, da der PIT auf 20Hz gestellt ist -> in eax liegen sekunden
			xor edx,edx			;lösche edx, da hier rest der division ist
			mov ebx,60			;auf ebx 60 schieben
				div ebx			;um eax damit zu teilen, sodass in edx sekunden liegn und in eax minuten
			push edx			;speichere sekunden
			xor edx,edx			;lösche edx, da da divisionsrest liegt
			mov ebx,60			;schiebe auf ebx 60
				div ebx			;teile eax mit 60, sodass in edx minuten und in eax stunden liegen
			push edx			;speichere Minuten
			xor edx,edx			;lösche Divisionsrest
			mov ebx,24			;schiebe auf ebx 24
				div ebx			;und teile eax damit, sodass in edx die Stunden und in eax die Tage liegen
			push edx			;speichere die Tage
			
			mov ebx,10			;schiebe auf ebx die basis für das Dezimalsystem
			call zahlausgabe	;und geb die Tage aus
			mov ax,0e3ah		;dann auf ah 0eh(teletypeoutput) und 3ah als zeichen auf al
			int 10h				;gib 3ah, also ":" aus
			pop eax				;stelle Stunden wieder her
			call zahlausgabe	;und gib diese aus
			mov ax,0e3ah		;dann auf ah 0eh(teletypeoutput) und 3ah als zeichen auf al
			int 10h				;gib 3ah, also ":" aus
			pop eax				;stelle Minuten wieder her
			call zahlausgabe	;und gib diese aus
			mov ax,0e3ah		;dann auf ah 0eh(teletypeoutput) und 3ah als zeichen auf al
			int 10h				;gib 3ah, also ":" aus
			pop eax				;stelle Sekunden wieder her
			call zahlausgabe	;und gib diese aus
			
			mov eax,[cpu_clock_cycles_tmp]
			mov edx,[cpu_clock_cycles_tmp+4]
			lea esi,[cpu_cycles_msg]
			call write
			mov ebx, 16
			xchg eax,edx
			or eax,eax
			jz rdtsc_high_is_zero
				call zahlausgabe
			rdtsc_high_is_zero:
			xchg eax,edx
				call zahlausgabe
			
			lea esi,[cpu_speed_by_timer]
			call write
			mov eax,[cpu_clock_cycles_tmp]
			mov edx,[cpu_clock_cycles_tmp+4]
			mov ebx,[timer_tmp]
			sub ebx,[start_time]
			div ebx
			mov ebx,20
			mul ebx
			mov ebx,10
			call zahlausgabe
			
			lea esi,[hashes_per_sec]
			call write
			mov eax,[cracks_tested]
			mov ebx,20
			mul ebx
			mov ebx,[timer_tmp]
			sub ebx,[start_time]
			div ebx
			mov ebx,10
			call zahlausgabe
			
			lea esi,[tested_cracks_msg]
			call write
			mov eax, [cracks_tested]
			mov ebx, 10
			call zahlausgabe
			
			lea esi,[cpu_cycles_per_hash]
			call write
			mov eax,[cpu_clock_cycles_tmp]
			mov edx,[cpu_clock_cycles_tmp+4]
			mov ecx,[cracks_tested]
			div ecx
			mov ebx, 10
			call zahlausgabe
			ret
	no_key:
			
	pop ds									;Register wiederherstellen
	pop es									;	~
	mov al,20h								;kopiere auf al das byte für den EOI
	out 20h,al								;sende EOI
	popad									;	~
	iret
