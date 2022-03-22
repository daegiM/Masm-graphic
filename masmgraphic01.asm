




;--- this is a 16bit sample for DOS. To create a simple DOS 16bit
;--- real-mode binary enter:
;---   JWasm -mz Dos2.asm
;--- or, if a linker is to be used:
;---   JWasm Dos2.asm
;---   wlink format dos file Dos2.obj

;--- To debug the sample with MS CodeView enter
;---   JWasm -Zi Dos2.asm
;---   link /CO Dos2.obj;
;---   cv Dos2.exe

;--- Optionally, the module can be linked as a DPMI 16bit protected-mode
;--- application. There are 2 ways to achieve this:
;--- 1. use Borland DOS extender:
;---   JWasm Dos2.asm
;---   tlink /Tx Dos2.obj;
;--- The resulting binary will need Borland's RTM.EXE and DPMI16BI.OVL.
;--- [To debug the application with TD run "JWasm /Zd Dos2.asm" and
;--- then run "tlink /Tx /v Dos2.obj".]
;--- 2. use HX DOS extender:
;---   JWasm Dos2.asm
;---   wlink format windows file Dos2.obj op stub=hdld16.bin
;---   patchne Dos2.exe
;--- The result is a 16bit DPMI application which includes a DPMI host.
;--- [To get files HDLD16.BIN and PATCHNE.EXE download HXDEV16.ZIP].

    .model small
    .stack 1024

    .data

;text db "Hello,fantastic world!",13,10,'$'

;	org $+1024


FontColor 		db 01010101b		
;FontColor 		db 01010101b	

UserRam db 256 DUP (0)
	
	
CursorX			equ UserRam
CursorY			equ UserRam+1

MonitorBak_AX 	equ UserRam+2
MonitorBak_F	equ UserRam+4
MonitorBak_IP   equ UserRam+6
MonitorBak_ES   equ UserRam+8
MonitorBak_DS   equ UserRam+10

    .code

	mov ax, @data		;ES points to our Data segment
	mov ds, ax
	
	mov ax, @code		;DS points to our Code segment
    mov es, ax


	call ScreenInit
	
	call domonitor
	
	

		push ds
		push es
			mov bh,6	;X
			mov bl,11	;Y
			call getscreenpos

				mov ch,144	;Width
			mov cl,144		;Height
			
			mov ax, @code
			mov ds, ax
			mov si,offset BitmapTest ;DS:SI = Source Bitmap
DrawBitmap_Yagain:
			push di
			push cx
DrawBitmap_Xagain:				
					movsb				;DS:SI -> ES:DI
				dec ch
				jnz DrawBitmap_Xagain
			pop cx
			pop di
			call GetScreenNextLine
			inc bl
			dec cl
			jnz DrawBitmap_Yagain
		pop es
		pop ds

	
	mov ax,@code
	mov ds,ax
	mov si,offset palette
	xor ax,ax
	
Paletteagain:
	mov dx,ds:[si]
	call SetPalette ;Set Color AL to DX (-GRB)
	inc si			;Move down two bytes
	inc si
	inc ax
	cmp ax,16		;Are we done?
	jnz Paletteagain
	
	jmp $
	



GetScreenPos:	;BH,BL = X,Y	Returns ES:DI=Destination
	push bx
	push ax
		xor ax,ax
		mov al,bh
		mov di,ax
		xor ax,ax
		mov al,bl
		mov bx,320
		mul bx
		add di,ax
		mov ax,0A000h 
		mov es,ax
	pop ax
	pop bx
	ret
	

GetScreenNextLine:
	push ax
		mov ax,di
		add ax,320
		mov di,ax
	pop ax
	ret

ScreenInit:
	mov ah, 0           ; int 10,0
    mov al, 13h           ; mode 4 (cga 320x200 4 color)
    int 10h             ; bios int
	ret
 ; VGA 320x200, 256-color (video mode 13H)
 ; Segment: a000
  ; Layout: Linear, packed-pixel.  This mode uses one byte (8 bits) per
          ; pixel.  The colors displayed depend on the palette settings.

          ; Each scan line is 320 bytes long and there are 200 scan lines
          ; (regen size=64,000 bytes).  Each byte contains 1 pixel (64,000
          ; total pixels).



PrintChar:
	push si
	push di
	push dx
	push cx
	push bx
	push ax
		push ds
			push ax
				
				
				
			;	call domonitor
				
				mov ax,0
				mov al,[ds:CursorY]
				mov bx,320*8
				mul bx
				mov di,ax
				
				mov ax,0
				mov al,[ds:CursorX]
				mov cl,3
				rcl ax,cl
				add di,ax
				
				
				mov si,offset BitmapFont
				
				mov ax, seg BitmapFont
				mov ds, ax
			
			pop ax
			sub al,32
			mov ah,0
			rcl ax,1
			rcl ax,1
			rcl ax,1
			add si,ax
			
			
			mov cx,8
			push es
				mov ax,0A000h 
				mov es,ax
FontAgainVGA:	
				Call DoPairVGA
				add di,320
				;add di,2000h
				dec cx
				jnz FontAgainVGA
			pop es
		pop ds
		inc [ds:CursorX]
		cmp [ds:CursorX],40
		jne PrintCharNoNewLine
		call NewLine
PrintCharNoNewLine:
	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	pop si
	ret

	
DoPairVGA:
	mov al,ds:[si]
	push di
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
		call PrintCharDoPart
	pop di
	
	inc si
	ret	
PrintCharDoPart:
	mov ah,0
	rcl al,1
	rcl ah,1	
	mov dh,ah
	rcl ah,1
	or ah,dh
	rcl ah,1
	or ah,dh
	rcl ah,1
	or ah,dh
	;and ah,[FontColor]
	mov es:[di],ah
	inc di
	ret



Locate:
		mov [CursorX],bh
		mov [CursorY],bl
	ret
NewLine:
		mov [CursorX],0
		inc [CursorY]
	ret
	
PrintSpace:
	push ax
		mov al,' '
		call PrintChar
	pop ax
	ret
	


	

	
SetPalette:

	
	
	
	push dx
	push cx
	push bx
	push ax 
	
		mov bx,ax	;Color
		mov cl,2
		
		mov al,dh
		and al,00111111b
		shl al,cl
		mov ch,al	;G (6 bit 0-63)


		mov al,dl
		and al,11111100b
		shr al,cl
		mov dh,al	;r		
		
		mov al,dl
		and al,00111111b
		shl al,cl
		mov cl,al	;b
	
		mov bh,0 
		mov ax,1007h 	;Select Color
		int 10h 
		
		mov bl,bh 
		mov bh,0 
		mov ax,1010h 
		int 10h 
	pop ax
	pop bx
	pop cx
	pop dx
	ret 





MemDump:			;Dump [es:bp] for bx bytes
	push ax
	push bx
	push cx 
			mov ax,es
			mov al,ah
			call Printhex
			mov ax,es
			call Printhex
			mov al,':'
			call PrintChar
			mov ax,bp
			mov al,ah
			call Printhex
			mov ax,bp
			call Printhex
			call NewLine
MemDumpAgain:	
			mov al, es:[bp]
			call Printhex
			call PrintSpace
			inc bp
			dec bx
			mov al,bl
			and al,00000111b
			jne MemDumpAgain	
			
			add bx,8
			sub bp,8
MemDumpCharAgain:
			mov al,es:[bp] 
			cmp al,' '
			jg MemDumpOkChar
			mov al,'.'
MemDumpOkChar:			
			call PrintChar
			inc bp
			dec bx
			mov al,bl
			and al,00000111b
			jne MemDumpCharAgain
			
			
			call NewLine
		
			cmp bx,0
			jne MemDumpAgain
	pop cx
	pop bx
	pop ax
	
	ret
DoMonitor:
	push ds
		push es
			push ax
				mov ax, @data
				mov es, ax
				mov ax, @data	
				mov ds, ax
			pop WORD PTR [ds:MonitorBak_AX]
		pop WORD PTR [ds:MonitorBak_ES]
	pop WORD PTR [ds:MonitorBak_DS]
	
	pop WORD PTR [ds:MonitorBak_IP]
	dec sp
	dec sp
	
	pushf
	pop WORD PTR [ds:MonitorBak_F]
	
	mov ax,WORD PTR [ds:MonitorBak_AX]
	push ax
	push bx
		mov bx,ax
		mov al,'A'
		
		call DoMonitorOneX
		
	pop bx
	pop ax
	push ax
	push bx
		mov al,'B'
		call DoMonitorOneX

ifdef SmallScreen
	call newline
endif
		
		mov bx,cx
		mov al,'C'
		call DoMonitorOneX

		mov bx,dx
		mov al,'D'
		call DoMonitorOneX
		
		call newline
		
		mov bx,WORD PTR [ds:MonitorBak_F]
		mov ax,'F '
		call doMonitorOne
		
		mov bx,WORD PTR [ds:MonitorBak_IP]
		mov ax,'PI'
		call doMonitorOne
		
		call newline
		
		mov bx,sp
		mov ax,'PS'
		call doMonitorOne
		
		mov bx,bp
		mov ax,'PB'
		call doMonitorOne

ifdef SmallScreen
	call newline
endif		
	
		mov bx,di
		mov ax,'ID'
		call doMonitorOne
		
		mov bx,si
		mov ax,'IS'
		call doMonitorOne
		
		call newline
		
		mov bx,cs
		mov ax,'SC'
		call doMonitorOne
		
		mov bx,WORD PTR [ds:MonitorBak_DS]
		mov ax,'SD'
		call doMonitorOne
		
ifdef SmallScreen
	call newline
endif
		
		mov bx,WORD PTR [ds:MonitorBak_ES]
		mov ax,'SE'
		call doMonitorOne
		
		mov bx,ss
		mov ax,'SS'
		call doMonitorOne
		
		call newline
		
		mov es,WORD PTR [ds:MonitorBak_ES]
		
		
		mov ax,WORD PTR [ds:MonitorBak_F]
		push ax
		popf
		mov ds,WORD PTR [ds:MonitorBak_DS]
	pop bx
	pop ax
	ret

DoMonitorGetIP:	
	pop bx
	push bx
	ret
DoMonitorOneX:	
	mov ah,'X'
DoMonitorOne:	
	call MonitorColon
	mov al,bh
	call Printhex
	mov al,bl
	call Printhex
	call PrintSpace
	ret
MonitorColon:	
	push ax	
	call PrintChar
	mov al,ah
	call PrintChar
	mov al,':'
	call PrintChar
	pop ax
	ret
Printhex:
	push ax
		mov ah,al
		and ah,11110000B
		ror ah,1	;can't use >1 on 8086
		ror ah,1
		ror ah,1
		ror ah,1
		call PrintHexChar
		mov ah,al
		and ah,00001111B
		call PrintHexChar
	pop ax
	ret
PrintHexChar:
	push ax
		cmp ah,9
		jle PrintHexCharNumber
		add ah,'A'-('0'+10)
PrintHexCharNumber:
		add ah,'0'
		mov al,ah
		call PrintChar
	pop ax
	ret




BitmapFont :
 db 000h,000h,000h,000h,000h,000h,000h,000h ;@00000000



BitmapTest : 
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 055h, 00Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch
	db 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 04Ch, 00Ch, 04Ch, 056h, 056h, 056h
	db 056h, 056h, 056h, 015h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 057h, 055h, 00Ah, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 00Bh, 057h, 057h, 056h, 056h, 056h, 057h, 056h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 042h, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 05Fh, 04Ch, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 00Ah, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ch
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 055h
	db 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 097h, 00Dh, 056h, 056h, 056h, 056h, 056h, 056h
	db 057h, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Dh, 057h, 056h
	db 056h, 056h, 056h, 056h, 056h, 04Eh, 09Fh, 0F6h, 0B5h, 0F6h, 04Eh, 04Eh, 056h, 056h, 056h, 056h
	db 056h, 057h, 057h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 054h, 056h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 057h, 0F6h, 0ACh, 000h, 000h, 049h, 008h, 0EFh, 04Eh, 04Eh, 056h, 056h
	db 056h, 056h, 056h, 057h, 056h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 00Ah, 055h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 057h, 056h, 056h, 056h, 056h
	db 056h, 04Eh, 04Eh, 0BFh, 0F7h, 000h, 000h, 000h, 000h, 000h, 000h, 052h, 008h, 0AFh, 04Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 053h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 0ACh, 09Fh, 056h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 00Eh, 0EFh, 008h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 05Ah, 0FFh, 09Fh
	db 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 00Eh, 0F6h, 049h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 0F5h, 05Eh, 057h, 055h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0AFh
	db 0F6h, 051h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0A3h
	db 0FFh, 05Eh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 09Fh, 00Ch, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 055h, 047h, 0FFh, 049h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 0FFh, 00Eh, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 055h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0A7h, 0FFh, 051h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 0F7h, 0F6h, 05Eh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Bh, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 056h, 057h, 016h, 0F6h, 092h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 0F6h, 046h, 056h, 056h, 057h, 055h, 04Ah, 000h, 000h, 000h
	db 000h, 000h, 055h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 09Fh, 0F6h, 09Bh, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 0F5h, 0EFh, 056h, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Bh, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 056h, 056h, 04Eh, 0A7h, 0A3h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 05Ah, 0EFh, 04Eh, 056h, 056h, 056h, 057h, 057h, 042h, 000h
	db 00Ah, 057h, 04Eh, 056h, 056h, 056h, 056h, 056h, 04Eh, 096h, 0F6h, 0A4h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 048h, 007h, 0B7h, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 04Eh
	db 000h, 000h, 000h, 041h, 055h, 056h, 057h, 056h, 056h, 056h, 04Eh, 0F7h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F7h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 056h
	db 05Eh, 0EFh, 0EFh, 056h, 056h, 04Eh, 056h, 04Eh, 008h, 0F7h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 054h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 051h, 0FFh, 0AFh, 04Eh, 056h, 04Eh, 056h, 04Eh, 04Eh, 008h
	db 0A5h, 04Bh, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 04Eh, 008h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F7h, 046h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 00Eh, 09Eh, 09Fh, 007h, 0A6h, 0A6h, 009h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 00Ch, 000h, 000h, 04Dh, 000h, 000h, 00Bh, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 05Ah, 0F6h, 09Fh, 0A7h, 0A6h, 0F6h, 066h, 056h
	db 04Eh, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 046h, 0FFh, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F6h, 045h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 00Eh, 04Eh, 05Eh, 00Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 04Ah, 000h, 000h, 0EFh, 056h, 056h, 056h, 055h, 09Fh, 0EBh, 000h, 00Ah, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 0AEh, 0A7h, 00Eh, 046h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0EFh, 011h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 052h, 0F6h, 00Dh, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 057h, 056h, 055h, 00Ch, 04Bh, 04Ah, 041h
	db 000h, 000h, 000h, 09Bh, 05Fh, 057h, 00Dh, 056h, 056h, 056h, 056h, 05Eh, 00Ch, 05Fh, 0EFh, 000h
	db 000h, 000h, 041h, 04Bh, 04Ch, 015h, 095h, 05Fh, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 09Fh, 0A2h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h, 04Eh, 05Bh, 0EFh, 04Eh, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h
	db 057h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 057h
	db 057h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0F3h, 04Fh, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h, 056h, 056h, 00Dh, 0ABh, 09Fh, 04Eh, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 04Fh, 0F4h, 04Dh, 056h, 056h, 057h, 04Ah, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 04Bh, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 001h, 0E3h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh, 04Eh
	db 04Eh, 056h, 056h, 056h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 056h, 056h
	db 056h, 04Eh, 04Eh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 04Eh, 008h, 04Dh, 056h, 056h, 056h, 056h, 057h, 056h, 041h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 04Bh, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh, 0F6h, 000h, 0F7h, 057h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh, 007h, 0EFh, 0EFh
	db 0EEh, 05Eh, 096h, 00Eh, 0EFh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 0A7h, 09Eh, 00Eh
	db 05Eh, 0E7h, 0EFh, 0EFh, 0EFh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 04Eh, 0F6h, 04Bh, 04Fh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah
	db 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0B7h, 0F5h, 052h, 000h, 000h, 008h, 09Fh
	db 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 0A7h, 0AFh, 0F6h, 008h, 0F6h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 0FFh, 096h, 0AFh, 04Eh, 04Eh, 09Eh, 056h, 007h, 0FFh, 0F6h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 008h, 008h, 008h, 0EFh, 0EFh, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 057h, 008h, 000h, 052h, 0F6h, 09Fh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 05Fh
	db 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h
	db 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0EFh, 0F6h, 052h, 000h, 000h, 000h, 000h, 000h, 0F5h
	db 0B7h, 00Dh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 007h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 09Fh, 00Dh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 00Dh, 09Fh, 0FFh, 000h, 000h, 000h, 000h, 0F7h, 0FFh, 09Eh, 00Eh, 056h, 056h, 056h, 056h, 056h
	db 056h, 057h, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 05Fh, 056h, 056h
	db 056h, 056h, 056h, 056h, 00Eh, 0DFh, 0FFh, 09Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 092h, 0F6h, 09Eh, 00Eh, 04Eh, 056h, 056h, 09Eh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 052h, 05Bh
	db 05Bh, 05Bh, 049h, 0A4h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0A4h, 049h
	db 05Bh, 05Bh, 05Bh, 052h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0AFh, 04Eh, 056h, 056h, 00Eh, 05Fh
	db 0FFh, 0A3h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F5h, 0F6h, 056h, 04Eh, 056h, 056h, 056h
	db 056h, 056h, 056h, 05Fh, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h, 056h, 056h, 056h
	db 056h, 056h, 00Eh, 056h, 0FFh, 05Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 007h, 0FFh, 0EFh, 056h, 056h, 04Eh, 0EEh, 0FFh, 0FFh, 0FFh, 0FFh, 000h, 0F6h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F6h, 040h, 0A4h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0ADh, 040h, 008h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 000h, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 04Dh, 056h, 04Eh, 09Eh, 0FFh, 0F5h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F5h, 0EFh, 056h, 00Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 04Eh, 096h, 0F6h, 0A4h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 04Dh, 066h, 056h, 056h, 056h, 04Eh, 007h, 0FFh, 0FFh, 007h, 05Bh, 0FFh, 0FFh
	db 0EDh, 0EDh, 0EDh, 007h, 0FFh, 0A4h, 052h, 0FFh, 0FFh, 0FFh, 0FFh, 052h, 0A4h, 0FFh, 007h, 0EDh
	db 0EDh, 0EDh, 0FFh, 0FFh, 05Bh, 0F7h, 0FFh, 0FFh, 0FFh, 00Dh, 056h, 056h, 056h, 0A6h, 054h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 049h, 009h, 0B7h, 04Eh
	db 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 056h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh
	db 0AFh, 0F5h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 011h, 0F5h, 09Fh, 056h, 056h, 056h, 056h, 0F6h, 0FFh, 0FFh, 0F7h, 05Bh, 0FFh, 049h
	db 0F7h, 007h, 0F7h, 0A4h, 05Bh, 0FFh, 0A4h, 09Bh, 0FFh, 0FFh, 0A4h, 09Bh, 0FFh, 05Bh, 0A4h, 0EDh
	db 0F7h, 007h, 000h, 0F6h, 0A4h, 09Bh, 0FFh, 0FFh, 0FFh, 05Fh, 056h, 056h, 056h, 09Eh, 007h, 011h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 051h, 008h
	db 0EFh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 04Eh, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 00Ah, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0A7h, 007h
	db 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah
	db 04Ch, 04Ch, 055h, 00Dh, 056h, 056h, 056h, 056h, 056h, 04Dh, 0FFh, 0FFh, 008h, 052h, 05Bh, 008h
	db 09Bh, 052h, 000h, 052h, 0A3h, 0A4h, 007h, 052h, 0FFh, 0FFh, 000h, 007h, 09Bh, 0A4h, 052h, 000h
	db 049h, 092h, 0FFh, 049h, 09Bh, 007h, 0FFh, 0FFh, 066h, 04Eh, 056h, 056h, 056h, 056h, 04Ch, 04Dh
	db 015h, 04Bh, 00Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 09Bh, 007h, 0E7h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 016h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 046h, 0EFh, 008h, 052h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Ah
	db 0A5h, 09Ch, 0A6h, 09Fh, 056h, 056h, 056h, 056h, 056h, 056h, 001h, 0FFh, 0FFh, 049h, 052h, 0F7h
	db 000h, 000h, 000h, 000h, 000h, 000h, 0A5h, 0F7h, 09Ch, 0F7h, 0A4h, 09Bh, 049h, 000h, 000h, 000h
	db 000h, 000h, 064h, 0A4h, 040h, 0FFh, 0FFh, 0A6h, 00Eh, 09Eh, 056h, 056h, 056h, 056h, 056h, 0A6h
	db 0A5h, 0A4h, 0ACh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 09Bh, 0F6h, 09Fh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 04Dh, 05Fh, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 09Fh, 0F6h, 09Ah, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 04Bh, 04Ch, 04Ch, 055h, 00Dh
	db 044h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0AFh, 0FFh, 0FFh, 0FFh, 008h, 000h, 0B6h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Bh, 007h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 09Bh, 052h, 007h, 0FFh, 0FFh, 0FFh, 0F6h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 04Dh, 00Ch, 015h, 04Ch, 04Bh, 04Bh, 042h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 0ADh, 0F6h, 096h, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 015h
	db 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0A7h, 0F6h, 09Bh, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 041h, 04Bh, 00Bh, 056h, 057h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0A6h, 0FFh, 0FFh, 0FFh, 0A4h, 052h
	db 0ACh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 0F6h, 008h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 009h, 05Ah, 011h, 0FFh, 0FFh, 0FFh, 0EFh, 055h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 057h, 057h, 057h, 095h, 014h, 04Bh, 00Ah, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 009h, 0F6h, 04Eh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h
	db 057h, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h
	db 056h, 056h, 056h, 056h, 056h, 00Eh, 057h, 0F6h, 0ACh, 000h, 000h, 000h, 000h, 000h, 040h, 00Ah
	db 054h, 057h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Dh, 0A6h, 0FFh, 0FFh, 0F6h, 0F7h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F6h, 007h, 0FFh, 0FFh, 0FFh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 057h
	db 056h, 04Bh, 00Ah, 000h, 000h, 000h, 000h, 000h, 007h, 0EFh, 04Eh, 04Eh, 056h, 056h, 056h, 056h
	db 056h, 057h, 056h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 057h, 056h, 056h
	db 056h, 056h, 056h, 04Eh, 04Eh, 0EFh, 0F5h, 000h, 000h, 000h, 000h, 000h, 00Ah, 056h, 097h, 057h
	db 057h, 096h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0EFh, 0FFh, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 007h, 007h, 008h, 008h, 0EFh, 0EFh, 0F6h, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0EFh, 0FFh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 09Eh, 057h, 097h, 057h, 04Eh, 00Ah, 000h, 000h, 000h, 049h, 008h, 0AFh, 04Eh, 056h, 056h, 056h
	db 056h, 056h, 056h, 057h, 056h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 04Ah, 056h, 057h, 056h, 056h, 056h, 056h
	db 056h, 056h, 04Eh, 0EFh, 007h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 0EDh, 0F5h, 008h, 008h
	db 0E7h, 0A6h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 09Fh, 04Eh, 0AFh, 0FFh, 0FFh, 0FFh, 0F6h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0AFh, 0AFh, 0F6h, 007h, 0B7h, 0AFh, 0EEh, 007h, 0AFh, 0A6h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 04Dh, 057h, 05Eh, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 09Eh, 0EFh, 008h, 0F6h, 0F5h, 0F7h, 000h, 000h, 000h, 000h, 000h, 05Ah, 008h, 09Fh, 04Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 057h, 056h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 054h, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 008h, 051h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 015h, 00Dh, 04Dh, 04Eh
	db 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 046h, 0FFh, 0AFh, 056h, 0A7h, 095h
	db 0FFh, 0FFh, 0FFh, 0A7h, 007h, 009h, 052h, 000h, 000h, 000h, 000h, 052h, 0F7h, 0FFh, 04Dh, 0FFh
	db 0FFh, 0FFh, 0F6h, 04Dh, 0EEh, 09Eh, 0F6h, 0AFh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 04Eh, 04Eh, 04Dh, 00Dh, 00Dh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0A3h, 0EFh, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 05Ah, 0FFh, 09Eh, 00Eh, 056h, 056h, 056h, 056h
	db 056h, 056h, 04Eh, 00Dh, 000h, 000h, 000h, 000h, 015h, 056h, 057h, 057h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 04Eh, 056h, 00Dh, 0FFh
	db 0F6h, 04Eh, 09Fh, 00Dh, 0FFh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0FFh, 096h, 055h
	db 0A7h, 05Fh, 0FFh, 0A6h, 04Eh, 056h, 00Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 057h, 05Fh, 015h, 042h, 000h, 000h, 00Bh, 00Eh, 056h, 056h
	db 056h, 056h, 056h, 056h, 04Eh, 046h, 0EFh, 008h, 049h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 0ACh, 0FFh, 05Fh, 00Dh, 056h, 056h
	db 056h, 04Eh, 04Eh, 0EFh, 053h, 00Ch, 05Fh, 057h, 046h, 046h, 00Eh, 04Eh, 057h, 097h, 0AFh, 0AFh
	db 0AFh, 0AFh, 0B7h, 0EFh, 007h, 007h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 04Eh, 056h, 056h, 056h, 09Eh
	db 00Dh, 056h, 056h, 056h, 0EFh, 0A4h, 000h, 000h, 000h, 000h, 000h, 000h, 051h, 0FFh, 00Dh, 056h
	db 056h, 056h, 00Dh, 09Fh, 056h, 056h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 09Eh, 0AFh, 0AFh, 0AFh
	db 096h, 097h, 057h, 04Eh, 00Eh, 00Eh, 046h, 04Eh, 056h, 057h, 05Fh, 043h, 0EDh, 0A7h, 00Dh, 056h
	db 056h, 056h, 056h, 00Eh, 0B7h, 0F6h, 051h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F5h, 0F6h, 05Eh, 00Eh
	db 096h, 0AEh, 007h, 00Dh, 096h, 097h, 066h, 0AEh, 0F6h, 0FFh, 0F6h, 007h, 0EDh, 0F7h, 0EDh, 0E3h
	db 052h, 051h, 00Ah, 04Ch, 00Eh, 00Eh, 00Dh, 04Eh, 056h, 097h, 09Fh, 0AFh, 0AFh, 0AFh, 0A7h, 0AFh
	db 0A7h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Dh, 0FFh, 0AFh, 056h, 056h, 056h, 05Fh
	db 015h, 056h, 056h, 056h, 056h, 0FFh, 049h, 000h, 000h, 000h, 000h, 010h, 0FFh, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 0A7h, 0E7h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 09Eh, 0AFh, 0AFh, 066h, 0AFh, 0AFh, 0AFh, 09Fh, 08Fh, 056h, 00Eh, 00Dh, 046h, 046h, 09Eh, 0F7h
	db 0F7h, 0F7h, 0F7h, 007h, 0F6h, 0FFh, 0FFh, 0F6h, 0A5h, 09Fh, 09Fh, 056h, 00Dh, 05Dh, 0F6h, 0A6h
	db 046h, 00Eh, 09Eh, 0FFh, 052h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 007h, 0F6h
	db 0F6h, 09Eh, 007h, 0AEh, 007h, 09Eh, 056h, 056h, 055h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h
	db 042h, 057h, 05Fh, 0AFh, 0F6h, 0FFh, 0F6h, 0EEh, 0F5h, 008h, 007h, 0F5h, 0F7h, 0A4h, 0A4h, 0A4h
	db 04Dh, 09Eh, 09Fh, 09Fh, 09Fh, 09Fh, 09Eh, 09Fh, 04Eh, 04Eh, 056h, 09Eh, 056h, 056h, 056h, 0A7h
	db 0EFh, 04Eh, 056h, 056h, 056h, 046h, 0EFh, 0A3h, 0A4h, 0A4h, 0A2h, 0EFh, 04Eh, 056h, 056h, 056h
	db 056h, 04Eh, 0FFh, 04Eh, 056h, 056h, 0EEh, 056h, 00Dh, 04Eh, 09Fh, 09Fh, 09Fh, 09Fh, 09Eh, 09Fh
	db 09Eh, 04Dh, 0A4h, 09Bh, 0ACh, 0F7h, 0F7h, 007h, 008h, 0F7h, 007h, 0F6h, 0F6h, 0EFh, 09Fh, 096h
	db 04Eh, 00Ah, 000h, 000h, 000h, 000h, 000h, 054h, 056h, 056h, 0EFh, 0B7h, 0E7h, 008h, 09Fh, 0EFh
	db 0F6h, 0F6h, 0A3h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 049h
	db 0F6h, 0EFh, 04Dh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 057h, 056h, 049h, 000h, 000h, 000h, 05Bh
	db 007h, 007h, 0F7h, 049h, 049h, 049h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 0F5h
	db 0F5h, 008h, 007h, 0ADh, 0F7h, 0ADh, 0F5h, 0F6h, 0F5h, 0EFh, 056h, 056h, 056h, 056h, 056h, 04Eh
	db 0FFh, 09Eh, 056h, 056h, 056h, 056h, 04Eh, 097h, 096h, 096h, 097h, 04Eh, 056h, 056h, 056h, 056h
	db 00Dh, 0FFh, 04Eh, 056h, 056h, 056h, 04Eh, 056h, 0EFh, 007h, 008h, 0F5h, 0ADh, 0F7h, 0ADh, 008h
	db 008h, 0F7h, 0F5h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 049h, 049h, 0A3h, 0F5h
	db 007h, 0AEh, 052h, 000h, 000h, 04Ch, 057h, 057h, 056h, 056h, 04Eh, 04Eh, 056h, 046h, 09Eh, 0FFh
	db 0A4h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 051h, 0F6h, 0EFh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 056h, 000h, 000h, 051h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 05Fh, 0A6h, 056h, 056h, 056h, 056h, 056h
	db 09Eh, 0FFh, 046h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh
	db 0F6h, 0AEh, 04Eh, 056h, 056h, 056h, 056h, 09Eh, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 052h, 00Ah, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0EFh, 0F7h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 09Bh, 0F6h, 0A7h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 015h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0A3h, 09Bh, 0ECh, 04Eh, 04Eh, 056h, 056h
	db 056h, 0FFh, 007h, 00Eh, 056h, 056h, 056h, 04Eh, 056h, 05Eh, 04Eh, 056h, 056h, 056h, 04Eh, 0A7h
	db 0FFh, 00Dh, 056h, 056h, 04Eh, 057h, 0F5h, 09Bh, 0E4h, 049h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 00Ah, 055h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 046h, 0EFh, 008h, 049h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 0A3h, 0F6h, 09Fh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 05Fh
	db 00Ch, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 051h, 0F6h, 0FFh, 04Eh, 056h
	db 04Eh, 0F6h, 0FFh, 0F6h, 0A7h, 0A7h, 0AFh, 0EFh, 0EEh, 0E6h, 008h, 0AFh, 0A7h, 0A7h, 0F6h, 0FFh
	db 0FFh, 00Dh, 056h, 04Eh, 0F6h, 0F6h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 056h, 05Fh, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 0E7h, 0F6h, 051h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0ACh, 0FFh, 05Fh, 00Eh, 056h, 056h, 056h, 056h, 056h
	db 056h, 05Fh, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Fh, 056h
	db 056h, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0AFh, 05Eh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F6h, 00Eh, 056h, 05Eh, 000h, 000h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h
	db 056h, 056h, 056h, 056h, 056h, 056h, 046h, 0A7h, 0FFh, 052h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 009h, 0EFh, 04Eh, 00Eh, 056h, 056h, 056h
	db 056h, 056h, 056h, 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 056h
	db 04Eh, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0AEh, 0A7h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F6h, 00Eh, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 00Eh, 05Fh, 0F6h, 09Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 007h, 0EFh, 056h, 04Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Fh, 016h
	db 04Eh, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0EFh, 0A7h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 00Dh, 056h, 057h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 056h, 056h, 056h, 056h
	db 056h, 056h, 00Eh, 05Fh, 0EEh, 0A4h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 051h, 008h, 0AFh, 04Eh
	db 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 056h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 008h, 097h
	db 056h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0EFh, 0A7h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 04Dh, 09Fh, 0F6h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 056h, 057h, 056h, 056h, 056h, 056h, 056h
	db 04Eh, 04Fh, 0F6h, 0F7h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Ah, 0F6h
	db 0E7h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 049h, 000h, 05Eh
	db 056h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 007h, 09Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 08Fh, 05Dh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh
	db 0EFh, 0EDh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 0A3h, 0F6h, 09Fh, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ch
	db 09Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 09Eh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 09Eh, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 055h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0AFh, 007h
	db 051h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 0F7h, 0F6h, 056h, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 097h, 00Ch, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Dh
	db 09Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 09Fh, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 015h, 05Fh, 056h, 056h, 056h, 056h, 056h, 056h, 00Dh, 09Fh, 0F6h, 092h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 0F5h, 0FFh, 057h, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h
	db 04Eh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 00Eh, 04Ch, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 04Ch, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 09Fh, 0F6h, 09Ah, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 007h, 0EFh, 056h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h
	db 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah
	db 05Fh, 04Eh, 0EFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F6h
	db 04Eh, 057h, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch
	db 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 05Eh, 0F6h, 0A3h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 051h, 008h, 0EFh, 056h, 04Eh, 056h, 056h, 056h, 056h
	db 056h, 057h, 056h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 04Bh, 056h, 04Fh, 056h, 09Eh, 0A6h, 0EFh, 0EFh, 0EFh, 0EFh, 0EFh, 0EFh, 0DEh, 05Eh, 04Eh, 04Fh
	db 057h, 04Ch, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h
	db 056h, 056h, 056h, 056h, 056h, 00Eh, 056h, 007h, 0EDh, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 052h, 0F6h, 0EFh, 04Eh, 04Eh, 056h, 056h
	db 056h, 056h, 056h, 057h, 055h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 04Ah, 04Bh, 056h, 04Eh, 04Eh, 04Eh, 04Eh, 04Eh, 04Eh, 04Eh, 04Eh, 04Eh, 054h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 056h, 057h, 056h, 056h
	db 056h, 056h, 056h, 04Eh, 056h, 0EFh, 0F7h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 09Bh, 0F6h, 0E7h, 04Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 057h, 04Dh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 041h, 041h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 056h, 057h, 056h, 056h, 056h, 056h
	db 056h, 056h, 04Eh, 0AFh, 007h, 051h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0A4h, 0F6h, 09Fh
	db 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 054h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 04Eh, 0A7h, 008h, 052h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F7h
	db 0FFh, 04Eh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 097h, 00Bh, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 05Fh, 056h, 056h, 056h, 056h, 056h, 056h, 00Dh, 0A7h
	db 0F6h, 0A3h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 009h, 0F6h, 05Fh, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 00Ah, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 00Bh, 05Fh, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 09Fh, 0F6h, 0A3h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 049h, 008h, 0EFh, 056h, 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 056h, 04Ah, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 057h, 0EFh, 0F7h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 052h, 0F6h, 0EEh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 057h, 056h
	db 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 049h, 057h, 057h, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh, 007h, 0EDh, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 09Bh, 0F6h, 0AFh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h
	db 057h, 055h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 055h
	db 057h, 056h, 056h, 056h, 056h, 056h, 056h, 04Eh, 0EFh, 007h, 049h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0A4h, 0F6h, 09Fh, 04Eh, 056h, 056h, 056h, 056h
	db 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h, 056h
	db 056h, 056h, 056h, 056h, 056h, 04Eh, 0EFh, 0F6h, 051h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F7h, 0FFh, 097h, 04Eh, 056h, 056h
	db 056h, 056h, 056h, 057h, 057h, 04Bh, 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h, 056h, 056h
	db 056h, 056h, 056h, 00Eh, 09Fh, 0FFh, 052h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 008h, 0F6h, 05Eh, 04Eh
	db 056h, 056h, 056h, 056h, 056h, 057h, 05Fh, 04Bh, 000h, 04Bh, 05Fh, 057h, 056h, 056h, 056h, 056h
	db 056h, 00Dh, 057h, 0F6h, 0A4h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 049h, 0F6h, 0F6h
	db 056h, 00Eh, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh
	db 056h, 0F6h, 0ECh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 052h
	db 008h, 0EFh, 056h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 00Eh, 056h, 0EFh
	db 007h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 09Ah, 0F6h, 0AFh, 04Eh, 04Eh, 056h, 056h, 056h, 056h, 056h, 04Eh, 04Eh, 0AFh, 007h, 048h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 0A3h, 0F6h, 0A7h, 056h, 056h, 056h, 056h, 056h, 0EFh, 007h, 051h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 0F7h, 0F6h, 09Fh, 046h, 0A7h, 0F6h, 05Ah, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 013h, 055h, 000h, 054h, 014h, 041h, 056h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0F5h, 0FFh, 0A3h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 04Bh, 000h, 042h, 054h, 040h, 056h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 000h, 000h, 000h, 000h, 000h
	db 000h, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 04Ah, 042h
	db 04Ah, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 00Bh, 057h, 056h, 056h, 056h, 056h, 056h, 05Eh, 056h
	db 05Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 05Eh, 056h, 056h, 056h, 056h, 056h
	db 057h, 00Ah, 000h, 000h, 00Ah, 04Dh, 055h, 05Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 05Eh, 05Eh, 04Ch, 04Bh, 000h, 000h, 000h, 04Bh, 056h, 056h, 056h, 057h, 056h, 05Eh, 05Eh, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 05Eh, 05Eh, 056h, 056h, 056h, 056h
	db 056h, 041h, 000h, 000h, 000h, 04Ch, 056h, 05Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 05Eh, 055h, 095h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 056h, 05Eh, 056h, 056h, 056h
	db 05Eh, 04Dh, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 055h, 057h, 05Fh, 04Eh, 055h
	db 04Dh, 05Eh, 05Eh, 056h, 04Dh, 042h, 000h, 04Bh, 05Fh, 040h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 04Ah, 057h, 056h, 057h, 00Ch, 041h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 057h, 000h, 000h, 000h, 000h, 041h, 04Ch, 057h, 056h
	db 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 056h, 056h, 057h, 00Bh, 00Ah, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 015h, 000h, 000h, 000h, 000h, 04Ah, 04Ch, 057h, 056h
	db 057h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 04Eh, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h, 057h, 04Ch
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 057h, 057h, 00Ah, 000h, 000h, 000h
	db 000h, 000h, 000h, 00Ah, 014h, 057h, 057h, 057h, 056h, 040h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 04Ah, 057h, 056h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 056h
	db 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 013h, 056h, 056h, 056h, 056h, 056h, 057h, 00Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 055h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h
	db 057h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 057h, 056h, 056h, 056h, 056h, 056h, 00Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ch, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 05Fh, 056h, 057h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 057h, 056h, 056h, 040h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 04Ah, 097h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 057h, 04Ah, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 057h, 041h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 056h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 056h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h, 056h, 056h, 057h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h, 056h, 040h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 00Ah, 053h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 053h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 054h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 05Eh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 057h, 056h, 056h, 056h, 057h, 00Ah, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 057h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h, 056h, 056h, 056h, 056h, 057h, 056h, 04Bh, 049h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 057h
	db 056h, 056h, 04Ch, 049h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 057h, 056h, 016h, 04Ah, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ch, 057h, 057h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 05Fh, 056h, 00Ah, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Bh, 055h, 057h
	db 057h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 04Bh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 056h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 014h, 055h, 05Fh, 056h, 056h, 056h, 056h, 056h, 057h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h, 056h, 00Ch
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ch, 057h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 056h, 04Ch, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 055h, 057h, 056h, 056h, 056h, 057h, 00Ah, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 053h, 056h, 056h, 056h, 056h, 056h, 057h, 04Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 015h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 041h, 057h, 056h, 056h, 056h, 056h, 056h, 04Dh
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 057h, 056h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 055h, 057h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 04Dh, 056h, 056h, 056h, 057h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 013h, 056h, 056h, 056h, 056h, 056h, 057h, 00Ah
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 057h, 056h, 056h, 056h, 056h, 056h, 055h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 057h, 056h, 056h, 056h, 056h, 057h
	db 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 054h, 057h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 056h, 056h, 057h, 049h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 04Ch, 056h, 056h, 057h, 04Bh, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 00Ah, 056h, 056h, 056h, 056h, 056h, 056h, 057h, 00Ah, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 015h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 00Bh, 057h, 056h, 056h, 056h, 056h, 056h, 057h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ah, 057h, 056h, 056h, 056h, 056h
	db 057h, 00Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 056h, 05Fh, 041h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 056h, 056h, 056h, 057h, 055h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 00Ah, 057h, 056h, 057h, 04Bh, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 05Eh, 056h
	db 057h, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 056h, 05Eh, 00Bh, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 04Ah, 05Eh, 05Dh, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 057h
	db 05Fh, 055h, 05Eh, 04Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Ch, 05Eh, 056h
	db 05Fh, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 057h, 055h, 05Eh, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 04Bh, 04Ch, 057h, 057h
	db 057h, 057h, 057h, 055h, 055h, 055h, 055h, 05Dh, 056h, 05Fh, 056h, 04Bh, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 056h, 056h, 054h, 055h, 057h, 05Fh, 056h, 054h, 042h, 00Ah
	db 041h, 043h, 05Dh, 056h, 05Fh, 056h, 00Bh, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 00Ah, 04Bh, 04Bh, 04Ch, 04Ch, 04Ch, 04Bh, 042h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 053h, 000h, 000h, 000h, 000h, 000h, 000h, 043h, 054h, 054h
	db 054h, 04Ch, 043h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 042h, 057h, 056h, 056h
	db 056h, 056h, 056h, 015h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h
	db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 00Ah, 057h, 056h, 056h
	db 056h, 056h, 056h, 056h, 015h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h
	db 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 055h, 015h, 055h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h
	db 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h, 056h

Palette :
db 050h,041h,04Ch,00Dh,00Ah,032h,035h,036h ;@00000000
db 00Dh,00Ah,020h,020h,030h,020h,020h,020h ;@00000008
db 030h,020h,020h,020h,030h,00Dh,00Ah,031h ;@00000010
db 032h,038h,020h,020h,020h,030h,020h,020h ;@00000018
db 020h,030h,00Dh,00Ah,020h,020h,030h,020h ;@00000020
db 031h,032h,038h,020h,020h,020h,030h,00Dh ;@00000028
db 00Ah,031h,032h,038h,020h,031h,032h,038h ;@00000030
db 020h,020h,020h,030h,00Dh,00Ah,020h,020h ;@00000038
db 030h,020h,020h,020h,030h,020h,031h,032h ;@00000040
db 038h,00Dh,00Ah,031h,032h,038h,020h,020h ;@00000048
db 020h,030h,020h,031h,032h,038h,00Dh,00Ah ;@00000050
db 020h,020h,030h,020h,031h,032h,038h,020h ;@00000058
db 031h,032h,038h,00Dh,00Ah,031h,039h,032h ;@00000060
db 020h,031h,039h,032h,020h,031h,039h,032h ;@00000068
db 00Dh,00Ah,031h,039h,032h,020h,032h,032h ;@00000070
db 030h,020h,031h,039h,032h,00Dh,00Ah,031h ;@00000078
db 036h,036h,020h,032h,030h,032h,020h,032h ;@00000080
db 034h,030h,00Dh,00Ah,020h,036h,034h,020h ;@00000088
db 020h,033h,032h,020h,020h,020h,030h,00Dh ;@00000090
db 00Ah,020h,039h,036h,020h,020h,033h,032h ;@00000098
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@000000A0
db 038h,020h,020h,033h,032h,020h,020h,020h ;@000000A8
db 030h,00Dh,00Ah,031h,036h,030h,020h,020h ;@000000B0
db 033h,032h,020h,020h,020h,030h,00Dh,00Ah ;@000000B8
db 031h,039h,032h,020h,020h,033h,032h,020h ;@000000C0
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@000000C8
db 020h,020h,033h,032h,020h,020h,020h,030h ;@000000D0
db 00Dh,00Ah,020h,020h,030h,020h,020h,036h ;@000000D8
db 034h,020h,020h,020h,030h,00Dh,00Ah,020h ;@000000E0
db 033h,032h,020h,020h,036h,034h,020h,020h ;@000000E8
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@000000F0
db 020h,036h,034h,020h,020h,020h,030h,00Dh ;@000000F8
db 00Ah,020h,039h,036h,020h,020h,036h,034h ;@00000100
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@00000108
db 038h,020h,020h,036h,034h,020h,020h,020h ;@00000110
db 030h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000118
db 036h,034h,020h,020h,020h,030h,00Dh,00Ah ;@00000120
db 031h,039h,032h,020h,020h,036h,034h,020h ;@00000128
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@00000130
db 020h,020h,036h,034h,020h,020h,020h,030h ;@00000138
db 00Dh,00Ah,020h,020h,030h,020h,020h,039h ;@00000140
db 036h,020h,020h,020h,030h,00Dh,00Ah,020h ;@00000148
db 033h,032h,020h,020h,039h,036h,020h,020h ;@00000150
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@00000158
db 020h,039h,036h,020h,020h,020h,030h,00Dh ;@00000160
db 00Ah,020h,039h,036h,020h,020h,039h,036h ;@00000168
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@00000170
db 038h,020h,020h,039h,036h,020h,020h,020h ;@00000178
db 030h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000180
db 039h,036h,020h,020h,020h,030h,00Dh,00Ah ;@00000188
db 031h,039h,032h,020h,020h,039h,036h,020h ;@00000190
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@00000198
db 020h,020h,039h,036h,020h,020h,020h,030h ;@000001A0
db 00Dh,00Ah,020h,020h,030h,020h,031h,032h ;@000001A8
db 038h,020h,020h,020h,030h,00Dh,00Ah,020h ;@000001B0
db 033h,032h,020h,031h,032h,038h,020h,020h ;@000001B8
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@000001C0
db 031h,032h,038h,020h,020h,020h,030h,00Dh ;@000001C8
db 00Ah,020h,039h,036h,020h,031h,032h,038h ;@000001D0
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@000001D8
db 038h,020h,031h,032h,038h,020h,020h,020h ;@000001E0
db 030h,00Dh,00Ah,031h,036h,030h,020h,031h ;@000001E8
db 032h,038h,020h,020h,020h,030h,00Dh,00Ah ;@000001F0
db 031h,039h,032h,020h,031h,032h,038h,020h ;@000001F8
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@00000200
db 020h,031h,032h,038h,020h,020h,020h,030h ;@00000208
db 00Dh,00Ah,020h,020h,030h,020h,031h,036h ;@00000210
db 030h,020h,020h,020h,030h,00Dh,00Ah,020h ;@00000218
db 033h,032h,020h,031h,036h,030h,020h,020h ;@00000220
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@00000228
db 031h,036h,030h,020h,020h,020h,030h,00Dh ;@00000230
db 00Ah,020h,039h,036h,020h,031h,036h,030h ;@00000238
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@00000240
db 038h,020h,031h,036h,030h,020h,020h,020h ;@00000248
db 030h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000250
db 036h,030h,020h,020h,020h,030h,00Dh,00Ah ;@00000258
db 031h,039h,032h,020h,031h,036h,030h,020h ;@00000260
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@00000268
db 020h,031h,036h,030h,020h,020h,020h,030h ;@00000270
db 00Dh,00Ah,020h,020h,030h,020h,031h,039h ;@00000278
db 032h,020h,020h,020h,030h,00Dh,00Ah,020h ;@00000280
db 033h,032h,020h,031h,039h,032h,020h,020h ;@00000288
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@00000290
db 031h,039h,032h,020h,020h,020h,030h,00Dh ;@00000298
db 00Ah,020h,039h,036h,020h,031h,039h,032h ;@000002A0
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@000002A8
db 038h,020h,031h,039h,032h,020h,020h,020h ;@000002B0
db 030h,00Dh,00Ah,031h,036h,030h,020h,031h ;@000002B8
db 039h,032h,020h,020h,020h,030h,00Dh,00Ah ;@000002C0
db 031h,039h,032h,020h,031h,039h,032h,020h ;@000002C8
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@000002D0
db 020h,031h,039h,032h,020h,020h,020h,030h ;@000002D8
db 00Dh,00Ah,020h,020h,030h,020h,032h,032h ;@000002E0
db 034h,020h,020h,020h,030h,00Dh,00Ah,020h ;@000002E8
db 033h,032h,020h,032h,032h,034h,020h,020h ;@000002F0
db 020h,030h,00Dh,00Ah,020h,036h,034h,020h ;@000002F8
db 032h,032h,034h,020h,020h,020h,030h,00Dh ;@00000300
db 00Ah,020h,039h,036h,020h,032h,032h,034h ;@00000308
db 020h,020h,020h,030h,00Dh,00Ah,031h,032h ;@00000310
db 038h,020h,032h,032h,034h,020h,020h,020h ;@00000318
db 030h,00Dh,00Ah,031h,036h,030h,020h,032h ;@00000320
db 032h,034h,020h,020h,020h,030h,00Dh,00Ah ;@00000328
db 031h,039h,032h,020h,032h,032h,034h,020h ;@00000330
db 020h,020h,030h,00Dh,00Ah,032h,032h,034h ;@00000338
db 020h,032h,032h,034h,020h,020h,020h,030h ;@00000340
db 00Dh,00Ah,020h,020h,030h,020h,020h,020h ;@00000348
db 030h,020h,020h,036h,034h,00Dh,00Ah,020h ;@00000350
db 033h,032h,020h,020h,020h,030h,020h,020h ;@00000358
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000360
db 020h,020h,030h,020h,020h,036h,034h,00Dh ;@00000368
db 00Ah,020h,039h,036h,020h,020h,020h,030h ;@00000370
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@00000378
db 038h,020h,020h,020h,030h,020h,020h,036h ;@00000380
db 034h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000388
db 020h,030h,020h,020h,036h,034h,00Dh,00Ah ;@00000390
db 031h,039h,032h,020h,020h,020h,030h,020h ;@00000398
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@000003A0
db 020h,020h,020h,030h,020h,020h,036h,034h ;@000003A8
db 00Dh,00Ah,020h,020h,030h,020h,020h,033h ;@000003B0
db 032h,020h,020h,036h,034h,00Dh,00Ah,020h ;@000003B8
db 033h,032h,020h,020h,033h,032h,020h,020h ;@000003C0
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@000003C8
db 020h,033h,032h,020h,020h,036h,034h,00Dh ;@000003D0
db 00Ah,020h,039h,036h,020h,020h,033h,032h ;@000003D8
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@000003E0
db 038h,020h,020h,033h,032h,020h,020h,036h ;@000003E8
db 034h,00Dh,00Ah,031h,036h,030h,020h,020h ;@000003F0
db 033h,032h,020h,020h,036h,034h,00Dh,00Ah ;@000003F8
db 031h,039h,032h,020h,020h,033h,032h,020h ;@00000400
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@00000408
db 020h,020h,033h,032h,020h,020h,036h,034h ;@00000410
db 00Dh,00Ah,020h,020h,030h,020h,020h,036h ;@00000418
db 034h,020h,020h,036h,034h,00Dh,00Ah,020h ;@00000420
db 033h,032h,020h,020h,036h,034h,020h,020h ;@00000428
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000430
db 020h,036h,034h,020h,020h,036h,034h,00Dh ;@00000438
db 00Ah,020h,039h,036h,020h,020h,036h,034h ;@00000440
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@00000448
db 038h,020h,020h,036h,034h,020h,020h,036h ;@00000450
db 034h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000458
db 036h,034h,020h,020h,036h,034h,00Dh,00Ah ;@00000460
db 031h,039h,032h,020h,020h,036h,034h,020h ;@00000468
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@00000470
db 020h,020h,036h,034h,020h,020h,036h,034h ;@00000478
db 00Dh,00Ah,020h,020h,030h,020h,020h,039h ;@00000480
db 036h,020h,020h,036h,034h,00Dh,00Ah,020h ;@00000488
db 033h,032h,020h,020h,039h,036h,020h,020h ;@00000490
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000498
db 020h,039h,036h,020h,020h,036h,034h,00Dh ;@000004A0
db 00Ah,020h,039h,036h,020h,020h,039h,036h ;@000004A8
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@000004B0
db 038h,020h,020h,039h,036h,020h,020h,036h ;@000004B8
db 034h,00Dh,00Ah,031h,036h,030h,020h,020h ;@000004C0
db 039h,036h,020h,020h,036h,034h,00Dh,00Ah ;@000004C8
db 031h,039h,032h,020h,020h,039h,036h,020h ;@000004D0
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@000004D8
db 020h,020h,039h,036h,020h,020h,036h,034h ;@000004E0
db 00Dh,00Ah,020h,020h,030h,020h,031h,032h ;@000004E8
db 038h,020h,020h,036h,034h,00Dh,00Ah,020h ;@000004F0
db 033h,032h,020h,031h,032h,038h,020h,020h ;@000004F8
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000500
db 031h,032h,038h,020h,020h,036h,034h,00Dh ;@00000508
db 00Ah,020h,039h,036h,020h,031h,032h,038h ;@00000510
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@00000518
db 038h,020h,031h,032h,038h,020h,020h,036h ;@00000520
db 034h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000528
db 032h,038h,020h,020h,036h,034h,00Dh,00Ah ;@00000530
db 031h,039h,032h,020h,031h,032h,038h,020h ;@00000538
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@00000540
db 020h,031h,032h,038h,020h,020h,036h,034h ;@00000548
db 00Dh,00Ah,020h,020h,030h,020h,031h,036h ;@00000550
db 030h,020h,020h,036h,034h,00Dh,00Ah,020h ;@00000558
db 033h,032h,020h,031h,036h,030h,020h,020h ;@00000560
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000568
db 031h,036h,030h,020h,020h,036h,034h,00Dh ;@00000570
db 00Ah,020h,039h,036h,020h,031h,036h,030h ;@00000578
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@00000580
db 038h,020h,031h,036h,030h,020h,020h,036h ;@00000588
db 034h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000590
db 036h,030h,020h,020h,036h,034h,00Dh,00Ah ;@00000598
db 031h,039h,032h,020h,031h,036h,030h,020h ;@000005A0
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@000005A8
db 020h,031h,036h,030h,020h,020h,036h,034h ;@000005B0
db 00Dh,00Ah,020h,020h,030h,020h,031h,039h ;@000005B8
db 032h,020h,020h,036h,034h,00Dh,00Ah,020h ;@000005C0
db 033h,032h,020h,031h,039h,032h,020h,020h ;@000005C8
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@000005D0
db 031h,039h,032h,020h,020h,036h,034h,00Dh ;@000005D8
db 00Ah,020h,039h,036h,020h,031h,039h,032h ;@000005E0
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@000005E8
db 038h,020h,031h,039h,032h,020h,020h,036h ;@000005F0
db 034h,00Dh,00Ah,031h,036h,030h,020h,031h ;@000005F8
db 039h,032h,020h,020h,036h,034h,00Dh,00Ah ;@00000600
db 031h,039h,032h,020h,031h,039h,032h,020h ;@00000608
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@00000610
db 020h,031h,039h,032h,020h,020h,036h,034h ;@00000618
db 00Dh,00Ah,020h,020h,030h,020h,032h,032h ;@00000620
db 034h,020h,020h,036h,034h,00Dh,00Ah,020h ;@00000628
db 033h,032h,020h,032h,032h,034h,020h,020h ;@00000630
db 036h,034h,00Dh,00Ah,020h,036h,034h,020h ;@00000638
db 032h,032h,034h,020h,020h,036h,034h,00Dh ;@00000640
db 00Ah,020h,039h,036h,020h,032h,032h,034h ;@00000648
db 020h,020h,036h,034h,00Dh,00Ah,031h,032h ;@00000650
db 038h,020h,032h,032h,034h,020h,020h,036h ;@00000658
db 034h,00Dh,00Ah,031h,036h,030h,020h,032h ;@00000660
db 032h,034h,020h,020h,036h,034h,00Dh,00Ah ;@00000668
db 031h,039h,032h,020h,032h,032h,034h,020h ;@00000670
db 020h,036h,034h,00Dh,00Ah,032h,032h,034h ;@00000678
db 020h,032h,032h,034h,020h,020h,036h,034h ;@00000680
db 00Dh,00Ah,020h,020h,030h,020h,020h,020h ;@00000688
db 030h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000690
db 033h,032h,020h,020h,020h,030h,020h,031h ;@00000698
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@000006A0
db 020h,020h,030h,020h,031h,032h,038h,00Dh ;@000006A8
db 00Ah,020h,039h,036h,020h,020h,020h,030h ;@000006B0
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@000006B8
db 038h,020h,020h,020h,030h,020h,031h,032h ;@000006C0
db 038h,00Dh,00Ah,031h,036h,030h,020h,020h ;@000006C8
db 020h,030h,020h,031h,032h,038h,00Dh,00Ah ;@000006D0
db 031h,039h,032h,020h,020h,020h,030h,020h ;@000006D8
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@000006E0
db 020h,020h,020h,030h,020h,031h,032h,038h ;@000006E8
db 00Dh,00Ah,020h,020h,030h,020h,020h,033h ;@000006F0
db 032h,020h,031h,032h,038h,00Dh,00Ah,020h ;@000006F8
db 033h,032h,020h,020h,033h,032h,020h,031h ;@00000700
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@00000708
db 020h,033h,032h,020h,031h,032h,038h,00Dh ;@00000710
db 00Ah,020h,039h,036h,020h,020h,033h,032h ;@00000718
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@00000720
db 038h,020h,020h,033h,032h,020h,031h,032h ;@00000728
db 038h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000730
db 033h,032h,020h,031h,032h,038h,00Dh,00Ah ;@00000738
db 031h,039h,032h,020h,020h,033h,032h,020h ;@00000740
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@00000748
db 020h,020h,033h,032h,020h,031h,032h,038h ;@00000750
db 00Dh,00Ah,020h,020h,030h,020h,020h,036h ;@00000758
db 034h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000760
db 033h,032h,020h,020h,036h,034h,020h,031h ;@00000768
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@00000770
db 020h,036h,034h,020h,031h,032h,038h,00Dh ;@00000778
db 00Ah,020h,039h,036h,020h,020h,036h,034h ;@00000780
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@00000788
db 038h,020h,020h,036h,034h,020h,031h,032h ;@00000790
db 038h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000798
db 036h,034h,020h,031h,032h,038h,00Dh,00Ah ;@000007A0
db 031h,039h,032h,020h,020h,036h,034h,020h ;@000007A8
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@000007B0
db 020h,020h,036h,034h,020h,031h,032h,038h ;@000007B8
db 00Dh,00Ah,020h,020h,030h,020h,020h,039h ;@000007C0
db 036h,020h,031h,032h,038h,00Dh,00Ah,020h ;@000007C8
db 033h,032h,020h,020h,039h,036h,020h,031h ;@000007D0
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@000007D8
db 020h,039h,036h,020h,031h,032h,038h,00Dh ;@000007E0
db 00Ah,020h,039h,036h,020h,020h,039h,036h ;@000007E8
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@000007F0
db 038h,020h,020h,039h,036h,020h,031h,032h ;@000007F8
db 038h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000800
db 039h,036h,020h,031h,032h,038h,00Dh,00Ah ;@00000808
db 031h,039h,032h,020h,020h,039h,036h,020h ;@00000810
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@00000818
db 020h,020h,039h,036h,020h,031h,032h,038h ;@00000820
db 00Dh,00Ah,020h,020h,030h,020h,031h,032h ;@00000828
db 038h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000830
db 033h,032h,020h,031h,032h,038h,020h,031h ;@00000838
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@00000840
db 031h,032h,038h,020h,031h,032h,038h,00Dh ;@00000848
db 00Ah,020h,039h,036h,020h,031h,032h,038h ;@00000850
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@00000858
db 038h,020h,031h,032h,038h,020h,031h,032h ;@00000860
db 038h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000868
db 032h,038h,020h,031h,032h,038h,00Dh,00Ah ;@00000870
db 031h,039h,032h,020h,031h,032h,038h,020h ;@00000878
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@00000880
db 020h,031h,032h,038h,020h,031h,032h,038h ;@00000888
db 00Dh,00Ah,020h,020h,030h,020h,031h,036h ;@00000890
db 030h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000898
db 033h,032h,020h,031h,036h,030h,020h,031h ;@000008A0
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@000008A8
db 031h,036h,030h,020h,031h,032h,038h,00Dh ;@000008B0
db 00Ah,020h,039h,036h,020h,031h,036h,030h ;@000008B8
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@000008C0
db 038h,020h,031h,036h,030h,020h,031h,032h ;@000008C8
db 038h,00Dh,00Ah,031h,036h,030h,020h,031h ;@000008D0
db 036h,030h,020h,031h,032h,038h,00Dh,00Ah ;@000008D8
db 031h,039h,032h,020h,031h,036h,030h,020h ;@000008E0
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@000008E8
db 020h,031h,036h,030h,020h,031h,032h,038h ;@000008F0
db 00Dh,00Ah,020h,020h,030h,020h,031h,039h ;@000008F8
db 032h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000900
db 033h,032h,020h,031h,039h,032h,020h,031h ;@00000908
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@00000910
db 031h,039h,032h,020h,031h,032h,038h,00Dh ;@00000918
db 00Ah,020h,039h,036h,020h,031h,039h,032h ;@00000920
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@00000928
db 038h,020h,031h,039h,032h,020h,031h,032h ;@00000930
db 038h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000938
db 039h,032h,020h,031h,032h,038h,00Dh,00Ah ;@00000940
db 031h,039h,032h,020h,031h,039h,032h,020h ;@00000948
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@00000950
db 020h,031h,039h,032h,020h,031h,032h,038h ;@00000958
db 00Dh,00Ah,020h,020h,030h,020h,032h,032h ;@00000960
db 034h,020h,031h,032h,038h,00Dh,00Ah,020h ;@00000968
db 033h,032h,020h,032h,032h,034h,020h,031h ;@00000970
db 032h,038h,00Dh,00Ah,020h,036h,034h,020h ;@00000978
db 032h,032h,034h,020h,031h,032h,038h,00Dh ;@00000980
db 00Ah,020h,039h,036h,020h,032h,032h,034h ;@00000988
db 020h,031h,032h,038h,00Dh,00Ah,031h,032h ;@00000990
db 038h,020h,032h,032h,034h,020h,031h,032h ;@00000998
db 038h,00Dh,00Ah,031h,036h,030h,020h,032h ;@000009A0
db 032h,034h,020h,031h,032h,038h,00Dh,00Ah ;@000009A8
db 031h,039h,032h,020h,032h,032h,034h,020h ;@000009B0
db 031h,032h,038h,00Dh,00Ah,032h,032h,034h ;@000009B8
db 020h,032h,032h,034h,020h,031h,032h,038h ;@000009C0
db 00Dh,00Ah,020h,020h,030h,020h,020h,020h ;@000009C8
db 030h,020h,031h,039h,032h,00Dh,00Ah,020h ;@000009D0
db 033h,032h,020h,020h,020h,030h,020h,031h ;@000009D8
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@000009E0
db 020h,020h,030h,020h,031h,039h,032h,00Dh ;@000009E8
db 00Ah,020h,039h,036h,020h,020h,020h,030h ;@000009F0
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@000009F8
db 038h,020h,020h,020h,030h,020h,031h,039h ;@00000A00
db 032h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000A08
db 020h,030h,020h,031h,039h,032h,00Dh,00Ah ;@00000A10
db 031h,039h,032h,020h,020h,020h,030h,020h ;@00000A18
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000A20
db 020h,020h,020h,030h,020h,031h,039h,032h ;@00000A28
db 00Dh,00Ah,020h,020h,030h,020h,020h,033h ;@00000A30
db 032h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000A38
db 033h,032h,020h,020h,033h,032h,020h,031h ;@00000A40
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000A48
db 020h,033h,032h,020h,031h,039h,032h,00Dh ;@00000A50
db 00Ah,020h,039h,036h,020h,020h,033h,032h ;@00000A58
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000A60
db 038h,020h,020h,033h,032h,020h,031h,039h ;@00000A68
db 032h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000A70
db 033h,032h,020h,031h,039h,032h,00Dh,00Ah ;@00000A78
db 031h,039h,032h,020h,020h,033h,032h,020h ;@00000A80
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000A88
db 020h,020h,033h,032h,020h,031h,039h,032h ;@00000A90
db 00Dh,00Ah,020h,020h,030h,020h,020h,036h ;@00000A98
db 034h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000AA0
db 033h,032h,020h,020h,036h,034h,020h,031h ;@00000AA8
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000AB0
db 020h,036h,034h,020h,031h,039h,032h,00Dh ;@00000AB8
db 00Ah,020h,039h,036h,020h,020h,036h,034h ;@00000AC0
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000AC8
db 038h,020h,020h,036h,034h,020h,031h,039h ;@00000AD0
db 032h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000AD8
db 036h,034h,020h,031h,039h,032h,00Dh,00Ah ;@00000AE0
db 031h,039h,032h,020h,020h,036h,034h,020h ;@00000AE8
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000AF0
db 020h,020h,036h,034h,020h,031h,039h,032h ;@00000AF8
db 00Dh,00Ah,020h,020h,030h,020h,020h,039h ;@00000B00
db 036h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000B08
db 033h,032h,020h,020h,039h,036h,020h,031h ;@00000B10
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000B18
db 020h,039h,036h,020h,031h,039h,032h,00Dh ;@00000B20
db 00Ah,020h,039h,036h,020h,020h,039h,036h ;@00000B28
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000B30
db 038h,020h,020h,039h,036h,020h,031h,039h ;@00000B38
db 032h,00Dh,00Ah,031h,036h,030h,020h,020h ;@00000B40
db 039h,036h,020h,031h,039h,032h,00Dh,00Ah ;@00000B48
db 031h,039h,032h,020h,020h,039h,036h,020h ;@00000B50
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000B58
db 020h,020h,039h,036h,020h,031h,039h,032h ;@00000B60
db 00Dh,00Ah,020h,020h,030h,020h,031h,032h ;@00000B68
db 038h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000B70
db 033h,032h,020h,031h,032h,038h,020h,031h ;@00000B78
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000B80
db 031h,032h,038h,020h,031h,039h,032h,00Dh ;@00000B88
db 00Ah,020h,039h,036h,020h,031h,032h,038h ;@00000B90
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000B98
db 038h,020h,031h,032h,038h,020h,031h,039h ;@00000BA0
db 032h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000BA8
db 032h,038h,020h,031h,039h,032h,00Dh,00Ah ;@00000BB0
db 031h,039h,032h,020h,031h,032h,038h,020h ;@00000BB8
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000BC0
db 020h,031h,032h,038h,020h,031h,039h,032h ;@00000BC8
db 00Dh,00Ah,020h,020h,030h,020h,031h,036h ;@00000BD0
db 030h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000BD8
db 033h,032h,020h,031h,036h,030h,020h,031h ;@00000BE0
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000BE8
db 031h,036h,030h,020h,031h,039h,032h,00Dh ;@00000BF0
db 00Ah,020h,039h,036h,020h,031h,036h,030h ;@00000BF8
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000C00
db 038h,020h,031h,036h,030h,020h,031h,039h ;@00000C08
db 032h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000C10
db 036h,030h,020h,031h,039h,032h,00Dh,00Ah ;@00000C18
db 031h,039h,032h,020h,031h,036h,030h,020h ;@00000C20
db 031h,039h,032h,00Dh,00Ah,032h,032h,034h ;@00000C28
db 020h,031h,036h,030h,020h,031h,039h,032h ;@00000C30
db 00Dh,00Ah,020h,020h,030h,020h,031h,039h ;@00000C38
db 032h,020h,031h,039h,032h,00Dh,00Ah,020h ;@00000C40
db 033h,032h,020h,031h,039h,032h,020h,031h ;@00000C48
db 039h,032h,00Dh,00Ah,020h,036h,034h,020h ;@00000C50
db 031h,039h,032h,020h,031h,039h,032h,00Dh ;@00000C58
db 00Ah,020h,039h,036h,020h,031h,039h,032h ;@00000C60
db 020h,031h,039h,032h,00Dh,00Ah,031h,032h ;@00000C68
db 038h,020h,031h,039h,032h,020h,031h,039h ;@00000C70
db 032h,00Dh,00Ah,031h,036h,030h,020h,031h ;@00000C78
db 039h,032h,020h,031h,039h,032h,00Dh,00Ah ;@00000C80
db 032h,035h,035h,020h,032h,035h,031h,020h ;@00000C88
db 032h,034h,030h,00Dh,00Ah,031h,036h,030h ;@00000C90
db 020h,031h,036h,030h,020h,031h,036h,034h ;@00000C98
db 00Dh,00Ah,031h,032h,038h,020h,031h,032h ;@00000CA0
db 038h,020h,031h,032h,038h,00Dh,00Ah,032h ;@00000CA8
db 035h,035h,020h,020h,020h,030h,020h,020h ;@00000CB0
db 020h,030h,00Dh,00Ah,020h,020h,030h,020h ;@00000CB8
db 032h,035h,035h,020h,020h,020h,030h,00Dh ;@00000CC0
db 00Ah,032h,035h,035h,020h,032h,035h,035h ;@00000CC8
db 020h,020h,020h,030h,00Dh,00Ah,020h,020h ;@00000CD0
db 030h,020h,020h,020h,030h,020h,032h,035h ;@00000CD8
db 035h,00Dh,00Ah,032h,035h,035h,020h,020h ;@00000CE0
db 020h,030h,020h,032h,035h,035h,00Dh,00Ah ;@00000CE8
db 020h,020h,030h,020h,032h,035h,035h,020h ;@00000CF0
db 032h,035h,035h,00Dh,00Ah,032h,035h,035h ;@00000CF8
db 020h,032h,035h,035h,020h,032h,035h,035h ;@00000D00
db 00Dh,00Ah                               ;@00000D08
	
   end 	