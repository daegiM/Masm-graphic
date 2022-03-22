




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
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 088h, 088h, 088h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 032h, 037h, 033h, 033h, 037h
	db 033h, 037h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 073h, 033h, 07Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0F8h, 043h, 019h, 0A3h, 033h, 013h, 033h, 033h, 033h, 039h, 099h, 099h, 093h, 093h, 033h
	db 099h, 093h, 033h, 031h, 093h, 030h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 033h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 039h, 011h, 093h, 033h, 033h, 00Fh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 012h, 031h
	db 022h, 033h, 03Bh, 033h, 033h, 03Bh, 033h, 033h, 03Bh, 033h, 033h, 033h, 033h, 03Bh, 0BBh, 0BBh
	db 0BBh, 0B3h, 033h, 033h, 033h, 0B3h, 033h, 033h, 033h, 033h, 033h, 030h, 08Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 081h, 0A9h, 03Ah, 099h, 032h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B0h, 033h, 033h
	db 033h, 033h, 033h, 033h, 077h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 00Ah, 099h, 033h
	db 033h, 030h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 003h, 033h, 033h, 033h, 033h, 033h, 037h, 08Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F6h, 033h, 03Ah, 099h, 032h, 033h, 003h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 033h, 033h, 033h, 037h, 077h, 077h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 033h, 039h, 093h, 023h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 033h
	db 074h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 063h, 099h, 099h, 09Ah, 030h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 033h, 037h, 04Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 063h, 099h, 093h, 023h, 003h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 033h, 037h, 033h
	db 033h, 037h, 077h, 077h, 077h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 073h, 099h, 092h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 033h, 033h, 033h, 033h, 008h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 081h, 023h, 031h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 0B8h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 003h, 033h, 033h, 033h, 030h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F8h, 087h, 033h, 019h, 021h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h
	db 088h, 088h, 0B8h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 000h, 033h, 029h, 033h, 04Fh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 082h, 099h, 032h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h
	db 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 033h, 039h, 033h, 030h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 039h, 032h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 08Bh, 0BBh, 0BBh, 033h, 033h, 033h, 033h, 07Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 099h, 023h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h
	db 088h, 088h, 088h, 088h, 088h, 08Bh, 0B8h, 088h, 08Bh, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 088h, 0BBh, 0BBh, 032h, 033h, 033h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F1h, 0A9h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0C7h, 0BBh, 0B8h, 0BBh, 0BBh, 033h, 099h, 033h, 033h, 07Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 039h, 013h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0C7h, 0BBh
	db 0BBh, 0BBh, 0BBh, 0B1h, 099h, 093h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 099h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 08Bh, 0BBh, 0B7h, 087h, 0BBh, 0BBh, 0BBh, 0BBh, 0B9h, 029h, 0A3h, 033h, 07Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 099h, 023h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0B8h, 088h, 0BBh
	db 0BBh, 0BBh, 0BBh, 033h, 039h, 093h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 02Ah, 037h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 0BBh
	db 0BBh, 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 088h, 0BBh, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 032h, 099h, 093h, 033h, 07Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 087h, 077h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 08Bh, 0BBh, 0BBh, 0B8h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh
	db 0BBh, 0B2h, 033h, 093h, 033h, 033h, 037h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 044h, 00Bh, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 031h, 099h, 093h, 033h, 030h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h
	db 077h, 07Bh, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh
	db 0BBh, 029h, 099h, 033h, 033h, 037h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 077h, 078h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0B3h, 023h, 099h, 033h, 037h, 07Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h
	db 077h, 078h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 0B8h, 088h, 08Bh, 0BBh
	db 0B3h, 033h, 033h, 037h, 0F8h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 077h, 077h, 08Bh, 0BBh, 0B8h, 08Bh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 0BBh, 0BBh, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 08Bh, 0B8h, 08Fh, 0CBh, 0BBh, 033h, 033h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h
	db 077h, 073h, 0B7h, 07Bh, 017h, 083h, 0B8h, 088h, 088h, 088h, 07Fh, 088h, 088h, 088h, 088h, 088h
	db 08Fh, 08Bh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 0BBh, 0BBh, 087h, 0BBh, 073h
	db 033h, 034h, 077h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h
	db 007h, 077h, 077h, 070h, 078h, 077h, 078h, 077h, 077h, 071h, 010h, 011h, 031h, 073h, 003h, 033h
	db 033h, 077h, 037h, 077h, 077h, 077h, 077h, 077h, 077h, 078h, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h
	db 088h, 088h, 088h, 0BBh, 0BBh, 0BBh, 0B4h, 033h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 007h, 077h, 033h, 077h, 077h, 077h, 077h, 077h, 077h
	db 078h, 019h, 000h, 000h, 000h, 000h, 000h, 000h, 007h, 073h, 077h, 077h, 077h, 077h, 077h, 077h
	db 077h, 07Fh, 08Bh, 0BBh, 0BBh, 08Fh, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 033h
	db 033h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 077h
	db 073h, 033h, 077h, 077h, 077h, 077h, 077h, 078h, 073h, 011h, 000h, 000h, 000h, 000h, 010h, 090h
	db 011h, 073h, 037h, 077h, 077h, 077h, 077h, 077h, 077h, 078h, 0F8h, 0B8h, 0BBh, 0B3h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0CBh, 0BBh, 0B7h, 033h, 037h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0F8h, 073h, 037h, 073h, 077h, 077h, 077h, 077h, 087h, 088h, 088h, 088h, 088h, 08Fh
	db 003h, 033h, 031h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 077h, 077h, 077h, 077h, 082h
	db 056h, 015h, 031h, 011h, 000h, 013h, 033h, 033h, 03Bh, 0BBh, 0BBh, 03Bh, 0B8h, 0BBh, 0B7h, 033h
	db 037h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 080h, 033h, 033h, 033h, 037h, 07Bh
	db 0BBh, 088h, 088h, 088h, 088h, 088h, 0BBh, 088h, 0BBh, 0B3h, 033h, 039h, 039h, 099h, 099h, 099h
	db 033h, 033h, 033h, 037h, 077h, 077h, 077h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0B3h, 033h
	db 03Bh, 0BBh, 0BBh, 033h, 037h, 0FBh, 0B7h, 073h, 037h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 003h, 033h, 033h, 033h, 033h, 073h, 077h, 0B8h, 088h, 088h, 088h, 0B3h, 0BBh, 037h
	db 0B3h, 033h, 033h, 033h, 099h, 099h, 099h, 099h, 099h, 033h, 033h, 033h, 073h, 073h, 073h, 011h
	db 011h, 010h, 000h, 000h, 000h, 000h, 09Bh, 033h, 037h, 073h, 0B3h, 033h, 033h, 078h, 077h, 077h
	db 077h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 033h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 037h, 077h, 087h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 0A3h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 011h, 019h, 001h, 001h, 010h, 000h
	db 019h, 011h, 013h, 033h, 033h, 037h, 073h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 08Ch, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 073h, 033h, 033h, 031h
	db 011h, 031h, 011h, 011h, 011h, 011h, 011h, 011h, 013h, 033h, 033h, 033h, 033h, 039h, 099h, 093h
	db 099h, 033h, 033h, 03Ah, 011h, 019h, 000h, 000h, 000h, 000h, 001h, 011h, 013h, 037h, 077h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 08Ch, 033h, 099h, 0A3h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h
	db 013h, 033h, 033h, 033h, 033h, 039h, 099h, 099h, 099h, 033h, 033h, 031h, 011h, 011h, 010h, 000h
	db 000h, 000h, 001h, 011h, 013h, 037h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0F7h, 033h, 031h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 011h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 033h, 031h, 012h, 032h, 013h
	db 01Ah, 033h, 013h, 013h, 031h, 031h, 011h, 001h, 011h, 021h, 091h, 033h, 033h, 013h, 077h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 001h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 013h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 011h, 078h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0F7h, 037h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 011h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 001h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 001h, 011h
	db 011h, 011h, 011h, 011h, 033h, 033h, 039h, 099h, 099h, 099h, 0A3h, 033h, 033h, 011h, 018h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 077h, 033h
	db 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 0A3h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 031h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 013h, 033h
	db 033h, 033h, 033h, 033h, 033h, 011h, 013h, 0FFh, 0FFh, 0F8h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 033h, 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h
	db 033h, 033h, 033h, 033h, 0C7h, 077h, 077h, 077h, 077h, 077h, 077h, 033h, 033h, 031h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 013h, 033h, 033h, 033h, 033h, 077h
	db 077h, 073h, 077h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 071h
	db 033h, 011h, 011h, 011h, 000h, 011h, 011h, 013h, 033h, 033h, 033h, 033h, 03Bh, 0BBh, 0BBh, 0BBh
	db 0B3h, 037h, 073h, 033h, 033h, 031h, 011h, 011h, 011h, 019h, 001h, 011h, 000h, 001h, 011h, 011h
	db 011h, 011h, 013h, 033h, 033h, 033h, 033h, 078h, 033h, 033h, 033h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 083h, 011h, 011h, 011h, 011h, 011h, 011h, 013h
	db 033h, 033h, 033h, 033h, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 073h, 033h, 033h, 033h, 033h, 033h
	db 033h, 019h, 000h, 011h, 010h, 091h, 011h, 011h, 011h, 011h, 011h, 031h, 013h, 033h, 031h, 077h
	db 033h, 033h, 033h, 078h, 088h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F2h, 011h, 011h, 011h, 091h, 013h, 033h, 033h, 077h, 037h, 0CBh, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 033h, 0B3h, 077h, 077h, 033h, 077h, 073h, 090h, 000h, 099h, 011h, 090h, 011h
	db 091h, 011h, 011h, 011h, 011h, 011h, 011h, 033h, 033h, 033h, 033h, 033h, 0A1h, 028h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F3h, 000h, 011h, 011h, 001h, 013h, 033h, 033h
	db 037h, 034h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 043h, 033h, 033h
	db 037h, 039h, 000h, 000h, 000h, 011h, 000h, 011h, 000h, 011h, 011h, 011h, 011h, 011h, 011h, 033h
	db 033h, 033h, 033h, 033h, 011h, 018h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 083h, 011h, 011h, 011h, 001h, 013h, 033h, 033h, 00Bh, 03Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B4h, 077h, 033h, 033h, 033h, 011h, 011h, 011h, 009h, 009h, 011h
	db 011h, 011h, 011h, 011h, 031h, 011h, 013h, 033h, 033h, 033h, 033h, 031h, 011h, 032h, 08Fh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 011h, 010h, 011h, 001h, 033h, 073h, 073h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 08Bh, 0BBh, 0BBh, 0B8h, 0BBh, 0BBh, 078h, 0B3h
	db 0B7h, 073h, 033h, 031h, 011h, 001h, 091h, 011h, 011h, 011h, 011h, 011h, 083h, 013h, 033h, 033h
	db 033h, 033h, 033h, 013h, 011h, 033h, 030h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 003h, 000h, 000h, 011h, 011h, 037h, 077h, 037h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h
	db 08Bh, 0BBh, 0B8h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 073h, 033h, 033h, 011h, 001h, 011h, 011h
	db 011h, 011h, 011h, 011h, 077h, 033h, 031h, 031h, 011h, 033h, 031h, 013h, 031h, 013h, 033h, 08Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 001h, 011h, 011h, 011h, 037h, 077h, 073h
	db 033h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 0BBh, 034h, 07Bh, 0BBh, 0BBh, 0BBh
	db 0B7h, 073h, 033h, 031h, 011h, 001h, 011h, 011h, 011h, 011h, 011h, 000h, 087h, 031h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 033h, 037h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 080h
	db 033h, 091h, 011h, 013h, 033h, 03Ch, 0BBh, 0C3h, 033h, 030h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0B8h, 088h, 083h, 033h, 033h, 03Bh, 0BBh, 0C7h, 033h, 033h, 033h, 031h, 011h, 000h, 091h, 011h
	db 011h, 011h, 011h, 000h, 087h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 036h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 003h, 033h, 000h, 091h, 013h, 033h, 03Ch, 08Bh, 073h
	db 033h, 033h, 03Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 0F7h, 033h, 033h, 00Bh, 0BBh, 073h
	db 033h, 033h, 033h, 031h, 011h, 000h, 091h, 011h, 011h, 011h, 011h, 009h, 087h, 073h, 011h, 011h
	db 011h, 013h, 011h, 011h, 011h, 028h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F3h, 033h
	db 031h, 000h, 001h, 013h, 033h, 03Ch, 0FBh, 074h, 033h, 030h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0B8h, 088h, 083h, 033h, 033h, 03Bh, 0BBh, 0B0h, 033h, 033h, 033h, 031h, 011h, 011h, 011h, 011h
	db 011h, 011h, 010h, 011h, 07Fh, 08Fh, 087h, 087h, 077h, 087h, 087h, 077h, 087h, 07Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 011h, 011h, 011h, 001h, 033h, 033h, 03Ch, 0BBh, 0B7h
	db 077h, 07Bh, 0BBh, 0BBh, 0B8h, 088h, 0BBh, 0BBh, 0BBh, 08Bh, 0B7h, 073h, 033h, 0BBh, 0BBh, 0BBh
	db 0CBh, 033h, 033h, 033h, 011h, 011h, 011h, 033h, 011h, 011h, 010h, 011h, 07Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 031h, 001h
	db 000h, 011h, 001h, 033h, 033h, 077h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 011h, 011h, 013h, 033h
	db 011h, 011h, 010h, 011h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 031h, 001h, 011h, 011h, 001h, 033h, 033h, 077h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0C8h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 033h, 033h, 033h, 011h, 011h, 013h, 033h, 011h, 011h, 011h, 011h, 07Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 083h, 011h, 011h, 011h
	db 011h, 010h, 001h, 037h, 077h, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 08Bh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F3h, 021h, 001h, 000h, 019h, 001h, 000h, 011h, 037h, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 033h, 033h, 031h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 07Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 031h, 011h, 091h, 011h
	db 001h, 011h, 011h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 08Bh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 073h, 033h, 031h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 04Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0F8h, 033h, 031h, 011h, 001h, 010h, 011h, 013h, 013h, 077h, 0BBh, 0BBh, 0BBh, 0BBh
	db 088h, 0B8h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 037h, 073h, 013h, 011h, 019h, 009h, 001h, 011h, 011h, 011h, 011h, 019h, 02Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 033h, 031h, 011h, 001h, 010h
	db 011h, 033h, 033h, 077h, 08Bh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 070h, 037h, 073h, 011h, 011h, 010h, 000h, 091h, 011h
	db 011h, 011h, 011h, 009h, 00Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 080h, 033h, 031h, 010h, 011h, 011h, 011h, 033h, 033h, 077h, 0BBh, 0BBh, 0BBh, 0B8h
	db 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h
	db 037h, 073h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 00Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 003h, 033h, 031h, 011h, 011h, 011h
	db 013h, 037h, 033h, 07Bh, 0BBh, 0BBh, 0B8h, 088h, 088h, 088h, 088h, 088h, 088h, 0B8h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 037h, 033h, 013h, 031h, 033h, 031h, 033h, 033h
	db 011h, 011h, 011h, 011h, 01Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0F0h, 033h, 033h, 033h, 011h, 013h, 033h, 033h, 07Bh, 0B7h, 0BBh, 0BBh, 0B8h, 088h, 088h
	db 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h
	db 037h, 073h, 033h, 033h, 033h, 033h, 037h, 073h, 031h, 011h, 011h, 011h, 01Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 083h, 033h, 033h, 033h, 011h, 013h, 033h
	db 033h, 03Bh, 0BBh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 037h, 033h, 033h, 033h, 033h, 033h, 033h, 033h
	db 031h, 011h, 011h, 011h, 008h, 078h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F7h, 033h, 033h, 033h, 031h, 011h, 033h, 030h, 030h, 03Bh, 0BBh, 0B3h, 037h, 088h, 088h, 088h
	db 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h
	db 038h, 073h, 033h, 033h, 033h, 033h, 033h, 033h, 031h, 011h, 011h, 010h, 017h, 077h, 00Fh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 033h, 030h, 001h, 013h, 033h, 033h, 0CBh
	db 0BBh, 0BBh, 0BBh, 047h, 077h, 03Bh, 088h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 088h
	db 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0C7h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h
	db 031h, 011h, 011h, 011h, 013h, 077h, 070h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 080h
	db 033h, 033h, 0B0h, 001h, 013h, 033h, 037h, 07Bh, 0BBh, 0BBh, 0BBh, 073h, 037h, 07Bh, 088h, 088h
	db 088h, 088h, 088h, 08Ch, 0BBh, 0BBh, 0BBh, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 073h
	db 033h, 033h, 088h, 073h, 033h, 033h, 033h, 033h, 031h, 011h, 011h, 010h, 017h, 087h, 077h, 08Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 003h, 033h, 03Ah, 011h, 011h, 013h, 033h, 034h, 07Bh
	db 0BBh, 0BBh, 0BBh, 077h, 077h, 077h, 088h, 088h, 088h, 088h, 088h, 08Bh, 0B3h, 034h, 08Bh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 037h, 073h, 037h, 073h, 033h, 033h, 033h, 033h
	db 031h, 011h, 011h, 011h, 011h, 077h, 073h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 033h
	db 010h, 001h, 013h, 033h, 037h, 077h, 037h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 037h, 04Bh
	db 0B8h, 0BBh, 0BBh, 0BBh, 043h, 033h, 034h, 033h, 047h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0B3h, 037h, 073h, 033h, 033h, 033h, 033h, 031h, 011h, 011h, 011h, 011h, 001h, 001h, 038h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 03Ah, 010h, 001h, 013h, 033h, 033h, 073h, 037h, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 073h, 037h, 0BBh, 0BBh, 0BBh, 0B3h, 033h, 033h, 033h, 033h
	db 037h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 037h, 073h, 033h, 033h, 033h, 033h
	db 031h, 011h, 011h, 011h, 011h, 009h, 001h, 037h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F3h, 033h, 031h
	db 010h, 011h, 013h, 033h, 037h, 073h, 0C3h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B4h, 077h, 077h
	db 0B7h, 073h, 03Bh, 0B3h, 033h, 033h, 077h, 074h, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0B3h, 037h, 073h, 033h, 033h, 031h, 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 037h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 082h, 090h, 011h, 011h, 011h, 0A3h, 033h, 037h, 0BBh, 0B8h, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 033h, 033h, 033h, 033h, 037h, 087h, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B3h, 038h, 073h, 033h, 033h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 011h, 032h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 073h, 011h, 011h
	db 010h, 001h, 0A3h, 033h, 077h, 0BBh, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 077h
	db 033h, 033h, 033h, 033h, 03Bh, 0BBh, 0BBh, 0B8h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0B7h, 073h, 073h, 033h, 03Ah, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 001h, 033h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 073h, 011h, 011h, 011h, 001h, 0A3h, 033h, 077h, 0BBh, 0BBh, 0BBh
	db 0BBh, 073h, 037h, 08Bh, 0BBh, 0BBh, 0BBh, 074h, 033h, 033h, 033h, 033h, 0CBh, 0BBh, 0BBh, 0B8h
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 037h, 073h, 033h, 033h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 010h, 001h, 033h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 087h, 073h, 011h, 011h
	db 011h, 011h, 033h, 033h, 077h, 0BBh, 0BBh, 0BBh, 0BCh, 033h, 033h, 0CBh, 0BBh, 0BBh, 0BBh, 0B7h
	db 077h, 077h, 077h, 0BCh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0B7h, 033h, 033h, 011h, 011h, 011h, 001h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 010h
	db 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 087h, 083h, 011h, 011h, 011h, 011h, 033h, 033h, 078h, 0BBh, 0BBh, 0BBh
	db 077h, 033h, 033h, 078h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 07Bh, 077h, 033h, 073h, 010h, 000h, 000h, 000h
	db 001h, 011h, 011h, 011h, 011h, 011h, 011h, 000h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 087h, 073h, 011h, 011h
	db 011h, 011h, 033h, 033h, 078h, 0BBh, 0BBh, 0BBh, 0BCh, 033h, 033h, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 077h, 07Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0CBh, 077h, 033h, 033h, 019h, 010h, 010h, 000h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h
	db 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 080h, 039h, 009h, 011h, 011h, 011h, 033h, 033h, 037h, 0BBh, 0BBh, 0BBh
	db 0BBh, 0C7h, 077h, 077h, 03Ch, 03Bh, 0BBh, 0BBh, 088h, 088h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh
	db 073h, 033h, 073h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 08Bh, 033h, 033h, 031h, 011h, 011h, 011h, 011h
	db 011h, 011h, 013h, 033h, 031h, 010h, 011h, 011h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 023h, 000h, 001h
	db 011h, 011h, 033h, 033h, 037h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 073h, 033h, 0CBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0B8h, 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 073h, 033h, 031h, 011h, 011h, 011h, 011h, 011h, 011h, 013h, 033h, 031h, 011h, 011h, 010h
	db 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 071h, 001h, 011h, 011h, 011h, 0A3h, 033h, 037h, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 04Ch, 034h, 0CBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 030h, 033h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0C3h, 033h, 031h, 011h, 011h, 011h, 011h
	db 011h, 011h, 013h, 033h, 031h, 011h, 011h, 010h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F2h, 033h, 033h
	db 011h, 011h, 011h, 011h, 037h, 053h, 007h, 0BBh, 0BBh, 088h, 0BBh, 0BBh, 0CBh, 0BBh, 0BBh, 0B7h
	db 077h, 077h, 0CCh, 07Ch, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 077h, 077h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0B7h, 073h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 017h
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 003h, 033h, 010h, 000h, 001h, 001h, 013h, 033h, 034h, 0BBh
	db 0BBh, 08Ch, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 070h, 033h, 033h, 033h, 033h, 03Bh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 073h, 011h, 011h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 011h, 011h, 01Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 080h, 033h
	db 030h, 000h, 001h, 001h, 033h, 033h, 037h, 0CBh, 0BBh, 0BBh, 0BBh, 0B8h, 08Bh, 0BBh, 0BBh, 0B7h
	db 033h, 033h, 033h, 03Ch, 03Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BCh, 073h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 08Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 037h
	db 037h, 0BBh, 0BBh, 0B8h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 073h, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 043h, 033h, 033h, 011h, 011h, 010h, 091h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 000h, 090h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 033h
	db 033h, 033h, 033h, 033h, 073h, 031h, 033h, 033h, 033h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 033h
	db 033h, 033h, 011h, 011h, 010h, 001h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 000h, 007h, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 033h, 033h, 033h, 033h, 011h, 033h, 033h
	db 033h, 03Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 08Bh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 033h, 033h, 033h, 011h, 011h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 011h, 011h, 091h, 07Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0F8h, 001h, 013h, 085h, 073h, 053h, 033h, 033h, 033h, 034h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh
	db 088h, 088h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 047h, 030h, 033h, 033h, 033h
	db 033h, 033h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 017h, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 088h
	db 033h, 033h, 033h, 03Bh, 0BBh, 0BBh, 0BBh, 0B8h, 088h, 088h, 08Bh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 0BBh, 0BBh, 0B3h, 033h, 033h, 033h, 033h, 033h, 033h, 031h, 011h, 011h, 010h, 011h, 011h, 011h
	db 011h, 011h, 011h, 001h, 010h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 073h, 033h, 033h, 0CBh, 0BBh, 0BBh, 0BBh, 0BBh
	db 088h, 088h, 0B8h, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0BBh, 0B7h, 037h, 038h, 083h, 033h, 033h
	db 033h, 031h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 011h, 010h, 011h, 01Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F3h, 033h, 033h, 030h, 0B4h, 0BBh, 0BBh, 0BBh, 088h, 08Bh, 037h, 0B7h, 033h, 037h, 033h, 033h
	db 073h, 037h, 077h, 033h, 033h, 033h, 033h, 033h, 093h, 011h, 091h, 011h, 011h, 011h, 011h, 011h
	db 011h, 011h, 011h, 078h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 033h, 033h, 033h, 033h, 00Bh, 0BBh, 0BBh
	db 0BBh, 0B7h, 043h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 039h
	db 099h, 099h, 069h, 018h, 0F8h, 099h, 088h, 0F8h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 073h, 033h, 033h, 033h, 034h, 0BBh, 07Bh, 0BBh, 0B7h, 033h, 083h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 039h, 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 072h, 033h, 033h, 033h, 033h, 033h, 037h
	db 077h, 077h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 099h
	db 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 095h, 077h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 099h, 099h, 099h, 099h, 099h, 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 099h, 099h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 099h, 099h, 099h, 099h, 099h
	db 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0F9h, 099h, 097h, 077h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h
	db 033h, 033h, 033h, 099h, 091h, 099h, 099h, 099h, 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 019h, 099h, 091h, 088h, 088h, 088h, 087h, 077h
	db 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 033h, 08Fh, 088h, 069h, 099h, 099h
	db 099h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 081h
	db 091h, 091h, 096h, 0F8h, 088h, 088h, 08Fh, 087h, 033h, 033h, 077h, 077h, 033h, 038h, 073h, 033h
	db 033h, 033h, 037h, 0F8h, 08Fh, 076h, 099h, 099h, 019h, 099h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 07Eh, 078h, 078h, 067h, 088h, 088h, 088h, 088h, 0F7h
	db 077h, 077h, 077h, 077h, 077h, 077h, 077h, 077h, 073h, 033h, 037h, 088h, 088h, 087h, 087h, 077h
	db 081h, 069h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 007h, 0F8h
	db 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 08Fh, 077h, 077h, 077h, 077h, 077h, 077h
	db 077h, 077h, 07Fh, 088h, 088h, 088h, 088h, 088h, 088h, 0F6h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F9h, 06Fh, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0F0h, 077h, 077h, 0F8h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h
	db 088h, 0F7h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 079h, 078h, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 087h, 088h, 088h, 088h, 088h, 08Fh, 008h, 077h, 088h, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 0F0h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 099h, 078h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 077h, 088h
	db 088h, 088h, 088h, 088h, 087h, 077h, 0F8h, 088h, 088h, 088h, 088h, 077h, 087h, 088h, 088h, 088h
	db 088h, 080h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 059h, 099h, 068h, 088h
	db 088h, 088h, 088h, 088h, 088h, 087h, 077h, 078h, 088h, 088h, 088h, 088h, 087h, 077h, 088h, 088h
	db 088h, 088h, 088h, 077h, 077h, 088h, 088h, 088h, 088h, 070h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 077h, 077h, 077h, 075h, 083h, 077h, 08Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0F2h, 097h, 017h, 078h, 088h, 088h, 088h, 088h, 088h, 088h, 087h, 067h, 078h
	db 088h, 088h, 088h, 088h, 086h, 0E7h, 088h, 088h, 088h, 088h, 088h, 077h, 077h, 088h, 088h, 08Fh
	db 078h, 074h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 081h, 055h
	db 055h, 055h, 057h, 077h, 033h, 033h, 037h, 033h, 033h, 033h, 037h, 033h, 037h, 088h, 088h, 088h
	db 088h, 077h, 077h, 088h, 088h, 0F6h, 099h, 078h, 088h, 088h, 088h, 07Fh, 008h, 097h, 088h, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 088h, 087h, 077h, 070h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F7h, 046h, 0CCh, 0CCh, 0CCh, 07Bh, 066h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 077h, 0F8h, 088h, 088h, 088h, 077h, 077h, 078h, 08Fh, 076h, 099h, 067h
	db 088h, 088h, 088h, 0F1h, 089h, 097h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 087h
	db 077h, 073h, 022h, 078h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FCh, 0CCh, 076h
	db 066h, 067h, 076h, 066h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 037h, 088h, 088h, 088h
	db 088h, 077h, 077h, 088h, 088h, 087h, 077h, 077h, 0F8h, 088h, 08Fh, 077h, 077h, 077h, 07Fh, 088h
	db 088h, 088h, 088h, 088h, 088h, 088h, 088h, 087h, 077h, 073h, 022h, 022h, 022h, 022h, 028h, 08Fh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 080h, 00Ah, 0B3h, 062h, 026h, 066h, 066h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 033h, 077h, 037h, 088h, 088h, 077h, 077h, 078h, 088h, 088h, 088h, 08Fh
	db 07Eh, 086h, 071h, 077h, 078h, 087h, 0F8h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 088h, 087h
	db 077h, 073h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 038h, 027h, 077h, 077h, 077h, 077h, 0E7h
	db 076h, 078h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F8h, 002h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 037h, 077h
	db 077h, 077h, 077h, 077h, 077h, 088h, 088h, 088h, 069h, 099h, 099h, 070h, 0F8h, 088h, 087h, 077h
	db 078h, 088h, 088h, 088h, 088h, 088h, 088h, 0F7h, 077h, 073h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 026h, 077h, 088h, 08Fh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0F7h, 0F0h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 037h, 077h, 077h, 077h, 077h, 077h, 077h, 087h, 0FFh, 078h
	db 077h, 077h, 079h, 070h, 0F8h, 088h, 087h, 077h, 078h, 088h, 088h, 088h, 088h, 088h, 088h, 077h
	db 077h, 073h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 078h, 088h, 087h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0F0h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 033h, 033h
	db 083h, 033h, 037h, 08Fh, 088h, 08Fh, 0FFh, 0F8h, 088h, 088h, 088h, 0F8h, 088h, 088h, 088h, 088h
	db 088h, 088h, 088h, 0F8h, 08Fh, 088h, 088h, 077h, 033h, 033h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 077h, 077h, 077h, 077h, 07Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0F2h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 028h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 023h, 022h, 022h, 023h, 088h, 08Fh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 078h, 088h, 088h, 088h, 087h, 077h, 077h, 077h, 077h, 07Fh, 087h, 073h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 022h, 022h, 022h, 020h, 08Fh, 0FFh, 0FFh, 0FFh, 0F2h, 022h, 022h, 022h, 022h, 022h
	db 022h, 026h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h
	db 022h, 022h, 026h, 088h, 088h, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 078h, 088h, 088h, 088h
	db 088h, 077h, 077h, 077h, 077h, 088h, 087h, 073h, 022h, 022h, 022h, 022h, 022h, 022h, 026h, 022h
	db 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 022h, 020h, 008h, 0FFh
	db 0FFh, 0FFh, 082h, 0F2h, 022h, 028h, 077h, 077h, 077h, 077h, 078h, 077h, 077h, 078h, 077h, 087h
	db 076h, 076h, 060h, 000h, 026h, 066h, 066h, 068h, 066h, 067h, 077h, 078h, 087h, 08Fh, 0FFh, 0F8h
	db 0FFh, 0FFh, 0FFh, 0FFh, 078h, 088h, 088h, 088h, 088h, 0F8h, 088h, 088h, 088h, 088h, 088h, 077h
	db 077h, 087h, 077h, 087h, 077h, 078h, 058h, 077h, 077h, 077h, 087h, 077h, 077h, 078h, 087h, 077h
	db 077h, 077h, 077h, 078h, 078h, 086h, 047h, 0FFh, 0FFh, 0FFh, 0FFh, 0F3h, 022h, 08Fh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 088h, 08Fh, 088h, 088h, 088h, 088h, 08Fh, 0F8h
	db 0F8h, 0FFh, 0FFh, 0FFh, 0FFh, 088h, 0FFh, 0F8h, 0FFh, 0FFh, 0FFh, 0F8h, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh
	db 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 0FFh, 08Fh, 0FFh



Palette :
	db 000h, 000h, 000h, 000h, 000h, 000h, 080h, 000h, 000h, 080h, 000h, 000h, 000h, 080h, 080h, 000h
	db 080h, 000h, 000h, 000h, 080h, 000h, 080h, 000h, 080h, 080h, 000h, 000h, 080h, 080h, 080h, 000h
	db 0C0h, 0C0h, 0C0h, 000h, 000h, 000h, 0FFh, 000h, 000h, 0FFh, 000h, 000h, 000h, 0FFh, 0FFh, 000h
	db 0FFh, 000h, 000h, 000h, 0FFh, 000h, 0FFh, 000h, 0FFh, 0FFh, 000h, 000h, 0FFh, 0FFh, 0FFh, 000h
	
   end 	