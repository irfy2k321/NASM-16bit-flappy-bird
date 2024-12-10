; AX Will Contain File Handle if Open (CF = 0)
; AX Will Contain Error Code If Not Able to Open (CF = 1)
%MACRO openfile 1
push dx
mov dx, %1
mov al, 0
mov ah, 0x3D
int 0x21
pop dx
%ENDMACRO

; AX Will Contain Garbage if Closed (CF = 0)
; AX Will Contain Error Code If Not Able to Close (CF = 1)
%MACRO closefile 1
push bx
mov ah, 0x3E
mov bx, %1
int 0x21
pop bx
%ENDMACRO

; NOTE: CHANGES BX, DX, CX, AX
; AX Will Contain Number of Bytes Read if Read is Successfull (CF = 0)
; AX Will Contain Error Code If Not Able to Read (CF = 1)
%MACRO readfile 3
pusha
mov bx, %1  ; File Handle
mov cx, %2  ; Size to Read
mov dx, %3  ; Buffer Location
mov ah, 0x3F
int 0x21
popa
%ENDMACRO

; DX:AX Will Contain New Position if Set Cursor is Successfull (CF = 0)
; AX Will Contain Error Code If Not Able to Set Cursor (CF = 1)
%MACRO setCursor 3
push bx
push cx

push word %1
push word %2
push word %3

pop dx; Offset LSBs
pop cx; Offset MSBs
pop bx; File Handle
mov ax, 0x4200
int 0x21
pop bx
pop cx
%ENDMACRO