;Set cycles to around 7k-10k for optimal experience


%define PILLAR_WIDTH 32
%define PILLAR_HEIGHT 128
%define PILLAR_COUNT 3
%define STARTING_ROW 320-40
%define PILLAR_STEP 10
%define PILLAR_GAP 320/3
%define VERTICAL_PILLAR_GAP 40

%define BIRD_HEIGHT 24
%define BIRD_WIDTH 32

%define MUSIC_FREQ 440
%define MUSIC_DURATION 50
%define NOTE_COUNT 8

section .text
[org 0x0100]

jmp start
%include "file.asm"

transparent_pallette db 0xFF

bg_filename db 'space.bmp', 0       
bg_handle dw 0

bird_filename db 'ship.bmp', 0
bird_handle dw 0

pillar_filename db 'p2.bmp', 0
pillar_handle dw 0

down_pillar_filename db 'p1.bmp', 0
down_pillar_handle dw 0

start_screen_filename db 'start.bmp', 0
start_screen_handle dw 0

help_screen_filename db 'help.bmp', 0
help_screen_handle dw 0

gameOverScreen_filename db 'gameover.bmp', 0
gameOverScreen_handle dw 0

bird_row: dw 100
bird_column: dw 100

pillar_columns: dw -1, -1, STARTING_ROW
pillar_heights: dw 50, 70, 64

spacePressed: db 0
collsionFlag: db 0

escMsg: db 13, "Press Y to exit or N to continue!$"
YPressed: db 0
NPressed: db 0

score: dw 0
scoreAdded: db 0
transparentColor: db 0
ScoreBuffer db 5, 0, 0, 0, 0, '$' ;buffer for the string (up to 5 digits and $ for display)
scoreMessage db 'Score: $'

gameOverMessage db 'Game Over! Press any key to exit.$'
newLine db 10, 13, '$'

PILLAR_OUTLINE_COLOR: db 0x2C

sound_length dw 500         ; Sound duration in timer ticks
sound_freq_low  dw 0x34DC   ; Lower 16 bits of 1193180
sound_freq_high dw 0x0012   ; Upper 16 bits of 1193180
jump_freq    dw 1000        ; Frequency for jump sound
crash_freq   dw 200         ; Frequency for crash sound
score_freq   dw 800         ; Frequency for scoring sound

pcb: dw 0, 0; SS, SP
     dw 0, 0; SS, SP

stack: times 255 dw 0
current: dw 0


play_sound:
    pusha
    ; Set up the timer (mode 3, square wave)
    mov al, 0b6h
    out 43h, al
    
    ; Load and output frequency divider
    mov dx, [sound_freq_high]
    mov ax, [sound_freq_low]
    div word [jump_freq]    ; Divide by desired frequency
    out 42h, al            ; Output low byte
    mov al, ah
    out 42h, al            ; Output high byte
    
    ; Turn on speaker
    in al, 61h
    or al, 3
    out 61h, al
    
    ; Delay loop
    mov cx, [sound_length]
    .delay:
        push cx
        mov cx, 100
        .inner_delay:
            loop .inner_delay
        pop cx
        loop .delay
    
    ; Turn off speaker
    in al, 61h
    and al, 0FCh
    out 61h, al
    
    popa
    ret

play_jump_sound:
    pusha
    mov dx, [sound_freq_high]
    mov ax, [sound_freq_low]
    div word [jump_freq]
    mov word [sound_length], 100  ; Short duration
    call play_sound
    popa
    ret

play_crash_sound:
    pusha
    mov dx, [sound_freq_high]
    mov ax, [sound_freq_low]
    div word [crash_freq]
    mov word [sound_length], 1000 ; Longer duration
    call play_sound
    popa
    ret

play_score_sound:
    pusha
    mov dx, [sound_freq_high]
    mov ax, [sound_freq_low]
    div word [score_freq]
    mov word [sound_length], 200  ; Medium duration
    call play_sound
    popa
    ret

delay1:
    push cx
    mov cx, 10
    .l1:
        push cx
        mov cx, 0xFFFF
        .l2:
            loop .l2
        pop cx
        loop .l1
    pop cx
    ret

drawBG:
    pusha

    ; Read the BMP header
    setCursor [bg_handle], 0, 54

    ; Read Pallete
    readfile [bg_handle], 256*4, buffer

    ; Load The Palette
    mov cx, 256;
    mov bx, 0;
    mov si, buffer
    .palleteLoop:
        push cx
        mov ax, 0x1010
        mov dh, [si+2]
        shr dh, 2
        mov ch, [si+1]
        shr ch, 2
        mov cl, [si+0]
        shr cl, 2
        cmp byte [si+0], 0
        jne .noMatch
        cmp byte [si+1], 0
        jne .noMatch
        cmp byte [si+2], 135
        jne .noMatch
            mov [transparentColor], bl
        .noMatch:
        int 0x10;
        pop cx
        add si, 4;
        inc bx;
        loop .palleteLoop

    
    mov ax, 0xA000
    mov es, ax
    mov cx, 200
    mov di, 64000-320
    .readScreen:
        push cx

        readfile [bg_handle], 320, buffer
        mov si, buffer
        mov cx, 320
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            loop .readLine

        pop cx
        sub di, 320+320
        loop .readScreen
    
    popa
    ret

drawStartScreen:
    pusha

    ; Read the BMP header
    setCursor [start_screen_handle], 0, 54

    ; Read Pallete
    readfile [start_screen_handle], 256*4, buffer

    ; Load The Palette
    mov cx, 256;
    mov bx, 0;
    mov si, buffer
    .palleteLoop1:
        push cx
        mov ax, 0x1010
        mov dh, [si+2]
        shr dh, 2
        mov ch, [si+1]
        shr ch, 2
        mov cl, [si+0]
        shr cl, 2
        cmp byte [si+0], 0
        jne .noMatch1
        cmp byte [si+1], 0
        jne .noMatch1
        cmp byte [si+2], 135
        jne .noMatch1
            mov [transparentColor], bl
        .noMatch1:
        int 0x10;
        pop cx
        add si, 4;
        inc bx;
        loop .palleteLoop1

    
    mov ax, 0xA000
    mov es, ax
    mov cx, 200
    mov di, 64000-320
    .readScreen1:
        push cx

        readfile [start_screen_handle], 320, buffer
        mov si, buffer
        mov cx, 320
        .readLine1:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint1
            mov [es:di], al
            .dontPrint1:
            inc di
            inc si
            loop .readLine1

        pop cx
        sub di, 320+320
        loop .readScreen1

    popa
    ret    

Rules:
    pusha

    ; Read the BMP header
    setCursor [help_screen_handle], 0, 54

    ; Read Pallete
    readfile [help_screen_handle], 256*4, buffer

    ; Load The Palette
    mov cx, 256;
    mov bx, 0;
    mov si, buffer
    .palleteLoop1:
        push cx
        mov ax, 0x1010
        mov dh, [si+2]
        shr dh, 2
        mov ch, [si+1]
        shr ch, 2
        mov cl, [si+0]
        shr cl, 2
        cmp byte [si+0], 0
        jne .noMatch1
        cmp byte [si+1], 0
        jne .noMatch1
        cmp byte [si+2], 135
        jne .noMatch1
            mov [transparentColor], bl
        .noMatch1:
        int 0x10;
        pop cx
        add si, 4;
        inc bx;
        loop .palleteLoop1

    
    mov ax, 0xA000
    mov es, ax
    mov cx, 200
    mov di, 64000-320
    .readScreen1:
        push cx

        readfile [help_screen_handle], 320, buffer
        mov si, buffer
        mov cx, 320
        .readLine1:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint1
            mov [es:di], al
            .dontPrint1:
            inc di
            inc si
            loop .readLine1

        pop cx
        sub di, 320+320
        loop .readScreen1
    
    popa
    ret

gameOverscreen:
       pusha

    ; Read the BMP header
    setCursor [gameOverScreen_handle], 0, 54

    ; Read Pallete
    readfile [gameOverScreen_handle], 256*4, buffer

    ; Load The Palette
    mov cx, 256;
    mov bx, 0;
    mov si, buffer
    .palleteLoop1:
        push cx
        mov ax, 0x1010
        mov dh, [si+2]
        shr dh, 2
        mov ch, [si+1]
        shr ch, 2
        mov cl, [si+0]
        shr cl, 2
        cmp byte [si+0], 0
        jne .noMatch1
        cmp byte [si+1], 0
        jne .noMatch1
        cmp byte [si+2], 135
        jne .noMatch1
            mov [transparentColor], bl
        .noMatch1:
        int 0x10;
        pop cx
        add si, 4;
        inc bx;
        loop .palleteLoop1

    
    mov ax, 0xA000
    mov es, ax
    mov cx, 200
    mov di, 64000-320
    .readScreen1:
        push cx

        readfile [gameOverScreen_handle], 320, buffer
        mov si, buffer
        mov cx, 320
        .readLine1:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint1
            mov [es:di], al
            .dontPrint1:
            inc di
            inc si
            loop .readLine1

        pop cx
        sub di, 320+320
        loop .readScreen1

        mov dl, 13      ; Set cursor position(col)
        mov dh, 3      ; Set cursor position(row)
        mov bh, 0
        mov ah, 02h
        int 0x10
        mov ah, 0x09
        lea dx, scoreMessage
        int 0x21
    
        call printEndScore
    popa
    ret 

checkBirdInPillar:
    push bp
    mov bp, sp
    pusha

    mov word [bp+4], 0

    mov ax, [bird_column]
    cmp ax, [pillar_columns+0]
    jl .skipPillar0
    sub ax, PILLAR_WIDTH
    cmp ax, [pillar_columns+0]
    jg .skipPillar0
        mov word [bp+4], -1
    .skipPillar0:

    mov ax, [bird_column]
    cmp ax, [pillar_columns+2]
    jl .skipPillar1
    sub ax, PILLAR_WIDTH
    cmp ax, [pillar_columns+2]
    jg .skipPillar1
        mov word [bp+4], -1
    .skipPillar1:

        
    mov ax, [bird_column]
    cmp ax, [pillar_columns+4]
    jl .skipPillar2
    sub ax, PILLAR_WIDTH
    cmp ax, [pillar_columns+4]
    jg .skipPillar2
        mov word [bp+4], -1
    .skipPillar2:
        
    popa
    mov sp, bp
    pop bp
    ret

drawBird:
    push bp
    mov bp, sp
    pusha

    sub sp, 2
    call checkBirdInPillar
    pop ax
    cmp ax, 0
    je .notInPillar
        cmp byte [scoreAdded], 0
        jne .afterPillarCheck
        inc word [score]
        mov byte [scoreAdded], 1
        call play_crash_sound 
        ; popa
        ; mov sp, bp
        ; pop bp
        ; ret
        jmp .afterPillarCheck
    .notInPillar:
        mov byte [scoreAdded], 0
    .afterPillarCheck:

    setCursor [bird_handle], 0, 54+256*4
    mov ax, 0xA000
    mov es, ax
    mov ax, [bird_row]
    mov bx, 320
    mul bx
    add ax, [bird_column]
    mov di, ax
    mov cx, BIRD_HEIGHT
    mov byte [collsionFlag], 0
    .readScreen:
        push cx

        readfile [bird_handle], BIRD_WIDTH, buffer
        mov si, buffer
        mov cx, BIRD_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint
            push ax
            mov al, [es:di]
            cmp al, [PILLAR_OUTLINE_COLOR]
            jne .noCollision
                mov byte [collsionFlag], 1
            .noCollision:
            pop ax
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            loop .readLine

        pop cx
        sub di, 320+BIRD_WIDTH
        loop .readScreen
        
    popa
    mov sp, bp
    pop bp
    ret

; BP+4 => Pillar Row
; BP+6 => Pillar Column
; BP+8 => Pillar Height
drawUpPillar:
    push bp
    mov bp, sp
    pusha

    mov ax, PILLAR_HEIGHT
    sub ax, [BP+8]
    mov bx, PILLAR_WIDTH
    mul bx
    add ax, 54+256*4
    adc ax, 0

    setCursor [pillar_handle], dx, ax
    mov ax, 0xA000
    mov es, ax
    mov ax, [BP+4]
    mov bx, 320
    mul bx
    add ax, [BP+6]
    mov di, ax
    mov cx, [BP+8]
    .readScreen:
        push cx

        readfile [pillar_handle], PILLAR_WIDTH, buffer
        mov si, buffer
        mov cx, PILLAR_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            loop .readLine

        pop cx
        sub di, 320+PILLAR_WIDTH
        loop .readScreen

    popa
    mov sp, bp
    pop bp
    ret 6

; BP+4 => Pillar Column
; BP+6 => Pillar Height
drawDownPillar:
    push bp
    mov bp, sp
    pusha

    setCursor [down_pillar_handle], 0, 54+256*4
    mov ax, 0xA000
    mov es, ax
    mov ax, [BP+6]
    mov bx, 320
    mul bx
    add ax, [BP+4]
    mov di, ax
    mov cx, [BP+6]
    .readScreen:
        push cx

        readfile [down_pillar_handle], PILLAR_WIDTH, buffer
        mov si, buffer
        mov cx, PILLAR_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            jz .dontPrint
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            loop .readLine

        pop cx
        sub di, 320+PILLAR_WIDTH
        loop .readScreen

    popa
    mov sp, bp
    pop bp
    ret 4

; BP+4 => Pillar Column
; BP+6 => Pillar Height
drawBackgroundInDownPillarPlace:
    push bp
    mov bp, sp
    pusha

    setCursor [down_pillar_handle], 0, 54+256*4

    mov ax, 0xA000
    mov es, ax
    mov ax, [BP+6]
    mov bx, 320
    mul bx
    mov dx, 54+256*4 + 199*320
    sub dx, ax
    add dx, [BP+4]
    add ax, [BP+4]
    mov di, ax
    mov si, dx

    setCursor [bg_handle], 0, si
    mov cx, [BP+6]
    .readScreen:
        push cx

        readfile [down_pillar_handle], PILLAR_WIDTH, buffer
        readfile [bg_handle], 320, buffer+PILLAR_WIDTH
        mov si, buffer
        mov bx, buffer+PILLAR_WIDTH
        mov cx, PILLAR_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            je .dontPrint
            mov al, [bx]
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            inc bx
            loop .readLine

        pop cx
        sub di, 320+PILLAR_WIDTH
        loop .readScreen

    popa
    mov sp, bp
    pop bp
    ret 4

; BP+4 => Pillar Row
; BP+6 => Pillar Column
; BP+8 => Pillar Height
drawBackgroundInUpPillarPlace:
    push bp
    mov bp, sp
    pusha

    mov ax, PILLAR_HEIGHT
    sub ax, [BP+8]
    mov bx, PILLAR_WIDTH
    mul bx
    add ax, 54+256*4
    adc ax, 0

    setCursor [pillar_handle], dx, ax

    mov ax, 0xA000
    mov es, ax
    mov ax, [BP+4]
    mov bx, 320
    mul bx
    mov dx, 54+256*4 + 199*320
    sub dx, ax
    add dx, [BP+6]
    add ax, [BP+6]
    mov di, ax
    mov si, dx

    setCursor [bg_handle], 0, si
    mov cx, [BP+8]
    .readScreen:
        push cx

        readfile [pillar_handle], PILLAR_WIDTH, buffer
        readfile [bg_handle], 320, buffer+PILLAR_WIDTH
        mov si, buffer
        mov bx, buffer+PILLAR_WIDTH
        mov cx, PILLAR_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            je .dontPrint
            mov al, [bx]
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            inc bx
            loop .readLine

        pop cx
        sub di, 320+PILLAR_WIDTH
        loop .readScreen

    popa
    mov sp, bp
    pop bp
    ret 6

drawBackgroundInBirdPlace:
    pusha

    mov ax, 0xA000
    mov es, ax
    mov ax, [bird_row]
    mov bx, 320
    mul bx
    mov dx, 54+256*4 + 199*320
    sub dx, ax
    add dx, [bird_column]
    add ax, [bird_column]
    mov di, ax
    mov si, dx

    setCursor [bird_handle], 0, 54+256*4
    setCursor [bg_handle], 0, si
    mov cx, BIRD_HEIGHT
    .readScreen:
        push cx

        readfile [bird_handle], BIRD_WIDTH, buffer
        readfile [bg_handle], 320, buffer+BIRD_WIDTH
        mov si, buffer
        mov bx, buffer+BIRD_WIDTH
        mov cx, BIRD_WIDTH
        .readLine:
            mov al, [si]
            cmp al, [transparent_pallette]
            je .dontPrint
            mov al, [bx]
            mov [es:di], al
            .dontPrint:
            inc di
            inc si
            inc bx
            loop .readLine

        pop cx
        sub di, 320+BIRD_WIDTH
        loop .readScreen

    popa
    ret

music_counter dw 0
music_playing db 0
current_note db 0
note_frequencies: dw 440, 494, 523, 587, 659, 698, 784, 880



; PC Speaker Symphony
; A multi-movement musical composition for PC speaker

; Frequency constants (in Hz) for different octaves
; Lower octave
C3 equ 1193180 / 131
D3 equ 1193180 / 147
E3 equ 1193180 / 165
F3 equ 1193180 / 175
G3 equ 1193180 / 196
A3 equ 1193180 / 220
B3 equ 1193180 / 247

; Middle octave
C4 equ 1193180 / 261
D4 equ 1193180 / 294
E4 equ 1193180 / 329
F4 equ 1193180 / 349
G4 equ 1193180 / 392
A4 equ 1193180 / 440
B4 equ 1193180 / 493

; Higher octave
C5 equ 1193180 / 523
D5 equ 1193180 / 587
E5 equ 1193180 / 659
F5 equ 1193180 / 698
G5 equ 1193180 / 784
A5 equ 1193180 / 880
B5 equ 1193180 / 987

; Note durations
WHOLE equ 0FFFFh
HALF equ WHOLE / 2
QUARTER equ HALF / 2
EIGHTH equ QUARTER / 2

; Beethoven's Symphony No. 5 in C minor, Op. 67
; Adapted for PC Speaker
; First movement: Allegro con brio

; Note frequencies (Hz)
G3  equ 1193180 / 196
Gb3 equ 1193180 / 185
F3  equ 1193180 / 175
E3  equ 1193180 / 165
Eb3 equ 1193180 / 156
D3  equ 1193180 / 147
C3  equ 1193180 / 131
B3  equ 1193180 / 247
Bb3 equ 1193180 / 233
A3  equ 1193180 / 220

; Higher octave
G4  equ 1193180 / 392
Gb4 equ 1193180 / 370
F4  equ 1193180 / 349
E4  equ 1193180 / 329
Eb4 equ 1193180 / 311
D4  equ 1193180 / 294
C4  equ 1193180 / 261
B4  equ 1193180 / 493
Bb4 equ 1193180 / 466
A4  equ 1193180 / 440
Ab4 equ 1193180 / 415

; Note durations
WHOLE    equ 0FFFFh
HALF     equ WHOLE / 2
QUARTER  equ HALF / 2
EIGHTH   equ QUARTER / 2
SIXTEENTH equ EIGHTH / 2

; Note frequencies (Hz)
D4  equ 1193180 / 294
Db4 equ 1193180 / 277
C4  equ 1193180 / 261
B3  equ 1193180 / 247
Bb3 equ 1193180 / 233
A3  equ 1193180 / 220
Ab3 equ 1193180 / 208
G3  equ 1193180 / 196
F3  equ 1193180 / 175
E3  equ 1193180 / 165
Eb3 equ 1193180 / 156
D3  equ 1193180 / 147

; Higher octave
D5  equ 1193180 / 587
C5  equ 1193180 / 523
B4  equ 1193180 / 493
Bb4 equ 1193180 / 466
A4  equ 1193180 / 440
G4  equ 1193180 / 392
F4  equ 1193180 / 349
E4  equ 1193180 / 329

play_background_music:
    ; Initialize PIT for square wave generation
    mov al, 0b6h
    out 43h, al
    
    ; Main loop for the theme
    mov cx, 300      ; Play pattern twice
.main_loop:
    push cx
    
    ; First measure
    mov ax, D4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, D4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, D5
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, A4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, Ab4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, G4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, F4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    ; Second measure with variation
    mov ax, C4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, C4
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call turn_speaker_off
    
    mov ax, D5
    out 42h, al
    mov al, ah
    out 42h, al
    call turn_speaker_on
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    call delay
    
    call turn_speaker_off
    
    pop cx
    dec cx
    jnz .main_loop
    jmp play_background_music

turn_speaker_on:
    in al, 61h
    mov ah, al
    or al, 3h
    out 61h, al
    ret

turn_speaker_off:
    mov al, ah
    out 61h, al
    ret

delay:
    ; Adjustable delay for tempo control
    mov cx, 0FFFFh
.delay_loop:
    loop .delay_loop
    ret


oldisr: dd 0
oldtimer: dd 0

timer:
    push ds
    push es
    pusha

    push cs
    pop ds
    call printScore

    mov bx, [current]
    shl bx, 2

    mov [pcb+bx], ss
    mov [pcb+bx+2], sp

    add word [current], 1
    and word [current], 1
    mov bx, [current]
    shl bx, 2

    mov ss, [pcb+bx]
    mov sp, [pcb+bx+2]

    ; call play_background_music

exit_timer:
	mov al, 0x20
	out 0x20, al
    popa
    pop es
    pop ds
    iret 

kbisr:
    pusha
    in al, 0x60

    cmp al, 0xB9
    je .SpaceUP

    cmp al, 0X01
    je .escPressed

    jmp .retWithoutChaining
    .continueNormal:
        popa
        jmp far [oldisr]
    .retWithoutChaining:
        mov al, 0x20
        out 0x20, al
        popa
        iret

    .SpaceUP:
        mov byte [spacePressed], 1
        ;call play_score_sound 
        jmp .retWithoutChaining

    .escPressed:
        call turn_speaker_off
        mov byte[YPressed], 0
        mov byte[NPressed], 0
        call clrscrn
        call prompt_and_input_str
        call turn_speaker_on
        jmp .retWithoutChaining

movePillars:
    pusha
    mov cx, PILLAR_COUNT
    mov bx, PILLAR_COUNT*2
    .movePillar:
        sub bx, 2

        cmp word [pillar_columns+bx], -1
        je .skipPillar

        push word [pillar_heights+bx]
        push word [pillar_columns+bx]
        push 163
        call drawBackgroundInUpPillarPlace

        mov ax, [pillar_heights+bx]
        add ax, VERTICAL_PILLAR_GAP
        sub ax, 163
        neg ax
        push word ax
        push word [pillar_columns+bx]
        call drawBackgroundInDownPillarPlace

        sub word [pillar_columns+bx], PILLAR_STEP
        cmp word [pillar_columns+bx], 0
        jge .dontReset
            mov word [pillar_columns+bx], STARTING_ROW-PILLAR_STEP
        .dontReset:

        push word [pillar_heights+bx]
        push word [pillar_columns+bx]
        push 163
        call drawUpPillar

        mov ax, [pillar_heights+bx]
        add ax, VERTICAL_PILLAR_GAP
        sub ax, 163
        neg ax
        push word ax
        push word [pillar_columns+bx]
        call drawDownPillar

        cmp word [pillar_columns+bx], STARTING_ROW-PILLAR_GAP-PILLAR_STEP
        jg .skipPillar
        cmp bx, 0
        je .skipPillar
        cmp word [pillar_columns+bx-2], -1
        jne .skipPillar
        mov word [pillar_columns+bx-2], STARTING_ROW-PILLAR_STEP
        .skipPillar:
        loop .movePillar
    popa
    ret

moveGround:
    push bp
    mov bp, sp
    pusha
    mov ax, 0xA000
    mov es, ax

    mov cx, 200-160
    mov di, 320*160

    .next_row:
        mov al, byte[es:di]
        mov si, di
        mov dx, 320-1

        .shift_row:
            mov bl, byte[es:si+1]
            mov byte[es:si], bl
            inc si
            dec dx
            jnz .shift_row
        
        mov [es:si], al

        add di, 320
        loop .next_row

    popa
    mov sp, bp
    pop bp

    ret
    
    clrscrn:
    pusha
    mov ax, 0xA000
    mov es, ax

    mov di, 0          
    mov al, 0xFF       
    mov cx, 64000
        
    .clearLoop:
        stosb              
        loop .clearLoop   

    popa
    ret

prompt_and_input_str:
    push bp
    mov bp, sp
    pusha
    push es
    
    ; Center cursor
    mov dl, 0
    mov dh, 0
    mov bh, 0h
    mov ah, 02h
    int 0x10

    ; PROMPT
    mov ah, 09h
    lea dx, escMsg
    int 0x21

    ; INPUT
    .waitForInput:
        in al, 0x60             
        cmp al, 0x15           
        je .YPressed
        cmp al, 0x31            
        je .NPressed
        jmp .waitForInput

    .NPressed:
        call drawBG
        mov byte[NPressed], 1
        jmp .exit

    .YPressed:
        mov byte[YPressed], 1
        jmp .exit

    .exit:
        pop es
        popa
        mov sp, bp
        pop bp
        ret

printEndScore:
    pusha
    mov ax, [score]        
    mov bx, 10             
    lea di, [ScoreBuffer + 5] 
    mov byte [di], '$'     
    dec di                 

    .convert_loop:
        xor dx, dx         
        div bx             
        add dl, '0'        
        mov [di], dl       
        dec di             
        test ax, ax        
        jnz .convert_loop   

    lea si, [di+1]

    mov dl, 19
    mov dh, 3        
    mov bh, 0              
    mov ah, 02h            
    int 0x10               

    ; Set text color attribute (before printing)
    mov ah, 09h            ; BIOS function to write character and attribute
    mov bl, 0Bh           
    mov cx, 1             ; Number of times to print
    mov al, [si]          ; Character to print
    
    .print_loop:
        mov ah, 09h
        int 10h           ; Print character with color
        inc si
        inc dl            ; Move cursor right
        mov ah, 02h
        int 10h           ; Set cursor position
        mov al, [si]      ; Get next character
        cmp al, '$'       ; Check if we are done
        jne .print_loop

    popa
    ret

printScore:
    pusha
    mov ax, [score]        
    mov bx, 10             
    lea di, [ScoreBuffer + 5] 
    mov byte [di], '$'     
    dec di                 

    .convert_loop:
        xor dx, dx         
        div bx             
        add dl, '0'        
        mov [di], dl       
        dec di             
        test ax, ax        
        jnz .convert_loop   

    lea si, [di+1]

    mov dl, 19
    mov dh, 1        
    mov bh, 0              
    mov ah, 02h            
    int 0x10               

    ; Set text color attribute (before printing)
    mov ah, 09h            ; BIOS function to write character and attribute
    mov bl, 0Bh           
    mov cx, 1             ; Number of times to print
    mov al, [si]          ; Character to print
    
    .print_loop:
        mov ah, 09h
        int 10h           ; Print character with color
        inc si
        inc dl            ; Move cursor right
        mov ah, 02h
        int 10h           ; Set cursor position
        mov al, [si]      ; Get next character
        cmp al, '$'       ; Check if we are done
        jne .print_loop

    popa
    ret


start:
    mov ax, 1100
    out 0x40, al
    mov al, ah
    out 0x40, al

    xor ax, ax;
    int 0x16

    ; Set video mode to 13h (320x200, 256 colors)
    mov ax, 0x13
    int 0x10

    mov ax, 0;
    mov es, ax;

    mov word [pcb+1*4+0], ds
    mov word [stack+255*2-2], 0x0200
    mov word [stack+255*2-4], cs
    mov word [stack+255*2-6], play_background_music
    mov word [stack+255*2-8], ds
    mov word [stack+255*2-10-2*5], stack+255*2-10
    mov word [pcb+1*4+2], stack+255*2-10-16

startscreen:
    openfile start_screen_filename
    mov [start_screen_handle], ax
    call drawStartScreen


keypress:
    xor ax, ax    
    mov ah, 0
    int 0x16

    cmp al, 'R'
    je helpscreen
    cmp al, 'r'
    je helpscreen
    cmp al, 'P'
    je Resume
    cmp al, 'p'
    je Resume
    cmp al, 0x1B
    je exit
    jmp keypress
    

helpscreen:      
    openfile help_screen_filename
    mov [help_screen_handle], ax
    call Rules

    mov ah, 0
    int 0x16
    cmp al, 0x1B
    je startscreen


Resume:
    mov ax, 0
    mov es, ax

    ; SAVE PREVIOUS KBISR
    mov ax, [es:9*4]
    mov [oldisr], ax

    mov ax, [es:9*4+2];
    mov [oldisr+2], ax

    ; SAVE PREVIOUS TIMER
    mov ax,word[es:8*4];
    mov word[oldtimer], ax;

    mov ax,word[es:8*4+2];
    mov word[oldtimer+2], ax;

    ; HOOK
    cli
    mov word[es:8*4], timer  ; Set ISR address for IRQ0 (Timer)
    mov word[es:8*4+2], cs

    mov [es:9*4+2], cs
    mov word [es:9*4], kbisr
    sti

    openfile bird_filename
    mov [bird_handle], ax;
    openfile bg_filename
    mov [bg_handle], ax
    openfile pillar_filename
    mov [pillar_handle], ax
    openfile down_pillar_filename
    mov [down_pillar_handle], ax

    call drawBG

    .infLoop:
        call moveGround
        call drawBackgroundInBirdPlace
        cmp byte [spacePressed], 0
        je .dontMoveUp
            mov byte [spacePressed], 0
            cmp word [bird_row], 65
            jl .dontMoveUp
            sub word [bird_row], 25
        .dontMoveUp:
        add word [bird_row], 5
        call drawBird
        cmp byte[collsionFlag], 1
        je .gameOver
        cmp word [bird_row], 184
        jge .gameOver
        call movePillars
        call delay1
        cmp byte[YPressed], 1
        je .gameOver
        jmp .infLoop

    .gameOver:
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        ; call delay
        call turn_speaker_off
        closefile bird_handle
        closefile bg_handle
        closefile pillar_handle
        closefile down_pillar_handle

        ;UNHOOK
        mov ax, 0;
        mov es, ax;

        mov ax, [oldisr]
        mov [es:9*4], ax

        mov ax, [oldisr+2];
        mov [es:9*4+2], ax

        mov ax,word[oldtimer]
        mov word[es:8*4], ax

        mov ax,word[oldtimer+2]
        mov word[es:8*4+2], ax

        openfile gameOverScreen_filename
        mov [gameOverScreen_handle], ax    
        call gameOverscreen  
        mov ah, 0
        int 0x16 
        call clrscrn

        ; call clrscrn
        ; mov dl, 0
        ; mov dh, 0
        ; mov bh, 0
        ; mov ah, 02h
        ; int 0x10
        ; mov ah, 0x09
        ; lea dx, gameOverMessage
        ; int 0x21

        xor ax, ax;
        int 0x16

    ; Exit the program
    exit:
    call clrscrn
    mov dl, 0
    mov dh, 0
    mov bh, 0
    mov ah, 02h
    int 0x10
    mov ah, 0x09
    lea dx, gameOverMessage
    int 0x21
    mov ah, 0x09
    lea dx, newLine
    int 0x21
    mov ax, 0x4C00
    int 0x21

buffer: times 2000 db 0