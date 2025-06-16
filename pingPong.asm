[org 0x100]
jmp start

paddleAStart: dw 60
paddleBStart: dw 3900
oldkbisr: dd 0
oldtimer: dd 0
scoreA: dw 0
scoreB: dw 0
currentPlayer: db 0; 0 means pLayer A, 1 means player B
ballDirection: db 0; 0 means top right, 1 means bottom right, 2 means bottom left, 3 means top left
ballPosition: dw 3760
sideWallCollision: db 0; 0 means not a sideWallCollision, 1 means a sideWallCollision
messageA: db 'Score for Player A: '
messageB: db 'Score for Player B: '
WinningPlayerA: db 'Player A Wins!'
WinningPlayerB: db 'Player B Wins!'
timerCount: dw 0

; timer interrupt service routine
timer:
    push ax
    push es

    inc word [timerCount]
    cmp word [timerCount], 2
    jne exitTimer

    mov word [timerCount], 0

    mov ax, 0xb800
    mov es, ax

    cmp byte [ballDirection] , 0; check if ball is moving top-right
    je moveBallTR

    cmp byte [ballDirection], 1; check if ball is moving bottom-right
    je moveBallBR

    cmp byte [ballDirection], 2; check if ball is moving bottom-left
    je moveBallBL

    cmp byte [ballDirection], 3; check if ball is moving top-left
    je moveBallTL

; function to move ball top right
moveBallTR:
    mov si, [ballPosition]
    mov di, si
    
    sub di, 158

    cmp di, 0
    jb reset

    cmp di, 158
    jg checkSideWallTR

    jmp checkCollisionTopRow

checkSideWallTR:
    
    mov ax, di
    mov bl, 160
    div byte bl

    cmp ah, 158
    je checkCollisionSideWallTR

    jmp printBallTR

checkCollisionTopRow:

    mov ax, [es:di]

    cmp ax, 0x7020
    je paddleCollisionTR
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp reset

paddleCollisionTR:
    mov byte [currentPlayer], 1
    jmp changeDirection

checkCollisionSideWallTR:
    mov byte [sideWallCollision], 1
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp changeDirection

printBallTR:
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp exitTimer

; function to move ball bottom right
moveBallBR:
    mov si, [ballPosition]
    mov di, si

    add di, 162

    cmp di, 3998
    jg reset

    cmp di, 3840
    jl checkSideWallBR

    jmp checkCollisionBottomRow

checkSideWallBR:
    mov ax, di
    mov bl, 160
    div byte bl

    cmp ah, 158
    je checkCollisionSideWallBR

    jmp printBallBR

checkCollisionBottomRow:
    mov ax, [es:di]
    cmp ax, 0x7020
    je paddleCollisionBR
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp reset

paddleCollisionBR:
    mov byte [currentPlayer], 0
    jmp changeDirection

checkCollisionSideWallBR:
    mov byte [sideWallCollision], 1
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp changeDirection

printBallBR:
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp exitTimer


; function to move ball top left
moveBallTL:
    mov si, [ballPosition]
    mov di, si
    
    sub di, 162

    cmp di, 0
    jb reset

    cmp di, 158
    jg checkSideWallTL

    jmp checkCollisionTopRowL

checkSideWallTL:
    
    mov ax, di
    mov bl, 160
    div byte bl

    cmp ah, 0
    je checkCollisionSideWallTL

    jmp printBallTL

checkCollisionTopRowL:

    mov ax, [es:di]

    cmp ax, 0x7020
    je paddleCollisionTL
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp reset

paddleCollisionTL:
    mov byte [currentPlayer], 1
    jmp changeDirection

checkCollisionSideWallTL:
    mov byte [sideWallCollision], 1
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp changeDirection

printBallTL:
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp exitTimer


; function to move ball bottom left
moveBallBL:
    mov si, [ballPosition]
    mov di, si

    add di, 158

    cmp di, 3998
    jg reset

    cmp di, 3840
    jl checkSideWallBL

    jmp checkCollisionBottomRowL

checkSideWallBL:
    mov ax, di
    mov bl, 160
    div byte bl

    cmp ah, 0
    je checkCollisionSideWallBL

    jmp printBallBL

checkCollisionBottomRowL:
    mov ax, [es:di]
    cmp ax, 0x7020
    je paddleCollisionBL
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp reset

paddleCollisionBL:
    mov byte [currentPlayer], 0
    jmp changeDirection

checkCollisionSideWallBL:
    mov byte [sideWallCollision], 1
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp changeDirection

printBallBL:
    mov ax, [es:si]
    mov word [es:si], 0x0720
    mov word [es:di], ax
    mov word [ballPosition], di
    jmp exitTimer

; code to reset ball postion upon collision with top or bottom row
reset:
    cmp byte [currentPlayer], 0
    je playerAReset

playerBReset:
    inc word [scoreA]
    mov word [paddleAStart], 60
    mov word [paddleBStart], 3900
    mov byte [ballDirection], 1
    mov word [es:di],0x0720
    call drawBallA
    call drawPaddleA
    call drawPaddleB
    jmp exitTimer

playerAReset:
    inc word [scoreB]
    mov word [paddleAStart], 60
    mov word [paddleBStart], 3900
    mov byte [ballDirection], 0
    mov word [es:di],0x0720
    call drawBallB
    call drawPaddleA
    call drawPaddleB
    jmp exitTimer

; code to change direction of ball upon collision with side wall or a paddle
changeDirection:
    
    cmp byte [ballDirection] , 0; check if ball is moving top-right
    je TRChange

    cmp byte [ballDirection], 1; check if ball is moving bottom-right
    je BRChange

    cmp byte [ballDirection], 2; check if ball is moving bottom-left
    je BLChange

    cmp byte [ballDirection], 3; check if ball is moving top-left
    je TLChange

TRChange:
    cmp byte [sideWallCollision], 1
    je TRChangeSideWall

    mov byte [ballDirection], 1
    jmp exitTimer

TRChangeSideWall:
    mov byte [sideWallCollision], 0
    mov byte [ballDirection], 3
    jmp exitTimer

BRChange:
    cmp byte [sideWallCollision], 1
    je BRChangeSideWall

    mov byte [ballDirection], 0
    jmp exitTimer

BRChangeSideWall:
    mov byte [sideWallCollision], 0
    mov byte [ballDirection], 2
    jmp exitTimer

BLChange:
    cmp byte [sideWallCollision], 1
    je BLChangeSideWall

    mov byte [ballDirection], 3
    jmp exitTimer

BLChangeSideWall:
    mov byte [sideWallCollision], 0
    mov byte [ballDirection], 1
    jmp exitTimer

TLChange:
    cmp byte [sideWallCollision], 1
    je TLChangeSideWall

    mov byte [ballDirection], 2
    jmp exitTimer

TLChangeSideWall:
    mov byte [sideWallCollision], 0
    mov byte [ballDirection], 0
    jmp exitTimer

exitTimer:
    pop es
    pop ax
    jmp far [cs:oldtimer]



; keyboard interrupt handler to handle arrow keys for paddle movement depending on current player
kbisr:
    push ax
    in al, 0x60 ; read char from keyboard port

    cmp al,0x4B; left arrow key
    jne next

    cmp byte [currentPlayer], 0
    jne playerBTurnLeft

    playerATurnLeft:
    mov ax, [paddleAStart]
    sub ax, 2

    cmp word [paddleAStart], 0
    jbe exit

    call movePaddleALeft
    jmp exit

    playerBTurnLeft:
    mov ax, [paddleBStart]
    sub ax, 2

    cmp word ax, 3838
    jbe exit

    call movePaddleBLeft
    jmp exit

next:
    cmp al, 0x4D; right arrow key
    jne nomatch

    cmp byte [currentPlayer], 0
    jne playerBTurnRight

    playerATurnRight:
    mov ax, [paddleAStart]
    add ax, 42

    cmp word ax, 160
    jg exit

    call movePaddleARight
    jmp exit

    playerBTurnRight:
    mov ax, [paddleBStart]
    add ax, 42

    cmp word ax, 4000
    jg exit

    call movePaddleBRight
    jmp exit

nomatch: 
    pop ax
    jmp far [cs:oldkbisr] ; call original ISR

exit: 
    mov al, 0x20
    out 0x20, al ; send EOI to PIC
    pop ax
    iret
    


clearScreen:
    push es
    push ax
    push cx
    push di

    mov ax, 0xb800
    mov es, ax
    xor di, di
    mov ax, 0x0720
    mov cx, 2000
    cld
    rep stosw

    pop di
    pop cx
    pop ax
    pop es
    ret

clearTopRow:
    push es
    push ax
    push cx
    push di

    mov ax, 0xb800
    mov es, ax
    xor di, di
    mov ax, 0x0720
    mov cx, 80
    cld
    rep stosw

    pop di
    pop cx
    pop ax
    pop es
    ret

clearBottomRow:
    push es
    push ax
    push cx
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 3840
    mov ax, 0x0720
    mov cx, 78
    cld
    rep stosw

    pop di
    pop cx
    pop ax
    pop es
    ret

drawBallA:
    push ax
    push es
    push di
    push bx


    mov ax, 0xb800
    mov es, ax

    mov di, 240
    mov word [ballPosition], 240

    mov ah, 0x07
    mov al, '*'
    mov word [es:di], ax 

    pop bx
    pop di
    pop es
    pop ax
    ret

drawBallB:
    push ax
    push es
    push di
    push bx

    mov ax, 0xb800
    mov es, ax

    mov di,3760
    mov word [ballPosition], 3760

    mov ah, 0x07
    mov al, '*'
    mov word [es:di],ax
    
    pop bx
    pop di
    pop es
    pop ax
    ret



drawPaddleA:
    push ax
    push es
    push di
    push dx

    call clearTopRow

    mov ax, 0xb800
    mov es, ax


    mov di, 60
    mov ax, 0x7020
    mov cx, 20
    rep stosw


    pop dx
    pop di
    pop es
    pop ax
    ret

drawPaddleB:
    push ax
    push es
    push di
    push dx

    call clearBottomRow

    mov ax, 0xb800
    mov es, ax


    mov di, 3900
    mov ax, 0x7020
    mov cx, 20
    rep stosw

    pop dx
    pop di
    pop es
    pop ax
    ret

movePaddleALeft:
    push ax
    push es
    push di
    push dx
    push ds

    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    
    
    mov word di, [cs:paddleAStart]
    mov si, di
    add si, 38
    push si
    mov di, si
    sub di, 2

    std
    mov cx,20
    rep movsw

    sub word [cs:paddleAStart], 2
    pop si
    mov word [es:si], 0x0720

    pop ds
    pop dx
    pop di
    pop es
    pop ax
    ret

movePaddleARight:
    push ax
    push es
    push di
    push dx
    push ds

    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    
    mov word di, [cs:paddleAStart]
    mov si, di
    push si
    add di, 2

    cld
    mov cx, 20
    rep movsw

    add word [cs:paddleAStart], 2
    pop si
    mov word [es:si], 0x0720
    
    pop ds
    pop dx
    pop di
    pop es
    pop ax
    ret

movePaddleBLeft:
    push ax
    push es
    push di
    push dx
    push ds

    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    
    
    mov word di, [cs:paddleBStart]
    mov si, di
    add si, 38
    push si
    mov di, si
    sub di, 2

    std
    mov cx,20
    rep movsw

    sub word [cs:paddleBStart], 2
    pop si
    mov word [es:si], 0x0720

    pop ds
    pop dx
    pop di
    pop es
    pop ax
    ret

movePaddleBRight:
    push ax
    push es
    push di
    push dx
    push ds

    mov ax, 0xb800
    mov es, ax
    mov ds, ax
    
    mov word di, [cs:paddleBStart]
    mov si, di
    push si
    add di, 2

    cld
    mov cx, 20
    rep movsw

    add word [cs:paddleBStart], 2
    pop si
    mov word [es:si], 0x0720
    
    pop ds
    pop dx
    pop di
    pop es
    pop ax
    ret

printnum:     
    push bp 
    mov  bp, sp 
    push es 
    push ax 
    push bx 
    push cx 
    push dx 
    push di 

    mov  ax, 0xb800 
    mov  es, ax
    mov  ax, [bp+4]
    mov  bx, 10
    mov  cx, 0

nextdigit:    
    mov  dx, 0             
    div  bx                 
    add  dl, 0x30           
    push dx 
    inc  cx
    cmp  ax, 0
    jnz  nextdigit

    mov  di, [bp + 6]

nextpos:      
    pop  dx
    mov  dh, 0x04
    mov  [es:di], dx
    add  di, 2
    loop nextpos

    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax 
    pop es
    pop bp
    ret 4

start:
    call clearScreen
    call drawPaddleA
    call drawPaddleB
    call drawBallB

    xor ax, ax
    mov es, ax ; point es to IVT base
    mov ax, [es:9*4]
    mov [oldkbisr], ax ; save offset of old routine
    mov ax, [es:9*4+2]
    mov [oldkbisr+2], ax ; save segment of old routine

    mov ax, [es:8*4]
    mov [oldtimer], ax ; save offset of old routine
    mov ax, [es:8*4+2]
    mov [oldtimer+2], ax ; save segment of old routine

    cli ; disable interrupts
    mov word [es:9*4], kbisr ; store offset at n*4
    mov [es:9*4+2], cs ; store segment at n*4+2
    mov word [es:8*4], timer ; store offset at n*4
    mov [es:8*4+2], cs ; store segment at n*4+
    sti ; enable interrupts

match:
    cmp word [scoreA],5
    je gameEnding

    cmp word [scoreB],5
    je gameEnding

    jmp match

gameEnding:
    call clearScreen

    mov ah, 0x13; subservice 13- print string
    mov bh, 0; page number
    mov bl, 7; white on black attribute
    mov dx, 0x091B; location
    mov cx, 20; length of string
    push cs
    pop es; es:bp pointing to string
    mov bp, messageA
    int 0x10

    push word 1534
    push word [scoreA]
    call printnum

    mov ah, 0x13; subservice 13- print string
    mov bh, 0; page number
    mov bl, 7; white on black attribute
    mov dx, 0x0A1B; location
    mov cx, 20; length of string
    push cs
    pop es; es:bp pointing to string
    mov bp, messageB
    int 0x10

    push word 1694
    push word [scoreB]
    call printnum

    cmp word [scoreA], 5
    je playerAWins

    playerBWins:
    mov ah, 0x13; subservice 13- print string
    mov bh, 0; page number
    mov bl, 7; white on black attribute
    mov dx, 0x0B1B; location
    mov cx, 14; length of string
    push cs
    pop es; es:bp pointing to string
    mov bp,WinningPlayerB
    int 0x10

    jmp end
    
    playerAWins:
    mov ah, 0x13; subservice 13- print string
    mov bh, 0; page number
    mov bl, 7; white on black attribute
    mov dx, 0x0B1B; location
    mov cx, 14; length of string
    push cs
    pop es; es:bp pointing to string
    mov bp, WinningPlayerA
    int 0x10

end:
    xor ax, ax
    mov es, ax ; point es to IVT base

    cli ; disable interrupts
    mov ax,[oldkbisr]
    mov word [es:9*4], ax ; store offset at n*4
    mov ax,[oldkbisr+2]
    mov word [es:9*4+2], ax ; store offset at n*4

    mov ax,[oldtimer]
    mov word [es:8*4], ax ; store offset at n*4
    mov ax,[oldtimer+2]
    mov word [es:8*4+2], ax ; store offset at n*4
    sti ; enable interrupts

    mov ax, 0x4C00
    int 0x21