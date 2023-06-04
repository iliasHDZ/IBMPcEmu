[bits 16]
[org 0x800]

INPUT_PORT equ 0x10
OUTPUT_PORT equ 0x11
EXIT_PORT equ 0x12

start:
    mov bp, 0x1000
    mov sp, bp
    mov si, helloWorldText
    mov di, 0x880
    mov cx, 14
    rep
    movsd
    mov bx, 0x880
    call printText
    call exit

exit:
    out EXIT_PORT, al
    hlt

printText:
    cmp BYTE [bx], 0
    je printTextEnd
    mov al, BYTE [bx]
    out OUTPUT_PORT, al
    inc bx
    jmp printText
printTextEnd:
    ret

helloWorldText: db "Hello, world!", 0