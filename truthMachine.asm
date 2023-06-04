[bits 16]
[org 0x800]

INPUT_PORT equ 0x10
OUTPUT_PORT equ 0x11
EXIT_PORT equ 0x12
CURRENT_KEY equ 0x1080

start: ; Setup input interrupt
    mov WORD [0x20 * 0x4], keyInputInterrupt
    mov WORD [0x20 * 0x4 + 0x2], 0
    mov bp, 0x1000
    mov sp, bp
    sti ; Enable interrupt requests
    jmp keyLoop ; jump to keyLoop

keyInputInterrupt:
    push ax
    in al, INPUT_PORT ; Get the character that has been typed
    mov [CURRENT_KEY], al ; Store it in CURRENT_KEY
    pop ax
    iret ; Return from the interrupt

exit:
    out EXIT_PORT, al
    hlt

keyLoop:
    hlt ; Wait for an interrupt
    mov al, [CURRENT_KEY]
    cmp al, 0 ; Is CURRENT_KEY equal to 0?
    je keyLoop ; Goto back up to keyLoop
printLoop:
    out OUTPUT_PORT, al ; Print out the character
    cmp al, '1' ; Is the character a 1
    je printLoop ; Goto printLoop
    call exit ; Otherwise exit+