#include <iostream>
#include <fstream>

#include <stdio.h>
#include <stdlib.h>

#include "cpu8086.hpp"
#include "monochromeDisplay.hpp"

#define FLG_C 0b0000000000000001
#define FLG_P 0b0000000000000100
#define FLG_A 0b0000000000010000
#define FLG_Z 0b0000000001000000
#define FLG_S 0b0000000010000000
#define FLG_T 0b0000000100000000
#define FLG_I 0b0000001000000000
#define FLG_D 0b0000010000000000
#define FLG_O 0b0000100000000000

#define MEMORY_SIZE 16 * 1024

u8* memory = nullptr;

// Source: https://stackoverflow.com/questions/29242/off-the-shelf-c-hex-dump-code
void hexdump(void *ptr, int buflen) {
    unsigned char *buf = (unsigned char*)ptr;
    int i, j;
    for (i=0; i<buflen; i+=16) {
        printf("%06x: ", i);
        for (j=0; j<16; j++) 
            if (i+j < buflen)
                printf("%02x ", buf[i+j]);
            else
                printf("   ");
        printf(" ");
        for (j=0; j<16; j++) 
            if (i+j < buflen)
                printf("%c", isprint(buf[i+j]) ? buf[i+j] : '.');
        printf("\n");
    }
}

u8 readByte(u32 addr) {
    return memory[addr];
}

void writeByte(u32 addr, u8 data) {
    memory[addr] = data;
}

char CURRENT_KEY;

bool isRunning = true;

void portOut(u16 port, u8 data) {
    if (port == 0x11)
        putchar(data);
    if (port == 0x12)
        isRunning = false;
    // printf("OUT: %#04x, %#02x\n", port, data);
}

u8 portIn(u16 port) {
    if (port == 0x10) {
        char key = CURRENT_KEY;
        CURRENT_KEY = 0;
        return key;
    }
    return 0x4f;
}

int main() {
    memory = new u8[MEMORY_SIZE];

    cpu8086 cpu(readByte, writeByte, portOut, portIn);

    MonochromeDisplay display;
    display.init();

    std::ifstream f("rom.bin", std::ios::binary | std::ios::ate);
    size_t sz = f.tellg();
    f.seekg(0, std::ios_base::beg);
    f.read((char*)(memory + 0x800), sz);
    f.close();

    hexdump(memory + 0x800, sz);
    printf("\n");

    while (isRunning)
        cpu.step();

    hexdump(memory + 0x800, 0x100);

    printf("AX = %04x, BX = %04x, CX = %04x, DX = %04x\n", cpu.AX, cpu.BX, cpu.CX, cpu.DX);
    printf("SP = %04x, BP = %04x, SI = %04x, DI = %04x\n", cpu.SP, cpu.BP, cpu.SI, cpu.DI);
    printf("CS = %04x, DS = %04x, SS = %04x, ES = %04x\n", cpu.CS, cpu.DS, cpu.SS, cpu.ES);
    printf(
        "C=%i, P=%i, A=%i, Z=%i, S=%i, T=%i, I=%i, D=%i, O=%i\n",
        !!(cpu.FLAGS & FLG_C),
        !!(cpu.FLAGS & FLG_P),
        !!(cpu.FLAGS & FLG_A),
        !!(cpu.FLAGS & FLG_Z),
        !!(cpu.FLAGS & FLG_S),
        !!(cpu.FLAGS & FLG_T),
        !!(cpu.FLAGS & FLG_I),
        !!(cpu.FLAGS & FLG_D),
        !!(cpu.FLAGS & FLG_O)
    );

    getchar();
}