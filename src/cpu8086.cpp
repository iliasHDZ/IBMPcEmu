#include "cpu8086.hpp"

#include <stdio.h>
#include <stdarg.h>

#define RB_AL 0b000
#define RB_CL 0b001
#define RB_DL 0b010
#define RB_BL 0b011
#define RB_AH 0b100
#define RB_CH 0b101
#define RB_DH 0b110
#define RB_BH 0b111

#define RW_AX 0b000
#define RW_CX 0b001
#define RW_DX 0b010
#define RW_BX 0b011
#define RW_SP 0b100
#define RW_BP 0b101
#define RW_SI 0b110
#define RW_DI 0b111

// Unofficial defines
#define RW_ES 0b1000
#define RW_CS 0b1001
#define RW_SS 0b1010
#define RW_DS 0b1011

#define SZ_BYTE 0
#define SZ_WORD 1

#define OP_NONE    0
#define OP_MODREG  1
#define OP_MODRM   2
#define OP_IMM     3
#define OP_IMM8    4
#define OP_IMM16   5
#define OP_SXIMM8  6
#define OP_ABSMEM  7
#define OP_REG     0b1000
/* to be used as: OP_REG + regName */

#define ALU_ADD 0b000
#define ALU_OR  0b001
#define ALU_ADC 0b010
#define ALU_SBB 0b011
#define ALU_AND 0b100
#define ALU_SUB 0b101
#define ALU_XOR 0b110
#define ALU_CMP 0b111

#define UOP_NOT  0b010
#define UOP_NEG  0b011
#define UOP_MUL  0b100
#define UOP_IMUL 0b101
#define UOP_DIV  0b110
#define UOP_IDIV 0b111

#define SHF_ROL 0b000
#define SHF_ROR 0b001
#define SHF_RCL 0b010
#define SHF_RCR 0b011
#define SHF_SHL 0b100
#define SHF_SHR 0b101
#define SHF_SAR 0b111

#define JMP_JO   0b0000
#define JMP_JNO  0b0001
#define JMP_JC   0b0010
#define JMP_JNC  0b0011
#define JMP_JE   0b0100
#define JMP_JNE  0b0101
#define JMP_JBE  0b0110
#define JMP_JA   0b0111
#define JMP_JS   0b1000
#define JMP_JNS  0b1001
#define JMP_JP   0b1010
#define JMP_JNP  0b1011
#define JMP_JL   0b1100
#define JMP_JGE  0b1101
#define JMP_JLE  0b1110
#define JMP_JG   0b1111

#define INT_DIVIDE_ERROR 0x00
#define INT_SINGLE_STEP  0x01
#define INT_NON_MASKABLE 0x02
#define INT_BREAKPOINT   0x03
#define INT_OVERFLOW     0x04

/*
Flag Effects: 0=Set to 0, 1=Set to 1, X=Result dependant, U=Undefined
         O  D  I  T  S  Z  A  P  C
    ADD  X           X  X  X  X  X
    OR   0           X  X  U  X  0
    ADC  X           X  X  X  X  X
    SBB  X           X  X  X  X  X
    AND  0           X  X  U  X  0
    SUB  X           X  X  X  X  X
    XOR  0           X  X  U  X  0
    CMP  X           X  X  X  X  X
*/

#define FLG_C 0b0000000000000001
#define FLG_P 0b0000000000000100
#define FLG_A 0b0000000000010000
#define FLG_Z 0b0000000001000000
#define FLG_S 0b0000000010000000
#define FLG_T 0b0000000100000000
#define FLG_I 0b0000001000000000
#define FLG_D 0b0000010000000000
#define FLG_O 0b0000100000000000

#define MATCHBITS(A, B) !(A ^ B)
#define SETBITIF(A, B, C) C ? (A | B) : (A & ~B)

#define LOG_INS(...) printf(__VA_ARGS__)
#define DEBUG(...) /* printf(__VA_ARGS__) */

static const u16 dataMasks[] = {
    0xff,
    0xffff
};

static const u16 signMasks[] = {
    0x80,
    0x8000
};

static u32 rol(u32 x, u16 n, u8 bits) {
    return n ? (x << n) | (x >> (bits - n)) : x;
}

static u32 ror(u32 x, u16 n, u8 bits) {
    return n ? (x >> n) | (x << (bits - n)) : x;
}

cpu8086::cpu8086(ReadMemory read, WriteMemory write, PortOut out, PortIn in)
    : readRaw(read), writeRaw(write), rawOut(out), rawIn(in)
{
    reset();
}

void cpu8086::reset() {
    AX = 0, BX = 0, CX = 0, DX = 0;
    SP = 0, BP = 0, SI = 0, DI = 0;
    DS = 0, SS = 0, ES = 0;
    // CS = 0xFFFF;
    // THIS IS TEMPORARY AND SHOULD BE EVENTUALLY REPLACED WITH THE ABOVE COMMENT:
    CS = 0;
    // THE FOLLOWING IS ALSO TEMPORARY AND SHOULD BE REPLACED WITH 0:
    IP = 0x800;
    FLAGS = 0;
    halted = false;
    
    segDS = &DS;
    segSS = &SS;
}

void cpu8086::step() {
    prepare();

    if (!halted) {
        u8 ins = nextByte();
        execute(ins);
    }

    if (!iret) {
        if (swi) { // Check for Software Interrupt
            interrupt(swiCode);
            swi = false;
        } else if ((FLAGS & FLG_T) && !halted) // Check for Single Step Interrupt
            interrupt(INT_SINGLE_STEP);
        else if ((FLAGS & FLG_I) && !intReqQueue.empty()) { // Check for Interrupt Request
            u8 code = intReqQueue[0];
            intReqQueue.erase(intReqQueue.begin());
            interrupt(code);
        }
    } else
        iret = false;
}

void cpu8086::prepare() {
    hasModRM = false;
}

static const char* flagInstructions[] = {
    "pushf",
    "popf",
    "lahf",
    "sahf"
};

void cpu8086::execute(u8 ins) {
    u8 bit1 = ins & 1;

    // printf("FETCH: %02x\n", ins);

    if (MATCHBITS(ins >> 2, 0b100010)) { // mov reg, r/m  (or)  mov r/m, reg
        if (ins & 0b10)
            movIns(OP_MODREG, OP_MODRM, bit1);
        else
            movIns(OP_MODRM, OP_MODREG, bit1);
    } else if (MATCHBITS(ins >> 1, 0b1100011)) { // mov r/m, imm
        movIns(OP_MODRM, OP_IMM, bit1);
    } else if (MATCHBITS(ins >> 4, 0b1011)) { // mov r, imm
        movIns(OP_REG + (ins & 0b111), OP_IMM, (ins >> 3) & 1);
    } else if (MATCHBITS(ins >> 2, 0b101000)) { // mov AX, [imm16] (or) mov [imm16], AX
        if (ins & 0b10)
            movIns(OP_ABSMEM, OP_REG + RW_AX, bit1);
        else
            movIns(OP_REG + RW_AX, OP_ABSMEM, bit1);
    } else if (MATCHBITS(ins >> 2, 0b100011) && !bit1) { // mov sr, r/m (or) r/m, sr
        decodeModRM();
        u8 segReg = (OP_REG + 0b1000) + (REG & 0b11);
        if (ins & 0b10)
            movIns(segReg, OP_MODRM, SZ_WORD);
        else
            movIns(OP_MODRM, segReg, SZ_WORD);
    } else if ((ins >> 1) == 0b1111111) {
        decodeModRM();
        if (REG == 0b000) // inc r/m
            countIns(OP_MODRM, bit1, 0);
        else if (REG == 0b001) // dec r/m
            countIns(OP_MODRM, bit1, 1);

        if (REG == 0b010) // call r/m
            callIns(OP_MODRM, false);
        if (REG == 0b011) // call far r/m
            callIns(OP_MODRM, true);
        if (REG == 0b100) // jmp r/m
            jmpIns(OP_MODRM, false);
        else if (REG == 0b101) // jmp far r/m
            jmpIns(OP_MODRM, true);
        else if (REG == 0b110) // push r/m
            pushIns(OP_MODRM);
    } else if (ins == 0x8f) {
        decodeModRM();
        if (REG == 0b000) // pop r/m
            popIns(OP_MODRM);
    } else if (MATCHBITS(ins >> 4, 0b0101)) {
        u8 reg = OP_REG + (ins & 0b111);
        if (ins & 0b1000) // pop r
            popIns(reg);
        else // push r
            pushIns(reg);
    } else if ((ins >> 5) == 0b000 && (ins >> 1 & 0b11) == 0b11) {
        u8 segReg = (OP_REG + 0b1000) + (ins >> 3 & 0b11);
        if (bit1) // pop sr
            popIns(segReg);
        else // push sr
            pushIns(segReg);
    } else if (MATCHBITS(ins >> 1, 0b1000011)) { // xchg r/m, reg
        xchgIns(OP_MODREG, OP_MODRM, bit1);
    } else if (MATCHBITS(ins >> 3, 0b10010)) { // xchg AX, r16
        xchgIns(OP_REG + RW_AX, OP_REG + (ins & 0b111), bit1);
    } else if ((ins >> 2) == 0b100111) {
        switch (ins & 0b11) {
        case 0: pushWord(FLAGS); break; // pushf
        case 1: FLAGS = popWord(); break; // popf
        case 2: FLAGS = (FLAGS & 0xff00) | (AX >> 8); break; // sahf
        case 3: AX = (AX & 0x00ff) | (FLAGS << 8); break; // lahf
        }
        logIns(flagInstructions[ins & 0b11]);
    } else if (MATCHBITS(ins >> 6, 0b00) && !(ins & 0b100)) { // <alu> reg, r/m  (or)  mov r/m, reg
        u8 aluOp = (ins >> 3) & 0b111;
        if (ins & 0b10)
            aluIns(aluOp, OP_MODREG, OP_MODRM, bit1);
        else
            aluIns(aluOp, OP_MODRM, OP_MODREG, bit1);
    } else if (MATCHBITS(ins >> 2, 0b100000)) { // <alu> r/m, imm
        decodeModRM();
        u8 S = ins & 0b10;
        aluIns(REG, OP_MODRM, bit1 ? (S ? OP_SXIMM8 : OP_IMM16) : OP_IMM8, bit1);
    } else if ((ins >> 2) == 0b000100) { // test reg, r/m (or) r/m, reg
        if (ins & 0b10)
            testIns(OP_MODREG, OP_MODRM, bit1);
        else
            testIns(OP_MODRM, OP_MODREG, bit1);
    } else if ((ins >> 1) == 0b1010100) { // test A, imm
        testIns(OP_REG + RW_AX, OP_IMM, bit1);
    } else if (MATCHBITS(ins >> 6, 0b00) && (ins & 0b100) && !(ins & 0b10)) { // <alu> AX, imm16 (or) AL, imm8
        aluIns((ins >> 3) & 0b111, OP_REG + RW_AX, OP_IMM, bit1);
    } else if (MATCHBITS(ins >> 1, 0b1110010)) { // in (AX/AL), imm8
        inIns(OP_REG + RW_AX, OP_IMM8, bit1);
    } else if (MATCHBITS(ins >> 1, 0b1110110)) { // in (AX/AL), DX
        inIns(OP_REG + RW_AX, OP_REG + RW_DX, bit1);
    } else if (MATCHBITS(ins >> 1, 0b1110011)) { // out imm8, (AX/AL)
        outIns(OP_IMM8, OP_REG + RW_AX, bit1);
    } else if (MATCHBITS(ins >> 1, 0b1110111)) { // out DX, (AX/AL)
        outIns(OP_REG + RW_DX, OP_REG + RW_AX, bit1);
    } else if (ins == 0xd7) { // xlat
        xlatIns();
    } else if (ins == 0x8d) { // lea reg, mem
        leaIns(OP_MODREG, OP_MODRM);
    } else if ((ins >> 1) == 0b1100010) { // (lds/les) reg, mem
        lptrIns(OP_MODREG, OP_MODRM, bit1);
    } else if ((ins >> 4) == 0b0100) { // inc/dec r
        countIns(OP_REG + (ins & 0b111), SZ_WORD, (ins >> 3) & 1);
    } else if ((ins >> 1) == 0b1111011) {
        decodeModRM();
        if (REG == 0x00)
            testIns(OP_MODRM, OP_IMM, bit1); // test r/m, imm
        else
            uopIns(REG, OP_MODRM, bit1); // <uop> r/m
    } else if (ins == 0x98) { // cbw
        AX = (i16)(i8)(AX & 0xff);
        logIns("cbw");
    } else if (ins == 0x99) { // cbw
        u32 full = (i32)(i16)AX;
        AX = full & 0xffff;
        DX = full >> 16;
        logIns("cwd");
    } else if ((ins >> 2) == 0b110100) { // <shf> r/m[, cl]
        decodeModRM();
        shfIns(REG, OP_MODRM, (ins & 0b10) ? (OP_REG + RB_CL) : OP_NONE, bit1);
    } else if ((ins >> 1) == 0b1110100) {
        if (bit1) // jmp ip16
            jmpIns(OP_IMM16, false);
        else // call ip16
            callIns(OP_IMM16, false);
    } else if (ins == 0xeb) { // jmp short ip8
        jmpIns(OP_SXIMM8, false);
    } else if (ins == 0xea) { // jmp imm16:imm16
        jmpIns(OP_IMM16, true);
    } else if (ins == 0x9a) { // call imm16:imm16
        callIns(OP_IMM16, true);
    } else if ((ins >> 1) == 0b1100001) { // ret [imm16]
        retIns(false, !bit1);
    } else if ((ins >> 1) == 0b1100101) { // ret far [imm16]
        retIns(true, !bit1);
    } else if ((ins >> 4) == 0b0111) { // <jccc> imm8
        jcccIns(ins & 0xf, OP_SXIMM8);
    } else if ((ins >> 2) == 0b111000) { // <loop> imm8
        loopIns(ins & 0b11, OP_SXIMM8);
    } else if ((ins >> 1) == 0b1100110) { // int imm (or) int 0x3
        intIns(bit1 ? OP_IMM8 : OP_NONE, false);
    } else if (ins == 0xce) { // into
        intIns(OP_NONE, true);
    } else if (ins == 0b11001111) { // iret
        iretIns();
    } else if (strIns(ins)) { // movs (or) cmps (or) scas (or) lods (or) stos
        return;
    } else if ((ins >> 1) == 0b1111001) { // rep
        repIns(bit1);
    } else if (ins == 0xf8) {
        FLAGS &= ~(FLG_C);
        logIns("clc", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xf5) {
        FLAGS ^= FLG_C;
        logIns("cmc", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xf9) {
        FLAGS |= FLG_C;
        logIns("stc", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xfc) {
        FLAGS &= ~(FLG_D);
        logIns("cld", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xfd) {
        FLAGS |= FLG_D;
        logIns("std", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xfa) {
        FLAGS &= ~(FLG_I);
        logIns("cli", OP_NONE, OP_NONE, 0);
    } else if (ins == 0xfb) {
        FLAGS |= FLG_I;
        logIns("sti", OP_NONE, OP_NONE, 0);
    } else if (MATCHBITS(ins, 0b11110100)) { // hlt
        halted = true;
        logIns("hlt");
    } else if ((ins >> 5) == 0b001 && (ins & 0b111) == 0b110) { // segment
        segIns((ins >> 3) & 0b11);
    }
}

void cpu8086::movIns(u8 op1, u8 op2, u8 size) {
    checkModRM(op1, op2);
    u16 data = getOp(op2, size);
    setOp(op1, size, data);
    logIns("mov", op1, op2, size);
}

void cpu8086::pushIns(u8 op) {
    checkModRM(op, OP_NONE);
    u16 data = getOp(op, SZ_WORD);
    pushWord(data);
    logIns("push", op, OP_NONE, SZ_WORD);
}

void cpu8086::popIns(u8 op) {
    checkModRM(op, OP_NONE);
    u16 data = popWord();
    setOp(op, SZ_WORD, data);
    logIns("pop", op, OP_NONE, SZ_WORD);
}

void cpu8086::xchgIns(u8 op1, u8 op2, u8 size) {
    checkModRM(op1, op2);
    u16 dat1 = getOp(op1, size);
    u16 dat2 = getOp(op2, size);
    setOp(op2, size, dat1);
    setOp(op1, size, dat2);
    logIns("xchg", op1, op2, size);
}

void cpu8086::leaIns(u8 op1, u8 op2) {
    checkModRM(op1, op2);
    u16 addr = getRMAddr();
    setOp(op1, SZ_WORD, addr);
    logIns("lea", op1, op2, SZ_WORD);
}

void cpu8086::lptrIns(u8 op1, u8 op2, u8 lds) {
    // Apparently I cannot test LDS and LES because nasm won't assemble them
    // TODO: Test this somehow
    checkModRM(op1, op2);
    u16 addr = getRMAddr();
    u16 off = readWord(addr, DS);
    u16 seg = readWord(addr + 2, DS);
    setOp(op1, SZ_WORD, off);
    if (lds)
        DS = seg;
    else
        ES = seg;
    logIns(lds ? "lds" : "les", op1, op2, SZ_WORD);
}

void cpu8086::inIns(u8 op1, u8 op2, u8 size) {
    u16 port = getOp(op2, SZ_WORD);
    setOp(op1, size, (size == SZ_BYTE) ? inByte(port) : inWord(port));
    
    LOG_INS("in ");
    logOp(op1, size);
    LOG_INS(", ");
    logOp(op2, SZ_WORD);
    LOG_INS("\n");
}

void cpu8086::outIns(u8 op1, u8 op2, u8 size) {
    u16 port = getOp(op1, SZ_WORD);
    u16 data = getOp(op2, size);
    if (size == SZ_BYTE)
        outByte(port, data);
    else
        outWord(port, data);
    
    LOG_INS("out ");
    logOp(op1, SZ_WORD);
    LOG_INS(", ");
    logOp(op2, size);
    LOG_INS("\n");
}

void cpu8086::xlatIns() { // WARN: Not yet tested
    u8 idx = AX & 0xff;
    *(u8*)AX = readByte(BX + idx, CS);
    logIns("xlat");
}

static const char* aluInstructions[] = {
    "add",
    "or",
    "adc",
    "sbb",
    "and",
    "sub",
    "xor",
    "cmp"
};

void cpu8086::aluIns(u8 aluOp, u8 op1, u8 op2, u8 size) {
    checkModRM(op1, op2);
    u16 a = getOp(op1, size);
    u16 b = getOp(op2, size);

    u16 res = alu(aluOp, a, b, size);
    if (aluOp != ALU_CMP)
        setOp(op1, size, res);

    logIns(aluInstructions[aluOp], op1, op2, size);
}

void cpu8086::testIns(u8 op1, u8 op2, u8 size) { // WARN: Not test
    checkModRM(op1, op2);
    u16 a = getOp(op1, size);
    u16 b = getOp(op2, size);

    alu(ALU_AND, a, b, size);
    logIns("test", op1, op2, size);
}

static const char* uopInstructions[] = {
    "<invalid>",
    "<invalid>",
    "not",
    "neg",
    "mul",
    "imul",
    "div",
    "idiv"
};

void cpu8086::uopIns(u8 unop, u8 op, u8 size) {
    checkModRM(op, OP_NONE);
    u16 a = getOp(op, size);

    u16 res = uop(unop, a, size);
    if (unop == UOP_NEG || unop == UOP_NOT)
        setOp(op, size, res);

    logIns(uopInstructions[unop], op, OP_NONE, size);
}

static const char* shfInstructions[] = {
    "rol",
    "ror",
    "rcl",
    "rcr",
    "shl",
    "shr",
    "<invalid>",
    "sar"
};

void cpu8086::shfIns(u8 sop, u8 op1, u8 op2, u8 size) {
    checkModRM(op1, op2);
    u16 x = getOp(op1, size);
    u16 n;
    
    if (op2 == OP_NONE)
        n = 1;
    else
        n = getOp(op2, SZ_BYTE);

    u16 res = shf(sop, x, n, size);
    setOp(op1, size, res);
    
    LOG_INS("%s ", shfInstructions[sop]);
    logOp(op1, size);
    if (op2 != OP_NONE) {
        LOG_INS(", ");
        logOp(op2, SZ_BYTE);
    } else
        LOG_INS(", 1");
    LOG_INS("\n");
}

void cpu8086::countIns(u8 op, u8 size, u8 dec) {
    checkModRM(op, OP_NONE);
    u16 a = getOp(op, size);

    u16 res = alu(dec ? ALU_SUB : ALU_ADD, a, 1, size);
    setOp(op, size, res);

    logIns(dec ? "dec" : "inc", op, OP_NONE, size);
}

void cpu8086::jmpIns(u16 op, u8 interseg) {
    auto ptr = trsfIns(op, interseg, false);
    jmp(ptr.offset, ptr.segment);
}

void cpu8086::callIns(u16 op, u8 interseg) {
    auto ptr = trsfIns(op, interseg, true);
    call(ptr.offset + IP, ptr.segment, interseg);
}

void cpu8086::retIns(u8 interseg, u8 addsp) {
    u16 spoff = 0;
    if (addsp) {
        spoff = nextWord();
        imm = spoff;
    }

    IP = popWord();
    if (interseg)
        CS = popWord();
    
    logIns(interseg ? "ret far" : "ret", addsp ? OP_IMM16 : OP_NONE, OP_NONE, SZ_WORD);
}

static const char* jcccInstructions[] = {
    "jo",
    "jno",
    "jc",
    "jnc",
    "je",
    "jne",
    "jbe",
    "ja",
    "js",
    "jns",
    "jp",
    "jnp",
    "jl",
    "jge",
    "jle",
    "jg"
};

void cpu8086::jcccIns(u8 cond, u8 op) {
    u16 addr = getOp(op, SZ_BYTE);
    if (testCond(cond))
        IP += addr;
    logIns(jcccInstructions[cond], op);
};

static const char* loopInstructions[] = {
    "loopne",
    "loope",
    "loop",
    "jcxz"
};

// WARN: Most loop instructions are not yet tested
void cpu8086::loopIns(u8 type, u8 op) {
    bool cond = false;
    switch (type) {
    case 0: // loopne
        cond = (--CX != 0) && !(FLAGS & FLG_Z);
        break;
    case 1: // loope
        cond = (--CX != 0) &&  (FLAGS & FLG_Z);
        break;
    case 2: // loop
        cond = --CX != 0;
        break;
    case 3: // jcxz
        cond = !CX;
        break;
    }

    u16 addr = getOp(op, SZ_BYTE);
    if (cond) IP += addr;
    logIns(loopInstructions[type], op);
}

void cpu8086::intIns(u8 op, bool onOverflow) {
    if (onOverflow && !(FLAGS & FLG_O)) {
        logIns("into");
        return;
    }

    if (op == OP_NONE) {
        swiCode = onOverflow ? INT_OVERFLOW : INT_BREAKPOINT;
        LOG_INS(onOverflow ? "into\n" : "int 0x3\n");
    } else {
        swiCode = getOp(op, SZ_BYTE);
        logIns("int", op);
    }

    swi = true;
}

void cpu8086::iretIns() {
    DEBUG("prev IP = %04x\n", IP);
    IP = popWord();
    DEBUG("next IP = %04x\n", IP);
    CS = popWord();
    FLAGS = popWord();
    iret = true;
    logIns("iret");
}

#define STR_MOVS 0b1010010
#define STR_CMPS 0b1010011
#define STR_SCAS 0b1010111
#define STR_LODS 0b1010110
#define STR_STOS 0b1010101

bool cpu8086::strIns(u8 ins) {
    u8 insh  = ins >> 1;
    u8 w     = ins & 1;
    i8 delta = w + 1;
    if (FLAGS & FLG_D)
        delta = -delta;

    u16 a, b;
    switch (insh) {
    case STR_MOVS: // WARN: Only MOVS has been tested, the other string instructions may not function correctly
        a = w ? readWord(SI, *segDS) : readByte(SI, *segDS);
        if (w)
            writeWord(DI, ES, a);
        else
            writeByte(DI, ES, a);
        logIns("movs");
        break;
    case STR_CMPS:
        a = w ? readWord(DI, ES) : readByte(DI, ES);
        b = w ? readWord(SI, *segDS) : readByte(SI, *segDS);
        alu(ALU_SUB, a, b, w);
        logIns("cmps");
        break;
    case STR_SCAS:
        a = w ? readWord(DI, ES) : readByte(DI, ES);
        alu(ALU_SUB, a, getREG(RW_AX, w), w);
        logIns("scas");
        break;
    case STR_LODS:
        a = w ? readWord(SI, *segDS) : readByte(SI, *segDS);
        setREG(RW_AX, w, a);
        logIns("lods");
        break;
    case STR_STOS:
        a = getREG(RW_AX, w);
        if (w)
            writeWord(DI, ES, a);
        else
            writeByte(DI, ES, a);
        logIns("stos");
        break;
    default:
        return false;
    }

    if (insh != STR_LODS)
        DI += delta;
    if (insh != STR_SCAS && insh != STR_STOS)
        SI += delta;
    return true;
}

void cpu8086::repIns(u8 z) {
    u16 insAddr = IP - 1;
    logIns(z ? "rep" : "repne");
    if (CX == 0)
        return;
    CX--;

    DEBUG("CX: %02x\n", CX);

    u8 sins = readByte(IP, CS);
    if (sins == 0x66) {
        nextByte();
        sins = readByte(IP, CS);
    }

    if (!strIns(sins)) {
        printf("warning: unexpected instruction after rep\n");
        return;
    }
    nextByte();

    if (sins == STR_CMPS || sins == STR_SCAS)
        if (!!(FLAGS & FLG_Z) == z)
            return;
    
    if (CX != 0)
        IP = insAddr;
}

void cpu8086::segIns(u8 seg) { // WARN: Not yet tested
    u16* segPtr;

    switch (seg) {
    case RW_ES: segPtr = &ES; break;
    case RW_CS: segPtr = &CS; break;
    case RW_SS: segPtr = &SS; break;
    case RW_DS: segPtr = &DS; break;
    default: return;
    }

    segSS = segPtr;
    segDS = segPtr;

    execute(nextByte());
    segSS = &SS;
    segDS = &DS;
}

static const char* jmpInstructions[] = {
    "jmp %#04x:%#04x\n",
    "jmp far",
    "jmp short",
    "jmp"
};

static const char* callInstructions[] = {
    "call %#04x:%#04x\n",
    "call far",
    "call short",
    "call"
};

LongPointer cpu8086::trsfIns(u16 op, u8 interseg, bool call) {
    checkModRM(op, OP_NONE);
    u16 addr;
    u16 seg = CS;
    const char** ins = call ? callInstructions : jmpInstructions;

    if (interseg) {
        if (op == OP_IMM16) {
            addr = getOp(op, SZ_BYTE);
            seg = nextWord();
            LOG_INS(ins[0], seg, addr);
        } else {
            u16 paddr = getRMAddr();
            addr = readWord(paddr, *segDS);
            seg  = readWord(paddr + 2, *segDS);
            logIns(ins[1], op, OP_NONE, SZ_WORD);
        }
    } else {
        addr = getOp(op, SZ_WORD);
        if (op == OP_SXIMM8) {
            addr += IP;
            logIns(ins[2], op, OP_NONE, SZ_WORD);
        } else
            logIns(ins[3], op, OP_NONE, SZ_WORD);
    }

    return {addr, seg};
}

// WARN: Not all conditions are tested
bool cpu8086::testCond(u8 cond) {
    bool cf = !!(FLAGS & FLG_C);
    bool zf = !!(FLAGS & FLG_Z);
    bool sf = !!(FLAGS & FLG_S);
    bool of = !!(FLAGS & FLG_O);
    bool pf = !!(FLAGS & FLG_P);

    switch (cond) {
    case JMP_JO:  return of;
    case JMP_JNO: return !of;
    case JMP_JC:  return cf;
    case JMP_JNC: return !cf;
    case JMP_JE:  return zf;
    case JMP_JNE: return !zf;
    case JMP_JBE: return cf || zf;
    case JMP_JA:  return !(cf || zf);
    case JMP_JS:  return sf;
    case JMP_JNS: return !sf;
    case JMP_JP:  return pf;
    case JMP_JNP: return !pf;
    case JMP_JL:  return sf != of;
    case JMP_JGE: return !(sf != of);
    case JMP_JLE: return sf != of || zf;
    case JMP_JG:  return !(sf != of || zf);
    };

    return false;
}

void cpu8086::jmp(u16 addr, u16 seg) {
    DEBUG("prev IP = %04x\n", IP);
    DEBUG("next IP = %04x\n", addr);
    IP = addr;
    CS = seg;
}

void cpu8086::call(u16 addr, u16 seg, u8 interseg) {
    if (interseg) {
        pushWord(CS);
        CS = seg;
    }
    DEBUG("prev IP = %04x\n", IP);
    DEBUG("next IP = %04x\n", addr);
    pushWord(IP);
    IP = addr;
}

u16 cpu8086::readWordRaw(u32 addr) {
    return (u16)readRaw(addr) | (u16)readRaw(addr + 1) << 8;
}

u8 cpu8086::readByte(u16 offset, u16 segment) {
    return readRaw(resolveAddress(offset, segment));
}

u16 cpu8086::readWord(u16 offset, u16 segment) {
    return readWordRaw(resolveAddress(offset, segment));
}

void cpu8086::writeByte(u16 offset, u16 segment, u8 data) {
    writeRaw(resolveAddress(offset, segment), data);
}

void cpu8086::writeWord(u16 offset, u16 segment, u16 data) {
    u32 addr = resolveAddress(offset, segment);
    writeRaw(addr, data & 0xff);
    writeRaw(addr + 1, data >> 8);
}

void cpu8086::intr(u8 code) {
    intReqQueue.push_back(code);
}

void cpu8086::interrupt(u8 code) {
    DEBUG("-- INTERRUPT -- (code: %#02x)\n", code);

    u16 off = readWordRaw(code << 2);
    u16 seg = readWordRaw(code << 2 + 2);

    pushWord(FLAGS);

    bool tf = !!(FLAGS & FLG_T);
    FLAGS &= ~(FLG_I | FLG_T);

    pushWord(CS);
    CS = seg;
    pushWord(IP);
    IP = off;

    halted = false;

    // TODO: Find out why the manual says to check for NMI?

    if (tf && code != INT_SINGLE_STEP)
        interrupt(INT_SINGLE_STEP);
}

void cpu8086::pushWord(u16 data) {
    DEBUG("PUSH: %04x\n", data);
    SP -= 2;
    writeWord(SP, *segSS, data);
}

u16 cpu8086::popWord() {
    u16 ret = readWord(SP, *segSS);
    DEBUG("POP: %04x\n", ret);
    SP += 2;
    return ret;
}

u8 cpu8086::nextByte() {
    u8 ret = readByte(IP, CS);
    // DEBUG("NEXT BYTE: %02x(%04x)\n", ret, IP);
    IP++;
    return ret;
}

u16 cpu8086::nextWord() {
    u16 ret = readWord(IP, CS);
    // DEBUG("NEXT WORD: %04x(%04x)\n", ret, IP);
    IP += 2;
    return ret;
}

u8 cpu8086::inByte(u16 port) {
    return rawIn(port);
}

u16 cpu8086::inWord(u16 port) {
    return (u16)rawIn(port) | (u16)rawIn(port + 1) << 8;
}

void cpu8086::outByte(u16 port, u8 data) {
    rawOut(port, data);
}

void cpu8086::outWord(u16 port, u16 data) {
    rawOut(port, data & 0xff);
    rawOut(port + 1, data >> 8);
}

void cpu8086::setResFlags(u16 res, u8 size) {
    FLAGS = SETBITIF(FLAGS, FLG_Z, res == 0);

    /* // Parity calculation is slow (probably)
    int bitcount = 0;
    for (int i = 1; i <= 0x8000; i <<= 1)
        if (res & i) bitcount++;
    
    FLAGS = SETBITIF(FLAGS, FLG_P, !(bitcount & 1));
    */
    FLAGS &= ~FLG_P;
    FLAGS = SETBITIF(FLAGS, FLG_S, res & signMasks[size]);
}

u16 cpu8086::alu(u8 op, u16 a, u16 b, u8 size) {
    u32 res = 0;
    if (op == ALU_ADC || op == ALU_SBB)
        if (FLAGS & FLG_C) res++;

    bool checkCAndO = false;

    switch (op) {
    case ALU_ADC:
    case ALU_ADD:
        res = (u32)a + (u32)b + res;
        checkCAndO = true;
        break;
    case ALU_SBB:
    case ALU_SUB:
    case ALU_CMP:
        res = (u32)a - (u32)b - res;
        checkCAndO = true;
        break;
    case ALU_AND: res = a & b; break;
    case ALU_OR:  res = a | b; break;
    case ALU_XOR: res = a ^ b; break;
    }

    if (checkCAndO) {
        FLAGS = SETBITIF(FLAGS, FLG_C, res & ((size == SZ_BYTE) ? 0x100 : 0x10000));

        u16 sign = (size == SZ_BYTE) ? 7 : 15;

        /* Overflow detection not tested */
        u8 As = a >> sign;
        u8 Bs = a >> sign;
        u8 Os = res >> sign;
    
        if (op == ALU_ADD || op == ALU_ADC)
            FLAGS = SETBITIF(FLAGS, FLG_O, As == Bs && As != Os);
        else
            FLAGS = SETBITIF(FLAGS, FLG_O, As != Bs && As != Os);
    }

    if (size == SZ_BYTE)
        res &= 0xff;

    setResFlags(res, size);
    return res;
}

u16 cpu8086::uop(u8 uop, u16 a, u8 size) {
    u16 res = 0;
    switch (uop) {
    case UOP_NOT:
        res = ~a;
        break;
    case UOP_NEG:
        if (a == signMasks[size]) {
            FLAGS |= FLG_C;
            return a;
        }
        res = alu(ALU_SUB, 0, a, size);
        FLAGS &= ~FLG_C;
        break;
    }

    if (uop == UOP_NOT || uop == UOP_NEG) {
        setResFlags(res, size);
        return res;
    }

    u16 lores = 0;
    u16 hires = 0;
    u32 temp  = 0;

    u16 accum = getOp(OP_REG + RW_AX, size);

    if ((uop == UOP_IMUL || uop == UOP_IDIV) && size == SZ_BYTE) {
        accum = (i16)(i8)accum;
        a     = (i16)(i8)a;
    }

    // Signed multiplication and division is not tested and probably might not work!

    switch (uop) {
    case UOP_MUL:
    case UOP_IMUL:
        if (uop == UOP_MUL)
            temp = accum * a;
        else if (uop == UOP_IMUL)
            temp = (i16)accum * (i16)a;

        if (size == SZ_BYTE) {
            lores = temp & 0xff;
            hires = (temp >> 8) & 0xff;
        } else {
            lores = temp & 0xffff;
            hires = (temp >> 16) & 0xffff;
        }

        FLAGS = SETBITIF(FLAGS, FLG_C, hires != 0);
        break;
    case UOP_DIV:
    case UOP_IDIV:
        // FIXME: Check for division by zero and interrupt
        if (uop == UOP_DIV) {
            lores = accum / a;
            hires = accum % a;
        } else if (uop == UOP_IDIV) {
            lores = (i16)accum / (i16)a;
            hires = (i16)accum % (i16)a;
        }
        break;
    }

    if (size == SZ_BYTE) {
        setOp(OP_REG + RW_AX, SZ_WORD, (hires << 8) | (lores & 0xff));
    } else {
        setOp(OP_REG + RW_AX, SZ_WORD, lores);
        setOp(OP_REG + RW_DX, SZ_WORD, hires);
    }

    return 0;
}

u16 cpu8086::shf(u8 sop, u16 x, u16 n, u8 size) {
    u32 res = 0;
    u8  bits = size == SZ_BYTE ? 8 : 16;

    switch (sop) {
    case SHF_ROL:
        res = rol(x, n, bits);
        break;
    case SHF_ROR:
        res = ror(x, n, bits);
        break;
    case SHF_RCL:
        res = rol((u32)x << 1 | !!(FLAGS & FLG_C), n, bits + 1);
        FLAGS = SETBITIF(FLAGS, FLG_C, res & 1);
        res >>= 1;
        break;
    case SHF_RCR:
        res = rol((u32)x | (!!(FLAGS & FLG_C) << bits), n, bits + 1);
        FLAGS = SETBITIF(FLAGS, FLG_C, res >> bits);
        res &= dataMasks[size];
        break;
    case SHF_SHL:
        res = x << n;
        break;
    case SHF_SHR:
        res = x >> n;
        break;
    case SHF_SAR:
        if (size == SZ_BYTE)
            res = (i8)x >> n;
        else
            res = (i16)x >> n;
        break;
    }

    if (sop == SHF_SHL || sop == SHF_SHR || sop == SHF_SAR) {
        u16 mask = signMasks[size];
        FLAGS = SETBITIF(FLAGS, FLG_C, (res & mask) != (x & mask));
    }

    return res;
}

u16* cpu8086::ptrREG(u8 name, u8 size) {
    if (size == SZ_BYTE) {
        switch (name) {
        case RB_AL: return &AX;
        case RB_CL: return &CX;
        case RB_DL: return &DX;
        case RB_BL: return &BX;
        case RB_AH: return (u16*)((u8*)(&AX) + 1);
        case RB_CH: return (u16*)((u8*)(&CX) + 1);
        case RB_DH: return (u16*)((u8*)(&DX) + 1);
        case RB_BH: return (u16*)((u8*)(&BX) + 1);
        }
    } else {
        switch (name) {
        case RW_AX: return &AX;
        case RW_CX: return &CX;
        case RW_DX: return &DX;
        case RW_BX: return &BX;
        case RW_SP: return &SP;
        case RW_BP: return &BP;
        case RW_SI: return &SI;
        case RW_DI: return &DI;
        // Unofficial
        case RW_ES: return &ES;
        case RW_CS: return &CS;
        case RW_SS: return &SS;
        case RW_DS: return &DS;
        }
    }

    return nullptr;
}

u16 cpu8086::getREG(u8 name, u8 size) {
    u16* r = ptrREG(name, size);
    if (size == SZ_BYTE)
        return *(u8*)r;
    else
        return *r;
}

void cpu8086::setREG(u8 name, u8 size, u16 data) {
    u16* r = ptrREG(name, size);
    if (size == SZ_BYTE)
        *(u8*)r = (u8)data;
    else
        *r = data;
}

void cpu8086::decodeModRM() {
    if (hasModRM)
        return;

    modRM = nextByte();

    MOD = modRM >> 6;
    REG = (modRM >> 3) & 0b111;
    RM  = modRM & 0b111;

    if (MOD == 0b00 && RM == 0b110)
        modRMDisp = nextWord();
    else if (MOD == 0b01)
        modRMDisp = (i16)(i8)nextByte();
    else if (MOD == 0b10)
        modRMDisp = nextWord();
    
    hasModRM = true;
}

void cpu8086::checkModRM(u8 op1, u8 op2) {
    if (op1 == OP_MODREG || op1 == OP_MODRM || op2 == OP_MODREG || op2 == OP_MODRM)
        decodeModRM();
}

u16 cpu8086::getRMAddr() {
    u16 addr = 0;
    if (MOD == 0b11)
        return 0; // TODO: 286: Invalid Opcode

    switch (RM) {
    case 0b000: addr = BX + SI; break;
    case 0b001: addr = BX + DI; break;
    case 0b010: addr = BP + SI; break;
    case 0b011: addr = BP + DI; break;
    case 0b100: addr = SI; break;
    case 0b101: addr = DI; break;
    case 0b110: addr = (MOD == 0b00) ? modRMDisp : BP; break;
    case 0b111: addr = BX; break;
    };

    if (MOD == 0b01 || MOD == 0b10)
        return addr + modRMDisp;

    return addr;
}

u16 cpu8086::getRMSeg() {
    if (MOD == 0b11)
        return 0;
    
    if (RM == 0b010 || RM == 0b011 || (MOD != 0b00 && RM == 0b110))
        return *segSS;
    
    return *segDS;
}

u16 cpu8086::getOp(u8 name, u8 size) {
    if ((name == OP_MODREG || name == OP_MODRM) && !hasModRM)
        printf("error: modr/m not decoded yet\n");
    
    if (name == OP_MODREG)
        return getOp(OP_REG + REG, size);
    else if (name == OP_MODRM) {
        if (MOD == 0b11)
            return getOp(OP_REG + RM, size);
        else {
            if (size == SZ_BYTE)
                return readByte(getRMAddr(), getRMSeg());
            else
                return readWord(getRMAddr(), getRMSeg());
        }
    } else if (name == OP_IMM || name == OP_IMM8 || name == OP_IMM16) {
        u16 ret = 0;
        if ((name == OP_IMM && size == SZ_BYTE) || name == OP_IMM8)
            ret = nextByte();
        else
            ret = nextWord();
        imm = ret;
        return ret;
    } else if (name == OP_SXIMM8) {
        i8 ret = nextByte();
        imm = (i16)ret;
        return imm;
    } else if (name == OP_ABSMEM) {
        u16 addr = nextWord();
        imm = addr;
        if (size == SZ_BYTE)
            return readByte(addr, *segDS);
        else
            return readWord(addr, *segDS);
    } else if (name >= OP_REG)
        return getREG(name - OP_REG, size);

    return 0;
}

void cpu8086::setOp(u8 name, u8 size, u16 data) {
    if ((name == OP_MODREG || name == OP_MODRM) && !hasModRM)
        printf("error: modr/m not decoded yet\n");
    
    if (name == OP_MODREG)
        return setOp(OP_REG + REG, size, data);
    else if (name == OP_MODRM) {
        if (MOD == 0b11)
            return setOp(OP_REG + RM, size, data);
        else {
            if (size == SZ_BYTE)
                return writeByte(getRMAddr(), getRMSeg(), data);
            else
                return writeWord(getRMAddr(), getRMSeg(), data);
        }
    } else if (name == OP_ABSMEM) {
        u16 addr = nextWord();
        imm = addr;
        if (size == SZ_BYTE)
            writeByte(addr, *segDS, data);
        else
            writeWord(addr, *segDS, data);
        return;
    } else if (name >= OP_REG)
        return setREG(name - OP_REG, size, data);
}

void cpu8086::logOp(u16 op, u8 size) {
    if ((op == OP_MODREG || op == OP_MODRM) && !hasModRM) {
        LOG_INS("undefined");
        return;
    }

    if (op == OP_MODREG) {
        logOp(OP_REG + REG, size);
        return;
    } else if (op == OP_MODRM) {
        if (MOD == 0b11)
            logOp(OP_REG + RM, size);
        else {
            LOG_INS(size == SZ_BYTE ? "BYTE " : "WORD ");
            LOG_INS(toFormatRM(modRM), modRMDisp);
        }
        return;
    } else if (op == OP_IMM) {
        if (size == SZ_BYTE)
            LOG_INS("%#02x", imm);
        else
            LOG_INS("%#04x", imm);
        return;
    } else if (op == OP_IMM8) {
        LOG_INS("%#02x", imm);
        return;
    } else if (op == OP_IMM16 || op == OP_SXIMM8) {
        LOG_INS("%#04x", imm);
        return;
    } else if (op == OP_ABSMEM) {
        LOG_INS(size == SZ_BYTE ? "BYTE " : "WORD ");
        LOG_INS("[%#04x]", imm);
        return;
    } else if (op >= OP_REG) {
        LOG_INS(toStrREG(op - OP_REG, size));
        return;
    }

    LOG_INS("undefined");
}

void cpu8086::logIns(const char* mnem, u16 op1, u16 op2, u8 size) {
    LOG_INS("%s ", mnem);

    if (op1 != OP_NONE)
        logOp(op1, size);
    if (op2 != OP_NONE) {
        LOG_INS(", ");
        logOp(op2, size);
    }

    LOG_INS("\n");
}

static const char* reg_names[] = {
    // 8-bit Registers
    "AL",
    "CL",
    "DL",
    "BL",
    "AH",
    "CH",
    "DH",
    "BH",

    "", "", "", "",
    "", "", "", "",

    // 16-bit Registers
    "AX",
    "CX",
    "DX",
    "BX",
    "SP",
    "BP",
    "SI",
    "DI",

    "ES",
    "CS",
    "SS",
    "DS"
};

const char* cpu8086::toStrREG(u8 name, u8 size) {
    return reg_names[name | size << 4];
}

static const char* addr_modes[] = {
    "[BX+SI]",
    "[BX+DI]",
    "[BP+SI]",
    "[BP+DI]",
    "[SI]",
    "[DI]",
    "[%#04x]",
    "[BX]",
    
    "[BX+SI+%#02x]",
    "[BX+DI+%#02x]",
    "[BP+SI+%#02x]",
    "[BP+DI+%#02x]",
    "[SI+%#02x]",
    "[DI+%#02x]",
    "[BP+%#02x]",
    "[BX+%#02x]",
    
    "[BX+SI+%#04x]",
    "[BX+DI+%#04x]",
    "[BP+SI+%#04x]",
    "[BP+DI+%#04x]",
    "[SI+%#04x]",
    "[DI+%#04x]",
    "[BP+%#04x]",
    "[BX+%#04x]",
};

/*static const char* addr_seg_modes[] = {
    "[%#04x:BX+SI]",
    "[%#04x:BX+DI]",
    "[%#04x:BP+SI]",
    "[%#04x:BP+DI]",
    "[%#04x:SI]",
    "[%#04x:DI]",
    "[%#04x:%#04x]",
    "[%#04x:BX]",
    
    "[%#04x:BX+SI+%#02x]",
    "[%#04x:BX+DI+%#02x]",
    "[%#04x:BP+SI+%#02x]",
    "[%#04x:BP+DI+%#02x]",
    "[%#04x:SI+%#02x]",
    "[%#04x:DI+%#02x]",
    "[%#04x:BP+%#02x]",
    "[%#04x:BX+%#02x]",
    
    "[%#04x:BX+SI+%#04x]",
    "[%#04x:BX+DI+%#04x]",
    "[%#04x:BP+SI+%#04x]",
    "[%#04x:BP+DI+%#04x]",
    "[%#04x:SI+%#04x]",
    "[%#04x:DI+%#04x]",
    "[%#04x:BP+%#04x]",
    "[%#04x:BX+%#04x]",
};*/

const char* cpu8086::toFormatRM(u8 modRM, u8 seg) {
    u8 idx = (modRM >> 3) & 0b11000 | modRM & 0b00111;
    if ((idx & 0b11000) == 0b11000)
        return nullptr;
    return /*seg ? addr_seg_modes[idx] : */addr_modes[idx];
}

