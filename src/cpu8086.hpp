#pragma once

#include "util.hpp"

#include <vector>

typedef u8 (*ReadMemory)(u32 addr);
typedef void (*WriteMemory)(u32 addr, u8 data);

typedef u8 (*PortIn)(u16 port);
typedef void (*PortOut)(u16 port, u8 data);

struct LongPointer {
    u16 offset;
    u16 segment;
};

class cpu8086 {
public:
    cpu8086(ReadMemory read, WriteMemory write, PortOut out, PortIn in);

    void reset();

    void step();
    
    void intr(u8 code);

private:
    void prepare();
    void execute(u8 ins);

    void movIns(u8 op1, u8 op2, u8 size);
    void pushIns(u8 op);
    void popIns(u8 op);
    void xchgIns(u8 op1, u8 op2, u8 size);
    void leaIns(u8 op1, u8 op2);
    void lptrIns(u8 op1, u8 op2, u8 lds);
    void inIns(u8 op1, u8 op2, u8 size);
    void outIns(u8 op1, u8 op2, u8 size);
    void xlatIns();
    void aluIns(u8 aluOp, u8 op1, u8 op2, u8 size);
    void testIns(u8 op1, u8 op2, u8 size);
    void uopIns(u8 unop, u8 op, u8 size);
    void shfIns(u8 sop, u8 op1, u8 op2, u8 size);
    void countIns(u8 op, u8 size, u8 dec);
    void jmpIns(u16 op, u8 interseg);
    void callIns(u16 op, u8 interseg);
    void retIns(u8 interseg, u8 addsp);
    void jcccIns(u8 cond, u8 op);
    void loopIns(u8 type, u8 op);
    void intIns(u8 op, bool onOverflow);
    void iretIns();
    bool strIns(u8 ins);
    void repIns(u8 z);
    void segIns(u8 seg);

    LongPointer trsfIns(u16 op, u8 interseg, bool call);

    bool testCond(u8 cond);

    void jmp(u16 addr, u16 seg);
    void call(u16 addr, u16 seg, u8 interseg);

    u16 readWordRaw(u32 addr);

    u8  readByte(u16 offset, u16 segment);
    u16 readWord(u16 offset, u16 segment);

    void writeByte(u16 offset, u16 segment, u8 data);
    void writeWord(u16 offset, u16 segment, u16 data);

    void interrupt(u8 code);

    void pushWord(u16 data);
    u16  popWord();

    u8  nextByte();
    u16 nextWord();

    u8  inByte(u16 port);
    u16 inWord(u16 port);
    void outByte(u16 port, u8  data);
    void outWord(u16 port, u16 data);

    void setResFlags(u16 res, u8 size);
    u16 alu(u8 op, u16 a, u16 b, u8 size);
    u16 uop(u8 uop, u16 a, u8 size);
    u16 shf(u8 sop, u16 x, u16 n, u8 size);

    u16* ptrREG(u8 name, u8 size);
    
    u16  getREG(u8 name, u8 size);
    void setREG(u8 name, u8 size, u16 data);

    void decodeModRM();
    void checkModRM(u8 op1, u8 op2);

    u16 getRMAddr();
    u16 getRMSeg();

    u16  getOp(u8 name, u8 size);
    void setOp(u8 name, u8 size, u16 data);

    void logOp(u16 op, u8 size);

    void logIns(const char* mnem, u16 op1 = 0, u16 op2 = 0, u8 size = 0);

private:
    static inline u32 resolveAddress(u16 offset, u16 segment) {
        return (u32)(segment << 4) + (u32)offset;
    }

    static const char* toStrREG(u8 name, u8 size);

    static const char* toFormatRM(u8 modRM, u8 seg = 0);

public:
    u16 AX, BX, CX, DX;
    u16 SP, BP, SI, DI;
    u16 CS, DS, SS, ES;
    u16 IP;
    u16 FLAGS;

    u16* segSS = nullptr;
    u16* segDS = nullptr;

    bool halted;

private:
    bool hasModRM;
    u8   modRM;
    u16  modRMDisp;

    u8 MOD;
    u8 REG;
    u8 RM;

    u16 imm;
    u16 immseg;

    bool iret = false;

    bool swi = false;
    u8 swiCode = 0;

    std::vector<u8> intReqQueue;

private:
    ReadMemory  readRaw;
    WriteMemory writeRaw;
    PortOut rawOut;
    PortIn  rawIn;
};