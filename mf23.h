#ifndef MF23_H
#define MF23_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum {
    INS_PUSH=0, INS_CALLC,
    INS_ADD, INS_SUB, INS_DIV, INS_MUL,
    INS_AND, INS_OR, INS_XOR, INS_INV,
    INS_INC, INS_DEC, INS_SHL, INS_SHR, INS_ZEQ,
    INS_DUP, INS_OVER, INS_SWAP, INS_DROP, INS_PICK,
    INS_RPUSH, INS_RPOP, INS_CALL, INS_RET, INS_JMP, INS_JZ,
    INS_SET, INS_GET, INS_SETC, INS_GETC,
};

#define DICT_START 4
#define BASE_INDEX 0
#define STRING_START 14*1024*1024

unsigned char initDict[] = {
    10,0,0,0,
    '+',0,0,INS_ADD,INS_RET,5,0,
    '-',0,0,INS_SUB,INS_RET,5,0,
    '*',0,0,INS_MUL,INS_RET,5,0,
    '/','M','O','D',0,0,INS_DIV,INS_RET,8,0,
    '>','R',0,0,INS_RPOP,INS_SWAP,INS_RPUSH,INS_RPUSH,INS_RET,9,0,
    'R','>',0,0,INS_RPOP,INS_RPOP,INS_SWAP,INS_RPUSH,INS_RET,9,0,
    'S','W','A','P',0,0,INS_SWAP,INS_RET,8,0,
    'D','R','O','P',0,0,INS_DROP,INS_RET,8,0,
    'D','U','P',0,0,INS_DUP,INS_RET,7,0,
    'O','V','E','R',0,0,INS_OVER,INS_RET,8,0,
    'P','I','C','K',0,0,INS_PICK,INS_RET,8,0,
    'A','N','D',0,0,INS_AND,INS_RET,7,0,
    'X','O','R',0,0,INS_XOR,INS_RET,7,0,
    'O','R',0,0,INS_OR,INS_RET,6,0,
    'I','N','V','E','R','T',0,0,INS_INV,INS_RET,10,0,
    '0','=',0,0,INS_ZEQ,INS_RET,6,0,
    '1','+',0,0,INS_INC,INS_RET,6,0,
    '1','-',0,0,INS_DEC,INS_RET,6,0,
    '2','*',0,0,INS_SHL,INS_RET,6,0,
    '2','/',0,0,INS_SHR,INS_RET,6,0,
    '!',0,0,INS_SET,INS_RET,5,0,
    '@',0,0,INS_GET,INS_RET,5,0,
    'C','!',0,0,INS_SET,INS_RET,6,0,
    'C','@',0,0,INS_GET,INS_RET,6,0,
    '=',0,0,INS_SUB,INS_ZEQ,INS_RET,6,0,
    '>','=',0,0,INS_SUB,INS_PUSH,0,0,0,8,INS_AND,INS_ZEQ,INS_RET,13,0,
    '>',0,0,INS_DEC,INS_SUB,INS_PUSH,0,0,0,8,INS_AND,INS_ZEQ,INS_RET,13,0,
    '<',0,0,INS_SUB,INS_PUSH,0,0,0,8,
            INS_AND,INS_ZEQ,INS_ZEQ,INS_RET,13,0,
    '<','=',0,0,INS_INC,INS_SUB,INS_PUSH,0,0,0,8,
            INS_AND,INS_ZEQ,INS_ZEQ,INS_RET,15,0,
    'N','I','P',0,0,INS_SWAP,INS_DROP,INS_RET,8,0,
    '2','D','U','P',0,0,INS_OVER,INS_OVER,INS_RET,9,0,
    '2','O','V','E','R',0,0,INS_PUSH,4,0,0,0,INS_DUP,INS_DEC,INS_PICK,
                        INS_SWAP,INS_PICK,INS_SWAP,INS_RET,19,0,
    'R','O','T',0,0,INS_RPUSH,INS_SWAP,INS_RPOP,INS_SWAP,INS_RET,10,0,
    255,
};
unsigned char dict[16*1024*1024];
uint32_t size = 0, lastWord = 0;

uint32_t rstack[256];
uint32_t stack[256];
unsigned char sp = 0, rsp = 0;

char (*getNextC)();
void (*functions[3000])();
uint32_t nfunctions = 0;

uint32_t stringAddr = STRING_START;

char defGetNextC() {
    return fgetc(stdin);
}

void addWord(const char *s) {
    strcpy(dict+size, s);
    size += strlen(s)+1;
    dict[size++] = 0;
}

void endWord() {
    dict[size++] = INS_RET;
    *(uint16_t*)&dict[size] = size - lastWord - 2;
    lastWord = size;
    size += 2;
}

void addFunction(const char *s, void (*f)()) {
    addWord(s);
    dict[size++] = INS_PUSH;
    *(uint32_t*)&dict[size] = nfunctions;
    size += 4;
    dict[size++] = INS_CALLC;
    endWord();
    functions[nfunctions++] = f;
}

int getNext(int mn, int mx) {
    char *s = dict+stringAddr;
    for(;;) {
        *(s++) = getNextC();
        if((*(s-1) <= mx && *(s-1) >=mn) || *(s-1) == 0) {
            if(s == dict+stringAddr+1) {
                if(*(s-1) == 0) return 0;
                s = dict+stringAddr;
                continue;
            }
            *(s-1) = 0;
            return 1;
        }
    }
}

int getName() {
    char *s;
    if(!getNext(0, 32)) return 0;
    for(s = dict+stringAddr; *s; s++)
        if(*s >= 'a' && *s <= 'z')
            *s -= 32;
    return 1;
}

uint32_t findWord(const char *s) {
    uint32_t i, j;
    i = lastWord;
    for(;;) {
        j = i-*(uint16_t*)&dict[i];
        if(!strcmp(dict+j, s)) return j+strlen(dict+j)+2;
        if(j == DICT_START) return 0;
        i = j-2;
    }
}

void runAddr(uint32_t pc) {
    for(;;) {
        switch(dict[pc++]) {
        case INS_PUSH:
            stack[sp++] = *(uint32_t*)&dict[pc];
            pc += 4;
            break;
        case INS_CALLC:
            functions[stack[--sp]]();
            break;
        case INS_ADD: stack[sp-2] += stack[sp-1]; sp--; break;
        case INS_SUB: stack[sp-2] -= stack[sp-1]; sp--; break;
        case INS_DIV:
            rstack[rsp] = (int32_t)stack[sp-2] % (int32_t)stack[sp-1];
            stack[sp-1] = (int32_t)stack[sp-2] / (int32_t)stack[sp-1];
            stack[sp-2] = rstack[rsp];
            break;
        case INS_MUL:
            stack[sp-2] = (int32_t)stack[sp-2] * (int32_t)stack[sp-1];
            sp--;
            break;
        case INS_AND: stack[sp-2] &= stack[sp-1]; sp--; break;
        case INS_OR: stack[sp-2] |= stack[sp-1]; sp--; break;
        case INS_XOR: stack[sp-2] ^= stack[sp-1]; sp--; break;
        case INS_INV: stack[sp-1] = ~stack[sp-1]; break;
        case INS_INC: stack[sp-1]++; break;
        case INS_DEC: stack[sp-1]--; break;
        case INS_SHL: stack[sp-1]<<=1; break;
        case INS_SHR: stack[sp-1]>>=1; break;
        case INS_ZEQ: stack[sp-1] = (!stack[sp-1])*-1; break;
        case INS_DUP: stack[sp] = stack[sp-1]; sp++; break;
        case INS_OVER: stack[sp] = stack[sp-2]; sp++; break;
        case INS_SWAP:
            rstack[rsp] = stack[sp-1];
            stack[sp-1] = stack[sp-2];
            stack[sp-2] = rstack[rsp];
            break;
        case INS_DROP: sp--; break;
        case INS_PICK: stack[sp-1] = stack[sp-stack[sp-1]]; break;
        case INS_RPUSH: rstack[rsp++] = stack[--sp]; break;
        case INS_RPOP: stack[sp++] = rstack[--rsp]; break;
        case INS_CALL: rstack[rsp++] = pc; pc = stack[--sp]; break;
        case INS_RET:
            if(!rsp) return;
            pc = rstack[--rsp];
            break;
        case INS_JMP: pc = stack[--sp]; break;
        case INS_JZ: if(!stack[sp-2]) pc = stack[sp-1]; sp -= 2; break;
        case INS_SET:
            *(uint32_t*)&dict[stack[sp-1]] = stack[sp-2];
            sp -= 2;
            break;
        case INS_GET:
            stack[sp-1] = *(uint32_t*)&dict[stack[sp-1]];
            break;
        case INS_SETC:
            dict[stack[sp-1]] = stack[sp-2];
            sp -= 2;
            break;
        case INS_GETC:
            stack[sp-1] = dict[stack[sp-1]];
            break;
        }
    }
}

int number(const char *s, int32_t *n) {
    char neg = 0;
    uint32_t base;

    if(*s == 0) return 0;
    *n = 0;
    if(*s == '-') { neg = 1; if(!*(++s)) return 0; }

    base = *(uint32_t*)&dict[BASE_INDEX];
    while(*s) {
        if(base <= 10) {
            if(*s >= '0' && *s < '0'+base)
                *n = *n * base + *s - '0';
            else
                return 0;
        } else {
            if(*s >= '0' && *s <= '9')
                *n = *n * base + *s - '0';
            else if(*s >= 'A' && *s < 'A'+base-10)
                *n = *n * base + *s - 'A'+10;
            else
                return 0;
        }
        s++;
    }

    if(neg) *n *= -1;

    return 1;
}

void wEmit() {
    printf("%c", stack[--sp]);
}

void wPrint() {
    printf("%d ", stack[--sp]);
}

void wColon() {
    uint32_t addr;
    uint32_t old = size;
    int32_t n;

    if(!getName()) { printf("expect identifier after :\n"); return; }
    addWord(dict+stringAddr);

    while(getName()) {
        if(!strcmp(dict+stringAddr, ";")) break;

        addr = findWord(dict+stringAddr);

        if(addr) {
            if(dict[addr-1]) runAddr(addr);
            else {
                dict[size++] = INS_PUSH;
                *(uint32_t*)&dict[size] = addr;
                size += 4;
                dict[size++] = INS_CALL;
            }

        } else if(number(dict+stringAddr, &n)) {
            dict[size++] = INS_PUSH;
            *(int32_t*)&dict[size] = n;
            size += 4;

        } else {
            printf("%s ?\ncancelled :\n", dict+stringAddr);
            size = old;
            return;
        }
    }

    endWord();
}

void wBye() {
    exit(0);
}

void wImmediate() {
    uint32_t i = lastWord-*(uint16_t*)&dict[lastWord];
    dict[i+strlen(dict+i)+1] = 1;
}

void wHere() {
    stack[sp++] = size;
}

void wWords() {
    uint32_t i, j;
    i = lastWord;
    int x = 0;
    for(;;) {
        j = i-*(uint16_t*)&dict[i];
        x += strlen(dict+j)+1;
        if(x >= 80) { printf("\n"); x = 0; }
        printf("%s ", dict+j);
        if(j == DICT_START) break;
        i = j-2;
    }
    if(x) printf("\n");
}

void wIf() {
    dict[size++] = INS_PUSH;
    stack[sp++] = size;
    size += 4;
    dict[size++] = INS_JZ;
}

void wThen() {
    *(uint32_t*)&dict[stack[--sp]] = size;
}

void wElse() {
    *(uint32_t*)&dict[stack[sp-1]] = size+6;
    dict[size++] = INS_PUSH;
    stack[sp-1] = size;
    size += 4;
    dict[size++] = INS_JMP;
}

void wParseName() {
    getNext(0, 32);
    stack[sp++] = stringAddr;
    stack[sp++] = strlen(dict+stringAddr);
}

void wQuote() {
    getNext('"', '"');
    printf("%s", dict+stringAddr);
}

void wSQuote() {
    getNext('"', '"');
    stack[sp++] = stringAddr;
    stack[sp++] = strlen(dict+stringAddr);
    stringAddr += strlen(dict+stringAddr);
}

void wType() {
    while(stack[sp-1]--) printf("%c", dict[stack[sp-2]++]);
    sp -= 2;
}

void init() {
    for(size = 0; initDict[size] != 255; size++) dict[size] = initDict[size];
    lastWord = size - 2;
    getNextC = defGetNextC;
    stringAddr = STRING_START;
    addFunction("EMIT", wEmit);
    addFunction(".", wPrint);
    addFunction(":", wColon);
    addFunction("BYE", wBye);
    addFunction("IMMEDIATE", wImmediate);
    addFunction("HERE", wHere);
    addFunction("WORDS", wWords);
    addFunction("IF", wIf); wImmediate();
    addFunction("ELSE", wElse); wImmediate();
    addFunction("THEN", wThen); wImmediate();
    addFunction("PARSE-NAME", wParseName);
    addFunction(".\"", wQuote); wImmediate();
    addFunction("S\"", wSQuote); wImmediate();
    addFunction("TYPE", wType);
}

void run() {
    int i;
    uint32_t addr;
    int32_t n;

    while(getName()) {
        addr = findWord(dict+stringAddr);
        if(addr) runAddr(addr);
        else if(number(dict+stringAddr, &n)) stack[sp++] = n;
        else printf("%s ?\n", dict+stringAddr);
    }
}

#endif
