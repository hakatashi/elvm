#include <stdio.h>
#include <stdlib.h>

#include <ir/ir.h>
#include <target/util.h>

static int TRSQREG[] = {
  4,  // A
  5,  // B
  6,  // C
  7,  // D
  8,  // BP
  9,  // SP
  0,  // R0
  1,  // R1
  2,  // R2
  3,  // R3
  10, // TRSQ_MEM
  11, // RODATA
  12, // FFFFFF
  13, // TRSQ_SP
  14,
  15, // TRSQ_PC
};

static int TRSQREG_ADDR[] = {
  0x10, // A
  0x11, // B
  0x12, // C
  0x13, // D
  0x14, // BP
  0x15, // SP
  0x16, // R0
};

#define R0 ((Reg)6)
#define R1 ((Reg)7)
#define R2 ((Reg)8)
#define R3 ((Reg)9)
#define R4 A
#define R5 B
#define R6 C
#define R7 D
#define TRSQ_MEM 0x20
#define RODATA ((Reg)11)
#define FFFFFF ((Reg)12)
#define TRSQ_SP ((Reg)13)
#define TRSQ_PC ((Reg)15)

static uint8_t trsq_imm(uint v) {
  return v & 0xFF;
}

static uint8_t trsq_file(uint v) {
  return v & 0xFF;
}

static void emit_trsq_2le(int a, int b) {
  emit_1(a);
  emit_1(b);
}

static void emit_trsq_4le(int a, int b, int c, int d) {
  emit_1(d);
  emit_1(c);
  emit_1(b);
  emit_1(a);
}

static void emit_trsq_svc() {
  emit_trsq_4le(0xef, 0x00, 0x00, 0x00);
}

typedef enum {
  TRSQ_AND = 0x00,
  TRSQ_SUB = 0x40,
  TRSQ_ADD = 0x80,
} TrsqOp;

static void emit_trsq_reg2op(TrsqOp op, Reg dst, Reg src) {
  emit_trsq_4le(0xe0, op + TRSQREG[dst], TRSQREG[dst] * 16, TRSQREG[src]);
}

typedef enum {
  Trsq_Shl0 = 0,
  Trsq_Shl24 = 4,
  Trsq_Shl16 = 8,
  Trsq_Shl8 = 12
} TrsqImmRot;

static void emit_trsq_add(int addr) {
  emit_line("    ADD %d", addr);
}

static void emit_trsq_sub(int addr) {
  emit_line("    SUB %d", addr);
}

static void emit_trsq_and(int addr) {
  emit_line("    AND %d", addr);
}

static void emit_trsq_or(int addr) {
  emit_line("    OR %d", trsq_file(addr));
}

static void emit_trsq_not() {
  emit_line("    NOT");
}

static void emit_trsq_xor(int addr) {
  emit_line("    XOR %d", trsq_file(addr));
}

static void emit_trsq_btc(int bit, int addr) {
  emit_line("    BTC %d %d", bit, trsq_file(addr));
}

static void emit_trsq_bts(int bit, int addr) {
  emit_line("    BTS %d %d", bit, trsq_file(addr));
}

static void emit_trsq_st(int addr) {
  emit_line("    ST %d", trsq_file(addr));
}

static void emit_trsq_ld(int addr) {
  emit_line("    LD %d", trsq_file(addr));
}

static void emit_trsq_ldl(int imm8) {
  emit_line("    LDL %d", trsq_imm(imm8));
}

static void emit_trsq_ldr() {
  emit_line("    LDR");
}

static void emit_trsq_str() {
  emit_line("    STR");
}

static void emit_trsq_skc() {
  emit_line("    SKC");
}

static void emit_trsq_skz() {
  emit_line("    SKZ");
}

static void emit_trsq_nop() {
  emit_line("    NOP");
}

static void emit_trsq_halt() {
  emit_line("    HALT");
}

static void emit_trsq_goto_imm(int addr) {
  emit_line("    GOTO %d", trsq_file(addr));
}

static void emit_trsq_goto_label(int pc) {
  emit_line("    GOTO pc_%d", pc);
}

static void emit_trsq_return() {
  emit_line("    RETURN");
}

static void emit_trsq_label(int pc) {
  emit_line("pc_%d:", pc);
}

static void emit_trsq_mov_reg(Reg dst, Reg src) {
  emit_trsq_4le(0xe1, 0xa0, TRSQREG[dst] * 16, TRSQREG[src]);
}

static void emit_trsq_mov_imm8(Reg dst, int imm8, TrsqImmRot rot) {
  emit_trsq_4le(0xe3, 0xa0, TRSQREG[dst] * 16 + rot, imm8);
}

static void emit_trsq_mvn_imm8(Reg dst, int imm8, TrsqImmRot rot) {
  emit_trsq_4le(0xe3, 0xe0, TRSQREG[dst] * 16 + rot, imm8);
}

static void emit_trsq_add_imm8(Reg dst, int imm8, TrsqImmRot rot) {
  emit_trsq_4le(0xe2, TRSQ_ADD + TRSQREG[dst], TRSQREG[dst] * 16 + rot, imm8);
}

static void emit_trsq_sub_imm8(Reg dst, int imm8, TrsqImmRot rot) {
  emit_trsq_4le(0xe2, TRSQ_SUB + TRSQREG[dst], TRSQREG[dst] * 16 + rot, imm8);
}

static void emit_trsq_mov_imm(Reg dst, int imm) {
  emit_trsq_ldl(imm % 256);
  emit_trsq_st(dst);
}

static void emit_trsq_add_imm(Reg dst, int imm) {
  if (imm > 0xffff00) {
    emit_trsq_sub_imm8(dst, 0x1000000 - imm, Trsq_Shl0);
    return;
  }

  emit_trsq_add_imm8(dst, imm % 256, Trsq_Shl0);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_add_imm8(dst, imm % 256, Trsq_Shl8);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_add_imm8(dst, imm, Trsq_Shl16);
}

static void emit_trsq_sub_imm(Reg dst, int imm) {
  emit_trsq_sub_imm8(dst, imm % 256, Trsq_Shl0);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_sub_imm8(dst, imm % 256, Trsq_Shl8);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_sub_imm8(dst, imm, Trsq_Shl16);
}

typedef enum {
  TRSQ_MEM_STORE = 0x80,
  TRSQ_MEM_LOAD = 0x90,
} TrsqLoadOrStore;

static void emit_trsq_mem(TrsqLoadOrStore op, Reg dst, Reg base, Reg src) {
  emit_line("emit_trsq_mem val=%d, base=%d, offset=%d", dst, base, src);
  if (op == TRSQ_MEM_LOAD){
    emit_trsq_ld(TRSQREG_ADDR[src]);
    emit_trsq_st(0x03);
    emit_trsq_ldr();
    emit_trsq_st(TRSQREG_ADDR[dst]);
  } else {
    emit_trsq_ld(TRSQREG_ADDR[dst]);
    emit_trsq_st(0x03);
    emit_trsq_ld(TRSQREG_ADDR[src]);
    emit_trsq_str();
  }
}

static void emit_trsq_cmp(Inst* inst) {
  Reg reg;
  if (inst->src.type == REG) {
    reg = inst->src.reg;
  } else {
    reg = R0;
    emit_trsq_mov_imm(reg, inst->src.imm);
  }
  emit_trsq_4le(0xe1, 0x50 + TRSQREG[inst->dst.reg], 0x00, TRSQREG[reg]);
}

static void emit_trsq_setcc(Inst* inst, int op) {
  emit_trsq_cmp(inst);
  emit_trsq_mov_imm8(inst->dst.reg, 0, Trsq_Shl0);
  emit_trsq_4le(op, 0xa0, TRSQREG[inst->dst.reg] * 16, 0x01);
}

static void emit_trsq_jcc(Inst* inst, int op, int* pc2addr) {
  if (inst->op != JMP) {
    emit_trsq_cmp(inst);
  }

  if (inst->jmp.type == REG) {
    emit_trsq_mem(TRSQ_MEM_LOAD, TRSQ_PC, RODATA, inst->jmp.reg);
  } else {
    uint32_t v = pc2addr[inst->jmp.imm] / 4 - (emit_cnt() + 8) / 4;
    emit_1(v % 256);
    v /= 256;
    emit_1(v % 256);
    v /= 256;
    emit_1(v % 256);
    emit_1(op);
  }
}

static void init_state_trsq(Data* data, int rodata_addr) {
  emit_trsq_ldl(0);
  emit_trsq_st(TRSQREG_ADDR[A]);
  emit_trsq_st(TRSQREG_ADDR[B]);
  emit_trsq_st(TRSQREG_ADDR[C]);
  emit_trsq_st(TRSQREG_ADDR[D]);
  emit_trsq_st(TRSQREG_ADDR[BP]);
  emit_trsq_st(TRSQREG_ADDR[SP]);

  for (int mp = 0; data; data = data->next, mp++) {
    if (!data->v)
      continue;
    emit_trsq_ldl(trsq_imm(data->v));
    emit_trsq_st(TRSQ_MEM + mp);
  }
}

static void trsq_emit_trsq_inst(Inst* inst, int* pc2addr) {
  Reg reg;

  switch (inst->op) {
  case MOV:
    if (inst->src.type == REG) {
      emit_trsq_ld(TRSQREG_ADDR[inst->src.reg]);
    } else {
      emit_trsq_ldl(inst->src.imm);
    }
    emit_trsq_st(TRSQREG_ADDR[inst->dst.reg]);
    break;

  case ADD:
    emit_trsq_ld(TRSQREG_ADDR[inst->dst.reg]);
    emit_trsq_st(TRSQREG_ADDR[R0]);

    if (inst->src.type == REG) {
      emit_trsq_ld(TRSQREG_ADDR[inst->src.reg]);
    } else {
      emit_trsq_ldl(inst->src.imm);
    }

    emit_trsq_add(TRSQREG_ADDR[R0]);
    emit_trsq_st(TRSQREG_ADDR[inst->dst.reg]);
    break;

  case SUB:
    emit_trsq_ld(TRSQREG_ADDR[inst->dst.reg]);
    emit_trsq_st(TRSQREG_ADDR[R0]);

    if (inst->src.type == REG) {
      emit_trsq_ld(TRSQREG_ADDR[inst->src.reg]);
    } else {
      emit_trsq_ldl(inst->src.imm);
    }

    emit_trsq_sub(TRSQREG_ADDR[R0]);
    emit_trsq_st(TRSQREG_ADDR[inst->dst.reg]);
    break;

  case LOAD:
  case STORE:
    if (inst->src.type == REG) {
      reg = inst->src.reg;
    } else {
      emit_trsq_mov_imm(R0, inst->src.imm);
      reg = R0;
    }

    emit_trsq_mem(inst->op == LOAD ? TRSQ_MEM_LOAD : TRSQ_MEM_STORE,
		 inst->dst.reg, (Reg)TRSQ_MEM, reg);
    break;

  case PUTC:
    // if (inst->src.type == REG) {
    //   reg = inst->src.reg;
    // } else {
    //   reg = R0;
    //   emit_trsq_mov_imm8(reg, inst->src.imm, Trsq_Shl0);
    // }
    // emit_trsq_4le(0xe5, 0x2d, TRSQREG[reg] * 16, 0x04);  // push reg
    // emit_trsq_mov_imm8(R0, 1, Trsq_Shl0);  // stdout
    // emit_trsq_mov_reg(R1, TRSQ_SP);
    // emit_trsq_mov_imm8(R2, 1, Trsq_Shl0);
    // emit_trsq_mov_imm8(R7, 4, Trsq_Shl0);  // write
    // emit_trsq_svc();
    // emit_trsq_4le(0xe4, 0x9d, TRSQREG[R0] * 16, 0x04);  // pop R0
    break;

  case GETC:
    // emit_trsq_mov_imm8(R0, 0, Trsq_Shl0);  // stdin
    // emit_trsq_4le(0xe5, 0x2d, TRSQREG[R0] * 16, 0x04);  // push 0
    // emit_trsq_mov_reg(R1, TRSQ_SP);
    // emit_trsq_mov_imm8(R2, 1, Trsq_Shl0);
    // emit_trsq_mov_imm8(R7, 3, Trsq_Shl0);  // read
    // emit_trsq_svc();
    // emit_trsq_4le(0xe4, 0x9d, TRSQREG[inst->dst.reg] * 16, 0x04);  // pop dst
    break;

  case EXIT:
    emit_trsq_halt();
    break;

  case DUMP:
    break;

  case EQ:
    // emit_trsq_setcc(inst, 0x03);
    break;

  case NE:
    // emit_trsq_setcc(inst, 0x13);
    break;

  case LT:
    // emit_trsq_setcc(inst, 0xb3);
    break;

  case GT:
    // emit_trsq_setcc(inst, 0xc3);
    break;

  case LE:
    // emit_trsq_setcc(inst, 0xd3);
    break;

  case GE:
    // emit_trsq_setcc(inst, 0xa3);
    break;

  case JEQ:
    // emit_trsq_jcc(inst, 0x0a, pc2addr);
    break;

  case JNE:
    // emit_trsq_jcc(inst, 0x1a, pc2addr);
    break;

  case JLT:
    // emit_trsq_jcc(inst, 0xba, pc2addr);
    break;

  case JGT:
    // emit_trsq_jcc(inst, 0xca, pc2addr);
    break;

  case JLE:
    // emit_trsq_jcc(inst, 0xda, pc2addr);
    break;

  case JGE:
    // emit_trsq_jcc(inst, 0xaa, pc2addr);
    break;

  case JMP:
    // emit_trsq_jcc(inst, 0xea, pc2addr);
    break;

  default:
    error("oops");
  }
}

void target_trsq(Module* module) {
  emit_reset();
  emit_start();
  init_state_trsq(module->data, 0);

  int pc_cnt = 0;
  for (Inst* inst = module->text; inst; inst = inst->next) {
    pc_cnt++;
  }

  int* pc2addr = calloc(pc_cnt, sizeof(int));
  int prev_pc = -1;
  for (Inst* inst = module->text; inst; inst = inst->next) {
    if (prev_pc != inst->pc) {
      pc2addr[inst->pc] = emit_cnt();
      emit_trsq_label(inst->pc);
    }
    prev_pc = inst->pc;
    trsq_emit_trsq_inst(inst, pc2addr);
  }

  int rodata_addr = ELF_TEXT_START + emit_cnt() + ELF_HEADER_SIZE;
}
