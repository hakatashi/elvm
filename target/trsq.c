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

#define R0 ((Reg)6)
#define R1 ((Reg)7)
#define R2 ((Reg)8)
#define R3 ((Reg)9)
#define R4 A
#define R5 B
#define R6 C
#define R7 D
#define TRSQ_MEM ((Reg)10)
#define RODATA ((Reg)11)
#define FFFFFF ((Reg)12)
#define TRSQ_SP ((Reg)13)
#define TRSQ_PC ((Reg)15)

void emit_elf_header(uint16_t machine, uint32_t filesz);

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
  emit_trsq_mov_imm8(dst, imm % 256, Trsq_Shl0);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_add_imm8(dst, imm % 256, Trsq_Shl8);
  imm /= 256;
  if (!imm)
    return;
  emit_trsq_add_imm8(dst, imm % 256, Trsq_Shl16);
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

static void emit_trsq_mem(TrsqLoadOrStore op, Reg val, Reg base, Reg offset) {
  emit_trsq_4le(0xe7, op + TRSQREG[base], TRSQREG[val] * 16 + 1, TRSQREG[offset]);
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
  emit_trsq_mov_imm8(R0, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(R1, 4, Trsq_Shl24);
  emit_trsq_mov_imm8(R2, 3, Trsq_Shl0);  // PROT_READ | PROT_WRITE
  emit_trsq_mov_imm8(R3, 0x22, Trsq_Shl0);  // MAP_PRIVATE | MAP_ANONYMOUS
  emit_trsq_mvn_imm8(R4, 0, Trsq_Shl0);  // 0xffffffff
  emit_trsq_mov_imm8(R5, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(R7, 192, Trsq_Shl0);  // mmap2
  emit_trsq_svc();

  emit_trsq_mov_reg(TRSQ_MEM, R0);

  int prev = 0;
  for (int mp = 0; data; data = data->next, mp++) {
    if (!data->v)
      continue;
    int d = (mp - prev) * 4;
    if (d >= 4096) {
      emit_trsq_add_imm(R0, d);
      d = 0;
    }
    emit_trsq_mov_imm(R1, data->v);
    emit_trsq_4le(0xe5, 0xa0, 0x10 + d / 256, d % 256);
    prev = mp;
  }

  emit_trsq_mov_imm8(RODATA, rodata_addr % 256, Trsq_Shl0);
  rodata_addr /= 256;
  emit_trsq_add_imm8(RODATA, rodata_addr % 256, Trsq_Shl8);
  rodata_addr /= 256;
  emit_trsq_add_imm8(RODATA, rodata_addr % 256, Trsq_Shl16);
  emit_trsq_mvn_imm8(FFFFFF, 0xff, Trsq_Shl24);

  emit_trsq_mov_imm8(A, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(B, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(C, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(D, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(BP, 0, Trsq_Shl0);
  emit_trsq_mov_imm8(SP, 0, Trsq_Shl0);
}

static void trsq_emit_trsq_inst(Inst* inst, int* pc2addr) {
  Reg reg;

  switch (inst->op) {
  case MOV:
    if (inst->src.type == REG) {
      emit_trsq_mov_reg(inst->dst.reg, inst->src.reg);
    } else {
      emit_trsq_mov_imm(inst->dst.reg, inst->src.imm);
    }
    break;

  case ADD:
    if (inst->src.type == REG) {
      emit_trsq_reg2op(TRSQ_ADD, inst->dst.reg, inst->src.reg);
    } else {
      emit_trsq_add_imm(inst->dst.reg, inst->src.imm);
    }
    emit_trsq_reg2op(TRSQ_AND, inst->dst.reg, FFFFFF);
    break;

  case SUB:
    if (inst->src.type == REG) {
      emit_trsq_reg2op(TRSQ_SUB, inst->dst.reg, inst->src.reg);
    } else {
      emit_trsq_sub_imm(inst->dst.reg, inst->src.imm);
    }
    emit_trsq_reg2op(TRSQ_AND, inst->dst.reg, FFFFFF);
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
		 inst->dst.reg, TRSQ_MEM, reg);
    break;

  case PUTC:
    if (inst->src.type == REG) {
      reg = inst->src.reg;
    } else {
      reg = R0;
      emit_trsq_mov_imm8(reg, inst->src.imm, Trsq_Shl0);
    }
    emit_trsq_4le(0xe5, 0x2d, TRSQREG[reg] * 16, 0x04);  // push reg
    emit_trsq_mov_imm8(R0, 1, Trsq_Shl0);  // stdout
    emit_trsq_mov_reg(R1, TRSQ_SP);
    emit_trsq_mov_imm8(R2, 1, Trsq_Shl0);
    emit_trsq_mov_imm8(R7, 4, Trsq_Shl0);  // write
    emit_trsq_svc();
    emit_trsq_4le(0xe4, 0x9d, TRSQREG[R0] * 16, 0x04);  // pop R0
    break;

  case GETC:
    emit_trsq_mov_imm8(R0, 0, Trsq_Shl0);  // stdin
    emit_trsq_4le(0xe5, 0x2d, TRSQREG[R0] * 16, 0x04);  // push 0
    emit_trsq_mov_reg(R1, TRSQ_SP);
    emit_trsq_mov_imm8(R2, 1, Trsq_Shl0);
    emit_trsq_mov_imm8(R7, 3, Trsq_Shl0);  // read
    emit_trsq_svc();
    emit_trsq_4le(0xe4, 0x9d, TRSQREG[inst->dst.reg] * 16, 0x04);  // pop dst
    break;

  case EXIT:
    emit_trsq_mov_imm8(R0, 0, Trsq_Shl0);
    emit_trsq_mov_imm8(R7, 1, Trsq_Shl0);  // exit
    emit_trsq_svc();
    break;

  case DUMP:
    break;

  case EQ:
    emit_trsq_setcc(inst, 0x03);
    break;

  case NE:
    emit_trsq_setcc(inst, 0x13);
    break;

  case LT:
    emit_trsq_setcc(inst, 0xb3);
    break;

  case GT:
    emit_trsq_setcc(inst, 0xc3);
    break;

  case LE:
    emit_trsq_setcc(inst, 0xd3);
    break;

  case GE:
    emit_trsq_setcc(inst, 0xa3);
    break;

  case JEQ:
    emit_trsq_jcc(inst, 0x0a, pc2addr);
    break;

  case JNE:
    emit_trsq_jcc(inst, 0x1a, pc2addr);
    break;

  case JLT:
    emit_trsq_jcc(inst, 0xba, pc2addr);
    break;

  case JGT:
    emit_trsq_jcc(inst, 0xca, pc2addr);
    break;

  case JLE:
    emit_trsq_jcc(inst, 0xda, pc2addr);
    break;

  case JGE:
    emit_trsq_jcc(inst, 0xaa, pc2addr);
    break;

  case JMP:
    emit_trsq_jcc(inst, 0xea, pc2addr);
    break;

  default:
    error("oops");
  }
}

void target_trsq(Module* module) {
  emit_reset();
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
    }
    prev_pc = inst->pc;
    trsq_emit_trsq_inst(inst, pc2addr);
  }

  int rodata_addr = ELF_TEXT_START + emit_cnt() + ELF_HEADER_SIZE;

  emit_elf_header(40, emit_cnt() + pc_cnt * 4);

  emit_reset();
  emit_start();
  init_state_trsq(module->data, rodata_addr);

  for (Inst* inst = module->text; inst; inst = inst->next) {
    trsq_emit_trsq_inst(inst, pc2addr);
  }

  for (int i = 0; i < pc_cnt; i++) {
    emit_le(ELF_TEXT_START + pc2addr[i] + ELF_HEADER_SIZE);
  }
}
