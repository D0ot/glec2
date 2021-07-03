# GLEC2 Project

**A toy implementation of classic 5-stage rv32i(incomplete) based on powerful SpinalHDL**

---
## Implemented:

1. classic 5 stage pipeline.
2. data hazard resolve, stall or bypass supported.
3. control hazard resolve, `jal` and `jalr` executes in ID/EXE which is very earyly needing to insert a bubble. `beq, bne, etc..` executes in EXE/MEM, which need insert two bubbles.

---

## Not implemented:

1. csr instructions.
2. `ecall` and `ebreak` instructions.