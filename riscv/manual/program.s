start:
    auipc x2, 1
    addi x2, x2, 1024

    bne x0, x0, data_hazard
    
    addi x2, x2, 1
    addi x3, x3, 1
    jal myfunc

myfunc:
    li x6, 0xff
    li x7, 0xdd
    jalr x0, x1, 0

data_hazard:
    addi x1, x0, 1
    addi x2, x1, 1
    addi x3, x2, 1
    addi x4, x3, 1

    add x1, x2, x3
    add x4, x1, x2
    add x3, x4, x1
    add x2, x3, x4
    add x1, x2, x3


data_hazard_mem:
    li x1, 0
    addi x2, x1, 4
    addi x3, x2, 4
    addi x4, x3, 4

    li x5, 0xff

    sw x5, 0(x1)
    addi x5, x5, -1

    sw x5, 0(x2)
    addi x5, x5, -1

    sw x5, 0(x3)
    addi x5, x5, -1

    sw x5, 0(x4)
    addi x5, x5, -1

    sw x5, 4(x4)
    addi x5, x5, -1

    sw x5, 8(x4)
    addi x5, x5, -1

    sw x5, 12(x4)
    addi x5, x5, -1

    sw x5, 16(x4)
    addi x5, x5, -1

    lw x6, 0(x1)
    lw x6, 0(x2)
    lw x6, 0(x3)
    lw x6, 0(x4)

    lw x6, 4(x4)
    lw x6, 8(x4)
    lw x6, 12(x4)
    lw x6, 16(x4)

do_lu:
    lui x0, 0xff
    lui x1, 1
    lui x2, 2
    li x2, 5
    lui x3, 3
    sw x1, 0(x0)
    sb x2, 0(x0)
    lw x5, 0(x0)
    
do_auipc:
    auipc x0, 0
    auipc x1, 1
    auipc x2, 2
    auipc x3, 3

do_addi:
    addi x0, x0, 0
    addi x1, x1, 10
    addi x2, x2, 20
    addi x3, x3, 30

do_add:
    add x0, x0, 0
    add x1, x1, -1
    add x2, x2, -2
    add x3, x3, -3

do_ori:
    ori x0, x0, 0xff
    ori x1, x1, 1
    ori x2, x2, 2
    ori x3, x3, 3

do_xori:
    xori x0, x0, 0xff
    xori x1, x1, 1
    xori x2, x2, 2
    xori x3, x3, 3

    jal x4, do_lui
    
    andi x0, x0, 0xff
    andi x1, x1, 1
    andi x2, x2, 2
    andi x3, x3, 3

    nop
    nop
    nop
    nop



    lui x1, 1           
    auipc x2, 1

    jal x3, jal_label
    beq x0, x0, jump_jal
    

jal_label:
    jalr x0, x3, 0
    beq x0, x0, failed

jump_jal:
    beq x0, x1, failed
    bne x0, x1, bne_succ
    li x1, 12

bne_succ:
    slt x12, x1, x2 
    sltu x11, x1, x2
    add x12, x12, x11
    li x11, 2
    beq x11, x12, slt_sltu_succ
    beq x0, x0, failed

slt_sltu_succ:
    li x4, 1
    li x5, 2
    blt x4, x5, blt_succ
    beqz x0, failed

blt_succ:
    bge x5, x4, bge_succ
    beqz x0, failed

bge_succ:
    li x6, 0x10
    li x7, 0xff
    sw x7, 0(x6)
    lw x8, 0(x6)
    beq x7, x8, sl_succ
    beqz x0, failed

sl_succ:
    lui x1, 0x12345
    lui x2, 0x54321
    add x3, x1, x2
    sub x4, x2, x1
    and x5, x1, x2
    or x6, x1, x2
    xor x7, x1, x2
    li x2, 5
    sll x8, x1, x2
    lui x1, 0x80000
    srli x9, x2, 2
    
loop:
    beq x0, x0, loop

failed:
    lui x13, 0xdeadb
    addi x13, x0, 0x01ef
    beq x0, x0, failed

