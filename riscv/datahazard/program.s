start:
  addi x1, x0, 0
  addi x2, x1, 1
  addi x3, x2, 1
  addi x4, x3, 1
  addi x5, x4, 1
  addi x6, x5, 1


  la x1, 0x44332211
  sw x1, 0(x0)
  lw x2, 0(x0)
  addi x3, x2, 1

inf_loop:
  j inf_loop
  
