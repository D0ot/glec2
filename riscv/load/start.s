.section .data
.align 2
.space 1024
stack_addr:

.section .text
.global _start
_start:
  la sp, stack_addr
  call main

_inf_loop:
  j _inf_loop

  

