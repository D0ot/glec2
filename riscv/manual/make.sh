#!/bin/bash

riscv32-elf-as program.s
riscv32-elf-objcopy -O binary a.out program.bin
riscv32-elf-objdump -D -m riscv -M numeric a.out -j .text > ../dis.txt
rm ./a.out
cp program.bin ..

