
program.bin : main.c program.ld start.s
	riscv32-unknown-elf-gcc -ffreestanding -O0 -Wall -c main.c
	riscv32-unknown-elf-gcc -ffreestanding -O0 -Wall -c start.s
	riscv32-unknown-elf-gcc -nostartfiles -static -T program.ld main.o start.o
	riscv32-unknown-elf-objcopy -O binary a.out ../program.bin
	riscv32-unknown-elf-objdump -D -m riscv -M numeric a.out -j .text > ../dis.txt



