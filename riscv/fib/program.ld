ENTRY(_start)

SECTIONS 
{
  . = 0x0;
  .text : 
  {
    start.o(.text)
    *(.text)
  }

  . = ALIGN(2);
  .data :
  {
    *(.rodata)
    *(.data)
    *(.sdata)
    *(.bss)
    *(.sbss)
  }
}


