ENTRY(_start)

SECTIONS 
{
  . = 0x0;
  .text : 
  {
    *(.text)
  }

  . = ALIGN(16);

  .data :
  {
    *(.rodata)
    *(.data)
    *(.sdata)
    *(.bss)
    *(.sbss)
  }
}


