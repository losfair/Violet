OUTPUT_ARCH(riscv)

ENTRY(_start)

SECTIONS
{
    . = 0x0;

    .text : {
        *(.text.entry)
        *(.text .text.*)
        . = ALIGN(4);
    }

    .rodata : {
        *(.rodata .rodata.*)
    }

    .data : {
        *(.data .data.*)
        *(.sdata .sdata.*)
        *(.sbss .bss .bss.*)
        . = ALIGN(4);
    }

    /DISCARD/ : { *(.eh_frame) }
}
