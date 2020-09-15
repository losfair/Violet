_entry:
li a0, 0x200
li a1, 10000
li a2, 0

li t0, 42
sw t0, 4(a0)

_loop:
lw t0, 4(a0)
beq t0, a1, _end
add a2, a2, a0
addi t0, t0, 1
sw t0, 4(a0)
j _loop

_end:
j _end
