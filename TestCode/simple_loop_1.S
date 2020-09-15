_entry:
li a0, 42
li a1, 10000
li a2, 0

_loop:
beq a0, a1, _end
add a2, a2, a0
addi a0, a0, 1
j _loop

_end:
j _end
