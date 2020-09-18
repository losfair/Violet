_start:

li a0, 2
li a1, 3
li a2, 3
call do_mul

_end:
mv a0, a0
j _end

do_mul:
	addi	sp,sp,-48
	sw	s0,44(sp)
	addi	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	sw	zero,-24(s0)
	j	.L2
.L3:
	lw	a4,-36(s0)
	lw	a5,-40(s0)
	mul	a5,a4,a5
	addi	a5,a5,1
	lw	a4,-20(s0)
	add	a5,a4,a5
	sw	a5,-20(s0)
	lw	a5,-24(s0)
	addi	a5,a5,1
	sw	a5,-24(s0)
.L2:
	lw	a4,-24(s0)
	lw	a5,-44(s0)
	blt	a4,a5,.L3
	lw	a5,-20(s0)
	mv	a0,a5
	lw	s0,44(sp)
	addi	sp,sp,48
	jr	ra