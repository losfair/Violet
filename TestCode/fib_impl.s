	.file	"fib_impl.c"
	.option nopic
	.attribute arch, "rv32i2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	fib
	.type	fib, @function
fib:
	addi	sp,sp,-16
	sw	s0,8(sp)
	sw	ra,12(sp)
	addi	s0,a0,-1
	sw	s1,4(sp)
	sw	s2,0(sp)
	li	a5,1
	li	a0,1
	bleu	s0,a5,.L1
	li	s1,0
	li	s2,1
.L3:
	mv	a0,s0
	call	fib
	addi	s0,s0,-2
	add	s1,s1,a0
	bgtu	s0,s2,.L3
	addi	a0,s1,1
.L1:
	lw	ra,12(sp)
	lw	s0,8(sp)
	lw	s1,4(sp)
	lw	s2,0(sp)
	addi	sp,sp,16
	jr	ra
	.size	fib, .-fib
	.ident	"GCC: () 9.3.0"
