	.file	"sort_impl.c"
	.option nopic
	.attribute arch, "rv32i2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.globl	bubbleSort
	.type	bubbleSort, @function
bubbleSort:
	addi	a5,a1,-1
	blez	a5,.L1
	slli	a2,a1,2
	add	a2,a0,a2
	mv	a1,a5
	addi	a0,a0,4
.L3:
	mv	a5,a0
.L5:
	lw	a4,-4(a5)
	lw	a3,0(a5)
	ble	a4,a3,.L4
	sw	a3,-4(a5)
	sw	a4,0(a5)
.L4:
	addi	a5,a5,4
	bne	a2,a5,.L5
	addi	a1,a1,-1
	addi	a2,a2,-4
	bnez	a1,.L3
.L1:
	ret
	.size	bubbleSort, .-bubbleSort
	.ident	"GCC: () 9.3.0"
