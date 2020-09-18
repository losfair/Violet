_start:
li s0, 0x100

li t0, 12
sw t0, 0(s0)
li t0, 11
sw t0, 4(s0)
li t0, 3231
sw t0, 8(s0)
li t0, 128
sw t0, 12(s0)

mv a0, s0
li a1, 4

li sp, 0x4000
call bubbleSort

_start.loop:
lw t0, 0(s0)
lw t0, 4(s0)
lw t0, 8(s0)
lw t0, 12(s0)
j _start.loop

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