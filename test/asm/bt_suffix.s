# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected CF = 1)
	movq $0x4000000000000000, %r8
	btq $62, %r8

	# Long (expected CF = 0)
	movl $0xff7fffff, %r10d
	btl $23, %r10d

	# Word (expected CF = 1)
	movw $0b0000010000000000, %r12w
	btw $10, %r12w
