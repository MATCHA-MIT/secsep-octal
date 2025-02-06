# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x0, $0x2468acf02468acf0)
	movq $0x1234567812345678, %rax
	imulq $2
	movq %rdx, %r9
	movq %rax, %r8

	# Long (expected $0xa1, $0x928373ec)
	movl $0xf1e2d3c4, %eax
	imull $0xab
	movl %edx, %r11d
	movl %eax, %r10d

	# Word (expected 0x48, 0x8840)
	movw $0x8888, %ax
	imulw $0x88
	movw %dx, %r13w
	movw $ax, %r12w

	# Byte (expected 0xdc)
	movb $0x37, %al
	imulb $4
