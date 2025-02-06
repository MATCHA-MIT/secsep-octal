# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0xffccaa88ffffffff)
	movq $0xa5848280ffffffff, %r8
	movq $0x5a482808ffffffff, %r9
	orq %r8, %r9

	# Long (expected $0xeebb0000)
	movq $0xaaaa0000, %r10
	movq $0x44110000, %r11
	orl %r10d, %r11d

	# Word (expected 0b1111111111110000)
	movq $0b1100101011110000, %r12
	movq $0b0011010111110000, %r13
	orw %r12w, %r13w

	# Byte (expected 0b11111100)
	movq $0b10101100, %r14
	movq $0b01011100, %r15
	orb %r14b, %r15b
