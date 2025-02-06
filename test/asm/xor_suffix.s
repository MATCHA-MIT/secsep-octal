# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0xffccaa8800000000)
	movq $0xa5848280ffffffff, %r8
	movq $0x5a482808ffffffff, %r9
	xorq %r8, %r9

	# Long (expected $0xeebb0000)
	movq $0xaaaa0000, %r10
	movq $0x44110000, %r11
	xorl %r10d, %r11d

	# Word (expected 0b1111111100000000)
	movq $0b1100101011110000, %r12
	movq $0b0011010111110000, %r13
	xorw %r12w, %r13w

	# Byte (expected 0b11110000)
	movq $0b10101100, %r14
	movq $0b01011100, %r15
	xorb %r14b, %r15b
