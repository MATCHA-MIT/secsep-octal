# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x00000000ffffffff)
	movq $0xa5848280ffffffff, %r8
	movq $0x5a482808ffffffff, %r9
	andq %r8, %r9

	# Long (expected $0x00000000)
	movq $0xaaaa0000, %r10
	movq $0x44110000, %r11
	andl %r10d, %r11d

	# Word (expected 0b0000000011110000)
	movq $0b1100101011110000, %r12
	movq $0b0011010111110000, %r13
	andw %r12w, %r13w

	# Byte (expected 0b00001100)
	movq $0b10101100, %r14
	movq $0b01011100, %r15
	andb %r14b, %r15b
