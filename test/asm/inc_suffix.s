# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0x123456789abcdef)
	movq $0x123456789abcdee, %r8
	incq %r8

	# Long (expected $0x89898989)
	movq $0x89898988, %r10
	incl %r10d

	# Word (expected 0b1100101101001110)
	movq $0b1100101101001101, %r12
	incw %r12w

	# Byte (expected 0x00100000)
	movq $0b00011111, %r14
	incb %r14b
