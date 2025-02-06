# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0x123456789abcdef)
	movq $0x123456789abcdf0, %r8
	decq %r8

	# Long (expected $0x89898989)
	movq $0x8989898a, %r10
	decl %r10

	# Word (expected 0b1100101101001110)
	movq $0b1100101101001111, %r12
	decw %r12

	# Byte (expected 0x00100000)
	movq $0b00100001, %r14
	decb %r14
