# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0x00fedcba98765432)
	movq $0x0fedcba987654321, %r8
	shrq $4, %r8

	# Long (expected $0x00000aab)
	movq $0x000aabb0, %r10
	shrl $8, %r10

	# Word (expected 0b0011001011010011)
	movq $0b1100101101001101, %r12
	shrw $2, %r12

	# Byte (expected 0x00000011)
	movq $0b00011111, %r14
	shrb $3, %r14
