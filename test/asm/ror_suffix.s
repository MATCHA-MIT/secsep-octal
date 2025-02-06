# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0x10fedcba98765432)
	movq $0x0fedcba987654321, %r8
	rorq $4, %r8

	# Long (expected $0x0aab)
	movq $0xaabb0, %r10
	rorl $8, %r10d

	# Word (expected 0b0011001011010011)
	movq $0b1100101101001101, %r12
	rorw $2, %r12w

	# Byte (expected 0x00000011)
	movq $0b00011111, %r14
	rorb $3, %r14b
