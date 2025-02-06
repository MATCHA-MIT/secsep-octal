# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0x00fedcba98765432)
	movq $0x0fedcba987654321, %r8
	sarq $4, %r8

	# Long (expected $0x0aab)
	movq $0x000aabb0, %r10
	sarl $8, %r10d

	# Word (expected 0b1111001011010011)
	movq $0b1100101101001101, %r12
	sarw $2, %r12w

	# Byte (expected 0x11110011)
	movq $0b10011111, %r14
	sarb $3, %r14b
