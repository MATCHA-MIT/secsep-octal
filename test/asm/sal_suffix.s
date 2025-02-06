# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected 0xfedcba9876543210)
	movq $0x0fedcba987654321, %r8
	salq $4, %r8

	# Long (expected $0x0aabb000)
	movq $0x000aabb0, %r10
	sall $8, %r10

	# Word (expected 0b0010110100110100)
	movq $0b1100101101001101, %r12
	salw $2, %r12

	# Byte (expected 0x11111000)
	movq $0b00011111, %r14
	salb $3, %r14
