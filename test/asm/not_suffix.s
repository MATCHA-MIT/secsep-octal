# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x5a48280800000000)
	movq $0xa5848280ffffffff, %r8
	notq %r8

	# Long (expected $0x44441111)
	movq $0xaaaa0000, %r10
	notl %r10d

	# Word (expected 0b0011010100001111)
	movq $0b1100101011110000, %r12
	notw %r12w

	# Byte (expected 0b01010011)
	movb $0b10101100, %r14b
	notb %r14b
