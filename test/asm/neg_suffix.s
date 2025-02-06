# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x9988776655443322)
	movq $0x66778899aabbccdd, %r8
	negq %r8

	# Long (expected $0xaabbccdd)
	movl $0xaabbccdd, %r10
	negl %r10

	# Word (expected 0x7192)
	movw $0x7192, %r12
	negw %r12

	# Byte (expected 0x33)
	movb $0x33, %r14
	negb %r14
