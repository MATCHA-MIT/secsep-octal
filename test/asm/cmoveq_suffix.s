# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x9988776655443322)
	cmoveqq $0x9988776655443322, %r8

	# Long (expected $0xaabbccdd)
	cmoveql $0xaabbccdd, %r10

	# Word (expected 0x7192)
	cmoveqw $0x7192, %r12

	# Byte (expected 0x33)
	cmoveqb $0x33, %r14
