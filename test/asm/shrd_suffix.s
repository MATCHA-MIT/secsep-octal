# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0xac10101010101010)
	movq $0xacacacacacacacac, %rbx
	movq $0x1010101010101010, %rcx
	shrdq, $8, %rbx %rcx

	# Long (expected 0xacac1010)
	movl $0xacacacac, %r8d
	movl $0x10101010, %r9d
	shrdl $16, %r8d, %r9d

	# Word (expected 0xcac1)
	movw $0xacac, %r10w
	movw $0x1010, %r11w
	shrdw $12, %r10w, %r11w

