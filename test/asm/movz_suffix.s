# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Long to Q (expected $0xaabbccdd)
	movzlq $0xaabbccdd, %rax

	# Word to QL (expected 0x7192)
	movzwq $0x7192, %rbx
	movzwl $0x7192, %ecx

	# Byte to QLW (expected 127)
	movzbw $127, %r8w
	movzbl $127, %r9d
	movzbq $127, %r10
