# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Basic subtraction check (expected 0x85)
	movq $0xa9, %rbx
	movq $0x24, %rax
	subq %rax, %rbx

	# Check if higher bits of %rdx remain unaffected (no overflow)
	movq $0x00, %rdx
	negq %rdx
	movq $0xff, %rcx
	subb %rcx, %rdx

	# Check if higher bits of %r9 remain unaffected (with overflow)
	movq $0xff3c, %r9
	movq $0x003d, %r8
	subb %r8, %r9
