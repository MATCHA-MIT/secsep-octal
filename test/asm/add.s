# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Basic addition check
	movq $0x34, %rax
	movq $0x30, %rbx
	addq %rax, %rbx

	# Check if higher bits of %rdx remain unaffected (no overflow)
	movq $0x00, %rcx
	negq %rcx
	movq $0x00, %rdx
	addb %rcx, %rdx

	# Check if higher bits of %r9 remain unaffected (with overflow)
	movq $0x99,  %r8
	movq $0x167, %r9
	addb %r8, %r9
