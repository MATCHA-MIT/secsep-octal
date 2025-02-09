# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Basic addition check (expected 0x64)
	movq $0x34, %rax
	movq $0x30, %rbx
	addq %rax, %rbx

	# Check if higher bits of %rdx remain unaffected (no overflow) (expected 0xfff...f00)
	movq $0x00, %rcx
	movq $0x00, %rdx
	notq %rdx
	addb %cl, %dl

	# Check if higher bits of %r9 remain unaffected (with overflow) (expected 0x200)
	movq $0x99,  %r8
	movq $0x267, %r9
	addb %r8b, %r9b
