# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Carry Flag (and Overflow Flag)
	movq $0xff00000000000001, %rax
	movq $0x0100000000000000, %rbx
	addq %rax, %rbx

	# Parity Flag
	movq $0x73, %rcx
	movq $0x04, %rdx
	addq %rcx, %rdx

	# Auxiliary Carry Flag 
	movq $0x0f, %r8
	movq $0x02, %r9
	addq %r8, %r9

	# Zero Flag (and Parity Flag)
	movq $0x00, %r10
	movq $0x00, %r11
	addq %r10, %r11

	# Sign Flag
	movq $0x0000000000000000, %r12
	movq $0x0000000000000000, %r13
	addq %r12, %r13

	# Overflow Flag (and Carry Flag)
	movq $0xff00000000000007, %r14
	movq $0x0100000000000000, %r15
	addq %r14, %r15
