# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0xffcc996666778899)
	movq $0x9988776655443322, %r8
	movq $0x6644220011335577, %r9
	adcq %r8, %r9

	# Long (expected $0xeeeeeeee)
	movq $0xaabbccdd, %r10
	movq $0xddccbbaa, %r11
	adcl %r10d, %r11d

	# Word (expected 0x90f1)
	movq $0x7192, %r12
	movq $0x1f5f, %r13
	adcw %r12w, %r13w

	# Byte (expected 0x58)
	movq $0x33, %r14
	movq $0x25, %r15
	adcb %r14b, %r15b
