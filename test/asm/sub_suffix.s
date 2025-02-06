# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x3344556644113322)
	movq $0x9988776655443322, %r9
	movq $0x6644220011330000, %r8
	subq %r8, %r9

	# Long (expected $0xccbbaa99)
	movq $0xddccbbaa, %r11
	movq $0x11111111, %r10
	subl %r10d, %r11d

	# Word (expected 0x5233)
	movq $0x7192, %r13
	movq $0x1f5f, %r12
	subw %r12w, %r13w

	# Byte (expected 0x0e)
	movq $0x33, %r15
	movq $0x25, %r14
	subb %r14b, %r15b
