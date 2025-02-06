# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0x9988776655443322)
	cmoveq %r15, %r8

	# Long (expected $0xaabbccdd)
	cmovel %r15d, %r10d

	# Word (expected 0x7192)
	cmovew %r15w, %r12w
