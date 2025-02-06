# This Assembly program is written in GAS syntax (i.e. opcode src, dest)

.global main
main:
	# Quad (expected $0xf7e6d5c4b3a29180)
	movq $0x8091a2b3c4d5e6f7, %rbx
	bswapq %rbx

	# Long (expected 0xccaa8866)
	movl $0x6688aacc, %eax
	bswapl %eax

