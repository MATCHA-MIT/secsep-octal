sha512_block_data_order:
.LFB47:
	pushq	%r15
	salq	$7, %rdx
	pushq	%r14
	pushq	%r13
	pushq	%r12
	movq	%rsi, %r12
	pushq	%rbp
	pushq	%rbx
	subq	$72, %rsp
	leaq	(%rsi,%rdx), %rax
	movq	%rax, 56(%rsp)
.L8:
	movq	(%r12), %rax
	movq	%r12, 40(%rsp)
	movl	$K512, %r10d
.L7:
	movl	$K512+512, %ebx
	cmpq	%r10, %rbx
	jne	.L7
	movq	40(%rsp), %r12
	movq	56(%rsp), %rax
	cmpq	%rax, %r12
	jne	.L8
	addq	$72, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
