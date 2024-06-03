double_layer:
.LFB49:
	pushq	%r13
	leaq	80(%rdx), %r13
	pushq	%r12
	movq	%rdi, %r12
	pushq	%rbp
	movq	%rsi, %rbp
	pushq	%rbx
	movq	%rdx, %rbx
	subq	$8, %rsp
	movl	208(%rdx), %eax
	movb	$-128, 80(%rdx,%rax)
	addq	$1, %rax
	leaq	0(%r13,%rax), %rsi
	addq	$8, %rsp
	movl	$1, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret