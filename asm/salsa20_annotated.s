salsa20_block:
.LFB3:
	pushq	%rbx
	movq	%rdi, %rbx
	addq	$-128, %rsp
	movq	(%rsi), %rax  # rax is Top
	leaq	64(%rsp), %rdi
	movl	$1634760805, (%rsp)
	movq	%rax, 4(%rsp)
	movq	8(%rsi), %rax
	movl	$857760878, 20(%rsp)
	movq	%rax, 12(%rsp)
	movq	16(%rsi), %rax
	movq	%rdx, 24(%rsp)
	movq	%rax, 44(%rsp)
	movq	24(%rsi), %rax
	movq	%rsp, %rsi
	movq	%rcx, 32(%rsp)
	movq	%rax, 52(%rsp)
	movl	$2036477234, 40(%rsp)
	movl	$1797285236, 60(%rsp)
	call	salsa20_words               # For now, let's ignore function call
	xorl	%eax, %eax
.L7:
	movl	%eax, %edx
	movl	%eax, %ecx
	sarl	$2, %edx
	andl	$3, %ecx
	movslq	%edx, %rdx
	sall	$3, %ecx
	movl	64(%rsp,%rdx,4), %edx
	shrl	%cl, %edx
	movb	%dl, (%rbx,%rax)
	addq	$1, %rax
	cmpq	$64, %rax
	jne	.L7
	subq	$-128, %rsp
	popq	%rbx
	ret
