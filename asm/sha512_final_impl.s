sha512_final_impl:
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
	cmpq	$112, %rax
	jbe	.L13
	movl	$128, %edx
	subq	%rax, %rdx
	jne	.L73
.L14:
	movl	$1, %edx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	call	sha512_block_data_order
	movq	%r13, %rsi
	movl	$112, %edx
.L21:
	movl	%edx, %ecx
	cmpl	$8, %edx
	jb	.L74
	leaq	8(%rsi), %rdi
	movl	%edx, %eax
	movq	$0, (%rsi)
	movq	$0, -8(%rsi,%rax)
	andq	$-8, %rdi
	xorl	%eax, %eax
	subq	%rdi, %rsi
	leal	(%rdx,%rsi), %ecx
	shrl	$3, %ecx
#	rep stosq
.L22:
	movq	72(%rbx), %rax
	movq	64(%rbx), %rdx
	movq	%r13, %rsi
	movq	%rbx, %rdi
#	bswap	%rdx
#	bswap	%rax
#	movq	%rdx, %xmm1
#	movq	%rax, %xmm0
#	punpcklqdq	%xmm1, %xmm0
	movl	$1, %edx
#	movups	%xmm0, 192(%rbx)
	movq	%rax, 192(%rbx) # Replace the last store with this store - not the same meaning
	call	sha512_block_data_order
	testq	%r12, %r12
#	cmpq	%r12, $0
	je	.L31
	shrq	$3, %rbp
	je	.L30
	movq	(%rbx), %rax
#	bswap	%rax
	movq	%rax, (%r12)
	cmpq	$1, %rbp
	je	.L30
	movq	8(%rbx), %rax
#	bswap	%rax
	movq	%rax, 8(%r12)
	cmpq	$2, %rbp
	je	.L30
	movq	16(%rbx), %rax
#	bswap	%rax
	movq	%rax, 16(%r12)
	cmpq	$3, %rbp
	je	.L30
	movq	24(%rbx), %rax
#	bswap	%rax
	movq	%rax, 24(%r12)
	cmpq	$4, %rbp
	je	.L30
	movq	32(%rbx), %rax
#	bswap	%rax
	movq	%rax, 32(%r12)
	cmpq	$5, %rbp
	je	.L30
	movq	40(%rbx), %rax
#	bswap	%rax
	movq	%rax, 40(%r12)
	cmpq	$6, %rbp
	je	.L30
	movq	48(%rbx), %rax
#	bswap	%rax
	movq	%rax, 48(%r12)
	cmpq	$7, %rbp
	je	.L30
	movq	56(%rbx), %rax
#	bswap	%rax
	movq	%rax, 56(%r12)
.L30:
	addq	$8, %rsp
	movl	$1, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L74:
	andl	$4, %edx
	jne	.L75
	testl	%ecx, %ecx
#	cmpl	%ecx, $0
	je	.L22
	movb	$0, (%rsi)
	testb	$2, %cl
#	andb	$2, %cl
#	cmpb	%cl, $0
	je	.L22
	xorl	%eax, %eax
	movw	%ax, -2(%rsi,%rcx)
	jmp	.L22
.L73:
	xorl	%edi, %edi
	cmpq	$8, %rdx
	jb	.L76
	leaq	8(%rsi), %rcx
	movq	$0, (%rsi)
	movq	$0, -8(%rsi,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rsi
	addq	%rsi, %rdx
	andq	$-8, %rdx
	cmpq	$8, %rdx
	jb	.L14
	andq	$-8, %rdx
	xorl	%eax, %eax
.L19:
	movq	%rdi, (%rcx,%rax)
	addq	$8, %rax
	cmpq	%rdx, %rax
	jb	.L19
	jmp	.L14
.L13:
	movl	$112, %edx
	subq	%rax, %rdx
	je	.L22
	jmp	.L21
.L76:
	testb	$4, %dl
#	andb	$4, %dl
#	cmpb	%dl, $0
	jne	.L77
	testq	%rdx, %rdx
#	cmpq	%rdx, $0
	je	.L14
	movb	$0, (%rsi)
	testb	$2, %dl
#	andb	$2, %dl
#	cmpb	%dl, $0
	je	.L14
	xorl	%ecx, %ecx
	movw	%cx, -2(%rsi,%rdx)
	jmp	.L14
.L31:
	addq	$8, %rsp
	xorl	%eax, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L75:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rcx)
	jmp	.L22
.L77:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rdx)
	jmp	.L14
