	.file	"bench_ed25519_plain.c"
	.text
	.p2align 4
	.type	fiat_25519_carry_mul, @function
fiat_25519_carry_mul:
.LFB54:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %rax
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbx
	movq	32(%rdx), %rdi
	movq	24(%rdx), %rbp
	movq	32(%rsi), %r14
	leaq	(%rdi,%rdi,8), %rcx
	movq	16(%rdx), %rsi
	movq	24(%rax), %r11
	movq	8(%rax), %r10
	movq	(%rdx), %r15
	movq	%rdi, -48(%rsp)
	leaq	(%rdi,%rcx,2), %rdi
	leaq	0(%rbp,%rbp,8), %rcx
	movq	%rdi, -56(%rsp)
	movq	16(%rax), %rdi
	leaq	0(%rbp,%rcx,2), %r8
	leaq	(%rsi,%rsi,8), %rcx
	movq	(%rax), %rax
	movq	%r10, -64(%rsp)
	leaq	(%rsi,%rcx,2), %r9
	movq	8(%rdx), %rcx
	movq	%r11, -72(%rsp)
	movq	%rax, -80(%rsp)
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %r13
	movq	%r13, %rax
	mulq	%r14
	movq	%rax, %r12
	movq	%r11, %rax
	movq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	-56(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r10
	addq	%rax, %r12
	movq	-80(%rsp), %rax
	adcq	%rdx, %r13
	movq	%r12, %r10
	mulq	%r15
	movq	%r13, %r11
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	%r14
	movq	%r10, -40(%rsp)
	movq	%r11, -32(%rsp)
	movq	%rax, %r12
	movq	-72(%rsp), %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	-56(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	-64(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r15
	addq	%rax, %r12
	movq	-80(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rcx
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	-56(%rsp), %rax
	mulq	-72(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	-64(%rsp), %rax
	adcq	%rdx, %r9
	movq	%r8, %r10
	mulq	%rcx
	movq	%r9, %r11
	movq	%r12, %r9
	addq	%rax, %r10
	movq	-80(%rsp), %rax
	movq	%r12, -24(%rsp)
	adcq	%rdx, %r11
	movq	%r13, -16(%rsp)
	mulq	%rsi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r9
	xorl	%r13d, %r13d
	movq	%r9, %r12
	addq	%rax, %r12
	movq	-56(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r8
	movq	-72(%rsp), %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	-64(%rsp), %rax
	adcq	%rdx, %r9
	movq	%r8, %r10
	movq	%r12, %r8
	mulq	%rsi
	movq	%r9, %r11
	addq	%rax, %r10
	movq	-80(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rbp
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r8
	xorl	%r9d, %r9d
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	%r15
	movq	%rax, %r14
	movq	-72(%rsp), %rax
	movq	%rdx, %r15
	mulq	%rcx
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	%rsi
	movq	%rax, %rsi
	movq	-64(%rsp), %rax
	movq	%rdx, %rdi
	addq	%r14, %rsi
	adcq	%r15, %rdi
	mulq	%rbp
	addq	%rax, %rsi
	movq	-80(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	-48(%rsp)
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%r8, %rdi
	shrdq	$51, %r9, %rdi
	movq	%rdi, %rsi
	xorl	%edi, %edi
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	shrdq	$51, %rdi, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rcx
	movq	-40(%rsp), %rax
	movabsq	$2251799813685247, %rdx
	andq	%rdx, %r12
	andq	%rdx, %rsi
	andq	%rdx, %rax
	movq	%rsi, 32(%rbx)
	addq	%rax, %rcx
	movq	-24(%rsp), %rax
	movq	%rcx, %rdi
	andq	%rdx, %rcx
	andq	%rdx, %rax
	shrq	$51, %rdi
	movq	%rcx, (%rbx)
	addq	%rdi, %rax
	movq	%rax, %rcx
	shrq	$51, %rax
	addq	%r12, %rax
	andq	%rdx, %rcx
	movq	%rax, 16(%rbx)
	movq	%r8, %rax
	andq	%rdx, %rax
	movq	%rcx, 8(%rbx)
	movq	%rax, 24(%rbx)
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE54:
	.size	fiat_25519_carry_mul, .-fiat_25519_carry_mul
	.p2align 4
	.type	sc_muladd, @function
sc_muladd:
.LFB84:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %r9
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %rax
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$80, %rsp
	.cfi_def_cfa_offset 136
	movzbl	1(%r9), %r8d
	movzbl	2(%r9), %ecx
	movzbl	(%r9), %edi
	movzbl	15(%r9), %r10d
	salq	$16, %rcx
	salq	$8, %r8
	movl	2(%r9), %r12d
	movl	7(%r9), %ebx
	orq	%rcx, %r8
	movzbl	7(%r9), %ecx
	salq	$16, %r10
	shrq	$5, %r12
	orq	%rdi, %r8
	movzbl	6(%r9), %edi
	shrq	$7, %rbx
	salq	$16, %rcx
	andl	$2097151, %r8d
	andl	$2097151, %r12d
	andl	$2097151, %ebx
	salq	$8, %rdi
	orq	%rcx, %rdi
	movzbl	5(%r9), %ecx
	orq	%rcx, %rdi
	movl	10(%r9), %ecx
	shrq	$2, %rdi
	shrq	$4, %rcx
	andl	$2097151, %edi
	andl	$2097151, %ecx
	movq	%rcx, -104(%rsp)
	movzbl	14(%r9), %ecx
	salq	$8, %rcx
	orq	%r10, %rcx
	movzbl	13(%r9), %r10d
	orq	%r10, %rcx
	movzbl	20(%r9), %r10d
	shrq	%rcx
	andl	$2097151, %ecx
	salq	$16, %r10
	movq	%rcx, -80(%rsp)
	movl	15(%r9), %ecx
	shrq	$6, %rcx
	movq	%rcx, %r15
	movzbl	19(%r9), %ecx
	andl	$2097151, %r15d
	salq	$8, %rcx
	movq	%r15, -72(%rsp)
	orq	%r10, %rcx
	movzbl	18(%r9), %r10d
	orq	%r10, %rcx
	movzbl	21(%r9), %r10d
	movq	%rcx, %r11
	movzbl	22(%r9), %ecx
	shrq	$3, %r11
	movq	%r11, -40(%rsp)
	movzbl	23(%r9), %r11d
	salq	$8, %rcx
	salq	$16, %r11
	orq	%r11, %rcx
	orq	%r10, %rcx
	movq	%rcx, %r14
	movl	23(%r9), %ecx
	andl	$2097151, %r14d
	shrq	$5, %rcx
	movq	%r14, -32(%rsp)
	movq	%rcx, %rbp
	andl	$2097151, %ebp
	movq	%rbp, -24(%rsp)
	movzbl	27(%r9), %r10d
	movzbl	28(%r9), %ecx
	movzbl	(%rsi), %r11d
	salq	$8, %r10
	movl	2(%rsi), %ebp
	movzbl	14(%rsi), %r14d
	salq	$16, %rcx
	movzbl	15(%rsi), %r13d
	shrq	$5, %rbp
	orq	%r10, %rcx
	movzbl	26(%r9), %r10d
	salq	$8, %r14
	salq	$16, %r13
	andl	$2097151, %ebp
	orq	%r10, %rcx
	shrq	$2, %rcx
	movq	%rcx, %r10
	movl	28(%r9), %ecx
	andl	$2097151, %r10d
	movq	%r10, -16(%rsp)
	movq	%rcx, %r9
	movzbl	2(%rsi), %r10d
	movzbl	1(%rsi), %ecx
	shrq	$7, %r9
	salq	$16, %r10
	movq	%r9, -48(%rsp)
	salq	$8, %rcx
	orq	%r10, %rcx
	movzbl	6(%rsi), %r10d
	orq	%r11, %rcx
	movl	7(%rsi), %r11d
	movq	%rcx, %r9
	movzbl	7(%rsi), %ecx
	salq	$8, %r10
	shrq	$7, %r11
	andl	$2097151, %r9d
	salq	$16, %rcx
	andl	$2097151, %r11d
	orq	%r10, %rcx
	movzbl	5(%rsi), %r10d
	orq	%r10, %rcx
	movl	10(%rsi), %r10d
	shrq	$2, %rcx
	shrq	$4, %r10
	andl	$2097151, %ecx
	andl	$2097151, %r10d
	orq	%r14, %r13
	movzbl	13(%rsi), %r14d
	orq	%r14, %r13
	shrq	%r13
	movq	%r13, %r15
	movl	15(%rsi), %r13d
	andl	$2097151, %r15d
	shrq	$6, %r13
	movq	%r15, -120(%rsp)
	movq	%r13, %r14
	movzbl	20(%rsi), %r13d
	andl	$2097151, %r14d
	movq	%r14, -64(%rsp)
	movzbl	19(%rsi), %r14d
	salq	$16, %r13
	salq	$8, %r14
	orq	%r14, %r13
	movzbl	18(%rsi), %r14d
	orq	%r14, %r13
	movzbl	23(%rsi), %r14d
	movq	%r13, %r15
	movzbl	22(%rsi), %r13d
	shrq	$3, %r15
	salq	$16, %r14
	movq	%r15, -112(%rsp)
	salq	$8, %r13
	movzbl	21(%rsi), %r15d
	orq	%r14, %r13
	orq	%r15, %r13
	movq	%r13, %r15
	movl	23(%rsi), %r13d
	andl	$2097151, %r15d
	shrq	$5, %r13
	movq	%r15, -96(%rsp)
	movq	%r13, %r14
	andl	$2097151, %r14d
	movq	%r14, -88(%rsp)
	movzbl	27(%rsi), %r14d
	movzbl	28(%rsi), %r13d
	salq	$8, %r14
	salq	$16, %r13
	orq	%r14, %r13
	movzbl	26(%rsi), %r14d
	movl	28(%rsi), %esi
	shrq	$7, %rsi
	orq	%r14, %r13
	movq	%rsi, -56(%rsp)
	movzbl	(%rdx), %esi
	shrq	$2, %r13
	movq	%r13, %r15
	movzbl	2(%rdx), %r13d
	movq	%rsi, %r14
	movzbl	21(%rdx), %esi
	andl	$2097151, %r15d
	salq	$16, %r13
	movq	%rsi, 56(%rsp)
	movzbl	1(%rdx), %esi
	salq	$8, %rsi
	orq	%r13, %rsi
	movzbl	6(%rdx), %r13d
	orq	%r14, %rsi
	movq	%r8, %r14
	imulq	%r9, %r14
	andl	$2097151, %esi
	addq	%r14, %rsi
	movq	%r8, %r14
	movq	%rsi, -8(%rsp)
	movl	2(%rdx), %esi
	shrq	$5, %rsi
	imulq	%rbp, %r14
	salq	$8, %r13
	andl	$2097151, %esi
	addq	%r14, %rsi
	movq	%r12, %r14
	imulq	%r9, %r14
	addq	%r14, %rsi
	movq	%r8, %r14
	movq	%rsi, (%rsp)
	movzbl	7(%rdx), %esi
	imulq	%rcx, %r14
	salq	$16, %rsi
	orq	%r13, %rsi
	movzbl	5(%rdx), %r13d
	orq	%r13, %rsi
	movq	%r14, %r13
	movq	%rdi, %r14
	shrq	$2, %rsi
	imulq	%r9, %r14
	andl	$2097151, %esi
	addq	%rsi, %r13
	movq	%r12, %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	addq	%r14, %rsi
	movq	%r8, %r14
	movq	%rsi, 8(%rsp)
	movl	7(%rdx), %esi
	imulq	%r11, %r14
	shrq	$7, %rsi
	andl	$2097151, %esi
	addq	%r14, %rsi
	movq	%r12, %r14
	imulq	%rcx, %r14
	movq	%r14, %r13
	movq	%rbx, %r14
	addq	%rsi, %r13
	imulq	%r9, %r14
	movq	%rdi, %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	addq	%r14, %rsi
	movq	%r8, %r14
	imulq	%r10, %r14
	movq	%rsi, 16(%rsp)
	movl	10(%rdx), %esi
	shrq	$4, %rsi
	andl	$2097151, %esi
	movq	%r14, %r13
	movq	%rdi, %r14
	addq	%rsi, %r13
	movq	%r12, %rsi
	imulq	%r11, %rsi
	imulq	%rcx, %r14
	addq	%r13, %rsi
	movq	%r14, %r13
	movq	%r12, %r14
	addq	%rsi, %r13
	imulq	%r10, %r14
	movq	%rbx, %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-104(%rsp), %r13
	imulq	%r9, %r13
	addq	%r13, %rsi
	movzbl	14(%rdx), %r13d
	movq	%rsi, 24(%rsp)
	movzbl	15(%rdx), %esi
	salq	$8, %r13
	salq	$16, %rsi
	orq	%r13, %rsi
	movzbl	13(%rdx), %r13d
	orq	%r13, %rsi
	movq	-120(%rsp), %r13
	shrq	%rsi
	imulq	%r8, %r13
	andl	$2097151, %esi
	addq	%r13, %rsi
	movq	%r14, %r13
	movq	%rbx, %r14
	addq	%rsi, %r13
	imulq	%rcx, %r14
	movq	%rdi, %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	%r14, %r13
	addq	%rsi, %r13
	movq	-104(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-80(%rsp), %r13
	imulq	%r9, %r13
	leaq	(%rsi,%r13), %r14
	movl	15(%rdx), %esi
	movq	%r14, 32(%rsp)
	movq	-64(%rsp), %r14
	shrq	$6, %rsi
	movq	%r14, %r13
	andl	$2097151, %esi
	imulq	%r8, %r13
	addq	%rsi, %r13
	movq	-120(%rsp), %rsi
	imulq	%r12, %rsi
	addq	%r13, %rsi
	movq	%rdi, %r13
	imulq	%r10, %r13
	addq	%rsi, %r13
	movq	%rbx, %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	-104(%rsp), %r13
	imulq	%rcx, %r13
	addq	%rsi, %r13
	movq	-80(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-72(%rsp), %r13
	imulq	%r9, %r13
	addq	%rsi, %r13
	movzbl	20(%rdx), %esi
	movq	%r13, 40(%rsp)
	movzbl	19(%rdx), %r13d
	salq	$16, %rsi
	salq	$8, %r13
	orq	%r13, %rsi
	movzbl	18(%rdx), %r13d
	orq	%r13, %rsi
	movq	-112(%rsp), %r13
	shrq	$3, %rsi
	imulq	%r8, %r13
	addq	%r13, %rsi
	movq	%r14, %r13
	imulq	%r12, %r13
	addq	%rsi, %r13
	movq	-120(%rsp), %rsi
	imulq	%rdi, %rsi
	addq	%r13, %rsi
	movq	%rbx, %r13
	imulq	%r10, %r13
	addq	%rsi, %r13
	movq	-104(%rsp), %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	-80(%rsp), %r13
	imulq	%rcx, %r13
	addq	%rsi, %r13
	movq	-72(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-40(%rsp), %r13
	imulq	%r9, %r13
	addq	%rsi, %r13
	movzbl	22(%rdx), %esi
	movq	%r13, 48(%rsp)
	movzbl	23(%rdx), %r13d
	salq	$8, %rsi
	salq	$16, %r13
	orq	%r13, %rsi
	movq	56(%rsp), %r13
	orq	%r13, %rsi
	movq	-96(%rsp), %r13
	andl	$2097151, %esi
	imulq	%r8, %r13
	addq	%rsi, %r13
	movq	-112(%rsp), %rsi
	imulq	%r12, %rsi
	addq	%r13, %rsi
	movq	%r14, %r13
	imulq	%rdi, %r13
	imulq	%rbx, %r14
	addq	%rsi, %r13
	movq	-120(%rsp), %rsi
	imulq	%rbx, %rsi
	addq	%r13, %rsi
	movq	-104(%rsp), %r13
	imulq	%r10, %r13
	addq	%rsi, %r13
	movq	-80(%rsp), %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	-72(%rsp), %r13
	imulq	%rcx, %r13
	addq	%rsi, %r13
	movq	-40(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-32(%rsp), %r13
	imulq	%r9, %r13
	addq	%rsi, %r13
	movl	23(%rdx), %esi
	movq	%r13, 56(%rsp)
	movq	-88(%rsp), %r13
	shrq	$5, %rsi
	imulq	%r8, %r13
	andl	$2097151, %esi
	addq	%r13, %rsi
	movq	-96(%rsp), %r13
	imulq	%r12, %r13
	addq	%rsi, %r13
	movq	-112(%rsp), %rsi
	imulq	%rdi, %rsi
	addq	%r13, %rsi
	movq	%r14, %r13
	movq	-120(%rsp), %r14
	addq	%rsi, %r13
	movq	-104(%rsp), %rsi
	imulq	%r14, %rsi
	addq	%r13, %rsi
	movq	-80(%rsp), %r13
	imulq	%r10, %r13
	addq	%rsi, %r13
	movq	-72(%rsp), %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	-40(%rsp), %r13
	imulq	%rcx, %r13
	addq	%rsi, %r13
	movq	-32(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-24(%rsp), %r13
	imulq	%r9, %r13
	leaq	(%rsi,%r13), %r14
	movzbl	28(%rdx), %r13d
	movzbl	27(%rdx), %esi
	movq	%r14, 64(%rsp)
	movq	%r8, %r14
	imulq	%r15, %r14
	salq	$16, %r13
	salq	$8, %rsi
	orq	%r13, %rsi
	movzbl	26(%rdx), %r13d
	movl	28(%rdx), %edx
	orq	%r13, %rsi
	movq	%r14, %r13
	movq	-88(%rsp), %r14
	shrq	$2, %rsi
	imulq	%r12, %r14
	andl	$2097151, %esi
	addq	%rsi, %r13
	movq	%r14, %rsi
	movq	-96(%rsp), %r14
	addq	%r13, %rsi
	imulq	%rdi, %r14
	movq	%r14, %r13
	movq	-112(%rsp), %r14
	addq	%rsi, %r13
	imulq	%rbx, %r14
	movq	%r14, %rsi
	movq	-64(%rsp), %r14
	addq	%r13, %rsi
	movq	-104(%rsp), %r13
	imulq	%r14, %r13
	movq	-120(%rsp), %r14
	addq	%rsi, %r13
	movq	-80(%rsp), %rsi
	imulq	%r14, %rsi
	addq	%r13, %rsi
	movq	-72(%rsp), %r13
	imulq	%r10, %r13
	addq	%rsi, %r13
	movq	-40(%rsp), %rsi
	imulq	%r11, %rsi
	addq	%r13, %rsi
	movq	-32(%rsp), %r13
	imulq	%rcx, %r13
	shrq	$7, %rdx
	addq	%rsi, %r13
	movq	-24(%rsp), %rsi
	imulq	%rbp, %rsi
	addq	%r13, %rsi
	movq	-16(%rsp), %r13
	imulq	%r9, %r13
	leaq	(%rsi,%r13), %r14
	movq	-56(%rsp), %rsi
	movq	-112(%rsp), %r13
	movq	%r14, 72(%rsp)
	movq	-64(%rsp), %r14
	imulq	%rsi, %r8
	addq	%rdx, %r8
	movq	%r12, %rdx
	imulq	%r15, %rdx
	movq	%rdx, %rsi
	movq	-88(%rsp), %rdx
	addq	%r8, %rsi
	movq	-96(%rsp), %r8
	imulq	%rdi, %rdx
	imulq	%rbx, %r8
	addq	%rsi, %rdx
	movq	%r8, %rsi
	movq	-104(%rsp), %r8
	addq	%rdx, %rsi
	movq	%r8, %rdx
	imulq	%r13, %rdx
	movq	-80(%rsp), %r13
	addq	%rsi, %rdx
	movq	%r13, %rsi
	imulq	%r14, %rsi
	movq	-120(%rsp), %r14
	addq	%rdx, %rsi
	movq	-72(%rsp), %rdx
	imulq	%r14, %rdx
	movq	-40(%rsp), %r14
	addq	%rsi, %rdx
	movq	%r14, %rsi
	imulq	%r10, %rsi
	addq	%rdx, %rsi
	movq	-32(%rsp), %rdx
	imulq	%r11, %rdx
	addq	%rsi, %rdx
	movq	-24(%rsp), %rsi
	imulq	%rcx, %rsi
	addq	%rdx, %rsi
	movq	-16(%rsp), %rdx
	imulq	%rbp, %rdx
	addq	%rsi, %rdx
	movq	-48(%rsp), %rsi
	imulq	%rsi, %r9
	addq	%rdx, %r9
	movq	-56(%rsp), %rdx
	movq	%r9, -104(%rsp)
	movq	%rdi, %r9
	imulq	%r15, %r9
	imulq	%rdx, %r12
	movq	%r9, %rdx
	movq	-88(%rsp), %r9
	addq	%r12, %rdx
	movq	-64(%rsp), %r12
	imulq	%rbx, %r9
	movq	%r9, %rsi
	movq	-96(%rsp), %r9
	addq	%rdx, %rsi
	movq	%r8, %rdx
	imulq	%r9, %rdx
	movq	-112(%rsp), %r9
	addq	%rsi, %rdx
	movq	%r13, %rsi
	imulq	%r9, %rsi
	movq	-72(%rsp), %r9
	addq	%rdx, %rsi
	movq	%r9, %rdx
	imulq	%r12, %rdx
	movq	%r14, %r12
	addq	%rsi, %rdx
	movq	%r14, %rsi
	movq	-120(%rsp), %r14
	imulq	%r14, %rsi
	movq	-32(%rsp), %r14
	addq	%rdx, %rsi
	movq	%r14, %rdx
	imulq	%r10, %rdx
	addq	%rsi, %rdx
	movq	-24(%rsp), %rsi
	imulq	%r11, %rsi
	addq	%rdx, %rsi
	movq	-16(%rsp), %rdx
	imulq	%rcx, %rdx
	addq	%rsi, %rdx
	movq	-48(%rsp), %rsi
	imulq	%rsi, %rbp
	addq	%rdx, %rbp
	movq	-56(%rsp), %rdx
	movq	%rbp, -80(%rsp)
	movq	%rbx, %rbp
	imulq	%r15, %rbp
	imulq	%rdx, %rdi
	movq	%r8, %rdx
	movq	%rbp, %rsi
	movq	-24(%rsp), %rbp
	addq	%rdi, %rsi
	movq	-88(%rsp), %rdi
	imulq	%rdi, %rdx
	movq	-96(%rsp), %rdi
	addq	%rsi, %rdx
	movq	%r13, %rsi
	imulq	%rdi, %rsi
	movq	-112(%rsp), %rdi
	addq	%rdx, %rsi
	movq	%r9, %rdx
	imulq	%rdi, %rdx
	movq	-64(%rsp), %rdi
	addq	%rsi, %rdx
	movq	%r12, %rsi
	imulq	%rdi, %rsi
	movq	%r14, %rdi
	addq	%rdx, %rsi
	movq	%r14, %rdx
	movq	-120(%rsp), %r14
	imulq	%r14, %rdx
	movq	-64(%rsp), %r14
	addq	%rsi, %rdx
	movq	%rbp, %rsi
	imulq	%r10, %rsi
	addq	%rdx, %rsi
	movq	-16(%rsp), %rdx
	imulq	%r11, %rdx
	addq	%rsi, %rdx
	movq	-48(%rsp), %rsi
	imulq	%rsi, %rcx
	addq	%rdx, %rcx
	movq	-56(%rsp), %rdx
	movq	%rcx, -72(%rsp)
	movq	%r13, %rcx
	imulq	%rdx, %rbx
	movq	%r8, %rdx
	imulq	%r15, %rdx
	addq	%rbx, %rdx
	movq	-88(%rsp), %rbx
	imulq	%rbx, %rcx
	movq	-96(%rsp), %rbx
	addq	%rdx, %rcx
	movq	%r9, %rdx
	imulq	%rbx, %rdx
	movq	-112(%rsp), %rbx
	movq	-120(%rsp), %rsi
	addq	%rcx, %rdx
	movq	%r12, %rcx
	imulq	%rbx, %rcx
	movq	-16(%rsp), %rbx
	addq	%rdx, %rcx
	movq	%rdi, %rdx
	imulq	%r14, %rdx
	addq	%rcx, %rdx
	movq	%rbp, %rcx
	imulq	%rsi, %rcx
	addq	%rdx, %rcx
	movq	%rbx, %rdx
	imulq	%r10, %rdx
	addq	%rcx, %rdx
	movq	-48(%rsp), %rcx
	imulq	%rcx, %r11
	movq	%r13, %rcx
	imulq	%r15, %rcx
	addq	%r11, %rdx
	movq	-88(%rsp), %r11
	movq	%rdx, -120(%rsp)
	movq	-56(%rsp), %rdx
	imulq	%rdx, %r8
	movq	%r9, %rdx
	imulq	%r11, %rdx
	movq	-112(%rsp), %r11
	addq	%r8, %rcx
	movq	-96(%rsp), %r8
	addq	%rcx, %rdx
	movq	%r12, %rcx
	imulq	%r8, %rcx
	addq	%rdx, %rcx
	movq	%rdi, %rdx
	imulq	%r11, %rdx
	addq	%rcx, %rdx
	movq	%rbp, %rcx
	imulq	%r14, %rcx
	addq	%rdx, %rcx
	movq	%rbx, %rdx
	imulq	%rsi, %rdx
	addq	%rcx, %rdx
	movq	-48(%rsp), %rcx
	imulq	%rcx, %r10
	movq	%r9, %rcx
	imulq	%r15, %rcx
	addq	%r10, %rdx
	movq	-88(%rsp), %r10
	movq	%rdx, -112(%rsp)
	movq	-56(%rsp), %rdx
	imulq	%rdx, %r13
	movq	%r13, %rdx
	movq	-48(%rsp), %r13
	addq	%rcx, %rdx
	movq	%r12, %rcx
	imulq	%r10, %rcx
	imulq	%r13, %rsi
	addq	%rdx, %rcx
	movq	%rdi, %rdx
	imulq	%r8, %rdx
	addq	%rcx, %rdx
	movq	%rbp, %rcx
	imulq	%r11, %rcx
	addq	%rdx, %rcx
	movq	%rbx, %rdx
	imulq	%r14, %rdx
	imulq	%r13, %r14
	addq	%rcx, %rdx
	movq	%rsi, %rcx
	movq	%r12, %rsi
	addq	%rdx, %rcx
	movq	-56(%rsp), %rdx
	imulq	%r15, %rsi
	movq	%rcx, -96(%rsp)
	movq	%rdi, %rcx
	imulq	%rdx, %r9
	imulq	%r10, %rcx
	imulq	%rdx, %r12
	addq	%r9, %rsi
	movq	%rbx, %r9
	addq	%rsi, %rcx
	movq	%rbp, %rsi
	imulq	%r8, %rsi
	addq	%rcx, %rsi
	movq	%rbx, %rcx
	imulq	%r11, %rcx
	addq	%rsi, %rcx
	movq	%rdi, %rsi
	imulq	%r15, %rsi
	addq	%r14, %rcx
	movq	%rcx, -88(%rsp)
	movq	%rbp, %rcx
	addq	%r12, %rsi
	imulq	%r10, %rcx
	imulq	%r8, %r9
	imulq	%rdx, %rdi
	addq	%rsi, %rcx
	imulq	%r13, %r8
	movq	%rbp, %rsi
	imulq	%r15, %rsi
	addq	%rcx, %r9
	movq	%rbx, %rcx
	imulq	%r10, %rcx
	imulq	%r13, %r11
	addq	%rdi, %rsi
	movq	8(%rsp), %rdi
	imulq	%rdx, %rbp
	addq	%rsi, %rcx
	imulq	%r13, %r10
	movq	24(%rsp), %rsi
	leaq	(%rcx,%r8), %r8
	addq	%r11, %r9
	movq	%r8, -64(%rsp)
	movq	%rbx, %r8
	imulq	%rdx, %rbx
	imulq	%r15, %r8
	imulq	%r13, %r15
	imulq	%r13, %rdx
	addq	%rbp, %r8
	leaq	(%rbx,%r15), %r12
	movq	-8(%rsp), %rbx
	movq	40(%rsp), %r15
	addq	%r10, %r8
	movq	%r12, -56(%rsp)
	movq	56(%rsp), %r10
	movq	%rdx, %r14
	leaq	1048576(%rbx), %rcx
	movq	%rcx, %r11
	andq	$-2097152, %rcx
	sarq	$21, %r11
	subq	%rcx, %rbx
	leaq	1048576(%rdi), %rcx
	movq	%r11, %r13
	movq	(%rsp), %r11
	movq	%rcx, %r12
	movq	%rbx, -48(%rsp)
	andq	$-2097152, %rcx
	sarq	$21, %r12
	addq	%r11, %r13
	movq	16(%rsp), %r11
	subq	%rcx, %rdi
	leaq	1048576(%rsi), %rcx
	movq	%rdi, -40(%rsp)
	movq	64(%rsp), %rdi
	addq	%r11, %r12
	movq	%rcx, %r11
	andq	$-2097152, %rcx
	sarq	$21, %r11
	movq	%r11, %rbp
	movq	32(%rsp), %r11
	addq	%r11, %rbp
	subq	%rcx, %rsi
	leaq	1048576(%r15), %rcx
	movq	%rcx, %r11
	andq	$-2097152, %rcx
	movq	%rsi, -32(%rsp)
	sarq	$21, %r11
	subq	%rcx, %r15
	leaq	1048576(%r10), %rcx
	movq	%r11, %rbx
	movq	48(%rsp), %r11
	movq	%r15, -24(%rsp)
	addq	%r11, %rbx
	movq	%rcx, %r11
	andq	$-2097152, %rcx
	subq	%rcx, %r10
	sarq	$21, %r11
	movq	%r10, -16(%rsp)
	movq	72(%rsp), %rsi
	addq	%rdi, %r11
	movq	-80(%rsp), %r15
	movq	-104(%rsp), %r10
	leaq	1048576(%rsi), %rcx
	movq	-112(%rsp), %rdx
	movq	%rcx, %rdi
	andq	$-2097152, %rcx
	subq	%rcx, %rsi
	sarq	$21, %rdi
	leaq	1048576(%r15), %rcx
	addq	%r10, %rdi
	movq	%rcx, %r10
	andq	$-2097152, %rcx
	movq	%rsi, -104(%rsp)
	subq	%rcx, %r15
	sarq	$21, %r10
	movq	%r15, -80(%rsp)
	movq	%r10, %rsi
	movq	-120(%rsp), %r15
	movq	-72(%rsp), %r10
	addq	%r10, %rsi
	movq	%r15, %r10
	addq	$1048576, %r10
	movq	%r10, %rcx
	andq	$-2097152, %r10
	sarq	$21, %rcx
	subq	%r10, %r15
	addq	%rdx, %rcx
	movq	%r15, -72(%rsp)
	movq	-88(%rsp), %rdx
	movq	%rcx, -112(%rsp)
	movq	-96(%rsp), %rcx
	leaq	1048576(%rcx), %r15
	movq	%r15, %r10
	andq	$-2097152, %r15
	sarq	$21, %r10
	addq	%rdx, %r10
	movq	%rcx, %rdx
	movq	-64(%rsp), %rcx
	subq	%r15, %rdx
	movq	%rdx, -96(%rsp)
	leaq	1048576(%r9), %rdx
	movq	%rdx, %r15
	andq	$-2097152, %rdx
	subq	%rdx, %r9
	sarq	$21, %r15
	movq	%r9, -88(%rsp)
	leaq	1048576(%r8), %r9
	addq	%rcx, %r15
	movq	-56(%rsp), %rcx
	movq	%r9, %rdx
	sarq	$21, %rdx
	addq	%rcx, %rdx
	andq	$-2097152, %r9
	movq	-40(%rsp), %rcx
	subq	%r9, %r8
	leaq	1048576(%r14), %r9
	movq	%rdx, -120(%rsp)
	movq	%r8, -64(%rsp)
	movq	%r9, %r8
	andq	$-2097152, %r9
	subq	%r9, %r14
	leaq	1048576(%r13), %r9
	sarq	$21, %r8
	movq	%r9, %rdx
	andq	$-2097152, %r9
	movq	%r14, -56(%rsp)
	subq	%r9, %r13
	sarq	$21, %rdx
	leaq	1048576(%r12), %r9
	movq	%r13, -8(%rsp)
	leaq	(%rdx,%rcx), %rdx
	movq	%r9, %r13
	movq	-32(%rsp), %rcx
	andq	$-2097152, %r9
	sarq	$21, %r13
	movq	%rdx, -40(%rsp)
	subq	%r9, %r12
	leaq	1048576(%rbp), %r9
	addq	%rcx, %r13
	movq	-24(%rsp), %rcx
	movq	%r12, (%rsp)
	movq	%r9, %r12
	andq	$-2097152, %r9
	subq	%r9, %rbp
	sarq	$21, %r12
	leaq	1048576(%rbx), %r9
	movq	%r13, -32(%rsp)
	addq	%rcx, %r12
	movq	%rbp, 8(%rsp)
	movq	-16(%rsp), %rcx
	movq	%r9, %rbp
	sarq	$21, %rbp
	andq	$-2097152, %r9
	movq	%r12, -24(%rsp)
	subq	%r9, %rbx
	leaq	0(%rbp,%rcx), %r12
	leaq	1048576(%r11), %r9
	movq	-104(%rsp), %rcx
	movq	%r12, -16(%rsp)
	movq	%r9, %r12
	andq	$-2097152, %r9
	sarq	$21, %r12
	subq	%r9, %r11
	movq	%rbx, 16(%rsp)
	leaq	1048576(%rdi), %r9
	movq	%r12, %rbx
	movq	%r11, -104(%rsp)
	movq	%r9, %r13
	movq	-112(%rsp), %r11
	addq	%rcx, %rbx
	andq	$-2097152, %r9
	movq	-80(%rsp), %rcx
	sarq	$21, %r13
	subq	%r9, %rdi
	leaq	1048576(%rsi), %r9
	movq	-88(%rsp), %r14
	addq	%rcx, %r13
	movq	%r9, %r12
	movq	-72(%rsp), %rcx
	andq	$-2097152, %r9
	subq	%r9, %rsi
	sarq	$21, %r12
	leaq	1048576(%r11), %r9
	addq	%rcx, %r12
	movq	%r9, %rcx
	sarq	$21, %rcx
	movq	%rcx, %rbp
	movq	-96(%rsp), %rcx
	addq	%rcx, %rbp
	andq	$-2097152, %r9
	movq	%r11, %rcx
	subq	%r9, %rcx
	leaq	1048576(%r10), %r9
	movq	%r9, %r11
	andq	$-2097152, %r9
	subq	%r9, %r10
	leaq	1048576(%r15), %r9
	sarq	$21, %r11
	movq	%r9, %rdx
	addq	%r14, %r11
	movq	%r10, -112(%rsp)
	movq	-64(%rsp), %r14
	sarq	$21, %rdx
	andq	$-2097152, %r9
	movq	%rdx, %r10
	movq	-120(%rsp), %rdx
	subq	%r9, %r15
	addq	%r14, %r10
	leaq	1048576(%rdx), %r14
	movq	%r14, %rdx
	andq	$-2097152, %r14
	sarq	$21, %rdx
	movq	%rdx, %r9
	movq	-56(%rsp), %rdx
	addq	%rdx, %r9
	movq	-120(%rsp), %rdx
	subq	%r14, %rdx
	imulq	$666643, %r8, %r14
	addq	%r14, %rdi
	imulq	$470296, %r8, %r14
	addq	%r14, %r13
	imulq	$654183, %r8, %r14
	addq	%rsi, %r14
	imulq	$-997805, %r8, %rsi
	addq	%rsi, %r12
	imulq	$136657, %r8, %rsi
	imulq	$-683901, %r8, %r8
	addq	%rcx, %rsi
	imulq	$666643, %r9, %rcx
	addq	%rbp, %r8
	addq	%rbx, %rcx
	imulq	$470296, %r9, %rbx
	addq	%rbx, %rdi
	imulq	$654183, %r9, %rbx
	addq	%r13, %rbx
	imulq	$136657, %r9, %r13
	imulq	$-997805, %r9, %rbp
	imulq	$-683901, %r9, %r9
	addq	%r13, %r12
	imulq	$470296, %rdx, %r13
	addq	%r14, %rbp
	movq	-104(%rsp), %r14
	addq	%rsi, %r9
	imulq	$666643, %rdx, %rsi
	addq	%rcx, %r13
	imulq	$654183, %rdx, %rcx
	addq	%r14, %rsi
	movq	16(%rsp), %r14
	addq	%rcx, %rdi
	imulq	$-997805, %rdx, %rcx
	addq	%rcx, %rbx
	imulq	$136657, %rdx, %rcx
	imulq	$-683901, %rdx, %rdx
	addq	%rbp, %rcx
	imulq	$666643, %r10, %rbp
	addq	%r12, %rdx
	movq	-16(%rsp), %r12
	addq	%r12, %rbp
	imulq	$470296, %r10, %r12
	addq	%rsi, %r12
	imulq	$654183, %r10, %rsi
	addq	%rsi, %r13
	imulq	$-997805, %r10, %rsi
	addq	%rsi, %rdi
	imulq	$136657, %r10, %rsi
	imulq	$-683901, %r10, %r10
	addq	%rbx, %rsi
	addq	%rcx, %r10
	imulq	$666643, %r15, %rcx
	addq	%r14, %rcx
	imulq	$470296, %r15, %rbx
	movq	-24(%rsp), %r14
	addq	%rbp, %rbx
	imulq	$654183, %r15, %rbp
	addq	%r12, %rbp
	imulq	$-997805, %r15, %r12
	addq	%r13, %r12
	imulq	$136657, %r15, %r13
	imulq	$-683901, %r15, %r15
	addq	%r13, %rdi
	imulq	$470296, %r11, %r13
	addq	%rsi, %r15
	imulq	$666643, %r11, %rsi
	addq	%rcx, %r13
	imulq	$654183, %r11, %rcx
	addq	%r14, %rsi
	imulq	$136657, %r11, %r14
	addq	%rbx, %rcx
	imulq	$-997805, %r11, %rbx
	imulq	$-683901, %r11, %r11
	addq	%r12, %r14
	addq	%rbx, %rbp
	addq	%rdi, %r11
	leaq	1048576(%rsi), %rdi
	movq	%rdi, %rbx
	andq	$-2097152, %rdi
	subq	%rdi, %rsi
	sarq	$21, %rbx
	movq	%rsi, %r12
	leaq	1048576(%rcx), %rsi
	addq	%r13, %rbx
	movq	%rsi, %rdi
	andq	$-2097152, %rsi
	subq	%rsi, %rcx
	sarq	$21, %rdi
	movq	%rcx, %r13
	leaq	1048576(%r14), %rcx
	addq	%rdi, %rbp
	movq	%rcx, %rsi
	sarq	$21, %rsi
	addq	%rsi, %r11
	andq	$-2097152, %rcx
	subq	%rcx, %r14
	leaq	1048576(%r15), %rcx
	movq	%rcx, %rdi
	andq	$-2097152, %rcx
	subq	%rcx, %r15
	leaq	1048576(%rdx), %rcx
	sarq	$21, %rdi
	movq	%rcx, %rsi
	andq	$-2097152, %rcx
	addq	%r10, %rdi
	movq	-112(%rsp), %r10
	sarq	$21, %rsi
	subq	%rcx, %rdx
	addq	%r9, %rsi
	leaq	1048576(%r8), %r9
	movq	%r9, %rcx
	andq	$-2097152, %r9
	sarq	$21, %rcx
	subq	%r9, %r8
	addq	%r10, %rcx
	leaq	1048576(%rbx), %r10
	movq	%r10, %r9
	andq	$-2097152, %r10
	sarq	$21, %r9
	subq	%r10, %rbx
	addq	%r13, %r9
	leaq	1048576(%rbp), %r13
	movq	%r13, %r10
	andq	$-2097152, %r13
	sarq	$21, %r10
	subq	%r13, %rbp
	addq	%r14, %r10
	leaq	1048576(%r11), %r14
	movq	%r14, %r13
	andq	$-2097152, %r14
	subq	%r14, %r11
	sarq	$21, %r13
	leaq	1048576(%rdi), %r14
	addq	%r15, %r13
	movq	%r14, %r15
	andq	$-2097152, %r14
	subq	%r14, %rdi
	sarq	$21, %r15
	leaq	1048576(%rsi), %r14
	addq	%r15, %rdx
	movq	%r14, %r15
	sarq	$21, %r15
	addq	%r15, %r8
	andq	$-2097152, %r14
	movq	8(%rsp), %r15
	subq	%r14, %rsi
	imulq	$666643, %rcx, %r14
	addq	%r15, %r14
	imulq	$470296, %rcx, %r15
	addq	%r15, %r12
	imulq	$654183, %rcx, %r15
	addq	%r15, %rbx
	imulq	$-997805, %rcx, %r15
	addq	%r9, %r15
	imulq	$136657, %rcx, %r9
	imulq	$-683901, %rcx, %rcx
	addq	%rbp, %r9
	movq	-32(%rsp), %rbp
	addq	%r10, %rcx
	imulq	$666643, %r8, %r10
	addq	%rbp, %r10
	imulq	$470296, %r8, %rbp
	addq	%r14, %rbp
	imulq	$654183, %r8, %r14
	addq	%r14, %r12
	imulq	$-997805, %r8, %r14
	addq	%r14, %rbx
	imulq	$136657, %r8, %r14
	imulq	$-683901, %r8, %r8
	addq	%r15, %r14
	imulq	$666643, %rsi, %r15
	addq	%r9, %r8
	movq	(%rsp), %r9
	addq	%r9, %r15
	imulq	$470296, %rsi, %r9
	addq	%r9, %r10
	imulq	$654183, %rsi, %r9
	addq	%r9, %rbp
	imulq	$-997805, %rsi, %r9
	addq	%r9, %r12
	imulq	$136657, %rsi, %r9
	imulq	$-683901, %rsi, %rsi
	addq	%rbx, %r9
	imulq	$666643, %rdx, %rbx
	addq	%r14, %rsi
	movq	-40(%rsp), %r14
	addq	%r14, %rbx
	imulq	$470296, %rdx, %r14
	addq	%r15, %r14
	imulq	$654183, %rdx, %r15
	addq	%r10, %r15
	imulq	$-997805, %rdx, %r10
	addq	%r10, %rbp
	imulq	$136657, %rdx, %r10
	imulq	$-683901, %rdx, %rdx
	addq	%r12, %r10
	imulq	$666643, %rdi, %r12
	addq	%r9, %rdx
	movq	-8(%rsp), %r9
	addq	%r9, %r12
	imulq	$470296, %rdi, %r9
	addq	%r9, %rbx
	imulq	$654183, %rdi, %r9
	addq	%r9, %r14
	imulq	$-997805, %rdi, %r9
	addq	%r9, %r15
	imulq	$136657, %rdi, %r9
	imulq	$-683901, %rdi, %rdi
	addq	%rbp, %r9
	movq	-48(%rsp), %rbp
	addq	%r10, %rdi
	imulq	$666643, %r13, %r10
	addq	%rbp, %r10
	imulq	$470296, %r13, %rbp
	addq	%r12, %rbp
	imulq	$654183, %r13, %r12
	addq	%r12, %rbx
	imulq	$-997805, %r13, %r12
	addq	%r14, %r12
	imulq	$136657, %r13, %r14
	imulq	$-683901, %r13, %r13
	addq	%r15, %r14
	addq	%r9, %r13
	leaq	1048576(%r10), %r9
	movq	%r9, %r15
	andq	$-2097152, %r9
	sarq	$21, %r15
	subq	%r9, %r10
	leaq	1048576(%rbx), %r9
	addq	%r15, %rbp
	movq	%r9, %r15
	andq	$-2097152, %r9
	sarq	$21, %r15
	subq	%r9, %rbx
	leaq	1048576(%r14), %r9
	addq	%r15, %r12
	movq	%r9, %r15
	andq	$-2097152, %r9
	sarq	$21, %r15
	subq	%r9, %r14
	leaq	1048576(%rdi), %r9
	addq	%r15, %r13
	movq	%r9, %r15
	andq	$-2097152, %r9
	subq	%r9, %rdi
	sarq	$21, %r15
	leaq	1048576(%rsi), %r9
	addq	%rdx, %r15
	movq	%r9, %rdx
	andq	$-2097152, %r9
	sarq	$21, %rdx
	subq	%r9, %rsi
	addq	%r8, %rdx
	leaq	1048576(%rcx), %r8
	movq	%r8, %r9
	sarq	$21, %r9
	addq	%r11, %r9
	andq	$-2097152, %r8
	subq	%r8, %rcx
	leaq	1048576(%rbp), %r8
	movq	%r8, %r11
	andq	$-2097152, %r8
	subq	%r8, %rbp
	sarq	$21, %r11
	leaq	1048576(%r12), %r8
	addq	%r11, %rbx
	movq	%r8, %r11
	andq	$-2097152, %r8
	subq	%r8, %r12
	sarq	$21, %r11
	leaq	1048576(%r13), %r8
	addq	%r11, %r14
	movq	%r8, %r11
	andq	$-2097152, %r8
	subq	%r8, %r13
	sarq	$21, %r11
	leaq	1048576(%r15), %r8
	addq	%r11, %rdi
	movq	%r8, %r11
	andq	$-2097152, %r8
	subq	%r8, %r15
	sarq	$21, %r11
	leaq	1048576(%rdx), %r8
	addq	%r11, %rsi
	movq	%r8, %r11
	andq	$-2097152, %r8
	sarq	$21, %r11
	subq	%r8, %rdx
	addq	%r11, %rcx
	leaq	1048576(%r9), %r11
	movq	%r11, %r8
	andq	$-2097152, %r11
	sarq	$21, %r8
	subq	%r11, %r9
	imulq	$666643, %r8, %r11
	addq	%r11, %r10
	imulq	$470296, %r8, %r11
	addq	%rbp, %r11
	imulq	$654183, %r8, %rbp
	addq	%rbp, %rbx
	imulq	$-997805, %r8, %rbp
	addq	%r12, %rbp
	imulq	$136657, %r8, %r12
	imulq	$-683901, %r8, %r8
	addq	%r14, %r12
	addq	%r13, %r8
	movq	%r10, %r13
	sarq	$21, %r13
	addq	%r11, %r13
	movq	%r10, %r11
	movq	%r13, %r10
	andl	$2097151, %r11d
	sarq	$21, %r10
	addq	%rbx, %r10
	movq	%r13, %rbx
	movq	%r10, %r13
	andl	$2097151, %r10d
	andl	$2097151, %ebx
	sarq	$21, %r13
	addq	%rbp, %r13
	movq	%r10, %rbp
	movq	%r13, %r10
	sarq	$21, %r10
	addq	%r12, %r10
	movq	%r13, %r12
	movq	%r10, %r13
	andl	$2097151, %r12d
	sarq	$21, %r13
	addq	%r8, %r13
	movq	%r10, %r8
	movq	%r13, %r10
	andl	$2097151, %r8d
	sarq	$21, %r10
	addq	%rdi, %r10
	movq	%r13, %rdi
	movq	%r10, %r13
	andl	$2097151, %edi
	andl	$2097151, %r10d
	sarq	$21, %r13
	addq	%r15, %r13
	movq	%r13, %r14
	andl	$2097151, %r13d
	sarq	$21, %r14
	addq	%r14, %rsi
	movq	%rsi, %r14
	sarq	$21, %r14
	addq	%rdx, %r14
	andl	$2097151, %esi
	movq	%r14, %rdx
	andl	$2097151, %r14d
	sarq	$21, %rdx
	addq	%rcx, %rdx
	movq	%rdx, %rcx
	andl	$2097151, %edx
	sarq	$21, %rcx
	addq	%r9, %rcx
	movq	%rdx, %r9
	movq	%rcx, %r15
	andl	$2097151, %ecx
	sarq	$21, %r15
	imulq	$666643, %r15, %rdx
	addq	%r11, %rdx
	imulq	$470296, %r15, %r11
	addq	%rbx, %r11
	imulq	$654183, %r15, %rbx
	addq	%rbx, %rbp
	imulq	$-997805, %r15, %rbx
	addq	%r12, %rbx
	imulq	$136657, %r15, %r12
	imulq	$-683901, %r15, %r15
	addq	%r8, %r12
	movq	%rdx, %r8
	andl	$2097151, %edx
	sarq	$21, %r8
	addq	%r15, %rdi
	movq	%rdx, %r15
	addq	%r8, %r11
	movw	%r15w, (%rax)
	movq	%r11, %rdx
	andl	$2097151, %r11d
	sarq	$21, %rdx
	addq	%rbp, %rdx
	movq	%rdx, %rbp
	sarq	$21, %rbp
	addq	%rbx, %rbp
	movq	%rdx, %rbx
	movq	%rbp, %r8
	andl	$2097151, %ebx
	sarq	$21, %r8
	addq	%r12, %r8
	andl	$2097151, %ebp
	sarq	$16, %r15
	movq	%r8, %rdx
	andl	$2097151, %r8d
	sarq	$21, %rdx
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	andl	$2097151, %edx
	sarq	$21, %rdi
	addq	%r10, %rdi
	movq	%rdx, %r10
	movq	%rdi, %r12
	andl	$2097151, %edi
	sarq	$21, %r12
	addq	%r13, %r12
	movq	%r12, %rdx
	andl	$2097151, %r12d
	sarq	$21, %rdx
	addq	%rdx, %rsi
	movq	%rsi, %rdx
	andl	$2097151, %esi
	sarq	$21, %rdx
	addq	%r14, %rdx
	movq	%rdx, %r13
	andl	$2097151, %edx
	sarq	$21, %r13
	addq	%r9, %r13
	movq	%r13, %r9
	sarq	$21, %r9
	addq	%rcx, %r9
	movq	%r13, %rcx
	movq	%r11, %r13
	salq	$5, %r13
	andl	$2097151, %ecx
	orl	%r13d, %r15d
	movq	%r11, %r13
	sarq	$3, %r13
	movb	%r15b, 2(%rax)
	movb	%r13b, 3(%rax)
	movq	%r11, %r13
	sarq	$19, %r11
	sarq	$11, %r13
	movb	%r13b, 4(%rax)
	leaq	0(,%rbx,4), %r13
	orl	%r13d, %r11d
	movb	%r11b, 5(%rax)
	movq	%rbx, %r11
	sarq	$14, %rbx
	sarq	$6, %r11
	movb	%r11b, 6(%rax)
	movq	%rbp, %r11
	salq	$7, %r11
	orl	%r11d, %ebx
	movq	%rbp, %r11
	sarq	%r11
	movb	%bl, 7(%rax)
	movb	%r11b, 8(%rax)
	movq	%rbp, %r11
	sarq	$17, %rbp
	sarq	$9, %r11
	movb	%r11b, 9(%rax)
	movq	%r8, %r11
	salq	$4, %r11
	orl	%r11d, %ebp
	movq	%r8, %r11
	sarq	$4, %r11
	movb	%bpl, 10(%rax)
	movb	%r11b, 11(%rax)
	movq	%r8, %r11
	sarq	$20, %r8
	sarq	$12, %r11
	movb	%r11b, 12(%rax)
	leaq	(%r10,%r10), %r11
	orl	%r11d, %r8d
	movw	%si, 21(%rax)
	sarq	$16, %rsi
	movb	%r8b, 13(%rax)
	movq	%r10, %r8
	sarq	$15, %r10
	sarq	$7, %r8
	movb	%r8b, 14(%rax)
	movq	%rdi, %r8
	salq	$6, %r8
	orl	%r8d, %r10d
	movq	%rdi, %r8
	sarq	$2, %r8
	movb	%r10b, 15(%rax)
	movb	%r8b, 16(%rax)
	movq	%rdi, %r8
	sarq	$18, %rdi
	sarq	$10, %r8
	movb	%r8b, 17(%rax)
	leaq	0(,%r12,8), %r8
	orl	%r8d, %edi
	movb	%dil, 18(%rax)
	movq	%r12, %rdi
	sarq	$13, %r12
	sarq	$5, %rdi
	movb	%r12b, 20(%rax)
	movb	%dil, 19(%rax)
	movq	%rdx, %rdi
	salq	$5, %rdi
	orl	%edi, %esi
	movb	%sil, 23(%rax)
	movq	%rdx, %rsi
	sarq	$3, %rsi
	movb	%sil, 24(%rax)
	movq	%rdx, %rsi
	sarq	$19, %rdx
	sarq	$11, %rsi
	movb	%sil, 25(%rax)
	leaq	0(,%rcx,4), %rsi
	orl	%esi, %edx
	movb	%dl, 26(%rax)
	movq	%rcx, %rdx
	sarq	$14, %rcx
	sarq	$6, %rdx
	movb	%dl, 27(%rax)
	movq	%r9, %rdx
	salq	$7, %rdx
	orl	%edx, %ecx
	movq	%r9, %rdx
	sarq	%rdx
	movb	%cl, 28(%rax)
	movb	%dl, 29(%rax)
	movq	%r9, %rdx
	sarq	$9, %rdx
	sarq	$17, %r9
	movb	%dl, 30(%rax)
	movb	%r9b, 31(%rax)
	addq	$80, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE84:
	.size	sc_muladd, .-sc_muladd
	.p2align 4
	.type	sha512_block_data_order, @function
sha512_block_data_order:
.LFB47:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	salq	$7, %rdx
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	movq	%rsi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$72, %rsp
	.cfi_def_cfa_offset 128
	movq	32(%rdi), %rcx
	movq	56(%rdi), %rax
	movq	%rdi, 64(%rsp)
	movq	(%rdi), %r13
	movq	%rcx, 8(%rsp)
	movq	40(%rdi), %rcx
	movq	%rax, 32(%rsp)
	leaq	(%rsi,%rdx), %rax
	movq	%rcx, 16(%rsp)
	movq	48(%rdi), %rcx
	movq	%rax, 56(%rsp)
	movq	%rcx, 24(%rsp)
	movq	8(%rdi), %rcx
	movq	%rcx, -16(%rsp)
	movq	16(%rdi), %rcx
	movq	%rcx, -8(%rsp)
	movq	24(%rdi), %rcx
	movq	%rcx, (%rsp)
	.p2align 4,,10
	.p2align 3
.L8:
	movq	8(%rsp), %rdi
	movq	(%r12), %rax
	movq	16(%rsp), %rbx
	movq	24(%rsp), %r10
	bswap	%rax
	movq	%rdi, %rcx
	movq	%rax, %rsi
	movq	%rdi, %rdx
	movq	%rax, -40(%rsp)
	movq	%rdi, %rax
	rorq	$14, %rcx
	andq	%rbx, %rdx
	rorq	$18, %rax
	movq	-16(%rsp), %r14
	movq	-8(%rsp), %r15
	xorq	%rax, %rcx
	movq	%rdi, %rax
	rolq	$23, %rax
	xorq	%rax, %rcx
	movq	32(%rsp), %rax
	addq	%rsi, %rax
	movabsq	$4794697086780616226, %rsi
	addq	%rsi, %rax
	movq	%r14, %rsi
	addq	%rax, %rcx
	movq	%rdi, %rax
	andq	%r15, %rsi
	notq	%rax
	andq	%r10, %rax
	xorq	%rdx, %rax
	movq	%r13, %rdx
	addq	%rax, %rcx
	movq	%r13, %rax
	rolq	$30, %rdx
	rorq	$28, %rax
	xorq	%rdx, %rax
	movq	%r13, %rdx
	rolq	$25, %rdx
	xorq	%rdx, %rax
	movq	%r14, %rdx
	xorq	%r15, %rdx
	andq	%r13, %rdx
	xorq	%rsi, %rdx
	movq	(%rsp), %rsi
	addq	%rdx, %rax
	leaq	(%rcx,%rsi), %r9
	addq	%rax, %rcx
	movq	8(%r12), %rax
	movq	%r9, %rdx
	movq	%rcx, %r8
	bswap	%rax
	movq	%rax, %rsi
	movq	%rax, -120(%rsp)
	movq	%r9, %rax
	rorq	$14, %rax
	rorq	$18, %rdx
	xorq	%rax, %rdx
	movq	%r9, %rax
	rolq	$23, %rax
	xorq	%rax, %rdx
	leaq	(%rsi,%r10), %rax
	movq	%rbx, %r10
	movabsq	$8158064640168781261, %rsi
	addq	%rsi, %rax
	movq	%rdi, %rsi
	addq	%rax, %rdx
	movq	%r9, %rax
	andq	%r9, %rsi
	notq	%rax
	andq	%rbx, %rax
	movq	%rdi, %rbx
	movq	%r14, %rdi
	xorq	%rsi, %rax
	movq	%rcx, %rsi
	andq	%r13, %rdi
	andq	%r13, %r8
	addq	%rax, %rdx
	movq	%rcx, %rax
	rolq	$30, %rsi
	rorq	$28, %rax
	leaq	(%rdx,%r15), %r11
	movabsq	$4131703408338449720, %r15
	xorq	%rax, %rsi
	movq	%rcx, %rax
	rolq	$25, %rax
	xorq	%rax, %rsi
	movq	%r14, %rax
	xorq	%r13, %rax
	andq	%rcx, %rax
	xorq	%rdi, %rax
	addq	%rsi, %rax
	movq	%r11, %rsi
	addq	%rax, %rdx
	movq	16(%r12), %rax
	rorq	$18, %rsi
	movq	%rdx, %rbp
	bswap	%rax
	movq	%rax, %rdi
	movq	%rax, -112(%rsp)
	movq	%r11, %rax
	rorq	$14, %rax
	xorq	%rax, %rsi
	movq	%r11, %rax
	rolq	$23, %rax
	xorq	%rax, %rsi
	leaq	(%rdi,%r10), %rax
	movabsq	$-5349999486874862801, %rdi
	addq	%rdi, %rax
	movq	%r9, %rdi
	addq	%rax, %rsi
	movq	%r11, %rax
	andq	%r11, %rdi
	notq	%rax
	andq	%rbx, %rax
	xorq	%rdi, %rax
	movq	%rdx, %rdi
	addq	%rax, %rsi
	movq	%rdx, %rax
	rolq	$30, %rdi
	rorq	$28, %rax
	leaq	(%rsi,%r14), %r10
	xorq	%rax, %rdi
	movq	%rdx, %rax
	rolq	$25, %rax
	xorq	%rax, %rdi
	movq	%rcx, %rax
	xorq	%r13, %rax
	andq	%rdx, %rax
	xorq	%r8, %rax
	movq	%r11, %r8
	addq	%rdi, %rax
	movq	%r10, %rdi
	andq	%r10, %r8
	addq	%rax, %rsi
	movq	24(%r12), %rax
	rorq	$18, %rdi
	bswap	%rax
	movq	%rax, %r14
	movq	%rax, -104(%rsp)
	movq	%r10, %rax
	rorq	$14, %rax
	xorq	%rax, %rdi
	movq	%r10, %rax
	rolq	$23, %rax
	xorq	%rax, %rdi
	leaq	(%r14,%rbx), %rax
	movq	32(%r12), %r14
	movabsq	$-1606136188198331460, %rbx
	addq	%rbx, %rax
	movq	%rcx, %rbx
	addq	%rax, %rdi
	movq	%r10, %rax
	andq	%rdx, %rbx
	bswap	%r14
	notq	%rax
	movq	%r14, -96(%rsp)
	andq	%r9, %rax
	xorq	%r8, %rax
	movq	%rsi, %r8
	addq	%rax, %rdi
	movq	%rsi, %rax
	rolq	$30, %r8
	rorq	$28, %rax
	xorq	%rax, %r8
	movq	%rsi, %rax
	rolq	$25, %rax
	xorq	%rax, %r8
	movq	%rcx, %rax
	xorq	%rdx, %rax
	andq	%rsi, %rax
	xorq	%rbx, %rax
	addq	%rax, %r8
	leaq	(%rdi,%r13), %rax
	movq	%rax, %rbx
	addq	%r8, %rdi
	movq	%rax, %r8
	rorq	$14, %rbx
	rorq	$18, %r8
	xorq	%rbx, %r8
	movq	%rax, %rbx
	rolq	$23, %rbx
	xorq	%r8, %rbx
	leaq	(%r14,%r15), %r8
	movq	40(%r12), %r14
	movabsq	$6480981068601479193, %r15
	addq	%r9, %r8
	movq	%rax, %r9
	addq	%rbx, %r8
	notq	%r9
	movq	%r10, %rbx
	bswap	%r14
	andq	%r11, %r9
	andq	%rax, %rbx
	andq	%rsi, %rbp
	movq	%r14, -88(%rsp)
	xorq	%rbx, %r9
	movq	%rdi, %rbx
	addq	%r9, %r8
	movq	%rdi, %r9
	rolq	$30, %rbx
	rorq	$28, %r9
	addq	%r8, %rcx
	xorq	%r9, %rbx
	movq	%rdi, %r9
	rolq	$25, %r9
	xorq	%r9, %rbx
	movq	%rdx, %r9
	xorq	%rsi, %r9
	andq	%rdi, %r9
	xorq	%rbp, %r9
	movq	%rsi, %rbp
	addq	%rbx, %r9
	movq	%rcx, %rbx
	addq	%r9, %r8
	movq	%rcx, %r9
	rorq	$14, %rbx
	rorq	$18, %r9
	xorq	%rbx, %r9
	movq	%rcx, %rbx
	rolq	$23, %rbx
	xorq	%r9, %rbx
	leaq	(%r14,%r15), %r9
	movq	48(%r12), %r14
	movabsq	$-7908458776815382629, %r15
	addq	%r11, %r9
	movq	%rcx, %r11
	addq	%rbx, %r9
	notq	%r11
	movq	%rax, %rbx
	bswap	%r14
	andq	%rcx, %rbx
	andq	%r10, %r11
	movq	%r14, -80(%rsp)
	xorq	%rbx, %r11
	movq	%r8, %rbx
	addq	%r11, %r9
	movq	%r8, %r11
	rolq	$30, %rbx
	rorq	$28, %r11
	xorq	%r11, %rbx
	movq	%r8, %r11
	rolq	$25, %r11
	xorq	%r11, %rbx
	movq	%rsi, %r11
	xorq	%rdi, %r11
	andq	%rdi, %rbp
	addq	%r9, %rdx
	andq	%r8, %r11
	xorq	%rbp, %r11
	movq	%rdi, %rbp
	addq	%rbx, %r11
	movq	%rdx, %rbx
	andq	%r8, %rbp
	addq	%r11, %r9
	movq	%rdx, %r11
	rorq	$14, %rbx
	rorq	$18, %r11
	xorq	%rbx, %r11
	movq	%rdx, %rbx
	rolq	$23, %rbx
	xorq	%r11, %rbx
	leaq	(%r14,%r15), %r11
	movq	56(%r12), %r14
	movabsq	$-6116909921290321640, %r15
	addq	%r10, %r11
	movq	%rdx, %r10
	addq	%rbx, %r11
	notq	%r10
	movq	%rcx, %rbx
	bswap	%r14
	andq	%rdx, %rbx
	andq	%rax, %r10
	movq	%r14, -72(%rsp)
	xorq	%rbx, %r10
	movq	%r9, %rbx
	addq	%r10, %r11
	movq	%r9, %r10
	rolq	$30, %rbx
	rorq	$28, %r10
	addq	%r11, %rsi
	xorq	%r10, %rbx
	movq	%r9, %r10
	rolq	$25, %r10
	xorq	%r10, %rbx
	movq	%rdi, %r10
	xorq	%r8, %r10
	andq	%r9, %r10
	xorq	%rbp, %r10
	movq	%r8, %rbp
	addq	%rbx, %r10
	movq	%rsi, %rbx
	addq	%r10, %r11
	movq	%rsi, %r10
	rorq	$14, %rbx
	rorq	$18, %r10
	xorq	%rbx, %r10
	movq	%rsi, %rbx
	andq	%r9, %rbp
	rolq	$23, %rbx
	xorq	%r10, %rbx
	leaq	(%r14,%r15), %r10
	addq	%rax, %r10
	movq	%rsi, %rax
	addq	%rbx, %r10
	notq	%rax
	movq	%rdx, %rbx
	andq	%rsi, %rbx
	andq	%rcx, %rax
	xorq	%rbx, %rax
	movq	%r11, %rbx
	addq	%rax, %r10
	movq	%r11, %rax
	rolq	$30, %rbx
	rorq	$28, %rax
	addq	%r10, %rdi
	xorq	%rax, %rbx
	movq	%r11, %rax
	rolq	$25, %rax
	xorq	%rax, %rbx
	movq	%r8, %rax
	xorq	%r9, %rax
	andq	%r11, %rax
	xorq	%rbp, %rax
	movq	%r9, %rbp
	addq	%rbx, %rax
	movq	%rdi, %rbx
	addq	%rax, %r10
	movq	64(%r12), %rax
	rorq	$14, %rbx
	bswap	%rax
	movq	%rax, %r15
	movq	%rax, -64(%rsp)
	movq	%rdi, %rax
	rorq	$18, %rax
	xorq	%rbx, %rax
	movq	%rdi, %rbx
	rolq	$23, %rbx
	xorq	%rax, %rbx
	movabsq	$-2880145864133508542, %rax
	addq	%r15, %rax
	addq	%rcx, %rax
	movq	%rdi, %rcx
	addq	%rbx, %rax
	notq	%rcx
	movq	%rsi, %rbx
	andq	%rdi, %rbx
	andq	%rdx, %rcx
	xorq	%rbx, %rcx
	movq	%r10, %rbx
	addq	%rcx, %rax
	movq	%r10, %rcx
	rolq	$30, %rbx
	andq	%r11, %rbp
	rorq	$28, %rcx
	addq	%rax, %r8
	xorq	%rcx, %rbx
	movq	%r10, %rcx
	rolq	$25, %rcx
	xorq	%rcx, %rbx
	movq	%r9, %rcx
	xorq	%r11, %rcx
	andq	%r10, %rcx
	xorq	%rbp, %rcx
	movq	%r11, %rbp
	addq	%rbx, %rcx
	movq	%r8, %rbx
	addq	%rcx, %rax
	movq	72(%r12), %rcx
	rorq	$14, %rbx
	bswap	%rcx
	movq	%rcx, %r14
	movq	%rcx, -56(%rsp)
	movq	%r8, %rcx
	rorq	$18, %rcx
	xorq	%rbx, %rcx
	movq	%r8, %rbx
	rolq	$23, %rbx
	xorq	%rcx, %rbx
	movabsq	$1334009975649890238, %rcx
	addq	%r14, %rcx
	addq	%rdx, %rcx
	movq	%r8, %rdx
	addq	%rbx, %rcx
	notq	%rdx
	movq	%rdi, %rbx
	andq	%r8, %rbx
	andq	%rsi, %rdx
	xorq	%rbx, %rdx
	movq	%rax, %rbx
	addq	%rdx, %rcx
	movq	%rax, %rdx
	rolq	$30, %rbx
	rorq	$28, %rdx
	xorq	%rdx, %rbx
	movq	%rax, %rdx
	rolq	$25, %rdx
	xorq	%rdx, %rbx
	movq	%r11, %rdx
	xorq	%r10, %rdx
	andq	%rax, %rdx
	andq	%r10, %rbp
	addq	%rcx, %r9
	xorq	%rbp, %rdx
	movq	%r10, %rbp
	addq	%rbx, %rdx
	movq	%r9, %rbx
	andq	%rax, %rbp
	addq	%rdx, %rcx
	movq	80(%r12), %rdx
	rorq	$18, %rbx
	bswap	%rdx
	movq	%rdx, %r15
	movq	%rdx, -48(%rsp)
	movq	%r9, %rdx
	rorq	$14, %rdx
	xorq	%rdx, %rbx
	movq	%r9, %rdx
	rolq	$23, %rdx
	xorq	%rdx, %rbx
	movabsq	$2608012711638119052, %rdx
	addq	%r15, %rdx
	addq	%rsi, %rdx
	movq	%r9, %rsi
	addq	%rbx, %rdx
	notq	%rsi
	movq	%r8, %rbx
	andq	%r9, %rbx
	andq	%rdi, %rsi
	xorq	%rbx, %rsi
	movq	%rcx, %rbx
	addq	%rsi, %rdx
	movq	%rcx, %rsi
	rolq	$30, %rbx
	rorq	$28, %rsi
	addq	%rdx, %r11
	xorq	%rsi, %rbx
	movq	%rcx, %rsi
	rolq	$25, %rsi
	xorq	%rsi, %rbx
	movq	%r10, %rsi
	xorq	%rax, %rsi
	andq	%rcx, %rsi
	xorq	%rbp, %rsi
	movq	%rax, %rbp
	addq	%rbx, %rsi
	movq	%r11, %rbx
	addq	%rsi, %rdx
	movq	88(%r12), %rsi
	rorq	$18, %rbx
	bswap	%rsi
	movq	%rsi, %r14
	movq	%rsi, -32(%rsp)
	movq	%r11, %rsi
	rorq	$14, %rsi
	xorq	%rsi, %rbx
	movq	%r11, %rsi
	rolq	$23, %rsi
	andq	%rcx, %rbp
	xorq	%rsi, %rbx
	movabsq	$6128411473006802146, %rsi
	addq	%r14, %rsi
	addq	%rdi, %rsi
	movq	%r11, %rdi
	addq	%rbx, %rsi
	notq	%rdi
	movq	%r9, %rbx
	andq	%r11, %rbx
	andq	%r8, %rdi
	xorq	%rbx, %rdi
	movq	%rdx, %rbx
	addq	%rdi, %rsi
	movq	%rdx, %rdi
	rolq	$30, %rbx
	rorq	$28, %rdi
	addq	%rsi, %r10
	xorq	%rdi, %rbx
	movq	%rdx, %rdi
	rolq	$25, %rdi
	xorq	%rdi, %rbx
	movq	%rax, %rdi
	xorq	%rcx, %rdi
	andq	%rdx, %rdi
	xorq	%rbp, %rdi
	movq	%rcx, %rbp
	addq	%rbx, %rdi
	movq	%r10, %rbx
	addq	%rdi, %rsi
	movq	96(%r12), %rdi
	rorq	$18, %rbx
	bswap	%rdi
	movq	%rdi, -24(%rsp)
	movq	%rdi, %r15
	movq	%r10, %rdi
	rorq	$14, %rdi
	movq	104(%r12), %r14
	movq	%r13, 48(%rsp)
	xorq	%rdi, %rbx
	movq	%r10, %rdi
	rolq	$23, %rdi
	bswap	%r14
	movq	%r14, 40(%rsp)
	movq	40(%rsp), %r13
	xorq	%rdi, %rbx
	movq	%r12, 40(%rsp)
	movabsq	$8268148722764581231, %rdi
	addq	%r15, %rdi
	movq	112(%r12), %r15
	addq	%r8, %rdi
	movq	%r10, %r8
	addq	%rbx, %rdi
	notq	%r8
	movq	%r11, %rbx
	bswap	%r15
	andq	%r10, %rbx
	andq	%r9, %r8
	xorq	%rbx, %r8
	movq	%rsi, %rbx
	addq	%r8, %rdi
	movq	%rsi, %r8
	rolq	$30, %rbx
	andq	%rdx, %rbp
	rorq	$28, %r8
	addq	%rdi, %rax
	xorq	%r8, %rbx
	movq	%rsi, %r8
	rolq	$25, %r8
	xorq	%r8, %rbx
	movq	%rcx, %r8
	xorq	%rdx, %r8
	andq	%rsi, %r8
	xorq	%rbp, %r8
	movq	%rdx, %rbp
	addq	%rbx, %r8
	movq	%rax, %rbx
	addq	%r8, %rdi
	movq	%rax, %r8
	rorq	$18, %rbx
	rorq	$14, %r8
	xorq	%r8, %rbx
	movq	%rax, %r8
	rolq	$23, %r8
	xorq	%r8, %rbx
	movabsq	$-9160688886553864527, %r8
	addq	%r14, %r8
	addq	%r9, %r8
	movq	%rax, %r9
	addq	%rbx, %r8
	notq	%r9
	movq	%r10, %rbx
	andq	%r11, %r9
	andq	%rax, %rbx
	xorq	%rbx, %r9
	movq	%rdi, %rbx
	addq	%r9, %r8
	movq	%rdi, %r9
	rolq	$30, %rbx
	rorq	$28, %r9
	xorq	%r9, %rbx
	movq	%rdi, %r9
	rolq	$25, %r9
	xorq	%r9, %rbx
	movq	%rdx, %r9
	xorq	%rsi, %r9
	andq	%rdi, %r9
	andq	%rsi, %rbp
	addq	%r8, %rcx
	xorq	%rbp, %r9
	movq	%rsi, %rbp
	addq	%rbx, %r9
	movq	%rcx, %rbx
	andq	%rdi, %rbp
	addq	%r9, %r8
	movq	%rcx, %r9
	rorq	$18, %rbx
	rorq	$14, %r9
	xorq	%r9, %rbx
	movq	%rcx, %r9
	rolq	$23, %r9
	xorq	%r9, %rbx
	movabsq	$-7215885187991268811, %r9
	addq	%r15, %r9
	addq	%r11, %r9
	movq	%rcx, %r11
	addq	%rbx, %r9
	notq	%r11
	movq	%rax, %rbx
	andq	%r10, %r11
	andq	%rcx, %rbx
	xorq	%rbx, %r11
	movq	%r8, %rbx
	addq	%r11, %r9
	movq	%r8, %r11
	rolq	$30, %rbx
	rorq	$28, %r11
	addq	%r9, %rdx
	xorq	%r11, %rbx
	movq	%r8, %r11
	rolq	$25, %r11
	xorq	%r11, %rbx
	movq	%rsi, %r11
	xorq	%rdi, %r11
	andq	%r8, %r11
	xorq	%rbp, %r11
	movq	%rdi, %rbp
	addq	%rbx, %r11
	movq	%rdx, %rbx
	addq	%r11, %r9
	movq	120(%r12), %r11
	rorq	$18, %rbx
	bswap	%r11
	movq	%r11, %r14
	movq	%rdx, %r11
	rorq	$14, %r11
	xorq	%r11, %rbx
	movq	%rdx, %r11
	rolq	$23, %r11
	andq	%r8, %rbp
	xorq	%r11, %rbx
	movabsq	$-4495734319001033068, %r11
	addq	%r14, %r11
	addq	%r10, %r11
	movq	%rdx, %r10
	addq	%rbx, %r11
	notq	%r10
	movq	%rcx, %rbx
	andq	%rdx, %rbx
	andq	%rax, %r10
	xorq	%rbx, %r10
	movq	%r9, %rbx
	addq	%r10, %r11
	movq	%r9, %r10
	rolq	$30, %rbx
	rorq	$28, %r10
	addq	%r11, %rsi
	xorq	%r10, %rbx
	movq	%r9, %r10
	rolq	$25, %r10
	xorq	%r10, %rbx
	movq	%rdi, %r10
	xorq	%r8, %r10
	andq	%r9, %r10
	xorq	%rbp, %r10
	addq	%rbx, %r10
	addq	%r10, %r11
	movl	$K512, %r10d
.L7:
	movq	-120(%rsp), %r12
	movq	%r12, %rbp
	movq	%r12, %rbx
	rorq	$8, %rbp
	rorq	%rbx
	xorq	%rbp, %rbx
	movq	%r12, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %rbx
	movq	%r15, %rbp
	rolq	$3, %rbp
	movq	%rbp, %r12
	movq	%r15, %rbp
	rorq	$19, %rbp
	xorq	%r12, %rbp
	movq	%r15, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	movq	%rsi, %r12
	addq	%rbp, %rbx
	movq	-56(%rsp), %rbp
	rolq	$23, %r12
	addq	%rbp, %rbx
	addq	%rbx, -40(%rsp)
	movq	%rsi, %rbx
	rorq	$18, %rbx
	movq	%rbx, %rbp
	movq	%rsi, %rbx
	rorq	$14, %rbx
	xorq	%rbp, %rbx
	xorq	%r12, %rbx
	movq	%rsi, %r12
	notq	%r12
	movq	%r12, %rbp
	movq	%rsi, %r12
	andq	%rdx, %r12
	andq	%rcx, %rbp
	xorq	%r12, %rbp
	movq	%r11, %r12
	addq	%rbp, %rbx
	movq	-40(%rsp), %rbp
	addq	128(%r10), %rbx
	rorq	$28, %r12
	addq	%rbp, %rbx
	movq	%r12, %rbp
	movq	%r9, %r12
	addq	%rbx, %rax
	movq	%r11, %rbx
	rolq	$30, %rbx
	xorq	%rbx, %rbp
	movq	%r11, %rbx
	rolq	$25, %rbx
	xorq	%rbx, %rbp
	movq	%r9, %rbx
	xorq	%r8, %rbx
	andq	%r11, %rbx
	andq	%r8, %r12
	addq	%rax, %rdi
	addq	136(%r10), %rcx
	xorq	%r12, %rbx
	addq	%rbp, %rbx
	addq	%rbx, %rax
	movq	-112(%rsp), %rbx
	movq	%rbx, %r12
	rorq	$8, %r12
	movq	%r12, %rbp
	movq	%rbx, %r12
	rorq	%rbx
	xorq	%rbp, %rbx
	movq	%r12, %rbp
	movq	-48(%rsp), %r12
	shrq	$7, %rbp
	xorq	%rbp, %rbx
	movq	%r14, %rbp
	addq	%r12, %rbx
	movq	-120(%rsp), %r12
	rorq	$19, %rbp
	addq	%r12, %rbx
	movq	%r14, %r12
	rolq	$3, %r12
	xorq	%r12, %rbp
	movq	%r14, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	leaq	(%rbx,%rbp), %r12
	movq	%rdi, %rbx
	rorq	$18, %rbx
	movq	%r12, -120(%rsp)
	movq	%rdi, %r12
	movq	%rbx, %rbp
	movq	%rdi, %rbx
	rolq	$23, %r12
	rorq	$14, %rbx
	xorq	%rbp, %rbx
	movq	%r11, %rbp
	xorq	%rbx, %r12
	movq	-120(%rsp), %rbx
	addq	%rbx, %rcx
	movq	%rdi, %rbx
	addq	%rcx, %r12
	notq	%rbx
	movq	%rdi, %rcx
	andq	%rsi, %rcx
	andq	%rdx, %rbx
	xorq	%rcx, %rbx
	movq	%rax, %rcx
	addq	%rbx, %r12
	movq	%rax, %rbx
	rorq	$28, %rcx
	rolq	$30, %rbx
	xorq	%rcx, %rbx
	movq	%rax, %rcx
	rolq	$25, %rcx
	andq	%r9, %rbp
	addq	%r12, %r8
	addq	144(%r10), %rdx
	xorq	%rcx, %rbx
	movq	%r11, %rcx
	xorq	%r9, %rcx
	andq	%rax, %rcx
	xorq	%rbp, %rcx
	movq	-104(%rsp), %rbp
	addq	%rbx, %rcx
	addq	%rcx, %r12
	movq	%rbp, %rbx
	movq	%rbp, %rcx
	rorq	%rbx
	rorq	$8, %rcx
	xorq	%rbx, %rcx
	movq	%rbp, %rbx
	movq	-112(%rsp), %rbp
	shrq	$7, %rbx
	xorq	%rbx, %rcx
	movq	-32(%rsp), %rbx
	addq	%rbx, %rcx
	movq	-40(%rsp), %rbx
	addq	%rbp, %rcx
	movq	%rbx, %rbp
	rolq	$3, %rbx
	rorq	$19, %rbp
	xorq	%rbp, %rbx
	movq	-40(%rsp), %rbp
	shrq	$6, %rbp
	xorq	%rbp, %rbx
	movq	%r8, %rbp
	addq	%rbx, %rcx
	rorq	$14, %rbp
	movq	%rax, %rbx
	movq	%rcx, -112(%rsp)
	movq	%r8, %rcx
	rorq	$18, %rcx
	xorq	%rbp, %rcx
	movq	%r8, %rbp
	rolq	$23, %rbp
	xorq	%rcx, %rbp
	movq	-112(%rsp), %rcx
	addq	%rcx, %rdx
	addq	%rdx, %rbp
	movq	%r8, %rdx
	notq	%rdx
	movq	%rdx, %rcx
	movq	%rdi, %rdx
	andq	%r8, %rdx
	andq	%rsi, %rcx
	xorq	%rdx, %rcx
	movq	%r12, %rdx
	andq	%r11, %rbx
	addq	%rcx, %rbp
	movq	%r12, %rcx
	rorq	$28, %rdx
	rolq	$30, %rcx
	addq	%rbp, %r9
	xorq	%rdx, %rcx
	movq	%r12, %rdx
	rolq	$25, %rdx
	xorq	%rdx, %rcx
	movq	%rax, %rdx
	xorq	%r11, %rdx
	andq	%r12, %rdx
	xorq	%rbx, %rdx
	movq	-96(%rsp), %rbx
	addq	%rcx, %rdx
	addq	%rdx, %rbp
	movq	%rbx, %rcx
	movq	%rbx, %rdx
	rorq	%rcx
	rorq	$8, %rdx
	xorq	%rcx, %rdx
	movq	%rbx, %rcx
	movq	-24(%rsp), %rbx
	shrq	$7, %rcx
	xorq	%rcx, %rdx
	movq	-120(%rsp), %rcx
	addq	%rbx, %rdx
	movq	-104(%rsp), %rbx
	addq	%rbx, %rdx
	movq	%rcx, %rbx
	rolq	$3, %rcx
	rorq	$19, %rbx
	xorq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	shrq	$6, %rbx
	xorq	%rbx, %rcx
	movq	%r9, %rbx
	addq	%rcx, %rdx
	rolq	$23, %rbx
	movq	%rdx, -104(%rsp)
	movq	%r9, %rdx
	rorq	$14, %rdx
	movq	%rdx, %rcx
	movq	%r9, %rdx
	rorq	$18, %rdx
	xorq	%rcx, %rdx
	xorq	%rdx, %rbx
	movq	-104(%rsp), %rdx
	addq	152(%r10), %rsi
	addq	%rdx, %rsi
	movq	%r9, %rdx
	addq	%rsi, %rbx
	notq	%rdx
	movq	%r8, %rsi
	andq	%r9, %rsi
	andq	%rdi, %rdx
	xorq	%rsi, %rdx
	movq	%rbp, %rsi
	addq	%rdx, %rbx
	rolq	$30, %rsi
	movq	%rbp, %rdx
	rorq	$28, %rdx
	movq	%rsi, %rcx
	movq	%rax, %rsi
	addq	%rbx, %r11
	xorq	%rdx, %rcx
	movq	%rbp, %rdx
	andq	%r12, %rsi
	rolq	$25, %rdx
	xorq	%rdx, %rcx
	movq	%rax, %rdx
	xorq	%r12, %rdx
	andq	%rbp, %rdx
	xorq	%rsi, %rdx
	movq	-88(%rsp), %rsi
	addq	%rcx, %rdx
	addq	%rdx, %rbx
	movq	%rsi, %rdx
	rorq	%rdx
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	shrq	$7, %rsi
	rorq	$8, %rdx
	xorq	%rcx, %rdx
	movq	-112(%rsp), %rcx
	xorq	%rsi, %rdx
	movq	-96(%rsp), %rsi
	addq	%r13, %rdx
	addq	%rsi, %rdx
	movq	%rcx, %rsi
	rolq	$3, %rcx
	rorq	$19, %rsi
	xorq	%rsi, %rcx
	movq	-112(%rsp), %rsi
	shrq	$6, %rsi
	xorq	%rsi, %rcx
	leaq	(%rdx,%rcx), %rsi
	movq	%r11, %rcx
	movq	%r11, %rdx
	rorq	$14, %rcx
	rorq	$18, %rdx
	addq	160(%r10), %rdi
	movq	%rsi, -96(%rsp)
	xorq	%rcx, %rdx
	movq	%r11, %rcx
	addq	%rsi, %rdi
	rolq	$23, %rcx
	xorq	%rdx, %rcx
	movq	%r11, %rdx
	addq	%rdi, %rcx
	notq	%rdx
	movq	%r9, %rdi
	andq	%r11, %rdi
	andq	%r8, %rdx
	xorq	%rdi, %rdx
	movq	%rbx, %rdi
	addq	%rdx, %rcx
	rolq	$30, %rdi
	movq	%rbx, %rdx
	rorq	$28, %rdx
	movq	%rdi, %rsi
	movq	%r12, %rdi
	addq	%rcx, %rax
	xorq	%rdx, %rsi
	movq	%rbx, %rdx
	andq	%rbp, %rdi
	rolq	$25, %rdx
	xorq	%rdx, %rsi
	movq	%r12, %rdx
	xorq	%rbp, %rdx
	andq	%rbx, %rdx
	xorq	%rdi, %rdx
	movq	-80(%rsp), %rdi
	addq	%rsi, %rdx
	addq	%rdx, %rcx
	movq	%rdi, %rdx
	rorq	%rdx
	movq	%rdx, %rsi
	movq	%rdi, %rdx
	shrq	$7, %rdi
	rorq	$8, %rdx
	xorq	%rsi, %rdx
	movq	-104(%rsp), %rsi
	xorq	%rdi, %rdx
	movq	-88(%rsp), %rdi
	addq	%r15, %rdx
	addq	%rdi, %rdx
	movq	%rsi, %rdi
	rolq	$3, %rsi
	rorq	$19, %rdi
	xorq	%rdi, %rsi
	movq	-104(%rsp), %rdi
	addq	168(%r10), %r8
	shrq	$6, %rdi
	xorq	%rdi, %rsi
	addq	%rsi, %rdx
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rax, %rdx
	rorq	$18, %rsi
	rorq	$14, %rdx
	addq	%rdi, %r8
	movq	%rdi, -88(%rsp)
	xorq	%rdx, %rsi
	movq	%rax, %rdx
	rolq	$23, %rdx
	xorq	%rsi, %rdx
	addq	%r8, %rdx
	movq	%rax, %r8
	notq	%r8
	movq	%r8, %rsi
	movq	%r11, %r8
	andq	%rax, %r8
	andq	%r9, %rsi
	xorq	%r8, %rsi
	movq	%rcx, %r8
	rorq	$28, %r8
	addq	%rsi, %rdx
	movq	%r8, %rsi
	movq	%rcx, %r8
	addq	%rdx, %r12
	rolq	$30, %r8
	movq	%r8, %rdi
	movq	%rcx, %r8
	xorq	%rsi, %rdi
	rolq	$25, %r8
	xorq	%r8, %rdi
	movq	%rbp, %r8
	xorq	%rbx, %r8
	movq	%r8, %rsi
	movq	%rbp, %r8
	andq	%rbx, %r8
	andq	%rcx, %rsi
	xorq	%r8, %rsi
	movq	-72(%rsp), %r8
	addq	%rdi, %rsi
	addq	%rsi, %rdx
	movq	%r8, %rdi
	movq	%r8, %rsi
	shrq	$7, %r8
	rorq	%rdi
	rorq	$8, %rsi
	xorq	%rdi, %rsi
	movq	-96(%rsp), %rdi
	xorq	%r8, %rsi
	movq	-80(%rsp), %r8
	addq	%r14, %rsi
	addq	%r8, %rsi
	movq	%rdi, %r8
	rolq	$3, %rdi
	rorq	$19, %r8
	xorq	%r8, %rdi
	movq	-96(%rsp), %r8
	shrq	$6, %r8
	xorq	%r8, %rdi
	leaq	(%rsi,%rdi), %r8
	movq	%r12, %rsi
	rorq	$14, %rsi
	movq	%r8, -80(%rsp)
	movq	%rsi, %rdi
	movq	%r12, %rsi
	rorq	$18, %rsi
	xorq	%rdi, %rsi
	movq	%r12, %rdi
	rolq	$23, %rdi
	xorq	%rsi, %rdi
	movq	176(%r10), %rsi
	addq	%r8, %rsi
	addq	%r9, %rsi
	movq	%r12, %r9
	notq	%r9
	addq	%rdi, %rsi
	movq	%r9, %rdi
	movq	%rax, %r9
	andq	%r12, %r9
	andq	%r11, %rdi
	xorq	%r9, %rdi
	movq	%rdx, %r9
	rorq	$28, %r9
	addq	%rdi, %rsi
	movq	%r9, %rdi
	movq	%rdx, %r9
	addq	%rsi, %rbp
	rolq	$30, %r9
	movq	%r9, %r8
	movq	%rdx, %r9
	xorq	%rdi, %r8
	rolq	$25, %r9
	xorq	%r9, %r8
	movq	%rbx, %r9
	xorq	%rcx, %r9
	movq	%r9, %rdi
	movq	%rbx, %r9
	andq	%rcx, %r9
	andq	%rdx, %rdi
	xorq	%r9, %rdi
	movq	-64(%rsp), %r9
	addq	%r8, %rdi
	addq	%rdi, %rsi
	movq	%r9, %rdi
	rorq	%rdi
	movq	%rdi, %r8
	movq	%r9, %rdi
	shrq	$7, %r9
	rorq	$8, %rdi
	xorq	%r8, %rdi
	movq	-88(%rsp), %r8
	xorq	%r9, %rdi
	movq	-40(%rsp), %r9
	addq	%r9, %rdi
	movq	-72(%rsp), %r9
	addq	%r9, %rdi
	movq	%r8, %r9
	rolq	$3, %r8
	rorq	$19, %r9
	xorq	%r9, %r8
	movq	-88(%rsp), %r9
	shrq	$6, %r9
	xorq	%r9, %r8
	addq	%r8, %rdi
	movq	%rdi, %r9
	movq	%rbp, %rdi
	rorq	$14, %rdi
	movq	%r9, -72(%rsp)
	movq	%rdi, %r8
	movq	%rbp, %rdi
	rorq	$18, %rdi
	xorq	%r8, %rdi
	movq	%rbp, %r8
	rolq	$23, %r8
	xorq	%rdi, %r8
	movq	184(%r10), %rdi
	addq	%r9, %rdi
	addq	%r11, %rdi
	movq	%r12, %r11
	addq	%r8, %rdi
	movq	%rbp, %r8
	andq	%rbp, %r11
	notq	%r8
	andq	%rax, %r8
	xorq	%r11, %r8
	movq	%rsi, %r11
	addq	%r8, %rdi
	rolq	$30, %r11
	movq	%rsi, %r8
	rorq	$28, %r8
	movq	%r11, %r9
	movq	%rcx, %r11
	xorq	%r8, %r9
	movq	%rsi, %r8
	rolq	$25, %r8
	xorq	%r8, %r9
	movq	%rcx, %r8
	xorq	%rdx, %r8
	andq	%rsi, %r8
	andq	%rdx, %r11
	addq	%rdi, %rbx
	xorq	%r11, %r8
	movq	-56(%rsp), %r11
	addq	%r9, %r8
	addq	%r8, %rdi
	movq	%r11, %r8
	rorq	%r8
	movq	%r8, %r9
	movq	%r11, %r8
	shrq	$7, %r11
	rorq	$8, %r8
	xorq	%r9, %r8
	movq	-80(%rsp), %r9
	xorq	%r11, %r8
	movq	-120(%rsp), %r11
	addq	%r11, %r8
	movq	-64(%rsp), %r11
	addq	%r11, %r8
	movq	%r9, %r11
	rolq	$3, %r9
	rorq	$19, %r11
	xorq	%r11, %r9
	movq	-80(%rsp), %r11
	shrq	$6, %r11
	xorq	%r11, %r9
	leaq	(%r8,%r9), %r11
	movq	%rbx, %r8
	rorq	$14, %r8
	movq	%r11, -64(%rsp)
	movq	%r8, %r9
	movq	%rbx, %r8
	rorq	$18, %r8
	xorq	%r9, %r8
	movq	%rbx, %r9
	rolq	$23, %r9
	xorq	%r8, %r9
	movq	192(%r10), %r8
	addq	%r11, %r8
	movq	%rdx, %r11
	addq	%r8, %rax
	movq	%rbx, %r8
	addq	%r9, %rax
	notq	%r8
	movq	%rbp, %r9
	andq	%rbx, %r9
	andq	%r12, %r8
	xorq	%r9, %r8
	movq	%rdi, %r9
	addq	%r8, %rax
	movq	%rdi, %r8
	rolq	$30, %r9
	rorq	$28, %r8
	xorq	%r8, %r9
	movq	%rdi, %r8
	rolq	$25, %r8
	andq	%rsi, %r11
	xorq	%r8, %r9
	movq	%rdx, %r8
	xorq	%rsi, %r8
	andq	%rdi, %r8
	xorq	%r11, %r8
	movq	-48(%rsp), %r11
	addq	%r8, %r9
	leaq	(%rcx,%rax), %r8
	addq	%r9, %rax
	movq	%r11, %rcx
	movq	%r11, %r9
	rorq	%r9
	rorq	$8, %rcx
	xorq	%r9, %rcx
	movq	%r11, %r9
	movq	-112(%rsp), %r11
	shrq	$7, %r9
	xorq	%r9, %rcx
	movq	-72(%rsp), %r9
	addq	%r11, %rcx
	movq	-56(%rsp), %r11
	addq	%r11, %rcx
	movq	%r9, %r11
	rolq	$3, %r9
	rorq	$19, %r11
	xorq	%r11, %r9
	movq	-72(%rsp), %r11
	shrq	$6, %r11
	xorq	%r11, %r9
	addq	%r9, %rcx
	movq	%rcx, %r11
	movq	%r8, %rcx
	rorq	$14, %rcx
	movq	%r11, -56(%rsp)
	movq	%rcx, %r9
	movq	%r8, %rcx
	rorq	$18, %rcx
	xorq	%r9, %rcx
	movq	%r8, %r9
	rolq	$23, %r9
	xorq	%rcx, %r9
	movq	200(%r10), %rcx
	addq	%r11, %rcx
	addq	%r12, %rcx
	movq	%r8, %r12
	notq	%r12
	addq	%r9, %rcx
	movq	%r12, %r9
	movq	%rbx, %r12
	andq	%r8, %r12
	andq	%rbp, %r9
	xorq	%r12, %r9
	movq	%rax, %r12
	addq	%r9, %rcx
	rorq	$28, %r12
	movq	%r12, %r9
	movq	%rax, %r12
	rolq	$30, %r12
	movq	%r12, %r11
	movq	%rax, %r12
	xorq	%r9, %r11
	rolq	$25, %r12
	xorq	%r12, %r11
	movq	%rsi, %r12
	xorq	%rdi, %r12
	movq	%r12, %r9
	movq	%rsi, %r12
	andq	%rdi, %r12
	andq	%rax, %r9
	xorq	%r12, %r9
	addq	%r9, %r11
	leaq	(%rdx,%rcx), %r9
	movq	-32(%rsp), %rdx
	addq	%r11, %rcx
	movq	%rdx, %r12
	rorq	%r12
	movq	%r12, %r11
	movq	%rdx, %r12
	rorq	$8, %rdx
	xorq	%r11, %rdx
	shrq	$7, %r12
	movq	-64(%rsp), %r11
	xorq	%r12, %rdx
	movq	-104(%rsp), %r12
	addq	%r12, %rdx
	movq	-48(%rsp), %r12
	addq	%r12, %rdx
	movq	%r11, %r12
	rolq	$3, %r11
	rorq	$19, %r12
	xorq	%r12, %r11
	movq	-64(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %r11
	addq	%r11, %rdx
	movq	%rdx, %r12
	movq	%r9, %rdx
	rorq	$14, %rdx
	movq	%r12, -48(%rsp)
	movq	%rdx, %r11
	movq	%r9, %rdx
	rorq	$18, %rdx
	xorq	%r11, %rdx
	movq	%r9, %r11
	rolq	$23, %r11
	xorq	%rdx, %r11
	movq	208(%r10), %rdx
	addq	%r12, %rdx
	movq	%rdi, %r12
	addq	%rbp, %rdx
	movq	%r9, %rbp
	notq	%rbp
	addq	%r11, %rdx
	andq	%rax, %r12
	movq	%rbp, %r11
	movq	%r8, %rbp
	andq	%r9, %rbp
	andq	%rbx, %r11
	xorq	%rbp, %r11
	movq	%rcx, %rbp
	rorq	$28, %rbp
	addq	%r11, %rdx
	movq	%rbp, %r11
	movq	%rcx, %rbp
	rolq	$30, %rbp
	xorq	%r11, %rbp
	movq	%rcx, %r11
	rolq	$25, %r11
	xorq	%r11, %rbp
	movq	%rdi, %r11
	xorq	%rax, %r11
	andq	%rcx, %r11
	xorq	%r12, %r11
	movq	-24(%rsp), %r12
	addq	%r11, %rbp
	leaq	(%rsi,%rdx), %r11
	addq	%rbp, %rdx
	movq	%r12, %rsi
	movq	%r12, %rbp
	rorq	%rbp
	rorq	$8, %rsi
	xorq	%rbp, %rsi
	movq	%r12, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %rsi
	movq	-96(%rsp), %rbp
	addq	%rbp, %rsi
	movq	-32(%rsp), %rbp
	addq	%rbp, %rsi
	movq	-56(%rsp), %rbp
	movq	%rbp, %r12
	rolq	$3, %rbp
	rorq	$19, %r12
	xorq	%r12, %rbp
	movq	-56(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	addq	%rbp, %rsi
	movq	%rsi, %r12
	movq	%r11, %rsi
	rorq	$14, %rsi
	movq	%r12, -32(%rsp)
	movq	%rsi, %rbp
	movq	%r11, %rsi
	rorq	$18, %rsi
	xorq	%rbp, %rsi
	movq	%r11, %rbp
	rolq	$23, %rbp
	xorq	%rsi, %rbp
	movq	216(%r10), %rsi
	addq	%r12, %rsi
	movq	%rax, %r12
	addq	%rbx, %rsi
	movq	%r11, %rbx
	andq	%rcx, %r12
	addq	%rbp, %rsi
	notq	%rbx
	movq	%r9, %rbp
	andq	%r11, %rbp
	andq	%r8, %rbx
	xorq	%rbp, %rbx
	movq	%rdx, %rbp
	addq	%rbx, %rsi
	movq	%rdx, %rbx
	rolq	$30, %rbp
	rorq	$28, %rbx
	xorq	%rbx, %rbp
	movq	%rdx, %rbx
	rolq	$25, %rbx
	xorq	%rbx, %rbp
	movq	%rax, %rbx
	xorq	%rcx, %rbx
	andq	%rdx, %rbx
	xorq	%r12, %rbx
	addq	%rbx, %rbp
	leaq	(%rdi,%rsi), %rbx
	movq	%r13, %rdi
	rorq	%rdi
	addq	%rbp, %rsi
	movq	%rdi, %rbp
	movq	%r13, %rdi
	rorq	$8, %rdi
	xorq	%rbp, %rdi
	movq	%r13, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %rdi
	movq	-88(%rsp), %rbp
	addq	%rbp, %rdi
	movq	-24(%rsp), %rbp
	addq	%rbp, %rdi
	movq	-48(%rsp), %rbp
	movq	%rbp, %r12
	rolq	$3, %rbp
	rorq	$19, %r12
	xorq	%r12, %rbp
	movq	-48(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	addq	%rbp, %rdi
	movq	%rbx, %rbp
	movq	%rdi, %r12
	movq	%rbx, %rdi
	rorq	$14, %rbp
	rorq	$18, %rdi
	movq	%r12, -24(%rsp)
	xorq	%rbp, %rdi
	movq	%rbx, %rbp
	rolq	$23, %rbp
	xorq	%rdi, %rbp
	movq	224(%r10), %rdi
	addq	%r12, %rdi
	movq	%rcx, %r12
	addq	%r8, %rdi
	movq	%rbx, %r8
	andq	%rdx, %r12
	addq	%rbp, %rdi
	notq	%r8
	movq	%r11, %rbp
	andq	%rbx, %rbp
	andq	%r9, %r8
	xorq	%rbp, %r8
	movq	%rsi, %rbp
	addq	%r8, %rdi
	movq	%rsi, %r8
	rolq	$30, %rbp
	rorq	$28, %r8
	addq	%rdi, %rax
	xorq	%r8, %rbp
	movq	%rsi, %r8
	rolq	$25, %r8
	xorq	%r8, %rbp
	movq	%rcx, %r8
	xorq	%rdx, %r8
	andq	%rsi, %r8
	xorq	%r12, %r8
	addq	%rbp, %r8
	movq	%r15, %rbp
	addq	%r8, %rdi
	movq	%r15, %r8
	rorq	%rbp
	rorq	$8, %r8
	xorq	%rbp, %r8
	movq	%r15, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %r8
	movq	-80(%rsp), %rbp
	addq	%rbp, %r8
	addq	%r13, %r8
	movq	-32(%rsp), %r13
	movq	%r13, %rbp
	rorq	$19, %rbp
	movq	%rbp, %r12
	movq	%r13, %rbp
	shrq	$6, %r13
	rolq	$3, %rbp
	xorq	%r12, %rbp
	movq	%rdx, %r12
	xorq	%r13, %rbp
	andq	%rsi, %r12
	leaq	(%r8,%rbp), %r13
	movq	%rax, %rbp
	movq	%rax, %r8
	rorq	$14, %rbp
	rorq	$18, %r8
	xorq	%rbp, %r8
	movq	%rax, %rbp
	rolq	$23, %rbp
	xorq	%r8, %rbp
	movq	232(%r10), %r8
	addq	%r13, %r8
	addq	%r9, %r8
	addq	%rbp, %r8
	movq	%rax, %rbp
	notq	%rbp
	movq	%rbp, %r9
	movq	%rbx, %rbp
	andq	%r11, %r9
	andq	%rax, %rbp
	xorq	%rbp, %r9
	movq	%rdi, %rbp
	rorq	$28, %rbp
	addq	%r9, %r8
	movq	%rbp, %r9
	movq	%rdi, %rbp
	addq	%r8, %rcx
	rolq	$30, %rbp
	xorq	%r9, %rbp
	movq	%rdi, %r9
	rolq	$25, %r9
	xorq	%r9, %rbp
	movq	%rdx, %r9
	xorq	%rsi, %r9
	andq	%rdi, %r9
	xorq	%r12, %r9
	addq	%rbp, %r9
	movq	%r14, %rbp
	addq	%r9, %r8
	movq	%r14, %r9
	rorq	%rbp
	rorq	$8, %r9
	xorq	%rbp, %r9
	movq	%r14, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %r9
	movq	-72(%rsp), %rbp
	addq	%rbp, %r9
	addq	%r15, %r9
	movq	-24(%rsp), %r15
	movq	%r15, %rbp
	rorq	$19, %rbp
	movq	%rbp, %r12
	movq	%r15, %rbp
	shrq	$6, %r15
	rolq	$3, %rbp
	xorq	%r12, %rbp
	movq	%rsi, %r12
	xorq	%r15, %rbp
	andq	%rdi, %r12
	leaq	(%r9,%rbp), %r15
	movq	%rcx, %rbp
	movq	%rcx, %r9
	rorq	$14, %rbp
	rorq	$18, %r9
	xorq	%rbp, %r9
	movq	%rcx, %rbp
	rolq	$23, %rbp
	xorq	%r9, %rbp
	movq	240(%r10), %r9
	addq	%r15, %r9
	addq	%r11, %r9
	addq	%rbp, %r9
	movq	%rcx, %rbp
	notq	%rbp
	movq	%rbp, %r11
	movq	%rax, %rbp
	andq	%rbx, %r11
	andq	%rcx, %rbp
	xorq	%rbp, %r11
	movq	%r8, %rbp
	rorq	$28, %rbp
	addq	%r11, %r9
	movq	%rbp, %r11
	movq	%r8, %rbp
	addq	%r9, %rdx
	rolq	$30, %rbp
	xorq	%r11, %rbp
	movq	%r8, %r11
	rolq	$25, %r11
	xorq	%r11, %rbp
	movq	%rsi, %r11
	xorq	%rdi, %r11
	andq	%r8, %r11
	xorq	%r12, %r11
	movq	-40(%rsp), %r12
	addq	%rbp, %r11
	addq	%r11, %r9
	movq	%r12, %rbp
	movq	%r12, %r11
	rorq	%rbp
	rorq	$8, %r11
	xorq	%rbp, %r11
	movq	%r12, %rbp
	movq	-64(%rsp), %r12
	shrq	$7, %rbp
	xorq	%rbp, %r11
	movq	%r13, %rbp
	addq	%r12, %r11
	movq	%r13, %r12
	rolq	$3, %rbp
	rorq	$19, %r12
	addq	%r14, %r11
	xorq	%r12, %rbp
	movq	%r13, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	movq	%rdi, %r12
	leaq	(%r11,%rbp), %r14
	movq	%rdx, %rbp
	movq	%rdx, %r11
	rorq	$14, %rbp
	rorq	$18, %r11
	xorq	%rbp, %r11
	movq	%rdx, %rbp
	rolq	$23, %rbp
	xorq	%r11, %rbp
	movq	248(%r10), %r11
	addq	%r14, %r11
	addq	%rbx, %r11
	movq	%rdx, %rbx
	addq	%rbp, %r11
	notq	%rbx
	movq	%rcx, %rbp
	andq	%rdx, %rbp
	andq	%rax, %rbx
	xorq	%rbp, %rbx
	movq	%r9, %rbp
	addq	%rbx, %r11
	movq	%r9, %rbx
	rolq	$30, %rbp
	rorq	$28, %rbx
	xorq	%rbx, %rbp
	movq	%r9, %rbx
	rolq	$25, %rbx
	xorq	%rbx, %rbp
	movq	%rdi, %rbx
	xorq	%r8, %rbx
	andq	%r9, %rbx
	andq	%r8, %r12
	addq	%r11, %rsi
	subq	$-128, %r10
	xorq	%r12, %rbx
	addq	%rbp, %rbx
	addq	%rbx, %r11
	movl	$K512+512, %ebx
	cmpq	%r10, %rbx
	jne	.L7
	movq	64(%rsp), %rbx
	addq	%r9, -16(%rsp)
	movq	-16(%rsp), %r15
	movq	48(%rsp), %r13
	addq	%rdi, (%rsp)
	movq	%rbx, %rdi
	movq	40(%rsp), %r12
	movq	%r15, 8(%rbx)
	movq	(%rsp), %r15
	addq	%r11, %r13
	addq	%r8, -8(%rsp)
	movq	-8(%rsp), %r14
	subq	$-128, %r12
	movq	%r13, (%rbx)
	movq	%r14, 16(%rbx)
	movq	%r15, 24(%rbx)
	addq	%rsi, 8(%rsp)
	movq	8(%rsp), %rbx
	movq	%rdi, %rsi
	addq	%rdx, 16(%rsp)
	movq	%rbx, 32(%rdi)
	movq	16(%rsp), %rdi
	addq	%rcx, 24(%rsp)
	movq	24(%rsp), %rcx
	addq	%rax, 32(%rsp)
	movq	32(%rsp), %rax
	movq	%rdi, 40(%rsi)
	movq	%rcx, 48(%rsi)
	movq	%rax, 56(%rsi)
	movq	56(%rsp), %rax
	cmpq	%rax, %r12
	jne	.L8
	addq	$72, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE47:
	.size	sha512_block_data_order, .-sha512_block_data_order
	.p2align 4
	.type	sha512_final_impl, @function
sha512_final_impl:
.LFB49:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	leaq	80(%rdx), %r13
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdi, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rsi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdx, %rbx
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
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
	rep stosq
.L22:
	movq	72(%rbx), %rax
	movq	64(%rbx), %rdx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	bswap	%rdx
	bswap	%rax
	movq	%rdx, %xmm1
	movq	%rax, %xmm0
	punpcklqdq	%xmm1, %xmm0
	movl	$1, %edx
	movups	%xmm0, 192(%rbx)
	call	sha512_block_data_order
	testq	%r12, %r12
	je	.L31
	shrq	$3, %rbp
	je	.L30
	movq	(%rbx), %rax
	bswap	%rax
	movq	%rax, (%r12)
	cmpq	$1, %rbp
	je	.L30
	movq	8(%rbx), %rax
	bswap	%rax
	movq	%rax, 8(%r12)
	cmpq	$2, %rbp
	je	.L30
	movq	16(%rbx), %rax
	bswap	%rax
	movq	%rax, 16(%r12)
	cmpq	$3, %rbp
	je	.L30
	movq	24(%rbx), %rax
	bswap	%rax
	movq	%rax, 24(%r12)
	cmpq	$4, %rbp
	je	.L30
	movq	32(%rbx), %rax
	bswap	%rax
	movq	%rax, 32(%r12)
	cmpq	$5, %rbp
	je	.L30
	movq	40(%rbx), %rax
	bswap	%rax
	movq	%rax, 40(%r12)
	cmpq	$6, %rbp
	je	.L30
	movq	48(%rbx), %rax
	bswap	%rax
	movq	%rax, 48(%r12)
	cmpq	$7, %rbp
	je	.L30
	movq	56(%rbx), %rax
	bswap	%rax
	movq	%rax, 56(%r12)
.L30:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	movl	$1, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L74:
	.cfi_restore_state
	andl	$4, %edx
	jne	.L75
	testl	%ecx, %ecx
	je	.L22
	movb	$0, (%rsi)
	testb	$2, %cl
	je	.L22
	xorl	%eax, %eax
	movw	%ax, -2(%rsi,%rcx)
	jmp	.L22
	.p2align 4,,10
	.p2align 3
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
	.p2align 4,,10
	.p2align 3
.L13:
	movl	$112, %edx
	subq	%rax, %rdx
	je	.L22
	jmp	.L21
	.p2align 4,,10
	.p2align 3
.L76:
	testb	$4, %dl
	jne	.L77
	testq	%rdx, %rdx
	je	.L14
	movb	$0, (%rsi)
	testb	$2, %dl
	je	.L14
	xorl	%ecx, %ecx
	movw	%cx, -2(%rsi,%rdx)
	jmp	.L14
	.p2align 4,,10
	.p2align 3
.L31:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
.L75:
	.cfi_restore_state
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rcx)
	jmp	.L22
.L77:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rdx)
	jmp	.L14
	.cfi_endproc
.LFE49:
	.size	sha512_final_impl, .-sha512_final_impl
	.p2align 4
	.type	table_select, @function
table_select:
.LFB82:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movl	%edx, %r9d
	movsbq	%dl, %rdx
	xorl	%eax, %eax
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdi, %r8
	movl	$11, %ecx
	movslq	%esi, %rsi
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	leaq	(%rsi,%rsi,2), %rsi
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	salq	$8, %rsi
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	addq	$k25519Precomp, %rsi
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdx, %rbx
	sarq	$63, %rbx
	subq	$144, %rsp
	.cfi_def_cfa_offset 200
	leaq	-80(%rsp), %rdi
	movq	%rbx, -96(%rsp)
	movq	$0, -87(%rsp)
	movq	$0, (%rsp)
	rep stosq
	movq	%rdx, %rax
	subq	$1, %rdx
	movl	$1, %edi
	notq	%rax
	andq	%rdx, %rax
	sarq	$63, %rax
	andl	$1, %eax
	movb	%al, -88(%rsp)
	movb	%al, -56(%rsp)
	movl	%ebx, %eax
	andl	%r9d, %eax
	addl	%eax, %eax
	subl	%eax, %r9d
	movzbl	%r9b, %r9d
	.p2align 4,,10
	.p2align 3
.L80:
	movq	%r9, %rax
	xorq	%rdi, %rax
	movq	%rax, %rdx
	subq	$1, %rax
	notq	%rdx
	andq	%rax, %rdx
	xorl	%eax, %eax
	sarq	$63, %rdx
	.p2align 4,,10
	.p2align 3
.L79:
	movzbl	(%rsi,%rax), %ecx
	andl	%edx, %ecx
	xorb	%cl, -88(%rsp,%rax)
	addq	$1, %rax
	cmpq	$96, %rax
	jne	.L79
	addq	$1, %rdi
	movzbl	-88(%rsp), %eax
	addq	$96, %rsi
	cmpq	$9, %rdi
	jne	.L80
	movzbl	-82(%rsp), %ecx
	movzbl	-83(%rsp), %edx
	movzbl	-77(%rsp), %esi
	salq	$40, %rdx
	salq	$48, %rcx
	addq	%rdx, %rcx
	movzbl	-76(%rsp), %edx
	salq	$37, %rsi
	addq	%rax, %rcx
	movzbl	-84(%rsp), %eax
	salq	$45, %rdx
	addq	%rsi, %rdx
	movzbl	-78(%rsp), %esi
	salq	$32, %rax
	addq	%rax, %rcx
	movzbl	-85(%rsp), %eax
	salq	$29, %rsi
	addq	%rsi, %rdx
	movzbl	-79(%rsp), %esi
	salq	$24, %rax
	addq	%rax, %rcx
	movzbl	-86(%rsp), %eax
	salq	$21, %rsi
	addq	%rsi, %rdx
	movzbl	-80(%rsp), %esi
	salq	$16, %rax
	addq	%rax, %rcx
	movzbl	-87(%rsp), %eax
	salq	$13, %rsi
	addq	%rsi, %rdx
	movzbl	-81(%rsp), %esi
	salq	$8, %rax
	addq	%rax, %rcx
	movabsq	$2251799813685247, %rax
	salq	$5, %rsi
	movq	%rcx, %r11
	shrq	$51, %rcx
	addq	%rsi, %rdx
	movzbl	-70(%rsp), %esi
	andq	%rax, %r11
	addq	%rcx, %rdx
	movzbl	-69(%rsp), %ecx
	salq	$42, %rsi
	movq	%rdx, %r10
	salq	$50, %rcx
	andq	%rax, %r10
	addq	%rsi, %rcx
	movzbl	-71(%rsp), %esi
	salq	$34, %rsi
	addq	%rsi, %rcx
	movzbl	-72(%rsp), %esi
	salq	$26, %rsi
	shrq	$51, %rdx
	addq	%rsi, %rcx
	movzbl	-73(%rsp), %esi
	salq	$18, %rsi
	addq	%rsi, %rcx
	movzbl	-74(%rsp), %esi
	salq	$10, %rsi
	addq	%rsi, %rcx
	movzbl	-75(%rsp), %esi
	leaq	(%rcx,%rsi,4), %rcx
	movzbl	-64(%rsp), %esi
	addq	%rdx, %rcx
	movzbl	-63(%rsp), %edx
	salq	$39, %rsi
	movq	%rcx, %r9
	shrq	$51, %rcx
	salq	$47, %rdx
	andq	%rax, %r9
	addq	%rsi, %rdx
	movzbl	-65(%rsp), %esi
	salq	$31, %rsi
	addq	%rsi, %rdx
	movzbl	-66(%rsp), %esi
	salq	$23, %rsi
	addq	%rsi, %rdx
	movzbl	-67(%rsp), %esi
	salq	$15, %rsi
	addq	%rsi, %rdx
	movzbl	-68(%rsp), %esi
	salq	$7, %rsi
	addq	%rsi, %rdx
	movzbl	-58(%rsp), %esi
	addq	%rcx, %rdx
	movzbl	-57(%rsp), %ecx
	salq	$36, %rsi
	movq	%rdx, %rdi
	salq	$44, %rcx
	andq	%rax, %rdi
	addq	%rsi, %rcx
	movzbl	-59(%rsp), %esi
	salq	$28, %rsi
	addq	%rsi, %rcx
	movzbl	-60(%rsp), %esi
	salq	$20, %rsi
	addq	%rsi, %rcx
	movzbl	-61(%rsp), %esi
	salq	$12, %rsi
	addq	%rsi, %rcx
	movzbl	-62(%rsp), %esi
	salq	$4, %rsi
	shrq	$51, %rdx
	addq	%rsi, %rcx
	leaq	(%rcx,%rdx), %rbx
	movq	%rbx, -104(%rsp)
	movzbl	-50(%rsp), %ecx
	movzbl	-51(%rsp), %edx
	movzbl	-45(%rsp), %esi
	salq	$48, %rcx
	salq	$40, %rdx
	salq	$37, %rsi
	addq	%rdx, %rcx
	movzbl	-56(%rsp), %edx
	addq	%rdx, %rcx
	movzbl	-52(%rsp), %edx
	salq	$32, %rdx
	addq	%rdx, %rcx
	movzbl	-53(%rsp), %edx
	salq	$24, %rdx
	addq	%rdx, %rcx
	movzbl	-54(%rsp), %edx
	salq	$16, %rdx
	addq	%rdx, %rcx
	movzbl	-55(%rsp), %edx
	salq	$8, %rdx
	addq	%rdx, %rcx
	movzbl	-44(%rsp), %edx
	movq	%rcx, %r13
	shrq	$51, %rcx
	salq	$45, %rdx
	andq	%rax, %r13
	addq	%rsi, %rdx
	movzbl	-46(%rsp), %esi
	salq	$29, %rsi
	addq	%rsi, %rdx
	movzbl	-47(%rsp), %esi
	salq	$21, %rsi
	addq	%rsi, %rdx
	movzbl	-48(%rsp), %esi
	salq	$13, %rsi
	addq	%rsi, %rdx
	movzbl	-49(%rsp), %esi
	salq	$5, %rsi
	addq	%rsi, %rdx
	movzbl	-38(%rsp), %esi
	addq	%rcx, %rdx
	movzbl	-37(%rsp), %ecx
	movq	%rdx, %r12
	salq	$42, %rsi
	salq	$50, %rcx
	andq	%rax, %r12
	addq	%rsi, %rcx
	movzbl	-39(%rsp), %esi
	shrq	$51, %rdx
	salq	$34, %rsi
	addq	%rsi, %rcx
	movzbl	-40(%rsp), %esi
	salq	$26, %rsi
	addq	%rsi, %rcx
	movzbl	-41(%rsp), %esi
	salq	$18, %rsi
	addq	%rsi, %rcx
	movzbl	-42(%rsp), %esi
	salq	$10, %rsi
	addq	%rsi, %rcx
	movzbl	-43(%rsp), %esi
	leaq	(%rcx,%rsi,4), %rcx
	movzbl	-32(%rsp), %esi
	addq	%rdx, %rcx
	movzbl	-31(%rsp), %edx
	salq	$39, %rsi
	movq	%rcx, %rbp
	shrq	$51, %rcx
	salq	$47, %rdx
	andq	%rax, %rbp
	addq	%rsi, %rdx
	movzbl	-33(%rsp), %esi
	salq	$31, %rsi
	addq	%rsi, %rdx
	movzbl	-34(%rsp), %esi
	salq	$23, %rsi
	addq	%rsi, %rdx
	movzbl	-35(%rsp), %esi
	salq	$15, %rsi
	addq	%rsi, %rdx
	movzbl	-36(%rsp), %esi
	salq	$7, %rsi
	addq	%rsi, %rdx
	addq	%rcx, %rdx
	movzbl	-25(%rsp), %ecx
	movq	%rdx, %rsi
	andq	%rax, %rsi
	salq	$44, %rcx
	movq	%rsi, %rbx
	movzbl	-26(%rsp), %esi
	salq	$36, %rsi
	addq	%rsi, %rcx
	movzbl	-27(%rsp), %esi
	salq	$28, %rsi
	addq	%rsi, %rcx
	movzbl	-28(%rsp), %esi
	salq	$20, %rsi
	addq	%rsi, %rcx
	movzbl	-29(%rsp), %esi
	shrq	$51, %rdx
	salq	$12, %rsi
	addq	%rsi, %rcx
	movzbl	-30(%rsp), %esi
	salq	$4, %rsi
	addq	%rsi, %rcx
	leaq	(%rcx,%rdx), %rsi
	movq	%rsi, -112(%rsp)
	movzbl	-18(%rsp), %edx
	movzbl	-19(%rsp), %ecx
	movzbl	-12(%rsp), %esi
	salq	$48, %rdx
	salq	$40, %rcx
	salq	$45, %rsi
	addq	%rcx, %rdx
	movzbl	-24(%rsp), %ecx
	addq	%rcx, %rdx
	movzbl	-20(%rsp), %ecx
	salq	$32, %rcx
	addq	%rcx, %rdx
	movzbl	-21(%rsp), %ecx
	salq	$24, %rcx
	addq	%rcx, %rdx
	movzbl	-22(%rsp), %ecx
	salq	$16, %rcx
	addq	%rcx, %rdx
	movzbl	-23(%rsp), %ecx
	salq	$8, %rcx
	addq	%rcx, %rdx
	movzbl	-13(%rsp), %ecx
	movq	%rdx, %r14
	shrq	$51, %rdx
	salq	$37, %rcx
	andq	%rax, %r14
	addq	%rcx, %rsi
	movzbl	-14(%rsp), %ecx
	movq	%r14, -120(%rsp)
	movzbl	-6(%rsp), %r14d
	salq	$29, %rcx
	addq	%rcx, %rsi
	movzbl	-15(%rsp), %ecx
	salq	$21, %rcx
	addq	%rcx, %rsi
	movzbl	-16(%rsp), %ecx
	salq	$13, %rcx
	addq	%rcx, %rsi
	movzbl	-17(%rsp), %ecx
	salq	$5, %rcx
	addq	%rsi, %rcx
	addq	%rdx, %rcx
	movq	%rcx, %rdx
	andq	%rax, %rdx
	salq	$42, %r14
	movq	%rdx, %rsi
	movzbl	-5(%rsp), %edx
	shrq	$51, %rcx
	salq	$50, %rdx
	addq	%r14, %rdx
	movzbl	-7(%rsp), %r14d
	salq	$34, %r14
	addq	%r14, %rdx
	movzbl	-8(%rsp), %r14d
	salq	$26, %r14
	addq	%r14, %rdx
	movzbl	-9(%rsp), %r14d
	salq	$18, %r14
	addq	%r14, %rdx
	movzbl	-10(%rsp), %r14d
	salq	$10, %r14
	addq	%r14, %rdx
	movzbl	-11(%rsp), %r14d
	leaq	(%rdx,%r14,4), %r15
	movzbl	1(%rsp), %edx
	movzbl	(%rsp), %r14d
	addq	%rcx, %r15
	salq	$39, %r14
	salq	$47, %rdx
	movq	%r15, %rcx
	addq	%r14, %rdx
	movzbl	-1(%rsp), %r14d
	shrq	$51, %r15
	andq	%rax, %rcx
	salq	$31, %r14
	addq	%r14, %rdx
	movzbl	-2(%rsp), %r14d
	salq	$23, %r14
	addq	%r14, %rdx
	movzbl	-3(%rsp), %r14d
	salq	$15, %r14
	addq	%r14, %rdx
	movzbl	-4(%rsp), %r14d
	salq	$7, %r14
	addq	%rdx, %r14
	movzbl	7(%rsp), %edx
	addq	%r15, %r14
	movzbl	6(%rsp), %r15d
	salq	$44, %rdx
	andq	%r14, %rax
	salq	$36, %r15
	addq	%r15, %rdx
	movzbl	5(%rsp), %r15d
	salq	$28, %r15
	shrq	$51, %r14
	addq	%r15, %rdx
	movzbl	4(%rsp), %r15d
	salq	$20, %r15
	addq	%r15, %rdx
	movzbl	3(%rsp), %r15d
	salq	$12, %r15
	addq	%r15, %rdx
	movzbl	2(%rsp), %r15d
	movq	%rdi, 24(%r8)
	movq	%rdi, 72(%rsp)
	salq	$4, %r15
	movq	%rsi, 88(%r8)
	addq	%rdx, %r15
	movq	%rcx, 96(%r8)
	movabsq	$4503599627370458, %rdx
	addq	%r14, %r15
	movq	-120(%rsp), %r14
	movq	%rax, 104(%r8)
	movq	%r11, (%r8)
	subq	%r14, %rdx
	movq	%r10, 8(%r8)
	movdqu	(%r8), %xmm0
	movq	%rdx, 88(%rsp)
	movdqu	(%r8), %xmm2
	movabsq	$4503599627370494, %rdx
	movq	%rdx, %rdi
	movq	%r13, 8(%rsp)
	subq	%rsi, %rdi
	movq	%rdx, %rsi
	movq	%r12, 16(%rsp)
	pxor	8(%rsp), %xmm0
	subq	%rcx, %rsi
	movq	%rdx, %rcx
	movq	%rbp, 24(%rsp)
	subq	%r15, %rdx
	subq	%rax, %rcx
	movq	-96(%rsp), %rax
	movq	%r9, 16(%r8)
	xorq	%r15, %rdx
	movq	%rbx, 32(%rsp)
	movq	%rax, %xmm1
	movq	%rdi, 96(%rsp)
	punpcklqdq	%xmm1, %xmm1
	movq	%rcx, 112(%rsp)
	pand	%xmm1, %xmm0
	movq	%r13, 40(%r8)
	pxor	%xmm2, %xmm0
	movq	%r12, 48(%r8)
	movq	%rbp, 56(%r8)
	movq	%rbx, 64(%r8)
	movq	%r14, 80(%r8)
	movq	%r11, 48(%rsp)
	movq	%r10, 56(%rsp)
	movq	%r9, 64(%rsp)
	movq	%rsi, 104(%rsp)
	movups	%xmm0, (%r8)
	movdqu	16(%r8), %xmm0
	pxor	24(%rsp), %xmm0
	movdqu	16(%r8), %xmm3
	movdqu	40(%r8), %xmm4
	pand	%xmm1, %xmm0
	movdqu	56(%r8), %xmm6
	movdqu	80(%r8), %xmm2
	pxor	%xmm3, %xmm0
	movq	-104(%rsp), %rbx
	movq	-112(%rsp), %rdi
	movups	%xmm0, 16(%r8)
	movdqu	48(%rsp), %xmm0
	movdqu	96(%r8), %xmm3
	movq	%rbx, %rcx
	pxor	%xmm4, %xmm0
	xorq	%rdi, %rcx
	pand	%xmm1, %xmm0
	andq	%rax, %rcx
	andq	%rdx, %rax
	pxor	%xmm4, %xmm0
	xorq	%rcx, %rbx
	xorq	%r15, %rax
	xorq	%rdi, %rcx
	movups	%xmm0, 40(%r8)
	movdqu	64(%rsp), %xmm0
	movq	%rbx, 32(%r8)
	pxor	%xmm6, %xmm0
	movq	%rcx, 72(%r8)
	pand	%xmm1, %xmm0
	movq	%rax, 112(%r8)
	pxor	%xmm6, %xmm0
	movups	%xmm0, 56(%r8)
	movdqu	80(%r8), %xmm0
	pxor	88(%rsp), %xmm0
	pand	%xmm1, %xmm0
	pxor	%xmm2, %xmm0
	movups	%xmm0, 80(%r8)
	movdqu	96(%r8), %xmm0
	pxor	104(%rsp), %xmm0
	pand	%xmm1, %xmm0
	pxor	%xmm3, %xmm0
	movups	%xmm0, 96(%r8)
	addq	$144, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE82:
	.size	table_select, .-table_select
	.p2align 4
	.type	ge_madd, @function
ge_madd:
.LFB76:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %rcx
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	%rdi, %rbx
	movq	%rdx, %rdi
	subq	$48, %rsp
	.cfi_def_cfa_offset 104
	movq	(%rsi), %rax
	addq	40(%rsi), %rax
	movq	%rax, %r14
	movq	24(%rsi), %rax
	addq	64(%rsi), %rax
	movq	8(%rsi), %r11
	movq	16(%rsi), %r9
	movq	%rax, %r15
	addq	48(%rsi), %r11
	addq	56(%rsi), %r9
	movq	32(%rsi), %rax
	addq	72(%rsi), %rax
	movq	%r14, (%rbx)
	movq	%r11, 8(%rbx)
	movq	%rax, %rbp
	movq	%r9, 16(%rbx)
	movq	%r15, 24(%rbx)
	movq	%rax, 32(%rbx)
	movabsq	$4503599627370458, %rax
	addq	40(%rsi), %rax
	subq	(%rsi), %rax
	movq	%rax, %rsi
	movabsq	$4503599627370494, %rax
	addq	48(%rcx), %rax
	subq	8(%rcx), %rax
	movq	%rax, %rdx
	movabsq	$4503599627370494, %rax
	addq	56(%rcx), %rax
	subq	16(%rcx), %rax
	movq	%rax, %r8
	movabsq	$4503599627370494, %rax
	addq	64(%rcx), %rax
	subq	24(%rcx), %rax
	movq	%rax, %r10
	movabsq	$4503599627370494, %rax
	addq	72(%rcx), %rax
	subq	32(%rcx), %rax
	movq	%rsi, -80(%rsp)
	movq	%rsi, 40(%rbx)
	movq	%rdx, 48(%rbx)
	movq	%r8, 56(%rbx)
	movq	%r10, 64(%rbx)
	movq	%rax, 72(%rbx)
	movq	%rbx, -120(%rsp)
	movq	32(%rdi), %rbx
	movq	%rax, -104(%rsp)
	leaq	(%rbx,%rbx,8), %rax
	movq	%r8, -112(%rsp)
	leaq	(%rbx,%rax,2), %rsi
	movq	24(%rdi), %rbx
	movq	%r10, -88(%rsp)
	movq	%rdx, -56(%rsp)
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r13
	movq	16(%rdi), %rbx
	movq	%r13, %r10
	leaq	(%rbx,%rbx,8), %rax
	movq	%r10, -72(%rsp)
	leaq	(%rbx,%rax,2), %r8
	movq	8(%rdi), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r13
	movq	%r11, %rbx
	movq	%r13, %rax
	mulq	%rbp
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r15
	addq	%rax, %r12
	movq	%r10, %rax
	movq	%r9, %r10
	adcq	%rdx, %r13
	mulq	%r9
	movq	%r10, -24(%rsp)
	addq	%rax, %r12
	movq	%r11, %rax
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	(%rdi)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%rbp
	movq	%r12, -40(%rsp)
	movq	%r13, -32(%rsp)
	movq	%rax, %r8
	movq	-72(%rsp), %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	(%rdi)
	movq	%r8, %r10
	movq	%r9, %r11
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	8(%rdi)
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	movq	%rax, %r12
	movq	-72(%rsp), %rax
	adcq	%r9, %rdx
	movq	%rdx, %r13
	movq	%r12, -72(%rsp)
	mulq	%rbp
	movq	%rax, %r8
	movq	%rsi, %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	-24(%rsp), %rax
	adcq	%rdx, %r9
	mulq	(%rdi)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	8(%rdi)
	movq	%r8, %r10
	movq	%r9, %r11
	movq	%r12, %r9
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	16(%rdi)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r9
	xorl	%r13d, %r13d
	movq	%r9, %r12
	addq	%rax, %r12
	movq	%rsi, %rax
	movabsq	$2251799813685247, %rsi
	adcq	%rdx, %r13
	mulq	%rbp
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	(%rdi)
	addq	%rax, %r8
	movq	-24(%rsp), %rax
	adcq	%rdx, %r9
	mulq	8(%rdi)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	16(%rdi)
	movq	%r8, %r10
	movq	%r9, %r11
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	addq	%r10, %r8
	adcq	%r11, %r9
	mulq	24(%rdi)
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %rax
	xorl	%edx, %edx
	movq	72(%rdi), %r13
	addq	%rax, %r8
	movq	%rbp, %rax
	adcq	%rdx, %r9
	mulq	(%rdi)
	movq	%r8, %r11
	andq	%rsi, %r11
	movq	%r11, 24(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r15, %rax
	mulq	8(%rdi)
	addq	%rax, %r10
	movq	-24(%rsp), %rax
	adcq	%rdx, %r11
	mulq	16(%rdi)
	addq	%rax, %r10
	movq	%rbx, %rax
	movq	-40(%rsp), %rbx
	adcq	%rdx, %r11
	mulq	24(%rdi)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	32(%rdi)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r9, %r8
	xorl	%r11d, %r11d
	addq	%r8, %rax
	adcq	%r11, %rdx
	movq	%rax, %r15
	andq	%rsi, %rbx
	shrdq	$51, %rdx, %rax
	andq	%rsi, %r15
	leaq	(%rax,%rax,8), %rdx
	movq	%r15, 32(%rsp)
	movq	56(%rdi), %r15
	leaq	(%rax,%rdx,2), %rdx
	addq	%rbx, %rdx
	movq	-72(%rsp), %rbx
	movq	%rdx, %rbp
	shrq	$51, %rdx
	andq	%rsi, %rbx
	andq	%rsi, %rbp
	movq	%rbx, %rax
	movq	%r12, %rbx
	movq	%rbp, -40(%rsp)
	movq	48(%rdi), %rbp
	addq	%rdx, %rax
	andq	%rsi, %rbx
	movq	%rax, %r9
	shrq	$51, %rax
	leaq	(%rbx,%rax), %rbx
	leaq	0(%r13,%r13,8), %rax
	andq	%rsi, %r9
	leaq	0(%r13,%rax,2), %r14
	movq	64(%rdi), %r13
	movq	%r9, -16(%rsp)
	movq	%rbx, 8(%rsp)
	movq	-56(%rsp), %rbx
	leaq	0(%r13,%r13,8), %rax
	leaq	0(%r13,%rax,2), %r8
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %r13
	movq	%r13, %rax
	mulq	-104(%rsp)
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	movq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	-112(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	-80(%rsp), %rax
	adcq	%rdx, %r13
	mulq	40(%rdi)
	movq	%r12, %r10
	movq	%r13, %r11
	addq	%rax, %r10
	movq	%r9, %rax
	movq	-104(%rsp), %r9
	adcq	%rdx, %r11
	movq	%r10, -72(%rsp)
	mulq	%r9
	movq	%r11, -64(%rsp)
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	-112(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	40(%rdi)
	addq	%rax, %r12
	movq	-80(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rbp
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r8
	movq	-88(%rsp), %rax
	movq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	-112(%rsp), %rax
	movq	%r12, -56(%rsp)
	adcq	%rdx, %r9
	mulq	40(%rdi)
	movq	%r13, -48(%rsp)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	%rbp
	movq	%r8, %r10
	movq	%r9, %r11
	movq	%r12, %r9
	addq	%rax, %r10
	movq	-80(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r15
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r9
	xorl	%r13d, %r13d
	movq	%r9, %r12
	addq	%rax, %r12
	movq	%r14, %rax
	movq	-80(%rsp), %r14
	adcq	%rdx, %r13
	mulq	-104(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	-88(%rsp), %rax
	mulq	40(%rdi)
	addq	%rax, %r8
	movq	-112(%rsp), %rax
	adcq	%rdx, %r9
	movq	%r8, %r10
	movq	%r12, %r8
	mulq	%rbp
	movq	%r9, %r11
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%r15
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	64(%rdi)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r8
	xorl	%r9d, %r9d
	movq	152(%rcx), %r13
	addq	%rax, %r8
	movq	%r8, %rax
	adcq	%rdx, %r9
	andq	%rsi, %rax
	movq	%rax, -80(%rsp)
	movq	-104(%rsp), %rax
	mulq	40(%rdi)
	movq	%rax, %r10
	movq	-88(%rsp), %rax
	movq	%rdx, %r11
	mulq	%rbp
	movq	128(%rcx), %rbp
	addq	%rax, %r10
	movq	-112(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	96(%rdi), %r15
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	64(%rdi)
	addq	%rax, %r10
	movq	%r14, %rax
	movq	104(%rdi), %r14
	adcq	%rdx, %r11
	mulq	72(%rdi)
	addq	%rax, %r10
	adcq	%rdx, %r11
	shrdq	$51, %r9, %r8
	xorl	%edx, %edx
	addq	%r8, %r10
	adcq	%rdx, %r11
	movq	%r10, %rdx
	shrdq	$51, %r11, %r10
	andq	%rsi, %rdx
	movq	112(%rdi), %r11
	leaq	(%r10,%r10,8), %rax
	movq	%rdx, -24(%rsp)
	leaq	(%r10,%rax,2), %rdx
	movq	-72(%rsp), %rax
	andq	%rsi, %rax
	addq	%rax, %rdx
	movq	-56(%rsp), %rax
	movq	%rdx, %rbx
	shrq	$51, %rdx
	andq	%rsi, %rax
	andq	%rsi, %rbx
	addq	%rdx, %rax
	movq	%r12, %rdx
	andq	%rsi, %rdx
	movq	%rax, %r8
	shrq	$51, %rax
	andq	%rsi, %r8
	leaq	(%rdx,%rax), %r10
	leaq	0(%r13,%r13,8), %rax
	movq	%r8, -72(%rsp)
	leaq	0(%r13,%rax,2), %r8
	movq	144(%rcx), %r13
	movq	%r8, -104(%rsp)
	leaq	0(%r13,%r13,8), %rax
	movq	%r10, -56(%rsp)
	movq	88(%rdi), %r10
	leaq	0(%r13,%rax,2), %r8
	movq	136(%rcx), %r13
	movq	80(%rdi), %rdi
	movq	%r11, -112(%rsp)
	movq	%r10, -88(%rsp)
	leaq	0(%r13,%r13,8), %rax
	leaq	0(%r13,%rax,2), %r9
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %r13
	movq	%r13, %rax
	mulq	%r11
	movq	%rax, %r12
	movq	%r9, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	120(%rcx)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%r15
	addq	%rax, %r12
	movq	-104(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r10
	addq	%rax, %r12
	movq	%r10, %rax
	adcq	%rdx, %r13
	mulq	120(%rcx)
	movq	%r12, -8(%rsp)
	movq	%r13, (%rsp)
	movq	%rax, %r10
	movq	%rbp, %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%r9, %rax
	movq	-112(%rsp), %r9
	adcq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	-104(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r15
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	120(%rcx)
	movq	%r13, -96(%rsp)
	movq	%rax, %r10
	movq	-88(%rsp), %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	136(%rcx)
	addq	%rax, %r10
	movq	%r8, %rax
	movq	-104(%rsp), %r8
	movq	%r12, -104(%rsp)
	adcq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r14
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r13d, %r13d
	movq	%r10, %r12
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	120(%rcx)
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	movq	-88(%rsp), %rax
	adcq	%rdx, %r11
	mulq	136(%rcx)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	144(%rcx)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r9
	movq	%r12, %r9
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r9
	movq	80(%rcx), %r13
	movq	%r9, %r8
	xorl	%r9d, %r9d
	addq	%rax, %r8
	movq	%r8, %rax
	adcq	%rdx, %r9
	andq	%rsi, %rax
	movq	%rax, 16(%rsp)
	movq	-112(%rsp), %rax
	mulq	120(%rcx)
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rbp
	movq	88(%rcx), %rbp
	addq	%rax, %r10
	movq	%r15, %rax
	movabsq	$4503599627370458, %r15
	adcq	%rdx, %r11
	mulq	136(%rcx)
	addq	%rax, %r10
	movq	-88(%rsp), %rax
	adcq	%rdx, %r11
	mulq	144(%rcx)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	152(%rcx)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r9, %r8
	xorl	%r11d, %r11d
	movq	%r8, %r10
	addq	%rax, %r10
	adcq	%rdx, %r11
	movq	%r10, %rdi
	shrdq	$51, %r11, %r10
	andq	%rsi, %rdi
	movq	96(%rcx), %r11
	leaq	(%r10,%r10,8), %rax
	movq	%rdi, %rdx
	leaq	(%r10,%rax,2), %rdi
	movq	-8(%rsp), %rax
	andq	%rsi, %rax
	addq	%rax, %rdi
	movq	-104(%rsp), %rax
	movq	104(%rcx), %r10
	movq	%rbx, -112(%rsp)
	movq	%rdi, %r9
	shrq	$51, %rdi
	movq	-16(%rsp), %r14
	andq	%rsi, %rax
	andq	%rsi, %r9
	addq	%rdi, %rax
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rsi, %rdi
	movq	%rdi, %r8
	movq	%r12, %rdi
	movq	-72(%rsp), %r12
	andq	%rsi, %rdi
	addq	%rax, %rdi
	leaq	(%r13,%r13), %rax
	movq	112(%rcx), %r13
	movq	-40(%rsp), %rcx
	addq	%r15, %rcx
	addq	$36, %r15
	subq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	movq	%rcx, (%rbx)
	leaq	(%r14,%r15), %rcx
	movq	8(%rsp), %r14
	subq	%r12, %rcx
	movq	-56(%rsp), %r12
	movq	%rcx, 8(%rbx)
	leaq	(%r14,%r15), %rcx
	movq	24(%rsp), %r14
	subq	%r12, %rcx
	movq	-80(%rsp), %r12
	movq	%rcx, 16(%rbx)
	leaq	(%r14,%r15), %rcx
	subq	%r12, %rcx
	movq	32(%rsp), %r12
	movq	%rcx, 24(%rbx)
	movq	-24(%rsp), %rbx
	leaq	(%r12,%r15), %rcx
	subq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	movq	%rcx, 32(%rbx)
	movq	-112(%rsp), %rbx
	movq	-40(%rsp), %rcx
	addq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	movq	%rcx, 40(%rbx)
	movq	-72(%rsp), %rbx
	movq	-16(%rsp), %rcx
	addq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	movq	%rcx, 48(%rbx)
	movq	8(%rsp), %rbx
	movq	-56(%rsp), %rcx
	addq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	movq	%rcx, 56(%rbx)
	movq	-80(%rsp), %rcx
	addq	%r14, %rcx
	movq	%rcx, 64(%rbx)
	movq	-24(%rsp), %rcx
	addq	%r12, %rcx
	movq	%rcx, 72(%rbx)
	movq	%rax, %rcx
	andq	%rsi, %rax
	shrq	$51, %rcx
	leaq	(%rcx,%rbp,2), %rbp
	movq	%rbp, %rcx
	shrq	$51, %rcx
	leaq	(%rcx,%r11,2), %r11
	movq	%r11, %rcx
	shrq	$51, %rcx
	leaq	(%rcx,%r10,2), %r10
	movq	%r10, %rcx
	shrq	$51, %rcx
	leaq	(%rcx,%r13,2), %rcx
	movq	%rcx, %r13
	shrq	$51, %r13
	leaq	0(%r13,%r13,8), %r14
	leaq	0(%r13,%r14,2), %r13
	movq	%rbx, %r14
	addq	%r13, %rax
	movq	%rax, %r13
	shrq	$51, %r13
	andq	%rsi, %rbp
	andq	%rsi, %rax
	andq	%rsi, %r10
	addq	%r13, %rbp
	andq	%rsi, %rcx
	andq	%rsi, %r11
	movq	%rbp, %r13
	shrq	$51, %rbp
	andq	%rsi, %r13
	leaq	(%rax,%r9), %rsi
	addq	%rbp, %r11
	movq	%rsi, 80(%rbx)
	leaq	0(%r13,%r8), %rsi
	addq	%r15, %r13
	movq	%rsi, 88(%rbx)
	leaq	(%r11,%rdi), %rsi
	addq	%r15, %r11
	subq	%r8, %r13
	movq	%rsi, 96(%rbx)
	movq	16(%rsp), %rbx
	subq	%rdi, %r11
	movq	%r13, 128(%r14)
	leaq	(%r10,%rbx), %rsi
	addq	%r15, %r10
	movq	%r11, 136(%r14)
	movq	%rsi, 104(%r14)
	leaq	(%rcx,%rdx), %rsi
	addq	%r15, %rcx
	subq	%rbx, %r10
	movq	%rsi, 112(%r14)
	subq	%rdx, %rcx
	movabsq	$4503599627370458, %rsi
	addq	%rsi, %rax
	movq	%r10, 144(%r14)
	subq	%r9, %rax
	movq	%rcx, 152(%r14)
	movq	%rax, 120(%r14)
	addq	$48, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE76:
	.size	ge_madd, .-ge_madd
	.p2align 4
	.type	ge_p2_dbl, @function
ge_p2_dbl:
.LFB77:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$144, %rsp
	.cfi_def_cfa_offset 200
	movq	32(%rsi), %r12
	movq	24(%rsi), %r9
	movq	16(%rsi), %r13
	movq	8(%rsi), %rbp
	movq	%rdi, 128(%rsp)
	leaq	(%r12,%r12,8), %rax
	movq	%r12, -48(%rsp)
	movq	(%rsi), %rbx
	leaq	(%r9,%r9), %rdi
	leaq	(%r12,%rax,2), %r8
	leaq	(%r9,%r9,8), %rax
	movq	%r13, -40(%rsp)
	leaq	(%r9,%rax,2), %r11
	leaq	(%r8,%r8), %r12
	movq	%r9, 64(%rsp)
	leaq	(%r11,%r11), %r15
	leaq	(%r13,%r13), %rcx
	movq	%rbx, -32(%rsp)
	movq	%r15, %rax
	mulq	%r13
	movq	%rax, %r14
	movq	%r12, %rax
	movq	%rdx, %r15
	mulq	%rbp
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	%rbx
	addq	%rax, %r14
	movq	%r11, %rax
	adcq	%rdx, %r15
	mulq	%r9
	movq	%r14, -120(%rsp)
	movq	%r15, -112(%rsp)
	movq	%rax, %r10
	movq	%r13, %rax
	movq	%rdx, %r11
	mulq	%r12
	addq	%rax, %r10
	leaq	(%rbp,%rbp), %rax
	adcq	%rdx, %r11
	mulq	%rbx
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r14
	xorl	%r11d, %r11d
	addq	%r14, %rax
	adcq	%r11, %rdx
	movq	%rax, %r14
	movq	%r12, %rax
	movq	%rdx, %r15
	mulq	%r9
	movq	%rax, %r12
	movq	%rbx, %rax
	movq	%rdx, %r13
	mulq	%rcx
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rbp, %rax
	addq	%r12, %r10
	movq	%r14, %r12
	adcq	%r13, %r11
	mulq	%rbp
	movq	-32(%rsp), %r13
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	shrdq	$51, %r15, %r12
	xorl	%edx, %edx
	movq	56(%rsi), %r15
	addq	%r12, %r10
	movq	-48(%rsp), %r12
	adcq	%rdx, %r11
	mulq	%r12
	leaq	(%r12,%r12), %rbx
	movq	%rax, %r8
	movq	%rcx, %rax
	movq	%rdx, %r9
	movq	%r10, %rcx
	mulq	%rbp
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	%rbx, %rax
	movq	-40(%rsp), %rbx
	adcq	%rdx, %r9
	shrdq	$51, %r11, %rcx
	xorl	%edx, %edx
	addq	%rcx, %r8
	movabsq	$2251799813685247, %rcx
	adcq	%rdx, %r9
	movq	%r8, %rdx
	andq	%rcx, %rdx
	movq	%rdx, 72(%rsp)
	mulq	%r13
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rbp
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%rbx
	addq	%rax, %r12
	adcq	%rdx, %r13
	shrdq	$51, %r9, %r8
	xorl	%edx, %edx
	addq	%r8, %r12
	adcq	%rdx, %r13
	movq	%r12, %r8
	movq	-120(%rsp), %rdx
	andq	%rcx, %r14
	shrdq	$51, %r13, %r12
	andq	%rcx, %r10
	andq	%rcx, %r8
	leaq	(%r12,%r12,8), %rax
	andq	%rcx, %rdx
	movq	%r8, 80(%rsp)
	leaq	(%r12,%rax,2), %rax
	addq	%rdx, %rax
	movq	%rax, %rdi
	shrq	$51, %rax
	addq	%rax, %r14
	andq	%rcx, %rdi
	movq	%r14, %rdx
	shrq	$51, %r14
	movq	%rdi, 88(%rsp)
	leaq	(%r15,%r15), %rdi
	andq	%rcx, %rdx
	leaq	(%r10,%r14), %r11
	movq	%rdx, 16(%rsp)
	movq	72(%rsi), %rdx
	movq	%r11, 24(%rsp)
	leaq	(%rdx,%rdx,8), %rax
	movq	%rdx, -88(%rsp)
	leaq	(%rdx,%rax,2), %r9
	movq	64(%rsi), %rdx
	movq	48(%rsi), %r12
	movq	%r15, -56(%rsp)
	leaq	(%r9,%r9), %r8
	movq	40(%rsi), %r11
	leaq	(%rdx,%rdx,8), %rax
	movq	%rdx, -72(%rsp)
	leaq	(%rdx,%rdx), %rbx
	leaq	(%rdx,%rax,2), %r10
	movq	%r11, %r13
	movq	%r12, -120(%rsp)
	leaq	(%r10,%r10), %r14
	movq	%r13, -104(%rsp)
	movq	%r14, %rax
	mulq	%r15
	movq	%rax, %r14
	movq	%r12, %rax
	movq	%rdx, %r15
	mulq	%r8
	addq	%rax, %r14
	movq	%r11, %rax
	adcq	%rdx, %r15
	mulq	%r11
	addq	%rax, %r14
	movq	%r10, %rax
	adcq	%rdx, %r15
	mulq	-72(%rsp)
	movq	%r14, -8(%rsp)
	movq	%r15, (%rsp)
	movq	%rax, %r10
	movq	-56(%rsp), %rax
	movq	%rdx, %r11
	mulq	%r8
	addq	%rax, %r10
	leaq	(%r12,%r12), %rax
	movq	%r14, %r12
	adcq	%rdx, %r11
	mulq	%r13
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r12
	xorl	%r15d, %r15d
	movq	%r12, %r14
	addq	%rax, %r14
	movq	%r8, %rax
	movq	-120(%rsp), %r8
	adcq	%rdx, %r15
	mulq	-72(%rsp)
	movq	%rax, %r10
	movq	-104(%rsp), %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r14, %r11
	shrdq	$51, %r15, %r11
	movq	%r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r9, %rax
	movq	%rdx, %r13
	mulq	-88(%rsp)
	movq	%r12, %r10
	movq	%rax, %r8
	movq	%rdi, %rax
	movq	-120(%rsp), %rdi
	movq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	-104(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r9d, %r9d
	movq	%r10, %r8
	addq	%rax, %r8
	movq	%r8, %r10
	adcq	%rdx, %r9
	andq	%rcx, %r10
	movq	%r10, -24(%rsp)
	movq	-88(%rsp), %r10
	addq	%r10, %r10
	movq	%r10, %rax
	mulq	-104(%rsp)
	movq	%rax, %r10
	movq	%rbx, %rax
	movq	%rdx, %r11
	movq	112(%rsi), %rbx
	mulq	%rdi
	movq	-56(%rsp), %rdi
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	88(%rsi), %rdi
	addq	%rax, %r10
	adcq	%rdx, %r11
	shrdq	$51, %r9, %r8
	xorl	%edx, %edx
	addq	%r8, %r10
	adcq	%rdx, %r11
	movq	%r10, %r8
	andq	%rcx, %r14
	shrdq	$51, %r11, %r10
	andq	%rcx, %r8
	leaq	(%r10,%r10,8), %rax
	movq	%r8, -16(%rsp)
	leaq	(%r10,%rax,2), %rdx
	movq	-8(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	%r14, %rax
	movq	%rdx, %r8
	shrq	$51, %rdx
	addq	%rdx, %rax
	andq	%rcx, %r8
	movq	%rax, %r14
	shrq	$51, %rax
	movq	%r8, 56(%rsp)
	movq	104(%rsi), %r8
	andq	%rcx, %r14
	movq	%r14, -8(%rsp)
	movq	%r12, %r14
	leaq	(%r8,%r8), %r15
	andq	%rcx, %r14
	movq	%r15, 32(%rsp)
	leaq	(%r14,%rax), %r14
	leaq	(%rbx,%rbx,8), %rax
	movq	%r14, 8(%rsp)
	leaq	(%rbx,%rax,2), %r14
	leaq	(%r8,%r8,8), %rax
	leaq	(%r14,%r14), %r9
	movq	%r14, 120(%rsp)
	movq	96(%rsi), %r14
	leaq	(%r14,%r14), %r10
	movq	%r14, %r12
	movq	%r10, 40(%rsp)
	leaq	(%r8,%rax,2), %r10
	movq	80(%rsi), %rsi
	leaq	(%r10,%r10), %r15
	movq	%r12, 96(%rsp)
	movq	%r15, %rax
	mulq	%r14
	movq	%rax, %r14
	movq	%r9, %rax
	movq	%rdx, %r15
	mulq	%rdi
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	%rsi
	addq	%rax, %r14
	movq	%r10, %rax
	adcq	%rdx, %r15
	mulq	%r8
	movq	%r14, 104(%rsp)
	movq	%r15, 112(%rsp)
	movq	%rax, %r10
	movq	%r12, %rax
	movq	%rdx, %r11
	movq	%r14, %r12
	mulq	%r9
	addq	%rax, %r10
	leaq	(%rdi,%rdi), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r12
	xorl	%r15d, %r15d
	movq	%r12, %r14
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	%r9
	movq	%r14, %r10
	movq	%rax, %r8
	movq	40(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r10
	xorl	%r13d, %r13d
	movq	%r10, %r12
	addq	%rax, %r12
	movq	120(%rsp), %rax
	adcq	%rdx, %r13
	movq	%r12, %r10
	mulq	%rbx
	movq	%rax, %r8
	movq	40(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	32(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r9d, %r9d
	movq	-32(%rsp), %r13
	movq	%r10, %r8
	leaq	(%rbx,%rbx), %r10
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	movq	%r8, 40(%rsp)
	movq	96(%rsp), %r8
	movq	%r9, 48(%rsp)
	movq	48(%rsp), %r9
	movq	%rax, %r10
	movq	32(%rsp), %rax
	movq	%rdx, %r11
	movq	%r10, %rsi
	movq	104(%rsp), %r10
	mulq	%rdi
	movq	%r11, %rdi
	addq	%rax, %rsi
	movq	%r8, %rax
	adcq	%rdx, %rdi
	mulq	%r8
	movq	40(%rsp), %r8
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r8
	xorl	%edx, %edx
	addq	%r8, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%rcx, %r10
	andq	%rcx, %r14
	shrdq	$51, %rdi, %rax
	andq	%rcx, %rsi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rax
	addq	%r10, %rax
	movq	%rax, %r15
	andq	%rcx, %rax
	shrq	$51, %r15
	leaq	(%rax,%rax), %r9
	shrq	$50, %rax
	addq	%r15, %r14
	andq	%rcx, %r9
	movq	%r14, %r15
	shrq	$51, %r14
	andq	%rcx, %r15
	leaq	(%rax,%r15,2), %r10
	movq	%r12, %rax
	movq	%r15, %rdx
	movq	-48(%rsp), %r12
	andq	%rcx, %rax
	shrq	$50, %rdx
	andq	%rcx, %r10
	addq	%rax, %r14
	movq	40(%rsp), %rax
	leaq	(%rdx,%r14,2), %r14
	movq	%r14, 32(%rsp)
	andq	%rcx, %rax
	shrq	$51, %r14
	leaq	(%r14,%rax,2), %r8
	movq	%r8, 40(%rsp)
	shrq	$51, %r8
	leaq	(%r8,%rsi,2), %rsi
	movq	%rsi, 96(%rsp)
	shrq	$51, %rsi
	leaq	(%rsi,%rsi,8), %rdx
	leaq	(%rsi,%rdx,2), %rax
	movq	-72(%rsp), %rdx
	movq	-104(%rsp), %rsi
	leaq	(%rax,%r9), %r15
	movq	64(%rsp), %r9
	movq	%r15, 104(%rsp)
	shrq	$51, %r15
	leaq	(%rsi,%r13), %rbx
	movq	-120(%rsp), %rsi
	leaq	(%r15,%r10), %r8
	movq	-40(%rsp), %r13
	movq	-56(%rsp), %r15
	movq	%r8, 120(%rsp)
	leaq	(%rdx,%r9), %r8
	movq	-88(%rsp), %rdx
	addq	%rbp, %rsi
	leaq	(%r15,%r13), %rbp
	leaq	(%rdx,%r12), %rdi
	leaq	(%rbp,%rbp), %r11
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%r8,%r8), %r12
	leaq	(%rdi,%rax,2), %r9
	leaq	(%r8,%r8,8), %rax
	movq	%r12, -120(%rsp)
	leaq	(%r8,%rax,2), %r10
	movq	%r9, -104(%rsp)
	addq	%r9, %r9
	leaq	(%r10,%r10), %r13
	movq	%r11, -56(%rsp)
	movq	%r13, %rax
	mulq	%rbp
	movq	%rax, %r14
	movq	%r9, %rax
	movq	%rdx, %r15
	mulq	%rsi
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	%rbx
	addq	%rax, %r14
	movq	%r10, %rax
	adcq	%rdx, %r15
	mulq	%r8
	movq	%r14, %r12
	movq	%rax, %r10
	movq	%r9, %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	leaq	(%rsi,%rsi), %rax
	adcq	%rdx, %r11
	mulq	%rbx
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r12
	xorl	%r11d, %r11d
	movq	80(%rsp), %r15
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r9
	movq	%r12, %r10
	movq	%rax, %r8
	movq	-56(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r9d, %r9d
	movq	72(%rsp), %r13
	addq	%r10, %rax
	movq	%rax, %r10
	movq	-104(%rsp), %rax
	adcq	%r9, %rdx
	movq	%rdx, %r11
	movq	%r10, -104(%rsp)
	mulq	%rdi
	movq	%r11, -96(%rsp)
	movq	%rax, %r8
	movq	-56(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	-120(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%r8, %rax
	leaq	(%rdi,%rdi), %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	mulq	%rbx
	movq	%r10, -88(%rsp)
	movq	%r11, -80(%rsp)
	movq	%rax, %r8
	movq	-120(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rbp, %rax
	addq	%r8, %rsi
	movq	%r10, %r8
	adcq	%r9, %rdi
	mulq	%rbp
	movq	-8(%rsp), %rbp
	addq	%rsi, %rax
	movq	88(%rsp), %rsi
	adcq	%rdi, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	movq	%rsi, %r11
	adcq	%rdi, %rdx
	movq	%rax, -72(%rsp)
	andq	%rcx, %r14
	andq	%rcx, %r12
	shrdq	$51, %rdx, %rax
	movq	%rdx, -64(%rsp)
	movq	56(%rsp), %rdi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	addq	%rdi, %r11
	movq	128(%rsp), %rax
	addq	%rdx, %r14
	movq	8(%rsp), %rdx
	movq	%r14, %r9
	movq	%r11, 40(%rax)
	andq	%rcx, %r14
	shrq	$51, %r9
	leaq	(%r12,%r9), %rbx
	movq	16(%rsp), %r9
	movq	24(%rsp), %r12
	movq	%rbx, -48(%rsp)
	movq	-16(%rsp), %rbx
	leaq	0(%rbp,%r9), %r10
	movq	-24(%rsp), %rbp
	leaq	(%rdx,%r12), %r9
	movabsq	$4503599627370494, %rdx
	movq	%r10, 48(%rax)
	leaq	0(%rbp,%r13), %r8
	leaq	(%rbx,%r15), %rbp
	movq	%r9, 56(%rax)
	movabsq	$4503599627370458, %rbx
	leaq	(%rdi,%rbx), %rdi
	movq	%r8, 64(%rax)
	addq	%rbx, %r14
	subq	%rsi, %rdi
	movq	-8(%rsp), %rsi
	movq	%rbp, 72(%rax)
	addq	%rdx, %rsi
	movq	%rsi, %r12
	movq	16(%rsp), %rsi
	subq	%rsi, %r12
	movq	%r12, -120(%rsp)
	movq	8(%rsp), %r12
	leaq	(%r12,%rdx), %rsi
	movq	%rsi, %r12
	movq	24(%rsp), %rsi
	movq	%rdi, 80(%rax)
	subq	%rsi, %r12
	movq	-24(%rsp), %rsi
	movq	%r12, -56(%rsp)
	movq	-120(%rsp), %r12
	addq	%rdx, %rsi
	subq	%r13, %rsi
	movq	%r12, 88(%rax)
	movq	-56(%rsp), %r12
	movq	%rsi, %r13
	movq	-16(%rsp), %rsi
	movq	%r12, 96(%rax)
	addq	%rdx, %rsi
	movq	%r13, 104(%rax)
	subq	%r15, %rsi
	movq	%rsi, 112(%rax)
	movq	%rsi, %r15
	movq	%r11, %rsi
	andq	%rcx, %r11
	shrq	$51, %rsi
	addq	%r10, %rsi
	movq	%rsi, %r10
	andq	%rcx, %rsi
	shrq	$51, %r10
	addq	%r9, %r10
	movq	%r10, %r9
	shrq	$51, %r9
	addq	%r8, %r9
	movq	%r9, %r8
	shrq	$51, %r8
	addq	%rbp, %r8
	movq	%r8, %rbp
	shrq	$51, %rbp
	leaq	0(%rbp,%rbp,8), %r12
	leaq	0(%rbp,%r12,2), %rbp
	addq	%rbp, %r11
	movq	%r11, %rbp
	andq	%rcx, %r11
	shrq	$51, %rbp
	addq	%rbp, %rsi
	subq	%r11, %r14
	andq	%rcx, %r10
	andq	%rcx, %r9
	movq	%r14, (%rax)
	movq	-48(%rsp), %r14
	movq	%rsi, %rbp
	shrq	$51, %rsi
	andq	%rcx, %rbp
	andq	%rcx, %r8
	movq	%r14, %r11
	shrq	$51, %r14
	andq	%rcx, %r11
	movq	%r14, %r12
	movq	%rax, %r14
	addq	%rdx, %r11
	subq	%rbp, %r11
	movq	%r11, 8(%rax)
	movq	-104(%rsp), %r11
	andq	%rcx, %r11
	addq	%rdx, %r11
	addq	%r11, %r12
	movq	-88(%rsp), %r11
	subq	%r10, %r12
	andq	%rcx, %r11
	subq	%rsi, %r12
	movq	%r11, %rsi
	movq	%r12, 16(%rax)
	movq	-120(%rsp), %r12
	addq	%rdx, %rsi
	subq	%r9, %rsi
	movq	%rsi, 24(%rax)
	movq	-72(%rsp), %rax
	andq	%rcx, %rax
	addq	%rdx, %rax
	subq	%r8, %rax
	movq	%rax, 32(%r14)
	movq	%rdi, %rax
	andq	%rcx, %rdi
	shrq	$51, %rax
	movq	%rax, %r8
	addq	%r12, %r8
	movq	-56(%rsp), %r12
	movq	%r8, %r9
	shrq	$51, %r9
	addq	%r12, %r9
	movq	%r9, %rax
	shrq	$51, %rax
	movq	%rax, %rsi
	addq	%r13, %rsi
	movq	%rsi, %rax
	shrq	$51, %rax
	addq	%r15, %rax
	movq	%rax, %r11
	shrq	$51, %r11
	movq	%r11, %r10
	leaq	(%r11,%r11,8), %r11
	leaq	(%r10,%r11,2), %r10
	addq	%r10, %rdi
	movq	104(%rsp), %r10
	movq	%rdi, %r11
	shrq	$51, %r11
	andq	%rcx, %r10
	andq	%rcx, %rdi
	andq	%rcx, %r8
	addq	%rbx, %r10
	movq	120(%rsp), %rbx
	addq	%r11, %r8
	andq	%rcx, %r9
	subq	%rdi, %r10
	andq	%rcx, %rsi
	andq	%rcx, %rax
	movq	%rbx, %r15
	movq	%r10, 120(%r14)
	movq	%r8, %r10
	shrq	$51, %rbx
	andq	%rcx, %r15
	andq	%rcx, %r10
	shrq	$51, %r8
	movq	%r15, %rdi
	addq	%rdx, %rdi
	subq	%r10, %rdi
	movq	%rdi, 128(%r14)
	movq	32(%rsp), %rdi
	andq	%rcx, %rdi
	addq	%rdx, %rdi
	subq	%r9, %rdi
	addq	%rbx, %rdi
	subq	%r8, %rdi
	movq	%rdi, 136(%r14)
	movq	40(%rsp), %rdi
	andq	%rcx, %rdi
	addq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	96(%rsp), %rsi
	movq	%rdi, 144(%r14)
	andq	%rcx, %rsi
	addq	%rsi, %rdx
	subq	%rax, %rdx
	movq	%rdx, 152(%r14)
	addq	$144, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE77:
	.size	ge_p2_dbl, .-ge_p2_dbl
	.p2align 4
	.globl	SHA512_Init
	.type	SHA512_Init, @function
SHA512_Init:
.LFB8:
	.cfi_startproc
	movdqa	.LC0(%rip), %xmm0
	movq	.LC4(%rip), %rax
	movups	%xmm0, (%rdi)
	movdqa	.LC1(%rip), %xmm0
	movq	%rax, 208(%rdi)
	movl	$1, %eax
	movups	%xmm0, 16(%rdi)
	movdqa	.LC2(%rip), %xmm0
	movups	%xmm0, 32(%rdi)
	movdqa	.LC3(%rip), %xmm0
	movups	%xmm0, 48(%rdi)
	pxor	%xmm0, %xmm0
	movups	%xmm0, 64(%rdi)
	ret
	.cfi_endproc
.LFE8:
	.size	SHA512_Init, .-SHA512_Init
	.p2align 4
	.globl	SHA512_Final
	.type	SHA512_Final, @function
SHA512_Final:
.LFB9:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	leaq	80(%rsi), %r13
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rsi, %rbx
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	movl	208(%rsi), %eax
	movl	212(%rsi), %r12d
	movb	$-128, 80(%rsi,%rax)
	addq	$1, %rax
	leaq	0(%r13,%rax), %rsi
	cmpq	$112, %rax
	jbe	.L90
	movl	$128, %edx
	subq	%rax, %rdx
	jne	.L150
.L91:
	movl	$1, %edx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	call	sha512_block_data_order
	movq	%r13, %rsi
	movl	$112, %edx
.L98:
	movl	%edx, %ecx
	cmpl	$8, %edx
	jb	.L151
	leaq	8(%rsi), %rdi
	movl	%edx, %eax
	movq	$0, (%rsi)
	movq	$0, -8(%rsi,%rax)
	andq	$-8, %rdi
	xorl	%eax, %eax
	subq	%rdi, %rsi
	leal	(%rdx,%rsi), %ecx
	shrl	$3, %ecx
	rep stosq
.L99:
	movq	72(%rbx), %rax
	movq	64(%rbx), %rdx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	bswap	%rdx
	bswap	%rax
	movq	%rdx, %xmm1
	movq	%rax, %xmm0
	punpcklqdq	%xmm1, %xmm0
	movl	$1, %edx
	movups	%xmm0, 192(%rbx)
	call	sha512_block_data_order
	testq	%rbp, %rbp
	je	.L108
	shrq	$3, %r12
	je	.L107
	movq	(%rbx), %rax
	bswap	%rax
	movq	%rax, 0(%rbp)
	cmpq	$1, %r12
	je	.L107
	movq	8(%rbx), %rax
	bswap	%rax
	movq	%rax, 8(%rbp)
	cmpq	$2, %r12
	je	.L107
	movq	16(%rbx), %rax
	bswap	%rax
	movq	%rax, 16(%rbp)
	cmpq	$3, %r12
	je	.L107
	movq	24(%rbx), %rax
	bswap	%rax
	movq	%rax, 24(%rbp)
	cmpq	$4, %r12
	je	.L107
	movq	32(%rbx), %rax
	bswap	%rax
	movq	%rax, 32(%rbp)
	cmpq	$5, %r12
	je	.L107
	movq	40(%rbx), %rax
	bswap	%rax
	movq	%rax, 40(%rbp)
	cmpq	$6, %r12
	je	.L107
	movq	48(%rbx), %rax
	bswap	%rax
	movq	%rax, 48(%rbp)
	cmpq	$7, %r12
	je	.L107
	movq	56(%rbx), %rax
	bswap	%rax
	movq	%rax, 56(%rbp)
.L107:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	movl	$1, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L151:
	.cfi_restore_state
	andl	$4, %edx
	jne	.L152
	testl	%ecx, %ecx
	je	.L99
	movb	$0, (%rsi)
	testb	$2, %cl
	je	.L99
	xorl	%eax, %eax
	movw	%ax, -2(%rsi,%rcx)
	jmp	.L99
	.p2align 4,,10
	.p2align 3
.L150:
	xorl	%edi, %edi
	cmpq	$8, %rdx
	jb	.L153
	leaq	8(%rsi), %rcx
	movq	$0, (%rsi)
	movq	$0, -8(%rsi,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rsi
	addq	%rsi, %rdx
	andq	$-8, %rdx
	cmpq	$8, %rdx
	jb	.L91
	andq	$-8, %rdx
	xorl	%eax, %eax
.L96:
	movq	%rdi, (%rcx,%rax)
	addq	$8, %rax
	cmpq	%rdx, %rax
	jb	.L96
	jmp	.L91
	.p2align 4,,10
	.p2align 3
.L90:
	movl	$112, %edx
	subq	%rax, %rdx
	je	.L99
	jmp	.L98
	.p2align 4,,10
	.p2align 3
.L153:
	testb	$4, %dl
	jne	.L154
	testq	%rdx, %rdx
	je	.L91
	movb	$0, (%rsi)
	testb	$2, %dl
	je	.L91
	xorl	%ecx, %ecx
	movw	%cx, -2(%rsi,%rdx)
	jmp	.L91
	.p2align 4,,10
	.p2align 3
.L108:
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
.L152:
	.cfi_restore_state
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rcx)
	jmp	.L99
.L154:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rdx)
	jmp	.L91
	.cfi_endproc
.LFE9:
	.size	SHA512_Final, .-SHA512_Final
	.p2align 4
	.globl	OPENSSL_cleanse
	.type	OPENSSL_cleanse, @function
OPENSSL_cleanse:
.LFB10:
	.cfi_startproc
	testq	%rsi, %rsi
	jne	.L164
	ret
	.p2align 4,,10
	.p2align 3
.L164:
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movq	%rsi, %rdx
	xorl	%esi, %esi
	call	memset
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE10:
	.size	OPENSSL_cleanse, .-OPENSSL_cleanse
	.p2align 4
	.globl	x25519_ge_p1p1_to_p2
	.type	x25519_ge_p1p1_to_p2, @function
x25519_ge_p1p1_to_p2:
.LFB35:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %r10
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	152(%rsi), %rax
	movq	152(%rsi), %rbx
	movq	136(%rsi), %r11
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rdi
	movq	144(%rsi), %rax
	movq	144(%rsi), %rbx
	movq	128(%rsi), %rsi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rcx
	leaq	(%r11,%r11,8), %rax
	leaq	(%r11,%rax,2), %r8
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rbx
	movq	%rbx, %rax
	mulq	32(%r10)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r8, %rax
	mulq	24(%r10)
	addq	%rax, %r14
	movq	120(%r10), %rax
	adcq	%rdx, %r15
	mulq	(%r10)
	addq	%rax, %r14
	movq	%rcx, %rax
	adcq	%rdx, %r15
	mulq	16(%r10)
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	8(%r10)
	addq	%rax, %r14
	movq	8(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, -104(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	mulq	(%r10)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	32(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	addq	%r14, %r8
	adcq	%r15, %r9
	mulq	24(%r10)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	16(%r10)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r15d, %r15d
	movq	%r12, %r14
	addq	%rax, %r14
	movq	16(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rsi, %rax
	mulq	8(%r10)
	addq	%rax, %r12
	movq	%r11, %rax
	adcq	%rdx, %r13
	mulq	(%r10)
	addq	%rax, %r12
	movq	%rcx, %rax
	movq	%r14, %rcx
	adcq	%rdx, %r13
	mulq	32(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	24(%r10)
	addq	%rax, %r8
	movq	24(%r10), %rax
	adcq	%rdx, %r9
	shrdq	$51, %r15, %rcx
	xorl	%edx, %edx
	addq	%rcx, %r8
	adcq	%rdx, %r9
	mulq	120(%r10)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	mulq	16(%r10)
	addq	%rax, %rcx
	movq	%r11, %rax
	adcq	%rdx, %rbx
	mulq	8(%r10)
	addq	%rax, %rcx
	movq	144(%r10), %rax
	adcq	%rdx, %rbx
	mulq	(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	addq	%rcx, %r12
	adcq	%rbx, %r13
	mulq	32(%r10)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	shrdq	$51, %r9, %rax
	xorl	%ebx, %ebx
	addq	%rax, %r12
	movq	32(%r10), %rax
	adcq	%rbx, %r13
	mulq	120(%r10)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%r12, %rsi
	mulq	24(%r10)
	addq	%rax, %rcx
	movq	%r11, %rax
	adcq	%rdx, %rbx
	mulq	16(%r10)
	addq	%rax, %rcx
	movq	8(%r10), %rax
	adcq	%rdx, %rbx
	mulq	144(%r10)
	addq	%rax, %rcx
	movq	(%r10), %rax
	adcq	%rdx, %rbx
	mulq	152(%r10)
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rsi
	xorl	%edx, %edx
	movq	%rsi, %rax
	movq	-104(%rsp), %rsi
	addq	%rcx, %rax
	movabsq	$2251799813685247, %rcx
	adcq	%rbx, %rdx
	movq	%rax, -88(%rsp)
	andq	%rcx, %rsi
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r8
	andq	%rcx, %r12
	leaq	(%rax,%rax,8), %rdx
	movq	%r12, 24(%rbp)
	leaq	(%rax,%rdx,2), %rdx
	movq	-88(%rsp), %rax
	addq	%rsi, %rdx
	movq	%rdx, %rsi
	andq	%rcx, %rdx
	andq	%rcx, %rax
	shrq	$51, %rsi
	movq	%rdx, 0(%rbp)
	addq	%rsi, %r14
	movq	%rax, 32(%rbp)
	movq	%r14, %rdx
	shrq	$51, %r14
	addq	%r14, %r8
	andq	%rcx, %rdx
	movq	%r8, 16(%rbp)
	movq	%rdx, 8(%rbp)
	movq	112(%r10), %rax
	movq	112(%r10), %rbx
	movq	88(%r10), %rdi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %r11
	movq	104(%r10), %rax
	movq	104(%r10), %rbx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rsi
	movq	96(%r10), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r8
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r9
	movq	%r9, %rax
	mulq	72(%r10)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r8, %rax
	mulq	64(%r10)
	addq	%rax, %r14
	movq	80(%r10), %rax
	adcq	%rdx, %r15
	mulq	40(%r10)
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	56(%r10)
	addq	%rax, %r14
	movq	%r11, %rax
	adcq	%rdx, %r15
	mulq	48(%r10)
	addq	%rax, %r14
	movq	48(%r10), %rax
	adcq	%rdx, %r15
	mulq	80(%r10)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, -104(%rsp)
	movq	%r13, -96(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rdi, %rax
	mulq	40(%r10)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	72(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r14, %r8
	adcq	%r15, %r9
	mulq	64(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	56(%r10)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r9d, %r9d
	addq	%r12, %rax
	adcq	%r9, %rdx
	movq	%rax, %r14
	movq	56(%r10), %rax
	movq	%rdx, %r15
	mulq	80(%r10)
	movq	%r14, -88(%rsp)
	movq	%r15, -80(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	48(%r10)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	40(%r10)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	72(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	64(%r10)
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r8
	xorl	%r15d, %r15d
	movq	%r8, %r14
	addq	%rax, %r14
	movq	64(%r10), %rax
	adcq	%rdx, %r15
	mulq	80(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	56(%r10)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	48(%r10)
	addq	%rax, %r12
	movq	104(%r10), %rax
	adcq	%rdx, %r13
	mulq	40(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r11, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	72(%r10)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r15, %rax
	xorl	%r13d, %r13d
	addq	%rax, %r8
	movq	72(%r10), %rax
	adcq	%r13, %r9
	mulq	80(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	64(%r10)
	movq	%r12, %rsi
	movq	%r13, %rdi
	addq	%rax, %rsi
	movq	%rbx, %rax
	movq	%r8, %rbx
	adcq	%rdx, %rdi
	mulq	56(%r10)
	addq	%rax, %rsi
	movq	48(%r10), %rax
	adcq	%rdx, %rdi
	mulq	104(%r10)
	addq	%rax, %rsi
	movq	40(%r10), %rax
	adcq	%rdx, %rdi
	mulq	112(%r10)
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r9, %rbx
	xorl	%edi, %edi
	addq	%rbx, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r11
	andq	%rcx, %r14
	andq	%rcx, %r8
	shrdq	$51, %rdx, %rax
	movq	%r8, 64(%rbp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rsi
	movq	-104(%rsp), %rdx
	movq	%r11, %rax
	andq	%rcx, %rax
	andq	%rcx, %rdx
	movq	%rax, 72(%rbp)
	addq	%rdx, %rsi
	movq	-88(%rsp), %rdx
	movq	%rsi, %rdi
	andq	%rcx, %rsi
	shrq	$51, %rdi
	andq	%rcx, %rdx
	movq	%rsi, 40(%rbp)
	addq	%rdi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	addq	%rdx, %r14
	andq	%rcx, %rsi
	movq	%rsi, 48(%rbp)
	movq	%r14, 56(%rbp)
	movq	152(%r10), %rbx
	movq	136(%r10), %r8
	movq	112(%r10), %r14
	leaq	(%rbx,%rbx,8), %rax
	movq	%rbx, -16(%rsp)
	movq	120(%r10), %r15
	leaq	(%rbx,%rax,2), %rax
	movq	144(%r10), %rbx
	movq	80(%r10), %r9
	movq	%rax, -72(%rsp)
	movq	96(%r10), %r11
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rdi
	leaq	(%r8,%r8,8), %rax
	movq	%r11, -88(%rsp)
	movq	%rdi, -56(%rsp)
	leaq	(%r8,%rax,2), %rsi
	movq	128(%r10), %rdi
	movq	88(%r10), %rax
	movq	%rsi, -40(%rsp)
	movq	104(%r10), %rsi
	movq	%rax, -104(%rsp)
	leaq	(%rdi,%rdi,8), %rax
	movq	-104(%rsp), %r10
	leaq	(%rdi,%rax,2), %r13
	movq	%r13, %rax
	mulq	%r14
	movq	%rax, %r12
	movq	-40(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r15, %rax
	adcq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	-56(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r11
	addq	%rax, %r12
	movq	-72(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r10
	addq	%rax, %r12
	movq	%r10, %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	-40(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	-56(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	-72(%rsp), %rax
	adcq	%rdx, %r11
	mulq	-88(%rsp)
	movq	%r12, -40(%rsp)
	movq	%r13, -32(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	adcq	%r11, %rdx
	movq	%rdx, %r13
	mulq	%r15
	movq	%r13, -48(%rsp)
	movq	%rax, %r10
	movq	-104(%rsp), %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	-56(%rsp), %rax
	movq	%r12, -56(%rsp)
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	-72(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	shrdq	$51, %r13, %r11
	movq	%r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%r15
	movq	%r11, -64(%rsp)
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	-104(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	-72(%rsp), %rax
	movq	%r10, -72(%rsp)
	adcq	%rdx, %r13
	mulq	%r14
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r11d, %r11d
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	%rax, %r14
	movq	%rsi, %rax
	movq	%rdx, %r15
	mulq	%rdi
	movq	%r14, %rsi
	movq	%r15, %rdi
	addq	%rax, %rsi
	movq	-88(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%r8
	movq	%r10, %r8
	addq	%rax, %rsi
	movq	-104(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%rbx
	addq	%rax, %rsi
	movq	-16(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%r9
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	movq	%r8, %rbx
	movq	%rdi, %rsi
	addq	%rax, %rbx
	adcq	%rdx, %rsi
	movq	%rbx, %rax
	andq	%rcx, %r10
	shrdq	$51, %rsi, %rax
	movq	%r10, 104(%rbp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	-40(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	-56(%rsp), %rax
	movq	%rdx, %rdi
	andq	%rcx, %rdx
	andq	%rcx, %rax
	shrq	$51, %rdi
	movq	%rdx, 80(%rbp)
	addq	%rdi, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rcx, %rdx
	movq	%rdx, 88(%rbp)
	movq	-72(%rsp), %rdx
	andq	%rcx, %rdx
	addq	%rdx, %rax
	movq	%rax, 96(%rbp)
	movq	%rbx, %rax
	andq	%rcx, %rax
	movq	%rax, 112(%rbp)
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE35:
	.size	x25519_ge_p1p1_to_p2, .-x25519_ge_p1p1_to_p2
	.p2align 4
	.globl	x25519_ge_p1p1_to_p3
	.type	x25519_ge_p1p1_to_p3, @function
x25519_ge_p1p1_to_p3:
.LFB36:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %r10
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	152(%rsi), %rax
	movq	152(%rsi), %rbx
	movq	136(%rsi), %r11
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rdi
	movq	144(%rsi), %rax
	movq	144(%rsi), %rbx
	movq	128(%rsi), %rsi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rcx
	leaq	(%r11,%r11,8), %rax
	leaq	(%r11,%rax,2), %r8
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rbx
	movq	%rbx, %rax
	mulq	32(%r10)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r8, %rax
	mulq	24(%r10)
	addq	%rax, %r14
	movq	120(%r10), %rax
	adcq	%rdx, %r15
	mulq	(%r10)
	addq	%rax, %r14
	movq	%rcx, %rax
	adcq	%rdx, %r15
	mulq	16(%r10)
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	8(%r10)
	addq	%rax, %r14
	movq	8(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, -104(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	mulq	(%r10)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	32(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	addq	%r14, %r8
	adcq	%r15, %r9
	mulq	24(%r10)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	16(%r10)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r15d, %r15d
	movq	%r12, %r14
	addq	%rax, %r14
	movq	16(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rsi, %rax
	mulq	8(%r10)
	addq	%rax, %r12
	movq	%r11, %rax
	adcq	%rdx, %r13
	mulq	(%r10)
	addq	%rax, %r12
	movq	%rcx, %rax
	movq	%r14, %rcx
	adcq	%rdx, %r13
	mulq	32(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	24(%r10)
	addq	%rax, %r8
	movq	24(%r10), %rax
	adcq	%rdx, %r9
	shrdq	$51, %r15, %rcx
	xorl	%edx, %edx
	addq	%rcx, %r8
	adcq	%rdx, %r9
	mulq	120(%r10)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	mulq	16(%r10)
	addq	%rax, %rcx
	movq	%r11, %rax
	adcq	%rdx, %rbx
	mulq	8(%r10)
	addq	%rax, %rcx
	movq	144(%r10), %rax
	adcq	%rdx, %rbx
	mulq	(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	addq	%rcx, %r12
	adcq	%rbx, %r13
	mulq	32(%r10)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	shrdq	$51, %r9, %rax
	xorl	%ebx, %ebx
	addq	%rax, %r12
	movq	32(%r10), %rax
	adcq	%rbx, %r13
	mulq	120(%r10)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%r12, %rsi
	mulq	24(%r10)
	addq	%rax, %rcx
	movq	%r11, %rax
	adcq	%rdx, %rbx
	mulq	16(%r10)
	addq	%rax, %rcx
	movq	8(%r10), %rax
	adcq	%rdx, %rbx
	mulq	144(%r10)
	addq	%rax, %rcx
	movq	(%r10), %rax
	adcq	%rdx, %rbx
	mulq	152(%r10)
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rsi
	xorl	%edx, %edx
	movq	%rsi, %rax
	movq	-104(%rsp), %rsi
	addq	%rcx, %rax
	movabsq	$2251799813685247, %rcx
	adcq	%rbx, %rdx
	movq	%rax, -88(%rsp)
	andq	%rcx, %rsi
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r8
	andq	%rcx, %r12
	leaq	(%rax,%rax,8), %rdx
	movq	%r12, 24(%rbp)
	leaq	(%rax,%rdx,2), %rdx
	movq	-88(%rsp), %rax
	addq	%rsi, %rdx
	movq	%rdx, %rsi
	andq	%rcx, %rdx
	andq	%rcx, %rax
	shrq	$51, %rsi
	movq	%rdx, 0(%rbp)
	addq	%rsi, %r14
	movq	%rax, 32(%rbp)
	movq	%r14, %rdx
	shrq	$51, %r14
	addq	%r14, %r8
	andq	%rcx, %rdx
	movq	%r8, 16(%rbp)
	movq	%rdx, 8(%rbp)
	movq	112(%r10), %rax
	movq	112(%r10), %rbx
	movq	88(%r10), %rdi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %r11
	movq	104(%r10), %rax
	movq	104(%r10), %rbx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rsi
	movq	96(%r10), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r8
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r9
	movq	%r9, %rax
	mulq	72(%r10)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r8, %rax
	mulq	64(%r10)
	addq	%rax, %r14
	movq	80(%r10), %rax
	adcq	%rdx, %r15
	mulq	40(%r10)
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	56(%r10)
	addq	%rax, %r14
	movq	%r11, %rax
	adcq	%rdx, %r15
	mulq	48(%r10)
	addq	%rax, %r14
	movq	48(%r10), %rax
	adcq	%rdx, %r15
	mulq	80(%r10)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, -104(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rdi, %rax
	mulq	40(%r10)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	72(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r14, %r8
	adcq	%r15, %r9
	mulq	64(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	56(%r10)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r9d, %r9d
	addq	%r12, %rax
	adcq	%r9, %rdx
	movq	%rax, %r14
	movq	56(%r10), %rax
	movq	%rdx, %r15
	mulq	80(%r10)
	movq	%r14, -88(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	48(%r10)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	40(%r10)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	72(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	64(%r10)
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r8
	xorl	%r15d, %r15d
	movq	%r8, %r14
	addq	%rax, %r14
	movq	64(%r10), %rax
	adcq	%rdx, %r15
	mulq	80(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	56(%r10)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	48(%r10)
	addq	%rax, %r12
	movq	104(%r10), %rax
	adcq	%rdx, %r13
	mulq	40(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r11, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	72(%r10)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r15, %rax
	xorl	%r13d, %r13d
	addq	%rax, %r8
	movq	72(%r10), %rax
	adcq	%r13, %r9
	mulq	80(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	64(%r10)
	movq	%r12, %rsi
	movq	%r13, %rdi
	addq	%rax, %rsi
	movq	%rbx, %rax
	movq	%r8, %rbx
	adcq	%rdx, %rdi
	mulq	56(%r10)
	addq	%rax, %rsi
	movq	48(%r10), %rax
	adcq	%rdx, %rdi
	mulq	104(%r10)
	addq	%rax, %rsi
	movq	40(%r10), %rax
	adcq	%rdx, %rdi
	mulq	112(%r10)
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r9, %rbx
	xorl	%edi, %edi
	addq	%rbx, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r11
	andq	%rcx, %r14
	andq	%rcx, %r8
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rsi
	movq	-104(%rsp), %rdx
	movq	%r11, %rax
	andq	%rcx, %rax
	andq	%rcx, %rdx
	addq	%rdx, %rsi
	movq	-88(%rsp), %rdx
	movq	%r8, 64(%rbp)
	movq	%rsi, %rdi
	andq	%rcx, %rsi
	movq	%rax, 72(%rbp)
	shrq	$51, %rdi
	andq	%rcx, %rdx
	movq	%rsi, 40(%rbp)
	addq	%rdi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	addq	%rdx, %r14
	andq	%rcx, %rsi
	movq	%rsi, 48(%rbp)
	movq	%r14, 56(%rbp)
	movq	152(%r10), %rax
	movq	152(%r10), %rbx
	movq	128(%r10), %rdi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %r11
	movq	144(%r10), %rax
	movq	144(%r10), %rbx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rsi
	movq	136(%r10), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r8
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r9
	movq	%r9, %rax
	mulq	112(%r10)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r8, %rax
	mulq	104(%r10)
	addq	%rax, %r14
	movq	120(%r10), %rax
	adcq	%rdx, %r15
	mulq	80(%r10)
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	96(%r10)
	addq	%rax, %r14
	movq	%r11, %rax
	adcq	%rdx, %r15
	mulq	88(%r10)
	addq	%rax, %r14
	movq	88(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, -104(%rsp)
	movq	%r13, -96(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rdi, %rax
	mulq	80(%r10)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	112(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r14, %r8
	adcq	%r15, %r9
	mulq	104(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	96(%r10)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r9d, %r9d
	addq	%r12, %rax
	adcq	%r9, %rdx
	movq	%rax, %r14
	movq	96(%r10), %rax
	movq	%rdx, %r15
	mulq	120(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	88(%r10)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	80(%r10)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	112(%r10)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	104(%r10)
	movq	%r14, -88(%rsp)
	movq	%r15, -80(%rsp)
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r8
	xorl	%r15d, %r15d
	movq	%r8, %r14
	addq	%rax, %r14
	movq	104(%r10), %rax
	adcq	%rdx, %r15
	mulq	120(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	96(%r10)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	88(%r10)
	addq	%rax, %r12
	movq	144(%r10), %rax
	adcq	%rdx, %r13
	mulq	80(%r10)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r11, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	112(%r10)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r15, %rax
	xorl	%r13d, %r13d
	addq	%rax, %r8
	movq	112(%r10), %rax
	adcq	%r13, %r9
	mulq	120(%r10)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	104(%r10)
	movq	%r12, %rsi
	movq	%r13, %rdi
	addq	%rax, %rsi
	movq	%rbx, %rax
	movq	%r8, %rbx
	adcq	%rdx, %rdi
	mulq	96(%r10)
	addq	%rax, %rsi
	movq	88(%r10), %rax
	adcq	%rdx, %rdi
	mulq	144(%r10)
	addq	%rax, %rsi
	movq	80(%r10), %rax
	adcq	%rdx, %rdi
	mulq	152(%r10)
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r9, %rbx
	xorl	%edi, %edi
	addq	%rbx, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r11
	andq	%rcx, %r14
	andq	%rcx, %r8
	shrdq	$51, %rdx, %rax
	movq	%r8, 104(%rbp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rsi
	movq	-104(%rsp), %rdx
	movq	%r11, %rax
	andq	%rcx, %rax
	andq	%rcx, %rdx
	movq	%rax, 112(%rbp)
	addq	%rdx, %rsi
	movq	-88(%rsp), %rdx
	movq	%rsi, %rdi
	andq	%rcx, %rsi
	shrq	$51, %rdi
	andq	%rcx, %rdx
	movq	%rsi, 80(%rbp)
	addq	%rdi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	addq	%rdx, %r14
	andq	%rcx, %rsi
	movq	%rsi, 88(%rbp)
	movq	%r14, 96(%rbp)
	movq	72(%r10), %rbx
	movq	56(%r10), %r8
	movq	32(%r10), %r14
	leaq	(%rbx,%rbx,8), %rax
	movq	%rbx, -16(%rsp)
	leaq	(%rbx,%rax,2), %rax
	movq	64(%r10), %rbx
	movq	%rax, -72(%rsp)
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rdi
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rsi
	movq	%rdi, -56(%rsp)
	movq	48(%r10), %rdi
	movq	%rsi, -40(%rsp)
	movq	24(%r10), %rsi
	movq	8(%r10), %rax
	movq	40(%r10), %r15
	movq	(%r10), %r9
	movq	16(%r10), %r11
	movq	%rax, -104(%rsp)
	leaq	(%rdi,%rdi,8), %rax
	movq	-104(%rsp), %r10
	leaq	(%rdi,%rax,2), %r13
	movq	%r11, -88(%rsp)
	movq	%r13, %rax
	mulq	%r14
	movq	%rax, %r12
	movq	-40(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r15, %rax
	adcq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	-56(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r11
	addq	%rax, %r12
	movq	-72(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r10
	addq	%rax, %r12
	movq	%r10, %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%r13, -32(%rsp)
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	-40(%rsp), %rax
	movq	%r12, -40(%rsp)
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	-56(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	-72(%rsp), %rax
	adcq	%rdx, %r11
	mulq	-88(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	adcq	%r11, %rdx
	movq	%rdx, %r13
	mulq	%r15
	movq	%r13, -48(%rsp)
	movq	%rax, %r10
	movq	-104(%rsp), %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	-56(%rsp), %rax
	movq	%r12, -56(%rsp)
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	-72(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	shrdq	$51, %r13, %r11
	movq	%r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%r15
	movq	%r11, -64(%rsp)
	movq	%rax, %r12
	movq	-88(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	-104(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	-72(%rsp), %rax
	movq	%r10, -72(%rsp)
	adcq	%rdx, %r13
	mulq	%r14
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r11d, %r11d
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	%rax, %r14
	movq	%rsi, %rax
	movq	%rdx, %r15
	mulq	%rdi
	movq	%r14, %rsi
	movq	%r15, %rdi
	addq	%rax, %rsi
	movq	-88(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%r8
	movq	%r10, %r8
	addq	%rax, %rsi
	movq	-104(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%rbx
	addq	%rax, %rsi
	movq	-16(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%r9
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	movq	%r8, %rbx
	movq	%rdi, %rsi
	addq	%rax, %rbx
	adcq	%rdx, %rsi
	movq	%rbx, %rax
	andq	%rcx, %r10
	shrdq	$51, %rsi, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	-40(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	-56(%rsp), %rax
	movq	%r10, 144(%rbp)
	movq	%rdx, %rdi
	andq	%rcx, %rdx
	andq	%rcx, %rax
	shrq	$51, %rdi
	movq	%rdx, 120(%rbp)
	addq	%rdi, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rcx, %rdx
	movq	%rdx, 128(%rbp)
	movq	-72(%rsp), %rdx
	andq	%rcx, %rdx
	addq	%rdx, %rax
	movq	%rax, 136(%rbp)
	movq	%rbx, %rax
	andq	%rcx, %rax
	movq	%rax, 152(%rbp)
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE36:
	.size	x25519_ge_p1p1_to_p3, .-x25519_ge_p1p1_to_p3
	.p2align 4
	.globl	CRYPTO_memcmp
	.type	CRYPTO_memcmp, @function
CRYPTO_memcmp:
.LFB46:
	.cfi_startproc
	movq	%rdi, %rcx
	movq	%rdx, %rdi
	testq	%rdx, %rdx
	je	.L176
	leaq	-1(%rdx), %rax
	cmpq	$14, %rax
	jbe	.L177
	andq	$-16, %rdx
	xorl	%eax, %eax
	pxor	%xmm1, %xmm1
	.p2align 4,,10
	.p2align 3
.L172:
	movdqu	(%rcx,%rax), %xmm0
	movdqu	(%rsi,%rax), %xmm3
	addq	$16, %rax
	pxor	%xmm3, %xmm0
	por	%xmm0, %xmm1
	cmpq	%rax, %rdx
	jne	.L172
	movdqa	%xmm1, %xmm0
	movq	%rdi, %r8
	psrldq	$8, %xmm0
	andq	$-16, %r8
	por	%xmm1, %xmm0
	movdqa	%xmm0, %xmm2
	psrldq	$4, %xmm2
	por	%xmm2, %xmm0
	movdqa	%xmm0, %xmm2
	psrldq	$2, %xmm2
	por	%xmm2, %xmm0
	movdqa	%xmm0, %xmm2
	psrldq	$1, %xmm2
	por	%xmm2, %xmm0
	movd	%xmm0, %eax
	movdqa	%xmm1, %xmm0
	psrldq	$8, %xmm1
	por	%xmm0, %xmm1
	testb	$15, %dil
	je	.L173
.L171:
	movq	%rdi, %r9
	subq	%r8, %r9
	leaq	-1(%r9), %rdx
	cmpq	$6, %rdx
	jbe	.L174
	movq	(%rcx,%r8), %xmm0
	movq	(%rsi,%r8), %xmm2
	pxor	%xmm2, %xmm0
	por	%xmm1, %xmm0
	movq	%xmm0, %rdx
	movq	%rdx, %r10
	movzbl	%dh, %eax
	orl	%edx, %eax
	shrq	$16, %r10
	orl	%r10d, %eax
	movq	%rdx, %r10
	shrq	$24, %r10
	orl	%r10d, %eax
	movq	%rdx, %r10
	shrq	$32, %r10
	orl	%r10d, %eax
	movq	%rdx, %r10
	shrq	$40, %r10
	orl	%r10d, %eax
	movq	%rdx, %r10
	shrq	$56, %rdx
	shrq	$48, %r10
	orl	%r10d, %eax
	orl	%edx, %eax
	movq	%r9, %rdx
	andq	$-8, %rdx
	addq	%rdx, %r8
	andl	$7, %r9d
	je	.L173
.L174:
	movzbl	(%rcx,%r8), %edx
	xorb	(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	1(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	1(%rsi,%r8), %edx
	xorb	1(%rcx,%r8), %dl
	orl	%edx, %eax
	leaq	2(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	2(%rcx,%r8), %edx
	xorb	2(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	3(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	3(%rcx,%r8), %edx
	xorb	3(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	4(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	4(%rcx,%r8), %edx
	xorb	4(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	5(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	5(%rcx,%r8), %edx
	xorb	5(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	6(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L173
	movzbl	6(%rcx,%r8), %edx
	xorb	6(%rsi,%r8), %dl
	orl	%edx, %eax
.L173:
	movzbl	%al, %eax
	ret
	.p2align 4,,10
	.p2align 3
.L176:
	xorl	%eax, %eax
	ret
.L177:
	pxor	%xmm1, %xmm1
	xorl	%r8d, %r8d
	xorl	%eax, %eax
	jmp	.L171
	.cfi_endproc
.LFE46:
	.size	CRYPTO_memcmp, .-CRYPTO_memcmp
	.p2align 4
	.globl	SHA512_Update
	.type	SHA512_Update, @function
SHA512_Update:
.LFB48:
	.cfi_startproc
	testq	%rdx, %rdx
	je	.L226
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	leaq	0(,%rdx,8), %rax
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	leaq	80(%rdi), %r13
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	movq	%rdx, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbx
	subq	$24, %rsp
	.cfi_def_cfa_offset 64
	movq	72(%rdi), %rcx
	addq	64(%rdi), %rax
	movq	%rax, 64(%rdi)
	movl	208(%rdi), %eax
	adcq	$0, %rcx
	shrq	$61, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 72(%rdi)
	testl	%eax, %eax
	je	.L230
	movl	$128, %ecx
	leaq	0(%r13,%rax), %r8
	subq	%rax, %rcx
	cmpq	%rcx, %rbp
	jb	.L231
	leaq	-128(%rax,%rbp), %r14
	leaq	(%rsi,%rcx), %rbp
	testq	%rcx, %rcx
	jne	.L232
	movl	$0, 208(%rdi)
	movl	$1, %edx
	movq	%r13, %rsi
	call	sha512_block_data_order
	cmpq	$127, %r14
	jbe	.L229
.L206:
	movq	%rbp, %rsi
	movq	%r14, %rbp
	.p2align 4,,10
	.p2align 3
.L193:
	movq	%rbp, %rdx
	movq	%rbx, %rdi
	movq	%rsi, 8(%rsp)
	movq	%rbp, %r14
	shrq	$7, %rdx
	andq	$-128, %rbp
	andl	$127, %r14d
	call	sha512_block_data_order
	movq	8(%rsp), %rsi
	addq	%rsi, %rbp
	jmp	.L200
	.p2align 4,,10
	.p2align 3
.L230:
	cmpq	$127, %rbp
	ja	.L193
.L192:
	movl	%ebp, %ecx
	cmpl	$8, %ebp
	jb	.L233
	movq	(%rsi), %rax
	leaq	8(%r13), %rdi
	movq	%r13, %rcx
	andq	$-8, %rdi
	movq	%rax, 80(%rbx)
	movl	%ebp, %eax
	subq	%rdi, %rcx
	movq	-8(%rsi,%rax), %rdx
	subq	%rcx, %rsi
	addl	%ebp, %ecx
	shrl	$3, %ecx
	movq	%rdx, -8(%r13,%rax)
	rep movsq
.L203:
	movl	%ebp, 208(%rbx)
.L213:
	addq	$24, %rsp
	.cfi_def_cfa_offset 40
	movl	$1, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L226:
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 13
	.cfi_restore 14
	movl	$1, %eax
	ret
	.p2align 4,,10
	.p2align 3
.L232:
	.cfi_def_cfa_offset 64
	.cfi_offset 3, -40
	.cfi_offset 6, -32
	.cfi_offset 13, -24
	.cfi_offset 14, -16
	cmpq	$8, %rcx
	jnb	.L196
	testb	$4, %cl
	jne	.L234
	testq	%rcx, %rcx
	jne	.L235
.L197:
	movl	$0, 208(%rbx)
	movl	$1, %edx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	call	sha512_block_data_order
	cmpq	$127, %r14
	ja	.L206
.L200:
	testq	%r14, %r14
	je	.L213
	.p2align 4,,10
	.p2align 3
.L229:
	movq	%rbp, %rsi
	movq	%r14, %rbp
	jmp	.L192
	.p2align 4,,10
	.p2align 3
.L233:
	testb	$4, %bpl
	jne	.L236
	testl	%ecx, %ecx
	je	.L203
	movzbl	(%rsi), %eax
	movb	%al, 80(%rbx)
	testb	$2, %cl
	je	.L203
	movzwl	-2(%rsi,%rcx), %eax
	movw	%ax, -2(%r13,%rcx)
	jmp	.L203
	.p2align 4,,10
	.p2align 3
.L231:
	movq	%rbp, %rdx
	movq	%r8, %rdi
	call	memcpy
	addl	%ebp, 208(%rbx)
	jmp	.L213
	.p2align 4,,10
	.p2align 3
.L196:
	movq	(%rsi), %rax
	leaq	8(%r8), %rdi
	andq	$-8, %rdi
	movq	%rax, (%r8)
	movq	-8(%rsi,%rcx), %rax
	movq	%rax, -8(%r8,%rcx)
	movq	%r8, %rax
	subq	%rdi, %rax
	addq	%rax, %rcx
	subq	%rax, %rsi
	shrq	$3, %rcx
	rep movsq
	jmp	.L197
	.p2align 4,,10
	.p2align 3
.L235:
	movzbl	(%rsi), %eax
	movb	%al, (%r8)
	testb	$2, %cl
	je	.L197
	movzwl	-2(%rsi,%rcx), %eax
	movw	%ax, -2(%r8,%rcx)
	jmp	.L197
.L236:
	movl	(%rsi), %eax
	movl	%eax, 80(%rbx)
	movl	-4(%rsi,%rcx), %eax
	movl	%eax, -4(%r13,%rcx)
	jmp	.L203
.L234:
	movl	(%rsi), %eax
	movl	%eax, (%r8)
	movl	-4(%rsi,%rcx), %eax
	movl	%eax, -4(%r8,%rcx)
	jmp	.L197
	.cfi_endproc
.LFE48:
	.size	SHA512_Update, .-SHA512_Update
	.p2align 4
	.globl	SHA512
	.type	SHA512, @function
SHA512:
.LFB50:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	movq	%rdx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	subq	$232, %rsp
	.cfi_def_cfa_offset 272
	movdqa	.LC0(%rip), %xmm0
	movq	.LC4(%rip), %rax
	movq	%rsp, %r13
	movaps	%xmm0, (%rsp)
	movdqa	.LC1(%rip), %xmm0
	movq	%rax, 208(%rsp)
	movaps	%xmm0, 16(%rsp)
	movdqa	.LC2(%rip), %xmm0
	movaps	%xmm0, 32(%rsp)
	movdqa	.LC3(%rip), %xmm0
	movaps	%xmm0, 48(%rsp)
	pxor	%xmm0, %xmm0
	movaps	%xmm0, 64(%rsp)
	testq	%rsi, %rsi
	je	.L238
	movq	%rsi, %rax
	movq	%rdi, %rbp
	movq	%rsi, %rbx
	movq	%rsp, %r13
	shrq	$61, %rax
	movq	%rax, 72(%rsp)
	leaq	0(,%rsi,8), %rax
	movq	%rax, 64(%rsp)
	cmpq	$127, %rsi
	ja	.L320
.L239:
	leaq	80(%rsp), %r8
	movl	%ebx, %edx
	movq	%r8, %rdi
	cmpl	$8, %ebx
	jnb	.L321
	xorl	%eax, %eax
	testb	$4, %dl
	jne	.L322
.L241:
	testb	$2, %dl
	jne	.L323
.L242:
	andl	$1, %edx
	jne	.L324
.L243:
	movl	%ebx, 208(%rsp)
	movl	212(%rsp), %ebp
	movb	$-128, 80(%rsp,%rbx)
	addq	$1, %rbx
	cmpq	$112, %rbx
	jbe	.L244
	movl	$128, %eax
	subq	%rbx, %rax
	jne	.L325
.L245:
	movq	%r8, %rsi
	movl	$1, %edx
	movq	%r13, %rdi
	call	sha512_block_data_order
	leaq	80(%rsp), %r8
	movl	$112, %eax
	movq	%r8, %rbx
.L252:
	movl	%eax, %ecx
	cmpl	$8, %eax
	jb	.L326
	leaq	8(%rbx), %rdi
	movl	%eax, %edx
	movq	$0, (%rbx)
	movq	$0, -8(%rbx,%rdx)
	andq	$-8, %rdi
	subq	%rdi, %rbx
	leal	(%rax,%rbx), %ecx
	xorl	%eax, %eax
	shrl	$3, %ecx
	rep stosq
.L253:
	movq	72(%rsp), %rax
	movq	64(%rsp), %rdx
	movq	%r8, %rsi
	movq	%r13, %rdi
	bswap	%rdx
	bswap	%rax
	movq	%rdx, %xmm1
	movq	%rax, %xmm0
	punpcklqdq	%xmm1, %xmm0
	movl	$1, %edx
	movaps	%xmm0, 192(%rsp)
	call	sha512_block_data_order
	testq	%r12, %r12
	je	.L259
	shrq	$3, %rbp
	je	.L259
	movq	(%rsp), %rax
	bswap	%rax
	movq	%rax, (%r12)
	cmpq	$1, %rbp
	je	.L259
	movq	8(%rsp), %rax
	bswap	%rax
	movq	%rax, 8(%r12)
	cmpq	$2, %rbp
	je	.L259
	movq	16(%rsp), %rax
	bswap	%rax
	movq	%rax, 16(%r12)
	cmpq	$3, %rbp
	je	.L259
	movq	24(%rsp), %rax
	bswap	%rax
	movq	%rax, 24(%r12)
	cmpq	$4, %rbp
	je	.L259
	movq	32(%rsp), %rax
	bswap	%rax
	movq	%rax, 32(%r12)
	cmpq	$5, %rbp
	je	.L259
	movq	40(%rsp), %rax
	bswap	%rax
	movq	%rax, 40(%r12)
	cmpq	$6, %rbp
	je	.L259
	movq	48(%rsp), %rax
	bswap	%rax
	movq	%rax, 48(%r12)
	cmpq	$7, %rbp
	je	.L259
	movq	56(%rsp), %rax
	bswap	%rax
	movq	%rax, 56(%r12)
.L259:
	xorl	%eax, %eax
	movq	%r13, %rdi
	movl	$27, %ecx
	rep stosq
	addq	$232, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	movq	%r12, %rax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L320:
	.cfi_restore_state
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	%r13, %rdi
	shrq	$7, %rdx
	call	sha512_block_data_order
	movq	%rbx, %rax
	andl	$127, %eax
	jne	.L327
.L238:
	movb	$-128, 80(%rsp)
	movl	$64, %ebp
	movl	$111, %eax
	movl	$1, %ebx
	leaq	80(%rsp), %r8
.L261:
	addq	%r8, %rbx
	jmp	.L252
	.p2align 4,,10
	.p2align 3
.L324:
	movzbl	0(%rbp,%rax), %edx
	movb	%dl, (%rdi,%rax)
	jmp	.L243
	.p2align 4,,10
	.p2align 3
.L323:
	movzwl	0(%rbp,%rax), %ecx
	movw	%cx, (%rdi,%rax)
	addq	$2, %rax
	andl	$1, %edx
	je	.L243
	jmp	.L324
	.p2align 4,,10
	.p2align 3
.L322:
	movl	0(%rbp), %eax
	movl	%eax, (%rdi)
	movl	$4, %eax
	testb	$2, %dl
	je	.L242
	jmp	.L323
	.p2align 4,,10
	.p2align 3
.L326:
	testb	$4, %al
	jne	.L328
	testl	%ecx, %ecx
	je	.L253
	movb	$0, (%rbx)
	testb	$2, %cl
	je	.L253
	xorl	%eax, %eax
	movw	%ax, -2(%rbx,%rcx)
	jmp	.L253
	.p2align 4,,10
	.p2align 3
.L325:
	addq	%r8, %rbx
	xorl	%edi, %edi
	cmpl	$8, %eax
	jb	.L329
	leaq	8(%rbx), %rcx
	movl	%eax, %edx
	movq	$0, (%rbx)
	movq	$0, -8(%rbx,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rbx
	addl	%ebx, %eax
	andl	$-8, %eax
	cmpl	$8, %eax
	jb	.L245
	andl	$-8, %eax
	xorl	%edx, %edx
.L250:
	movl	%edx, %esi
	addl	$8, %edx
	movq	%rdi, (%rcx,%rsi)
	cmpl	%eax, %edx
	jb	.L250
	jmp	.L245
	.p2align 4,,10
	.p2align 3
.L321:
	movl	%ebx, %ecx
	movq	%rbp, %rsi
	xorl	%eax, %eax
	shrl	$3, %ecx
	rep movsq
	movq	%rsi, %rbp
	testb	$4, %dl
	je	.L241
	jmp	.L322
	.p2align 4,,10
	.p2align 3
.L329:
	testb	$4, %al
	jne	.L330
	testl	%eax, %eax
	je	.L245
	movb	$0, (%rbx)
	testb	$2, %al
	je	.L245
	movl	%eax, %eax
	xorl	%edx, %edx
	movw	%dx, -2(%rbx,%rax)
	jmp	.L245
.L327:
	subq	%rax, %rbx
	addq	%rbx, %rbp
	movq	%rax, %rbx
	jmp	.L239
.L328:
	movl	$0, (%rbx)
	movl	$0, -4(%rbx,%rcx)
	jmp	.L253
.L330:
	movl	%eax, %eax
	movl	$0, (%rbx)
	movl	$0, -4(%rbx,%rax)
	jmp	.L245
.L244:
	movl	$112, %eax
	subq	%rbx, %rax
	je	.L253
	jmp	.L261
	.cfi_endproc
.LFE50:
	.size	SHA512, .-SHA512
	.p2align 4
	.globl	x25519_sc_reduce
	.type	x25519_sc_reduce, @function
x25519_sc_reduce:
.LFB80:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rdi, %rax
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movzbl	1(%rdi), %edx
	movzbl	2(%rdi), %ecx
	movzbl	(%rdi), %esi
	salq	$8, %rdx
	salq	$16, %rcx
	orq	%rcx, %rdx
	movzbl	7(%rdi), %ecx
	orq	%rsi, %rdx
	andl	$2097151, %edx
	salq	$16, %rcx
	movq	%rdx, -80(%rsp)
	movl	2(%rdi), %edx
	shrq	$5, %rdx
	movq	%rdx, %rsi
	movzbl	6(%rdi), %edx
	andl	$2097151, %esi
	salq	$8, %rdx
	movq	%rsi, -72(%rsp)
	movzbl	21(%rdi), %esi
	orq	%rcx, %rdx
	movzbl	5(%rdi), %ecx
	orq	%rcx, %rdx
	shrq	$2, %rdx
	movq	%rdx, %rbx
	movl	7(%rdi), %edx
	andl	$2097151, %ebx
	shrq	$7, %rdx
	movq	%rbx, -64(%rsp)
	movq	%rdx, %rcx
	movl	10(%rdi), %edx
	andl	$2097151, %ecx
	shrq	$4, %rdx
	movq	%rcx, -56(%rsp)
	movzbl	15(%rdi), %ecx
	andl	$2097151, %edx
	movq	%rdx, -48(%rsp)
	movzbl	14(%rdi), %edx
	salq	$16, %rcx
	salq	$8, %rdx
	orq	%rcx, %rdx
	movzbl	13(%rdi), %ecx
	orq	%rcx, %rdx
	movzbl	23(%rdi), %ecx
	shrq	%rdx
	movq	%rdx, %r8
	movl	15(%rdi), %edx
	salq	$16, %rcx
	andl	$2097151, %r8d
	shrq	$6, %rdx
	movq	%r8, -40(%rsp)
	movq	%rdx, %r9
	movzbl	22(%rdi), %edx
	andl	$2097151, %r9d
	salq	$8, %rdx
	movq	%r9, -32(%rsp)
	orq	%rcx, %rdx
	orq	%rsi, %rdx
	movq	%rdx, %r11
	movl	23(%rdi), %edx
	andl	$2097151, %r11d
	shrq	$5, %rdx
	movq	%r11, -24(%rsp)
	movq	%rdx, %rbp
	andl	$2097151, %ebp
	movq	%rbp, -16(%rsp)
	movzbl	27(%rdi), %r10d
	movzbl	28(%rdi), %edx
	movzbl	35(%rdi), %ebx
	salq	$8, %r10
	movzbl	44(%rdi), %ecx
	movzbl	42(%rdi), %esi
	salq	$16, %rdx
	salq	$8, %rbx
	movl	28(%rdi), %r12d
	movl	31(%rdi), %ebp
	orq	%rdx, %r10
	movzbl	26(%rdi), %edx
	salq	$16, %rcx
	movl	36(%rdi), %r15d
	movl	52(%rdi), %r9d
	shrq	$7, %r12
	shrq	$4, %rbp
	movl	57(%rax), %r8d
	orq	%rdx, %r10
	movzbl	36(%rdi), %edx
	shrq	$6, %r15
	andl	$2097151, %r12d
	shrq	$2, %r10
	andl	$2097151, %ebp
	andl	$2097151, %r15d
	salq	$16, %rdx
	andl	$2097151, %r10d
	orq	%rdx, %rbx
	movzbl	34(%rdi), %edx
	orq	%rdx, %rbx
	movzbl	43(%rdi), %edx
	shrq	%rbx
	salq	$8, %rdx
	andl	$2097151, %ebx
	orq	%rcx, %rdx
	movzbl	49(%rdi), %ecx
	orq	%rsi, %rdx
	movl	49(%rdi), %esi
	movq	%rdx, %r11
	movl	44(%rdi), %edx
	salq	$16, %rcx
	shrq	$7, %rsi
	andl	$2097151, %r11d
	shrq	$5, %rdx
	movq	%rdx, %r13
	movzbl	48(%rdi), %edx
	andl	$2097151, %r13d
	salq	$8, %rdx
	movq	%r13, -8(%rsp)
	orq	%rdx, %rcx
	movzbl	47(%rdi), %edx
	movzbl	56(%rdi), %edi
	orq	%rdx, %rcx
	movzbl	57(%rax), %edx
	shrq	$2, %rcx
	andl	$2097151, %ecx
	andl	$2097151, %esi
	salq	$8, %rdi
	salq	$16, %rdx
	shrq	$6, %r8
	orq	%rdi, %rdx
	movzbl	55(%rax), %edi
	andl	$2097151, %r8d
	shrq	$4, %r9
	andl	$2097151, %r9d
	orq	%rdi, %rdx
	movl	60(%rax), %edi
	shrq	%rdx
	shrq	$3, %rdi
	andl	$2097151, %edx
	imulq	$666643, %rdi, %r14
	imulq	$470296, %rdi, %r13
	addq	%r12, %r14
	imulq	$654183, %rdi, %r12
	addq	%rbp, %r13
	imulq	$-997805, %rdi, %rbp
	addq	%rbx, %r12
	movzbl	41(%rax), %ebx
	addq	%r15, %rbp
	movzbl	40(%rax), %r15d
	salq	$16, %rbx
	salq	$8, %r15
	orq	%r15, %rbx
	movzbl	39(%rax), %r15d
	orq	%r15, %rbx
	imulq	$136657, %rdi, %r15
	imulq	$-683901, %rdi, %rdi
	shrq	$3, %rbx
	addq	%rbx, %r15
	imulq	$470296, %r8, %rbx
	addq	%r11, %rdi
	imulq	$666643, %r8, %r11
	addq	%r10, %r11
	addq	%r14, %rbx
	movq	-16(%rsp), %r14
	imulq	$654183, %r8, %r10
	addq	%r10, %r13
	imulq	$-997805, %r8, %r10
	addq	%r10, %r12
	imulq	$136657, %r8, %r10
	imulq	$-683901, %r8, %r8
	addq	%rbp, %r10
	imulq	$666643, %rdx, %rbp
	addq	%r15, %r8
	movq	-24(%rsp), %r15
	addq	%r14, %rbp
	imulq	$470296, %rdx, %r14
	addq	%r11, %r14
	imulq	$654183, %rdx, %r11
	addq	%r11, %rbx
	imulq	$-997805, %rdx, %r11
	addq	%r11, %r13
	imulq	$136657, %rdx, %r11
	imulq	$-683901, %rdx, %rdx
	addq	%r12, %r11
	imulq	$666643, %r9, %r12
	addq	%r10, %rdx
	imulq	$470296, %r9, %r10
	addq	%r15, %r12
	addq	%r10, %rbp
	imulq	$654183, %r9, %r10
	addq	%r10, %r14
	imulq	$-997805, %r9, %r10
	addq	%r10, %rbx
	imulq	$136657, %r9, %r10
	imulq	$-683901, %r9, %r9
	addq	%r10, %r13
	movzbl	20(%rax), %r10d
	addq	%r11, %r9
	movzbl	19(%rax), %r11d
	salq	$16, %r10
	salq	$8, %r11
	orq	%r11, %r10
	movzbl	18(%rax), %r11d
	movq	-32(%rsp), %r15
	orq	%r11, %r10
	imulq	$666643, %rsi, %r11
	shrq	$3, %r10
	addq	%r11, %r10
	imulq	$470296, %rsi, %r11
	addq	%r11, %r12
	imulq	$654183, %rsi, %r11
	addq	%r11, %rbp
	imulq	$-997805, %rsi, %r11
	addq	%r11, %r14
	imulq	$136657, %rsi, %r11
	imulq	$-683901, %rsi, %rsi
	addq	%rbx, %r11
	imulq	$666643, %rcx, %rbx
	addq	%r13, %rsi
	imulq	$470296, %rcx, %r13
	addq	%r15, %rbx
	movq	-8(%rsp), %r15
	addq	%r10, %r13
	imulq	$654183, %rcx, %r10
	addq	%r10, %r12
	imulq	$-997805, %rcx, %r10
	addq	%r10, %rbp
	imulq	$136657, %rcx, %r10
	imulq	$-683901, %rcx, %rcx
	addq	%r10, %r14
	addq	%r11, %rcx
	leaq	1048576(%rbx), %r11
	movq	%r11, %r10
	sarq	$21, %r10
	addq	%r13, %r10
	andq	$-2097152, %r11
	leaq	1048576(%r12), %r13
	subq	%r11, %rbx
	movq	%r13, %r11
	andq	$-2097152, %r13
	subq	%r13, %r12
	sarq	$21, %r11
	leaq	1048576(%r14), %r13
	addq	%rbp, %r11
	movq	%r13, %rbp
	andq	$-2097152, %r13
	sarq	$21, %rbp
	subq	%r13, %r14
	addq	%rcx, %rbp
	leaq	1048576(%rsi), %rcx
	movq	%rcx, %r13
	andq	$-2097152, %rcx
	sarq	$21, %r13
	subq	%rcx, %rsi
	addq	%r13, %r9
	leaq	1048576(%rdx), %r13
	movq	%r13, %rcx
	andq	$-2097152, %r13
	subq	%r13, %rdx
	sarq	$21, %rcx
	leaq	1048576(%rdi), %r13
	addq	%r8, %rcx
	movq	%r13, %r8
	andq	$-2097152, %r13
	sarq	$21, %r8
	subq	%r13, %rdi
	addq	%r15, %r8
	leaq	1048576(%r10), %r15
	movq	%r15, %r13
	andq	$-2097152, %r15
	sarq	$21, %r13
	subq	%r15, %r10
	addq	%r12, %r13
	leaq	1048576(%r11), %r12
	movq	%r12, %r15
	andq	$-2097152, %r12
	subq	%r12, %r11
	sarq	$21, %r15
	leaq	1048576(%rbp), %r12
	addq	%r15, %r14
	movq	%r12, %r15
	sarq	$21, %r15
	addq	%r15, %rsi
	andq	$-2097152, %r12
	subq	%r12, %rbp
	leaq	1048576(%r9), %r12
	movq	%r12, %r15
	andq	$-2097152, %r12
	sarq	$21, %r15
	subq	%r12, %r9
	addq	%r15, %rdx
	leaq	1048576(%rcx), %r15
	movq	%r15, %r12
	andq	$-2097152, %r15
	subq	%r15, %rcx
	sarq	$21, %r12
	imulq	$666643, %r8, %r15
	addq	%rdi, %r12
	movq	-40(%rsp), %rdi
	addq	%rdi, %r15
	imulq	$470296, %r8, %rdi
	addq	%rdi, %rbx
	imulq	$654183, %r8, %rdi
	addq	%rdi, %r10
	imulq	$-997805, %r8, %rdi
	addq	%rdi, %r13
	imulq	$136657, %r8, %rdi
	imulq	$-683901, %r8, %r8
	addq	%r11, %rdi
	imulq	$666643, %r12, %r11
	addq	%r14, %r8
	movq	-48(%rsp), %r14
	addq	%r14, %r11
	imulq	$470296, %r12, %r14
	addq	%r15, %r14
	imulq	$654183, %r12, %r15
	addq	%r15, %rbx
	imulq	$-997805, %r12, %r15
	addq	%r10, %r15
	imulq	$136657, %r12, %r10
	addq	%r13, %r10
	imulq	$-683901, %r12, %r12
	imulq	$666643, %rcx, %r13
	addq	%rdi, %r12
	movq	-56(%rsp), %rdi
	addq	%rdi, %r13
	imulq	$470296, %rcx, %rdi
	addq	%rdi, %r11
	imulq	$654183, %rcx, %rdi
	addq	%rdi, %r14
	imulq	$-997805, %rcx, %rdi
	addq	%rdi, %rbx
	imulq	$136657, %rcx, %rdi
	imulq	$-683901, %rcx, %rcx
	addq	%r15, %rdi
	imulq	$666643, %rdx, %r15
	addq	%r10, %rcx
	movq	-64(%rsp), %r10
	addq	%r10, %r15
	imulq	$470296, %rdx, %r10
	addq	%r10, %r13
	imulq	$654183, %rdx, %r10
	addq	%r10, %r11
	imulq	$-997805, %rdx, %r10
	addq	%r10, %r14
	imulq	$136657, %rdx, %r10
	imulq	$-683901, %rdx, %rdx
	addq	%rbx, %r10
	imulq	$666643, %r9, %rbx
	addq	%rdi, %rdx
	movq	-72(%rsp), %rdi
	addq	%rdi, %rbx
	imulq	$470296, %r9, %rdi
	addq	%rdi, %r15
	imulq	$654183, %r9, %rdi
	addq	%rdi, %r13
	imulq	$-997805, %r9, %rdi
	addq	%rdi, %r11
	imulq	$136657, %r9, %rdi
	imulq	$-683901, %r9, %r9
	addq	%r14, %rdi
	movq	-80(%rsp), %r14
	addq	%r10, %r9
	imulq	$666643, %rsi, %r10
	addq	%r14, %r10
	imulq	$470296, %rsi, %r14
	addq	%r14, %rbx
	imulq	$654183, %rsi, %r14
	addq	%r15, %r14
	imulq	$-997805, %rsi, %r15
	addq	%r15, %r13
	imulq	$136657, %rsi, %r15
	imulq	$-683901, %rsi, %rsi
	addq	%r15, %r11
	leaq	1048576(%r10), %r15
	addq	%rdi, %rsi
	movq	%r15, %rdi
	andq	$-2097152, %r15
	sarq	$21, %rdi
	addq	%rbx, %rdi
	movq	%r10, %rbx
	leaq	1048576(%r14), %r10
	subq	%r15, %rbx
	movq	%r10, %r15
	andq	$-2097152, %r10
	subq	%r10, %r14
	sarq	$21, %r15
	leaq	1048576(%r11), %r10
	addq	%r15, %r13
	movq	%r10, %r15
	andq	$-2097152, %r10
	subq	%r10, %r11
	sarq	$21, %r15
	leaq	1048576(%r9), %r10
	addq	%rsi, %r15
	movq	%r10, %rsi
	sarq	$21, %rsi
	addq	%rdx, %rsi
	andq	$-2097152, %r10
	subq	%r10, %r9
	leaq	1048576(%rcx), %r10
	movq	%r10, %rdx
	andq	$-2097152, %r10
	sarq	$21, %rdx
	subq	%r10, %rcx
	addq	%r12, %rdx
	leaq	1048576(%r8), %r12
	movq	%r12, %r10
	andq	$-2097152, %r12
	subq	%r12, %r8
	sarq	$21, %r10
	leaq	1048576(%rdi), %r12
	addq	%rbp, %r10
	movq	%r12, %rbp
	sarq	$21, %rbp
	addq	%r14, %rbp
	movq	%r12, %r14
	andq	$-2097152, %r14
	subq	%r14, %rdi
	movq	%rdi, %r12
	leaq	1048576(%r13), %rdi
	movq	%rdi, %r14
	andq	$-2097152, %rdi
	subq	%rdi, %r13
	sarq	$21, %r14
	leaq	1048576(%r15), %rdi
	addq	%r11, %r14
	movq	%rdi, %r11
	andq	$-2097152, %rdi
	subq	%rdi, %r15
	sarq	$21, %r11
	leaq	1048576(%rsi), %rdi
	addq	%r11, %r9
	movq	%rdi, %r11
	andq	$-2097152, %rdi
	subq	%rdi, %rsi
	sarq	$21, %r11
	leaq	1048576(%rdx), %rdi
	addq	%r11, %rcx
	movq	%rdi, %r11
	andq	$-2097152, %rdi
	sarq	$21, %r11
	subq	%rdi, %rdx
	addq	%r11, %r8
	leaq	1048576(%r10), %r11
	movq	%r11, %rdi
	andq	$-2097152, %r11
	sarq	$21, %rdi
	subq	%r11, %r10
	imulq	$666643, %rdi, %r11
	addq	%rbx, %r11
	imulq	$470296, %rdi, %rbx
	addq	%r12, %rbx
	imulq	$654183, %rdi, %r12
	addq	%r12, %rbp
	imulq	$-997805, %rdi, %r12
	addq	%r13, %r12
	imulq	$136657, %rdi, %r13
	imulq	$-683901, %rdi, %rdi
	addq	%r14, %r13
	movq	%r11, %r14
	sarq	$21, %r14
	addq	%r15, %rdi
	addq	%rbx, %r14
	movq	%r11, %rbx
	movq	%r14, %r11
	andl	$2097151, %r14d
	andl	$2097151, %ebx
	sarq	$21, %r11
	addq	%rbp, %r11
	movq	%r14, %rbp
	movq	%r11, %r14
	andl	$2097151, %r11d
	sarq	$21, %r14
	addq	%r12, %r14
	movq	%r11, %r12
	movq	%r14, %r11
	andl	$2097151, %r14d
	sarq	$21, %r11
	addq	%r13, %r11
	movq	%r14, %r13
	movq	%r11, %r14
	sarq	$21, %r14
	addq	%rdi, %r14
	movq	%r11, %rdi
	movq	%r14, %r11
	andl	$2097151, %edi
	andl	$2097151, %r14d
	sarq	$21, %r11
	addq	%r9, %r11
	movq	%r11, %r15
	sarq	$21, %r15
	addq	%rsi, %r15
	andl	$2097151, %r11d
	movq	%r15, %r9
	andl	$2097151, %r15d
	sarq	$21, %r9
	addq	%rcx, %r9
	movq	%r9, %rcx
	andl	$2097151, %r9d
	sarq	$21, %rcx
	addq	%rdx, %rcx
	movq	%rcx, %rdx
	andl	$2097151, %ecx
	sarq	$21, %rdx
	addq	%r8, %rdx
	movq	%rcx, %r8
	movq	%rdx, %rcx
	andl	$2097151, %edx
	sarq	$21, %rcx
	addq	%r10, %rcx
	movq	%rdx, %r10
	movq	%rcx, %rsi
	andl	$2097151, %ecx
	sarq	$21, %rsi
	imulq	$666643, %rsi, %rdx
	addq	%rbx, %rdx
	imulq	$470296, %rsi, %rbx
	addq	%rbp, %rbx
	imulq	$654183, %rsi, %rbp
	addq	%rbp, %r12
	imulq	$-997805, %rsi, %rbp
	addq	%r13, %rbp
	imulq	$136657, %rsi, %r13
	imulq	$-683901, %rsi, %rsi
	addq	%rdi, %r13
	movq	%rdx, %rdi
	andl	$2097151, %edx
	sarq	$21, %rdi
	addq	%r14, %rsi
	movq	%rdx, %r14
	addq	%rdi, %rbx
	movw	%r14w, (%rax)
	movq	%rbx, %rdx
	sarq	$21, %rdx
	addq	%r12, %rdx
	andl	$2097151, %ebx
	sarq	$16, %r14
	movq	%rdx, %r12
	andl	$2097151, %edx
	sarq	$21, %r12
	addq	%rbp, %r12
	movq	%rdx, %rbp
	movq	%r12, %rdi
	andl	$2097151, %r12d
	sarq	$21, %rdi
	addq	%r13, %rdi
	movq	%rdi, %rdx
	andl	$2097151, %edi
	sarq	$21, %rdx
	addq	%rsi, %rdx
	movq	%rdx, %rsi
	andl	$2097151, %edx
	sarq	$21, %rsi
	addq	%r11, %rsi
	movq	%rdx, %r11
	movq	%rsi, %rdx
	andl	$2097151, %esi
	sarq	$21, %rdx
	addq	%r15, %rdx
	movq	%rdx, %r13
	andl	$2097151, %edx
	sarq	$21, %r13
	addq	%r9, %r13
	movq	%rdx, %r9
	movq	%r13, %rdx
	sarq	$21, %rdx
	addq	%r8, %rdx
	movq	%r13, %r8
	movq	%rdx, %r13
	andl	$2097151, %r8d
	andl	$2097151, %edx
	sarq	$21, %r13
	movw	%r8w, 21(%rax)
	addq	%r10, %r13
	movq	%r13, %r10
	sarq	$21, %r10
	addq	%rcx, %r10
	movq	%r13, %rcx
	movq	%rbx, %r13
	salq	$5, %r13
	andl	$2097151, %ecx
	orl	%r13d, %r14d
	movq	%rbx, %r13
	sarq	$3, %r13
	sarq	$16, %r8
	movb	%r14b, 2(%rax)
	movb	%r13b, 3(%rax)
	movq	%rbx, %r13
	sarq	$19, %rbx
	sarq	$11, %r13
	movb	%r13b, 4(%rax)
	leaq	0(,%rbp,4), %r13
	orl	%r13d, %ebx
	movb	%bl, 5(%rax)
	movq	%rbp, %rbx
	sarq	$14, %rbp
	sarq	$6, %rbx
	movb	%bl, 6(%rax)
	movq	%r12, %rbx
	salq	$7, %rbx
	orl	%ebx, %ebp
	movq	%r12, %rbx
	sarq	%rbx
	movb	%bpl, 7(%rax)
	movb	%bl, 8(%rax)
	movq	%r12, %rbx
	sarq	$17, %r12
	sarq	$9, %rbx
	movb	%bl, 9(%rax)
	movq	%rdi, %rbx
	salq	$4, %rbx
	orl	%ebx, %r12d
	movq	%rdi, %rbx
	sarq	$4, %rbx
	movb	%r12b, 10(%rax)
	movb	%bl, 11(%rax)
	movq	%rdi, %rbx
	sarq	$20, %rdi
	sarq	$12, %rbx
	movb	%bl, 12(%rax)
	leaq	(%r11,%r11), %rbx
	orl	%ebx, %edi
	movb	%dil, 13(%rax)
	movq	%r11, %rdi
	sarq	$15, %r11
	sarq	$7, %rdi
	movb	%dil, 14(%rax)
	movq	%rsi, %rdi
	salq	$6, %rdi
	orl	%edi, %r11d
	movq	%rsi, %rdi
	sarq	$2, %rdi
	movb	%r11b, 15(%rax)
	movb	%dil, 16(%rax)
	movq	%rsi, %rdi
	sarq	$18, %rsi
	sarq	$10, %rdi
	movb	%dil, 17(%rax)
	leaq	0(,%r9,8), %rdi
	orl	%edi, %esi
	movb	%sil, 18(%rax)
	movq	%r9, %rsi
	sarq	$13, %r9
	sarq	$5, %rsi
	movb	%r9b, 20(%rax)
	movb	%sil, 19(%rax)
	movq	%rdx, %rsi
	salq	$5, %rsi
	orl	%esi, %r8d
	movq	%rdx, %rsi
	sarq	$3, %rsi
	movb	%r8b, 23(%rax)
	movb	%sil, 24(%rax)
	movq	%rdx, %rsi
	sarq	$11, %rsi
	sarq	$19, %rdx
	movb	%sil, 25(%rax)
	leaq	0(,%rcx,4), %rsi
	orl	%esi, %edx
	movb	%dl, 26(%rax)
	movq	%rcx, %rdx
	sarq	$14, %rcx
	popq	%rbx
	.cfi_def_cfa_offset 48
	sarq	$6, %rdx
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	movb	%dl, 27(%rax)
	movq	%r10, %rdx
	popq	%r13
	.cfi_def_cfa_offset 24
	salq	$7, %rdx
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	orl	%edx, %ecx
	movq	%r10, %rdx
	sarq	%rdx
	movb	%cl, 28(%rax)
	movb	%dl, 29(%rax)
	movq	%r10, %rdx
	sarq	$17, %r10
	sarq	$9, %rdx
	movb	%r10b, 31(%rax)
	movb	%dl, 30(%rax)
	ret
	.cfi_endproc
.LFE80:
	.size	x25519_sc_reduce, .-x25519_sc_reduce
	.p2align 4
	.globl	x25519_ge_scalarmult_base
	.type	x25519_ge_scalarmult_base, @function
x25519_ge_scalarmult_base:
.LFB83:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	subq	$1144, %rsp
	.cfi_def_cfa_offset 1200
	movdqu	(%rsi), %xmm1
	movdqu	(%rsi), %xmm2
	movdqa	.LC6(%rip), %xmm0
	leaq	656(%rsp), %rax
	leaq	719(%rsp), %rdi
	psrlw	$4, %xmm1
	pand	%xmm0, %xmm1
	pand	%xmm0, %xmm2
	movdqa	%xmm2, %xmm3
	punpckhbw	%xmm1, %xmm2
	movaps	%xmm2, 672(%rsp)
	movdqu	16(%rsi), %xmm2
	punpcklbw	%xmm1, %xmm3
	xorl	%esi, %esi
	movaps	%xmm3, 656(%rsp)
	movdqa	%xmm2, %xmm1
	psrlw	$4, %xmm2
	pand	%xmm0, %xmm1
	pand	%xmm2, %xmm0
	movdqa	%xmm1, %xmm2
	punpckhbw	%xmm0, %xmm1
	punpcklbw	%xmm0, %xmm2
	movaps	%xmm1, 704(%rsp)
	movaps	%xmm2, 688(%rsp)
	.p2align 4,,10
	.p2align 3
.L334:
	addb	(%rax), %sil
	addq	$1, %rax
	movl	%esi, %edx
	leal	8(%rdx), %ecx
	movl	%ecx, %esi
	andl	$-16, %ecx
	subl	%ecx, %edx
	sarb	$4, %sil
	movb	%dl, -1(%rax)
	cmpq	%rax, %rdi
	jne	.L334
	addb	%sil, 719(%rsp)
	pxor	%xmm0, %xmm0
	movabsq	$2251799813685247, %r12
	movq	$0, 32(%rbp)
	movq	$1, 40(%rbp)
	movq	$1, 80(%rbp)
	movq	$0, 152(%rbp)
	movq	$1, (%rsp)
	movups	%xmm0, 0(%rbp)
	movups	%xmm0, 16(%rbp)
	movups	%xmm0, 48(%rbp)
	movups	%xmm0, 64(%rbp)
	movups	%xmm0, 88(%rbp)
	movups	%xmm0, 104(%rbp)
	movups	%xmm0, 120(%rbp)
	movups	%xmm0, 136(%rbp)
	.p2align 4,,10
	.p2align 3
.L335:
	movq	(%rsp), %rax
	leaq	720(%rsp), %rdi
	movsbl	656(%rsp,%rax), %edx
	movl	%eax, %esi
	sarl	%esi
	call	table_select
	leaq	720(%rsp), %rdx
	movq	%rbp, %rsi
	leaq	976(%rsp), %rdi
	call	ge_madd
	movq	1128(%rsp), %rbx
	movq	1120(%rsp), %rdi
	movq	$0, 56(%rsp)
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rbx
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rcx
	movq	1112(%rsp), %rdi
	movq	%rbx, %r15
	movq	%rcx, 16(%rsp)
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r11
	movq	1104(%rsp), %rdi
	movq	%r11, 32(%rsp)
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rdi
	movq	%rdi, %rax
	mulq	1008(%rsp)
	movq	%rdi, 640(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r11, %rax
	mulq	1000(%rsp)
	addq	%rax, %rsi
	movq	1096(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	976(%rsp)
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	992(%rsp)
	addq	%rax, %rsi
	movq	%rbx, %rax
	adcq	%rdx, %rdi
	mulq	984(%rsp)
	addq	%rax, %rsi
	movq	984(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, %r13
	movq	%rdi, %r14
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	1104(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	1008(%rsp)
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	1000(%rsp)
	addq	%rax, %rsi
	movq	%rbx, %rax
	adcq	%rdx, %rdi
	mulq	992(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r13, %rax
	addq	%rsi, %r8
	adcq	%rdi, %r9
	shrdq	$51, %r14, %rax
	movq	%rax, 48(%rsp)
	movq	992(%rsp), %rax
	addq	48(%rsp), %r8
	adcq	56(%rsp), %r9
	mulq	1096(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	984(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	1112(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	976(%rsp)
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	1008(%rsp)
	addq	%rax, %rsi
	movq	%rbx, %rax
	adcq	%rdx, %rdi
	mulq	1000(%rsp)
	movq	$0, 72(%rsp)
	movq	$0, 88(%rsp)
	movq	%r15, 632(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	shrdq	$51, %r9, %rax
	movq	%rax, 64(%rsp)
	movq	1000(%rsp), %rax
	addq	64(%rsp), %rcx
	adcq	72(%rsp), %rbx
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	992(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	984(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	1120(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%r15, %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rcx, %rax
	addq	%r10, %rsi
	adcq	%r11, %rdi
	shrdq	$51, %rbx, %rax
	movq	%rax, 80(%rsp)
	movq	1008(%rsp), %rax
	addq	80(%rsp), %rsi
	adcq	88(%rsp), %rdi
	mulq	1096(%rsp)
	movq	%rsi, 560(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	1000(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	992(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	984(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1120(%rsp)
	addq	%rax, %r10
	movq	976(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1128(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rdi, %rsi
	movq	%rsi, 96(%rsp)
	addq	96(%rsp), %rax
	movq	$0, 104(%rsp)
	adcq	104(%rsp), %rdx
	andq	%r12, %r13
	andq	%r12, %r8
	movq	%rax, 576(%rsp)
	shrdq	$51, %rdx, %rax
	andq	%r12, %rcx
	movq	576(%rsp), %rbx
	movq	$0, 120(%rsp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	andq	%r12, %rbx
	addq	%rdx, %r13
	movq	%rbx, 648(%rsp)
	movq	%r13, %rdx
	movq	%rbx, 32(%rbp)
	movq	1088(%rsp), %rbx
	andq	%r12, %r13
	shrq	$51, %rdx
	movq	%r13, 0(%rbp)
	movq	1056(%rsp), %r13
	addq	%r8, %rdx
	leaq	(%rbx,%rbx,8), %rax
	movq	%rdx, %rsi
	leaq	(%rbx,%rax,2), %rdi
	shrq	$51, %rdx
	movq	1080(%rsp), %rbx
	andq	%r12, %rsi
	addq	%rdx, %rcx
	movq	%rsi, 8(%rbp)
	movq	560(%rsp), %rsi
	leaq	(%rbx,%rbx,8), %rax
	movq	%rcx, 16(%rbp)
	movq	1064(%rsp), %rcx
	leaq	(%rbx,%rax,2), %r8
	andq	%r12, %rsi
	movq	%rsi, 24(%rbp)
	movq	1072(%rsp), %rsi
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r9
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %r14
	movq	%r14, %rax
	mulq	1048(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r9, %rax
	mulq	1040(%rsp)
	addq	%rax, %r14
	movq	%r13, %rax
	adcq	%rdx, %r15
	mulq	1016(%rsp)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	1032(%rsp)
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	1024(%rsp)
	addq	%rax, %r14
	movq	%r13, %rax
	adcq	%rdx, %r15
	mulq	1024(%rsp)
	movq	%r14, 560(%rsp)
	movq	%r15, 568(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	1040(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	1032(%rsp)
	addq	%r10, %rax
	movq	%r14, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r10
	movq	%r10, 112(%rsp)
	addq	112(%rsp), %rax
	adcq	120(%rsp), %rdx
	movq	%rax, %r14
	movq	%r13, %rax
	movq	$0, 136(%rsp)
	movq	%rdx, %r15
	mulq	1032(%rsp)
	movq	%r14, 576(%rsp)
	movq	%r15, 584(%rsp)
	movq	$0, 152(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	movq	%r14, %r8
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	1040(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r8
	movq	%r8, 128(%rsp)
	addq	128(%rsp), %rax
	adcq	136(%rsp), %rdx
	movq	%rax, %r14
	movq	%r13, %rax
	movq	%rdx, %r15
	mulq	1040(%rsp)
	movq	%r14, 592(%rsp)
	movq	%r15, 600(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	addq	%r10, %r8
	adcq	%r11, %r9
	shrdq	$51, %r15, %rax
	movq	%rax, 144(%rsp)
	movq	%r9, %r10
	movq	%r13, %rax
	movq	%r8, %r9
	addq	144(%rsp), %r9
	adcq	152(%rsp), %r10
	mulq	1048(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rcx, %rax
	mulq	1040(%rsp)
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	1032(%rsp)
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	1024(%rsp)
	addq	%rax, %r14
	movq	1016(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1088(%rsp)
	movq	%r9, 608(%rsp)
	movq	608(%rsp), %r8
	movq	$0, 168(%rsp)
	movq	%r10, 616(%rsp)
	movq	$0, 184(%rsp)
	addq	%r14, %rax
	adcq	%r15, %rdx
	shrdq	$51, %r10, %r9
	movq	%r9, 160(%rsp)
	addq	160(%rsp), %rax
	adcq	168(%rsp), %rdx
	movq	%rax, %r14
	andq	%r12, %r8
	movq	576(%rsp), %r9
	shrdq	$51, %rdx, %rax
	movq	%r8, 64(%rbp)
	leaq	(%rax,%rax,8), %rdx
	andq	%r12, %r9
	leaq	(%rax,%rdx,2), %rdi
	movq	560(%rsp), %rdx
	movq	%r14, %rax
	andq	%r12, %rax
	andq	%r12, %rdx
	movq	%rax, 72(%rbp)
	movq	640(%rsp), %rax
	addq	%rdx, %rdi
	movq	%rdi, %rdx
	andq	%r12, %rdi
	shrq	$51, %rdx
	movq	%rdi, 40(%rbp)
	addq	%r9, %rdx
	movq	%rdx, %r9
	shrq	$51, %rdx
	andq	%r12, %r9
	movq	%r9, 48(%rbp)
	movq	592(%rsp), %r9
	andq	%r12, %r9
	movq	%r9, %rdi
	addq	%rdx, %rdi
	mulq	1088(%rsp)
	movq	%rdi, 56(%rbp)
	movq	632(%rsp), %rdi
	movq	%rax, %r8
	movq	32(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	movq	%r8, %r10
	mulq	%rsi
	movq	%r9, %r11
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	%rcx
	movq	%r10, %r14
	movq	%r11, %r15
	addq	%rax, %r14
	movq	%rcx, %rax
	adcq	%rdx, %r15
	mulq	1096(%rsp)
	movq	%r15, 40(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r13, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	32(%rsp), %rax
	movq	%r14, 32(%rsp)
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r8
	movq	%r8, 176(%rsp)
	addq	176(%rsp), %rax
	adcq	184(%rsp), %rdx
	movq	%rax, %r14
	movq	%rsi, %rax
	movq	$0, 200(%rsp)
	movq	%rdx, %r15
	movq	%r14, %r9
	movq	$0, 216(%rsp)
	mulq	1096(%rsp)
	movq	$0, 232(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	16(%rsp), %rax
	movq	%r14, 16(%rsp)
	adcq	%rdx, %r11
	mulq	1088(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	%rbx
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r9
	movq	%r9, 192(%rsp)
	addq	192(%rsp), %rax
	adcq	200(%rsp), %rdx
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%rdx, %r15
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	addq	%r8, %r10
	adcq	%r9, %r11
	mulq	1088(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	shrdq	$51, %r15, %rax
	movq	%rax, 208(%rsp)
	movq	1088(%rsp), %rax
	addq	208(%rsp), %r10
	adcq	216(%rsp), %r11
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rcx, %rax
	addq	%r8, %rsi
	adcq	%r9, %rdi
	mulq	1120(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r13, %rax
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	mulq	1128(%rsp)
	addq	%rcx, %rax
	movq	%r10, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r11, %rcx
	movq	%rcx, 224(%rsp)
	addq	224(%rsp), %rax
	adcq	232(%rsp), %rdx
	movq	%rax, %r8
	movq	16(%rsp), %rsi
	andq	%r12, %r10
	shrdq	$51, %rdx, %rax
	movq	%r10, 104(%rbp)
	movq	1048(%rsp), %rbx
	leaq	(%rax,%rax,8), %rdx
	andq	%r12, %rsi
	movq	$0, 248(%rsp)
	leaq	(%rax,%rdx,2), %rcx
	movq	32(%rsp), %rdx
	movq	%r8, %rax
	andq	%r12, %rax
	andq	%r12, %rdx
	movq	%rax, 112(%rbp)
	leaq	(%rbx,%rbx,8), %rax
	addq	%rdx, %rcx
	movq	%rcx, %rdx
	andq	%r12, %rcx
	shrq	$51, %rdx
	movq	%rcx, 80(%rbp)
	addq	%rsi, %rdx
	movq	%rdx, %rcx
	shrq	$51, %rdx
	andq	%r12, %rcx
	movq	%rcx, 88(%rbp)
	movq	%r14, %rcx
	andq	%r12, %rcx
	addq	%rdx, %rcx
	movq	%rcx, 96(%rbp)
	leaq	(%rbx,%rax,2), %rcx
	movq	1040(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rsi
	movq	1032(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rdi
	movq	1024(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rbx
	movq	%rbx, %rax
	mulq	1008(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	mulq	1000(%rsp)
	addq	%rax, %r10
	movq	1016(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	992(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	984(%rsp)
	addq	%rax, %r10
	movq	1016(%rsp), %rax
	adcq	%rdx, %r11
	mulq	984(%rsp)
	movq	%r10, 16(%rsp)
	movq	%r11, 24(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1024(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	movq	%r8, 240(%rsp)
	addq	240(%rsp), %rax
	adcq	248(%rsp), %rdx
	movq	%rax, %r14
	movq	1016(%rsp), %rax
	movq	$0, 264(%rsp)
	movq	$0, 280(%rsp)
	movq	%rdx, %r15
	mulq	992(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	1024(%rsp), %rax
	mulq	984(%rsp)
	addq	%rax, %r10
	movq	1032(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rcx, %rax
	addq	%r10, %rsi
	adcq	%r11, %rdi
	mulq	1000(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	addq	%rsi, %r8
	adcq	%rdi, %r9
	shrdq	$51, %r15, %rax
	movq	%rax, 256(%rsp)
	movq	1016(%rsp), %rax
	addq	256(%rsp), %r8
	adcq	264(%rsp), %r9
	mulq	1000(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	1024(%rsp), %rax
	mulq	992(%rsp)
	addq	%rax, %r10
	movq	1032(%rsp), %rax
	adcq	%rdx, %r11
	mulq	984(%rsp)
	addq	%rax, %r10
	movq	1040(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	shrdq	$51, %r9, %rax
	movq	%rax, 272(%rsp)
	movq	1016(%rsp), %rax
	addq	272(%rsp), %rcx
	adcq	280(%rsp), %rbx
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	1024(%rsp), %rax
	mulq	1000(%rsp)
	movq	16(%rsp), %r13
	movq	$0, 296(%rsp)
	addq	%rax, %rsi
	movq	1032(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	992(%rsp)
	addq	%rax, %rsi
	movq	1040(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	984(%rsp)
	addq	%rax, %rsi
	movq	1048(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	976(%rsp)
	addq	%rsi, %rax
	movq	%rcx, %rsi
	adcq	%rdi, %rdx
	shrdq	$51, %rbx, %rsi
	movq	%rsi, 288(%rsp)
	addq	288(%rsp), %rax
	adcq	296(%rsp), %rdx
	movq	%rax, %r10
	andq	%r12, %r13
	andq	%r12, %r14
	shrdq	$51, %rdx, %rax
	addq	$2, (%rsp)
	andq	%r12, %r8
	andq	%r12, %rcx
	leaq	(%rax,%rax,8), %rdx
	movq	%rcx, 144(%rbp)
	leaq	(%rax,%rdx,2), %rdx
	movq	%r10, %rax
	addq	%rdx, %r13
	andq	%r12, %rax
	movq	%r13, %rdx
	movq	%rax, 152(%rbp)
	movq	(%rsp), %rax
	andq	%r12, %r13
	shrq	$51, %rdx
	movq	%r13, 120(%rbp)
	addq	%rdx, %r14
	movq	%r14, %rdx
	shrq	$51, %r14
	andq	%r12, %rdx
	addq	%r14, %r8
	movq	%rdx, 128(%rbp)
	movq	%r8, 136(%rbp)
	cmpq	$65, %rax
	jne	.L335
	movq	648(%rsp), %rax
	movdqu	0(%rbp), %xmm4
	leaq	848(%rsp), %rsi
	leaq	976(%rsp), %rdi
	movdqu	16(%rbp), %xmm5
	movdqu	40(%rbp), %xmm6
	movq	%rax, 880(%rsp)
	movq	72(%rbp), %rax
	movdqu	56(%rbp), %xmm7
	movaps	%xmm4, 848(%rsp)
	movdqu	80(%rbp), %xmm4
	movq	%rax, 920(%rsp)
	movq	112(%rbp), %rax
	movaps	%xmm5, 864(%rsp)
	movdqu	96(%rbp), %xmm5
	movq	%rax, 960(%rsp)
	movups	%xmm6, 888(%rsp)
	movups	%xmm7, 904(%rsp)
	movaps	%xmm4, 928(%rsp)
	movaps	%xmm5, 944(%rsp)
	call	ge_p2_dbl
	movq	1128(%rsp), %rax
	movq	1128(%rsp), %rbx
	movq	1120(%rsp), %rcx
	movq	1104(%rsp), %r15
	leaq	(%rax,%rax,8), %rax
	movq	1008(%rsp), %rdi
	movq	1000(%rsp), %rsi
	leaq	(%rbx,%rax,2), %rbx
	movq	1120(%rsp), %rax
	movq	976(%rsp), %r14
	movq	984(%rsp), %r13
	movq	%rbx, %r11
	leaq	(%rax,%rax,8), %rax
	movq	%r11, (%rsp)
	leaq	(%rcx,%rax,2), %r10
	movq	1112(%rsp), %rax
	movq	1112(%rsp), %rcx
	movq	%r10, 32(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %rcx
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %rax
	movq	%rcx, 16(%rsp)
	movq	%rax, 96(%rsp)
	mulq	%rdi
	movq	%rax, %r8
	movq	%rcx, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	%r13
	movq	%r8, %rcx
	movq	%r9, %rbx
	addq	%rax, %rcx
	movq	%r13, %rax
	adcq	%rdx, %rbx
	mulq	1096(%rsp)
	movq	%rcx, 48(%rsp)
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%r8, %rax
	movq	%rcx, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rbx, %r8
	xorl	%ebx, %ebx
	addq	%r8, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r10
	movq	992(%rsp), %rax
	movq	%rdx, %r11
	mulq	1096(%rsp)
	movq	%r10, 64(%rsp)
	movq	%rax, %r8
	movq	%r13, %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	32(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rdi
	movq	%rax, %rcx
	movq	(%rsp), %rax
	movq	%rdx, %rbx
	addq	%r8, %rcx
	adcq	%r9, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r11, %rax
	xorl	%edx, %edx
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	1096(%rsp)
	movq	%rcx, %r9
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r15, %rax
	mulq	992(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1120(%rsp)
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %r9
	movq	%r9, %r8
	xorl	%r9d, %r9d
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	movq	1088(%rsp), %rbx
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%r15
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	992(%rsp), %rax
	addq	%r10, %rsi
	movq	%r8, %r10
	adcq	%r11, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%r13, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	addq	%rax, %rsi
	movq	%r14, %rax
	movq	1072(%rsp), %r14
	adcq	%rdx, %rdi
	mulq	1128(%rsp)
	addq	%rax, %rsi
	movq	64(%rsp), %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r10
	xorl	%edx, %edx
	addq	%r10, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %r13
	andq	%r12, %rax
	andq	%r12, %rcx
	shrdq	$51, %rdi, %rsi
	movq	48(%rsp), %rdi
	andq	%r12, %r8
	leaq	(%rsi,%rsi,8), %rdx
	movq	%r8, 872(%rsp)
	andq	%r12, %rdi
	leaq	(%rsi,%rdx,2), %rdx
	movq	%r13, %rsi
	movq	1080(%rsp), %r13
	addq	%rdi, %rdx
	andq	%r12, %rsi
	movq	%rdx, %rdi
	andq	%r12, %rdx
	movq	%rsi, 880(%rsp)
	shrq	$51, %rdi
	movq	%rdx, 848(%rsp)
	addq	%rdi, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	addq	%rax, %rcx
	leaq	(%rbx,%rbx,8), %rax
	andq	%r12, %rdx
	leaq	(%rbx,%rax,2), %rsi
	movq	1064(%rsp), %rbx
	leaq	0(%r13,%r13,8), %rax
	movq	%rcx, 864(%rsp)
	leaq	0(%r13,%rax,2), %rcx
	leaq	(%r14,%r14,8), %rax
	movq	%rdx, 856(%rsp)
	leaq	(%r14,%rax,2), %rdi
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r9
	movq	%r9, %rax
	mulq	1048(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	mulq	1040(%rsp)
	addq	%rax, %r10
	movq	1056(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	1024(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1056(%rsp)
	movq	%r10, 48(%rsp)
	movq	%r11, 56(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	1032(%rsp), %rax
	movq	%rdx, %r11
	mulq	1056(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	1024(%rsp)
	movq	%r10, 64(%rsp)
	movq	%r11, 72(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%r10, %r9
	shrdq	$51, %r11, %r9
	movq	%r9, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r8
	movq	1040(%rsp), %rax
	movq	%rdx, %r9
	mulq	1056(%rsp)
	movq	%r8, 80(%rsp)
	movq	%r9, 88(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rbx, %rax
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	movq	%r8, %rsi
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r9, %rsi
	xorl	%r11d, %r11d
	movq	%rsi, %r10
	addq	%rax, %r10
	movq	1048(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1056(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	1016(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	64(%rsp), %rdi
	andq	%r12, %r10
	shrdq	$51, %rdx, %rax
	movq	%r10, 912(%rsp)
	leaq	(%rax,%rax,8), %rdx
	andq	%r12, %rdi
	leaq	(%rax,%rdx,2), %rcx
	movq	48(%rsp), %rdx
	movq	%r8, %rax
	andq	%r12, %rax
	andq	%r12, %rdx
	movq	%rax, 920(%rsp)
	movq	96(%rsp), %rax
	addq	%rdx, %rcx
	movq	%rcx, %rdx
	andq	%r12, %rcx
	shrq	$51, %rdx
	movq	%rcx, 888(%rsp)
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	shrq	$51, %rdx
	andq	%r12, %rdi
	movq	%rdi, 896(%rsp)
	movq	80(%rsp), %rdi
	andq	%r12, %rdi
	movq	%rdi, %rcx
	addq	%rdx, %rcx
	mulq	1088(%rsp)
	movq	%rcx, 904(%rsp)
	movq	%rax, %rsi
	movq	16(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	1056(%rsp), %rax
	movq	32(%rsp), %r11
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	(%rsp), %rcx
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r14
	movq	%rsi, %r8
	movq	%rdi, %r9
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	movq	%r9, %r10
	movq	%r8, %r9
	addq	%rax, %r9
	movq	%rbx, %rax
	adcq	%rdx, %r10
	mulq	1096(%rsp)
	movq	%r9, %r8
	movq	%r9, (%rsp)
	movq	%r10, 8(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r15, %rax
	mulq	1056(%rsp)
	addq	%rax, %rsi
	movq	16(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1088(%rsp)
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	%r14
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r10, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	movq	%r14, %rax
	movq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdi, 24(%rsp)
	movq	%rax, %r8
	movq	%rbx, %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%r13
	addq	%r8, %rax
	movq	%rsi, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rdi, %r8
	xorl	%r11d, %r11d
	movq	%r8, %r10
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1096(%rsp)
	movq	%r10, %rsi
	movq	%rax, %r8
	movq	%r14, %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %rsi
	xorl	%edi, %edi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	1088(%rsp), %rax
	movq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %rsi
	movq	%r13, %rax
	movq	%rdx, %rdi
	mulq	%r15
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%rbx, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	leaq	976(%rsp), %rdi
	addq	%rax, %rcx
	movq	1056(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	1128(%rsp)
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r9, %rax
	xorl	%edx, %edx
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	movq	%rcx, %rax
	andq	%r12, %r10
	andq	%r12, %r8
	shrdq	$51, %rbx, %rax
	andq	%r12, %rcx
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	(%rsp), %rax
	andq	%r12, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rsi
	andq	%r12, %rdx
	andq	%r12, %rax
	shrq	$51, %rsi
	movq	%rdx, 928(%rsp)
	addq	%rsi, %rax
	leaq	848(%rsp), %rsi
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%r12, %rdx
	addq	%r10, %rax
	movq	%rdx, 936(%rsp)
	movq	%rax, 944(%rsp)
	movq	%r8, 952(%rsp)
	movq	%rcx, 960(%rsp)
	call	ge_p2_dbl
	movq	1128(%rsp), %rax
	movq	1128(%rsp), %rbx
	movq	1120(%rsp), %rcx
	movq	1008(%rsp), %rdi
	leaq	(%rax,%rax,8), %rax
	movq	1000(%rsp), %rsi
	movq	984(%rsp), %r13
	leaq	(%rbx,%rax,2), %rbx
	movq	1120(%rsp), %rax
	movq	%rbx, %r11
	leaq	(%rax,%rax,8), %rax
	movq	%r11, (%rsp)
	leaq	(%rcx,%rax,2), %r10
	movq	1112(%rsp), %rax
	movq	1112(%rsp), %rcx
	movq	%r10, 16(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %r15
	movq	1104(%rsp), %rax
	movq	1104(%rsp), %rcx
	movq	%r15, 32(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %rax
	movq	%rax, 80(%rsp)
	mulq	%rdi
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	1096(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	%r13
	movq	%r8, %rcx
	movq	%r9, %rbx
	addq	%rax, %rcx
	movq	%r13, %rax
	adcq	%rdx, %rbx
	mulq	1096(%rsp)
	movq	%rcx, 48(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1104(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%r8, %rax
	movq	%rcx, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rbx, %r8
	xorl	%r15d, %r15d
	movq	%r8, %r14
	addq	%rax, %r14
	movq	992(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r13, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	1112(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r11, %rax
	addq	%r8, %rcx
	adcq	%r9, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r14, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r15, %rax
	xorl	%edx, %edx
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	992(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	1120(%rsp), %rax
	movq	1072(%rsp), %r15
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%r10, %rax
	movq	%rcx, %r10
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %r10
	xorl	%r9d, %r9d
	movq	1048(%rsp), %rbx
	movq	%r10, %r8
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	1104(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	992(%rsp), %rax
	addq	%r10, %rsi
	movq	%r8, %r10
	adcq	%r11, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%r13, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	addq	%rax, %rsi
	movq	976(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1128(%rsp)
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r10
	xorl	%edx, %edx
	addq	%r10, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%r12, %rcx
	andq	%r12, %rsi
	shrdq	$51, %rdi, %rax
	andq	%r12, %r8
	movq	%rsi, 880(%rsp)
	leaq	(%rax,%rax,8), %rdx
	movq	%r8, 872(%rsp)
	leaq	(%rax,%rdx,2), %rdx
	movq	48(%rsp), %rax
	andq	%r12, %rax
	addq	%rax, %rdx
	movq	%r14, %rax
	movq	1080(%rsp), %r14
	movq	%rdx, %rdi
	andq	%r12, %rax
	andq	%r12, %rdx
	shrq	$51, %rdi
	movq	%rdx, 848(%rsp)
	addq	%rdi, %rax
	movq	1088(%rsp), %rdi
	movq	%rax, %rdx
	shrq	$51, %rax
	addq	%rax, %rcx
	leaq	(%rdi,%rdi,8), %rax
	andq	%r12, %rdx
	leaq	(%rdi,%rax,2), %rdi
	movq	%rcx, 864(%rsp)
	leaq	(%r14,%r14,8), %rax
	movq	1064(%rsp), %rcx
	leaq	(%r14,%rax,2), %rsi
	leaq	(%r15,%r15,8), %rax
	movq	%rdx, 856(%rsp)
	leaq	(%r15,%rax,2), %r8
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %r9
	movq	%r9, %rax
	mulq	%rbx
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%r8, %rax
	mulq	1040(%rsp)
	addq	%rax, %r12
	movq	1056(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1016(%rsp)
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	1032(%rsp)
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	1024(%rsp)
	movq	%r12, %r10
	movq	%r13, %r11
	addq	%rax, %r10
	movq	1024(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1056(%rsp)
	movq	%r10, 48(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rcx, %rax
	mulq	1016(%rsp)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%rbx
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	movq	%r11, 56(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r13d, %r13d
	movq	%r10, %r12
	addq	%rax, %r12
	movq	1032(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1056(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%r12, %r9
	shrdq	$51, %r13, %r9
	movq	16(%rsp), %r13
	movq	%r9, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	1040(%rsp), %rax
	movq	%rdx, %r11
	mulq	1056(%rsp)
	movq	%r10, 64(%rsp)
	movq	%r11, 72(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	mulq	1032(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r11, %rax
	xorl	%r11d, %r11d
	movq	%rax, %r10
	movq	%rbx, %rax
	movabsq	$2251799813685247, %rbx
	addq	%r8, %r10
	adcq	%r9, %r11
	mulq	1056(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	1016(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	movq	%r12, %rdi
	andq	%rbx, %rax
	shrdq	$51, %rdx, %rsi
	andq	%rbx, %rdi
	movq	%rax, 920(%rsp)
	movq	80(%rsp), %rax
	leaq	(%rsi,%rsi,8), %rdx
	andq	%rbx, %r10
	leaq	(%rsi,%rdx,2), %rsi
	movq	48(%rsp), %rdx
	movq	%r10, 912(%rsp)
	movq	(%rsp), %r10
	andq	%rbx, %rdx
	addq	%rdx, %rsi
	movq	%rsi, %rdx
	andq	%rbx, %rsi
	shrq	$51, %rdx
	movq	%rsi, 888(%rsp)
	addq	%rdi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rbx, %rsi
	movq	%rsi, 896(%rsp)
	movq	64(%rsp), %rsi
	andq	%rbx, %rsi
	addq	%rdx, %rsi
	mulq	1088(%rsp)
	movq	%rsi, 904(%rsp)
	movq	%rax, %rsi
	movq	32(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r14
	addq	%rax, %rsi
	movq	1056(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	addq	%rax, %rsi
	movq	%r13, %rax
	adcq	%rdx, %rdi
	mulq	%r15
	movq	%rsi, %r8
	movq	%rdi, %r9
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	movq	%r8, %r11
	movq	%r9, %r12
	addq	%rax, %r11
	movq	%rcx, %rax
	adcq	%rdx, %r12
	mulq	1096(%rsp)
	movq	%r11, %r8
	movq	%r12, 40(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	1056(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	32(%rsp), %rax
	movq	%r11, 32(%rsp)
	adcq	%rdx, %rdi
	mulq	1088(%rsp)
	addq	%rax, %rsi
	movq	%r13, %rax
	adcq	%rdx, %rdi
	mulq	%r14
	addq	%rax, %rsi
	movq	%r10, %rax
	adcq	%rdx, %rdi
	mulq	%r15
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r12, %r8
	xorl	%r13d, %r13d
	movq	%r8, %r12
	addq	%rax, %r12
	movq	%r15, %rax
	adcq	%rdx, %r13
	mulq	1096(%rsp)
	movq	%r12, %rsi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %rsi
	xorl	%r11d, %r11d
	movq	%rsi, %r10
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1096(%rsp)
	movq	%r10, %rsi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	addq	%rax, %r8
	movq	(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %rsi
	xorl	%edi, %edi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	1088(%rsp), %rax
	movq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	movq	%r8, %r14
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	%r15, %rax
	adcq	%rdx, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	addq	%rax, %rsi
	movq	1056(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1128(%rsp)
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r14
	xorl	%edx, %edx
	addq	%r14, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%rbx, %r10
	andq	%rbx, %r8
	shrdq	$51, %rdi, %rax
	andq	%rbx, %rsi
	movq	%r8, 952(%rsp)
	leaq	976(%rsp), %rdi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	andq	%rbx, %rax
	addq	%rax, %rdx
	movq	%r12, %rax
	movq	%rdx, %rcx
	andq	%rbx, %rax
	andq	%rbx, %rdx
	shrq	$51, %rcx
	movq	%rdx, 928(%rsp)
	addq	%rcx, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rbx, %rdx
	addq	%r10, %rax
	movq	%rdx, 936(%rsp)
	movq	%rax, 944(%rsp)
	movq	%rsi, 960(%rsp)
	leaq	848(%rsp), %rsi
	call	ge_p2_dbl
	movq	1128(%rsp), %rax
	movq	1128(%rsp), %rdi
	movq	1120(%rsp), %rcx
	movq	992(%rsp), %r12
	leaq	(%rax,%rax,8), %rax
	movq	984(%rsp), %r13
	leaq	(%rdi,%rax,2), %rdi
	movq	1120(%rsp), %rax
	movq	%rdi, %r11
	leaq	(%rax,%rax,8), %rax
	movq	%r11, (%rsp)
	leaq	(%rcx,%rax,2), %r10
	movq	1112(%rsp), %rax
	movq	1112(%rsp), %rcx
	movq	%r10, 80(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %r15
	movq	1104(%rsp), %rax
	movq	1104(%rsp), %rcx
	movq	%r15, 16(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %rax
	movq	1000(%rsp), %rcx
	movq	%rax, 96(%rsp)
	mulq	1008(%rsp)
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	1096(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%r12
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%r13
	movq	%r8, %rsi
	movq	%r9, %rdi
	addq	%rax, %rsi
	movq	%r13, %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, 32(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1104(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	%r12
	addq	%r8, %rax
	movq	%rsi, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rdi, %r8
	xorl	%r15d, %r15d
	movq	%r8, %r14
	addq	%rax, %r14
	movq	%r12, %rax
	adcq	%rdx, %r15
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r13, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	1112(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r11, %rax
	addq	%r8, %rsi
	adcq	%r9, %rdi
	mulq	%rcx
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r15, %rax
	xorl	%edx, %edx
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r12, %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	1120(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	movq	1064(%rsp), %r15
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	addq	%r10, %rax
	movq	%rsi, %r10
	adcq	%r11, %rdx
	shrdq	$51, %rdi, %r10
	xorl	%r9d, %r9d
	movq	1088(%rsp), %rdi
	movq	%r10, %r8
	addq	%rax, %r8
	movq	1008(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	movq	%r8, %r12
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	movq	1080(%rsp), %r13
	adcq	%rdx, %r11
	mulq	1120(%rsp)
	addq	%rax, %r10
	movq	976(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1128(%rsp)
	addq	%rax, %r10
	adcq	%rdx, %r11
	shrdq	$51, %r9, %r12
	xorl	%edx, %edx
	addq	%r12, %r10
	movq	1072(%rsp), %r12
	adcq	%rdx, %r11
	movq	%r10, %rax
	andq	%rbx, %rsi
	andq	%rbx, %r10
	shrdq	$51, %r11, %rax
	movq	%r10, 880(%rsp)
	andq	%rbx, %r8
	leaq	(%rax,%rax,8), %rdx
	movq	%r8, 872(%rsp)
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	andq	%rbx, %rax
	addq	%rax, %rdx
	movq	%r14, %rax
	movq	%rdx, %r14
	andq	%rbx, %rax
	andq	%rbx, %rdx
	shrq	$51, %r14
	movq	%rdx, 848(%rsp)
	addq	%r14, %rax
	movq	1056(%rsp), %r14
	movq	%rax, %rdx
	shrq	$51, %rax
	addq	%rax, %rsi
	leaq	(%rdi,%rdi,8), %rax
	andq	%rbx, %rdx
	movq	%rsi, 864(%rsp)
	leaq	(%rdi,%rax,2), %rsi
	leaq	0(%r13,%r13,8), %rax
	leaq	0(%r13,%rax,2), %rcx
	leaq	(%r12,%r12,8), %rax
	movq	%rdx, 856(%rsp)
	leaq	(%r12,%rax,2), %rdi
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movq	%r9, %rax
	mulq	1048(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	mulq	1040(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	movq	%r10, 32(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	movq	%r11, 40(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	1032(%rsp)
	movq	%r10, 48(%rsp)
	movq	%r11, 56(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%r10, %r9
	shrdq	$51, %r11, %r9
	movq	%r9, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r8
	movq	%r14, %rax
	movq	%rdx, %r9
	mulq	1040(%rsp)
	movq	%r8, 64(%rsp)
	movq	%r9, 72(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r15, %rax
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	movq	%r8, %rsi
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r9, %rsi
	xorl	%r11d, %r11d
	movq	%rsi, %r10
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	1016(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	48(%rsp), %rdi
	andq	%rbx, %r10
	shrdq	$51, %rdx, %rax
	movq	%r10, 912(%rsp)
	leaq	(%rax,%rax,8), %rdx
	andq	%rbx, %rdi
	leaq	(%rax,%rdx,2), %rcx
	movq	32(%rsp), %rdx
	movq	%r8, %rax
	andq	%rbx, %rax
	andq	%rbx, %rdx
	movq	%rax, 920(%rsp)
	movq	96(%rsp), %rax
	addq	%rdx, %rcx
	movq	%rcx, %rdx
	andq	%rbx, %rcx
	shrq	$51, %rdx
	movq	%rcx, 888(%rsp)
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	shrq	$51, %rdx
	andq	%rbx, %rdi
	movq	%rdi, 896(%rsp)
	movq	64(%rsp), %rdi
	andq	%rbx, %rdi
	movq	%rdi, %rcx
	addq	%rdx, %rcx
	mulq	1088(%rsp)
	movq	%rcx, 904(%rsp)
	movq	%rax, %rsi
	movq	16(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	80(%rsp), %r11
	movq	(%rsp), %rcx
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r12
	movq	%rsi, %r8
	movq	%rdi, %r9
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%r15
	movq	%r9, %r10
	movq	%r8, %r9
	addq	%rax, %r9
	movq	%r15, %rax
	adcq	%rdx, %r10
	mulq	1096(%rsp)
	movq	%r9, %r8
	movq	%r9, (%rsp)
	movq	%r10, 8(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	16(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	1088(%rsp)
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	%r12
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r10, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	movq	%r12, %rax
	movq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdi, 24(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%r13
	addq	%r8, %rax
	movq	%rsi, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rdi, %r8
	xorl	%r11d, %r11d
	movq	%r8, %r10
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1096(%rsp)
	movq	%r10, %rsi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r12, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %rsi
	xorl	%edi, %edi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	1088(%rsp), %rax
	movq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r13, %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	%r12, %rax
	movq	%r8, %r12
	adcq	%rdx, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%r15, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	1128(%rsp)
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r12
	xorl	%edx, %edx
	addq	%r12, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%rbx, %r10
	andq	%rbx, %rsi
	shrdq	$51, %rdi, %rax
	andq	%rbx, %r8
	movq	%rsi, 960(%rsp)
	leaq	976(%rsp), %rdi
	leaq	(%rax,%rax,8), %rdx
	leaq	848(%rsp), %rsi
	movq	%r8, 952(%rsp)
	leaq	(%rax,%rdx,2), %rdx
	movq	(%rsp), %rax
	andq	%rbx, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rcx
	andq	%rbx, %rdx
	shrq	$51, %rcx
	andq	%rbx, %rax
	movq	%rdx, 928(%rsp)
	addq	%rcx, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rbx, %rdx
	addq	%r10, %rax
	movq	%rdx, 936(%rsp)
	movq	%rax, 944(%rsp)
	call	ge_p2_dbl
	movq	1128(%rsp), %rax
	movq	1128(%rsp), %rdi
	movq	1120(%rsp), %rcx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdi,%rax,2), %rdi
	movq	1120(%rsp), %rax
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %r10
	movq	1112(%rsp), %rax
	movq	1112(%rsp), %rcx
	movq	%r10, 96(%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %rsi
	movq	1104(%rsp), %rax
	movq	1104(%rsp), %rcx
	movq	%rsi, (%rsp)
	leaq	(%rax,%rax,8), %rax
	leaq	(%rcx,%rax,2), %rax
	movq	%rdi, %rcx
	movq	%rax, 112(%rsp)
	mulq	1008(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	1096(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	984(%rsp)
	movq	%r8, %r14
	movq	%r9, %r15
	addq	%rax, %r14
	movq	984(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1104(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r11
	movq	992(%rsp), %rax
	movq	%rdx, %r12
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	984(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	1112(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rcx, %rax
	addq	%r8, %rsi
	movq	%r11, %r8
	adcq	%r9, %rdi
	mulq	1000(%rsp)
	movq	%r11, 16(%rsp)
	movq	%rcx, 80(%rsp)
	addq	%rax, %rsi
	movq	1000(%rsp), %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r12, %r8
	xorl	%edx, %edx
	addq	%r8, %rsi
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, 32(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	992(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	984(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	1120(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	addq	%r10, %r8
	adcq	%r11, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	1008(%rsp), %rax
	adcq	%rdx, %r9
	shrdq	$51, %rdi, %rsi
	xorl	%edx, %edx
	addq	%rsi, %r8
	movq	32(%rsp), %rsi
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	1000(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r12
	movq	992(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1112(%rsp)
	addq	%rax, %r12
	movq	984(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1120(%rsp)
	addq	%rax, %r12
	movq	976(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1128(%rsp)
	addq	%r12, %rax
	movq	%r8, %r12
	adcq	%r13, %rdx
	shrdq	$51, %r9, %r12
	xorl	%r11d, %r11d
	movq	1080(%rsp), %r13
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	andq	%rbx, %r14
	andq	%rbx, %rsi
	shrdq	$51, %rdx, %rax
	andq	%rbx, %r8
	leaq	(%rax,%rax,8), %rdx
	movq	%r8, 24(%rbp)
	leaq	(%rax,%rdx,2), %rcx
	movq	16(%rsp), %rdx
	movq	%r12, %rax
	movq	1072(%rsp), %r12
	addq	%r14, %rcx
	andq	%rbx, %rax
	movq	%rcx, %rdi
	andq	%rbx, %rdx
	movq	%rax, 32(%rbp)
	andq	%rbx, %rcx
	shrq	$51, %rdi
	movq	%rcx, 0(%rbp)
	addq	%rdi, %rdx
	movq	%rdx, %r15
	shrq	$51, %rdx
	addq	%rdx, %rsi
	movq	1088(%rsp), %rdx
	andq	%rbx, %r15
	movq	%r15, 8(%rbp)
	movq	1064(%rsp), %r15
	leaq	(%rdx,%rdx,8), %rax
	movq	%rsi, 16(%rbp)
	movq	1056(%rsp), %r14
	leaq	(%rdx,%rax,2), %rsi
	leaq	0(%r13,%r13,8), %rax
	leaq	0(%r13,%rax,2), %rcx
	leaq	(%r12,%r12,8), %rax
	leaq	(%r12,%rax,2), %rdi
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movq	%r9, %rax
	mulq	1048(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	mulq	1040(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	movq	%r10, 16(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	1032(%rsp)
	movq	%r10, 32(%rsp)
	movq	%r11, 40(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	mulq	1016(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1048(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1040(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%r10, %r9
	shrdq	$51, %r11, %r9
	movq	%r9, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r8
	movq	%r14, %rax
	movq	%rdx, %r9
	mulq	1040(%rsp)
	movq	%r8, 48(%rsp)
	movq	%r9, 56(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r15, %rax
	mulq	1032(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	1024(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	movq	%r8, %rsi
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r9, %rsi
	xorl	%r11d, %r11d
	movq	%rsi, %r10
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1040(%rsp)
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	mulq	1032(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1024(%rsp)
	addq	%rax, %r8
	movq	1016(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, 64(%rsp)
	andq	%rbx, %r10
	movq	96(%rsp), %r11
	shrdq	$51, %rdx, %rax
	movq	%rdx, 72(%rsp)
	leaq	(%rax,%rax,8), %rdx
	movq	%r10, 64(%rbp)
	leaq	(%rax,%rdx,2), %rcx
	movq	16(%rsp), %rdx
	movq	64(%rsp), %rax
	andq	%rbx, %rdx
	andq	%rbx, %rax
	addq	%rdx, %rcx
	movq	32(%rsp), %rdx
	movq	%rax, 72(%rbp)
	movq	112(%rsp), %rax
	andq	%rbx, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	andq	%rbx, %rcx
	shrq	$51, %rdx
	movq	%rcx, 40(%rbp)
	addq	%rsi, %rdx
	movq	%rdx, %r9
	shrq	$51, %rdx
	andq	%rbx, %r9
	movq	%r9, 48(%rbp)
	movq	48(%rsp), %r9
	andq	%rbx, %r9
	movq	%r9, %rcx
	addq	%rdx, %rcx
	mulq	1088(%rsp)
	movq	%rcx, 56(%rbp)
	movq	80(%rsp), %rcx
	movq	%rax, %rsi
	movq	(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	1096(%rsp)
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r12
	movq	%rsi, %r8
	movq	%rdi, %r9
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%r15
	movq	%r9, %r10
	movq	%r8, %r9
	addq	%rax, %r9
	movq	%r15, %rax
	adcq	%rdx, %r10
	mulq	1096(%rsp)
	movq	%r9, %r8
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	(%rsp), %rax
	movq	%r9, (%rsp)
	adcq	%rdx, %rdi
	mulq	1088(%rsp)
	addq	%rax, %rsi
	movq	%r11, %rax
	adcq	%rdx, %rdi
	mulq	%r13
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	%r12
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r10, %r8
	xorl	%edi, %edi
	addq	%r8, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	movq	%r12, %rax
	movq	%rdx, %rdi
	mulq	1096(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdi, 24(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%r13
	addq	%r8, %rax
	movq	%rsi, %r8
	adcq	%r9, %rdx
	shrdq	$51, %rdi, %r8
	xorl	%r11d, %r11d
	movq	%r8, %r10
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1096(%rsp)
	movq	%r10, %rsi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r12, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %rsi
	xorl	%edi, %edi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %r8
	movq	1088(%rsp), %rax
	movq	%rdx, %r9
	mulq	1096(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r13, %rax
	mulq	1104(%rsp)
	addq	%rax, %rsi
	movq	%r12, %rax
	movq	%r8, %r12
	adcq	%rdx, %rdi
	mulq	1112(%rsp)
	addq	%rax, %rsi
	movq	%r15, %rax
	adcq	%rdx, %rdi
	mulq	1120(%rsp)
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	1128(%rsp)
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r12
	xorl	%edx, %edx
	addq	%r12, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%rbx, %rsi
	andq	%rbx, %r8
	shrdq	$51, %rdi, %rax
	movq	1048(%rsp), %rdi
	movq	%rsi, 112(%rbp)
	leaq	(%rax,%rax,8), %rdx
	movq	%r8, 104(%rbp)
	leaq	(%rax,%rdx,2), %rdx
	movq	(%rsp), %rax
	andq	%rbx, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rcx
	andq	%rbx, %rdx
	andq	%rbx, %rax
	shrq	$51, %rcx
	movq	%rdx, 80(%rbp)
	addq	%rcx, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rbx, %rdx
	movq	%rdx, 88(%rbp)
	movq	%r10, %rdx
	andq	%rbx, %rdx
	addq	%rax, %rdx
	movq	1048(%rsp), %rax
	movq	%rdx, 96(%rbp)
	movq	1024(%rsp), %rdx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdi,%rax,2), %rsi
	movq	1040(%rsp), %rax
	movq	1040(%rsp), %rdi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdi,%rax,2), %rcx
	movq	1032(%rsp), %rax
	movq	1032(%rsp), %rdi
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdi,%rax,2), %rdi
	movq	1024(%rsp), %rax
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdx,%rax,2), %r11
	movq	%r11, %rax
	mulq	1008(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rdi, %rax
	mulq	1000(%rsp)
	addq	%rax, %r10
	movq	1016(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	992(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	984(%rsp)
	movq	%r10, %r14
	movq	%r11, %r15
	addq	%rax, %r14
	movq	1016(%rsp), %rax
	adcq	%rdx, %r15
	mulq	984(%rsp)
	movq	%r14, %r10
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1024(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %r12
	movq	1016(%rsp), %rax
	movq	%rdx, %r13
	mulq	992(%rsp)
	movq	%r12, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	1024(%rsp), %rax
	mulq	984(%rsp)
	addq	%rax, %r10
	movq	1032(%rsp), %rax
	adcq	%rdx, %r11
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r10, %r8
	movq	%r12, %r10
	adcq	%r11, %r9
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	1016(%rsp), %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %r10
	xorl	%edx, %edx
	addq	%r10, %r8
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	1024(%rsp), %rax
	mulq	992(%rsp)
	addq	%rax, %r12
	movq	1032(%rsp), %rax
	adcq	%rdx, %r13
	mulq	984(%rsp)
	addq	%rax, %r12
	movq	1040(%rsp), %rax
	adcq	%rdx, %r13
	mulq	976(%rsp)
	addq	%rax, %r12
	movq	%rsi, %rax
	movq	%r8, %rsi
	adcq	%rdx, %r13
	mulq	1008(%rsp)
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r9, %rsi
	xorl	%r11d, %r11d
	movq	%rsi, %r10
	addq	%rax, %r10
	movq	1016(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	1024(%rsp), %rax
	mulq	1000(%rsp)
	addq	%rax, %rsi
	movq	1032(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	992(%rsp)
	addq	%rax, %rsi
	movq	1040(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	984(%rsp)
	addq	%rax, %rsi
	movq	1048(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	976(%rsp)
	addq	%rax, %rsi
	movq	%r10, %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r11, %rax
	xorl	%edx, %edx
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %rax
	andq	%rbx, %r14
	andq	%rbx, %r8
	shrdq	$51, %rdi, %rax
	andq	%rbx, %r10
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	(%rsp), %rax
	addq	%r14, %rdx
	movq	%rdx, %rcx
	andq	%rbx, %rax
	andq	%rbx, %rdx
	shrq	$51, %rcx
	movq	%rdx, 120(%rbp)
	addq	%rcx, %rax
	movq	%r10, 144(%rbp)
	movq	%rax, %rdx
	shrq	$51, %rax
	movq	$0, (%rsp)
	andq	%rbx, %rdx
	addq	%r8, %rax
	andq	%rsi, %rbx
	movq	%rdx, 128(%rbp)
	movq	%rax, 136(%rbp)
	movq	%rbx, 152(%rbp)
	.p2align 4,,10
	.p2align 3
.L336:
	movq	(%rsp), %rax
	leaq	720(%rsp), %rdi
	movsbl	656(%rsp,%rax), %edx
	movl	%eax, %esi
	sarl	%esi
	call	table_select
	leaq	720(%rsp), %rdx
	movq	%rbp, %rsi
	leaq	976(%rsp), %rdi
	call	ge_madd
	movq	1128(%rsp), %rbx
	movq	1120(%rsp), %rdi
	movq	$0, 312(%rsp)
	movq	1112(%rsp), %rcx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rbx
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rdi
	leaq	(%rcx,%rcx,8), %rax
	movq	%rbx, %r13
	leaq	(%rcx,%rax,2), %r14
	movq	1104(%rsp), %rcx
	movq	%rdi, 32(%rsp)
	movq	%r14, 96(%rsp)
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rcx
	movq	%rcx, %rax
	mulq	1008(%rsp)
	movq	%rcx, 112(%rsp)
	movabsq	$2251799813685247, %rcx
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	1096(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	984(%rsp)
	movq	%r8, %rbx
	movq	%r9, %rsi
	addq	%rax, %rbx
	movq	984(%rsp), %rax
	adcq	%rdx, %rsi
	mulq	1096(%rsp)
	movq	%rbx, 48(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1104(%rsp), %rax
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rbx, %rax
	movq	%r13, %rbx
	addq	%r8, %r10
	adcq	%r9, %r11
	shrdq	$51, %rsi, %rax
	movq	%rax, 304(%rsp)
	movq	992(%rsp), %rax
	addq	304(%rsp), %r10
	adcq	312(%rsp), %r11
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	984(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	1112(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1008(%rsp)
	movq	%rbx, 16(%rsp)
	movq	$0, 328(%rsp)
	movq	$0, 344(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	1000(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r10, %rax
	addq	%r8, %rsi
	adcq	%r9, %rdi
	shrdq	$51, %r11, %rax
	movq	%rax, 320(%rsp)
	movq	1000(%rsp), %rax
	addq	320(%rsp), %rsi
	adcq	328(%rsp), %rdi
	mulq	1096(%rsp)
	movq	%rsi, 64(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	992(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r12
	movq	984(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1112(%rsp)
	addq	%rax, %r12
	movq	1120(%rsp), %rax
	adcq	%rdx, %r13
	mulq	976(%rsp)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	1008(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	shrdq	$51, %rdi, %rax
	movq	%rax, 336(%rsp)
	movq	1008(%rsp), %rax
	addq	336(%rsp), %r8
	adcq	344(%rsp), %r9
	mulq	1096(%rsp)
	movq	%r8, %r12
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	1000(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r14
	movq	992(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1112(%rsp)
	addq	%rax, %r14
	movq	984(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1120(%rsp)
	addq	%rax, %r14
	movq	976(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1128(%rsp)
	movq	48(%rsp), %rbx
	movq	$0, 360(%rsp)
	movq	1072(%rsp), %rdi
	movq	$0, 376(%rsp)
	addq	%r14, %rax
	adcq	%r15, %rdx
	shrdq	$51, %r9, %r12
	movq	%r12, 352(%rsp)
	addq	352(%rsp), %rax
	adcq	360(%rsp), %rdx
	movq	%rax, %r14
	andq	%rcx, %rbx
	andq	%rcx, %r10
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r8
	leaq	(%rax,%rax,8), %rdx
	movq	%r8, 24(%rbp)
	leaq	(%rax,%rdx,2), %rdx
	movq	%r14, %rax
	addq	%rdx, %rbx
	andq	%rcx, %rax
	movq	%rbx, %rdx
	movq	%rax, 32(%rbp)
	andq	%rcx, %rbx
	shrq	$51, %rdx
	movq	%rbx, 0(%rbp)
	movq	1024(%rsp), %rbx
	addq	%r10, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rcx, %rsi
	movq	%rsi, 8(%rbp)
	movq	64(%rsp), %rsi
	andq	%rcx, %rsi
	addq	%rdx, %rsi
	movq	%rsi, 16(%rbp)
	movq	1088(%rsp), %rsi
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r8
	movq	1080(%rsp), %rsi
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r9
	movq	1064(%rsp), %rsi
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r10
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r11
	movq	%r11, %rax
	mulq	1048(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%r10, %rax
	mulq	1040(%rsp)
	addq	%rax, %r14
	movq	1056(%rsp), %rax
	adcq	%rdx, %r15
	mulq	1016(%rsp)
	addq	%rax, %r14
	movq	%r9, %rax
	adcq	%rdx, %r15
	mulq	1032(%rsp)
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	%rbx
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	1056(%rsp)
	movq	%r14, %r12
	movq	%r15, %r13
	movq	%r12, 48(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	mulq	1016(%rsp)
	addq	%rax, %r14
	movq	%r10, %rax
	adcq	%rdx, %r15
	mulq	1048(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r9, %rax
	addq	%r14, %r10
	adcq	%r15, %r11
	mulq	1040(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	1032(%rsp)
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	movq	%r10, 368(%rsp)
	addq	368(%rsp), %rax
	adcq	376(%rsp), %rdx
	movq	%rax, %r14
	movq	1032(%rsp), %rax
	movq	$0, 392(%rsp)
	movq	%rdx, %r15
	mulq	1056(%rsp)
	movq	%r14, 64(%rsp)
	movq	%r15, 72(%rsp)
	movq	$0, 408(%rsp)
	movq	%rax, %r10
	movq	%rbx, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	1016(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	1048(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	1040(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r14, %r11
	shrdq	$51, %r15, %r11
	movq	%r11, 384(%rsp)
	addq	384(%rsp), %rax
	adcq	392(%rsp), %rdx
	movq	%rax, %r14
	movq	1040(%rsp), %rax
	movq	%rdx, %r15
	mulq	1056(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rsi, %rax
	mulq	1032(%rsp)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	1080(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1016(%rsp)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	1048(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	shrdq	$51, %r15, %rax
	movq	%rax, 400(%rsp)
	movq	%r9, %r10
	movq	1048(%rsp), %rax
	movq	%r8, %r9
	addq	400(%rsp), %r9
	adcq	408(%rsp), %r10
	mulq	1056(%rsp)
	movq	%r9, %r8
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rsi, %rax
	mulq	1040(%rsp)
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	1032(%rsp)
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	1080(%rsp)
	addq	%rax, %r12
	movq	1016(%rsp), %rax
	adcq	%rdx, %r13
	mulq	1088(%rsp)
	movq	%r9, 80(%rsp)
	movq	%r10, 88(%rsp)
	movq	32(%rsp), %r15
	movq	$0, 424(%rsp)
	movq	$0, 440(%rsp)
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r10, %r8
	movq	64(%rsp), %r10
	movq	16(%rsp), %r13
	movq	%r8, 416(%rsp)
	addq	416(%rsp), %rax
	adcq	424(%rsp), %rdx
	movq	%rax, %r12
	andq	%rcx, %r10
	movq	80(%rsp), %r8
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	andq	%rcx, %r8
	leaq	(%rax,%rdx,2), %r9
	movq	48(%rsp), %rdx
	movq	%r12, %rax
	movq	%r8, 64(%rbp)
	andq	%rcx, %rax
	andq	%rcx, %rdx
	movq	%rax, 72(%rbp)
	movq	112(%rsp), %rax
	addq	%rdx, %r9
	movq	%r9, %rdx
	andq	%rcx, %r9
	shrq	$51, %rdx
	movq	%r9, 40(%rbp)
	addq	%r10, %rdx
	movq	%rdx, %r9
	shrq	$51, %rdx
	andq	%rcx, %r9
	movq	%r9, 48(%rbp)
	movq	%r14, %r9
	movq	96(%rsp), %r14
	andq	%rcx, %r9
	addq	%rdx, %r9
	mulq	1088(%rsp)
	movq	%r9, 56(%rbp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r14, %rax
	mulq	1080(%rsp)
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1096(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	movq	%r8, %r10
	movq	%r9, %r11
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	movq	%r11, %r12
	movq	%r10, %r11
	addq	%rax, %r11
	movq	%rsi, %rax
	adcq	%rdx, %r12
	mulq	1096(%rsp)
	movq	%r11, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1056(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	1088(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	1080(%rsp)
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%r8, %rax
	movq	%r11, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r12, %r8
	movq	%r8, 432(%rsp)
	addq	432(%rsp), %rax
	adcq	440(%rsp), %rdx
	movq	%rax, %r14
	movq	%rdi, %rax
	movq	$0, 456(%rsp)
	movq	%rdx, %r15
	movq	%r14, %r9
	movq	$0, 472(%rsp)
	mulq	1096(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	1104(%rsp)
	addq	%rax, %r10
	movq	1056(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1112(%rsp)
	addq	%rax, %r10
	movq	32(%rsp), %rax
	adcq	%rdx, %r11
	mulq	1088(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	1080(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r9
	movq	%r9, 448(%rsp)
	addq	448(%rsp), %rax
	adcq	456(%rsp), %rdx
	movq	%rax, %r12
	movq	1080(%rsp), %rax
	movq	%rdx, %r13
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	1056(%rsp), %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	16(%rsp), %rax
	addq	%r8, %r10
	adcq	%r9, %r11
	mulq	1088(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	shrdq	$51, %r13, %rax
	movq	%rax, 464(%rsp)
	movq	1088(%rsp), %rax
	addq	464(%rsp), %r10
	adcq	472(%rsp), %r11
	mulq	1096(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	1080(%rsp), %rax
	mulq	1104(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	1112(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	1120(%rsp)
	movq	$0, 488(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	1056(%rsp), %rax
	addq	%r8, %rsi
	adcq	%r9, %rdi
	mulq	1128(%rsp)
	addq	%rsi, %rax
	movq	%r10, %rsi
	adcq	%rdi, %rdx
	shrdq	$51, %r11, %rsi
	movq	1048(%rsp), %rdi
	movq	%rsi, 480(%rsp)
	addq	480(%rsp), %rax
	adcq	488(%rsp), %rdx
	movq	%rax, %r8
	andq	%rcx, %r14
	andq	%rcx, %r10
	shrdq	$51, %rdx, %rax
	movq	%r10, 104(%rbp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rsi
	movq	48(%rsp), %rdx
	movq	%r8, %rax
	andq	%rcx, %rax
	andq	%rcx, %rdx
	movq	%rax, 112(%rbp)
	leaq	(%rdi,%rdi,8), %rax
	addq	%rdx, %rsi
	movq	%rsi, %rdx
	andq	%rcx, %rsi
	shrq	$51, %rdx
	movq	%rsi, 80(%rbp)
	addq	%r14, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rcx, %rsi
	movq	%rsi, 88(%rbp)
	movq	%r12, %rsi
	andq	%rcx, %rsi
	addq	%rdx, %rsi
	movq	%rsi, 96(%rbp)
	leaq	(%rdi,%rax,2), %rsi
	movq	1040(%rsp), %rdi
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r8
	movq	1032(%rsp), %rdi
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rdi
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r11
	movq	%r11, %rax
	mulq	1008(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	1000(%rsp)
	addq	%rax, %r12
	movq	1016(%rsp), %rax
	adcq	%rdx, %r13
	mulq	976(%rsp)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	992(%rsp)
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	984(%rsp)
	movq	%r12, %r14
	movq	%r13, %r15
	addq	%rax, %r14
	movq	1016(%rsp), %rax
	adcq	%rdx, %r15
	mulq	984(%rsp)
	movq	%r14, 16(%rsp)
	movq	%r15, 24(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rbx, %rax
	mulq	976(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	1008(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	1000(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	992(%rsp)
	addq	%r10, %rax
	movq	%r14, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r10
	movq	%r10, 496(%rsp)
	movq	$0, 504(%rsp)
	addq	496(%rsp), %rax
	adcq	504(%rsp), %rdx
	movq	%rax, %r14
	movq	1016(%rsp), %rax
	movq	$0, 520(%rsp)
	movq	$0, 536(%rsp)
	movq	%rdx, %r15
	mulq	992(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rbx, %rax
	mulq	984(%rsp)
	addq	%rax, %r12
	movq	1032(%rsp), %rax
	adcq	%rdx, %r13
	mulq	976(%rsp)
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	1008(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r12, %r8
	adcq	%r13, %r9
	mulq	1000(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r14, %rax
	addq	%r8, %r10
	adcq	%r9, %r11
	shrdq	$51, %r15, %rax
	movq	%rax, 512(%rsp)
	movq	1016(%rsp), %rax
	addq	512(%rsp), %r10
	adcq	520(%rsp), %r11
	mulq	1000(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rbx, %rax
	mulq	992(%rsp)
	addq	%rax, %r12
	movq	1032(%rsp), %rax
	adcq	%rdx, %r13
	mulq	984(%rsp)
	addq	%rax, %r12
	movq	1040(%rsp), %rax
	adcq	%rdx, %r13
	mulq	976(%rsp)
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	1008(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r10, %rax
	addq	%r12, %rsi
	adcq	%r13, %rdi
	shrdq	$51, %r11, %rax
	movq	%rax, 528(%rsp)
	movq	1016(%rsp), %rax
	addq	528(%rsp), %rsi
	adcq	536(%rsp), %rdi
	mulq	1008(%rsp)
	movq	%rsi, %r12
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	1000(%rsp)
	addq	%rax, %r8
	movq	1032(%rsp), %rax
	adcq	%rdx, %r9
	mulq	992(%rsp)
	movq	$0, 552(%rsp)
	addq	%rax, %r8
	movq	1040(%rsp), %rax
	adcq	%rdx, %r9
	mulq	984(%rsp)
	addq	%rax, %r8
	movq	1048(%rsp), %rax
	adcq	%rdx, %r9
	mulq	976(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %rdi, %rsi
	movq	16(%rsp), %rdi
	movq	%rsi, 544(%rsp)
	addq	544(%rsp), %rax
	movq	%r12, %rsi
	adcq	552(%rsp), %rdx
	movq	%rax, 32(%rsp)
	andq	%rcx, %rdi
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	movq	%rdx, 40(%rsp)
	andq	%rcx, %r10
	andq	%rcx, %rsi
	leaq	(%rax,%rax,8), %rdx
	addq	$2, (%rsp)
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	movq	%rsi, 144(%rbp)
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	andq	%rcx, %rax
	andq	%rcx, %rdx
	shrq	$51, %rdi
	movq	%rdx, 120(%rbp)
	addq	%rdi, %r14
	movq	%rax, 152(%rbp)
	movq	(%rsp), %rax
	movq	%r14, %rdx
	shrq	$51, %r14
	andq	%rcx, %rdx
	addq	%r14, %r10
	movq	%rdx, 128(%rbp)
	movq	%r10, 136(%rbp)
	cmpq	$64, %rax
	jne	.L336
	addq	$1144, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE83:
	.size	x25519_ge_scalarmult_base, .-x25519_ge_scalarmult_base
	.p2align 4
	.globl	ED25519_sign
	.type	ED25519_sign, @function
ED25519_sign:
.LFB85:
	.cfi_startproc
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	movq	%rsi, %r15
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	movq	%rdx, %r14
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movabsq	$274877906976, %rbx
	subq	$952, %rsp
	.cfi_def_cfa_offset 1008
	movdqu	16(%rcx), %xmm7
	movdqu	(%rcx), %xmm5
	movdqa	.LC0(%rip), %xmm0
	movdqa	.LC1(%rip), %xmm1
	movq	%rdi, 224(%rsp)
	leaq	368(%rsp), %rdi
	movdqa	.LC2(%rip), %xmm2
	movdqa	.LC3(%rip), %xmm3
	movq	%rsi, 232(%rsp)
	movl	$64, %esi
	movdqa	.LC8(%rip), %xmm4
	movq	%rdx, 240(%rsp)
	leaq	720(%rsp), %rdx
	movq	%rcx, 248(%rsp)
	movq	%rbx, 928(%rsp)
	movaps	%xmm0, 720(%rsp)
	movaps	%xmm1, 736(%rsp)
	movaps	%xmm2, 752(%rsp)
	movaps	%xmm3, 768(%rsp)
	movaps	%xmm4, 784(%rsp)
	movaps	%xmm5, 800(%rsp)
	movaps	%xmm7, (%rsp)
	movaps	%xmm7, 816(%rsp)
	call	sha512_final_impl
	xorl	%eax, %eax
	movl	$27, %ecx
	leaq	720(%rsp), %rdi
	rep stosq
	leaq	720(%rsp), %rax
	movzbl	399(%rsp), %eax
	movdqa	.LC0(%rip), %xmm1
	movq	%r14, %rdx
	movq	%r15, %rsi
	movdqa	.LC1(%rip), %xmm2
	movdqa	.LC2(%rip), %xmm3
	leaq	720(%rsp), %rdi
	movq	%rbx, 928(%rsp)
	movdqa	.LC3(%rip), %xmm4
	andl	$63, %eax
	movdqa	.LC8(%rip), %xmm5
	movaps	%xmm1, 720(%rsp)
	movdqa	400(%rsp), %xmm6
	orl	$64, %eax
	movdqa	416(%rsp), %xmm7
	movaps	%xmm2, 736(%rsp)
	andb	$-8, 368(%rsp)
	movaps	%xmm3, 752(%rsp)
	movaps	%xmm4, 768(%rsp)
	movaps	%xmm5, 784(%rsp)
	movaps	%xmm6, 800(%rsp)
	movaps	%xmm7, 816(%rsp)
	movb	%al, 399(%rsp)
	call	SHA512_Update
	movl	932(%rsp), %esi
	leaq	720(%rsp), %rdx
	leaq	432(%rsp), %rdi
	call	sha512_final_impl
	leaq	432(%rsp), %rdi
	call	x25519_sc_reduce
	leaq	432(%rsp), %rsi
	leaq	560(%rsp), %rdi
	call	x25519_ge_scalarmult_base
	movq	672(%rsp), %rax
	movq	672(%rsp), %rbx
	movq	664(%rsp), %rdx
	leaq	(%rax,%rax,8), %rax
	leaq	(%rbx,%rax,2), %rbx
	movq	664(%rsp), %rax
	leaq	(%rbx,%rbx), %rdi
	leaq	(%rax,%rax), %rsi
	movq	656(%rsp), %rax
	leaq	(%rax,%rax), %rcx
	movq	664(%rsp), %rax
	leaq	(%rax,%rax,8), %rax
	leaq	(%rdx,%rax,2), %r8
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	656(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	648(%rsp)
	addq	%rax, %r12
	movq	640(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rax
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	664(%rsp)
	movq	%r12, %r10
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	656(%rsp)
	addq	%rax, %r8
	movq	648(%rsp), %rax
	adcq	%rdx, %r9
	addq	%rax, %rax
	mulq	640(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r10
	xorl	%r15d, %r15d
	movq	%r10, %r14
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	664(%rsp)
	movq	%r14, %r10
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rcx, %rax
	mulq	640(%rsp)
	addq	%rax, %r8
	movq	648(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rax
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r15, %r10
	xorl	%r9d, %r9d
	addq	%r10, %rax
	adcq	%r9, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rax
	movq	%rdx, %rbp
	mulq	672(%rsp)
	movq	%rdi, (%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rcx, %rax
	movq	%rdi, %rcx
	mulq	648(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r10, %r8
	adcq	%r11, %r9
	mulq	640(%rsp)
	addq	%rax, %r8
	movq	672(%rsp), %rax
	adcq	%rdx, %r9
	shrdq	$51, %rbp, %rcx
	xorl	%edx, %edx
	leaq	(%rax,%rax), %rbx
	addq	%rcx, %r8
	movabsq	$2251799813685247, %rcx
	adcq	%rdx, %r9
	movq	%rbx, %rax
	movq	%r8, %rbp
	mulq	640(%rsp)
	andq	%rcx, %rbp
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	648(%rsp)
	movq	%rax, %rsi
	movq	656(%rsp), %rax
	movq	%rdx, %rdi
	addq	%r10, %rsi
	adcq	%r11, %rdi
	mulq	%rax
	addq	%rax, %rsi
	adcq	%rdx, %rdi
	shrdq	$51, %r9, %r8
	xorl	%edx, %edx
	leaq	(%rbp,%rbp), %r9
	addq	%r8, %rsi
	adcq	%rdx, %rdi
	movq	%rsi, %r11
	andq	%rcx, %r12
	shrdq	$51, %rdi, %rsi
	andq	%rcx, %r11
	leaq	(%rsi,%rsi,8), %rax
	movq	%r11, 64(%rsp)
	leaq	(%rsi,%rax,2), %rdx
	movq	%r14, %rax
	addq	%r12, %rdx
	andq	%rcx, %rax
	movq	%rdx, %rbx
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	andq	%rcx, %rbx
	movq	%rax, %r14
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %r14
	leaq	(%rdx,%rax), %r15
	leaq	(%r11,%r11,8), %rax
	movq	%r14, %r10
	leaq	(%r11,%rax,2), %r8
	movq	%rbx, %rax
	movq	%rbx, %r11
	movq	%r15, 16(%rsp)
	mulq	%rbx
	leaq	(%r8,%r8), %rdi
	movq	%r10, 80(%rsp)
	leaq	(%r15,%r15), %rsi
	movq	%r11, 32(%rsp)
	movq	%rax, %r12
	movq	%r14, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %rbx
	adcq	%rdx, %r13
	movq	%r12, %r14
	leaq	(%rbx,%rbx), %rax
	mulq	%r15
	movq	%r13, %r15
	leaq	(%r10,%r10), %r13
	addq	%rax, %r14
	movq	%r13, %rax
	adcq	%rdx, %r15
	mulq	%r11
	movq	%rax, %r12
	movq	16(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	%rbx, %rax
	movq	32(%rsp), %rbx
	adcq	%rdx, %r13
	mulq	%rbp
	addq	%r12, %rax
	movq	%r14, %r12
	adcq	%r13, %rdx
	movq	%r15, %r13
	shrdq	$51, %r15, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r10, %rax
	adcq	%rdx, %r13
	mulq	%r10
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	movq	%r13, %r11
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, (%rsp)
	movq	%r8, %rax
	movq	%rdx, 8(%rsp)
	mulq	64(%rsp)
	movq	%rax, %r10
	movq	%rbx, %rax
	movq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	80(%rsp)
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	%r10, %rsi
	adcq	%r11, %rdi
	movq	64(%rsp), %r11
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %rsi
	leaq	(%r11,%r11), %r10
	adcq	%rdx, %rdi
	movq	%rsi, %r8
	movq	%r10, %rax
	andq	%rcx, %r8
	mulq	%rbx
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r9, %rax
	movq	16(%rsp), %r9
	mulq	80(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	%r9
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	andq	%rcx, %rsi
	leaq	(%rax,%rax,8), %rdx
	movq	%rsi, %rbx
	movq	%r12, %rsi
	leaq	(%rax,%rdx,2), %rdx
	andq	%rcx, %rsi
	addq	%r14, %rdx
	movq	%rsi, %rax
	movq	%rdx, %rdi
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	andq	%rcx, %rdi
	movq	%rax, %rsi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rsi
	leaq	(%rdx,%rax), %r14
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r15
	leaq	(%r8,%r8), %rax
	leaq	(%r15,%r15), %r9
	movq	%rax, (%rsp)
	leaq	(%r14,%r14), %rdx
	movq	%r9, %rax
	movq	%rdx, %r11
	mulq	%rsi
	movq	%r11, 96(%rsp)
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %r10
	adcq	%rdx, %r13
	leaq	(%r10,%r10), %rax
	mulq	%r14
	addq	%rax, %r12
	adcq	%rdx, %r13
	movq	%r12, 48(%rsp)
	movq	%r13, 56(%rsp)
	leaq	(%rsi,%rsi), %r13
	movq	%r13, %rax
	mulq	%rdi
	movq	%rax, %r12
	movq	%r14, %rax
	movq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	%r10, %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	movq	48(%rsp), %r12
	adcq	%r13, %rdx
	movq	56(%rsp), %r13
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r9, %rax
	adcq	%rdx, %r13
	mulq	%r8
	movq	%rax, %r8
	movq	%rsi, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%r8, %rax
	movq	%r12, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r8
	movq	%r13, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	%r15, %rax
	movq	(%rsp), %r15
	movq	%rdx, %r11
	mulq	%rbx
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	96(%rsp), %rax
	movq	%r10, 96(%rsp)
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	movq	%r10, %r8
	leaq	(%rbx,%rbx), %r10
	adcq	%r9, %rdx
	movq	%r11, %r9
	shrdq	$51, %r11, %r8
	shrq	$51, %r9
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	movq	%r8, %rdx
	andq	%rcx, %rdx
	movq	%rdx, (%rsp)
	mulq	%rdi
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rdx, %r11
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	addq	%r10, %rsi
	movq	96(%rsp), %r10
	adcq	%r11, %rdi
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	addq	%rsi, %r14
	adcq	%rdi, %r15
	movq	%r14, %rax
	shrdq	$51, %r9, %r8
	movq	%r15, %rdx
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r15
	andq	%rcx, %r10
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r15
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	48(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	%r12, %rax
	andq	%rcx, %rax
	movq	%rdx, %r14
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	andq	%rcx, %r14
	movq	%rax, %rsi
	shrq	$51, %rax
	leaq	(%r10,%rax), %rbx
	leaq	(%r15,%r15,8), %rax
	andq	%rcx, %rsi
	leaq	(%r15,%rax,2), %rdi
	leaq	(%rdx,%rdx,8), %rax
	leaq	(%rdx,%rax,2), %r8
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r9
	movq	%r8, %rax
	mulq	656(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	648(%rsp)
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	640(%rsp)
	addq	%rax, %r12
	leaq	(%rsi,%rsi,8), %rax
	adcq	%rdx, %r13
	leaq	(%rsi,%rax,2), %rax
	mulq	672(%rsp)
	addq	%rax, %r12
	movq	%r9, %rax
	adcq	%rdx, %r13
	mulq	664(%rsp)
	movq	%r12, %r10
	movq	%r13, %r11
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	664(%rsp)
	movq	%r10, 48(%rsp)
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rdi, %rax
	mulq	656(%rsp)
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	648(%rsp)
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	640(%rsp)
	addq	%rax, %r12
	movq	%r9, %rax
	adcq	%rdx, %r13
	mulq	672(%rsp)
	movq	%r11, 56(%rsp)
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r12
	xorl	%r11d, %r11d
	addq	%r12, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	672(%rsp)
	movq	%r12, 96(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	664(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	656(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	648(%rsp)
	movq	%r8, %r10
	movq	%r9, %r11
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	640(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	shrdq	$51, %r13, %r11
	xorl	%r13d, %r13d
	movq	%r11, %r12
	addq	%rax, %r12
	movq	%rdi, %rax
	movq	(%rsp), %rdi
	adcq	%rdx, %r13
	mulq	672(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	mulq	640(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	664(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	656(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rbx, %rax
	addq	%r8, %r10
	movq	%r12, %r8
	adcq	%r9, %r11
	mulq	648(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	shrdq	$51, %r13, %r8
	xorl	%edx, %edx
	addq	%r8, %r10
	adcq	%rdx, %r11
	movq	%r10, %rdx
	andq	%rcx, %rdx
	movq	%rdx, (%rsp)
	mulq	648(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r15, %rax
	mulq	640(%rsp)
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	672(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	664(%rsp)
	addq	%rax, %r8
	movq	%rbx, %rax
	movq	32(%rsp), %rbx
	adcq	%rdx, %r9
	mulq	656(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	xorl	%edi, %edi
	movq	80(%rsp), %r11
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%rdi, %rdx
	movq	%rax, %rsi
	andq	%rcx, %r10
	shrdq	$51, %rdx, %rax
	andq	%rcx, %rsi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	48(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	96(%rsp), %rax
	movq	%rsi, 96(%rsp)
	movq	%rdx, %r15
	shrq	$51, %rdx
	andq	%rcx, %rax
	andq	%rcx, %r15
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	movq	%rax, %r14
	shrq	$51, %rax
	leaq	(%r10,%rax), %r10
	leaq	(%rsi,%rsi,8), %rax
	andq	%rcx, %r14
	leaq	(%rsi,%rax,2), %rsi
	leaq	(%rdx,%rdx,8), %rax
	movq	%r14, %r9
	movq	%r10, 48(%rsp)
	leaq	(%rdx,%rax,2), %r8
	leaq	(%r10,%r10,8), %rax
	movq	%r9, 120(%rsp)
	leaq	(%r10,%rax,2), %rdi
	movq	16(%rsp), %rax
	movq	%r15, %r10
	movq	%r10, 32(%rsp)
	mulq	%r8
	movq	%rax, %r12
	movq	%r11, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r15
	addq	%rax, %r12
	leaq	(%r14,%r14,8), %rax
	adcq	%rdx, %r13
	leaq	(%r14,%rax,2), %rax
	mulq	64(%rsp)
	addq	%rax, %r12
	movq	%rbp, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%r12, %r14
	movq	%r13, %r15
	addq	%rax, %r14
	movq	%rbp, %rax
	adcq	%rdx, %r15
	mulq	%r8
	movq	%rax, %r12
	movq	16(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r11, %rax
	movq	%r15, %r11
	adcq	%rdx, %r13
	mulq	%r10
	movq	%r14, %r10
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r9
	addq	%rax, %r12
	movq	%rdi, %rax
	movq	64(%rsp), %rdi
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r15, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r8
	movq	%rbp, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	mulq	32(%rsp)
	movq	%r8, %r10
	movq	%r12, 128(%rsp)
	movq	%r9, %r11
	addq	%rax, %r10
	movq	80(%rsp), %rax
	adcq	%rdx, %r11
	mulq	120(%rsp)
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	48(%rsp)
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rbx, %rax
	mulq	(%rsp)
	addq	%rax, %rsi
	movq	32(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	%rbp
	addq	%rax, %rsi
	movq	16(%rsp), %rax
	adcq	%rdx, %rdi
	mulq	120(%rsp)
	movq	%rsi, %r8
	movq	80(%rsp), %rsi
	movq	%rdi, %r9
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	48(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r12, %rax
	movq	%r13, %rdx
	addq	%r8, %r10
	adcq	%r9, %r11
	shrdq	$51, %r13, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%r10, %rax
	adcq	%rdx, %r11
	andq	%rcx, %rax
	movq	%rax, %rdi
	movq	%rsi, %rax
	mulq	(%rsp)
	movq	%rdi, 344(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	mulq	96(%rsp)
	addq	%rax, %r8
	movq	64(%rsp), %rax
	adcq	%rdx, %r9
	mulq	32(%rsp)
	addq	%rax, %r8
	movq	%rbp, %rax
	adcq	%rdx, %r9
	mulq	120(%rsp)
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	mulq	48(%rsp)
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r10
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r10
	leaq	(%rax,%rax,8), %rdx
	movq	%r10, %rbx
	movq	%r10, 352(%rsp)
	leaq	(%rax,%rdx,2), %rdx
	addq	%r14, %rdx
	movq	%rdx, %rax
	shrq	$51, %rdx
	andq	%rcx, %rax
	movq	%rax, %r8
	movq	128(%rsp), %rax
	movq	%r8, 320(%rsp)
	andq	%rcx, %rax
	addq	%rdx, %rax
	movq	%r12, %rdx
	movq	%rax, %r14
	andq	%rcx, %rdx
	shrq	$51, %rax
	andq	%rcx, %r14
	movq	%r14, %rsi
	leaq	(%rdx,%rax), %r14
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %r15
	leaq	(%rdi,%rdi), %rax
	movq	%rsi, 328(%rsp)
	leaq	(%r15,%r15), %r9
	movq	%rax, 16(%rsp)
	leaq	(%r14,%r14), %r10
	movq	%r9, %rax
	movq	%r10, 64(%rsp)
	mulq	%rsi
	movq	%r14, 336(%rsp)
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rbp
	adcq	%rdx, %r13
	movq	%r12, %r10
	leaq	(%rbp,%rbp), %rax
	movq	%r13, %r11
	leaq	(%rsi,%rsi), %r13
	mulq	%r14
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%r8
	movq	%r10, 80(%rsp)
	movq	%rax, %r12
	movq	%r9, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%rbp, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	movq	%r11, %r13
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	%r9
	movq	64(%rsp), %r9
	movq	%r12, %rdi
	movq	%r13, %rbp
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %rdi
	shrq	$51, %rbp
	addq	%rdi, %rax
	adcq	%rbp, %rdx
	movq	%rax, %rdi
	movq	%r15, %rax
	movq	16(%rsp), %r15
	movq	%rdx, %rbp
	mulq	%rbx
	movq	%rdi, 16(%rsp)
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rdx, %r11
	mulq	%r8
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	shrdq	$51, %rbp, %rax
	movq	%rbp, %rdx
	shrq	$51, %rdx
	addq	%rax, %r10
	adcq	%rdx, %r11
	addq	%rbx, %rbx
	movq	%r10, %rbp
	movq	%rbx, %rax
	andq	%rcx, %rbp
	mulq	%r8
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	addq	%r8, %rsi
	adcq	%r9, %rdi
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	addq	%rsi, %r14
	adcq	%rdi, %r15
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %r14
	movq	16(%rsp), %r10
	adcq	%r11, %r15
	movq	%r14, %rax
	movq	%r15, %rdx
	movq	%r14, %r15
	andq	%rcx, %r10
	shrdq	$51, %rdx, %rax
	andq	%rcx, %r15
	movq	%r10, %rsi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	80(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	%r12, %rax
	andq	%rcx, %rax
	movq	%rdx, %r14
	shrq	$51, %rdx
	addq	%rdx, %rax
	andq	%rcx, %r14
	movq	%rax, %rdi
	shrq	$51, %rax
	addq	%rax, %rsi
	leaq	(%r15,%r15,8), %rax
	andq	%rcx, %rdi
	leaq	(%r15,%rax,2), %rbx
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %r9
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r8
	movq	48(%rsp), %rax
	mulq	%r9
	movq	%rax, %r12
	movq	120(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rbx
	addq	%rax, %r12
	movq	32(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	leaq	(%rdi,%rdi,8), %rax
	adcq	%rdx, %r13
	leaq	(%rdi,%rax,2), %rax
	mulq	96(%rsp)
	addq	%rax, %r12
	movq	(%rsp), %rax
	adcq	%rdx, %r13
	movq	%r12, %r10
	mulq	%r8
	movq	%r13, %r11
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	movq	%r10, 64(%rsp)
	mulq	%r9
	movq	%r11, 72(%rsp)
	movq	%rax, %r12
	movq	48(%rsp), %rax
	movq	%rdx, %r13
	mulq	%rbx
	addq	%rax, %r12
	movq	120(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	32(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	%r8, %rax
	movq	96(%rsp), %r8
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%r9, %rax
	movq	%rdx, %r13
	mulq	%r8
	movq	%r12, 80(%rsp)
	movq	%rax, %r8
	movq	(%rsp), %rax
	movq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	48(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	adcq	%rdx, %r9
	movq	%r8, %r10
	movq	32(%rsp), %r8
	movq	%r9, %r11
	movq	120(%rsp), %r9
	movq	%r9, %rax
	mulq	%rdi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rbx, %rax
	movq	%r9, %rbx
	adcq	%rdx, %r13
	mulq	96(%rsp)
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	48(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%r12, %rax
	movq	%r13, %rdx
	addq	%r10, %r8
	adcq	%r11, %r9
	shrdq	$51, %r13, %rax
	shrq	$51, %rdx
	addq	%rax, %r8
	movq	%r8, %rax
	adcq	%rdx, %r9
	andq	%rcx, %rax
	movq	%rax, 16(%rsp)
	movq	%rbx, %rax
	mulq	%rbp
	movq	%rax, %r10
	movq	32(%rsp), %rax
	movq	%rdx, %r11
	mulq	%r15
	addq	%rax, %r10
	movq	96(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r14
	movq	%rax, %r14
	movq	(%rsp), %rax
	movq	%rdx, %r15
	addq	%r10, %r14
	adcq	%r11, %r15
	mulq	%rdi
	addq	%rax, %r14
	movq	48(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	addq	%r14, %rsi
	adcq	%r15, %rdi
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%r8, %rsi
	adcq	%r9, %rdi
	movq	%rsi, %r11
	andq	%rcx, %r12
	movq	16(%rsp), %r9
	shrdq	$51, %rdi, %rsi
	andq	%rcx, %r11
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rdx
	movq	64(%rsp), %rax
	movq	%r11, 64(%rsp)
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	80(%rsp), %rax
	movq	%rdx, %rbx
	shrq	$51, %rdx
	andq	%rcx, %rax
	andq	%rcx, %rbx
	addq	%rdx, %rax
	movq	%rax, %rbp
	shrq	$51, %rax
	leaq	(%r12,%rax), %rdi
	leaq	(%r11,%r11,8), %rax
	andq	%rcx, %rbp
	leaq	(%r11,%rax,2), %r8
	leaq	(%r9,%r9,8), %rax
	leaq	(%r8,%r8), %rsi
	leaq	(%r9,%rax,2), %r14
	movq	%r8, 32(%rsp)
	movq	%rsi, %rax
	leaq	(%r9,%r9), %r8
	movq	%rdi, %r9
	movq	%r14, 48(%rsp)
	mulq	%rbp
	movq	%r9, (%rsp)
	leaq	(%rdi,%rdi), %rdi
	movq	%rax, %r12
	movq	%rbx, %rax
	movq	%rdx, %r13
	mulq	%rbx
	addq	%rax, %r12
	leaq	(%r14,%r14), %rax
	adcq	%rdx, %r13
	mulq	%r9
	movq	%r12, %r14
	leaq	(%rbp,%rbp), %r12
	movq	%r13, %r15
	addq	%rax, %r14
	movq	%r12, %rax
	adcq	%rdx, %r15
	mulq	%rbx
	movq	%rax, %r10
	movq	%r9, %rax
	movq	%rdx, %r11
	movq	16(%rsp), %r9
	mulq	%rsi
	movq	%rbx, 120(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	48(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	shrdq	$51, %r15, %rax
	movq	%r15, %rdx
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%r9
	movq	%r10, %r12
	movq	%r11, %r13
	movq	%r12, 80(%rsp)
	movq	%rax, %r10
	movq	%rbp, %rax
	movq	%rdx, %r11
	mulq	%rbp
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	64(%rsp), %rax
	adcq	%rdx, %r11
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%r10, %r12
	adcq	%r11, %r13
	mulq	32(%rsp)
	movq	%rax, %r10
	movq	%rbx, %rax
	movq	%rdx, %r11
	mulq	%r8
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	%rbp
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r12, %rax
	movq	%r13, %rdx
	addq	%r10, %rsi
	adcq	%r11, %rdi
	shrdq	$51, %r13, %rax
	movq	64(%rsp), %r11
	shrq	$51, %rdx
	addq	%rax, %rsi
	movq	%rsi, %rax
	adcq	%rdx, %rdi
	leaq	(%r11,%r11), %r10
	andq	%rcx, %rax
	movq	%rax, %r9
	movq	%r10, %rax
	mulq	%rbx
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	movq	(%rsp), %r8
	mulq	%rbp
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rdi
	andq	%rcx, %r14
	shrdq	$51, %rdx, %rax
	andq	%rcx, %rdi
	leaq	(%rax,%rax,8), %rdx
	movq	%rdi, %rbx
	leaq	(%rax,%rdx,2), %rdx
	addq	%r14, %rdx
	movq	%rdx, %rax
	shrq	$51, %rdx
	andq	%rcx, %rax
	movq	%rax, %r8
	movq	80(%rsp), %rax
	andq	%rcx, %rax
	addq	%rdx, %rax
	movq	%r12, %rdx
	andq	%rcx, %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	leaq	(%rdx,%rax), %r14
	leaq	(%rbx,%rbx,8), %rax
	andq	%rcx, %rdi
	leaq	(%rbx,%rax,2), %r15
	leaq	(%r9,%r9), %rax
	movq	%rax, 144(%rsp)
	leaq	(%r14,%r14), %rdx
	movq	%r8, %rax
	leaq	(%r15,%r15), %rsi
	movq	%rdx, 80(%rsp)
	mulq	%r8
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r10
	adcq	%rdx, %r13
	leaq	(%r10,%r10), %rax
	mulq	%r14
	addq	%rax, %r12
	adcq	%rdx, %r13
	movq	%r12, 96(%rsp)
	movq	%r13, 104(%rsp)
	leaq	(%rdi,%rdi), %r13
	movq	104(%rsp), %r11
	movq	%r13, %rax
	mulq	%r8
	movq	%rax, %r12
	movq	%r14, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r10, %rax
	movq	96(%rsp), %r10
	adcq	%rdx, %r13
	mulq	%r9
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%r12, 128(%rsp)
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	movq	80(%rsp), %rsi
	mulq	%r9
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	movq	%r13, %r12
	shrdq	$51, %r13, %r11
	shrq	$51, %r12
	addq	%r11, %rax
	movq	%rax, 80(%rsp)
	adcq	%r12, %rdx
	movq	%r15, %rax
	movq	144(%rsp), %r15
	movq	%rdx, 88(%rsp)
	mulq	%rbx
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	80(%rsp), %rax
	movq	88(%rsp), %rdx
	addq	%r12, %r10
	leaq	(%rbx,%rbx), %r12
	adcq	%r13, %r11
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	movq	%r10, %rdx
	andq	%rcx, %rdx
	movq	%rdx, %r9
	mulq	%r8
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	96(%rsp), %rdx
	addq	%r12, %r14
	adcq	%r13, %r15
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %r14
	adcq	%r11, %r15
	movq	%r14, %rdi
	andq	%rcx, %rdx
	shrdq	$51, %r15, %r14
	andq	%rcx, %rdi
	leaq	(%r14,%r14,8), %rax
	movq	%rdi, %rbx
	leaq	(%r14,%rax,2), %rax
	addq	%rdx, %rax
	movq	128(%rsp), %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rdi
	addq	%rdx, %rax
	movq	80(%rsp), %rdx
	movq	%rdi, %r8
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rdi
	leaq	(%rax,%rdx), %r14
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r15
	leaq	(%r9,%r9), %rax
	leaq	(%r14,%r14), %rdx
	movq	%rax, 144(%rsp)
	movq	%r8, %rax
	leaq	(%r15,%r15), %rsi
	movq	%rdx, 80(%rsp)
	mulq	%r8
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r10
	adcq	%rdx, %r13
	leaq	(%r10,%r10), %rax
	mulq	%r14
	addq	%rax, %r12
	adcq	%rdx, %r13
	movq	%r12, 96(%rsp)
	movq	%r13, 104(%rsp)
	leaq	(%rdi,%rdi), %r13
	movq	104(%rsp), %r11
	movq	%r13, %rax
	mulq	%r8
	movq	%rax, %r12
	movq	%r14, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r10, %rax
	movq	96(%rsp), %r10
	adcq	%rdx, %r13
	mulq	%r9
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	movq	80(%rsp), %rsi
	mulq	%r9
	movq	%r12, 128(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	movq	%r13, %r12
	shrdq	$51, %r13, %r11
	shrq	$51, %r12
	addq	%r11, %rax
	movq	%rax, 80(%rsp)
	adcq	%r12, %rdx
	movq	%r15, %rax
	movq	144(%rsp), %r15
	movq	%rdx, 88(%rsp)
	mulq	%rbx
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	80(%rsp), %rax
	movq	88(%rsp), %rdx
	addq	%r12, %r10
	leaq	(%rbx,%rbx), %r12
	adcq	%r13, %r11
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	movq	%r10, %rdx
	andq	%rcx, %rdx
	movq	%rdx, %r9
	mulq	%r8
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%rcx, %rdi
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	96(%rsp), %rdx
	addq	%r12, %r14
	adcq	%r13, %r15
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %r14
	adcq	%r11, %r15
	andq	%r14, %rdi
	andq	%rcx, %rdx
	shrdq	$51, %r15, %r14
	movq	%rdi, %rbx
	leaq	(%r14,%r14,8), %rax
	leaq	(%r14,%rax,2), %rax
	addq	%rdx, %rax
	movq	128(%rsp), %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rdi
	addq	%rdx, %rax
	movq	80(%rsp), %rdx
	movq	%rdi, %r8
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rdi
	leaq	(%rax,%rdx), %r14
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r15
	leaq	(%r9,%r9), %rax
	movq	%rax, 144(%rsp)
	leaq	(%r14,%r14), %rdx
	movq	%r8, %rax
	leaq	(%r15,%r15), %rsi
	movq	%rdx, 80(%rsp)
	mulq	%r8
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r10
	adcq	%rdx, %r13
	leaq	(%r10,%r10), %rax
	mulq	%r14
	addq	%rax, %r12
	adcq	%rdx, %r13
	movq	%r12, 96(%rsp)
	movq	%r13, 104(%rsp)
	leaq	(%rdi,%rdi), %r13
	movq	104(%rsp), %r11
	movq	%r13, %rax
	mulq	%r8
	movq	%rax, %r12
	movq	%r14, %rax
	movq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%r10, %rax
	movq	96(%rsp), %r10
	adcq	%rdx, %r13
	mulq	%r9
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%r12, 128(%rsp)
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	movq	80(%rsp), %rsi
	mulq	%r9
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%r8
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%r12, %r11
	movq	%r13, %r12
	shrdq	$51, %r13, %r11
	shrq	$51, %r12
	addq	%r11, %rax
	movq	%rax, 80(%rsp)
	adcq	%r12, %rdx
	movq	%r15, %rax
	movq	144(%rsp), %r15
	movq	%rdx, 88(%rsp)
	mulq	%rbx
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	80(%rsp), %rax
	movq	88(%rsp), %rdx
	addq	%r12, %r10
	leaq	(%rbx,%rbx), %r12
	adcq	%r13, %r11
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	movq	%rcx, %rdx
	andq	%r10, %rdx
	movq	%rdx, %r9
	mulq	%r8
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rdi
	movq	%rcx, %rdi
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	96(%rsp), %rdx
	addq	%r12, %r14
	adcq	%r13, %r15
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %r14
	adcq	%r11, %r15
	andq	%r14, %rdi
	andq	%rcx, %rdx
	shrdq	$51, %r15, %r14
	movq	%rdi, %rbx
	leaq	(%r14,%r14,8), %rax
	leaq	(%r14,%rax,2), %rax
	addq	%rdx, %rax
	movq	128(%rsp), %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rdi
	addq	%rdx, %rax
	movq	80(%rsp), %rdx
	movq	%rax, %rsi
	shrq	$51, %rax
	andq	%rcx, %rdx
	andq	%rcx, %rsi
	leaq	(%rax,%rdx), %r14
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %r15
	leaq	(%r9,%r9), %rax
	leaq	(%r15,%r15), %r8
	leaq	(%r14,%r14), %rdx
	movq	%rax, 96(%rsp)
	movq	%r8, %rax
	movq	%rdx, 128(%rsp)
	mulq	%rsi
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r10
	adcq	%rdx, %r13
	leaq	(%r10,%r10), %rax
	mulq	%r14
	addq	%rax, %r12
	movq	%r12, 80(%rsp)
	leaq	(%rsi,%rsi), %r12
	adcq	%rdx, %r13
	movq	%r12, %rax
	movq	%r13, 88(%rsp)
	movq	88(%rsp), %r11
	mulq	%rdi
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%r10, %rax
	movq	80(%rsp), %r10
	adcq	%rdx, %r13
	mulq	%r9
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	movq	128(%rsp), %r13
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r11
	movq	%r9, %rax
	movq	%rdx, %r12
	mulq	%r8
	movq	%r11, 128(%rsp)
	movq	%r12, 136(%rsp)
	movq	%rax, %r8
	movq	%rsi, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r12, %r11
	shrq	$51, %r12
	addq	%r11, %rax
	adcq	%r12, %rdx
	movq	%rax, %r11
	movq	%r15, %rax
	movq	96(%rsp), %r15
	movq	%rdx, %r12
	mulq	%rbx
	movq	%r11, 96(%rsp)
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rdi
	addq	%rax, %r8
	movq	%r13, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r12, %rax
	movq	%r12, %rdx
	leaq	(%rbx,%rbx), %r12
	shrq	$51, %rdx
	addq	%rax, %r8
	movq	%r12, %rax
	movq	120(%rsp), %rbx
	adcq	%rdx, %r9
	movq	%rcx, %rdx
	andq	%r8, %rdx
	movq	%rdx, %r10
	mulq	%rdi
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r14, %rax
	addq	%r12, %rsi
	adcq	%r13, %rdi
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	addq	%rsi, %r14
	adcq	%rdi, %r15
	shrdq	$51, %r9, %r8
	movq	%rcx, %rdi
	shrq	$51, %r9
	addq	%r8, %r14
	movq	(%rsp), %r8
	adcq	%r9, %r15
	andq	%r14, %rdi
	shrdq	$51, %r15, %r14
	leaq	(%r14,%r14,8), %rax
	leaq	(%r14,%rax,2), %rdx
	movq	80(%rsp), %rax
	andq	%rcx, %rax
	addq	%rax, %rdx
	movq	128(%rsp), %rax
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rcx, %rax
	andq	%rcx, %rsi
	addq	%rdx, %rax
	movq	%rax, %rdx
	shrq	$51, %rax
	andq	%rcx, %rdx
	movq	%rdx, %r9
	movq	96(%rsp), %rdx
	andq	%rcx, %rdx
	leaq	(%rdx,%rax), %r11
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %r8
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %r15
	movq	%r15, %rax
	mulq	%rdi
	movq	%rax, %r14
	movq	%r8, %rax
	movq	%rdx, %r15
	mulq	%r10
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	%rsi
	addq	%rax, %r14
	movq	32(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%r9
	addq	%rax, %r14
	movq	48(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%r11
	addq	%rax, %r14
	movq	%r8, %rax
	movq	32(%rsp), %r8
	adcq	%rdx, %r15
	mulq	%rdi
	movq	%r14, 80(%rsp)
	movq	%r15, 88(%rsp)
	movq	%rax, %r12
	movq	48(%rsp), %rax
	movq	%rdx, %r13
	mulq	%r10
	addq	%rax, %r12
	movq	%rbp, %rax
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r9
	movq	%r12, %r14
	movq	80(%rsp), %r12
	movq	%r13, %r15
	movq	88(%rsp), %r13
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	%r11
	addq	%r14, %rax
	adcq	%r15, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%r12, %rax
	movq	%rax, 96(%rsp)
	movq	48(%rsp), %rax
	adcq	%r13, %rdx
	movq	%rdx, 104(%rsp)
	mulq	%rdi
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	movq	(%rsp), %r8
	mulq	%r10
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%rsi
	movq	%r12, %r14
	movq	%r13, %r15
	movq	96(%rsp), %r13
	addq	%rax, %r14
	movq	%rbp, %rax
	adcq	%rdx, %r15
	mulq	%r9
	addq	%rax, %r14
	movq	%rbx, %rax
	adcq	%rdx, %r15
	mulq	%r11
	addq	%r14, %rax
	movq	104(%rsp), %r14
	adcq	%r15, %rdx
	shrdq	$51, %r14, %r13
	shrq	$51, %r14
	addq	%r13, %rax
	movq	%rax, (%rsp)
	movq	32(%rsp), %rax
	adcq	%r14, %rdx
	movq	%rdx, 8(%rsp)
	mulq	%rdi
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%rdx, %r15
	mulq	%r10
	addq	%rax, %r14
	movq	16(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%rsi
	addq	%rax, %r14
	movq	%r8, %rax
	adcq	%rdx, %r15
	mulq	%r9
	addq	%rax, %r14
	movq	%rbp, %rax
	adcq	%rdx, %r15
	mulq	%r11
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	%r14, %r12
	adcq	%r15, %r13
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r12
	movq	%rbx, %rax
	movq	%rcx, %rbx
	adcq	%rdx, %r13
	movq	%rcx, %rdx
	andq	%r12, %rdx
	movq	%rdx, 152(%rsp)
	mulq	%rdi
	movq	%rax, %r14
	movq	%rbp, %rax
	movq	%rdx, %r15
	mulq	%r10
	addq	%rax, %r14
	movq	64(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%rsi
	movq	%rax, %rsi
	movq	16(%rsp), %rax
	movq	%rdx, %rdi
	addq	%r14, %rsi
	adcq	%r15, %rdi
	mulq	%r9
	addq	%rax, %rsi
	movq	%r8, %rax
	adcq	%rdx, %rdi
	mulq	%r11
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	shrdq	$51, %r13, %r12
	movq	80(%rsp), %rdi
	shrq	$51, %r13
	addq	%r12, %rax
	adcq	%r13, %rdx
	andq	%rax, %rbx
	andq	%rcx, %rdi
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	addq	%rdi, %rdx
	movq	%rdx, %r11
	shrq	$51, %rdx
	andq	%rcx, %r11
	movq	%r11, 64(%rsp)
	movq	96(%rsp), %rdi
	movq	%r11, %rbp
	movq	(%rsp), %rsi
	movq	152(%rsp), %r9
	movq	%rbx, 168(%rsp)
	andq	%rcx, %rdi
	movl	$9, 80(%rsp)
	movq	%rdi, %rax
	andq	%rcx, %rsi
	leaq	(%r9,%r9), %r12
	addq	%rdx, %rax
	movq	%rax, %rdi
	shrq	$51, %rax
	leaq	(%rsi,%rax), %rsi
	leaq	(%rbx,%rbx,8), %rax
	andq	%rcx, %rdi
	leaq	(%rbx,%rax,2), %rax
	movq	%rsi, %r14
	leaq	(%rsi,%rsi), %rbx
	movq	%rdi, %r13
	leaq	(%rax,%rax), %r8
	movq	%rax, 96(%rsp)
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r15
	movq	%rdi, %rax
	movq	%rdi, 128(%rsp)
	mulq	%r8
	movq	%r15, 144(%rsp)
	movq	%rax, %rsi
	movq	%r11, %rax
	movq	%rdx, %rdi
	mulq	%r11
	movq	%rax, %r10
	movq	%rdx, %r11
	leaq	(%r15,%r15), %rax
	addq	%rsi, %r10
	movq	%r14, %rsi
	adcq	%rdi, %r11
	mulq	%r14
	leaq	(%r13,%r13), %rdi
	movq	%rsi, 120(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	%rbp
	movq	%rax, %r14
	movq	%rsi, %rax
	movq	%rdx, %r15
	mulq	%r8
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r9, %rax
	addq	%r14, %rsi
	adcq	%r15, %rdi
	mulq	144(%rsp)
	addq	%rax, %rsi
	movq	%r10, %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r11, %rax
	movq	%r11, %rdx
	shrq	$51, %rdx
	addq	%rax, %rsi
	movq	%r8, %rax
	adcq	%rdx, %rdi
	mulq	%r9
	movq	%rsi, %r14
	movq	%rbp, %rsi
	movq	%rdi, 24(%rsp)
	movq	%r14, 16(%rsp)
	movq	%rax, %r8
	movq	%r13, %rax
	movq	%rdx, %r9
	mulq	%r13
	addq	%rax, %r8
	movq	%rbp, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	shrdq	$51, %rdi, %rax
	movq	%rdi, %rdx
	movq	%r8, %rdi
	shrq	$51, %rdx
	movq	%r9, %rbp
	addq	%rax, %rdi
	movq	168(%rsp), %rax
	adcq	%rdx, %rbp
	mulq	96(%rsp)
	movq	%rax, %r14
	movq	%rsi, %rax
	movq	%rdx, %r15
	mulq	%r12
	addq	%rax, %r14
	movq	%rbx, %rax
	movq	168(%rsp), %rbx
	adcq	%rdx, %r15
	mulq	%r13
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	movq	%rbp, %rdx
	addq	%r14, %r8
	adcq	%r15, %r9
	shrdq	$51, %rbp, %rax
	movabsq	$2251799813685247, %rbp
	shrq	$51, %rdx
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	andq	%r8, %rax
	addq	%rbx, %rbx
	movq	%rax, (%rsp)
	movq	%rbx, %rax
	mulq	%rsi
	movq	120(%rsp), %rsi
	movq	%rax, %r14
	movq	%r12, %rax
	movq	%rdx, %r15
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%rsi, %rax
	addq	%r14, %r12
	adcq	%r15, %r13
	mulq	%rsi
	movq	%rcx, %r15
	movq	16(%rsp), %rsi
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	andq	%rax, %r15
	andq	%rcx, %r10
	andq	%rcx, %rsi
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rax
	addq	%r10, %rax
	movq	%rax, %r14
	shrq	$51, %rax
	addq	%rsi, %rax
	andq	%rcx, %r14
	movq	%rax, %rsi
	shrq	$51, %rax
	andq	%rcx, %rsi
	andq	%rdi, %rcx
	movq	(%rsp), %rdi
	leaq	(%rcx,%rax), %r11
	.p2align 4,,10
	.p2align 3
.L342:
	leaq	(%r15,%r15,8), %rax
	leaq	(%rdi,%rdi), %rbx
	leaq	(%r15,%rax,2), %rax
	movq	%rbx, (%rsp)
	leaq	(%r11,%r11), %rcx
	leaq	(%rax,%rax), %r10
	movq	%rax, 48(%rsp)
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rbx
	leaq	(%rbx,%rbx), %r9
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, %r8
	movq	%r10, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	%rbx, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	movq	%r8, %r12
	movq	%r9, %r13
	movq	%r12, 16(%rsp)
	movq	%r13, 24(%rsp)
	movq	%rax, %r8
	movq	%r10, %rax
	movq	%rdx, %r9
	mulq	%r11
	addq	%rax, %r8
	leaq	(%rsi,%rsi), %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	movq	%r12, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r8
	movq	%r13, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%r10
	movq	%r12, 32(%rsp)
	movq	%r13, %r10
	movq	%r13, 40(%rsp)
	movq	%rax, %r8
	movq	%rcx, %rax
	movq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%r12, %r9
	shrq	$51, %r10
	shrdq	$51, %r13, %r9
	movq	%r10, %r13
	movq	(%rsp), %r10
	movq	%r9, %r12
	addq	%rax, %r12
	movq	48(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%rax, %r8
	movq	%rcx, %rax
	movq	%rdx, %r9
	mulq	%rsi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r10, %rax
	addq	%r8, %rcx
	leaq	(%r15,%r15), %r8
	adcq	%r9, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	movq	%rcx, %rdi
	andq	%rbp, %rdi
	movq	%rax, %r8
	movq	%r10, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r11, %rax
	adcq	%rdx, %r9
	mulq	%r11
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r15
	andq	%rbp, %r12
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r15
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	16(%rsp), %rax
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	32(%rsp), %rax
	movq	%rdx, %r14
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %r14
	addq	%rdx, %rax
	movq	%rax, %rsi
	shrq	$51, %rax
	andq	%rbp, %rsi
	subl	$1, 80(%rsp)
	leaq	(%r12,%rax), %r11
	jne	.L342
	movq	120(%rsp), %r13
	movq	128(%rsp), %r10
	movq	%r11, 16(%rsp)
	leaq	0(%r13,%r13,8), %rax
	leaq	0(%r13,%rax,2), %r12
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %rax
	movq	%r12, 208(%rsp)
	movq	%rax, 216(%rsp)
	mulq	%r15
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	64(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	96(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	144(%rsp), %rax
	adcq	%rdx, %rbx
	movq	%rcx, %r8
	mulq	%r11
	movq	%rbx, %r9
	addq	%rax, %r8
	movq	%r12, %rax
	movq	%rdi, %r12
	adcq	%rdx, %r9
	mulq	%r15
	movq	%r12, (%rsp)
	movq	%rax, %rcx
	movq	144(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rdi
	movq	%r10, %rdi
	addq	%rax, %rcx
	movq	%r10, %rax
	movq	%r8, %r10
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	64(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	96(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r11
	movq	%r9, %r11
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r9, %r10
	shrq	$51, %r11
	addq	%rax, %r10
	movq	144(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	%rax, %rcx
	movq	96(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r12
	movq	%r10, %r12
	addq	%rax, %rcx
	movq	%r13, %rax
	movq	%r11, %r13
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	64(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	16(%rsp)
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	96(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	64(%rsp), %rax
	mulq	(%rsp)
	addq	%rax, %rcx
	movq	152(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	16(%rsp)
	addq	%rcx, %rax
	movq	%r12, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %rcx
	movq	%r13, %rbx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, 32(%rsp)
	movq	%rax, %rdi
	movq	%r15, %rax
	movq	%rdx, 40(%rsp)
	mulq	64(%rsp)
	andq	%rbp, %rdi
	movq	%rdi, 184(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	(%rsp), %rax
	mulq	128(%rsp)
	addq	%rax, %rcx
	movq	%r14, %rax
	adcq	%rdx, %rbx
	mulq	168(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	%rsi, %rax
	addq	%rcx, %r14
	adcq	%rbx, %r15
	mulq	152(%rsp)
	addq	%rax, %r14
	movq	16(%rsp), %rax
	adcq	%rdx, %r15
	mulq	120(%rsp)
	movq	32(%rsp), %rsi
	movl	$19, 48(%rsp)
	movq	40(%rsp), %rdi
	addq	%r14, %rax
	adcq	%r15, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	movq	%rax, %rcx
	andq	%rbp, %r8
	shrdq	$51, %rdx, %rax
	andq	%rbp, %rcx
	leaq	(%rax,%rax,8), %rdx
	movq	%rcx, 192(%rsp)
	leaq	(%rax,%rdx,2), %rdx
	movq	%r10, %rax
	addq	%r8, %rdx
	andq	%rbp, %rax
	movq	184(%rsp), %r8
	movq	%rdx, %rbx
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	%r12, %rdx
	leaq	(%r8,%r8), %rsi
	andq	%rbp, %rbx
	andq	%rbp, %rdx
	movq	%rax, %r9
	shrq	$51, %rax
	movq	%rbx, 160(%rsp)
	leaq	(%rdx,%rax), %r14
	leaq	(%rcx,%rcx,8), %rax
	andq	%rbp, %r9
	leaq	(%rcx,%rax,2), %r11
	leaq	(%r8,%r8,8), %rax
	movq	%r9, %r10
	leaq	(%r8,%rax,2), %r15
	leaq	(%r11,%r11), %rcx
	movq	%r9, %rax
	movq	%rbx, %r9
	mulq	%rcx
	leaq	(%r14,%r14), %rdi
	movq	%r15, 176(%rsp)
	movq	%r10, 200(%rsp)
	movq	%r11, 256(%rsp)
	movq	%rax, %r12
	movq	%rbx, %rax
	movq	%rdx, %r13
	mulq	%rbx
	movq	%r14, %rbx
	movq	%rbx, 80(%rsp)
	addq	%rax, %r12
	leaq	(%r15,%r15), %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%r12, %r14
	movq	%r13, %r15
	leaq	(%r10,%r10), %r13
	addq	%rax, %r14
	movq	%r13, %rax
	adcq	%rdx, %r15
	mulq	%r9
	movq	%rax, %r12
	movq	%rbx, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	176(%rsp)
	addq	%r12, %rax
	movq	%r14, %r12
	adcq	%r13, %rdx
	movq	%r15, %r13
	shrdq	$51, %r15, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rcx, %rax
	adcq	%rdx, %r13
	mulq	%r8
	movq	%rax, %rcx
	movq	%r10, %rax
	movq	%rdx, %rbx
	mulq	%r10
	movq	%r9, %r10
	addq	%rax, %rcx
	movq	%r9, %rax
	adcq	%rdx, %rbx
	mulq	%rdi
	addq	%rcx, %rax
	movq	%r12, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %rcx
	movq	%r13, %rbx
	shrq	$51, %rbx
	addq	%rcx, %rax
	movq	%rax, (%rsp)
	movq	192(%rsp), %rax
	adcq	%rbx, %rdx
	movq	%rdx, 8(%rsp)
	mulq	%r11
	movq	%rax, %r8
	movq	%r10, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	200(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	%r8, %rcx
	adcq	%r9, %rbx
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	movq	192(%rsp), %rdx
	movq	%rcx, %rdi
	andq	%rbp, %rdi
	leaq	(%rdx,%rdx), %r9
	movq	%r9, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	movq	80(%rsp), %rsi
	mulq	200(%rsp)
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r10
	andq	%rbp, %r14
	movq	%r12, %rbx
	shrdq	$51, %rdx, %rax
	andq	%rbp, %rbx
	andq	%rbp, %r10
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	%rbx, %rax
	addq	%r14, %rdx
	movq	%rdx, %r15
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	andq	%rbp, %r15
	movq	%rax, %r14
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %r14
	movabsq	$2251799813685247, %rbp
	addq	%rdx, %rax
	movq	%rax, %rcx
	.p2align 4,,10
	.p2align 3
.L343:
	leaq	(%r10,%r10,8), %rax
	leaq	(%rdi,%rdi), %rbx
	leaq	(%r10,%rax,2), %r11
	leaq	(%rcx,%rcx), %rax
	movq	%rax, (%rsp)
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%r11,%r11), %rsi
	leaq	(%rdi,%rax,2), %r8
	leaq	(%r8,%r8), %r9
	movq	%r9, %rax
	mulq	%rcx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%r15, %rax
	adcq	%rdx, %r13
	mulq	%r15
	addq	%rax, %r12
	movq	%r8, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%r12, 16(%rsp)
	movq	%r13, 24(%rsp)
	movq	%rax, %r8
	movq	%rsi, %rax
	movq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	leaq	(%r14,%r14), %rax
	adcq	%rdx, %r9
	mulq	%r15
	addq	%r8, %rax
	movq	%r12, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r8
	movq	%r13, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r8
	movq	%rdi, %rax
	movq	%rdx, %r9
	mulq	%rsi
	movq	%r8, 32(%rsp)
	movq	%r9, 40(%rsp)
	movq	%rax, %rsi
	movq	(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r15
	addq	%rax, %rsi
	movq	%r14, %rax
	adcq	%rdx, %rdi
	mulq	%r14
	addq	%rsi, %rax
	movq	%r8, %rsi
	adcq	%rdi, %rdx
	shrdq	$51, %r9, %rsi
	movq	%r9, %rdi
	shrq	$51, %rdi
	movq	%rsi, %r12
	addq	%rax, %r12
	movq	%rdi, %r13
	movq	%r11, %rax
	adcq	%rdx, %r13
	mulq	%r10
	movq	%rax, %rsi
	movq	(%rsp), %rax
	movq	%rdx, %rdi
	mulq	%r14
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rbx, %rax
	addq	%rsi, %r8
	adcq	%rdi, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %r8
	adcq	%rdx, %r9
	addq	%r10, %r10
	movq	%r8, %rdi
	movq	%r10, %rax
	andq	%rbp, %rdi
	mulq	%r15
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rbx
	addq	%rax, %r10
	movq	%rcx, %rax
	adcq	%rdx, %r11
	mulq	%rcx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	movq	%r9, %rdx
	addq	%r10, %rcx
	adcq	%r11, %rbx
	shrdq	$51, %r9, %rax
	shrq	$51, %rdx
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	movq	%rcx, %r10
	andq	%rbp, %r12
	shrdq	$51, %rbx, %rcx
	andq	%rbp, %r10
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	movq	16(%rsp), %rax
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	32(%rsp), %rax
	movq	%rdx, %r15
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %r15
	addq	%rdx, %rax
	movq	%rax, %r14
	shrq	$51, %rax
	andq	%rbp, %r14
	subl	$1, 48(%rsp)
	leaq	(%r12,%rax), %rcx
	jne	.L343
	movq	80(%rsp), %rbx
	movq	200(%rsp), %r9
	movq	%rcx, %r8
	movq	256(%rsp), %r11
	movq	%r8, (%rsp)
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rsi
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %rcx
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, %rcx
	movq	%rsi, %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	160(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%r11, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	176(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r8
	addq	%rax, %rcx
	movq	%rsi, %rax
	movq	%r9, %rsi
	adcq	%rdx, %rbx
	mulq	%r10
	movq	%rcx, %r12
	movq	%rbx, %r13
	movq	%rax, %rcx
	movq	176(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	%r9, %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	160(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	mulq	%r11
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	176(%rsp), %rax
	adcq	%rdx, %rbx
	movq	%rcx, %r8
	mulq	%r10
	movq	%rbx, %r9
	movq	%r8, 16(%rsp)
	movq	%rax, %rcx
	movq	%r11, %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	80(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	160(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	(%rsp)
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r9, %rax
	movq	%r9, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r11, %rax
	movq	%rsi, %r11
	adcq	%rdx, %rbx
	mulq	%r10
	movq	%rcx, %r8
	movq	%rbx, %r9
	movq	%r8, 32(%rsp)
	movq	%rax, %rcx
	movq	160(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	184(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	80(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	(%rsp)
	addq	%rax, %rcx
	movq	160(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%rcx, %r8
	adcq	%rbx, %r9
	mulq	%r10
	movq	%r8, %rcx
	andq	%rbp, %rcx
	movq	%rcx, %rsi
	movq	%rax, %rcx
	movq	%r11, %rax
	movq	%rdx, %rbx
	mulq	%rdi
	addq	%rax, %rcx
	movq	192(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	184(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	80(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	(%rsp)
	addq	%rcx, %rax
	movq	%r8, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r9, %rcx
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r9
	leaq	(%rsi,%rsi), %rbx
	andq	%rbp, %r12
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	movq	%rbx, (%rsp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	16(%rsp), %rax
	addq	%r12, %rdx
	movq	%rdx, %rdi
	andq	%rbp, %rax
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	32(%rsp), %rdx
	andq	%rbp, %rdi
	movq	%rax, %rcx
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %rcx
	leaq	(%rdx,%rax), %r8
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r14
	leaq	(%r8,%r8), %rax
	leaq	(%r14,%r14), %r15
	movq	%rax, 16(%rsp)
	movq	%r15, %rax
	mulq	%rcx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rbx
	adcq	%rdx, %r13
	movq	%r12, %r10
	leaq	(%rbx,%rbx), %rax
	movq	%r13, %r11
	leaq	(%rcx,%rcx), %r13
	mulq	%r8
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, 32(%rsp)
	movq	%r11, 40(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	16(%rsp), %rsi
	movq	%rdx, %r13
	mulq	%r15
	movq	%r12, 16(%rsp)
	movq	%r13, 24(%rsp)
	movq	(%rsp), %rbx
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%rdx, %r15
	mulq	%rdi
	addq	%rax, %r14
	movq	%rsi, %rax
	leaq	(%r9,%r9), %rsi
	adcq	%rdx, %r15
	mulq	%rcx
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r12, %rax
	movq	%r13, %rdx
	addq	%r14, %r10
	adcq	%r15, %r11
	shrdq	$51, %r13, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, %r15
	andq	%rbp, %r15
	movq	%rax, %rsi
	movq	%rbx, %rax
	movq	%rdx, %rdi
	mulq	%rcx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%rsi, %rcx
	movq	%r11, %rsi
	adcq	%rdi, %rbx
	mulq	%r8
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%r10, %rbx
	shrq	$51, %rsi
	shrdq	$51, %r11, %rbx
	addq	%rbx, %rax
	adcq	%rsi, %rdx
	movq	%rax, %rcx
	shrdq	$51, %rdx, %rax
	andq	%rbp, %rcx
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	movl	$9, 32(%rsp)
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %rsi
	addq	%rdx, %rax
	movq	%r12, %rdx
	movq	%rax, %r14
	andq	%rbp, %rdx
	shrq	$51, %rax
	andq	%rbp, %r14
	leaq	(%rdx,%rax), %rbx
	.p2align 4,,10
	.p2align 3
.L344:
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rbx,%rbx), %r13
	leaq	(%rcx,%rax,2), %rbp
	leaq	(%r15,%r15), %rax
	movq	%rax, 16(%rsp)
	leaq	(%r15,%r15,8), %rax
	leaq	(%rbp,%rbp), %rdi
	leaq	(%r15,%rax,2), %r8
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	%rbx
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	%rax, %r8
	movq	%rdi, %rax
	movq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	leaq	(%r14,%r14), %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	movq	%r11, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, (%rsp)
	movq	%r15, %rax
	movq	(%rsp), %r12
	movq	%rdx, 8(%rsp)
	mulq	%rdi
	movq	%r13, %r15
	movq	%rax, %r8
	movq	%r13, %rax
	movq	%rdx, %r9
	movq	8(%rsp), %r13
	mulq	%rsi
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rbp, %rax
	adcq	%rdx, %r13
	mulq	%rcx
	movq	%rax, %rdi
	movq	%r15, %rax
	movq	%rdx, %rbp
	movabsq	$2251799813685247, %r15
	mulq	%r14
	movq	%rax, %r8
	movq	%rdx, %r9
	addq	%rdi, %r8
	leaq	(%rcx,%rcx), %rdi
	movabsq	$2251799813685247, %rcx
	adcq	%rbp, %r9
	movq	16(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rsi
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rsi
	andq	%r8, %r15
	movq	%rax, %rsi
	movq	%rbp, %rax
	movq	%rdx, %rdi
	mulq	%r14
	movabsq	$2251799813685247, %r14
	addq	%rax, %rsi
	movq	%rbx, %rax
	adcq	%rdx, %rdi
	mulq	%rbx
	movabsq	$2251799813685247, %rbx
	addq	%rsi, %rax
	movabsq	$2251799813685247, %rsi
	adcq	%rdi, %rdx
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%rax, %r8
	adcq	%rdx, %r9
	andq	%r8, %rcx
	andq	%rbx, %r12
	shrdq	$51, %r9, %r8
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rdx
	movabsq	$2251799813685247, %rax
	andq	%r10, %rax
	addq	%rax, %rdx
	movabsq	$2251799813685247, %rax
	andq	(%rsp), %rax
	andq	%rdx, %rsi
	shrq	$51, %rdx
	addq	%rdx, %rax
	andq	%rax, %r14
	shrq	$51, %rax
	subl	$1, 32(%rsp)
	leaq	(%r12,%rax), %rbx
	jne	.L344
	movq	%rcx, %rdi
	movq	%rbx, %rbp
	movq	64(%rsp), %r9
	movq	96(%rsp), %r13
	movq	216(%rsp), %rax
	movq	208(%rsp), %r12
	movq	%rsi, 16(%rsp)
	movq	144(%rsp), %r8
	movq	%rdi, (%rsp)
	mulq	%rcx
	movq	%rbp, 32(%rsp)
	movq	%rax, %rcx
	movq	%r12, %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%r9, %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r13, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	mulq	%rdi
	movq	%rcx, %r10
	movq	%rbx, %r11
	movq	%r10, %r12
	movq	%rax, %rcx
	movq	%r8, %rax
	movq	%rdx, %rbx
	movq	128(%rsp), %r8
	mulq	%r15
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r9, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r13, %rax
	movq	%r11, %r13
	adcq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	144(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rcx, %r12
	adcq	%rbx, %r13
	mulq	%rdi
	movq	%rax, %rcx
	movq	96(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r8, %rax
	movq	%r12, %r8
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r9, %rax
	movq	%r13, %r9
	adcq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	96(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %r8
	shrq	$51, %r9
	addq	%rcx, %r8
	adcq	%rbx, %r9
	mulq	%rdi
	movq	%r9, %rdi
	movq	%rax, %rcx
	movq	64(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	152(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	movq	%r8, %rsi
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	128(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%rbp
	movabsq	$2251799813685247, %rbp
	addq	%rax, %rcx
	movq	64(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r9, %rsi
	shrq	$51, %rdi
	addq	%rcx, %rsi
	adcq	%rbx, %rdi
	mulq	(%rsp)
	andq	%rsi, %rbp
	leaq	(%rbp,%rbp), %r11
	movq	%rbp, 176(%rsp)
	movq	%rax, %rcx
	movq	128(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	168(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	16(%rsp)
	addq	%rax, %rcx
	movq	152(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	32(%rsp)
	addq	%rcx, %rax
	movabsq	$2251799813685247, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	movabsq	$2251799813685247, %rsi
	adcq	%rdi, %rdx
	andq	%rax, %rcx
	andq	%rsi, %r10
	movq	%rsi, %rdi
	shrdq	$51, %rdx, %rax
	andq	%rsi, %r12
	movq	%rsi, %rbx
	andq	%rsi, %r8
	leaq	(%rax,%rax,8), %rdx
	movq	%rcx, 168(%rsp)
	leaq	(%rax,%rdx,2), %rax
	addq	%r10, %rax
	movq	%rbp, %r10
	andq	%rax, %rdi
	shrq	$51, %rax
	addq	%r12, %rax
	movq	%rdi, 80(%rsp)
	andq	%rax, %rbx
	shrq	$51, %rax
	leaq	(%r8,%rax), %rsi
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rax
	movq	%rsi, %r14
	leaq	(%rsi,%rsi), %rsi
	leaq	(%rax,%rax), %r12
	movq	%rax, 96(%rsp)
	leaq	0(%rbp,%rbp,8), %rax
	leaq	0(%rbp,%rax,2), %r15
	movq	%rbx, %rax
	movq	%rbx, %rbp
	mulq	%r12
	movq	%r15, %r13
	movq	%r15, 144(%rsp)
	movq	%rax, %rcx
	movq	%rdi, %rax
	movq	%rdx, %rbx
	mulq	%rdi
	movq	%rax, %r8
	movq	%rdx, %r9
	leaq	(%r15,%r15), %rax
	addq	%rcx, %r8
	adcq	%rbx, %r9
	mulq	%r14
	movq	%r14, %rbx
	addq	%rax, %r8
	adcq	%rdx, %r9
	movq	%r8, 16(%rsp)
	movq	%rdi, %r8
	movq	%r9, 24(%rsp)
	leaq	(%rbp,%rbp), %r9
	movq	%r9, %rax
	movq	%rbx, 128(%rsp)
	mulq	%rdi
	movq	%rbp, 120(%rsp)
	movl	$49, 48(%rsp)
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%rdx, %r15
	mulq	%r12
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r10, %rax
	addq	%r14, %rcx
	adcq	%r15, %rbx
	mulq	%r13
	addq	%rax, %rcx
	movq	16(%rsp), %rax
	adcq	%rdx, %rbx
	movq	24(%rsp), %rdx
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	mulq	%r10
	movq	%rax, %r12
	movq	%rbp, %rax
	movq	%rdx, %r13
	mulq	%rbp
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%rax, %r12
	movq	%rcx, %rax
	adcq	%rdx, %r13
	movq	%r12, %r9
	shrdq	$51, %rbx, %rax
	movq	%rbx, %rdx
	movq	%r13, %r10
	movq	168(%rsp), %r13
	shrq	$51, %rdx
	addq	%rax, %r9
	adcq	%rdx, %r10
	movq	%r13, %rax
	mulq	96(%rsp)
	movq	%rax, %r14
	movq	%rdi, %rax
	movq	%rdx, %r15
	mulq	%r11
	addq	%rax, %r14
	movq	%rsi, %rax
	adcq	%rdx, %r15
	mulq	%rbp
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%r9, %rax
	movq	%r10, %rdx
	addq	%r14, %rsi
	leaq	(%r13,%r13), %r14
	adcq	%r15, %rdi
	shrdq	$51, %r10, %rax
	shrq	$51, %rdx
	addq	%rax, %rsi
	movabsq	$2251799813685247, %rax
	adcq	%rdx, %rdi
	andq	%rsi, %rax
	movq	%rax, %rbp
	movq	%r14, %rax
	mulq	%r8
	movq	16(%rsp), %r8
	movq	%rax, %r12
	movq	%rdx, %r13
	movq	%r11, %rax
	movq	128(%rsp), %r11
	mulq	120(%rsp)
	addq	%rax, %r12
	movq	%r11, %rax
	adcq	%rdx, %r13
	mulq	%r11
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	movabsq	$2251799813685247, %rsi
	adcq	%rdi, %rdx
	andq	%rsi, %r8
	movq	%rsi, %r15
	andq	%rsi, %rcx
	movabsq	$2251799813685247, %rdi
	movq	%rsi, %r14
	andq	%rax, %rdi
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	movq	%rdi, (%rsp)
	leaq	(%rax,%rdx,2), %rax
	movq	%rsi, %rdx
	addq	%r8, %rax
	andq	%r9, %rdx
	andq	%rax, %r15
	shrq	$51, %rax
	addq	%rcx, %rax
	andq	%rax, %r14
	shrq	$51, %rax
	leaq	(%rdx,%rax), %rcx
	.p2align 4,,10
	.p2align 3
.L345:
	movq	(%rsp), %r9
	leaq	(%rcx,%rcx), %rdi
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %rbx
	leaq	(%rbp,%rbp), %rax
	movq	%rax, 16(%rsp)
	leaq	0(%rbp,%rbp,8), %rax
	leaq	(%rbx,%rbx), %rsi
	leaq	0(%rbp,%rax,2), %r8
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	%rcx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%r15, %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%r12, %r11
	movq	%r13, %r12
	addq	%rax, %r11
	movq	%r8, %rax
	adcq	%rdx, %r12
	mulq	%rbp
	movq	%r11, 32(%rsp)
	movq	%r12, %r13
	movq	%r12, 40(%rsp)
	movq	%r11, %r12
	movq	%rax, %r8
	movq	%rsi, %rax
	movq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	leaq	(%r14,%r14), %rax
	adcq	%rdx, %r9
	mulq	%r15
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rbp, %rax
	movabsq	$2251799813685247, %rbp
	adcq	%rdx, %r13
	mulq	%rsi
	movq	%r12, %r10
	movq	%r13, %r11
	movq	%rax, %r8
	movq	%rdi, %rax
	movq	%rdx, %r9
	mulq	%r15
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	(%rsp), %r9
	shrdq	$51, %r13, %r10
	shrq	$51, %r11
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%r9
	leaq	(%r9,%r9), %r8
	movq	%rax, %rbx
	movq	%rdi, %rax
	movq	%rdx, %rsi
	mulq	%r14
	addq	%rax, %rbx
	adcq	%rdx, %rsi
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	movq	16(%rsp), %rbx
	movq	%rbx, %rax
	mulq	%r15
	addq	%rax, %rsi
	movq	%r10, %rax
	adcq	%rdx, %rdi
	shrdq	$51, %r11, %rax
	movq	%r11, %rdx
	shrq	$51, %rdx
	addq	%rax, %rsi
	movq	%r8, %rax
	adcq	%rdx, %rdi
	mulq	%r15
	andq	%rsi, %rbp
	movabsq	$2251799813685247, %r15
	movq	%rax, %r8
	movq	%rbx, %rax
	movq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movabsq	$2251799813685247, %rax
	movabsq	$2251799813685247, %rdx
	addq	%r8, %rcx
	adcq	%r9, %rbx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	andq	%rcx, %rax
	andq	32(%rsp), %rdx
	movabsq	$2251799813685247, %rdi
	shrdq	$51, %rbx, %rcx
	movq	%rax, (%rsp)
	andq	%rdi, %r12
	movq	%rdi, %r14
	leaq	(%rcx,%rcx,8), %rax
	andq	%rdi, %r10
	leaq	(%rcx,%rax,2), %rax
	addq	%rdx, %rax
	andq	%rax, %r15
	shrq	$51, %rax
	addq	%rax, %r12
	andq	%r12, %r14
	shrq	$51, %r12
	subl	$1, 48(%rsp)
	leaq	(%r10,%r12), %rcx
	jne	.L345
	movq	128(%rsp), %rsi
	movq	120(%rsp), %rdi
	movq	%rcx, %r11
	movq	(%rsp), %r12
	movq	80(%rsp), %r10
	leaq	(%rsi,%rsi,8), %rax
	movq	96(%rsp), %r13
	leaq	(%rsi,%rax,2), %r9
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rbx
	movq	%r12, %rax
	movq	%r9, 192(%rsp)
	mulq	%rbx
	movq	%rbx, 256(%rsp)
	movq	%rax, %rcx
	movq	%r9, %rax
	movq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%r13, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	144(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r11
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	mulq	192(%rsp)
	movq	%rcx, %r8
	movq	%rbx, %r9
	movq	%rax, %rcx
	movq	144(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%r10, %rax
	movq	%r8, %r10
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r13, %rax
	movq	%r11, %r13
	adcq	%rdx, %rbx
	mulq	%r11
	movq	%r9, %r11
	movq	%r13, 16(%rsp)
	addq	%rax, %rcx
	movq	144(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r9, %r10
	shrq	$51, %r11
	addq	%rcx, %r10
	adcq	%rbx, %r11
	mulq	%r12
	movq	%r10, %r12
	movq	%rax, %rcx
	movq	96(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	80(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r13
	movq	%r11, %r13
	addq	%rax, %rcx
	movq	96(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rcx, %r12
	adcq	%rbx, %r13
	mulq	(%rsp)
	movq	%rax, %rcx
	movq	80(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%rbp
	addq	%rax, %rcx
	movq	176(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%rsi, %rax
	movq	%r12, %rsi
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%rdi, %rax
	movq	%r13, %rdi
	adcq	%rdx, %rbx
	mulq	16(%rsp)
	addq	%rax, %rcx
	movabsq	$2251799813685247, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rsi
	shrq	$51, %rdi
	addq	%rcx, %rsi
	adcq	%rbx, %rdi
	andq	%rsi, %rax
	movq	%rax, 184(%rsp)
	movq	(%rsp), %rax
	mulq	80(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rbp, %rax
	mulq	120(%rsp)
	addq	%rax, %rcx
	movq	%r15, %rax
	adcq	%rdx, %rbx
	mulq	168(%rsp)
	addq	%rax, %rcx
	movq	%r14, %rax
	adcq	%rdx, %rbx
	mulq	176(%rsp)
	movq	%rax, %r14
	movq	%rdx, %r15
	movq	16(%rsp), %rax
	addq	%rcx, %r14
	movabsq	$2251799813685247, %rcx
	adcq	%rbx, %r15
	mulq	128(%rsp)
	movabsq	$2251799813685247, %rbx
	addq	%r14, %rax
	adcq	%r15, %rdx
	shrdq	$51, %rdi, %rsi
	shrq	$51, %rdi
	addq	%rsi, %rax
	adcq	%rdi, %rdx
	andq	%rax, %rcx
	andq	%rbx, %r8
	movabsq	$2251799813685247, %rdi
	shrdq	$51, %rdx, %rax
	movq	%rcx, 208(%rsp)
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movabsq	$2251799813685247, %rax
	addq	%r8, %rdx
	andq	%r10, %rax
	andq	%rdx, %rbx
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	%rbx, %r14
	movq	%rbx, 152(%rsp)
	movabsq	$2251799813685247, %rdx
	andq	%rax, %rdi
	movq	184(%rsp), %rbx
	andq	%r12, %rdx
	shrq	$51, %rax
	leaq	(%rdx,%rax), %rsi
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	leaq	(%rbx,%rbx,8), %rax
	movq	%rsi, %r15
	leaq	(%rbx,%rax,2), %rbp
	movq	%r14, %rax
	leaq	(%rdx,%rdx), %rcx
	movq	%rdx, 160(%rsp)
	mulq	%r14
	movq	%rbp, 200(%rsp)
	leaq	(%rbx,%rbx), %r8
	movq	%r15, 216(%rsp)
	leaq	(%rsi,%rsi), %rsi
	movl	$99, 64(%rsp)
	movq	%rax, %r12
	movq	%rcx, %rax
	movq	%rdx, %r13
	movq	%rdi, 264(%rsp)
	mulq	%rdi
	addq	%rax, %r12
	leaq	(%rbp,%rbp), %rax
	adcq	%rdx, %r13
	mulq	%r15
	movq	%r12, %r9
	movq	%r13, %r10
	leaq	(%rdi,%rdi), %r13
	addq	%rax, %r9
	movq	%r13, %rax
	adcq	%rdx, %r10
	mulq	%r14
	movq	%r9, 16(%rsp)
	movq	%r10, 24(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%rbp
	movq	%r14, %rbp
	addq	%r12, %rax
	movq	%r9, %r12
	movq	208(%rsp), %r9
	adcq	%r13, %rdx
	movq	%r10, %r13
	shrdq	$51, %r10, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rdi, %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%rbx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r14, %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	mulq	%rsi
	addq	%rcx, %rax
	movq	%r12, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %rcx
	movq	%r13, %rbx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r14
	movq	%r9, %rax
	movq	%rdx, %r15
	mulq	160(%rsp)
	movq	%rax, %r10
	movq	%rbp, %rax
	movq	%rdx, %r11
	mulq	%r8
	addq	%rax, %r10
	movq	%rsi, %rax
	movq	216(%rsp), %rsi
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r14, %rax
	movq	%r15, %rdx
	addq	%r10, %rcx
	adcq	%r11, %rbx
	shrdq	$51, %r15, %rax
	leaq	(%r9,%r9), %r11
	shrq	$51, %rdx
	addq	%rax, %rcx
	movabsq	$2251799813685247, %rax
	adcq	%rdx, %rbx
	andq	%rcx, %rax
	movq	%rax, (%rsp)
	movq	%r11, %rax
	movq	(%rsp), %r15
	mulq	%rbp
	movabsq	$2251799813685247, %rbp
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rsi, %rax
	addq	%r10, %r8
	adcq	%r11, %r9
	mulq	%rsi
	movabsq	$2251799813685247, %r11
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	movabsq	$2251799813685247, %rcx
	adcq	%rbx, %rdx
	andq	%rax, %r11
	movabsq	$2251799813685247, %rbx
	shrdq	$51, %rdx, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movabsq	$2251799813685247, %rax
	andq	16(%rsp), %rax
	addq	%rax, %rdx
	movabsq	$2251799813685247, %rax
	andq	%rdx, %rbx
	andq	%r12, %rax
	shrq	$51, %rdx
	addq	%rdx, %rax
	movabsq	$2251799813685247, %rdx
	andq	%r14, %rdx
	andq	%rax, %rcx
	shrq	$51, %rax
	movq	%r11, %r14
	leaq	(%rdx,%rax), %rsi
	.p2align 4,,10
	.p2align 3
.L346:
	leaq	(%r14,%r14,8), %rax
	leaq	(%r15,%r15), %rdx
	leaq	(%r14,%rax,2), %rax
	movq	%rdx, (%rsp)
	leaq	(%rsi,%rsi), %r9
	leaq	(%rax,%rax), %rdi
	movq	%rax, 48(%rsp)
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r8
	movq	%r9, 16(%rsp)
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	%rsi
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%rbx
	movq	%r12, %r11
	movq	%r13, %r12
	addq	%rax, %r11
	movq	%r8, %rax
	adcq	%rdx, %r12
	mulq	%r15
	movq	%r11, 32(%rsp)
	movq	%r12, 40(%rsp)
	movq	%rax, %r8
	movq	%rdi, %rax
	movq	%rdx, %r9
	mulq	%rsi
	addq	%rax, %r8
	leaq	(%rcx,%rcx), %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%r8, %rax
	movq	%r11, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r12, %r8
	movq	%r12, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r10
	movq	%r15, %rax
	movq	16(%rsp), %r15
	movq	%rdx, %r11
	mulq	%rdi
	movq	%r10, %r12
	movq	%r10, 16(%rsp)
	movq	%r11, %r13
	movq	%r11, 24(%rsp)
	movq	(%rsp), %rdi
	movq	%rax, %r8
	movq	%r15, %rax
	movq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%rcx, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	48(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%r14
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rdx, %r11
	mulq	%rcx
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	%rdi, %rax
	addq	%r10, %r8
	leaq	(%r14,%r14), %r10
	adcq	%r11, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %r8
	movq	%r10, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	movq	%r8, %r15
	andq	%rbp, %r15
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rdx, %rdi
	addq	%r10, %rsi
	adcq	%r11, %rdi
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%r8, %rsi
	adcq	%r9, %rdi
	movq	%rsi, %r14
	andq	%rbp, %r12
	shrdq	$51, %rdi, %rsi
	andq	%rbp, %r14
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rdx
	movq	32(%rsp), %rax
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rbx
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %rbx
	addq	%rdx, %rax
	movq	%rax, %rcx
	shrq	$51, %rax
	andq	%rbp, %rcx
	subl	$1, 64(%rsp)
	leaq	(%r12,%rax), %rsi
	jne	.L346
	movq	%r15, %r9
	movq	216(%rsp), %r15
	movq	264(%rsp), %rdi
	movq	%r14, %r13
	movq	152(%rsp), %r12
	movq	%r9, (%rsp)
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r8
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r11
	movq	%r11, %rax
	mulq	%r14
	movq	%rsi, %r14
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	mulq	%r9
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	%rbx
	addq	%rax, %r10
	movq	160(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	movq	200(%rsp), %rax
	adcq	%rdx, %r11
	mulq	%rsi
	movq	%r13, %rsi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r13
	movq	%r11, %r13
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	200(%rsp), %rax
	mulq	(%rsp)
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r12, %rax
	movq	%r10, %r12
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	160(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	200(%rsp), %rax
	adcq	%rdx, %r13
	movq	%r12, 16(%rsp)
	movq	%r15, %r12
	mulq	%rsi
	movq	%r13, 24(%rsp)
	movq	%rsi, %r13
	movq	%r14, %rsi
	movq	%rsi, 32(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	160(%rsp), %rax
	mulq	(%rsp)
	addq	%rax, %r8
	movq	%r15, %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%rdi, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	152(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%rax, %r8
	movq	16(%rsp), %rax
	adcq	%rdx, %r9
	movq	24(%rsp), %rdx
	movq	%r8, %r14
	movq	%r9, %r15
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r14
	movq	160(%rsp), %rax
	adcq	%rdx, %r15
	movq	%r14, 48(%rsp)
	mulq	%r13
	movq	%r15, 56(%rsp)
	movq	%rax, %r8
	movq	%rdx, %r9
	movq	152(%rsp), %rax
	mulq	(%rsp)
	addq	%rax, %r8
	movq	184(%rsp), %rax
	adcq	%rdx, %r9
	mulq	%rbx
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	movq	%rsi, %rax
	adcq	%rdx, %r9
	mulq	%rdi
	addq	%r8, %rax
	movq	%r14, %r8
	adcq	%r9, %rdx
	movq	%r15, %r9
	shrdq	$51, %r15, %r8
	shrq	$51, %r9
	addq	%rax, %r8
	movq	%r8, %rax
	adcq	%rdx, %r9
	andq	%rbp, %rax
	movq	%rax, %rsi
	movq	152(%rsp), %rax
	mulq	%r13
	movq	%rax, %r13
	movq	%rdx, %r14
	movq	%rdi, %rax
	mulq	(%rsp)
	addq	%rax, %r13
	movq	208(%rsp), %rax
	adcq	%rdx, %r14
	mulq	%rbx
	movq	%r14, %r15
	movq	%r13, %r14
	addq	%rax, %r14
	movq	184(%rsp), %rax
	adcq	%rdx, %r15
	mulq	%rcx
	movq	%r14, %rcx
	movq	%r15, %rbx
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	mulq	32(%rsp)
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r9, %r8
	leaq	(%rsi,%rsi), %rbx
	shrq	$51, %r9
	addq	%r8, %rax
	movq	%rbx, (%rsp)
	adcq	%r9, %rdx
	movq	%rax, %r9
	andq	%rbp, %r10
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	16(%rsp), %rax
	addq	%r10, %rdx
	andq	%rbp, %rax
	movq	%rdx, %rdi
	shrq	$51, %rdx
	addq	%rdx, %rax
	movq	48(%rsp), %rdx
	andq	%rbp, %rdi
	movq	%rax, %rcx
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %rcx
	leaq	(%rdx,%rax), %r8
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r14
	leaq	(%r8,%r8), %rax
	leaq	(%r14,%r14), %r15
	movq	%rax, 16(%rsp)
	movq	%r15, %rax
	movl	$49, 48(%rsp)
	mulq	%rcx
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%rdi
	addq	%rax, %r12
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rbx
	adcq	%rdx, %r13
	movq	%r12, %r10
	leaq	(%rbx,%rbx), %rax
	movq	%r13, %r11
	leaq	(%rcx,%rcx), %r13
	mulq	%r8
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, 32(%rsp)
	movq	%r11, 40(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%r8
	addq	%rax, %r12
	movq	%rbx, %rax
	movq	(%rsp), %rbx
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rcx, %rax
	movq	%rdx, %r13
	mulq	%rcx
	movq	%r13, 24(%rsp)
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	movq	16(%rsp), %rsi
	mulq	%r15
	movq	%r12, 16(%rsp)
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r14, %rax
	adcq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r14
	movq	%rbx, %rax
	movq	%rdx, %r15
	mulq	%rdi
	addq	%rax, %r14
	movq	%rsi, %rax
	leaq	(%r9,%r9), %rsi
	adcq	%rdx, %r15
	mulq	%rcx
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r12, %rax
	movq	%r13, %rdx
	addq	%r14, %r10
	adcq	%r15, %r11
	shrdq	$51, %r13, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, %r15
	andq	%rbp, %r15
	movq	%rax, %rsi
	movq	%rbx, %rax
	movq	%rdx, %rdi
	mulq	%rcx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%rsi, %rcx
	movq	%r11, %rsi
	adcq	%rdi, %rbx
	mulq	%r8
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%r10, %rbx
	shrq	$51, %rsi
	shrdq	$51, %r11, %rbx
	addq	%rbx, %rax
	adcq	%rsi, %rdx
	movq	%rax, %rdi
	shrdq	$51, %rdx, %rax
	andq	%rbp, %rdi
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	16(%rsp), %rax
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %rsi
	addq	%rdx, %rax
	movq	%r12, %rdx
	movq	%rax, %r14
	andq	%rbp, %rdx
	shrq	$51, %rax
	andq	%rbp, %r14
	leaq	(%rdx,%rax), %rcx
	movabsq	$2251799813685247, %rbp
	.p2align 4,,10
	.p2align 3
.L347:
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%r15,%r15), %rdx
	leaq	(%rdi,%rax,2), %rax
	movq	%rdx, 16(%rsp)
	leaq	(%rcx,%rcx), %r13
	leaq	(%rax,%rax), %rbx
	movq	%rax, 32(%rsp)
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r8
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	%rcx
	movq	%rax, %r10
	movq	%rbx, %rax
	movq	%rdx, %r11
	mulq	%r14
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%r15
	movq	%rax, %r8
	movq	%rbx, %rax
	movq	%rdx, %r9
	mulq	%rcx
	addq	%rax, %r8
	leaq	(%r14,%r14), %rax
	adcq	%rdx, %r9
	mulq	%rsi
	addq	%r8, %rax
	movq	%r10, %r8
	adcq	%r9, %rdx
	shrdq	$51, %r11, %r8
	movq	%r11, %r9
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, (%rsp)
	movq	%r15, %rax
	movq	(%rsp), %r12
	movq	%rdx, 8(%rsp)
	mulq	%rbx
	movq	%r13, %r15
	movq	16(%rsp), %rbx
	movq	%rax, %r8
	movq	%r13, %rax
	movq	%rdx, %r9
	movq	8(%rsp), %r13
	mulq	%rsi
	addq	%rax, %r8
	movq	%r14, %rax
	adcq	%rdx, %r9
	mulq	%r14
	addq	%r8, %rax
	adcq	%r9, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	32(%rsp), %rax
	adcq	%rdx, %r13
	mulq	%rdi
	movq	%rax, 32(%rsp)
	movq	%r15, %rax
	movq	%rdx, 40(%rsp)
	mulq	%r14
	movq	%rax, %r8
	movq	%rbx, %rax
	addq	32(%rsp), %r8
	movq	%rdx, %r9
	adcq	40(%rsp), %r9
	mulq	%rsi
	addq	%rax, %r8
	movq	%r12, %rax
	adcq	%rdx, %r9
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %r8
	adcq	%rdx, %r9
	addq	%rdi, %rdi
	movq	%r8, %r15
	movq	%rdi, %rax
	andq	%rbp, %r15
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rbx, %rax
	movq	%rdx, %rdi
	mulq	%r14
	addq	%rax, %rsi
	movq	%rcx, %rax
	adcq	%rdx, %rdi
	mulq	%rcx
	movq	%rax, %rcx
	movq	%rdx, %rbx
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%r8, %rcx
	adcq	%r9, %rbx
	movq	%rcx, %rdi
	andq	%rbp, %r12
	shrdq	$51, %rbx, %rcx
	andq	%rbp, %rdi
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	movq	%r10, %rax
	andq	%rbp, %rax
	addq	%rax, %rdx
	movq	(%rsp), %rax
	movq	%rdx, %rsi
	shrq	$51, %rdx
	andq	%rbp, %rax
	andq	%rbp, %rsi
	addq	%rdx, %rax
	movq	%rax, %r14
	shrq	$51, %rax
	andq	%rbp, %r14
	subl	$1, 48(%rsp)
	leaq	(%r12,%rax), %rcx
	jne	.L347
	movq	%rcx, %r11
	movq	80(%rsp), %r10
	movq	%rdi, %r9
	movq	%rsi, %r8
	movq	256(%rsp), %rax
	movq	%r11, (%rsp)
	movq	%r8, 16(%rsp)
	mulq	%rdi
	movq	96(%rsp), %rdi
	movq	%rax, %rcx
	movq	192(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%rsi
	movq	144(%rsp), %rsi
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%rsi, %rax
	adcq	%rdx, %rbx
	mulq	%r11
	addq	%rax, %rcx
	movq	192(%rsp), %rax
	adcq	%rdx, %rbx
	movq	%rcx, %r12
	mulq	%r9
	movq	%rbx, %r13
	movq	%rax, %rcx
	movq	%rsi, %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r8
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	%r11
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	144(%rsp), %rax
	adcq	%rdx, %rbx
	movq	%rcx, %rsi
	mulq	%r9
	movq	%rbx, %rdi
	movq	%rsi, 32(%rsp)
	movq	%rax, %rcx
	movq	96(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	128(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r8
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	%r10, %rax
	movq	%rsi, %r10
	adcq	%rdx, %rbx
	mulq	%r11
	movq	%rdi, %r11
	addq	%rax, %rcx
	movq	96(%rsp), %rax
	adcq	%rdx, %rbx
	shrdq	$51, %rdi, %r10
	movq	%r9, %rdi
	shrq	$51, %r11
	addq	%rcx, %r10
	adcq	%rbx, %r11
	mulq	%r9
	movq	%r11, %r9
	movq	%rax, %rcx
	movq	80(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	addq	%rax, %rcx
	movq	176(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r8
	movq	%r10, %r8
	addq	%rax, %rcx
	movq	128(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	120(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	(%rsp)
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	shrdq	$51, %r11, %r8
	shrq	$51, %r9
	addq	%rcx, %r8
	movq	%r8, %rax
	adcq	%rbx, %r9
	andq	%rbp, %rax
	movq	%rax, %rsi
	movq	80(%rsp), %rax
	mulq	%rdi
	movq	%rax, %rcx
	movq	120(%rsp), %rax
	movq	%rdx, %rbx
	mulq	%r15
	leaq	(%rsi,%rsi), %r15
	addq	%rax, %rcx
	movq	168(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	16(%rsp)
	addq	%rax, %rcx
	movq	176(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	%r14
	addq	%rax, %rcx
	movq	128(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	(%rsp)
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r9, %r8
	shrq	$51, %r9
	addq	%r8, %rax
	adcq	%r9, %rdx
	movq	%rax, %r9
	andq	%rbp, %r12
	andq	%rbp, %r10
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	32(%rsp), %rax
	addq	%r12, %rdx
	andq	%rbp, %rax
	movq	%rdx, %rdi
	shrq	$51, %rdx
	addq	%rdx, %rax
	andq	%rbp, %rdi
	movq	%rax, %rcx
	shrq	$51, %rax
	leaq	(%r10,%rax), %r14
	leaq	(%r9,%r9,8), %rax
	andq	%rbp, %rcx
	leaq	(%r9,%rax,2), %rbx
	leaq	(%r14,%r14), %rax
	leaq	(%rbx,%rbx), %r8
	movq	%rax, (%rsp)
	leaq	(%rcx,%rcx), %r13
	movq	%r8, %rax
	mulq	%rcx
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	leaq	(%rsi,%rsi,8), %rax
	adcq	%rdx, %r11
	leaq	(%rsi,%rax,2), %rdx
	leaq	(%rdx,%rdx), %rax
	movq	%rdx, 16(%rsp)
	mulq	%r14
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %r12
	movq	%r8, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	16(%rsp), %rax
	movq	%r10, 16(%rsp)
	adcq	%rdx, %r13
	mulq	%rsi
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	movq	%r11, %r13
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rsi, %rax
	adcq	%rdx, %r13
	mulq	%r8
	movq	(%rsp), %r8
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	movq	%r13, %r11
	shrq	$51, %r11
	addq	%r10, %rax
	movq	%rax, (%rsp)
	adcq	%r11, %rdx
	movq	%rbx, %rax
	movq	%rdx, 8(%rsp)
	mulq	%r9
	movq	%rax, %rbx
	movq	%r15, %rax
	movq	%rdx, %rsi
	mulq	%rdi
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r8, %rax
	addq	%rbx, %r10
	adcq	%rsi, %r11
	mulq	%rcx
	leaq	(%r9,%r9), %rsi
	addq	%rax, %r10
	movq	(%rsp), %rax
	adcq	%rdx, %r11
	movq	8(%rsp), %rdx
	shrdq	$51, %rdx, %rax
	shrq	$51, %rdx
	addq	%rax, %r10
	movq	%rsi, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, %r8
	andq	%rbp, %r8
	movq	%rax, %rsi
	movq	%rcx, %rax
	movq	%rdx, %rdi
	mulq	%r15
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r14, %rax
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	mulq	%r14
	movq	(%rsp), %r14
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r11, %r10
	movq	16(%rsp), %rbx
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r9
	andq	%rbp, %rbx
	andq	%rbp, %r14
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	addq	%rbx, %rdx
	movq	%r12, %rbx
	andq	%rbp, %rbx
	movq	%rdx, %rdi
	shrq	$51, %rdx
	movq	%rbx, %rax
	andq	%rbp, %rdi
	addq	%rdx, %rax
	movq	%rax, %rsi
	shrq	$51, %rax
	leaq	(%r14,%rax), %r15
	leaq	(%r9,%r9,8), %rax
	andq	%rbp, %rsi
	leaq	(%r9,%rax,2), %r14
	leaq	(%r8,%r8), %rax
	movq	%rax, 16(%rsp)
	movq	%rdi, %rax
	leaq	(%r14,%r14), %rcx
	leaq	(%r15,%r15), %rbx
	mulq	%rdi
	movq	%rbx, (%rsp)
	leaq	(%rsi,%rsi), %r13
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rbx
	adcq	%rdx, %r11
	leaq	(%rbx,%rbx), %rax
	mulq	%r15
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, 32(%rsp)
	movq	%r11, 40(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%r8
	movq	(%rsp), %r8
	movq	%r12, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	mulq	%rdi
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r14, %rax
	movq	16(%rsp), %r14
	adcq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	leaq	(%r9,%r9), %r10
	adcq	%r11, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%rdi
	movq	%rcx, %r8
	andq	%rbp, %r8
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r15, %rax
	adcq	%rdx, %r11
	mulq	%r15
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r9
	andq	%rbp, %r12
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rax
	movq	32(%rsp), %rdx
	andq	%rbp, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %rdi
	addq	%rdx, %rax
	movq	%rax, %rsi
	shrq	$51, %rax
	leaq	(%rax,%r12), %r15
	leaq	(%r9,%r9,8), %rax
	andq	%rbp, %rsi
	leaq	(%r9,%rax,2), %r14
	leaq	(%r8,%r8), %rax
	movq	%rax, 32(%rsp)
	movq	%rdi, %rax
	leaq	(%r14,%r14), %rcx
	leaq	(%r15,%r15), %rbx
	mulq	%rdi
	movq	%rbx, (%rsp)
	leaq	(%rsi,%rsi), %r13
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rbx
	adcq	%rdx, %r11
	leaq	(%rbx,%rbx), %rax
	mulq	%r15
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, 16(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%r8
	movq	(%rsp), %r8
	movq	%r12, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	mulq	%rdi
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r14, %rax
	movq	32(%rsp), %r14
	adcq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	leaq	(%r9,%r9), %r10
	adcq	%r11, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%rdi
	movq	%rcx, %r8
	andq	%rbp, %r8
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r15, %rax
	adcq	%rdx, %r11
	mulq	%r15
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r9
	andq	%rbp, %r12
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rax
	movq	16(%rsp), %rdx
	andq	%rbp, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %rdi
	addq	%rdx, %rax
	movq	%rax, %rsi
	shrq	$51, %rax
	leaq	(%rax,%r12), %r15
	leaq	(%r9,%r9,8), %rax
	andq	%rbp, %rsi
	leaq	(%r9,%rax,2), %r14
	leaq	(%r8,%r8), %rax
	movq	%rax, 32(%rsp)
	movq	%rdi, %rax
	leaq	(%r14,%r14), %rcx
	leaq	(%r15,%r15), %rbx
	mulq	%rdi
	movq	%rbx, (%rsp)
	leaq	(%rsi,%rsi), %r13
	movq	%rax, %r10
	movq	%rsi, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rbx
	adcq	%rdx, %r11
	leaq	(%rbx,%rbx), %rax
	mulq	%r15
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rdi
	movq	%r10, 16(%rsp)
	movq	%rax, %r12
	movq	%r15, %rax
	movq	%rdx, %r13
	mulq	%rcx
	addq	%rax, %r12
	movq	%rbx, %rax
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	adcq	%r13, %rdx
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%r10, %rax
	adcq	%r11, %rdx
	movq	%rax, %r12
	movq	%rsi, %rax
	movq	%rdx, %r13
	mulq	%rsi
	movq	%rax, %r10
	movq	%rcx, %rax
	movq	%rdx, %r11
	mulq	%r8
	movq	(%rsp), %r8
	movq	%r12, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	mulq	%rdi
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r13, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%r14, %rax
	movq	32(%rsp), %r14
	adcq	%rdx, %r13
	mulq	%r9
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rdi
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	leaq	(%r9,%r9), %r10
	adcq	%r11, %rbx
	mulq	%rsi
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r13, %rax
	movq	%r13, %rdx
	shrq	$51, %rdx
	addq	%rax, %rcx
	movq	%r10, %rax
	adcq	%rdx, %rbx
	mulq	%rdi
	movq	%rcx, %r8
	andq	%rbp, %r8
	movq	%rax, %r10
	movq	%r14, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%r15, %rax
	adcq	%rdx, %r11
	mulq	%r15
	leaq	(%r8,%r8), %r15
	addq	%r10, %rax
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %rcx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r9
	andq	%rbp, %r12
	shrdq	$51, %rdx, %rax
	andq	%rbp, %r9
	leaq	(%rax,%rax,8), %rdx
	leaq	(%rax,%rdx,2), %rax
	movq	16(%rsp), %rdx
	andq	%rbp, %rdx
	addq	%rdx, %rax
	movq	(%rsp), %rdx
	movq	%rax, %rsi
	shrq	$51, %rax
	andq	%rbp, %rdx
	andq	%rbp, %rsi
	addq	%rdx, %rax
	movq	%rax, %rcx
	shrq	$51, %rax
	leaq	(%rax,%r12), %r14
	leaq	(%r9,%r9,8), %rax
	andq	%rbp, %rcx
	leaq	(%r9,%rax,2), %rax
	leaq	(%r14,%r14), %rbx
	movq	%rax, 32(%rsp)
	leaq	(%rax,%rax), %rdi
	movq	%rsi, %rax
	leaq	(%rcx,%rcx), %r13
	mulq	%rsi
	movq	%rbx, (%rsp)
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	%rdx, %r11
	mulq	%rcx
	addq	%rax, %r10
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rbx
	adcq	%rdx, %r11
	leaq	(%rbx,%rbx), %rax
	mulq	%r14
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	movq	%r10, 16(%rsp)
	movq	%r11, 24(%rsp)
	movq	%rax, %r12
	movq	%rdi, %rax
	movq	%rdx, %r13
	mulq	%r14
	addq	%rax, %r12
	movq	%rbx, %rax
	movq	(%rsp), %rbx
	adcq	%rdx, %r13
	mulq	%r8
	addq	%r12, %rax
	movq	%r10, %r12
	adcq	%r13, %rdx
	movq	%r11, %r13
	shrdq	$51, %r11, %r12
	shrq	$51, %r13
	addq	%rax, %r12
	movq	%rcx, %rax
	adcq	%rdx, %r13
	mulq	%rcx
	movq	%rax, %r10
	movq	%r8, %rax
	movq	%rdx, %r11
	mulq	%rdi
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%rsi
	addq	%r10, %rax
	movq	%r12, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r13, %r10
	movq	%r13, %r11
	shrq	$51, %r11
	addq	%r10, %rax
	movq	%rax, (%rsp)
	movq	32(%rsp), %rax
	adcq	%r11, %rdx
	movq	%rdx, 8(%rsp)
	mulq	%r9
	movq	%rax, %r10
	movq	%r15, %rax
	movq	%rdx, %r11
	mulq	%rsi
	addq	%rax, %r10
	movq	%rbx, %rax
	adcq	%rdx, %r11
	mulq	%rcx
	addq	%r10, %rax
	movq	(%rsp), %r10
	adcq	%r11, %rdx
	movq	8(%rsp), %r11
	shrdq	$51, %r11, %r10
	shrq	$51, %r11
	addq	%rax, %r10
	adcq	%rdx, %r11
	addq	%r9, %r9
	movq	%r9, %rax
	mulq	%rsi
	movq	%rax, %rsi
	movq	%rcx, %rax
	movq	%rdx, %rdi
	mulq	%r15
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r14, %rax
	addq	%rsi, %rcx
	adcq	%rdi, %rbx
	mulq	%r14
	movq	(%rsp), %r14
	leaq	272(%rsp), %rdi
	addq	%rcx, %rax
	movq	%r10, %rcx
	adcq	%rbx, %rdx
	shrdq	$51, %r11, %rcx
	movq	%r11, %rbx
	shrq	$51, %rbx
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	movq	%rax, %rcx
	andq	%rbp, %r14
	andq	%rbp, %r10
	shrdq	$51, %rdx, %rcx
	andq	%rbp, %rax
	movq	%r10, 520(%rsp)
	leaq	(%rcx,%rcx,8), %rdx
	movq	%rax, 528(%rsp)
	leaq	(%rcx,%rdx,2), %rcx
	movq	16(%rsp), %rdx
	andq	%rbp, %rdx
	addq	%rdx, %rcx
	movq	%r12, %rdx
	movq	%rcx, %rsi
	andq	%rbp, %rdx
	andq	%rbp, %rcx
	shrq	$51, %rsi
	movq	%rcx, 496(%rsp)
	addq	%rsi, %rdx
	leaq	496(%rsp), %rsi
	movq	%rdx, %rcx
	shrq	$51, %rdx
	addq	%r14, %rdx
	andq	%rbp, %rcx
	movq	%rdx, 512(%rsp)
	leaq	320(%rsp), %rdx
	movq	%rcx, 504(%rsp)
	call	fiat_25519_carry_mul
	movq	304(%rsp), %rax
	movq	296(%rsp), %r12
	movq	304(%rsp), %rbx
	movq	288(%rsp), %r9
	leaq	(%rax,%rax,8), %rax
	movq	280(%rsp), %rsi
	movq	272(%rsp), %rdi
	leaq	(%rbx,%rax,2), %r8
	leaq	(%r12,%r12,8), %rax
	leaq	(%r12,%rax,2), %r13
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r14
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	movq	%r14, (%rsp)
	movq	%rax, 120(%rsp)
	mulq	592(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%r14, %rax
	mulq	584(%rsp)
	addq	%rax, %r10
	movq	%rdi, %rax
	adcq	%rdx, %r11
	mulq	560(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	576(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	568(%rsp)
	movq	%r10, %rcx
	movq	%r11, %rbx
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	568(%rsp)
	movq	%rcx, 32(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	560(%rsp)
	addq	%rax, %r10
	movq	%r14, %rax
	adcq	%rdx, %r11
	mulq	592(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	584(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	576(%rsp)
	addq	%r10, %rax
	movq	%rcx, %r10
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %r10
	xorl	%r15d, %r15d
	movq	%r10, %r14
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	576(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	568(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	560(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	592(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	584(%rsp)
	addq	%r10, %rax
	movq	%r14, %r10
	adcq	%r11, %rdx
	shrdq	$51, %r15, %r10
	xorl	%ebx, %ebx
	addq	%r10, %rax
	adcq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%rdi, %rax
	movq	%rdx, %rbx
	mulq	584(%rsp)
	movq	%rcx, 48(%rsp)
	movq	%rbx, 56(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	576(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	568(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	560(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	592(%rsp)
	addq	%r10, %rax
	movq	%rcx, %r10
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %r10
	xorl	%ebx, %ebx
	addq	%r10, %rax
	adcq	%rbx, %rdx
	movq	%rax, 16(%rsp)
	movq	%rdi, %rax
	movq	%rdx, 24(%rsp)
	mulq	592(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	584(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	576(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	568(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	560(%rsp), %rax
	addq	%r10, %rcx
	adcq	%r11, %rbx
	mulq	304(%rsp)
	movq	32(%rsp), %r11
	addq	%rax, %rcx
	movq	16(%rsp), %rax
	adcq	%rdx, %rbx
	movq	24(%rsp), %rdx
	shrdq	$51, %rdx, %rax
	xorl	%edx, %edx
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	movq	%rcx, %rdx
	andq	%rbp, %r11
	movq	%rcx, 64(%rsp)
	shrdq	$51, %rbx, %rdx
	movq	%rbx, 72(%rsp)
	andq	%rbp, %r14
	movq	%rdx, %rax
	leaq	(%rdx,%rdx,8), %rdx
	leaq	(%rax,%rdx,2), %rdx
	movq	120(%rsp), %rax
	leaq	(%rdx,%r11), %rbx
	mulq	632(%rsp)
	movq	%rbx, 80(%rsp)
	movq	%rbx, %r11
	shrq	$51, %r11
	leaq	(%r14,%r11), %r15
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	(%rsp), %rax
	movq	%r15, 96(%rsp)
	mulq	624(%rsp)
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	mulq	600(%rsp)
	movq	%rcx, %r14
	movq	%rbx, %r15
	addq	%rax, %r14
	movq	%r13, %rax
	adcq	%rdx, %r15
	mulq	616(%rsp)
	movq	%r14, %r10
	movq	%r15, %r11
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	608(%rsp)
	movq	%r10, %r14
	movq	%r11, %r15
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	608(%rsp)
	movq	%r14, %r10
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	mulq	600(%rsp)
	addq	%rax, %rcx
	movq	(%rsp), %rax
	movq	%r14, (%rsp)
	adcq	%rdx, %rbx
	mulq	632(%rsp)
	addq	%rax, %rcx
	movq	%r13, %rax
	adcq	%rdx, %rbx
	mulq	624(%rsp)
	addq	%rax, %rcx
	movq	%r8, %rax
	adcq	%rdx, %rbx
	mulq	616(%rsp)
	addq	%rcx, %rax
	adcq	%rbx, %rdx
	shrdq	$51, %r15, %r10
	xorl	%r15d, %r15d
	movq	%r10, %r14
	addq	%rax, %r14
	movq	%rdi, %rax
	adcq	%rdx, %r15
	mulq	616(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	608(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	600(%rsp)
	addq	%rax, %r10
	movq	%r13, %rax
	adcq	%rdx, %r11
	mulq	632(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%r8, %rax
	addq	%r10, %rcx
	movq	%r14, %r10
	adcq	%r11, %rbx
	mulq	624(%rsp)
	addq	%rax, %rcx
	movq	%rdi, %rax
	adcq	%rdx, %rbx
	shrdq	$51, %r15, %r10
	xorl	%edx, %edx
	movabsq	$2251799813685229, %r15
	addq	%r10, %rcx
	adcq	%rdx, %rbx
	mulq	624(%rsp)
	movq	%rcx, 32(%rsp)
	movq	%rbx, 40(%rsp)
	movq	%rax, %r10
	movq	%rdx, %r11
	movq	%rsi, %rax
	mulq	616(%rsp)
	addq	%rax, %r10
	movq	%r9, %rax
	adcq	%rdx, %r11
	mulq	608(%rsp)
	addq	%rax, %r10
	movq	%r12, %rax
	adcq	%rdx, %r11
	mulq	600(%rsp)
	addq	%rax, %r10
	movq	%r8, %rax
	adcq	%rdx, %r11
	mulq	632(%rsp)
	addq	%r10, %rax
	movq	%rcx, %r10
	adcq	%r11, %rdx
	shrdq	$51, %rbx, %r10
	xorl	%ebx, %ebx
	addq	%r10, %rax
	adcq	%rbx, %rdx
	movq	%rax, %r10
	movq	%rdi, %rax
	movq	(%rsp), %rdi
	movq	%rdx, %r11
	mulq	632(%rsp)
	movq	%rax, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	%r10, %rsi
	mulq	624(%rsp)
	addq	%rax, %rcx
	movq	%r9, %rax
	adcq	%rdx, %rbx
	mulq	616(%rsp)
	addq	%rax, %rcx
	movq	%r12, %rax
	adcq	%rdx, %rbx
	mulq	608(%rsp)
	addq	%rax, %rcx
	movq	600(%rsp), %rax
	adcq	%rdx, %rbx
	mulq	304(%rsp)
	addq	%rax, %rcx
	adcq	%rdx, %rbx
	shrdq	$51, %r11, %rsi
	xorl	%edx, %edx
	addq	%rsi, %rcx
	adcq	%rdx, %rbx
	movq	%rcx, %rsi
	andq	%rbp, %rdi
	andq	%rbp, %r14
	shrdq	$51, %rbx, %rsi
	andq	%rbp, %rcx
	leaq	(%rsi,%rsi,8), %rdx
	leaq	(%rsi,%rdx,2), %rsi
	movabsq	$-2251799813685229, %rdx
	addq	%rdi, %rsi
	movq	%rsi, %rdi
	andq	%rbp, %rsi
	shrq	$51, %rdi
	addq	%rdx, %rsi
	movq	%rdi, %rax
	addq	%r14, %rax
	movq	%rax, %rdi
	shrq	$51, %rax
	andq	%rbp, %rdi
	movq	%rdi, %rdx
	movq	%rsi, %rdi
	sarq	$51, %rdi
	negl	%edi
	movzbl	%dil, %edi
	subq	%rdi, %rdx
	movq	32(%rsp), %rdi
	subq	%rbp, %rdx
	andq	%rbp, %rdi
	addq	%rdi, %rax
	movq	%rdx, %rdi
	sarq	$51, %rdi
	negl	%edi
	movzbl	%dil, %edi
	subq	%rdi, %rax
	movq	%r10, %rdi
	subq	%rbp, %rax
	andq	%rbp, %rdi
	movq	%rax, %r8
	sarq	$51, %r8
	negl	%r8d
	movzbl	%r8b, %r8d
	subq	%r8, %rdi
	subq	%rbp, %rdi
	movq	%rdi, %r8
	sarq	$51, %r8
	negl	%r8d
	movzbl	%r8b, %r8d
	subq	%r8, %rcx
	subq	%rbp, %rcx
	movq	%rcx, %r8
	sarq	$51, %r8
	movzbl	%r8b, %r8d
	movq	%r8, %r9
	andq	%rbp, %rsi
	andq	%rbp, %r8
	andq	%rbp, %rdx
	andq	%r15, %r9
	andq	%rbp, %rax
	andq	%rbp, %rdi
	andq	%rbp, %rcx
	leaq	(%rsi,%r9), %r10
	leaq	(%rdx,%r8), %r9
	addq	%r8, %rdi
	addq	%r8, %rcx
	movq	%r10, %rsi
	shrq	$51, %r10
	leaq	(%rax,%r8), %rdx
	addq	%r10, %r9
	andq	%rbp, %rsi
	movq	%r9, %r14
	leaq	0(,%r9,8), %rax
	movq	%rsi, %r13
	movq	%rsi, %r12
	movabsq	$18014398509481976, %r9
	shrq	$51, %r14
	addq	%r14, %rdx
	andq	%r9, %rax
	movq	%rsi, %r9
	movq	%rdx, %r14
	shrq	$48, %r9
	movq	%rax, %r8
	shrq	$51, %r14
	salq	$6, %rdx
	addq	%r14, %rdi
	leaq	(%rax,%r9), %r14
	movabsq	$144115188075855808, %r9
	andq	%r9, %rdx
	movq	%rax, %r9
	movzbl	%r14b, %r14d
	shrq	$48, %r9
	addq	%r9, %rdx
	movq	%rdi, %r9
	shrq	$51, %rdi
	andq	%rbp, %r9
	addq	%rcx, %rdi
	movabsq	$36028797018963952, %rcx
	movq	%r9, %r10
	movq	%rdx, %r9
	salq	$4, %rdi
	shrq	$56, %r9
	andq	%rdi, %rcx
	leaq	(%r9,%r10,2), %rbx
	movq	%rax, %r9
	movq	%rdx, %r10
	movq	%rbx, %rdi
	shrq	$48, %rdi
	addq	%rdi, %rcx
	movq	%rsi, %rdi
	shrq	$16, %rdi
	movq	%rdi, %r11
	movq	%rsi, %rdi
	shrq	$24, %rdi
	shrq	$16, %r9
	shrq	$24, %r8
	movq	%r9, (%rsp)
	movq	%rax, %r9
	shrq	$40, %r13
	movq	%r8, 32(%rsp)
	movq	%rax, %r8
	andl	$65280, %eax
	movzbl	%r13b, %r13d
	shrq	$32, %r12
	orq	%r14, %rax
	shrq	$16, %r10
	salq	$8, %rax
	movzbl	%r12b, %r12d
	shrq	$40, %r9
	orq	%r13, %rax
	movzbl	%r9b, %r9d
	shrq	$32, %r8
	salq	$8, %rax
	movzbl	%r8b, %r8d
	orq	%r12, %rax
	movzbl	%dil, %r12d
	salq	$8, %rax
	orq	%r12, %rax
	movzbl	%r11b, %r12d
	movq	%rax, %rdi
	movq	%rsi, %rax
	movzbl	%sil, %esi
	salq	$8, %rdi
	movzbl	%ah, %eax
	movq	%rdi, %r11
	movzbl	%dh, %edi
	orq	%r12, %r11
	salq	$8, %r11
	orq	%rax, %r11
	movq	%r10, %rax
	movzbl	%r10b, %r10d
	andl	$65280, %eax
	salq	$8, %r11
	orq	%r10, %rax
	movzbl	%dl, %r10d
	salq	$8, %rax
	orq	%rdi, %rax
	movzbl	32(%rsp), %edi
	salq	$8, %rax
	orq	%r10, %rax
	salq	$8, %rax
	orq	%r9, %rax
	movq	%r11, %r9
	movq	224(%rsp), %r11
	salq	$8, %rax
	orq	%r8, %rax
	movq	%rbx, %r8
	salq	$8, %rax
	orq	%rdi, %rax
	orq	%rsi, %r9
	movzbl	(%rsp), %edi
	shrq	$16, %r8
	movq	%r9, (%rsp)
	movq	%rbx, %r9
	salq	$8, %rax
	movzbl	%r8b, %r8d
	shrq	$24, %r9
	movq	%rax, %rsi
	movq	%r9, %rax
	movzbl	%r9b, %r9d
	orq	%rdi, %rsi
	movq	%rdx, %rdi
	andl	$65280, %eax
	movq	%rsi, 8(%rsp)
	movq	%rdx, %rsi
	shrq	$48, %rdx
	orq	%r9, %rax
	movzbl	%dl, %edx
	shrq	$40, %rdi
	movdqa	(%rsp), %xmm0
	salq	$8, %rax
	movzbl	%dil, %edi
	shrq	$32, %rsi
	orq	%r8, %rax
	movzbl	%sil, %esi
	movups	%xmm0, (%r11)
	movq	%rax, %r9
	movzbl	%bh, %eax
	movq	%rax, %r8
	movq	%r9, %rax
	salq	$8, %rax
	orq	%r8, %rax
	movzbl	%bl, %r8d
	shrq	$40, %rbx
	salq	$8, %rax
	movzbl	%bl, %ebx
	orq	%r8, %rax
	salq	$8, %rax
	orq	%rdx, %rax
	movzbl	%ch, %edx
	salq	$8, %rax
	orq	%rdi, %rax
	movq	%rcx, %rdi
	salq	$8, %rax
	shrq	$16, %rdi
	orq	%rsi, %rax
	movq	%rax, 16(%r11)
	movzbl	%dil, %eax
	movq	%rcx, %rdi
	sall	$8, %eax
	shrq	$24, %rdi
	orl	%edx, %eax
	movzbl	%cl, %edx
	movb	%dil, 28(%r11)
	movq	%rcx, %rdi
	sall	$8, %eax
	orl	%edx, %eax
	movq	64(%rsp), %rdx
	sall	$8, %eax
	orl	%ebx, %eax
	shrq	$32, %rdi
	movq	%r11, %rbx
	andq	%rbp, %rdx
	movb	%dil, 29(%r11)
	movq	%rcx, %rdi
	shrq	$40, %rdi
	movl	%eax, 24(%r11)
	movb	%dil, 30(%r11)
	movq	80(%rsp), %rdi
	movq	16(%rsp), %rsi
	movq	96(%rsp), %r11
	andq	%rbp, %rdi
	movdqa	(%rsp), %xmm6
	movdqa	.LC0(%rip), %xmm1
	movq	%rdi, %rax
	movq	48(%rsp), %rdi
	movq	%r11, %r8
	andq	%rbp, %r11
	subq	%r15, %rax
	shrq	$51, %r8
	andq	%rbp, %rsi
	movdqa	.LC1(%rip), %xmm2
	movq	%rax, %r9
	andq	%rbp, %rdi
	andq	%rbp, %rax
	movdqa	.LC2(%rip), %xmm3
	sarq	$51, %r9
	addq	%r8, %rdi
	movq	%r11, %r8
	movdqa	.LC3(%rip), %xmm4
	negl	%r9d
	movdqa	.LC8(%rip), %xmm5
	movaps	%xmm1, 720(%rsp)
	movzbl	%r9b, %r9d
	movaps	%xmm2, 736(%rsp)
	subq	%r9, %r8
	movaps	%xmm3, 752(%rsp)
	subq	%rbp, %r8
	movaps	%xmm4, 768(%rsp)
	sarq	$51, %r8
	movaps	%xmm5, 784(%rsp)
	negl	%r8d
	movaps	%xmm6, 800(%rsp)
	movzbl	%r8b, %r8d
	subq	%r8, %rdi
	subq	%rbp, %rdi
	sarq	$51, %rdi
	negl	%edi
	movzbl	%dil, %edi
	subq	%rdi, %rsi
	leaq	720(%rsp), %rdi
	subq	%rbp, %rsi
	sarq	$51, %rsi
	negl	%esi
	movzbl	%sil, %esi
	subq	%rsi, %rdx
	movq	248(%rsp), %rsi
	subq	%rbp, %rdx
	sarq	$51, %rdx
	movzbl	%dl, %edx
	andq	%r15, %rdx
	addq	%rdx, %rax
	movl	$32, %edx
	sall	$7, %eax
	shrq	$48, %rcx
	addq	$32, %rsi
	xorl	%eax, %ecx
	movabsq	$274877906976, %rax
	movb	%cl, 31(%rbx)
	movdqu	16(%rbx), %xmm7
	movq	%rax, 928(%rsp)
	movaps	%xmm7, (%rsp)
	movaps	%xmm7, 816(%rsp)
	call	SHA512_Update
	movq	240(%rsp), %rdx
	movq	232(%rsp), %rsi
	leaq	720(%rsp), %rdi
	call	SHA512_Update
	movl	932(%rsp), %esi
	leaq	720(%rsp), %rdx
	leaq	496(%rsp), %rdi
	call	sha512_final_impl
	leaq	496(%rsp), %rdi
	call	x25519_sc_reduce
	leaq	32(%rbx), %rdi
	leaq	432(%rsp), %rcx
	leaq	368(%rsp), %rdx
	leaq	496(%rsp), %rsi
	call	sc_muladd
	addq	$952, %rsp
	.cfi_def_cfa_offset 56
	movl	$1, %eax
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE85:
	.size	ED25519_sign, .-ED25519_sign
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB86:
	.cfi_startproc
	subq	$184, %rsp
	.cfi_def_cfa_offset 192
	xorl	%ecx, %ecx
	movq	$1296236545, 32(%rsp)
	leaq	32(%rsp), %rax
	movl	%ecx, %edx
	movq	$message, 40(%rsp)
	movq	$256, 48(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
#APP
# 2261 "/root/benchmarks/src/boringssl_ed25519/bench_ed25519_plain.c" 1
	rolq $3,  %rdi ; rolq $13, %rdi
	rolq $61, %rdi ; rolq $51, %rdi
	xchgq %rbx,%rbx
# 0 "" 2
#NO_APP
	movq	%rdx, 8(%rsp)
	movl	%ecx, %edx
	movq	8(%rsp), %rax
	leaq	80(%rsp), %rax
	movq	$1296236545, 80(%rsp)
	movq	$signature, 88(%rsp)
	movq	$64, 96(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 120(%rsp)
#APP
# 2262 "/root/benchmarks/src/boringssl_ed25519/bench_ed25519_plain.c" 1
	rolq $3,  %rdi ; rolq $13, %rdi
	rolq $61, %rdi ; rolq $51, %rdi
	xchgq %rbx,%rbx
# 0 "" 2
#NO_APP
	movq	%rdx, 16(%rsp)
	movl	%ecx, %edx
	movq	16(%rsp), %rax
	leaq	128(%rsp), %rax
	movq	$1296236545, 128(%rsp)
	movq	$private_key, 136(%rsp)
	movq	$64, 144(%rsp)
	movq	$0, 152(%rsp)
	movq	$0, 160(%rsp)
	movq	$0, 168(%rsp)
#APP
# 2263 "/root/benchmarks/src/boringssl_ed25519/bench_ed25519_plain.c" 1
	rolq $3,  %rdi ; rolq $13, %rdi
	rolq $61, %rdi ; rolq $51, %rdi
	xchgq %rbx,%rbx
# 0 "" 2
#NO_APP
	movl	$private_key, %ecx
	movq	%rdx, 24(%rsp)
	movl	$message, %esi
	movl	$256, %edx
	movl	$signature, %edi
	movq	24(%rsp), %rax
	call	ED25519_sign
	xorl	%eax, %eax
	addq	$184, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE86:
	.size	main, .-main
	.globl	private_key
	.section	secret,"aw"
	.align 32
	.type	private_key, @object
	.size	private_key, 64
private_key:
	.ascii	"\006\343\024\201\017\353`\233\361\276\342\0253)\360\206^\002"
	.ascii	"c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347"
	.ascii	"H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<"
	.ascii	"\b\035\267\370\255\235\253"
	.globl	public_key
	.section	public,"aw"
	.align 32
	.type	public_key, @object
	.size	public_key, 32
public_key:
	.ascii	"\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022"
	.ascii	"\240\023\340\353\2703<\b\035\267\370\255\235\253"
	.globl	signature
	.section	secret
	.align 32
	.type	signature, @object
	.size	signature, 64
signature:
	.zero	64
	.globl	message
	.align 32
	.type	message, @object
	.size	message, 256
message:
	.string	"\006\343\024\201\017\353`\233\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703\364\356\330|\005\030\375\351x+`\255D\326`\027\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253\361\276\342\0253)\360\206^\002c\252\005\264\364\341^\002c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347H<\b\035\267\370\255\235\253"
	.zero	127
	.globl	k25519Precomp
	.section	.rodata
	.align 32
	.type	k25519Precomp, @object
	.size	k25519Precomp, 24576
k25519Precomp:
	.ascii	"\205;\214\365\306\223\274/\031\016\214\373\306-\223\317\302B"
	.ascii	"=d\230H\013'e\272\3243:\235\317\007"
	.ascii	">\221@\327\0059\020\235\263\276@\321\005\2379\375\t\212\217h"
	.ascii	"4\204\301\245g\022\370\230\222/\375D"
	.ascii	"h\252z\207\005\022\311\253\236\304\252\314#\350\331&\214YC\335"
	.ascii	"\313}\033Z\250e\f\237h{\021o"
	.ascii	"\327q<\223\374\347$\222\265\365\017z\226\235F\237\002\007\326"
	.ascii	"\341e\232\246Z..}\250?\006\fY"
	.ascii	"\250\325\264B`\245\231\212\366\254`N\f\201+\217\2527n\261k#\236"
	.ascii	"\340U%\311i\246\225\265k"
	.ascii	"_z\233\245\263\250\372Cx\317\232]\335k\30161j=\013\204\240\017"
	.ascii	"Ps\013\245>\261\365\032p"
	.ascii	"0\227\356L\250\260%\257\212K\206\3500\204Z\0022g\001\237\002"
	.ascii	"P\033\301\364\370\200\232\033N\026z"
	.ascii	"e\322\374\244\350\037aV}\272\301\345\375S\323;\275\326K!\032"
	.ascii	"\3631\201b\332[U\207\025\271*"
	.ascii	"\211\330\320\r?\223\256\024b\3325\034\"#\224XL\333\362\214E\345"
	.ascii	"p\321\306\264\271\022\257&(Z"
	.ascii	"\237\t\374\216\271Qs(8%\375}\364\306ege\222\n\373=\2154\312'"
	.ascii	"\207\345!\003\221\016h"
	.ascii	"\277\030h\005\n\005\376\225\251\372`Vq\211~2sP\240\006\315\343"
	.ascii	"\350\303\232\244EtL?\223'"
	.ascii	"\t\377v\304\351\373\023Zr\301\\{E9\236n\224D+\020\371\334\333"
	.ascii	"]+>Uc\277\f\235\177"
	.ascii	"3\273\245\bD\274\022\242\002\355^\307\303HP\215D\354\277Z\f\353"
	.ascii	"\033\335\353\006\342F\361\314E)"
	.string	"\272\326G\244\303\202\221\177\267)'K\321\024"
	.ascii	"\325\207\240d\270\034\361<\343\363U\033\353s~J\025"
	.ascii	"\205\202*\201\361\333\273\274\374\321\275\320\007\b\016'-\247"
	.ascii	"\275\033\013g\033\264\232\266;ki\276\252C"
	.string	"1q\025w\353\356\f:\210\257\310"
	.ascii	"\211\025'\2336\247Y\332h\266e\200\2758\314\242\266{\345Q"
	.ascii	"\244\214}{\266\006\230I9'\322'\204\342[W\271SE \347\\\b\273\204"
	.ascii	"xA\256AL\2668"
	.ascii	"qK\352\002g2\254\205\001\273\241A\003\340p\276D\301;\bK\242\344"
	.ascii	"S\343a\r\237\032\351\270\020"
	.ascii	"\277\243N\224\320\\\032k\322\300\235\263:5ptI.T(\202R\262q~\222"
	.ascii	"<(i\352\033F"
	.ascii	"\261!2\252\232,o\272\247#\272;S!\240l:,\031\222Ov\352\235\340"
	.ascii	"\027S.]\335n\035"
	.ascii	"\242\263\270\001\310m\203\361\232\244>\005G_\003\263\363\255"
	.ascii	"wX\272A\234R\247\220\017j\034\273\237z"
	.ascii	"\217>\335\004fY\267Y,p\210\342w\003\263l#\303\331^f\2343\261"
	.ascii	"/\345\274a`\347\025\t"
	.ascii	"\3314\222\363\355]\247\342\371X\265\341\200v=\226\373#<n\254"
	.ascii	"A',\303\001\0162\241$\220:"
	.ascii	"\032\221\242\311\331\365\301\347\327\247\314\213xq\243\2702*"
	.ascii	"\266\016\031\022dc\225N\314.\\|\220&"
	.ascii	"\035\234/c\016\335\314.\0251\211v\226\266\320QXzc\250k\267\337"
	.ascii	"R9\357\016\240I}\323m"
	.ascii	"^Q\252ITc[\355:\202\306\013\237\304e\250\304\321B[\351\037\f"
	.ascii	"\205\271\025\323\003om\3270"
	.ascii	"\307\344\006!\027DDli\177\215\222\200\326S\373&?Mi\244\236s\264"
	.ascii	"\260K\206.\021\227\306\020"
	.ascii	"\005\310X\203\240*\246\fGB z\343J=j\334\355\021;\246\323dt\357"
	.ascii	"\006\bU\257\233\277\003"
	.string	"\336_\276}'\304\223d\242~\255\031\255O]&\220E0F\310\337"
	.ascii	"\016\t\376f\355\253\034\346%"
	.ascii	"\004fX\314(\341\023?~tY\264\354sXo\365h\022\314\355=\266\240"
	.ascii	",\342\206EcxmV"
	.ascii	"\320/Z\306\205B\005\241\303g\026\363*\021dlX\356\032s@\342\n"
	.ascii	"h*\262\223G\363\245\373\024"
	.ascii	"4\b\301\234\237\2447\026Q\304\233\250\325V\216\274\333\322\177"
	.ascii	"\177\017\354\265\034\3315\314^\312[\2273"
	.string	"\324\367\205i\026F\327<W"
	.ascii	"\310\311\204^>Y\036\023a{\266\362\303/lR\374\203\352\234\202"
	.ascii	"\024"
	.ascii	"\270\354qN/\013\347!\343w\244@\271\335V\346\200O\035\316\316"
	.ascii	"Ve\277~{]S\304;\374\005"
	.ascii	"\302\225\335\227\204{C\377\247\265N\2520Ntl\213\350\205<a]\f"
	.ascii	"\236s\201u_\036\307\331/"
	.ascii	"\335\336\257R\256\263\270$\3170;\355\214c\2254\225\201\276\251"
	.ascii	"\203\274\2443\004\037e\\Gg77"
	.ascii	"\220e$\024\313\225@c5U\301\026@\024\022\357`\274\020\211\f\024"
	.ascii	"8\236\214|\2200W\220\365k"
	.ascii	"\331\255\321@\375\231\272/'\320\364\226o\026\007\263\256;\360"
	.ascii	"\025R\360cC\231\371\030;l\245\276\037"
	.ascii	"\212[A\341\361x\247\017~\247\303\272\367\237@\006P\232\242\232"
	.ascii	"\270\327RoVZcz\366\034R\002"
	.string	"\344^/w g\024\261\316\232\007\226\261\224\370\350J\202\254"
	.string	"M\"\370J\304l\315\367\331S\027"
	.ascii	"\224R\235\n\013\356?QfZ\337\017\\\347\230\217\316\007\341\277"
	.ascii	"\210\206a\324\355,8q~\n\240?"
	.ascii	"4\333=\226-#i<X8\227\264\332\207\336\035\205\362\221\240\371"
	.ascii	"\321\327\252\266\355H\240/\376\265\022"
	.ascii	"\222\036o\255&|+\337\023\211KP#\323fK\303\213\034u\300\235@\214"
	.ascii	"\270\307\226\007\302\223~o"
	.ascii	"M\343\374\226\304\373\360q\355[\363\255k\202\271sa\305(\377a"
	.ascii	"r\004\322o \261o\371v\233t"
	.ascii	"\005\256\246\256\004\366Z\037\231\234\344\276\361Q#\301fk\377"
	.ascii	"\356\265\b\250aQ!\340\001\017\301\316\017"
	.ascii	"EN$\304\235\322\362=\n\336\330\223t\016\002+M!\f\202~\006\310"
	.ascii	"l\n\271\352o\026y7A"
	.ascii	"D\036\376I\246XMd~w\2551\242\256\374!\322\320\177\210Z\034D\002"
	.ascii	"\363\021\305\203q\252\001I"
	.ascii	"\360\370\032\214T\267\261\b\264\231b$|z\017\3169\331\006\036"
	.ascii	"\371\260`\367\023\022mr{\210\273A"
	.ascii	"\256\221f|YL#~\310\264\205\n=\235\210d\347\372J5\f\311\342\332"
	.ascii	"\035\236j\f\007\036\207\n"
	.ascii	"\276FCtD}\350@%+\265\025\324\332H\035>`;\241\030\212:|\367\275"
	.ascii	"\315/\301(\267N"
	.ascii	"\211\211\274K\231\265\0013`B\335[:\256ks<\236\325\031\342\255"
	.ascii	"a\rd\324\205&\0170\347>"
	.ascii	"\030u\036\204Gy\372C\327F\234cY\372\306\345t+\005\343\035^\006"
	.ascii	"\2410\220\270\317\242\306G}"
	.ascii	"\267\326}\236\344U\322\365\254\036\013a\\\021\026\200\312\207"
	.ascii	"\341\222]\227\231<\302%\221\227bW\201\023"
	.ascii	"\340\326\360\216\024\320\332?<oT\221\232t>\235W\201\273&\020"
	.ascii	"b\354q\200\354\3114\215\365\214\024"
	.ascii	"mu\344\232}/W\342\177H\363\210\273E\303V\215\250`im\013\321\237"
	.ascii	"\271\241\256N\255\353\217'"
	.ascii	"'\3604y\366\222\244F\251\n\204\366\276\204\231FT\030a\211*\274"
	.ascii	"\241\\\324\273]\275\036\372\362?"
	.ascii	"f9\223\214\037h\252\261\230\f) \234\224!\214R<\235!\221R\021"
	.ascii	"9{g\234\376\002\335\004A"
	.ascii	"\270j\t\333\006N!\2015O\344\f\311\266\250!\365*\236@*\301$e\201"
	.ascii	"\244\374\216\244\265e\001"
	.string	"*B$\021^\277\262r\265:\243\2303\f\372\241f\266R\372\001a\313\224\325S\257\257"
	.ascii	";\206,"
	.ascii	"vj\204\240t\244\220\361\300|/\315\204\371\357\022\217+\252X\006"
	.ascii	")^i\270\310\376\277\331g\033Y"
	.ascii	"]\265\030\237q\263\271\231\036d\214\241\372\345e\344\355\005"
	.ascii	"\237\3026\021\ba\213\0220p\206O\233H"
	.ascii	"\372\233\264\200\034\r/1\212\354\363\253^QyY\210\034\360\236"
	.ascii	"\3003pr\313{\217\312\307.\340="
	.ascii	"\357\222\353:-\0202\322a\250\026a\264Sb\341$\252\013\031\347"
	.ascii	"\253~=\277\276lI\272\373\365I"
	.ascii	".W\234\036\214b]\025AG\210\305\254\206M\212\353cWQ\366R\243\221"
	.ascii	"[Qg\210\302\246\241\006"
	.ascii	"\324\317[\212\020\232\2240\353sd\274p\335@\334\034\r|0\301\224"
	.ascii	"\302\222tn\372\313m\250\004V"
	.ascii	"\266d\027|\324\321\210rQ\213A\340@\021Tr\321\366\254\030`\032"
	.ascii	"\003\237\306B'\376\211\236\230 "
	.ascii	".\354\352\205\213't\026\337+\313z\007\334!VZ\364\313a\026L\n"
	.ascii	"d\323\225\005\367P\231\013s"
	.ascii	"\177\314-:\375w\227I\222\330O\245,|\2052\240\343\007\322d\330"
	.ascii	"y\242)~\246\f\035\355\003\004"
	.ascii	"R\305N\2075-K\311\215o$\230\317\310\346\305\3165\300\026\372"
	.ascii	"F\313\367\314=0\bCE\327["
	.ascii	"*y\347\025!\223\304\205\311\335\315\275\242\211L\306b\327\243"
	.ascii	"\255\250=\036\235,\370g0\022\333\267["
	.string	"\302L\262(\225\321\232\177\201\3015ceTk\1776r\300On\266\270f\203\255\200s"
	.ascii	"x:\023"
	.ascii	"\276b\312\306g\364a\t\356R\031!\326!\354\004pG\325\233w`#\030"
	.ascii	"\322\340\360Xm\312\rt"
	.ascii	"<Cx\004W\214\032#\235C\201\302\016'\265\267\237\007\331\343\352"
	.ascii	"\231\252\333\331\003+l%\365\003,"
	.ascii	"N\316\317R\007\356H\337\267\b\354\006\363\372\377\303\304YT\271"
	.ascii	"*\013q\005\215\243>\226\372%\035\026"
	.ascii	"}\244S{u\030\017yyX\f\3170\001{0\371\367~%w=\2201\257\273\226"
	.ascii	"\275\275h\224i"
	.ascii	"H\031\251j\346=\335\330\314\322\300/\302dPH/\352\3754f$H\233"
	.ascii	":.JlN\034>)"
	.ascii	"\317\376\332\364F/\037\275\367\326\177\244\024\001\357|\177\263"
	.ascii	"GJ\332\375\037\323\205W\220s\244\031RR"
	.ascii	"\341\022Q\222K\023n7\240]\241\334\265x7p\0211\034F\257\211E\260"
	.ascii	"#(\003\177D\\`["
	.ascii	"L\360\347\360\306\376\351;bI\343u\236Wj\206\032\346\035\036\026"
	.ascii	"\357BU\325\275Z\314\364\376\022/"
	.string	"\211|\304 Y\200e\271\314\217;\222\f\020\360\347w\357\342\002e%\001"
	.ascii	"\356\263\256\250\316m\247$"
	.ascii	"@\307\300\337\262\"E\n\007\244\311@\177n\320\020h\366\317xA\024"
	.ascii	"\317\306\2207\244\030%{`^"
	.string	"\024\317\226\245\034C,\240"
	.ascii	"\344\323\256@-\304\343\333&\017.\200&E\322hpE\236\0233\037 "
	.ascii	"\030\030\337l\217\035\263X\242Xb\303O\247\3175n\035\346fO\377"
	.ascii	"\263\341\367\325\315l\253\254gP"
	.string	"Q\235\003\bk\177R\375\006"
	.ascii	"|\001dI\261\030\250\244%.\260\016\"\325u\003Fb\210\272|9"
	.ascii	"\347y\023\310\373\303\025x\361*\341\335 \224a\246\325\375\250"
	.ascii	"\205\370\300\251\377R\302\341\301\"@\033w"
	.ascii	"\262YY\360\2230\3010vy\251\351\215\241:\342&^\035r\221\324/\""
	.ascii	":lnv \3239#"
	.ascii	"\247/:Q\206\331}\330\b\317\324\371q\233\254\365\263\203\242\036"
	.ascii	"\033\303k\320v\032\227\031\222\030\0323"
	.ascii	"\257ru\235:/Q&\236J\007h\210\342\313[\304\367\200\021\301\301"
	.ascii	"\355\204{\246I\366\237a\311\032"
	.ascii	"\306\200O\373Eo\026\365\317u\307a\336\3076\234\034\331A\220\033"
	.ascii	"\350\324\343!\376\275\203k|\0261"
	.ascii	"h\020KRB8+\362\207\351\234\356;4hP\310PbJ\204q\235\374\021\261"
	.ascii	"\b\03746$a"
	.string	"8&-\032\343Ic\2135\375\323\233"
	.ascii	"\267\337\235\244k\240\243\270\361\213\177E\004\331x1\252\"\025"
	.ascii	"\215\211N\207\333A\235\331 \334\007l\361\245\376\t\274\233\017"
	.ascii	"\320g,=y@\377^\2360\342\353F"
	.ascii	"8IaiS/8,\020m-\267\232@\376\332'\362F\266\2213\310\350l0$\005"
	.ascii	"\365p\376E"
	.ascii	"\221\024\225\310 I\362b\242\fc?\310\007\360\005\270\324\311\365"
	.ascii	"\322E\273oE\"z\265m\237a\026"
	.ascii	"\214\013\f\226\246uH\332 /\016\357v\320h[\324\217\013=\317Q\373"
	.ascii	"\007\324\222\343\240#\026\215B"
	.ascii	"\375\b\243\001DJO\b\254\312\245v\303\031\"\250}\274\321CF\336"
	.ascii	"\270\336\3068\275`-Y\201\035"
	.ascii	"\350\305\205{\237\266e\207\262\272h\321\213g\360o\233\0173\035"
	.ascii	"|\347p:|\216\257\260Qm_:"
	.ascii	"_\254\r\246V\2076aW\334\253\353j/\340\027}\017\316L-?\031\177"
	.ascii	"\360\334\354\211wJ# "
	.ascii	"R\262xq\266\r\322v`\321\036\325\3714\034\007p\021\344\263 J*"
	.ascii	"\366f\343\377<5\202\326|"
	.ascii	"\363\364\254h`\315e\246\323\343\327<\030-\331B\331%`3\2358YW"
	.ascii	"\377\330,+;%\360>"
	.string	"\266\372\207\330[\244\341\013n;@\2722j\204*"
	.ascii	"`n\351\022\020\222\331C\t\334;\206\3108("
	.ascii	"0PFJ\317\260k\321\253w\305\025AkI\372\235A\253\364\212\256\317"
	.ascii	"\202\022(\250\006\246\270\334!"
	.string	"\2721w\276\372"
	.ascii	"\215\232\211\030\236b~`\003\202\177\331\363C7\002\314\262\213"
	.ascii	"gol\277\r\204]"
	.ascii	"\310\237\235\214F\004`\\\313\243*\324n\t@%\234/\356\022LM[\022"
	.ascii	"\253\035\243\224\201\320\303\013"
	.ascii	"\213\341\2370\r8np\307e\341\271\246-\260n\253 \256}\231\272\273"
	.ascii	"W\335\226\301*#vB:"
	.ascii	"\313~D\333r\301\370;\275-(\306\037\304\317_\376\025\252u\300"
	.ascii	"\377\254\200\371\251\341$\350\311p\007"
	.string	"\372\204p\212,CBKE\345\271\337\343\031\212\211]\344X\234!"
	.ascii	"\237\276\321\353m\241\316w\361\037"
	.ascii	"\375\265\265E\232\331a\317$y:\033\351\204\t\206\211>>0\031\t"
	.ascii	"0\347\036\013PA\375d\3629"
	.ascii	"\341{\t\376\253J\233\321)\031\340\337\341\374m\244\377\361\246"
	.ascii	",\224\b\311\303N\3615,'!\306e"
	.ascii	"\234\342\347\333\0274\255\247\234\023\234+j7\224\275\251{Y\223"
	.ascii	"\216\033\351\240@\230\210h4\327\022\027"
	.ascii	"\335\2231\316\370\211+\347\273\300%\241V3\020M\203\376\034.="
	.ascii	"\251\031\004r\342\234\261\n\200\371\""
	.ascii	"\254\375n\232\335\237\002BAI\2454\276\316\022\271{\363\275\207"
	.ascii	"\271d\017d\264\312\230\205\323\244qA"
	.ascii	"\313\370\236>\2126Z`\025GP\245\"\300\351\343\217$$_\260H=U\345"
	.ascii	"&vd\315\026\364\023"
	.string	"\214L\311\231\252X'\372\007\270"
	.string	"\260oo"
	.ascii	"#\222S\332\255\335\221\322\373\253\321KW\372\024\202P"
	.ascii	"\326\003\320S\273\025\032Fe\311\363\274\210(\020\262Z:hluv\305"
	.ascii	"'G\264l\310\244Xw:"
	.ascii	"K\376\326>\025i\002\302\304w\035Q9gZ\246\224\257\024,F&\336\313"
	.ascii	"K\247\253o\354`\371\""
	.ascii	"vP\256\223\366\021\201T\246T\375\035\337!\256\035e^\021\363\220"
	.ascii	"\214$\022\224\364\347\215_\321\237]"
	.ascii	"\036R\327\356*M$?\025\226.C(\220:\216\324\026\234.w\272d\341"
	.ascii	"\330\230\353G\372\207\301;"
	.ascii	"\177rcm\323\b\024\0033\265\307\327\357\2327jK\342\256\314\305"
	.ascii	"\217\341\251\323\276\217O\2215/3"
	.ascii	"\f\302\206\352\025\001Gm%\321Fl\313\267\212\231\210\001f:\265"
	.ascii	"2x\327\003\272o\220\316\201\rE"
	.ascii	"?t\256\034\226\330t\320\355c\034\356\365\030m\370)\355\364\347"
	.ascii	"[\305\275\227\b\261:fy\322\272L"
	.ascii	"uR \246\241\266{n\203\216<A\327!O\252\262\\\217\350U\321Vo\341"
	.ascii	"[4\246K]\342-"
	.string	"\315\037\327\240$\220\321\200\370\212(\373\n\302%\305\031d:_K\227\243\2613r"
	.ascii	"\342\357\274\177}"
	.ascii	"\224\220\302\363\305]|\315\253\005\221*\232\242\201\307X0\034"
	.ascii	"B6\035\306\200\327\324\330\334\226\321\234O"
	.ascii	"\001(k&j\036\357\372\026\237s\325\304hl\206,v\003\033\274/\212"
	.ascii	"\366\215Z\267\207^CuY"
	.ascii	"h7{j\330\227\222\031cz\321\032$X\320\320\027\f\034\\\255\234"
	.ascii	"\002\272\007\003z8\204\320\315|"
	.string	"\223\314`g\030\204\f\233\231*\263\032z"
	.ascii	"\256\315\030\332\013b\206\354\215\250D\312\220\201\204\312\223"
	.ascii	"5"
	.ascii	"\027\004&m,B\246\334\275@\202\224P=\025\256w\306h\373\264\301"
	.ascii	"\300\251S\317\320a\355\320\213B"
	.ascii	"\247\232\204^\232\030\023\222\315\372\330e5\303\330\324\321\273"
	.ascii	"\375S[TR\214\346c-\332\b\2039'"
	.ascii	"S$p\nL\016\241\271\336\033}\325fX\242\017\367\332'\315\265\331"
	.ascii	"\271\377\3753,IE),W"
	.ascii	"\023\324^C(\215\303B\311\314x2`\363P\275\357\003\332y\032\253"
	.ascii	"\007\273U3\214\276\256\227\225&"
	.ascii	"\2760\315\326E\307\177\307\373\256\272\343\323\350\337\344\f"
	.ascii	"\332]\2520\210,\242\200\312[\300\230T\230\177"
	.ascii	"cc\277\017R\025V\323\246\373M\317EZ\004\b\302\240?\207\274O\302"
	.ascii	"\356\347\022\233\326<e\3620"
	.ascii	"\027\341\013\237\210\316I8\210\242T{\033\255\005\200\034\222"
	.ascii	"\374#\237\303\243=\004\3631\nG\354\302v"
	.ascii	"\205\f\301\2528\311\b\212\313k'\333`\233\027Fp\254o\016\036\300"
	.ascii	" \251\332sdY\361s\022/"
	.ascii	"\300\013\247U\327\213H0\347B\324\361\244\265\326\006baY\274\236"
	.ascii	"\246\321\352\204\367\305\355\227\031\2548"
	.ascii	"\021\036\340\212|\3749G\237\253jJ\220tR\375.\217r\207\202\212"
	.ascii	"\331A\362i[\330*W\236]"
	.ascii	";\261Q\247\027\265f\006\214\205\233~\206\006}tI\336ME\021\300"
	.ascii	"\254\254\234\346\351\277\234\315\337\""
	.ascii	"\241\340;\020\264Y\354Vi\371Y\322\354\272\343.2\315\365\023\224"
	.ascii	"\262|yr\344\315$x\207\351\017"
	.ascii	"\331\f\r\303\340\322\333\2153C\273\254_f\216\255\037\226*2\214"
	.ascii	"%k\217\307\301HT\300\026)k"
	.ascii	";\221\272\n\3214\333~\016\254m.\202\315\243N\025\370xe\377=\b"
	.ascii	"f\027\n\360\1770?0L"
	.string	""
	.ascii	"E\331\rX\003\374)\223\354\273o\244z\322\354\370\247\342\302_"
	.ascii	"\025\n\023\325\241\006\267\032\025kA"
	.ascii	"\205\214\262\027\326;\n\323\352;w9\267w\323\305\277\\j\036\214"
	.ascii	"\347\306\306\304\267*\213\367\270a\r"
	.ascii	"\2606\301\351\357\327\250V K\344X\315\345\007\275\253\340W\033"
	.ascii	"\332/\346\257\322\350wB\367*\032\031"
	.string	"\373\016FOC+\346\237\326\0076\246\324\003\323\336$\332\240\267\016!R\360\223[T"
	.ascii	"\276}~#"
	.ascii	"1\024<\305K\367\026\316\336\355r \316%\227+\347>\262\265o\303"
	.ascii	"\271\270\b\311\\\013E\016.~"
	.ascii	"0\264\001g\355u5\001\020\375\013\237\346\224\020#\"\177\344\203"
	.ascii	"\025\0172u\343U\021\261\231\246\257q"
	.string	"\326P;G\034<B\352\020\3578;\037z\350Q\225\276\311\262_\277\204\233\034\232\370x\274\037s"
	.ascii	"\035\266S9\233o\316e\346A\241\257\3529X\306\376Y\367\251\375"
	.ascii	"_C\017\216\302\261\302\351B\021\002"
	.ascii	"\200\030\370H\030\3070\344\031\301\316^\"\f\226\277\343\025\272"
	.ascii	"k\203\340\332\266\bX\341G3oML"
	.string	"p\031\217\230\374\335\f/\033\365\271\260'b\221k\276v\221w\304\266\307n\250\237\217\250"
	.ascii	"\225\2778"
	.ascii	"\311\037}\301\317\354\367\030\024<@Q\246\365ul\337\f\356\367"
	.ascii	"+q\336\333\"z\344\247\252\335?\031"
	.ascii	"o\207\3507<\311\322\037,F\321\030Z\036\366\242v\022$9\202\365"
	.ascii	"\200PiI\r\277\236\271oj"
	.ascii	"\306#\344\266\265\"\261\356\216\377\206\362\020p\235\223\214"
	.ascii	"]\317\035\203*\251\220\020\353\305B\237\332o\023"
	.ascii	"\353U\bV\273\301Fj\235\360\223\3708\273\026$\301\254q\2177\021"
	.ascii	"\035\327\352\226\030\243\024i\367u"
	.ascii	"\321\275\005\243\261\337L\371\b,\370\237\235K6\017\212X\273\303"
	.ascii	"\245\330\207*\272\334\350\013Q\203!\002"
	.ascii	"\177z0C\001qZ\235_\244}\304\236\336c\260\323z\222\276R\376\273"
	.ascii	"\"lB@\375A\304\207\023"
	.ascii	"\024-\255^8f\367J0X|\312\200\330\216\240=\036!\020\346\246\023"
	.ascii	"\r\003l\200{\341\034\007j"
	.ascii	"\370\212\227\207\321\303\323\265\023D\016\177=Z+r\240|G\273H"
	.ascii	"H{\r\222\334\036\257j\262q1"
	.ascii	"\321G\212\262\330\267\r\246\361\244p\027\326\024\277\246X\275"
	.ascii	"\335S\223\370\241\324\351CB4cJQl"
	.ascii	"\250LV\227\2201/\251\031\341u\"L\270{\377PQ\207\2447\376UOZ\203"
	.ascii	"\360<\207\324\037\""
	.ascii	"Ac\025:O \"#-\003\n\272\351\340s\373\016\003\017AL\335\340\374"
	.ascii	"\252J\222\373\226\245\332H"
	.ascii	"\223\227L\310]\035\366\024\006\202A\357\343\371A\231\254wb4\217"
	.ascii	"\270\365\315\251y\212\016\3727\310X"
	.ascii	"\307\234\245\\f\216\312n\240\2548.K%G\250\316\027\036\322\b\307"
	.ascii	"\2571\367J\330\312\374\326mg"
	.ascii	"X\220\374\226\205h\371\f\033\240V{\363\273\334\035j\3265I}\347"
	.ascii	"\302\334\n\177\245\306\362sO\034"
	.ascii	"\2044|\374npn\263a\317\301\303\264\311\337s\345\307\034x\311"
	.ascii	"y\035\353\\g\257}\333\232Ep"
	.ascii	"\273\240_0\275Oz\016\255c\306T\340L\235\202H8\343/\203\303!\364"
	.ascii	"BL\366\033\r\310Zy"
	.ascii	"\263+\264\221I\333\221\033\312\334\002K#\226&W\334x\214\037\345"
	.ascii	"\236\337\237\323\037\342\214\204b\341_"
	.ascii	"\b\262|]-\205y(\347\362}hp\335\336\270\221xh!\253\377\013\334"
	.ascii	"5\252}gC\300D+"
	.string	"\032\226\224\341O!YNO\315q\r\307}\276I-\362P;\322\317"
	.ascii	"\2232r\221\374F\324\211G"
	.ascii	"\216\267N\007\253\207\034\032g\364\332\231\216\321\306\372g\220"
	.ascii	"OH\315\273\254>\344\244\271+\357.\305`"
	.ascii	"\021m\256|\302\305+p\253\214\244T\233i\307D\262.I\272V@\274\357"
	.ascii	"mg\266\331Hr\327p"
	.ascii	"\361\213\375;\274\211]\013\032U\363\3117\222k\260\365(0\325\260"
	.ascii	"\026L\016\253\312\317,1\234\274\020"
	.ascii	"[\240\302>K\350\212\252\340\201\027\355\364\236i\230\321\205"
	.ascii	"\216p\344\023Ey\023\364v\251\323[uc"
	.ascii	"\267\254\361\227\030\020\307=\330\273e\301^}\332]\017\002\241"
	.ascii	"\017\234[\216PV*\3057\027uc'"
	.ascii	"S\b\321*>\240_\265i5\346\236\220uo5\220\270i\276\375\361\371"
	.ascii	"\237\204o\301\213\304\301\214\r"
	.ascii	"\251\031\264n\323\002\224\002\245`\264w~N\264\360VI<\3240b\250"
	.ascii	"\317\347f\321z\212\335\302p"
	.ascii	"\023~\355\270}\226\324\221z\201v\327\n/%td%\205\r\340\202\t\344"
	.ascii	"\345<\245\0268a\2702"
	.ascii	"\016\354o\237P\224ae\215Q\306F\251~.\356\\\233\340g\363\3013"
	.ascii	"\227\225\204\224cc\254\017."
	.ascii	"d\315H\344\276\367\347y\320\206x\bg:\310j.\333\344\240\331\324"
	.ascii	"\237\370AOZs\\!yA"
	.ascii	"4\315k(\2713\256\344\334\326\235U\266~\357\267\037\216\323\263"
	.ascii	"\037\024\213'\206\302A\"f\205\3721"
	.ascii	"*\355\334\327\347\224p\214p\234\323G\303\212\373\227\002\331"
	.ascii	"\006\2513\340;\341v\235\331\f\243D\003p"
	.ascii	"\364\"6.Bl\202\257-P3\230\207) \301#\2218+\341\267\301\233\211"
	.ascii	"$\225\251\022#\273$"
	.ascii	"k\\\370\365*\f\370A\224g\372\004\303\204rh\255\033\272\243\231"
	.ascii	"\337E\211\026]\353\377\371*\035\r"
	.ascii	"\303g\3362\027\355\250\261HI\033F\030\224\264<\322\274\317vC"
	.ascii	"C\275\216\b\200\030\036\207>\356\017"
	.ascii	"\337\036b2\241\212\332\251ye\"Y\241\"\2700\223\301\232\247{\031"
	.ascii	"\004@v\035S\030\227\327\254\026"
	.string	"\255\266\207x\305\306Y\311\272\376\220_\255\236\341\224\004\365B\243bN\342\026"
	.ascii	"\027\026\030K\323N\026"
	.ascii	"=\035\233-\257r\337rZ$2\2446*Fc7\226\263\026y\240\316>\t#0\271"
	.ascii	"\366\016>\022"
	.ascii	"\232\346/\031L\331~H\023\025\221:\352,\256a'\336\244\271\323"
	.ascii	"\366{\207\353\363s\020\306\017\332x"
	.string	"\224:\fh\361\200\237\242\346\347\351\032\025~\367qsy\001HX\361"
	.ascii	"\021\335\215\263\026\263\244J\005"
	.ascii	"j\306+\345(]\361[\216\032\360p\030\343G,\335\213\302\006\274"
	.ascii	"\257\031$:\027k%\353\336%-"
	.ascii	"\270|&\031\215F\310\337\257M\345f\234x(\013\027\354nf*\035\353"
	.ascii	"*`\247}\253\246\020F\023"
	.ascii	"\025\365\321w\347e*\315\361`\252\217\207\221\211T\345\006\274"
	.ascii	"\332\274;\267\261\373\311|\251\313xHe"
	.ascii	"\376\260\366\215\307\216\023Q\033\365u\345\211\332\227S\271\361"
	.ascii	"zq\035z \tP\326 +\272\375\002!"
	.ascii	"\241\346\\\005\005\344\236\226)\255Q\022h\247\2746\025\244}\252"
	.ascii	"\027\365\032:\272\262\354)\333%\327\n"
	.ascii	"\205o\005\233\f\274\307\376\327\377\365\347hR}S\372\256\022C"
	.ascii	"b\306\257w\331\2379\002S_gO"
	.ascii	"W$N\203\261gB\334\305\033\316p\265Du\266\327^\321\367\013z\360"
	.ascii	"\032P6\240q\373\317\357J"
	.ascii	"\036\027\025\00466-\303;H\230\211\021\357+\315\020Q\224\320\255"
	.ascii	"n\n\207ae\250\242r\273\314\013"
	.ascii	"\226\022\376PL^m\030~\237\350\376\202{9\340\2601pP\305\366\307"
	.ascii	";\3027\217\020i\375xf"
	.ascii	"\310\251\261\352/\226^\030\315}\024e5\346\347\206\362m[\2731"
	.ascii	"\340\222\260>\267\326Y\253\360$@"
	.ascii	"\302chc1\372\206\025\3623-WH\214\366\007\374\256\236x\237\314"
	.ascii	"sO\001G\255\216\020\342B-"
	.ascii	"\223uS\017\r{q!L\006\036\023\013iN\221\237\340*u\256\207\266"
	.ascii	"\033n<B\233\247\363\013B"
	.ascii	"\233\322\337\224\025\023\365\227jL?1]\230Ua\020PE\b\007?\241"
	.ascii	"\353\"\323\322\270\b&kg"
	.ascii	"G+[\034e\2728\201\200\033\0331\354\266q\206\26051\274\261\f\377"
	.ascii	"{\340\361\f\234\372/]t"
	.ascii	"jN\323!W\3376`\320\263{\231'\210\333\261\372ju\310\303\t\302"
	.ascii	"\3239\310\035L\345[\341\006"
	.ascii	"\275\310\311+\036ZR\277\201\235G&\b&[\352\333U\001\337\016\307"
	.ascii	"\021\325\320\365\f\226\353<\342\032"
	.ascii	"J\2312\031\207]r[\260\332\261\316\265\03452\005\312\267\332I"
	.ascii	"\025\304}\367\301\216'a\330\336X"
	.ascii	"\250\311\302\266\250[\373-\214Y,\365\216\357\356Hs\025-\361\007"
	.ascii	"\221\2003\330[\035Ski\272\b"
	.ascii	"\\\305f\362\2237\027\330INE\314\305v\311\310\250\303&\274\370"
	.ascii	"\202\343\\\371\366\205T\350\235\363/"
	.ascii	"z\305\357\303\356>\355w\021H\377\324\027U\340\004\313q\246\361"
	.ascii	"?z=\352T\376|\224\2643\006\022"
	.ascii	"\n\020\022IG1\275\202\006\276o~m{#\336\306y\352\021\031v\036"
	.ascii	"\341\336;9\313\343;C\007"
	.string	"B"
	.ascii	"a\221x\230\224\013\350\372\353\354<\261\347N\300\244\360\224"
	.ascii	"\225s\276p\205\221\325\264\231\n\3235"
	.ascii	"\364\227\351\\\300Dy\377\243Q\\\260\344=]W|\204vZ\375\2013X\237"
	.ascii	"\332\366z\336>\207-"
	.ascii	"\201\371]N\341\002b\252\365\341\025P\027Y\r\242l\035\342\272"
	.ascii	"\323u\242\030S\002`\001\212aC\005"
	.ascii	"\t47Cd1z\025\331\201\252\364\356\267\270\372\006H\246\365\346"
	.ascii	"\376\223\260\266\247\177pT6w."
	.ascii	"\301#L\227\364\275\352\r\223F\316\235%\no\252,\272\232\242\270"
	.ascii	", \004\r\226\007-6C\024K"
	.ascii	"\313\234R\034\351T|\226\3735\306d\222&\3660e\031\022x\364\257"
	.ascii	"G'\\o\366\352\030\204\003\027"
	.ascii	"z\037n\266\307\267\304\314~/\f\365%~\025D\034\257>q\374m\360"
	.ascii	">\367c\332RgD/X"
	.ascii	"\344L2 \323{1\306\304\213H\244\350B\020\250d\023ZN\213\361\036"
	.ascii	"\262\311\215\242\315K\034*\f"
	.ascii	"Ei\275iH\201\304\355\"\215\034\276}\220m\r\253\305\\\325\022"
	.ascii	"\322;\306\203\334\024\2430\233jZ"
	.ascii	"G\004\037o\320\307M\322Y\300\207\333>\236&\262\217\322\262\373"
	.ascii	"r\002[\321wH\366\306\321\213U|"
	.ascii	"=F\226\323$\025\354\320\360$Z\303\212b\273\022\244_\274\034y"
	.ascii	":\f\245\303\257\373\n\312\245\004\004"
	.ascii	"\321oA*\033\236\274b\213YP\343(\367\306\265gi]=\330?4\004\230"
	.ascii	"\356\370\347\026uR9"
	.ascii	"\326C\247\n\007@\037\214\350^&[\313\320\272\314\336\322\217f"
	.ascii	"k\004KW3\226\335\312\375[9F"
	.string	"\234\232]\032-\333\177\021*\\"
	.ascii	"\321\274Ew\234\352o\325T\361\276\324\357\026\320\"\350)\232W"
	.ascii	"v"
	.ascii	"\3624\264R\023\265<3\341\200\336\223I(2\330\3165\ru\207(Q\265"
	.ascii	"\301w'*\273\024\305\002"
	.ascii	"\027*\300I~\216\266E\177\243\251\274\242Q\315#\033L\"\354\021"
	.ascii	"_\326>\261\275\005\236\334\204\243C"
	.ascii	"E\266\361\213\332\325KhSK\265\366~\323\213\373S\322\260\251\327"
	.ascii	"\02691Y\200Ta\t\222`\021"
	.ascii	"\315M\2336\026V8zc5\\e\247,\300u!\200\361\324\371\033\302}B\340"
	.ascii	"\346\221t}c/"
	.ascii	"\252\317\332)i\026M\264\217Y\023\204L\237R\332YU=E\312c\357\351"
	.ascii	"\013\216i\305[\022\0365"
	.ascii	"\276{\366\032F\233\264\324a\211\253\310z\003\003\326\373\231"
	.ascii	"\246\371\237\341\336q\232*\316\347\006-\030\177"
	.ascii	"\"u!\216rKE\t\330\270\204\324\364\350X\252<\220F\177M%X\323\027"
	.ascii	"R\034$C\300\254D"
	.ascii	"\354h\001\253d\216|zC\305\355\025UJZ\313\332\016\315G\323\031"
	.ascii	"U\t\260\223>4\214\254\324g"
	.ascii	"wWzO\273k}\034\341\023\203\221\324\3765\213\204Fk\311\306\241"
	.ascii	"\334J\275q\255\022\203\034mU"
	.ascii	"!\350\033\261Vg\360\201\335\363\243\020#\370\257\017]F\231jU"
	.ascii	"\320\262\370\005\177\214\3148\276z\t"
	.ascii	"\2029\215\f\343@\357\0274\372\243\025>\007\3671nds\007\313\363"
	.ascii	"!O\377N\202\035mllt"
	.ascii	"\244-\245~\207\311I\fC\035\334\233UiCL\322\353\314\367\t8,\002"
	.ascii	"\275\204\356K\243\024~W"
	.string	"+\327M\275\276\316\376\224\021\"\017\006\332Oj\364\377\321\310\300wYJ\022\225\222"
	.ascii	"\373\270\004Sp"
	.string	"\n;\247a\254h\342\360\365\245\2217\020\372\372\362\351"
	.ascii	"mk\202>\341\301B\217\327o\351~\372`"
	.ascii	"\306n)M5\035=\266\3301\255_>\005\303\363\354B\275\264\214\225"
	.ascii	"\013g\375Sc\241\f\2169!"
	.ascii	"\001V\267\264\371\252\230'r\255\215\\\023r\254^#\240\267aa\252"
	.ascii	"\316\322N}\217\351\204\262\277\033"
	.ascii	"\3633+8\212\005\365\211\264\300H\255\013\272\342Zn\263=\245\003"
	.ascii	"\265\223\217\3462\242\225\235\355\243Z"
	.ascii	"ae\331\307\351wge6\200\307rT\022+\313\356nP\331\2312\005e\314"
	.ascii	"W\211^N\341\007J"
	.ascii	"\233\244w\304\315X\013$\027\360Gd\336\3328\375\255j\310\2472"
	.ascii	"\215\222\031\201\240\257\204\355z\257P"
	.ascii	"\231\371\r\230\313\022\344Nq\307n<o\327\025\243\375w\\\222\336"
	.ascii	"\355\245\273\00241\0359\254\013?"
	.ascii	"\345[\366\025\001\336On\262\ta!!&\230)\331\326\255\013\201\005"
	.ascii	"\002x\006\320\353\272\026\243!\031"
	.ascii	"\213\301\363\331\232\255Z\327\234\301\261`\357\016jV\331\016"
	.ascii	"\\%\254\013\232>\365\307b\240\354\235\004{"
	.ascii	"\374p\270\337~/B\211\275\263vO\353k),\367M\3026\324\3618\007"
	.ascii	"\260\256s\342A\337Xd"
	.ascii	"\203DD5z\343\313\334\223\276\355\0173y\210u\207\335\305\022\303"
	.ascii	"\004`xd\016\225\302\313\334\223`"
	.ascii	"K\003\204`\276\356\336kT\270\017x\266\302\2311\225\006-\266\253"
	.ascii	"v3\227\220}d\213\311\2001n"
	.ascii	"mp\340\205\205\232\363\03739\347\263\330\245\3206;E\217q\341"
	.ascii	"\362\271C|\251'H\b\352\321W"
	.ascii	"q\260(\241\347\266z\356\252\213\250\223mY\301\2440a!\262\202"
	.ascii	"\336\264\367\030\275\227\335\235\231>6"
	.ascii	"\306\256K\342\334H\030/`\257\274\272Ur\233v1\351\357<n<\313\220"
	.ascii	"U\263\371\306\233\227\037#"
	.ascii	"\304\037\3565\301C\250\226\317\310\344\bU\263n\2270\323\214\265"
	.ascii	"\001h/\264+\005:ix\233\356H"
	.ascii	"\306\363*\314K\3361\\\037\215 \3760\260K\260f\264O\301\tp\215"
	.ascii	"\267\023$y\b\233\372\233\007"
	.ascii	"EB\325\242\200\355\311\363R9\366wx\213\240\nuT\b\321c\254m\327"
	.ascii	"kcp\224\025\373\364\036"
	.ascii	"\364\r0\332Q:\220\343\260Z\251=#d9\204\200d5\013-\361<\355\224"
	.ascii	"q\201\204\366w\214\003"
	.ascii	"\354{\026[\346^N\205\302\315\320\226B\nYY\231!\020\2304\337\262"
	.ascii	"rV\377\013J*\351^W"
	.ascii	"\001\330\244\nE\274F]\330\2713\245'\022\257\303\302\006\211+"
	.ascii	"&;\2368\033X/8~\036\n "
	.string	"\317/\030\212\220\200\300\324\275\235H\231\302p\3410\3363\367RW\275\272\005"
	.ascii	"\375\323,\021\347\324C"
	.ascii	"\305:\371\352g\271\215Q\300Rf\005\233\230\274q\365\227qV\331"
	.ascii	"\205+\3768N\036eR\312\016\005"
	.ascii	"\352h\346`v9\254\227\227\264:\025\376\273\031\233\237\247\354"
	.ascii	"4\265y\261LW\2561\241\237\300Qa"
	.ascii	"\234\f?E\336\032C\303\233;p\377^\004\365\351={\204\355\311z\331"
	.ascii	"\374\306\364X\034\302\346\016K"
	.ascii	"\226]\360\375\r\\\365:z\356\264*\340.&\335\t\027\027\022\207"
	.ascii	"\273\262\021\013\003\017\200\372$\357\037"
	.string	"\206k\2270\365\257\322\"\004F\322\302\006\270\220\215\345\272\345Ml\211\241\334\027\f4\310\346_"
	.ascii	"("
	.ascii	"\2261\247\032\373S\3267\030d\327?0\225\224\017\262\027:\373\t"
	.ascii	"\013 \255>a\310/)IMT"
	.ascii	"\210\206R4\237\272\357j\241}\020%\224\377\033\\6K\331f\315\273"
	.ascii	"[\367\372m1\017\223r\344r"
	.string	"'v*\3235\366\363\007\360fe_\206M\252zPD\320(\227\347\205<8d\340\017"
	.ascii	"\177\356\037"
	.ascii	"O\b\201\227\214 \225&\341\016E#\013*P\261\002\336\357\003\246"
	.ascii	"\256\235\375L\2433'\214.\235Z"
	.string	"\345\367\333\003\332\005Sv\275\3154\024I\362\332\244\354\210J\322\315\325J{C\005\004\356Q@\371"
	.ascii	"S\227\257\007\273\223\357\327\247f\267=\317\320>X\305\036\013"
	.ascii	"n\277\230i\316R\004\324]\322\377\267G"
	.ascii	"\2620\323\303#k5\215\006\033G\260\233\213\034\362<\270Bnl1l\263"
	.ascii	"\r\261\352\213~\234\327\007"
	.ascii	"\022\335\b\274\234\373\373\207\233\302\356\341:k\006\212\277"
	.ascii	"\301\037\333+$W\r\266K\246^\243 5\034"
	.ascii	"Y\300k!@o\250\315~\330\274\022\035#\273\037\220\t\307\027\236"
	.ascii	"j\225\264U.\321f;\fu8"
	.ascii	"J\243\313\274\246S\322\200\233!88\241\303a>\226\343\202\230\001"
	.ascii	"\266\303\220o\346\016]w\005=\034"
	.ascii	"\032\345\"\224@\361.iq\366]+<\307\300\313)\340Lt\347O\001!|H"
	.ascii	"0\323\307\342!\006"
	.string	"\363\360\333\260\226\027\256\267\226\341|\341\271\257\337T\264\243\252\351q0\222%\235."
	.ascii	"\241\234X\216]"
	.string	"\215\203Y\202\314`\230\257\334\232\237\306\301H\352\2200\036Xe7H&e\274\245\323{\t\326\007"
	.ascii	"K\251B\b\225\035\277\300>.\217Xc\303\323\262\357\342Q\2738\024"
	.ascii	"\226\n\206\277\034<x\327\203\025"
	.ascii	"\307(\235\314\004G\003\220\217\305,\367\236g\033\035&\207[\276"
	.ascii	"_+\341\026\nX\305\203N\006XI"
	.ascii	"\341z\242]\357\242\356\354t\001gU\024:|Yz\026\tf\022*\246\311"
	.ascii	"p\217\355\201._*%"
	.ascii	"\r\350fP&\224(\rk\214|0\205\367\303\374\375\022\021\fx\332S\033"
	.ascii	"\210\263C\330\013\027\234\007"
	.ascii	"V\320\325\300P\315\326\315;W\003\273mh\367\232H\357\303\363?"
	.ascii	"r\246<\314\212{1\327\300hg"
	.string	"\377o\372d\344\354\006\005#\345\005b\036C\343\276B\352\270Q$By5"
	.ascii	"\373\311J\343\005\354m"
	.string	"\263\301U\361\345%\266\224\221{{\231\247\363{A"
	.ascii	"&km\334\275,\302\364R\315\335\024^DQ"
	.ascii	"U\244\276+\253G1\211)\221\007\222O\242S\214\247\3670\276H\371"
	.ascii	"IK=\324On\b\220\351\022"
	.ascii	"QI\024;K+PW\263\274KDk\377g\216\333\205c\026'i\275\270\310\225"
	.ascii	"\222\3431o\030\023"
	.ascii	".\273\337\177\263\226\f\361\371\352\034\022^\223\232\237?\230"
	.ascii	"[:\3046\021\337\257\231>]\360\343\262w"
	.ascii	"\244\260\335\022\234c\230\325k\206$\3000\237\321\245`\344\374"
	.ascii	"X\003/|\321\212^\t.\025\225\241\007"
	.ascii	"\336\304.\234\305\251o)\313\363\204O\277a\213\274\b\371\250\027"
	.ascii	"\331\006w\034]%\323z\374\225\267c"
	.ascii	"\310_\2368\002\2176\250;\344\215\317\002;C\220C&A\305]\375\241"
	.ascii	"\2577\001/\003=\350\217>"
	.ascii	"<\321\357\350\215Lp\b17\3403\216\032\305\337\343\315`\022\245"
	.ascii	"]\235\245\206\214%\246\231\b\326\""
	.ascii	"\224\242p\005\271\025\213/IE\bgpB\362\224\204\375\273a\341Z\034"
	.ascii	"\336\007@\254\177y;\272u"
	.ascii	"\226\321\315p\300\3339b\232\212}l\213\212\376``\022@\353\274"
	.ascii	"G\210\263^\236w\207{\320\004\t"
	.ascii	"\271@\371Hf-2\3649\f-\275\f/\225\0061\371\201\240\255\227v\026"
	.ascii	"l*\367\272\316\252@b"
	.ascii	"\234\221\272\335\324\037\316\264\252\215L\307>\3331\317Q\314"
	.ascii	"\206\255c\314c,\007\336\035\274?\024\342C"
	.ascii	"\240\225\242[\234t4\370Z\3227\312[|\224\326j1\311\347\247;\361"
	.ascii	"f\254\f\264\215#\257\275V"
	.ascii	"\262;\235\301l\323\020\023\271\206#b\267k*\006\\O\241\327\221"
	.ascii	"\205\233|TW\036~P1\252\003"
	.ascii	"\35335\365\343\271*6@=\271n\325h\2053rUZ\035R\024\016\236\030"
	.ascii	"\023t\203m\250$\035"
	.ascii	"\037\316\324\377Hv\354\364\034\214\254T\360\352E\340|5\t\035"
	.ascii	"\202%\322\210YH\353\232\334a\262C"
	.string	"d\023\225l\213=Q\031{\364\013"
	.ascii	"&q\376\224g\225O\325\335\020\215\002d\t\224B\342\325\264\002"
	.ascii	"\273y\273\210\031\036[\345\2355z\301}\320\236\2403\352=`\342"
	.ascii	".,\260\302k'[\317U`2"
	.ascii	"\362\215\321(\313U\241\264\b\345l\030FF\314\352\211C\202l\223"
	.ascii	"\364\234\304\0204]\256\t\310\246'"
	.ascii	"Ti=\304\n',\315\262\312fjW>J\335l\003\327i$Y\372y\231%\214=`"
	.ascii	"\003\025\""
	.ascii	"\210\261\r\037\315\353\246\213\350[Zg:\327\3237ZX\365\025\243"
	.ascii	"\337.\362~\241`\377tq\266,"
	.ascii	"\320\341\0139\371\315\356Y\361\343\214rD B\251\364\360\224zf"
	.ascii	"\034\211\2026\364\2208\267\364\035{"
	.ascii	"\214\365\370\007\030\"._\324\t\224\324\237\\U\3430\246\266\037"
	.ascii	"\215\250\252\262=\340R\323E\202ih"
	.ascii	"$\242\262\263\340\362\222\344`\021U+\006\236l|\016{\177\r\342"
	.ascii	"\217\353\025\222Y\374X&\357\374a"
	.ascii	"z\030\030*\205]\261\333\327\254\335\206\323\252\344\363\202\304"
	.ascii	"\366\017\201\342\272D\317\001\257=GL\317F"
	.ascii	"@\201I\361\247n<!TH+9\370~\036|\272\316)V\214\303\210$\273\305"
	.ascii	"\214\r\345\252e\020"
	.ascii	"\371\345\304\236\355%eB\0033\220\026\001\332^\016\334\312\345"
	.ascii	"\313\362\247\261r@_\353\024\315{8)"
	.ascii	"W\r \337%E,\034Jg\312\277\326-;\\0@\203\341\261\347\007\n\026"
	.ascii	"\347\034O\346\230\241i"
	.ascii	"\355\312\305\3344D\001\3413\373\204<\226]\355G\347\240\206\355"
	.ascii	"v\225\001p\344\371g\322{i\262%"
	.ascii	"\274x\032\331\340\262b\220g\226P\310\234\210\311G\270pP@fJ\365"
	.ascii	"\235\277\241\223$\251\346is"
	.ascii	"dh\230\023\373?g\235\270\307]A\331\373\245<^;'\337;\314N\340"
	.ascii	"\322LN\265=h \024"
	.ascii	"\320Z\314\301o\273\3564\213\254F\226\351\f\033jS\336k\246I\332"
	.ascii	"\260\323\301\201\320aA;\3501"
	.string	"\227\321\235$\036\275x\264\002\301X^"
	.ascii	"5\fb\\\254\272\314/\323\002\373-\247\b\365\353;\266`"
	.ascii	"O+\006\236\022\307\350\227\330\n2)O\217\344I?h\030oK\341\354"
	.ascii	"[\027\003U-\266\036\317U"
	.ascii	"R\214\365}\343\265v06\314\231\347\335\271:\327 \356\023I\343"
	.ascii	"\034\203\2753\001\272b\252\373V\032"
	.ascii	"X=\302e\020\020yX\234\201\224Pm\b\235\213\247_\305\022\251/@"
	.ascii	"\342\324\221\bWde\232f"
	.ascii	"\354\311\235\\Pk>\224\0327|\247\273W%0Qv4AV\256s\230\\\212\305"
	.ascii	"\231g\203\304\023"
	.ascii	"\200\320\213]j\373\334\304BH\032W\354\304\353\336eS\345\270\203"
	.ascii	"\350\262\324'\270\345\310}\310\275P"
	.ascii	"\271\341\263ZF]:Ba?\361\307\207\301\023\374\266\271\265\354d"
	.ascii	"6\370\031\007\2667\246\223\f\370f"
	.ascii	"\021\341\337n\2037m`\331\253\021\360\025>52\226;\267%\303:\260"
	.ascii	"d\256\325_rDd\325\035"
	.string	"\232\310\272\b"
	.ascii	"\346\227\302\340\303\341\352\021\352L}|\227\347\237\341\213\343"
	.ascii	"\363\315\005\243c\017E::"
	.ascii	"}\022b3\370\177\244\217\025|\315q\304j\237\274\213\f\"ICEqn."
	.ascii	"s\237!\022Yd\016"
	.ascii	"'F9\3301/\217\007\020\245\224\336\2031\2358\200o\231\027ml\343"
	.ascii	"\321{\250\251\223\223\215\2141"
	.ascii	"\230\323\035\253)\236f];\236-4X\026\222\374\315sY\363\375\035"
	.ascii	"\205U\366\n\225%\303A\232P"
	.ascii	"\031\376\377*\003]t\362f\333$\177I<\237\f\357\230\205\272\343"
	.ascii	"\323\230\274\024S\035\232g|L\""
	.ascii	"\351%\371\246\334n\300\2753\037\033d\364\363>y\211>\203\235\200"
	.ascii	"\022\354\202\211\023\241(#\360\277\005"
	.ascii	"\344\022\305\r\335\240\201h\376\372\245D\310\r\347O@RJ\217k\216"
	.ascii	"t\037\352\243\001\356\315wbW"
	.ascii	"\013\340\312#p\02326Y\317\254\321\n\317JT\210\034\032\322I\020"
	.ascii	"t\226\247D*\372\303\214\013x"
	.ascii	"_0O#\274\212\363\036\b\336\005\024\275\177W\232\r*\3464\024\245"
	.ascii	"\202^\241\267qbr\030\364_"
	.ascii	"@\225\266\023\350G\333\345\341\020&C;*]\363v\022x8\351&\037\254"
	.ascii	"i\313\240\240\214\333\324)"
	.ascii	"\235\333\211\027\f\b\2169\365x\347\363% `\247]\003\275\006L\211"
	.ascii	"\230\372\276f\251%\334\003j\020"
	.ascii	"\320S33\257\n\255\331\345\t\323\254\245\235f8\360\367\210\310"
	.ascii	"\212eW<\372\276,\005Q\212\263J"
	.ascii	"\234\300\335_\357\321\317\326\316]W\367\375>+\350\3024\026 ]"
	.ascii	"k\325%\233+\355\004\273\306A0"
	.ascii	"\223\325hg%+|\332\023\312\"DW\300\301\230\035\316\n\312\325\013"
	.ascii	"\250\361\220\246\210\300\255\321\315)"
	.ascii	"H\341V\331\371\362\362\017.k5\237u\227\347\255\\\002l_\273\230"
	.ascii	"F\032{\232\004\024h\275K\020"
	.ascii	"c\361\177\326_\232]\251\201V\307L\235\346+\351W\362 \336L\002"
	.ascii	"\370\267\365-\007\373 *O "
	.ascii	"g\355\361h1\375\360Q\302;o\330\315\035\201,\336\362\322\004C"
	.ascii	"\\\334DIq*\tW\314\350["
	.ascii	"y\260\3530=;\024\3100.e\275Z\025\211u1\\m\2171<<e\037\026y\302"
	.ascii	"\027\373p%"
	.ascii	"Z$\270\013U\251.\031\321P\220\217\250\373\346\3105\311\244\210"
	.ascii	"-\352\206yh\206\001\336\221_\034$"
	.ascii	"u\025\266,\1776\372>l\002\326\034vo\371\365b%\265e*\024\307\350"
	.ascii	"\315\n\003S\352e\313="
	.ascii	"\252l\336@)\027\330(:s\331\"\360,\277\217\321\001[#\335\374\327"
	.ascii	"\026\345\360\315_\335\016B\b"
	.ascii	"\316\020\364\004N\303X\003\205\006n'Z[\023\266!\025\271\353\307"
	.ascii	"p\226]\234\210\333!\363T\326\004"
	.ascii	"J\372b\203\253 \377\315n>\032\342\324\030\341W+\3469\374\027"
	.ascii	"\226\027\343\375i\027\274\357S\232\r"
	.ascii	"\325\265\275\335\026\301}^-\335\245\215\266\336T)\222\24243\027"
	.ascii	"\b\266\034\327\032\231\030&OzJ"
	.ascii	"K*7\257\221\262\303$\362G\201qp\202\332\223\362\236\211\206d"
	.ascii	"\205\204\3353\356\340#B1\226J"
	.string	"\225_\261_\002\030\247\364\217\033\\k4_\366=\022\021\340"
	.ascii	"\205\360\374\315H\030\323\335L\f\265\021"
	.ascii	"\326\377\244\bD'\350\246\331v\025\234~\027\216s\362\263\002="
	.ascii	"\266H3wQ\314k\316M\316KO"
	.ascii	"o\013\235\304na\3420\027#\354\312\217qV\344\246Ok\362\233@\353"
	.ascii	"H7_Ya\345\316B0"
	.ascii	"\204%$\342Z\316\037\247\236\212\365\222Vr\352&\364<\352\034\327"
	.ascii	"\t\032\322\346\001\034\267\024\335\374s"
	.ascii	"A\254\233Dyp~B\n1\342\274m\343Z\205|\032\204_!v\256L\326\341"
	.ascii	"\234\232\ft\2368"
	.ascii	"(\254\016W\366x\275\311\341\234\221'2\013[\345\355\221\233\241"
	.ascii	"\253>\374e\2206&\326\345%\304%"
	.ascii	"\316\271\3344\256\263\374d\255\320H\343#\003P\227\0338\306b}"
	.ascii	"\360\263E\210gZFySTa"
	.ascii	"n\336\327\361\246\006>?\b#\006\216'v\371>wl\212N&\366\024\214"
	.ascii	"YGH\025\211\2409e"
	.ascii	"\031J\273\024\324\333\304\335\216OB\230<\274\262\031iq\3126\327"
	.ascii	"\237\250H\220\275\031\360\0162e\017"
	.ascii	"s\367\322\303t\037\322\351Eh\304%ATP\3013\236\271\371\350\\N"
	.ascii	"bl\030\315\305\252\344\305\021"
	.ascii	"\306\340\375\312\261\321\206\324\201Q;\026\343\346?O\232\223"
	.ascii	"\362\372\r\257\250Y*\0073\354\275\307\253L"
	.ascii	"\211\322x?\217x\217\300\237M@\241,\2470\376\235\314e\317\374"
	.ascii	"\213w\362! \313Z\026\230\344~"
	.ascii	".\n\234\b$\226\236#8G\376:\300\304H\307*\241Ov*\355\333\027\202"
	.ascii	"\205\0342\360\223\233c"
	.ascii	"\303\241\021\221\343\b\325{\211t\220\200\324\220++\031\375r\256"
	.ascii	"\302\256\322\347\246\002\266\205<I\337\016"
	.ascii	"\023Av\204\322\304gg5\370\365\367?@\220\240\336\276\346\312\372"
	.ascii	"\317\217\034i\243\337\321T\f\300\004"
	.ascii	"hZ\233YX\201\314\256\016\342\255\353\017OW\352\007\177\266\""
	.ascii	"t\035\344O\264O\235\001\343\222;@"
	.ascii	"\370\\F\213\201/\302M\370\357\200\024Z\363\240qW\326\307\004"
	.ascii	"\255\277\350\256\364va\262*\261[5"
	.ascii	"\030s\214Z\307\332\001\243\021\252\316\263\235\003\220\355-?"
	.ascii	"\256;\277|\007o\216\255R\340\370\352\030u"
	.ascii	"\364\273\223t\314d\036\247\303\260\243\354\331\204\275\345\205"
	.ascii	"\347\005\372\f\305k\n\022\303.\0302\201\233\017"
	.ascii	"2l\177\033\304Y\210\244\23028\364\274`-\017\331\321\261\311)"
	.ascii	"\251\025\030\304U\027\273\033\207\303G"
	.ascii	"\260fP\310P]\346\373\260\231\242\263\260\304\354b\340\350\032"
	.ascii	"D\352T7\345_\215\324\350,\240\376\b"
	.string	"HO\354q\227SDQn]\214\311}\261\005\370k\306\303G\032\301b\367\334\231Fv\205\233\270"
	.ascii	"\320\352\336hv\335M\202#]hK Ed\310e\326\211]\315\317\024\265"
	.ascii	"7\325uO\247)8G"
	.ascii	"\311\0029\255:S\331#\217X\003\357\316\335\302d\264/\341\317\220"
	.ascii	"s%\025\220\323\344DM\213fl"
	.ascii	"\030\304yFu\332\322\202\360\215a\262\330\327;\346\n\353G\254"
	.ascii	"$\357^5\264\3063HLhx "
	.ascii	"\f\202xz!\317H;\227>'\201\262\nj\367{\355\216\214\247el\251?"
	.ascii	"C\212O\005\246\021t"
	.ascii	"\264u\261\030=\345\232W\002\241\222\363Y1qh\3655\357\036\272"
	.ascii	"\354U\204\2179\214Er\250\311\036"
	.ascii	"m\310\235\2712\235eM\025\361:`u\334L\004\210\344\302\334,qL\263"
	.ascii	"\3774\201\373te\023|"
	.string	"\233P\242"
	.ascii	"\324\244\346\270\264\202\310\013\002\327\201\233au\225\361\233"
	.ascii	"\314\347W`d\315\307\245\210\335:"
	.ascii	"F09Y\324\230\302\205\354Y\366_\2305~\217:n\366\362*\242,\035"
	.ascii	" \247\006\2441\021\272a"
	.ascii	"\362\3345\266pW\211\253\274\037l\366l\357\337\002\207\321\266"
	.ascii	"\276h\002S\205t\236\207\314\374)\231$"
	.ascii	")\220\225\026\361\240\320\243\211\275~\272lk;\002\0073x&>Z\361"
	.ascii	"{\347\354\330\273\f1 V"
	.ascii	"\326\205\342w\364\265Ff\223a\217lg\377\350@\335\224\265\253\021"
	.ascii	"s\354\246M\354\214e\363F\310~"
	.ascii	"C\3264IC\223\211R\365\"\022\245\006\370\333\271\"\034\364\303"
	.ascii	"\217\207m\2170\227\235M*jg7"
	.ascii	"\307.\242\035?\217^\233\023\315\001lw\035\017\023\270\237\230"
	.ascii	"\242\317\217L!\325\235\2339#\367\252m"
	.ascii	"\242\216\255\254\277\004;X\204\350\213\024\350C\267)\333\305"
	.ascii	"\020\b;X\036+\252\273\263\216\345IT+"
	.string	"G\276=\353bu:_\270\240\275\216T8\352\367\231rtE1\345\303"
	.ascii	"Q\325'\026\347\351\004\023"
	.ascii	"\376\234\334j\322\024\230x\013\335H\213?\253\033<\n\306y\371"
	.ascii	"\377\341\017\332\223\326-|-\336hD"
	.ascii	"\316\007c\370\306\330\232K(\f]C15\021!,wze\305f\250\324Rs$c~"
	.ascii	"B\246]"
	.ascii	"\236F\031\224^5\273QT\307\335#L\334\3463b\231\177D\326\266\245"
	.ascii	"\223c\275D\373o|\316l"
	.ascii	"\312\"\254\336\210\306\224\032\370\037\256\273\367n\006\271\017"
	.ascii	"XY\2158\214\255\210\250,\237\347\277\232\362X"
	.ascii	"\366\315\016q\277dZK<),F8\345L\261\271:\013\325V\320C6pH[\030"
	.ascii	"$7\371j"
	.ascii	"h>\347\215\253\317\016\351\245v~7\237o\003T\202Y\001\276\013"
	.ascii	"[I\3606\036\364\247\304)vW"
	.ascii	"\210\250\306\tE\002 2s\211UK\0236\340\322\237(3<#6\342\203\217"
	.ascii	"\301\256\f\273%\037p"
	.ascii	"\023\301\276|\331\366\030\235\344\333\277t\346\006J\204\326`"
	.ascii	"N\254\"\265\365 Q^\225P\300[\nr"
	.string	"\355la\344\370\260\250\303}\250%\236\016f"
	.ascii	"\367\234\245\274\364\037\006\343a\351\013\304\275\277\222\f."
	.ascii	"5Z\200\233C\t?\f\374\253Bb7\213N\350F\223\"\\\363\027\024i\354"
	.ascii	"\360N\024\273\234\233\016"
	.ascii	"\356\276\261]\325\233\356\215\271?r\n7\253\303\311\221\327h\034"
	.ascii	"\277\361\250D\336<\375\034\031Dm6"
	.ascii	"\255 W\373\217\324\272\373\016\r\371\333k\221\201\356\277CUc"
	.ascii	"R1\201\324\330{3?\353\004\021\""
	.ascii	"\024\214\274\362C\027<\236;l\205\265\374&\332.\227\373\247h\016"
	.ascii	"/\270\314D2Y\274\346\244gA"
	.ascii	"\356\217\316\370e&\276\302,\326\200\350\024\377g\351\356N6/~"
	.ascii	"n.\361\366\322~\313p3\2634"
	.string	""
	.ascii	"'\366v(\235;d\353hv\016@\235\035]\204\006\374!\003CK\033j$U\""
	.ascii	"~\2738y"
	.ascii	"\314\326\201\206\356\221\305\315S\247\205\355\234\020\002\316"
	.ascii	"\203\210\200X\301\205t\355\344e\376-n\374v\021"
	.ascii	"\270\016wI\211\342\220\333\243@\364\254*\314\373\230\233\207"
	.ascii	"\327\336\376O5!\266\006i\362T>j\037"
	.ascii	"\233a\234[\320l\257\264\200\204\245\262\364\311\337-\304M\351"
	.ascii	"\353\002\245O=4_}gL:\374\b"
	.string	"\3524\007\323\231\301\244`\326\\\0261\266\205\300@\225\202Y\367#>3\342\321"
	.ascii	"\271\026\001\255/O"
	.ascii	"8\266;\267\035\331,\226\b\234\022\374\252w\005\346\211\026\266"
	.ascii	"\3639\233ao\201\356D)_\231Q4"
	.ascii	"TN\256\224A\262\276Dl\357W\030Q\034T_\230\004\2156-k\036\246"
	.ascii	"\253\367.\227\244\204TD"
	.ascii	"|}\352\237\320\374R\221\366\\\223\260\224l\201J@\\(G\252\232"
	.ascii	"\216%\267\223(\004\246\234\270\020"
	.ascii	"n\360EZ\276A9ue_\234m\355\256|\320\266Q\377r\234kw\021\251M\r"
	.ascii	"\357\331\321\322\027"
	.ascii	"\234(\030\227IGY=&?S$\305\370\353\022\025\357\303\024\313\277"
	.ascii	"b\002\216Q\267w\325x\270 "
	.string	"j>?\007\030\257\362'i\020R\327\031\345?\375\""
	.ascii	"\246<,\267\343\"\247\306e\314cO!r"
	.ascii	"\311);\364\271\267\235\035u\217QOJ\202\005\326\304\235/1\275"
	.ascii	"r\300\362\260E\025Z\205\254$\037"
	.ascii	"\223\246\007S@\177\343\264\225g3/\327\024\247\253\231\020vs\247"
	.ascii	"\320\373\326\311\313q\201\305H\337_"
	.string	"\252\005\225\2162\b\326$\356 \024\f\321\301HG\242%\373\006\\\344\377\307\346\225\343*\236s\272"
	.ascii	"&\273\210\352\365&D\256\373;\227\204\331y\0066PNi&\f\003\237"
	.ascii	"\\&\322\030\325\347})r"
	.ascii	"\326\220\207\\\336\230.Y\337\242\302E\323\267\277\345\"\231\264"
	.ascii	"\371`;Z\021\363x\255g>:(\003"
	.ascii	"9\271\f\276\307\035$H\2000c\213M\233\3612\b\223(\002\r\311\337"
	.ascii	"\323E\031'Fh)\341\005"
	.ascii	"PE,$\310\273\277\255\331\2010\320\354\f\310\274\222\337\310\365"
	.ascii	"\246f5\204L\316X\202\323%\317x"
	.ascii	"ZI\234-\263\356\202\272|\271+\361\374\310\357\316\340\321\265"
	.ascii	"\223\256\253-\260\233\215i\023\234\f\3009"
	.string	"h\235H1\216k\256\025\207\360+\234\253\034\205\252\005\372N\360\227Z\247\3112\370?k\007Rk"
	.ascii	"-\b\316\271\026~\313\365)\274zAL\361\0074\253\247\364+\316k\263"
	.ascii	"\324\316u\237\032V\351\342}"
	.ascii	"\034x\225\235\341\317\340)\342\020c\226\030\337\201\2669kQp\323"
	.ascii	"9\337W\"a\307;D\343WM"
	.ascii	"\313^\245\266\364\324p\336\231\333\205]\177R\001H\201\232\356"
	.ascii	"\323@\304\311\333\355)`\032\257\220*k"
	.ascii	"\n\330\262[$\363\353w\233\007\271/G\0330\3303s\356L\362\346G"
	.ascii	"\306\t!l'\310\022XF"
	.ascii	"\227\036\346\232\374\364#i\321_?\340\035(5W-\321\355\346C\256"
	.ascii	"d\247J>-\321\351\364\330_"
	.ascii	"\331b\020*\262\276CM\026\33418u\373ep\327h)\336{J\r\030\220g"
	.ascii	"\261\034+,\263\005"
	.ascii	"\225\201\325z,\244\374\367\314\3633Cn(\0242\235\227\0134\r\235"
	.ascii	"\302\266\341\007sVH\032w1"
	.ascii	"\375\250M\322\314^\300\310\203\357\337\005\254\032\317\241a\315"
	.ascii	"\371}\362\357\276\333\231\036G{\243VU;"
	.ascii	"\202\324M\341$\305\2602\266\244+\032TQ\263\355\363Z+(H`\321\243"
	.ascii	"\3536sz\322y\300O"
	.ascii	"\r\305\206\fD\2134\334Q\346\224\314\311\3137\023\271<>dM\367"
	.ascii	"\"d\b\315\343\272\302p\021$"
	.ascii	"\177/\277\211\2608\311Q\247\351\337\002e\275\227$S\344\200x\234"
	.ascii	"\300\377\377\222\216\371\312\316gE\022"
	.string	"\264s\304\n\206\253\371?5\344\023\001\356\035\221\360\257\304\306\353`P\347J\r"
	.ascii	"\207l\226\022\206?"
	.ascii	"\023\215\0046\372\374\030\234\335\235\211s\263\235\025)\252\320"
	.ascii	"\222\237\0135\237\334\324\031\212\207\356~\365&"
	.ascii	"\336\r*x\311\f\232U\205\203q\352\262\315\035U\214#\3571[\206"
	.ascii	"b\177=asyv\247JP"
	.ascii	"\261\357\207V\325,\253\f{\361z$b\321\200Qg$ZO4Z\301\205i0\272"
	.ascii	"\235=\224A@"
	.string	"\335\252l\242Cw!K\316\267\212d$\264\246G\343\311\373\003zO\035\313\031\320"
	.ascii	"\230B1\331\022"
	.ascii	"\226\314\353C\272\356\300\303\257\234\352&\234\234t\215\306\314"
	.ascii	"w\034\356\225\372\331\0174\204v\331\241 \024"
	.string	"OY7\323\231w\306"
	.ascii	"{\244:\262@Q<^\225\363_\343T(\030D\022\240YC1\222O\033"
	.ascii	"\261f\230\24400\3173YH_!\322s\037%\366\364\336Q@\252\202\253"
	.ascii	"\366#\232o\325\221\361_"
	.ascii	"Q\t\025\211\235\020\\>ji\351-\221\372\3169 0_\227?\344\352 \256"
	.ascii	"-\023\177*W\233#"
	.ascii	"h\220-\2543\324\236\201#\205\311_y\253\203(=\353\223U\200rE\357"
	.ascii	"\3136\217ujR\f\002"
	.ascii	"\211\314B\360Y\3571\351\266K\022\216\235\234X,\227Y\307\256\212"
	.ascii	"\341\310\255\f\305\002V\n\376,E"
	.ascii	"\274\333\330\236\3704\230wl\244|\334\371\252\362\310t\260\341"
	.ascii	"\243\334LR\251w81\025F\314\252\002"
	.ascii	"\337wxd\240\367\240\206\237|`\016'd\304\273\311\021\373\361%"
	.ascii	"\352\027\253{\207K0{}\373L"
	.ascii	"\022\357\211\227\302\231\206\342\r\031W\337q\315n+\320p\311\354"
	.ascii	"W\310C\303\305:MC\274L\035["
	.ascii	"\376u\233\270l=\264r\200\334j\234\331\224\306T\237L\343>7\252"
	.ascii	"\303\270dS\0079+b\264\024"
	.ascii	"&\237\n\314\025&\373\266\345\314\215\270+\016O:\005\247i3\213"
	.ascii	"I\001\023\321-YX\022\367\230/"
	.ascii	"\001\247TOD\256\022.\336\327\313\251\360>\376\374\340]\203u\r"
	.ascii	"\211\277\316TEa\347\351b\200\035"
	.ascii	"V\236\017\265L\247\224\f \023\216\216\251\364\037[g\0170\202"
	.ascii	"!\314*\232\371\252\006\330I\342j:"
	.ascii	"Z|\220\251\205\332zeb\017\271\221\265\250\016\032\351\2644\337"
	.ascii	"\373\035\016\215\363_\362\256\350\214\213)"
	.ascii	"\336e!\n\352rz\203\366y\317\013\264\007\253?p\2568w\3076\026"
	.ascii	"R\334\327\247\003\030'\246k"
	.ascii	"\262\f\367\357Sy\222*vp\025y*\311\211Kj\317\2470zE\030\224\205"
	.ascii	"\344\\M@\250\2704"
	.ascii	"53i\203\265\354n\302\375\376\265c\337\023\250\325s%\262\244\232"
	.ascii	"\252\223\242j\034^F\335+\326q"
	.ascii	"\365^\367\261\332\265-\315\365e\260\026\317\225\177\327\205\360"
	.ascii	"I?\352\037W\024=++&!63\034"
	.ascii	"\200\337x\323(\3143e\264\244\017\nyC\333\366Z\332\001\367\371"
	.ascii	"_d\343\244+\027\363\027\363\325t"
	.ascii	"\201\312\331gT\345o\2507\214)+u|\2139;b\254\343\222\bm\332\214"
	.ascii	"\331\351GE\314\353J"
	.ascii	"\020\266Ts\236\215@\013n[\250[S2k\200\007\242XJ\003:\346\333"
	.ascii	",\337\241\311\335\331;\027"
	.ascii	"\311\001m'\033\007\360\022p\214\304\206\305\272\270\347\251\373"
	.ascii	"\326q\233\022\bS\222\267=Z\371\373\210]"
	.ascii	"\337rX\376\036\017P+\301\0309\324.X\326X\340:g\311\216'\355\346"
	.ascii	"\031\243\236\261\023\315\341\006"
	.ascii	"S\003[\236b\257+GG\004\215'\220\013\252;'\277C\226F_x\f\023{"
	.ascii	"\203\215\032j:\177"
	.ascii	"#o\026oQ\255\320@\276j\253\037\2232\216\021\216\bM\240\024^\343"
	.ascii	"?fb\341&5`\2000"
	.ascii	"\013\200=]9D\346\367\366\355\001\311U\325\250\2259c,Y0x\315h"
	.ascii	"~0Q.\355\375\3200"
	.ascii	"PG\270h\036\227\264\234\317\273df)r\225\240+A\372r&\347\215\\"
	.ascii	"\331\211\305QC\b\025F"
	.ascii	"\2633\022\362\032MY\340\234M\314\360\216\347\333\033w\232I\217"
	.ascii	"\177\030eih\230\t, \024\222\n"
	.ascii	".\240\271\256\300\031\220\274\256L\003\026\r\021\307U\3542\231"
	.ascii	"e\001\365m\016\376]\312\225(\r\312;"
	.ascii	"\277\001\314\236\266\216h\234o\211D\246\255\203\274\360\342\237"
	.ascii	"z__\225-\312A\202\362\215\003\264\250N"
	.ascii	"\244b]<\2741\360@`z\360\317>\213\374\031E\265\017\023\242=\030"
	.ascii	"\230\315\023\217\256\335\3361V"
	.ascii	"\002\322\312\361\nF\355*\203\356\214\244\005S0F_\032\361IEw!"
	.ascii	"\221c\244,T0\t\316$"
	.ascii	"\205\013\363\375U\241\317?\244.76\216\026\367\322D\370\222d\336"
	.ascii	"d\340\262\200BO2\247(\231T"
	.ascii	"\006\301\006\375\365\220\350\037\362\020\210]5h\304\265>\257"
	.ascii	"\214n\376\bx\202K\327\006\212\302\343\324A"
	.ascii	".\032\356c\2472n\362\352\375_\322\267\344\221\256iM\177\321;"
	.ascii	"\323;\274j\377\334\300\336f\033I"
	.ascii	"\241d\332\320\216J\360uK(\342g\257,\"\355\244{{\037y\2434\202"
	.ascii	"g\213\001\267\260\270\366L"
	.ascii	"\2472\352\307=\261\365\230\230\333\026~\314\370\325\343G\331"
	.ascii	"\370\313R\277\n\254\254\344^\310\3208\363\b"
	.ascii	"\275s\032\231!\250\203\303z\f2\337\001\274'\253cpw\204\0333="
	.ascii	"\301\231\212\007\353\202J\rS"
	.string	"\236\277\232lEsim\200\250"
	.ascii	"I\374\262\177%P\270\317\310\022\364\254+[\275\277\f\340\347\263"
	.ascii	"\r"
	.string	"%H\371\34106L"
	.ascii	"ZS\253\214&x-~\213\377\204\314##H\307\271p\027\020?u\352e"
	.ascii	"cc\t\342>\374f=k\313\265a\177,\326\201\032;D\023B\004\276\017"
	.ascii	"\333\241\341!\031\354\244\002"
	.ascii	"_y\317\361ba\310\365\362W\356&\031\206\214\021x5\006\034\205"
	.ascii	"$!\027\317\177\006\354]+\3216"
	.ascii	"\242\270$;\232%\346\\\270\240\257E\314zW\2707p\240\213\350\346"
	.ascii	"\313\314\277\tx\022Q<\024="
	.ascii	"WE\025y\221'm\022\n:x\374\\\217\344\325\254\233\027\337\350\266"
	.ascii	"\2756Y(\250[\210\027\365."
	.string	"Q/[0\373\277\356\226\270\226\225\210\2558\371\323%\335\325F\307-\365\360\225"
	.ascii	":\273\220\202\226W"
	.ascii	"\334\256X\214N\2277F\244A\360\253\373\"\357\271\212q\200\351"
	.ascii	"V\331\205\341\246\250C\261\372x\033/"
	.ascii	"\001\341 \nC\270\032\367G\354\360$\215e\223\363\321\356\342n"
	.ascii	"\250\tu\317\341\243*\3345>\304}"
	.ascii	"\030\227>'\\*xZ\224\375N^\231\306v5>}#\037\005\330.\017\231\n"
	.ascii	"\325\202\035\270O\004"
	.ascii	"\303\331}\210ef\226\205US\260K1\233\017\311\261y \357\370\215"
	.ascii	"\340\306/\301\214u\026 \367~"
	.ascii	"\331\343\007\251\305\030\337\301YcL\316\0357\263WI\273\001\262"
	.ascii	"4Ep\312.\3350\234?\202y\177"
	.string	"\272\207\365h\360\037\234j\336\310P"
	.ascii	"N\211'\b\347[\355}U\231\277<\360\326\006\034C\260\251d"
	.ascii	"\350\023\265\2439\3224\203\330\250\037\271\324p6\3013\275\220"
	.ascii	"\3656A\265\022\264\331\204\327s\003N\n"
	.ascii	"\031)}[\241\326\263.5\202:\325\240\366\264\260G]\244\211C\316"
	.ascii	"Vql4\030\316\n}\032\007"
	.ascii	"1D\341 R5\f\314AQ\261\t\007\225e\r6_\235 \033b\365\232\323Uw"
	.ascii	"a\367\274i|"
	.ascii	"\013\272\207\310\252-\007\323\356b\245\277\005)&\001\213v\357"
	.ascii	"\300\0020T\317\234~\352Fq\314;,"
	.ascii	"_)\350\004\353\327\360\007}\363P/%\030\333\020\327\230\027\027"
	.ascii	"\243\251Q\351\035\245\254\"s\232Zo"
	.ascii	"\276D\331\243\353\324)\347\236\257x\200@\t\236\215\003\234\206"
	.ascii	"GzV%E$;\215\356\200\226\253\002"
	.string	"\305\306A/\f"
	.ascii	"\241\213\233\373\376\f\301y\237\304\237\034\305<pG\372N\312\257"
	.ascii	"G\341\242!NI"
	.ascii	"\232\r\345\335\205\212\244\357I\242\271\017N\"\232!\331\366\036"
	.ascii	"\331\035\037\t\3724\273F\352\313v]k"
	.ascii	"\"%x\036\027A\371\340\3236i\003t\256\346\361F\307\374\320\242"
	.ascii	">\213@>1\335\003\234\206\373\026"
	.ascii	"\224\331\f\354lUW\210\272\035\320\\o\334rdw\264B\217\024i\001"
	.ascii	"\257Ts'\205\3663\343\n"
	.ascii	"b\t\2663\227\031\216(3\341\253\330\264r\374$>\320\221\t\355\367"
	.ascii	"\021Hu\320p\217\213\343\201?"
	.ascii	"$\310\027_5\177\333\n\244\231B\327\303#\271t\367\352\370\313"
	.ascii	"\213>|\325=\334\336L\323\342\323\n"
	.ascii	"\376\257\331~\314\017\221\177K\207e$\241\270\\T\004G\fK\322~"
	.ascii	"9\250\223\t\365\004\301\017QP"
	.string	"\235$n3\305\017\fo\331\3171\303\031\336^t\034\376\356\t"
	.ascii	"\375\326\362\276\036\372\360\213\025|\022"
	.ascii	"t\271Q\256\304\217\242\336\226\376Mt\323s\231\035\250H8\207\013"
	.ascii	"h@b\225\337g\321y$\330N"
	.ascii	"\242y\230.B|\031\366G6\312R\324\335J\244\313\254NK\301?A\233"
	.ascii	"hO\357\007}\370N5"
	.ascii	"u\331\305`\"\265\343\376\270\260A\353\374.5P<e\366\2510\254\b"
	.ascii	"\210m#9\005\322\222-0"
	.ascii	"w\361\340\344\266o\274-\223j\275\244)\277\341\004\350\366zx\324"
	.ascii	"f\031^`\320&\264^_\334\016"
	.ascii	"=(\244\274\242\301\023x\331=\206\241\221\360b\355\206\372h\302"
	.ascii	"\270\274\307\256L\256\034o\267\323\345\020"
	.ascii	"g\216\332S\326\277STA\366\251$\354\036\334\351#\212W\003;&\207"
	.ascii	"\277r\272\0346Ql\264E"
	.ascii	"\344\343\177\212\335M\235\3160\016bvVd\023\253X\231\016\263{"
	.ascii	"OYK\337)\0222\357\n\034\\"
	.ascii	"\241\177O1\277*@\251P\364\214\216\334\361W\342\204\276\250#K"
	.ascii	"\325\273\035;q\313m\243\277w!"
	.ascii	"\217\333y\372\274\033\b7\263Y_\302\036\201H`\207$\203\234evz"
	.ascii	"\b\273\265\212}8\031\346J"
	.ascii	"\203\373[\230D~\021a61\226q*F\340\374K\220%\324H4\254\203d=\244"
	.ascii	"[\276Zhu"
	.string	".\243DS\252\366\333\215x@\033\264\264\352\210}`\r\023J\227\353\260^\003>\277\027\033\331"
	.ascii	"\032"
	.ascii	"\262\362a\3533\t\226nRI\377\311\250\017=Tie\366z\020ur\337\252"
	.ascii	"\346\260#\266)U\023"
	.ascii	"\376\203.\342\274\026\307\365\301\205\t\350\031\353+\264\256"
	.ascii	"J%\0247\246\235\354\023\246\220\025\005\352rY"
	.ascii	"\030\325\321\255\327\333\360\030\021\037\301\317\210x\237\227"
	.ascii	"\233u\024q\360\3412\207\001:\312e\032\270\265y"
	.ascii	"\021x\217\334 \254\324\017\250OM\254\224\322\232\2324\0046\263"
	.ascii	"d-\033\300\333;_\220\225\234~O"
	.ascii	"\376\231R5=D\310q\327\352\353\333\034;\315\213f\224\244\361\236"
	.ascii	"I\222\200\310\255D\241\304\356B\031"
	.ascii	".0\201W\274Kgb\017\334\255\2119\017R\330\306\331\373S\256\231"
	.ascii	")\214L\216c.\331:\2311"
	.ascii	"\222I#\256\031S\254}\222>\352\f\221=\033,\"\021<%\224\344<Uu"
	.ascii	"\312\371N1e\n*"
	.ascii	":y\034<\315\0326\317;\2745Z\254\274\236/\253\246\315\250\351"
	.ascii	"`\350`\023\032\352m\233\303]\005"
	.ascii	"\302'\371\367\177\223\267-5\246\320\027\006\037t\333v\257U\021"
	.ascii	"\242\363\202Y\355-|d\030\342\366L"
	.ascii	"\266[\215\302|\"\031\261\253\377Mw\274N\342\007\211,\243\344"
	.ascii	"\316x<\250\266$\252\020w0\032\022"
	.ascii	"\311\203t\307>qY\326\257\226+\270w\340\277\210\323\274\227\020"
	.ascii	"#(\236(\233:\355lJ\271{R"
	.ascii	"\227J\003\237^]\333\344-\27440\t\374S\341\261\323Q\225\221F\005"
	.ascii	"F-\345@zl\307?3"
	.ascii	".H[\231*\231=V\00188n|\320\0054\345\330d/\3365PH\367\251\247"
	.ascii	" \233\006\211k"
	.ascii	"w\333\307\265\214\372\202@U\3014\307\370\206\206\006~\245\347"
	.ascii	"\366\331\310\346)\317\233c\247\b\323s\004"
	.ascii	"\r\"pbA\240*\201N[$\371\372\211Z\231\005\357rP\316\304\255\377"
	.ascii	"s\353s\252\003!\274#"
	.ascii	"\005\236X\003&y\356\312\222\304\334F\022BK+O\251\001\346t\357"
	.ascii	"\241\002\0324\004\336\277s/\020"
	.ascii	"\232\034Q\265\340\332\264\242\006\377\377+)`\310z4BP\365]7\037"
	.ascii	"\230-\241N\332%\327k?"
	.ascii	"\306EW\177\253\271\030\353\220\306\207W\356\212:\002\251\257"
	.ascii	"\367-\332\022'\267=\001\\\352%}Y6"
	.ascii	"\254X`\020{\215Ms_\220\306o\236W@\331-\223\002\222\371\370fd"
	.ascii	"\320\326`\332\031\314~{"
	.ascii	"\233\372|\247QJ\256mP\206\243\347T6&\202\333\202-\217\315\377"
	.ascii	"\273\t\272\312\365\033f\334\276\003"
	.ascii	"\ri\\i<7\302xn\220B\006f.%\335\322+\341JDD\035\225V9t\001v\255"
	.ascii	"5B"
	.ascii	"\365u\211\007\r\313Xb\230\362\211\221TB)I\344n\343\342#\264\312"
	.ascii	"\240\241f\360\315\260\342|\016"
	.ascii	"\371pK\331\337\376\246\376-\272\374\301Q\3000\361\211\253/\177"
	.ascii	"~\324\202H\265\356\354\212\023VRa"
	.ascii	"\243\205\214\304:d\224\304\2559a<\364\0356\375HM\351:\335\027"
	.ascii	"\333\tJg\264\217]\nnf"
	.ascii	"\r\313pHN\366\273*k\213E\252\360\274e\315]\230\350u\272N\276"
	.ascii	"\232\344\336\024\325\020\310\013\177"
	.ascii	"\240\023rs\255\235\254\203\230.\367.\272\370\366\237Wi\354C\335"
	.ascii	".\0361u\253\305\336}\220:\035"
	.string	"o\023\364&\244k"
	.ascii	"\27150\340W\2366g\215(<FO\331\337\310\313\365\333\356\370\274"
	.ascii	"\215\037\r"
	.string	"\334\201\320>1\223\026\272\2004\033\205\255\2372)\313!\003\003<\001(\001\343\375\033\243D\033\001"
	.ascii	"\\\247\nji\037V\026j\275RX\\r\277\301\255fy\232\177\335\250\021"
	.ascii	"&\020\205\322\242\210\331c"
	.ascii	"\fl\306?l\240\337?\322\r\326M\216\343@]qM\216&8\213\343z\341"
	.ascii	"W\203n\221\215\304:"
	.string	".#\275\257S\007\022"
	.ascii	"\203\366\330\375\270\316+\351\221+\347\204\263i\026\370f\240"
	.ascii	"h#+\325\3723"
	.ascii	"\350\317\"\304\320\310,\215\313:\241\005{O+\007o\245\366\354"
	.ascii	"\346\266\376\243\342q\n\271\314U\303<"
	.ascii	"\026\036\344\305\306I\006T5w?30d\370\nF\347\005\363\322\374\254"
	.ascii	"\262\247\334V\242)\364\300\026"
	.ascii	"1\221>\220C\224\266\351\3167Vz\313\224\244\270D\222\272\272\244"
	.ascii	"\321|\310hu\256kB\257\036c"
	.ascii	"\350\rp\243\271u\331GR\005\370\342\373\305\200r\341]\3442'\217"
	.ascii	"eS\265\200_f\177,\037C"
	.ascii	"\237\376f\332\020\004\351\263\246\345\026lRK\335\205\203\277"
	.ascii	"\371\036a\227=\274\265\031\251\036\213d\231U"
	.string	"\031{\217\205Dc\002\326JQ\352\241/5\253\024\327\251\220 \032D"
	.ascii	"\211&;%\221_q\004{"
	.ascii	"\306\272\346\304\200\302v\263\013\233\035m\335\323\016\227D\371"
	.ascii	"\013EX\225\232\260#\342\315W\372\254\320H"
	.ascii	"C\256\366\254(\275\355\203\264z\\}\213|5\206D,\353\267iG@\300"
	.ascii	"?X\366\302\365{\263Y"
	.ascii	"q\346\253}\344&\017\2667:/b\227\241\321\361\224\003\226\351~"
	.ascii	"\316\bB\333;m3\221A#\026"
	.ascii	"@\206\363\037\326\234I\335\240%6\006\303\233\315)\303=\327=\002"
	.ascii	"\330\342Q1\222; zp%J"
	.ascii	"\366\177&\366\336\231\344\271C\b,t{\312rw\261\362\244\351?\025"
	.ascii	"\240#\006P\320\325\354\337\337,"
	.string	"j\355\366S\212f\267*\241p\321\035XBB0a\001\342:L\024"
	.ascii	"@\374I\216$m\211!W"
	.ascii	"N\332\320\241\221P](\b>\376\265\247o\252K\263\223\223\341|\027"
	.ascii	"\345c\3750\260\304\2575\311\003"
	.ascii	"\256\033\030\375\027Un\013\264c\271+\237b\"\220%F\0062\351\274"
	.ascii	"\tU\332\023<\366t\335\216W"
	.ascii	"=\f+I\306vr\231\374\005\342\337\304\302\314G<:b\335\204\233\322"
	.ascii	"\334\242\307\210\002Y\253\302>"
	.ascii	"\313\3212\256\t:!\247\325\302\365@\337\207+\017)\253\036\350"
	.ascii	"\306\244\256\013^\254\333jl\366\033\016"
	.ascii	"\271{\330\344{\322\240\241\355\0329a\353M\213\251\203\233\313"
	.ascii	"s\320\335\240\231\316\312\017 Z\302\325-"
	.string	"~\210,y\351\325\253\342]m\222\313\030"
	.ascii	"\002\032\036_\256\272\315i\272\277_\217\350Z\263H\005s"
	.ascii	"4\343\326\241K\t[\200\031?5\tw\361>\277+p\"\006\313\006?B\335"
	.ascii	"Ex\330w\"ZX"
	.ascii	"\356\270\250\313\243Q5\304\026_\021\262\035o\242eP8\214\253R"
	.ascii	"O\017v\312\270\035A;DC0"
	.ascii	"b\211\3243\202_\212\241\177%x\354\265\304\230f\377A>7\245o\216"
	.ascii	"\247\037\230\357P\211'Vv"
	.ascii	"\235\317\206\352\243sp\341\334_\025\007\267\373\214:\216\212"
	.ascii	"\2031\374\347SH\026\366\023\266\204\364\273("
	.ascii	"\300\310\037\325Y\317\3038\362\266\006\005\375\322\355\233\217"
	.ascii	"\016W\253\237\020\277&\246F\270\301\250`A?"
	.ascii	"|l\023o\\/a\362\276\021\335\366\007\321\352\2573o\336\023\322"
	.ascii	"\232~R]\367\210\2015\313y\036"
	.ascii	"\201\201\340\365\330S\351w\331\336\235)D\f\245\204\345%E\206"
	.ascii	"\f-l\334\364\362\3219-\265\212G"
	.ascii	"\361\343\367\356\30364\001\370\020\236\376\177j\213\202\374\336"
	.ascii	"\371\274\345\b\371\17718;:\033\225\327e"
	.ascii	"Y\321R\222\323\244\246f\007\310\032\207\274\341\335\345o\311"
	.ascii	"\301\246@k,\270\024\"!\032Az\330\026"
	.ascii	"\203\005N\325\342\325\244\373\372\231\275.\327\257\037\342\217"
	.ascii	"w\351ns\302zI\336mZzW\013\231\037"
	.ascii	"\025b\006BZ~\275\263\301$Z\f\315\343\233\207\267\224\371\326"
	.ascii	"\261]\300W\246\214\363e\201|\370("
	.ascii	"\326\367\350\033\255N4\243\217y\352\254\353P\036}R\340\rR\236"
	.ascii	"V\306w>mMS\341/\210E"
	.ascii	"\344o<\224)\231\254\330\242\222\203\243a\361\371\265\363\232"
	.ascii	"\310\276\023\333\231&t\360\005\344<\204\317}"
	.ascii	"\326\203yu]4if\246\021\252\027\021\355\266b\217\022^\230W\030"
	.ascii	"\335}\335\366&\366\270\345\217h"
	.ascii	"\3002GJH\326\220l\2312V\312\375C!\325\341\306]\221\303(\276\263"
	.ascii	"\033\031's~h9g"
	.ascii	"\300\032\f\310\235\314m\2466\2448\033\364\\\240\227\306\327\333"
	.ascii	"\225\276\363\353\247\253}~\215\366\270\240}"
	.ascii	"\246uV8\024 x\357\350\251\375\2520\237d\242\313\250\337\\P\353"
	.ascii	"\321L\263\300M\035\272Z\021F"
	.ascii	"v\332\265\303S\031\017\324\233\236\021!so\254\035`Y\262\376!"
	.ascii	"`\314\003KKg\203~\210_Z"
	.string	"\271C\246\240\323(\226\236d \303\346"
	.ascii	"\313\303\2652\354-|\211\002S\233\f\307\321\325\342z\343C"
	.ascii	"\021=\241p\317\001c\217\304\320\r5\025\270\316\317~\244\274\244"
	.ascii	"\324\227\002\3674\024M\344V\266i6"
	.ascii	"3\341\246\355\006?~8\300:\241\231Q\0350g\0218&6\370\330Z\275"
	.ascii	"\276\351\325O\315\346!j"
	.string	"\343\262\231f\022)A\357\001\023\215pG\b\323q\275\260\202\021\3202T26\213\036"
	.ascii	"\007\0337E"
	.string	"_\346F0\n\027\306\361$5\322"
	.ascii	"**qXU\267\202\214<\275\333iW\377\225\241\361\371kX"
	.ascii	"\013y\370^\215\b\333\246\3457\ta\334\360xR\270n\241a\322I\003"
	.ascii	"\254y!\345\2207\260\257\016"
	.ascii	"\035\256u\017^\200@Q0\314b&\343\373\002\354m9\222\352\036\337"
	.ascii	"\353,\263[C\305D3\256D"
	.ascii	"/\004H7\301U\005\226\021\252\013\202\346A\232!\fmHs8\367\201"
	.ascii	"\034a\306\002Zg\314\2320"
	.ascii	"\356C\245\273\271\211\362\234Bq\311Z\235\016v\363\252`\223O\306"
	.ascii	"\345\202\035\217g\224\177\033\"\325b"
	.ascii	"<z\367:&\324\205uM\024\351\376\021{\256\337=\031\367Y\200p\006"
	.ascii	"\2457 \222\203S\232\362\024"
	.ascii	"m\223\320\030\234)LR\f\032\f\212l\265k\3101\206J\333.\005u\243"
	.ascii	"bEu\274\344\375\016\\"
	.ascii	"\365\327\262%\334~q\337@0\265\231\333p\371!bL\355\303\2674\222"
	.ascii	"\332>\t\356{\\6r^"
	.ascii	">\263\b/\0069\223}\2762\237\337\345Y\226[\375\275\236\037\255"
	.ascii	"=\377\254\267Is\313U\005\262p"
	.ascii	"\177!qE\007\374[W[\331\224\006]gy73\036\031\364\2737\n\232\274"
	.ascii	"\352\264GL\020\361w"
	.ascii	"L,\021U\305\023Q\276\315\037\210\232:B\210fG;P^\205wfDJ@\006"
	.ascii	"J\21794\016"
	.ascii	"(\031K>\t\013\223\030@\366\363s\016\341\343}o]9s\332\0272\364"
	.ascii	">\2347\312\326\336\212o"
	.ascii	"\350\275\316>\331\"}\266\007/\202'A\350\263\t\215m[\260\037\246"
	.ascii	"?tr#6\2126\005T^"
	.ascii	"\232\262\267\375=\022@\343\221\262\032\242\341\227{H\236\224"
	.ascii	"\346\375\002}\226\371\227\336\323\310.\347\rx"
	.string	"r'\364"
	.ascii	"\363\352\037g\252A\214**\353r\217\22227\227\327\177\241)\246"
	.ascii	"\207\2652\255\306\357\035"
	.ascii	"\274\347\232\bE\205\342\n\006M\177\034\317\336\2158\270\021H"
	.ascii	"\nQ\025\2548\344\214\222q\366\213\262\016"
	.ascii	"\247\225Q\357\032\276[\257\355\025{\221w\022\214\024.\332\345"
	.ascii	"z\373\367\221)g(\335\370\033 }F"
	.ascii	"\251\347zV\275\364\036\274\275\230D\326\262Lb?\310N\037,\322"
	.ascii	"d\020\344\001@8\272\245\305\371."
	.ascii	"\255O\357t\232\221\376\225\242\b\243\366\354{\202:\001{\244\t"
	.ascii	"\323\001N\226\227\307\243[O<\304q"
	.ascii	"\315t\236\372\366m\375\266z&\257\344\274x\202\361\016\231\357"
	.ascii	"\361\320\263U\202\223\362\305\220\243\214uZ"
	.ascii	"\224\334a\035\213\221\340\214f0\201\232F6\355\215\323\252\350"
	.ascii	"\257)\250\346\324?\3249\366'\200s\n"
	.string	"\225$F\331\020'\267\242\003P}\325\322\306\250:\312\207\264\240\277"
	.ascii	"\324\343\354r\353\263D\342\272-"
	.string	"\314\341\377W/J\017\230C\230\203\341\r\rg"
	.ascii	"\375\025\373IJ?\\\020\234\246&Qc\312\230&"
	.string	"\016\331=^/p=.\206S\322\344\030\t?\236j\251M\002\366>w^23\372J\fK"
	.ascii	"<"
	.ascii	"x\272\2602\2101e\347\213\377\\\222\3671\0308\314\037)\240\221"
	.ascii	"\033\250\b\007\353\312I\314=\264\037"
	.ascii	"+\270\364\006\254F\251\232\363\304\006\250\245\204\242\034\207"
	.ascii	"G\315\306_&\323>\027\322\037\315\001\375Ck"
	.ascii	"\363\016v>XB\307\265\220\271\n\356\271R\334u?\222+\007\302'\024"
	.ascii	"\277\360\331\360o-\013Bs"
	.ascii	"D\305\227FK]\247\307\277\377\017\337H\370\375\025ZxF\252\353"
	.ascii	"\271h(\024\367R[\020\327hZ"
	.ascii	"\006\036\205\236\313\366,\257\3048\"\306\0239Y\217s\363\373\231"
	.ascii	"\226\270\212\332\236\2744\352/c\265="
	.ascii	"\325%\230\202\261\220I.\221\211\232>\207\353\352\355\370JpL9"
	.ascii	"=\360\356\016+\337\225\244~\031Y"
	.ascii	"\330\331]\367+\356n\364\245Yg9\366\261\027\rsr\236I1\321\362"
	.ascii	"\033\023_\327I\337\0322\004"
	.ascii	"\256Z\345\344\031`\341\004\351\222/~zC{\347\244\232\025o\301"
	.ascii	"-\316\307\300\f\327\364\301\375\352E"
	.ascii	"\355\261\314\317$F\016\266\225\003\\\275\222\302\333Y\311\201"
	.ascii	"\004\334\035\235\2401@\331V]\352\316s?"
	.ascii	"+\327E\200\205\001\204iQ\006/\317\242\372\"L\306-\"ke6\032\224"
	.ascii	"\336\332b\003\310\353^Z"
	.string	"\306\215N\n\321\277\247\2679\263\311D~"
	.ascii	"W\276\372\256W\025\177 \301`\333\030b&\221\210\005&"
	.ascii	"B\345v\306<\216\201L\255\314\316\003\223,B^\b\237\022\264\312"
	.ascii	"\314\007\354\270CD\262\020\372\355\r"
	.ascii	"\004\377`\203\246\004\367Y\364\346av\336?\331\303Q5\207\022s"
	.ascii	"*\033\203W]aN.\f\255T"
	.ascii	"*R+\270\325g;\356\353\301\245\237Fc\3616\323\237\301n\362\322"
	.ascii	"\264\245\b\224z\247\272\262\354b"
	.ascii	"t(\266\2576(\007\222\245\004\341y\205^\315_J\2410\306\255\001"
	.ascii	"\255Z\230?fuP=\221a"
	.ascii	"=+\025aRy\355\345\321\327\335\016}5bIqLk\271\320\310\202t\276"
	.ascii	"\330f\251\031\371Y."
	.ascii	"\33212\0326-\306\rp\002 \2242XG\372\316\224\225?Q\001\330\002"
	.ascii	"\\]\3001\241\302\333="
	.ascii	"\024\273\226'\242W\252\363!\332\007\233\267\272:\210\0349\240"
	.ascii	"1\030\342K\345\371\0052\3308\373\347^"
	.ascii	"K\305^\316\371\017\334\232\r\023/\214k*\234\003\025\225\370\360"
	.ascii	"\307\007\200\002k\263\004\254\024\203\226x"
	.ascii	"\216jDA\313\375\215S\3717IC\251\375\254\245x\214<&\215\220\257"
	.ascii	"F\t\r\312\233<c\320a"
	.ascii	"\337s\374\370\274(\243\255\3747\360\246]i\204\356\t\251\3028"
	.ascii	"\333\264\177c\334{\006\370-\254#["
	.ascii	"f%\333\3775Itc\273h\013x\211k\275\305\003\354>U\2002\033o\365"
	.ascii	"\327\256G\330_\226n"
	.ascii	"{R\200\356S\271\322\232\215m\336\372\252\031\217\350\317\202"
	.ascii	"\016\025\004\027q\016\334\336\225\335\271\273\271y"
	.ascii	"ts\237\216\256}\231\321\026\b\273\317\370\2422\240\n_Dm\022\272"
	.ascii	"l\3154\270\314\nF\021\250\033"
	.ascii	"\302&1j@U\263\353\223\303\310h\250\203c\322\202z\271\345)d\f"
	.ascii	"lG!\375\311X\361eP"
	.string	"T\231B\f\373i\201pg\317n\327\254"
	.ascii	"F\341\272E\346p\212\271\252.\362\372\244X\236\363\2019"
	.string	"\336o\346m\245\337E\310:H@,"
	.ascii	"\245R\3412\366\264\307c\341\322\351e\033\274\334.E\3640"
	.ascii	"\223\n#Yu\212\373\030]\364\346`i\217\026\035\265<\251\024E\251"
	.ascii	"\205:\375\320\254\0057\b\3348"
	.ascii	"@\227u\305\202'm\205\314\276\234\371iE\023\372qN\352\300s\374"
	.ascii	"D\210i$?Y\032\232-c"
	.ascii	"\247\204\f\355\021\375\t\277:i\237\r\201q\360cy\207\317W-\214"
	.ascii	"\220!\242K\366\212\362}Z:"
	.ascii	"\246\313\007\270\025k\273\366\327\360T\274\337\307#\030\013g"
	.ascii	")n\003\227\035\273WJ\355G\210\364$\013"
	.string	"\307\352\033Q\276\324\332\334\362\314&\355u\200S\244e\232_"
	.ascii	"\237\377\234\341c\037HuD\367\3744"
	.ascii	"\230\252\317x\253\035\273\245\362r\013\031g\242\355\\\216`\222"
	.ascii	"\n\021\311\t\223\260t\263/\004\243\031\001"
	.ascii	"\312g\227xL\340\227\301}F\3318\313Mq\270\250_\371\203\202\210"
	.ascii	"\336U\367c\372M\026\334;="
	.ascii	"}\027\302\350\234\330\242g\301\320\225h\366\245\235f\260\242"
	.ascii	"\202\262\345\230e\365s\n\342\355\361\210\300V"
	.ascii	"\002\217\363$\254_\033X\275\f\343\272\376\351\013\251\360\222"
	.ascii	"\317\212\002i!\232\217\003Y\203\244~\213\003"
	.ascii	"\027n\250\020\021=m3\372\262u\0132\210\363\327\210)\007%v3\025"
	.ascii	"\371\207\213\020\231kLg\t"
	.ascii	"\370o1\231!\370N\237O\215\247\352\202\322I/t1\357Z\253\245q\t"
	.ascii	"e\353iY\0021^n"
	.ascii	"\"b\006c\016\373\0043?\272\254\207\211\0065\373\243a\020\214"
	.ascii	"w$\031\275 \206\203\321C\255X0"
	.ascii	"\373\223\345\207\365bl\261q>]\312\336\355\231Im>\314\024\340"
	.ascii	"\301\221\264\250\333\250\211G\021\365\b"
	.ascii	"\320cv\345\375\017<2\020\246.\2428\337\303\005\232O\231\254\275"
	.ascii	"\212\307\275\231\334\343\357\244\237T&"
	.ascii	"nf?\257I\205F\333\245\016J\361\004\317\177\327G\f\272\244\367"
	.ascii	"?\362=\205<\3162\341\337\020:"
	.ascii	"\326\371k\036FZ\035t\201\245ww\374\263\005#\331\323td\242tU\324"
	.ascii	"\377\340\001d\334\341&\031"
	.ascii	"\240\316\027\352\212N\177\340\375\301\037:F\025\325/\361\300"
	.ascii	"\3621\375\"S\027\025]\036\206\035\320\241\037"
	.string	"\253\224\337\321"
	.ascii	"\254\3348\351\r\b\321\335+q.b\342\325\375>\351\023\177\345\001"
	.ascii	"\232\356\030\355\374s"
	.ascii	"2\230Y}\224U\200\314 U\3617\332VF\036 \223\005Nt\367\366\231"
	.ascii	"3\317uj\274c5w"
	.ascii	"\263\234\023c\b\351\261\006\315>\240\305g\332\223\2442\211c\255"
	.ascii	"\310\316w\215DO\206\033pkB\037"
	.ascii	"R%\241\221\3105~\361v\234^WS\201k\267>r\233\ro@\203\3728\344"
	.ascii	"\247?\033\273v\013"
	.ascii	"\001\034\221AL&\311\357%,\242\027\270\267\243\361G\024\017\363"
	.ascii	"k\332uX\220\2601\035'\365\032N"
	.ascii	"\233\223\222\177\371\301\270\bn\253D\324\313qg\276\027\200\273"
	.ascii	"\231cd\345\"U\251r\267\036\326m{"
	.ascii	"\307\322\001\253\371\2530W\030;\024@\334v\373\026\201\262\313"
	.ascii	"\240e\276l\206\376j\377\233e\233\372S"
	.ascii	"\222=\363P\350\301\255\267\317\325\214`O\372\230y\333[\374\215"
	.ascii	"\275-\226\255O/\035\257\316\233>p"
	.ascii	"UT\210\224\351\310\024l\345\324\256ef]:\204\361Z\326\274>\267"
	.ascii	"\033\030P\037\306\304\345\223\2159"
	.ascii	"\362\343\347\322`|\207\303\261\213\2020\240\2524;8\361\236s\347"
	.ascii	"&>(w\005\303\002\220\234\234i"
	.ascii	"\363H\3423g\321K\034_\n\277\025\207\022\236\275v\003\013\241"
	.ascii	"\360\214?\324\023\033\031\337]\233\260S"
	.ascii	"\314\361FY#\247\006\363}\331\345\314\265\030\027\222u\351\264"
	.ascii	"\201G\322\315(\007\331\315o\f\363\312Q"
	.ascii	"\307T\254\030\232\371zs\017\263\034\305\334x3\220\307\f\341L"
	.ascii	"3\274\211+\232\351\370\211\301)\256\022"
	.ascii	"\n\340tvB\247\013\246\363{z\241p\205\016c\314$3\317=VX7\252\375"
	.ascii	"\203#)\252\004U"
	.ascii	"\317\001\r\037\313\300\236\251\256\3674:\314\357\321\r\"N\234"
	.ascii	"\320!u\312U\352\245\353X\351O\321_"
	.ascii	"\216\313\223\277^\376B<_V\3246Q\250\337\276\350 B\210\236\205"
	.ascii	"\360\340(\321%\007\226?\327}"
	.ascii	",\253E(\337-\334\265\223\351\177\n\261\221\224\006F\343\002@"
	.ascii	"\326\363\252M\321tdXn\362?\t"
	.ascii	")\230\005h\376$\r\261\345#\257\333r\006su)\254W\264:%g\023\244"
	.ascii	"p\264\206\274\274Y/"
	.ascii	"\001\303\221\266`\325Ap\036\347\327\255?\033 \205\205U3\021c"
	.ascii	"\341\302\026\261(\b\001=^\245*"
	.ascii	"_\023\027\231B}\204\203\327\003}V\037\221\033\255\321\252w\276"
	.ascii	"\331Hw~J\257Q..\264XT"
	.string	"OD\007\f\346\222Q\355\020\035Bt-N\305Bd\310\265\375\202L+5d\206v\212J"
	.ascii	"\351\023"
	.string	"\177\207;\031\311"
	.ascii	".\273kP\334\340\220\250\343\354\237d\3366\300\267\363\354\032"
	.ascii	"\236\336\230\b\004F_"
	.ascii	"\333\316/\203E\210\235sc\370k\256\311\3268\372\367\376O\267\312"
	.ascii	"\r\2742^\344\274\024\210~\223s"
	.ascii	"\215\364{)\026q\003\2714h\360\324\";\321\251\306\275\226FW\025"
	.ascii	"\227\3415\350\325\221\350\244\370,"
	.ascii	"\242k\320\027~H\265,k\031P9\0348\322$0\212\227\205\201\234e\327"
	.ascii	"\366\244\326\221(\177oz"
	.ascii	"g\017\021\007\207\375\223mI\2658|\323\tL\335\206js\302Lj\261"
	.ascii	"|\t*%Xn\275I "
	.ascii	"I\357\232j\215\375\t}\013\271=[\276`\356\360\324\277\236Q,\265"
	.ascii	"!L\035\224E\305\337\252\021`"
	.ascii	"\220\370\313\002\310\320\336c\252j\377\r\312\230\320\373\231"
	.ascii	"\355\266\271\375\nMb\036\0134y\267\030\316i"
	.ascii	"<\370\225\317m\222g_q\220(qa\205~|[z\217\231\363\347\241\326"
	.ascii	"\340\371b\013\033\314\305o"
	.ascii	"\313y\230\262(U\357\321\222\220~\324<\256\032\335R#\237\030B"
	.ascii	"\004~\022\361\001q\345:kY\025"
	.ascii	"\312$Q~\0261\377\t\337E\307\331\213\025\344\013\345V\365~\"}"
	.ascii	"+)8\321\266\257A\342\244:"
	.ascii	"\242y\221?\3229'F\317\335\326\2271\022\203\377\212\024\362S\265"
	.ascii	"\336\007\023\332M_{h7\"\r"
	.ascii	"\365\0053*\2778\301,\303&\351\242\217?XH\353\322IU\242\261:\b"
	.ascii	"l\243\207Fn\252\3742"
	.ascii	"\337\314\207's\244\0072\370\343\023\362\b\031\343\027N\226\r"
	.ascii	"\366\327\354\262\325\351\013`\3026cot"
	.ascii	"\365\232}\305\215n\305{\362\275\360\235\355\322\013>\243\344"
	.ascii	"\357\"\336\024\300\252\\j\275\376\316\351'F"
	.string	"\034\227l\253E\363J?\037sC\231r\353\210\342m\030D\003\212jY3\223b\326~"
	.ascii	"\027I{"
	.ascii	"\335\242S\335(\0334T?\374B\337[\220\027\252\364\370\322M\331"
	.ascii	"\222\365\017}\323\214\340\017b\003\035"
	.ascii	"d\260\204\253\\\373\205-\024\274\363\211\322\020xI\f\316\025"
	.ascii	"{D\334jG{\375D\370v\243+\022"
	.ascii	"T\345\264\242\3152\002\302\177\030]\021B\375\320\236\331y\324"
	.ascii	"}\276\264\253.L\354h+\365\013\307\002"
	.ascii	"\341r\215E\2772\345\254\265<\267|\340h\347[\347\275\213\356\224"
	.ascii	"}\317V\003:\264\376\343\227\006k"
	.ascii	"\273/\013]K\354\207\242\312\202H\007\220W\\A\\\201\320\301\036"
	.ascii	"\246D\340\340\365\236@\nO3&"
	.string	"\300\243b\337J\360\310\266]\244m\007\357"
	.ascii	"\360>\251\322\360IX\271\234\234\256/\033DC\177\303\034"
	.ascii	"\271\256\316\311\361Vf\327je\345\030\370\025[\0344#L\2042(\347"
	.ascii	"&8h\031/wo4:"
	.ascii	"O2\307\\ZV\217P\"\251\006\345\300\304a\320\031\254E\\\333\253"
	.ascii	"\030\373J1\200\003\301\thl"
	.ascii	"\310j\332\342\022Q\325\322\355Q\350\2611\003\275\351br\306\216"
	.ascii	"\335F\007\226\320\305\367n\237\033\221\005"
	.ascii	"\357\352.Q\363\254ISI\313\301\034\323A\301 \215h\232\251\007"
	.ascii	"\f\030$\027-K\306\321\371^U"
	.ascii	"\273\016\337\365\203\2313\301\254L,Q\217u\363\300\341\230\263"
	.ascii	"\013\n\023\361,b\f'\252\371\354<k"
	.ascii	"\b\275s;\272p\2476\f\277\257\243\b\357Jb\362F\t\264\230\3777"
	.ascii	"W\235t\2013\341M_g"
	.ascii	"\035\263\332;\331\366/\241\376-e\235\017\330%\007\207\224\276"
	.ascii	"\232\363O\234\001C<\315\202\270P\364`"
	.ascii	"\374\202\027k\003R,\016\264\203\255l\201l\201d>\007di\331\275"
	.ascii	"\334\320 \305d\001\367\235\331\023"
	.string	"\312\300\345!\303^K\001\242\277\031\327\311i\313O\240#"
	.ascii	"u\030\034_N\200\254\355U\236\336\006\034"
	.ascii	"\252im\377@+\325\377\273I@\334\030\013S4\227\230M\243/\\J^-\272"
	.ascii	"2}\216o\tx"
	.ascii	"\342\304>\243\326z\017\231\216\340.\2768\371\bf\025E(c\305C\241"
	.ascii	"\234\r\266-\354\037\212\363L"
	.ascii	"\347\\\372\re\252\252\240\214G\265H*\236\304\371[r\003p}\314"
	.ascii	"\tO\276\032\t&:\255<7"
	.ascii	"\255\273\335\211\373\250\276\361\313\256\256a\274,\313;\235\215"
	.ascii	"\233\037\273\247X\217\206\246\022Q\332~T!"
	.string	"|\365\311\202Mc\224\2626E\223$\341\375\313\037Z\333\214A\263M\234\236\374\031DE\331\363@"
	.ascii	"\323\206Y\3759\351\375\336\f8\nQ\211,'\364\271\0311\273\007\244"
	.ascii	"+\267\364M%J3\nUc"
	.ascii	"I{TrEX\272\233\340\b\304\342\372\306\005\363\215\3614\307i\372"
	.ascii	"\350`zv}\252\257+\2519"
	.string	"7\317i\265\355\326\007e\341.\245\f\260)\204\027]\326k\353\220"
	.ascii	"|\352Q\217\367\332\307b\352>"
	.ascii	"N'\223\346\023\307$\235u\323\333hw\205c_\232\263\212\353`URp"
	.ascii	"\315\304\311e\006jCh"
	.ascii	"|\020 \350\027\323V\036e\351\n\204Dh&\305z\374\0172\306\241\340"
	.ascii	"\301r\024a\221\234fsS"
	.string	"'?/ \3505\002\274\260u\371d\342"
	.ascii	"\\\307\026$\214\243\325\351\244\221\371\211\267\212\366\347\266"
	.ascii	"\027"
	.ascii	"WR\016\232\253\024(]\374\263\312\311\204 \217\220\312\036-[\210"
	.ascii	"\365\312\257\021}\370x\246\265\264\034"
	.string	"\347\007\240\242b\252tk\261\307q\360\260\340\021\363#\342\013"
	.ascii	"8\344\007W\254n\357\202-\375\300-"
	.ascii	"l\374J9k\300d\266\261_\332\230$\336\210\f4\330\312K\026\003\215"
	.ascii	"O\2424t\336x\312\0133"
	.ascii	"Nt\031\021\204\377.\230$G\007+\226^i\371\373S\311\277O\301\212"
	.ascii	"\305\365\034\2376\033\2761<"
	.ascii	"rB\313\371\223\274h\301\230\333\316\307\037q\270\256z\215\254"
	.ascii	"4\252R\016\177\273U}~\t\301\316A"
	.ascii	"\356\212\224\bM\206\364\260o\034\272\221\356\031\334\007X\241"
	.ascii	"\254\246\256\315uy\273\324bB\023a\0133"
	.ascii	"\212\200m\242\327\031\226\367m\025\236\035\236\324\037\273'\337"
	.ascii	"\241\333l\303\327s}w(\037\331L\264&"
	.ascii	"\203\003sb\223\362\267\341,\212\312\353\377yRK\024\023\324\277"
	.ascii	"\212w\374\332\017ar\234\024\020\353}"
	.ascii	"ut8\217GH\360Q<\313\276\234\364\274]\262U \237\331D\022\253\232"
	.ascii	"\326\245\020\034l\236p,"
	.ascii	"z\356f\207j\257b\313\016\315SU\004\354\313f\265\344\013\0178"
	.ascii	"\001\200X\352\342,\366\237\216\346\b"
	.ascii	"\371\362\270\n\325\t-/\337#Y\305\215!\271\254\271lvs&4\217J\365"
	.ascii	"\031\3678\327;\261L"
	.ascii	"\2550\301K\nP\2554\234\324\013=I\3338\215\276\211\nP\230=\\\242"
	.ascii	"\t;\272\356\207?\037/"
	.ascii	"J\266\025\345u\214\204\3678\220J\333\272\001\225\245P\033u??"
	.ascii	"1\r\302\350.\256\300S\343\241\031"
	.ascii	"\275\275\226\325\315r!\264@\374\356\230CE\340\223\265\tA\264"
	.ascii	"GS\261\2374\256f\002\231\323ks"
	.ascii	"\303\005\372\272`u\034}a^\345\306\240\240\341\263sd\326\300\030"
	.ascii	"\227R\343\2064\f\302\021kTA"
	.ascii	"\264\2634\223P-S\205se\201`K\021\375Fu\203\\B0__\314\\\253\177"
	.ascii	"\270\242\225\"A"
	.string	"\306\352\223\342aRe.\333\2543!\003\222Z\204k\231"
	.ascii	"y\313u\tF\200\335Z\031\215\273`\007"
	.ascii	"\351\326~\365\210\233\311\031%\310\370m&\313\223Ss\322\n\263"
	.ascii	"\0232\356\\4.-\265\353S\341\024"
	.ascii	"\212\201\346\315\027\032>A\204\240i\355\251m\025W\261\314\312"
	.ascii	"F\217&\277,\362\305:\303\233\2764k"
	.ascii	"\323\362qeei\374\021zs\016SE\350\311\3065P\376\324\242\347:\343"
	.ascii	"\013\323m.\266\307\271\001"
	.ascii	"\262\300x:d/\337\363|\002.\362\036\227>L\243\265\301I^\034}\354"
	.ascii	"-\335\"\t\217\301\022 "
	.ascii	")\235\310Z\345U\013\210c\247\240E\037$\203\024\037l\347\302\337"
	.ascii	"\3576=\350\255KNx[\257\b"
	.ascii	"K,\314\211\322\024s\342\215\027\207\242\021\275\344K\316d3\372"
	.ascii	"\326(\325\030n\202\331\257\325\301#d"
	.ascii	"3%\037\210\334\2314(\266#\223w\332%\005\235\364A4g\373\335z\211"
	.ascii	"\215\026:\026q\235\2672"
	.ascii	"j\263\374\355\331\370\205\314\371\345F7\217\302\274\"\315\323"
	.ascii	"\345\3718\343\235\344\314->\301\373^\nH"
	.ascii	"\037\"\316B\344La\266(9\005L\314\235\031n\003\276\034\334\244"
	.ascii	"\264?f\006\216\034iG\035\263$"
	.ascii	"q b\001\013\347Q\013\305\257\035\213\317\005\265\006\315\253"
	.ascii	"Z\357a\260k,1\277\267\f`'\252G"
	.ascii	"\303\370\025\300\355\036T*|?i|~\376\244\021\326x\242N\023f\257"
	.ascii	"\360\224\240\335\024]X[T"
	.ascii	"\341!\263\343\320\344\004b\225\036\377(zc\252;\236\275\231[\375"
	.ascii	"\317\f\013q\320\310d>\334\"M"
	.ascii	"\017:\324\240^'\277g\276\356\233\b4\216\346\255.\347y\324L\023"
	.ascii	"\211BTT\2722\303\371b\017"
	.ascii	"9_;\326\211e\264\374a\317\313W?j\256\\\005\372:\225\322\302\272"
	.ascii	"\3766\02476\032\240\017\034"
	.ascii	"Pj\223\214\016+\bi\266\305\332\3015\240\311\3714\266\337\304"
	.ascii	"T>\267o@\301+\035\233A\005@"
	.ascii	"\377=\224\"\266\004\306\322\240\263\317D\316\276\214\274x\206"
	.ascii	"\200\227\363O%]\277\246\034;Oa\243\017"
	.ascii	"\360\202\276\271\275\376\003\240\220\254D:\257\301\211 \216\372"
	.ascii	"T\031\221\237I\370B\253@\357\212!\272\037"
	.ascii	"\224\001{>\004W>O\177\257\332\b\356>\035\250\361\336\334\231"
	.ascii	"\253\3069\310\325aw\377\023]Sl"
	.ascii	">\365\310\372H\224T\253A7\246{\232\350\366\201\001^+l}l\375t"
	.ascii	"Bn\310\250\312:.9"
	.ascii	"\2575\212>\3514\275L\026\350\207XD\201\007.\253\260\232\362v"
	.ascii	"\2341\031;\301\n\325\344\177\341%"
	.ascii	"\247!\361v\365\177_\221\343\207\315/'2J\303&\345\033M\336/\272"
	.ascii	"\314\233\211i\211\217\202\272k"
	.ascii	"v\366\004\036\327\233(\n\225\017B\326R\034\216 \253\037i4\260"
	.ascii	"\330\206QQ\263\237*DQW%"
	.ascii	"\0019\376\220f\274\321\342\325z\231\240\030J\265L\324`\204\257"
	.ascii	"\024i\035\227\344{k\177OP\235U"
	.ascii	"\375f\322\366\347\221H\234\033x\007\003\233\241D\007;\342a`\035"
	.ascii	"\2178\210\016\325K5\243\246>\022"
	.string	"\325T\353\263x\203s\247|<U\245f\323i\035\272"
	.ascii	"(\371b\317&\n\0272~\200\325\022\253\001"
	.ascii	"\226-\343A\220\030\215\021HX1\330\302\343\355\271\331E2\330q"
	.ascii	"B\253\036T\241\030\311\342a9J"
	.ascii	"\036?#\363D\326'\003\026\360\3744\016&\232Iy\271\332\362\026"
	.ascii	"\247\265\203\037\021\324\233\255\356\254h"
	.ascii	"\240\273\346\370\340;\334q\n\343\377~4\370\316\326jG:\341_B\222"
	.ascii	"\251c\267\035\373\343\274\326,"
	.ascii	"\020\302\327\363\016\311\2648\f\004\255\267$n\2160#>\347\267"
	.ascii	"\361\331`8\227\365\b\265\325`WY"
	.ascii	"\220'\002\375\353\313*\210`W\021\304\0053\257\211\364s4}\343"
	.ascii	"\222\364e+ZQT\337\305\262,"
	.string	"\227c\252\004\341\277)a\313\374\247\244\b"
	.ascii	"\226\217X\224\220}\211\300\213?\251\221\262\334>\244\237p"
	.ascii	"\312*\375c\214]\n\353\377Ni.f\301+\322:\260\313\370n\363#'\037"
	.ascii	"\023\310\360\354)\360p"
	.ascii	"\271\260\020^\252\257j*\251\032\004\357p\243\360x\037\326:\252"
	.ascii	"w\373>w\341\331K\247\242\245\354D"
	.ascii	"3>\355.\263\007\023F\347\201U\2443/\004\256f\003_\031\323ID\311"
	.ascii	"XH1l\212]}\013"
	.string	"C\325\225{2H\324%\035\0174\243"
	.ascii	"\203\323p+\305\341`\034S\034\336\344\351},Q$\"'"
	.ascii	"\374u\251B\212\273{\277X\243\255\226w9\\\214H\252\355\315o\307"
	.ascii	"\177\342\246 \274\366\327_s\031"
	.ascii	".4\305I\257\222\274\032\320\372\346\262\021\330\356\377)N\310"
	.ascii	"\374\215\214\242\357C\305L\244\030\337\265\021"
	.ascii	"fB\310B\320\220\253\343~T\031\177\017\216\204\353\271\227\244"
	.ascii	"e\320\241\003%_\211\337\221\021\221\357\017"
	.align 32
	.type	K512, @object
	.size	K512, 640
K512:
	.quad	4794697086780616226
	.quad	8158064640168781261
	.quad	-5349999486874862801
	.quad	-1606136188198331460
	.quad	4131703408338449720
	.quad	6480981068601479193
	.quad	-7908458776815382629
	.quad	-6116909921290321640
	.quad	-2880145864133508542
	.quad	1334009975649890238
	.quad	2608012711638119052
	.quad	6128411473006802146
	.quad	8268148722764581231
	.quad	-9160688886553864527
	.quad	-7215885187991268811
	.quad	-4495734319001033068
	.quad	-1973867731355612462
	.quad	-1171420211273849373
	.quad	1135362057144423861
	.quad	2597628984639134821
	.quad	3308224258029322869
	.quad	5365058923640841347
	.quad	6679025012923562964
	.quad	8573033837759648693
	.quad	-7476448914759557205
	.quad	-6327057829258317296
	.quad	-5763719355590565569
	.quad	-4658551843659510044
	.quad	-4116276920077217854
	.quad	-3051310485924567259
	.quad	489312712824947311
	.quad	1452737877330783856
	.quad	2861767655752347644
	.quad	3322285676063803686
	.quad	5560940570517711597
	.quad	5996557281743188959
	.quad	7280758554555802590
	.quad	8532644243296465576
	.quad	-9096487096722542874
	.quad	-7894198246740708037
	.quad	-6719396339535248540
	.quad	-6333637450476146687
	.quad	-4446306890439682159
	.quad	-4076793802049405392
	.quad	-3345356375505022440
	.quad	-2983346525034927856
	.quad	-860691631967231958
	.quad	1182934255886127544
	.quad	1847814050463011016
	.quad	2177327727835720531
	.quad	2830643537854262169
	.quad	3796741975233480872
	.quad	4115178125766777443
	.quad	5681478168544905931
	.quad	6601373596472566643
	.quad	7507060721942968483
	.quad	8399075790359081724
	.quad	8693463985226723168
	.quad	-8878714635349349518
	.quad	-8302665154208450068
	.quad	-8016688836872298968
	.quad	-6606660893046293015
	.quad	-4685533653050689259
	.quad	-4147400797238176981
	.quad	-3880063495543823972
	.quad	-3348786107499101689
	.quad	-1523767162380948706
	.quad	-757361751448694408
	.quad	500013540394364858
	.quad	748580250866718886
	.quad	1242879168328830382
	.quad	1977374033974150939
	.quad	2944078676154940804
	.quad	3659926193048069267
	.quad	4368137639120453308
	.quad	4836135668995329356
	.quad	5532061633213252278
	.quad	6448918945643986474
	.quad	6902733635092675308
	.quad	7801388544844847127
	.section	.rodata.cst16,"aM",@progbits,16
	.align 16
.LC0:
	.quad	7640891576956012808
	.quad	-4942790177534073029
	.align 16
.LC1:
	.quad	4354685564936845355
	.quad	-6534734903238641935
	.align 16
.LC2:
	.quad	5840696475078001361
	.quad	-7276294671716946913
	.align 16
.LC3:
	.quad	2270897969802886507
	.quad	6620516959819538809
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC4:
	.long	0
	.long	64
	.section	.rodata.cst16
	.align 16
.LC6:
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.byte	15
	.align 16
.LC8:
	.quad	256
	.quad	0
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
