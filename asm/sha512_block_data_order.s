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
.L8:
	movq	8(%rsp), %rdi
	movq	(%r12), %rax
	movq	16(%rsp), %rbx
	movq	24(%rsp), %r10
#	bswap	%rax
	movq	%rdi, %rcx
	movq	%rax, %rsi
	movq	%rdi, %rdx
	movq	%rax, -40(%rsp)
	movq	%rdi, %rax
#	rorq	$14, %rcx
	andq	%rbx, %rdx
#	rorq	$18, %rax
	movq	-16(%rsp), %r14
	movq	-8(%rsp), %r15
	xorq	%rax, %rcx
	movq	%rdi, %rax
#	rolq	$23, %rax
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
#	rolq	$30, %rdx
#	rorq	$28, %rax
	xorq	%rdx, %rax
	movq	%r13, %rdx
#	rolq	$25, %rdx
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
#	bswap	%rax
	movq	%rax, %rsi
	movq	%rax, -120(%rsp)
	movq	%r9, %rax
#	rorq	$14, %rax
#	rorq	$18, %rdx
	xorq	%rax, %rdx
	movq	%r9, %rax
#	rolq	$23, %rax
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
#	rolq	$30, %rsi
#	rorq	$28, %rax
	leaq	(%rdx,%r15), %r11
	movabsq	$4131703408338449720, %r15
	xorq	%rax, %rsi
	movq	%rcx, %rax
#	rolq	$25, %rax
	xorq	%rax, %rsi
	movq	%r14, %rax
	xorq	%r13, %rax
	andq	%rcx, %rax
	xorq	%rdi, %rax
	addq	%rsi, %rax
	movq	%r11, %rsi
	addq	%rax, %rdx
	movq	16(%r12), %rax
#	rorq	$18, %rsi
	movq	%rdx, %rbp
#	bswap	%rax
	movq	%rax, %rdi
	movq	%rax, -112(%rsp)
	movq	%r11, %rax
#	rorq	$14, %rax
	xorq	%rax, %rsi
	movq	%r11, %rax
#	rolq	$23, %rax
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
#	rolq	$30, %rdi
#	rorq	$28, %rax
	leaq	(%rsi,%r14), %r10
	xorq	%rax, %rdi
	movq	%rdx, %rax
#	rolq	$25, %rax
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
#	rorq	$18, %rdi
#	bswap	%rax
	movq	%rax, %r14
	movq	%rax, -104(%rsp)
	movq	%r10, %rax
#	rorq	$14, %rax
	xorq	%rax, %rdi
	movq	%r10, %rax
#	rolq	$23, %rax
	xorq	%rax, %rdi
	leaq	(%r14,%rbx), %rax
	movq	32(%r12), %r14
	movabsq	$-1606136188198331460, %rbx
	addq	%rbx, %rax
	movq	%rcx, %rbx
	addq	%rax, %rdi
	movq	%r10, %rax
	andq	%rdx, %rbx
#	bswap	%r14
	notq	%rax
	movq	%r14, -96(%rsp)
	andq	%r9, %rax
	xorq	%r8, %rax
	movq	%rsi, %r8
	addq	%rax, %rdi
	movq	%rsi, %rax
#	rolq	$30, %r8
#	rorq	$28, %rax
	xorq	%rax, %r8
	movq	%rsi, %rax
#	rolq	$25, %rax
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
#	rorq	$14, %rbx
#	rorq	$18, %r8
	xorq	%rbx, %r8
	movq	%rax, %rbx
#	rolq	$23, %rbx
	xorq	%r8, %rbx
	leaq	(%r14,%r15), %r8
	movq	40(%r12), %r14
	movabsq	$6480981068601479193, %r15
	addq	%r9, %r8
	movq	%rax, %r9
	addq	%rbx, %r8
	notq	%r9
	movq	%r10, %rbx
#	bswap	%r14
	andq	%r11, %r9
	andq	%rax, %rbx
	andq	%rsi, %rbp
	movq	%r14, -88(%rsp)
	xorq	%rbx, %r9
	movq	%rdi, %rbx
	addq	%r9, %r8
	movq	%rdi, %r9
#	rolq	$30, %rbx
#	rorq	$28, %r9
	addq	%r8, %rcx
	xorq	%r9, %rbx
	movq	%rdi, %r9
#	rolq	$25, %r9
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
#	rorq	$14, %rbx
#	rorq	$18, %r9
	xorq	%rbx, %r9
	movq	%rcx, %rbx
#	rolq	$23, %rbx
	xorq	%r9, %rbx
	leaq	(%r14,%r15), %r9
	movq	48(%r12), %r14
	movabsq	$-7908458776815382629, %r15
	addq	%r11, %r9
	movq	%rcx, %r11
	addq	%rbx, %r9
	notq	%r11
	movq	%rax, %rbx
#	bswap	%r14
	andq	%rcx, %rbx
	andq	%r10, %r11
	movq	%r14, -80(%rsp)
	xorq	%rbx, %r11
	movq	%r8, %rbx
	addq	%r11, %r9
	movq	%r8, %r11
#	rolq	$30, %rbx
#	rorq	$28, %r11
	xorq	%r11, %rbx
	movq	%r8, %r11
#	rolq	$25, %r11
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
#	rorq	$14, %rbx
#	rorq	$18, %r11
	xorq	%rbx, %r11
	movq	%rdx, %rbx
#	rolq	$23, %rbx
	xorq	%r11, %rbx
	leaq	(%r14,%r15), %r11
	movq	56(%r12), %r14
	movabsq	$-6116909921290321640, %r15
	addq	%r10, %r11
	movq	%rdx, %r10
	addq	%rbx, %r11
	notq	%r10
	movq	%rcx, %rbx
#	bswap	%r14
	andq	%rdx, %rbx
	andq	%rax, %r10
	movq	%r14, -72(%rsp)
	xorq	%rbx, %r10
	movq	%r9, %rbx
	addq	%r10, %r11
	movq	%r9, %r10
#	rolq	$30, %rbx
#	rorq	$28, %r10
	addq	%r11, %rsi
	xorq	%r10, %rbx
	movq	%r9, %r10
#	rolq	$25, %r10
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
#	rorq	$14, %rbx
#	rorq	$18, %r10
	xorq	%rbx, %r10
	movq	%rsi, %rbx
	andq	%r9, %rbp
#	rolq	$23, %rbx
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
#	rolq	$30, %rbx
#	rorq	$28, %rax
	addq	%r10, %rdi
	xorq	%rax, %rbx
	movq	%r11, %rax
#	rolq	$25, %rax
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
#	rorq	$14, %rbx
#	bswap	%rax
	movq	%rax, %r15
	movq	%rax, -64(%rsp)
	movq	%rdi, %rax
#	rorq	$18, %rax
	xorq	%rbx, %rax
	movq	%rdi, %rbx
#	rolq	$23, %rbx
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
#	rolq	$30, %rbx
	andq	%r11, %rbp
#	rorq	$28, %rcx
	addq	%rax, %r8
	xorq	%rcx, %rbx
	movq	%r10, %rcx
#	rolq	$25, %rcx
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
#	rorq	$14, %rbx
#	bswap	%rcx
	movq	%rcx, %r14
	movq	%rcx, -56(%rsp)
	movq	%r8, %rcx
#	rorq	$18, %rcx
	xorq	%rbx, %rcx
	movq	%r8, %rbx
#	rolq	$23, %rbx
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
#	rolq	$30, %rbx
#	rorq	$28, %rdx
	xorq	%rdx, %rbx
	movq	%rax, %rdx
#	rolq	$25, %rdx
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
#	rorq	$18, %rbx
#	bswap	%rdx
	movq	%rdx, %r15
	movq	%rdx, -48(%rsp)
	movq	%r9, %rdx
#	rorq	$14, %rdx
	xorq	%rdx, %rbx
	movq	%r9, %rdx
#	rolq	$23, %rdx
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
#	rolq	$30, %rbx
#	rorq	$28, %rsi
	addq	%rdx, %r11
	xorq	%rsi, %rbx
	movq	%rcx, %rsi
#	rolq	$25, %rsi
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
#	rorq	$18, %rbx
#	bswap	%rsi
	movq	%rsi, %r14
	movq	%rsi, -32(%rsp)
	movq	%r11, %rsi
#	rorq	$14, %rsi
	xorq	%rsi, %rbx
	movq	%r11, %rsi
#	rolq	$23, %rsi
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
#	rolq	$30, %rbx
#	rorq	$28, %rdi
	addq	%rsi, %r10
	xorq	%rdi, %rbx
	movq	%rdx, %rdi
#	rolq	$25, %rdi
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
#	rorq	$18, %rbx
#	bswap	%rdi
	movq	%rdi, -24(%rsp)
	movq	%rdi, %r15
	movq	%r10, %rdi
#	rorq	$14, %rdi
	movq	104(%r12), %r14
	movq	%r13, 48(%rsp)
	xorq	%rdi, %rbx
	movq	%r10, %rdi
#	rolq	$23, %rdi
#	bswap	%r14
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
#	bswap	%r15
	andq	%r10, %rbx
	andq	%r9, %r8
	xorq	%rbx, %r8
	movq	%rsi, %rbx
	addq	%r8, %rdi
	movq	%rsi, %r8
#	rolq	$30, %rbx
	andq	%rdx, %rbp
#	rorq	$28, %r8
	addq	%rdi, %rax
	xorq	%r8, %rbx
	movq	%rsi, %r8
#	rolq	$25, %r8
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
#	rorq	$18, %rbx
#	rorq	$14, %r8
	xorq	%r8, %rbx
	movq	%rax, %r8
#	rolq	$23, %r8
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
#	rolq	$30, %rbx
#	rorq	$28, %r9
	xorq	%r9, %rbx
	movq	%rdi, %r9
#	rolq	$25, %r9
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
#	rorq	$18, %rbx
#	rorq	$14, %r9
	xorq	%r9, %rbx
	movq	%rcx, %r9
#	rolq	$23, %r9
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
#	rolq	$30, %rbx
#	rorq	$28, %r11
	addq	%r9, %rdx
	xorq	%r11, %rbx
	movq	%r8, %r11
#	rolq	$25, %r11
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
#	rorq	$18, %rbx
#	bswap	%r11
	movq	%r11, %r14
	movq	%rdx, %r11
#	rorq	$14, %r11
	xorq	%r11, %rbx
	movq	%rdx, %r11
#	rolq	$23, %r11
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
#	rolq	$30, %rbx
#	rorq	$28, %r10
	addq	%r11, %rsi
	xorq	%r10, %rbx
	movq	%r9, %r10
#	rolq	$25, %r10
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
#	rorq	$8, %rbp
#	rorq	%rbx
	xorq	%rbp, %rbx
	movq	%r12, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %rbx
	movq	%r15, %rbp
#	rolq	$3, %rbp
	movq	%rbp, %r12
	movq	%r15, %rbp
#	rorq	$19, %rbp
	xorq	%r12, %rbp
	movq	%r15, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	movq	%rsi, %r12
	addq	%rbp, %rbx
	movq	-56(%rsp), %rbp
#	rolq	$23, %r12
	addq	%rbp, %rbx
	addq	%rbx, -40(%rsp)
	movq	%rsi, %rbx
#	rorq	$18, %rbx
	movq	%rbx, %rbp
	movq	%rsi, %rbx
#	rorq	$14, %rbx
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
#	rorq	$28, %r12
	addq	%rbp, %rbx
	movq	%r12, %rbp
	movq	%r9, %r12
	addq	%rbx, %rax
	movq	%r11, %rbx
#	rolq	$30, %rbx
	xorq	%rbx, %rbp
	movq	%r11, %rbx
#	rolq	$25, %rbx
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
#	rorq	$8, %r12
	movq	%r12, %rbp
	movq	%rbx, %r12
#	rorq	%rbx
	xorq	%rbp, %rbx
	movq	%r12, %rbp
	movq	-48(%rsp), %r12
	shrq	$7, %rbp
	xorq	%rbp, %rbx
	movq	%r14, %rbp
	addq	%r12, %rbx
	movq	-120(%rsp), %r12
#	rorq	$19, %rbp
	addq	%r12, %rbx
	movq	%r14, %r12
#	rolq	$3, %r12
	xorq	%r12, %rbp
	movq	%r14, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	leaq	(%rbx,%rbp), %r12
	movq	%rdi, %rbx
#	rorq	$18, %rbx
	movq	%r12, -120(%rsp)
	movq	%rdi, %r12
	movq	%rbx, %rbp
	movq	%rdi, %rbx
#	rolq	$23, %r12
#	rorq	$14, %rbx
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
#	rorq	$28, %rcx
#	rolq	$30, %rbx
	xorq	%rcx, %rbx
	movq	%rax, %rcx
#	rolq	$25, %rcx
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
#	rorq	%rbx
#	rorq	$8, %rcx
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
#	rolq	$3, %rbx
#	rorq	$19, %rbp
	xorq	%rbp, %rbx
	movq	-40(%rsp), %rbp
	shrq	$6, %rbp
	xorq	%rbp, %rbx
	movq	%r8, %rbp
	addq	%rbx, %rcx
#	rorq	$14, %rbp
	movq	%rax, %rbx
	movq	%rcx, -112(%rsp)
	movq	%r8, %rcx
#	rorq	$18, %rcx
	xorq	%rbp, %rcx
	movq	%r8, %rbp
#	rolq	$23, %rbp
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
#	rorq	$28, %rdx
#	rolq	$30, %rcx
	addq	%rbp, %r9
	xorq	%rdx, %rcx
	movq	%r12, %rdx
#	rolq	$25, %rdx
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
#	rorq	%rcx
#	rorq	$8, %rdx
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
#	rolq	$3, %rcx
#	rorq	$19, %rbx
	xorq	%rbx, %rcx
	movq	-120(%rsp), %rbx
	shrq	$6, %rbx
	xorq	%rbx, %rcx
	movq	%r9, %rbx
	addq	%rcx, %rdx
#	rolq	$23, %rbx
	movq	%rdx, -104(%rsp)
	movq	%r9, %rdx
#	rorq	$14, %rdx
	movq	%rdx, %rcx
	movq	%r9, %rdx
#	rorq	$18, %rdx
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
#	rolq	$30, %rsi
	movq	%rbp, %rdx
#	rorq	$28, %rdx
	movq	%rsi, %rcx
	movq	%rax, %rsi
	addq	%rbx, %r11
	xorq	%rdx, %rcx
	movq	%rbp, %rdx
	andq	%r12, %rsi
#	rolq	$25, %rdx
	xorq	%rdx, %rcx
	movq	%rax, %rdx
	xorq	%r12, %rdx
	andq	%rbp, %rdx
	xorq	%rsi, %rdx
	movq	-88(%rsp), %rsi
	addq	%rcx, %rdx
	addq	%rdx, %rbx
	movq	%rsi, %rdx
#	rorq	%rdx
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	shrq	$7, %rsi
#	rorq	$8, %rdx
	xorq	%rcx, %rdx
	movq	-112(%rsp), %rcx
	xorq	%rsi, %rdx
	movq	-96(%rsp), %rsi
	addq	%r13, %rdx
	addq	%rsi, %rdx
	movq	%rcx, %rsi
#	rolq	$3, %rcx
#	rorq	$19, %rsi
	xorq	%rsi, %rcx
	movq	-112(%rsp), %rsi
	shrq	$6, %rsi
	xorq	%rsi, %rcx
	leaq	(%rdx,%rcx), %rsi
	movq	%r11, %rcx
	movq	%r11, %rdx
#	rorq	$14, %rcx
#	rorq	$18, %rdx
	addq	160(%r10), %rdi
	movq	%rsi, -96(%rsp)
	xorq	%rcx, %rdx
	movq	%r11, %rcx
	addq	%rsi, %rdi
#	rolq	$23, %rcx
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
#	rolq	$30, %rdi
	movq	%rbx, %rdx
#	rorq	$28, %rdx
	movq	%rdi, %rsi
	movq	%r12, %rdi
	addq	%rcx, %rax
	xorq	%rdx, %rsi
	movq	%rbx, %rdx
	andq	%rbp, %rdi
#	rolq	$25, %rdx
	xorq	%rdx, %rsi
	movq	%r12, %rdx
	xorq	%rbp, %rdx
	andq	%rbx, %rdx
	xorq	%rdi, %rdx
	movq	-80(%rsp), %rdi
	addq	%rsi, %rdx
	addq	%rdx, %rcx
	movq	%rdi, %rdx
#	rorq	%rdx
	movq	%rdx, %rsi
	movq	%rdi, %rdx
	shrq	$7, %rdi
#	rorq	$8, %rdx
	xorq	%rsi, %rdx
	movq	-104(%rsp), %rsi
	xorq	%rdi, %rdx
	movq	-88(%rsp), %rdi
	addq	%r15, %rdx
	addq	%rdi, %rdx
	movq	%rsi, %rdi
#	rolq	$3, %rsi
#	rorq	$19, %rdi
	xorq	%rdi, %rsi
	movq	-104(%rsp), %rdi
	addq	168(%r10), %r8
	shrq	$6, %rdi
	xorq	%rdi, %rsi
	addq	%rsi, %rdx
	movq	%rax, %rsi
	movq	%rdx, %rdi
	movq	%rax, %rdx
#	rorq	$18, %rsi
#	rorq	$14, %rdx
	addq	%rdi, %r8
	movq	%rdi, -88(%rsp)
	xorq	%rdx, %rsi
	movq	%rax, %rdx
#	rolq	$23, %rdx
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
#	rorq	$28, %r8
	addq	%rsi, %rdx
	movq	%r8, %rsi
	movq	%rcx, %r8
	addq	%rdx, %r12
#	rolq	$30, %r8
	movq	%r8, %rdi
	movq	%rcx, %r8
	xorq	%rsi, %rdi
#	rolq	$25, %r8
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
#	rorq	%rdi
#	rorq	$8, %rsi
	xorq	%rdi, %rsi
	movq	-96(%rsp), %rdi
	xorq	%r8, %rsi
	movq	-80(%rsp), %r8
	addq	%r14, %rsi
	addq	%r8, %rsi
	movq	%rdi, %r8
#	rolq	$3, %rdi
#	rorq	$19, %r8
	xorq	%r8, %rdi
	movq	-96(%rsp), %r8
	shrq	$6, %r8
	xorq	%r8, %rdi
	leaq	(%rsi,%rdi), %r8
	movq	%r12, %rsi
#	rorq	$14, %rsi
	movq	%r8, -80(%rsp)
	movq	%rsi, %rdi
	movq	%r12, %rsi
#	rorq	$18, %rsi
	xorq	%rdi, %rsi
	movq	%r12, %rdi
#	rolq	$23, %rdi
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
#	rorq	$28, %r9
	addq	%rdi, %rsi
	movq	%r9, %rdi
	movq	%rdx, %r9
	addq	%rsi, %rbp
#	rolq	$30, %r9
	movq	%r9, %r8
	movq	%rdx, %r9
	xorq	%rdi, %r8
#	rolq	$25, %r9
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
#	rorq	%rdi
	movq	%rdi, %r8
	movq	%r9, %rdi
	shrq	$7, %r9
#	rorq	$8, %rdi
	xorq	%r8, %rdi
	movq	-88(%rsp), %r8
	xorq	%r9, %rdi
	movq	-40(%rsp), %r9
	addq	%r9, %rdi
	movq	-72(%rsp), %r9
	addq	%r9, %rdi
	movq	%r8, %r9
#	rolq	$3, %r8
#	rorq	$19, %r9
	xorq	%r9, %r8
	movq	-88(%rsp), %r9
	shrq	$6, %r9
	xorq	%r9, %r8
	addq	%r8, %rdi
	movq	%rdi, %r9
	movq	%rbp, %rdi
#	rorq	$14, %rdi
	movq	%r9, -72(%rsp)
	movq	%rdi, %r8
	movq	%rbp, %rdi
#	rorq	$18, %rdi
	xorq	%r8, %rdi
	movq	%rbp, %r8
#	rolq	$23, %r8
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
#	rolq	$30, %r11
	movq	%rsi, %r8
#	rorq	$28, %r8
	movq	%r11, %r9
	movq	%rcx, %r11
	xorq	%r8, %r9
	movq	%rsi, %r8
#	rolq	$25, %r8
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
#	rorq	%r8
	movq	%r8, %r9
	movq	%r11, %r8
	shrq	$7, %r11
#	rorq	$8, %r8
	xorq	%r9, %r8
	movq	-80(%rsp), %r9
	xorq	%r11, %r8
	movq	-120(%rsp), %r11
	addq	%r11, %r8
	movq	-64(%rsp), %r11
	addq	%r11, %r8
	movq	%r9, %r11
#	rolq	$3, %r9
#	rorq	$19, %r11
	xorq	%r11, %r9
	movq	-80(%rsp), %r11
	shrq	$6, %r11
	xorq	%r11, %r9
	leaq	(%r8,%r9), %r11
	movq	%rbx, %r8
#	rorq	$14, %r8
	movq	%r11, -64(%rsp)
	movq	%r8, %r9
	movq	%rbx, %r8
#	rorq	$18, %r8
	xorq	%r9, %r8
	movq	%rbx, %r9
#	rolq	$23, %r9
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
#	rolq	$30, %r9
#	rorq	$28, %r8
	xorq	%r8, %r9
	movq	%rdi, %r8
#	rolq	$25, %r8
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
#	rorq	%r9
#	rorq	$8, %rcx
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
#	rolq	$3, %r9
#	rorq	$19, %r11
	xorq	%r11, %r9
	movq	-72(%rsp), %r11
	shrq	$6, %r11
	xorq	%r11, %r9
	addq	%r9, %rcx
	movq	%rcx, %r11
	movq	%r8, %rcx
#	rorq	$14, %rcx
	movq	%r11, -56(%rsp)
	movq	%rcx, %r9
	movq	%r8, %rcx
#	rorq	$18, %rcx
	xorq	%r9, %rcx
	movq	%r8, %r9
#	rolq	$23, %r9
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
#	rorq	$28, %r12
	movq	%r12, %r9
	movq	%rax, %r12
#	rolq	$30, %r12
	movq	%r12, %r11
	movq	%rax, %r12
	xorq	%r9, %r11
#	rolq	$25, %r12
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
#	rorq	%r12
	movq	%r12, %r11
	movq	%rdx, %r12
#	rorq	$8, %rdx
	xorq	%r11, %rdx
	shrq	$7, %r12
	movq	-64(%rsp), %r11
	xorq	%r12, %rdx
	movq	-104(%rsp), %r12
	addq	%r12, %rdx
	movq	-48(%rsp), %r12
	addq	%r12, %rdx
	movq	%r11, %r12
#	rolq	$3, %r11
#	rorq	$19, %r12
	xorq	%r12, %r11
	movq	-64(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %r11
	addq	%r11, %rdx
	movq	%rdx, %r12
	movq	%r9, %rdx
#	rorq	$14, %rdx
	movq	%r12, -48(%rsp)
	movq	%rdx, %r11
	movq	%r9, %rdx
#	rorq	$18, %rdx
	xorq	%r11, %rdx
	movq	%r9, %r11
#	rolq	$23, %r11
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
#	rorq	$28, %rbp
	addq	%r11, %rdx
	movq	%rbp, %r11
	movq	%rcx, %rbp
#	rolq	$30, %rbp
	xorq	%r11, %rbp
	movq	%rcx, %r11
#	rolq	$25, %r11
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
#	rorq	%rbp
#	rorq	$8, %rsi
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
#	rolq	$3, %rbp
#	rorq	$19, %r12
	xorq	%r12, %rbp
	movq	-56(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	addq	%rbp, %rsi
	movq	%rsi, %r12
	movq	%r11, %rsi
#	rorq	$14, %rsi
	movq	%r12, -32(%rsp)
	movq	%rsi, %rbp
	movq	%r11, %rsi
#	rorq	$18, %rsi
	xorq	%rbp, %rsi
	movq	%r11, %rbp
#	rolq	$23, %rbp
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
#	rolq	$30, %rbp
#	rorq	$28, %rbx
	xorq	%rbx, %rbp
	movq	%rdx, %rbx
#	rolq	$25, %rbx
	xorq	%rbx, %rbp
	movq	%rax, %rbx
	xorq	%rcx, %rbx
	andq	%rdx, %rbx
	xorq	%r12, %rbx
	addq	%rbx, %rbp
	leaq	(%rdi,%rsi), %rbx
	movq	%r13, %rdi
#	rorq	%rdi
	addq	%rbp, %rsi
	movq	%rdi, %rbp
	movq	%r13, %rdi
#	rorq	$8, %rdi
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
#	rolq	$3, %rbp
#	rorq	$19, %r12
	xorq	%r12, %rbp
	movq	-48(%rsp), %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	addq	%rbp, %rdi
	movq	%rbx, %rbp
	movq	%rdi, %r12
	movq	%rbx, %rdi
#	rorq	$14, %rbp
#	rorq	$18, %rdi
	movq	%r12, -24(%rsp)
	xorq	%rbp, %rdi
	movq	%rbx, %rbp
#	rolq	$23, %rbp
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
#	rolq	$30, %rbp
#	rorq	$28, %r8
	addq	%rdi, %rax
	xorq	%r8, %rbp
	movq	%rsi, %r8
#	rolq	$25, %r8
	xorq	%r8, %rbp
	movq	%rcx, %r8
	xorq	%rdx, %r8
	andq	%rsi, %r8
	xorq	%r12, %r8
	addq	%rbp, %r8
	movq	%r15, %rbp
	addq	%r8, %rdi
	movq	%r15, %r8
#	rorq	%rbp
#	rorq	$8, %r8
	xorq	%rbp, %r8
	movq	%r15, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %r8
	movq	-80(%rsp), %rbp
	addq	%rbp, %r8
	addq	%r13, %r8
	movq	-32(%rsp), %r13
	movq	%r13, %rbp
#	rorq	$19, %rbp
	movq	%rbp, %r12
	movq	%r13, %rbp
	shrq	$6, %r13
#	rolq	$3, %rbp
	xorq	%r12, %rbp
	movq	%rdx, %r12
	xorq	%r13, %rbp
	andq	%rsi, %r12
	leaq	(%r8,%rbp), %r13
	movq	%rax, %rbp
	movq	%rax, %r8
#	rorq	$14, %rbp
#	rorq	$18, %r8
	xorq	%rbp, %r8
	movq	%rax, %rbp
#	rolq	$23, %rbp
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
#	rorq	$28, %rbp
	addq	%r9, %r8
	movq	%rbp, %r9
	movq	%rdi, %rbp
	addq	%r8, %rcx
#	rolq	$30, %rbp
	xorq	%r9, %rbp
	movq	%rdi, %r9
#	rolq	$25, %r9
	xorq	%r9, %rbp
	movq	%rdx, %r9
	xorq	%rsi, %r9
	andq	%rdi, %r9
	xorq	%r12, %r9
	addq	%rbp, %r9
	movq	%r14, %rbp
	addq	%r9, %r8
	movq	%r14, %r9
#	rorq	%rbp
#	rorq	$8, %r9
	xorq	%rbp, %r9
	movq	%r14, %rbp
	shrq	$7, %rbp
	xorq	%rbp, %r9
	movq	-72(%rsp), %rbp
	addq	%rbp, %r9
	addq	%r15, %r9
	movq	-24(%rsp), %r15
	movq	%r15, %rbp
#	rorq	$19, %rbp
	movq	%rbp, %r12
	movq	%r15, %rbp
	shrq	$6, %r15
#	rolq	$3, %rbp
	xorq	%r12, %rbp
	movq	%rsi, %r12
	xorq	%r15, %rbp
	andq	%rdi, %r12
	leaq	(%r9,%rbp), %r15
	movq	%rcx, %rbp
	movq	%rcx, %r9
#	rorq	$14, %rbp
#	rorq	$18, %r9
	xorq	%rbp, %r9
	movq	%rcx, %rbp
#	rolq	$23, %rbp
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
#	rorq	$28, %rbp
	addq	%r11, %r9
	movq	%rbp, %r11
	movq	%r8, %rbp
	addq	%r9, %rdx
#	rolq	$30, %rbp
	xorq	%r11, %rbp
	movq	%r8, %r11
#	rolq	$25, %r11
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
#	rorq	%rbp
#	rorq	$8, %r11
	xorq	%rbp, %r11
	movq	%r12, %rbp
	movq	-64(%rsp), %r12
	shrq	$7, %rbp
	xorq	%rbp, %r11
	movq	%r13, %rbp
	addq	%r12, %r11
	movq	%r13, %r12
#	rolq	$3, %rbp
#	rorq	$19, %r12
	addq	%r14, %r11
	xorq	%r12, %rbp
	movq	%r13, %r12
	shrq	$6, %r12
	xorq	%r12, %rbp
	movq	%rdi, %r12
	leaq	(%r11,%rbp), %r14
	movq	%rdx, %rbp
	movq	%rdx, %r11
#	rorq	$14, %rbp
#	rorq	$18, %r11
	xorq	%rbp, %r11
	movq	%rdx, %rbp
#	rolq	$23, %rbp
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
#	rolq	$30, %rbp
#	rorq	$28, %rbx
	xorq	%rbx, %rbp
	movq	%r9, %rbx
#	rolq	$25, %rbx
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
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
