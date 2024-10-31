	.file	"bench_sha512_plain.c"
	.text
	.p2align 4
	.type	sha512_block_data_order, @function
sha512_block_data_order:
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
	.p2align 4,,10
	.p2align 3
.L3:
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
.L2:
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
	jne	.L2
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
	jne	.L3
	addq	$72, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.size	sha512_block_data_order, .-sha512_block_data_order
	.p2align 4
	.globl	OPENSSL_cleanse
	.type	OPENSSL_cleanse, @function
OPENSSL_cleanse:
	testq	%rsi, %rsi
	jne	.L17
	ret
	.p2align 4,,10
	.p2align 3
.L17:
	subq	$8, %rsp
	movq	%rsi, %rdx
	xorl	%esi, %esi
	call	memset
	addq	$8, %rsp
	ret
	.size	OPENSSL_cleanse, .-OPENSSL_cleanse
	.p2align 4
	.globl	CRYPTO_memcmp
	.type	CRYPTO_memcmp, @function
CRYPTO_memcmp:
	movq	%rdi, %rcx
	movq	%rdx, %rdi
	testq	%rdx, %rdx
	je	.L25
	leaq	-1(%rdx), %rax
	cmpq	$14, %rax
	jbe	.L26
	andq	$-16, %rdx
	xorl	%eax, %eax
	pxor	%xmm1, %xmm1
	.p2align 4,,10
	.p2align 3
.L21:
	movdqu	(%rcx,%rax), %xmm0
	movdqu	(%rsi,%rax), %xmm3
	addq	$16, %rax
	pxor	%xmm3, %xmm0
	por	%xmm0, %xmm1
	cmpq	%rax, %rdx
	jne	.L21
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
	je	.L22
.L20:
	movq	%rdi, %r9
	subq	%r8, %r9
	leaq	-1(%r9), %rdx
	cmpq	$6, %rdx
	jbe	.L23
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
	je	.L22
.L23:
	movzbl	(%rcx,%r8), %edx
	xorb	(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	1(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	1(%rsi,%r8), %edx
	xorb	1(%rcx,%r8), %dl
	orl	%edx, %eax
	leaq	2(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	2(%rcx,%r8), %edx
	xorb	2(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	3(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	3(%rcx,%r8), %edx
	xorb	3(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	4(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	4(%rcx,%r8), %edx
	xorb	4(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	5(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	5(%rcx,%r8), %edx
	xorb	5(%rsi,%r8), %dl
	orl	%edx, %eax
	leaq	6(%r8), %rdx
	cmpq	%rdi, %rdx
	jnb	.L22
	movzbl	6(%rcx,%r8), %edx
	xorb	6(%rsi,%r8), %dl
	orl	%edx, %eax
.L22:
	movzbl	%al, %eax
	ret
	.p2align 4,,10
	.p2align 3
.L25:
	xorl	%eax, %eax
	ret
.L26:
	pxor	%xmm1, %xmm1
	xorl	%r8d, %r8d
	xorl	%eax, %eax
	jmp	.L20
	.size	CRYPTO_memcmp, .-CRYPTO_memcmp
	.p2align 4
	.globl	SHA512_Init
	.type	SHA512_Init, @function
SHA512_Init:
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
	.size	SHA512_Init, .-SHA512_Init
	.p2align 4
	.globl	SHA512_Update
	.type	SHA512_Update, @function
SHA512_Update:
	testq	%rdx, %rdx
	je	.L76
	pushq	%r14
	leaq	0(,%rdx,8), %rax
	pushq	%r13
	leaq	80(%rdi), %r13
	pushq	%rbp
	movq	%rdx, %rbp
	pushq	%rbx
	movq	%rdi, %rbx
	subq	$24, %rsp
	movq	72(%rdi), %rcx
	addq	64(%rdi), %rax
	movq	%rax, 64(%rdi)
	movl	208(%rdi), %eax
	adcq	$0, %rcx
	shrq	$61, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 72(%rdi)
	testl	%eax, %eax
	je	.L80
	movl	$128, %ecx
	leaq	0(%r13,%rax), %r8
	subq	%rax, %rcx
	cmpq	%rcx, %rbp
	jb	.L81
	leaq	-128(%rax,%rbp), %r14
	leaq	(%rsi,%rcx), %rbp
	testq	%rcx, %rcx
	jne	.L82
	movl	$0, 208(%rdi)
	movl	$1, %edx
	movq	%r13, %rsi
	call	sha512_block_data_order
	cmpq	$127, %r14
	jbe	.L79
.L56:
	movq	%rbp, %rsi
	movq	%r14, %rbp
	.p2align 4,,10
	.p2align 3
.L43:
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
	jmp	.L50
	.p2align 4,,10
	.p2align 3
.L80:
	cmpq	$127, %rbp
	ja	.L43
.L42:
	movl	%ebp, %ecx
	cmpl	$8, %ebp
	jb	.L83
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
.L53:
	movl	%ebp, 208(%rbx)
.L63:
	addq	$24, %rsp
	movl	$1, %eax
	popq	%rbx
	popq	%rbp
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
	.p2align 3
.L76:
	movl	$1, %eax
	ret
	.p2align 4,,10
	.p2align 3
.L82:
	cmpq	$8, %rcx
	jnb	.L46
	testb	$4, %cl
	jne	.L84
	testq	%rcx, %rcx
	jne	.L85
.L47:
	movl	$0, 208(%rbx)
	movl	$1, %edx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	call	sha512_block_data_order
	cmpq	$127, %r14
	ja	.L56
.L50:
	testq	%r14, %r14
	je	.L63
	.p2align 4,,10
	.p2align 3
.L79:
	movq	%rbp, %rsi
	movq	%r14, %rbp
	jmp	.L42
	.p2align 4,,10
	.p2align 3
.L83:
	testb	$4, %bpl
	jne	.L86
	testl	%ecx, %ecx
	je	.L53
	movzbl	(%rsi), %eax
	movb	%al, 80(%rbx)
	testb	$2, %cl
	je	.L53
	movzwl	-2(%rsi,%rcx), %eax
	movw	%ax, -2(%r13,%rcx)
	jmp	.L53
	.p2align 4,,10
	.p2align 3
.L81:
	movq	%rbp, %rdx
	movq	%r8, %rdi
	call	memcpy
	addl	%ebp, 208(%rbx)
	jmp	.L63
	.p2align 4,,10
	.p2align 3
.L46:
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
	jmp	.L47
	.p2align 4,,10
	.p2align 3
.L85:
	movzbl	(%rsi), %eax
	movb	%al, (%r8)
	testb	$2, %cl
	je	.L47
	movzwl	-2(%rsi,%rcx), %eax
	movw	%ax, -2(%r8,%rcx)
	jmp	.L47
.L86:
	movl	(%rsi), %eax
	movl	%eax, 80(%rbx)
	movl	-4(%rsi,%rcx), %eax
	movl	%eax, -4(%r13,%rcx)
	jmp	.L53
.L84:
	movl	(%rsi), %eax
	movl	%eax, (%r8)
	movl	-4(%rsi,%rcx), %eax
	movl	%eax, -4(%r8,%rcx)
	jmp	.L47
	.size	SHA512_Update, .-SHA512_Update
	.p2align 4
	.globl	SHA512_Final
	.type	SHA512_Final, @function
SHA512_Final:
	pushq	%r13
	leaq	80(%rsi), %r13
	pushq	%r12
	pushq	%rbp
	movq	%rdi, %rbp
	pushq	%rbx
	movq	%rsi, %rbx
	subq	$8, %rsp
	movl	208(%rsi), %eax
	movl	212(%rsi), %r12d
	movb	$-128, 80(%rsi,%rax)
	addq	$1, %rax
	leaq	0(%r13,%rax), %rsi
	cmpq	$112, %rax
	jbe	.L88
	movl	$128, %edx
	subq	%rax, %rdx
	jne	.L148
.L89:
	movl	$1, %edx
	movq	%r13, %rsi
	movq	%rbx, %rdi
	call	sha512_block_data_order
	movq	%r13, %rsi
	movl	$112, %edx
.L96:
	movl	%edx, %ecx
	cmpl	$8, %edx
	jb	.L149
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
.L97:
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
	je	.L106
	shrq	$3, %r12
	je	.L105
	movq	(%rbx), %rax
	bswap	%rax
	movq	%rax, 0(%rbp)
	cmpq	$1, %r12
	je	.L105
	movq	8(%rbx), %rax
	bswap	%rax
	movq	%rax, 8(%rbp)
	cmpq	$2, %r12
	je	.L105
	movq	16(%rbx), %rax
	bswap	%rax
	movq	%rax, 16(%rbp)
	cmpq	$3, %r12
	je	.L105
	movq	24(%rbx), %rax
	bswap	%rax
	movq	%rax, 24(%rbp)
	cmpq	$4, %r12
	je	.L105
	movq	32(%rbx), %rax
	bswap	%rax
	movq	%rax, 32(%rbp)
	cmpq	$5, %r12
	je	.L105
	movq	40(%rbx), %rax
	bswap	%rax
	movq	%rax, 40(%rbp)
	cmpq	$6, %r12
	je	.L105
	movq	48(%rbx), %rax
	bswap	%rax
	movq	%rax, 48(%rbp)
	cmpq	$7, %r12
	je	.L105
	movq	56(%rbx), %rax
	bswap	%rax
	movq	%rax, 56(%rbp)
.L105:
	addq	$8, %rsp
	movl	$1, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
	.p2align 3
.L149:
	andl	$4, %edx
	jne	.L150
	testl	%ecx, %ecx
	je	.L97
	movb	$0, (%rsi)
	testb	$2, %cl
	je	.L97
	xorl	%eax, %eax
	movw	%ax, -2(%rsi,%rcx)
	jmp	.L97
	.p2align 4,,10
	.p2align 3
.L148:
	xorl	%edi, %edi
	cmpq	$8, %rdx
	jb	.L151
	leaq	8(%rsi), %rcx
	movq	$0, (%rsi)
	movq	$0, -8(%rsi,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rsi
	addq	%rsi, %rdx
	andq	$-8, %rdx
	cmpq	$8, %rdx
	jb	.L89
	andq	$-8, %rdx
	xorl	%eax, %eax
.L94:
	movq	%rdi, (%rcx,%rax)
	addq	$8, %rax
	cmpq	%rdx, %rax
	jb	.L94
	jmp	.L89
	.p2align 4,,10
	.p2align 3
.L88:
	movl	$112, %edx
	subq	%rax, %rdx
	je	.L97
	jmp	.L96
	.p2align 4,,10
	.p2align 3
.L151:
	testb	$4, %dl
	jne	.L152
	testq	%rdx, %rdx
	je	.L89
	movb	$0, (%rsi)
	testb	$2, %dl
	je	.L89
	xorl	%ecx, %ecx
	movw	%cx, -2(%rsi,%rdx)
	jmp	.L89
	.p2align 4,,10
	.p2align 3
.L106:
	addq	$8, %rsp
	xorl	%eax, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L150:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rcx)
	jmp	.L97
.L152:
	movl	$0, (%rsi)
	movl	$0, -4(%rsi,%rdx)
	jmp	.L89
	.size	SHA512_Final, .-SHA512_Final
	.p2align 4
	.globl	SHA512
	.type	SHA512, @function
SHA512:
	pushq	%r13
	pushq	%r12
	movq	%rdx, %r12
	pushq	%rbp
	pushq	%rbx
	subq	$232, %rsp
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
	je	.L154
	movq	%rsi, %rax
	movq	%rdi, %rbp
	movq	%rsi, %rbx
	movq	%rsp, %r13
	shrq	$61, %rax
	movq	%rax, 72(%rsp)
	leaq	0(,%rsi,8), %rax
	movq	%rax, 64(%rsp)
	cmpq	$127, %rsi
	ja	.L236
.L155:
	leaq	80(%rsp), %r8
	movl	%ebx, %edx
	movq	%r8, %rdi
	cmpl	$8, %ebx
	jnb	.L237
	xorl	%eax, %eax
	testb	$4, %dl
	jne	.L238
.L157:
	testb	$2, %dl
	jne	.L239
.L158:
	andl	$1, %edx
	jne	.L240
.L159:
	movl	%ebx, 208(%rsp)
	movl	212(%rsp), %ebp
	movb	$-128, 80(%rsp,%rbx)
	addq	$1, %rbx
	cmpq	$112, %rbx
	jbe	.L160
	movl	$128, %eax
	subq	%rbx, %rax
	jne	.L241
.L161:
	movq	%r8, %rsi
	movl	$1, %edx
	movq	%r13, %rdi
	call	sha512_block_data_order
	leaq	80(%rsp), %r8
	movl	$112, %eax
	movq	%r8, %rbx
.L168:
	movl	%eax, %ecx
	cmpl	$8, %eax
	jb	.L242
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
.L169:
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
	je	.L175
	shrq	$3, %rbp
	je	.L175
	movq	(%rsp), %rax
	bswap	%rax
	movq	%rax, (%r12)
	cmpq	$1, %rbp
	je	.L175
	movq	8(%rsp), %rax
	bswap	%rax
	movq	%rax, 8(%r12)
	cmpq	$2, %rbp
	je	.L175
	movq	16(%rsp), %rax
	bswap	%rax
	movq	%rax, 16(%r12)
	cmpq	$3, %rbp
	je	.L175
	movq	24(%rsp), %rax
	bswap	%rax
	movq	%rax, 24(%r12)
	cmpq	$4, %rbp
	je	.L175
	movq	32(%rsp), %rax
	bswap	%rax
	movq	%rax, 32(%r12)
	cmpq	$5, %rbp
	je	.L175
	movq	40(%rsp), %rax
	bswap	%rax
	movq	%rax, 40(%r12)
	cmpq	$6, %rbp
	je	.L175
	movq	48(%rsp), %rax
	bswap	%rax
	movq	%rax, 48(%r12)
	cmpq	$7, %rbp
	je	.L175
	movq	56(%rsp), %rax
	bswap	%rax
	movq	%rax, 56(%r12)
.L175:
	xorl	%eax, %eax
	movq	%r13, %rdi
	movl	$27, %ecx
	rep stosq
	addq	$232, %rsp
	movq	%r12, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
	.p2align 3
.L236:
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	%r13, %rdi
	shrq	$7, %rdx
	call	sha512_block_data_order
	movq	%rbx, %rax
	andl	$127, %eax
	jne	.L243
.L154:
	movb	$-128, 80(%rsp)
	movl	$64, %ebp
	movl	$111, %eax
	movl	$1, %ebx
	leaq	80(%rsp), %r8
.L177:
	addq	%r8, %rbx
	jmp	.L168
	.p2align 4,,10
	.p2align 3
.L240:
	movzbl	0(%rbp,%rax), %edx
	movb	%dl, (%rdi,%rax)
	jmp	.L159
	.p2align 4,,10
	.p2align 3
.L239:
	movzwl	0(%rbp,%rax), %ecx
	movw	%cx, (%rdi,%rax)
	addq	$2, %rax
	andl	$1, %edx
	je	.L159
	jmp	.L240
	.p2align 4,,10
	.p2align 3
.L238:
	movl	0(%rbp), %eax
	movl	%eax, (%rdi)
	movl	$4, %eax
	testb	$2, %dl
	je	.L158
	jmp	.L239
	.p2align 4,,10
	.p2align 3
.L242:
	testb	$4, %al
	jne	.L244
	testl	%ecx, %ecx
	je	.L169
	movb	$0, (%rbx)
	testb	$2, %cl
	je	.L169
	xorl	%eax, %eax
	movw	%ax, -2(%rbx,%rcx)
	jmp	.L169
	.p2align 4,,10
	.p2align 3
.L241:
	addq	%r8, %rbx
	xorl	%edi, %edi
	cmpl	$8, %eax
	jb	.L245
	leaq	8(%rbx), %rcx
	movl	%eax, %edx
	movq	$0, (%rbx)
	movq	$0, -8(%rbx,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rbx
	addl	%ebx, %eax
	andl	$-8, %eax
	cmpl	$8, %eax
	jb	.L161
	andl	$-8, %eax
	xorl	%edx, %edx
.L166:
	movl	%edx, %esi
	addl	$8, %edx
	movq	%rdi, (%rcx,%rsi)
	cmpl	%eax, %edx
	jb	.L166
	jmp	.L161
	.p2align 4,,10
	.p2align 3
.L237:
	movl	%ebx, %ecx
	movq	%rbp, %rsi
	xorl	%eax, %eax
	shrl	$3, %ecx
	rep movsq
	movq	%rsi, %rbp
	testb	$4, %dl
	je	.L157
	jmp	.L238
	.p2align 4,,10
	.p2align 3
.L245:
	testb	$4, %al
	jne	.L246
	testl	%eax, %eax
	je	.L161
	movb	$0, (%rbx)
	testb	$2, %al
	je	.L161
	movl	%eax, %eax
	xorl	%edx, %edx
	movw	%dx, -2(%rbx,%rax)
	jmp	.L161
.L243:
	subq	%rax, %rbx
	addq	%rbx, %rbp
	movq	%rax, %rbx
	jmp	.L155
.L244:
	movl	$0, (%rbx)
	movl	$0, -4(%rbx,%rcx)
	jmp	.L169
.L246:
	movl	%eax, %eax
	movl	$0, (%rbx)
	movl	$0, -4(%rbx,%rax)
	jmp	.L161
.L160:
	movl	$112, %eax
	subq	%rbx, %rax
	je	.L169
	jmp	.L177
	.size	SHA512, .-SHA512
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
	pushq	%r15
	xorl	%ecx, %ecx
	pushq	%r14
	movl	%ecx, %edx
	pushq	%r13
	pushq	%r12
	pushq	%rbp
	pushq	%rbx
	subq	$360, %rsp
	movq	$1296236545, 32(%rsp)
	leaq	32(%rsp), %rax
	movq	$message, 40(%rsp)
	movq	$256, 48(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
#APP
# 263 "/root/benchmarks/src/boringssl_sha512/bench_sha512_plain.c" 1
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
	movq	$out, 88(%rsp)
	movq	$64, 96(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 120(%rsp)
#APP
# 264 "/root/benchmarks/src/boringssl_sha512/bench_sha512_plain.c" 1
	rolq $3,  %rdi ; rolq $13, %rdi
	rolq $61, %rdi ; rolq $51, %rdi
	xchgq %rbx,%rbx
# 0 "" 2
#NO_APP
	movq	%rdx, 16(%rsp)
	movq	16(%rsp), %rax
	leaq	208(%rsp), %r15
	movl	$10, %ebp
	movq	$256, 24(%rsp)
	movdqa	.LC0(%rip), %xmm6
	pxor	%xmm1, %xmm1
	movq	%r15, %r13
	movdqa	.LC1(%rip), %xmm5
	movq	%r15, %r12
	movdqa	.LC2(%rip), %xmm4
	movdqa	.LC3(%rip), %xmm3
	movq	.LC4(%rip), %xmm2
	.p2align 4,,10
	.p2align 3
.L269:
	movq	24(%rsp), %rbx
	movaps	%xmm6, 128(%rsp)
	movaps	%xmm5, 144(%rsp)
	movaps	%xmm4, 160(%rsp)
	movaps	%xmm3, 176(%rsp)
	movaps	%xmm1, 192(%rsp)
	movq	%xmm2, 336(%rsp)
	testq	%rbx, %rbx
	je	.L248
	movq	%rbx, %rax
	movl	$message, %esi
	shrq	$61, %rax
	movq	%rax, 200(%rsp)
	leaq	0(,%rbx,8), %rax
	movq	%rax, 192(%rsp)
	cmpq	$127, %rbx
	ja	.L329
.L249:
	movl	%ebx, %edx
	movq	%r15, %rdi
	cmpl	$8, %ebx
	jb	.L250
	movl	%ebx, %ecx
	shrl	$3, %ecx
	rep movsq
.L250:
	xorl	%eax, %eax
	testb	$4, %dl
	je	.L251
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	movl	$4, %eax
.L251:
	testb	$2, %dl
	je	.L252
	movzwl	(%rsi,%rax), %ecx
	movw	%cx, (%rdi,%rax)
	addq	$2, %rax
.L252:
	andl	$1, %edx
	je	.L253
	movzbl	(%rsi,%rax), %edx
	movb	%dl, (%rdi,%rax)
.L253:
	movl	%ebx, 336(%rsp)
	movl	340(%rsp), %r14d
	movb	$-128, 208(%rsp,%rbx)
	addq	$1, %rbx
	cmpq	$112, %rbx
	jbe	.L254
	movl	$128, %eax
	subq	%rbx, %rax
	je	.L255
	addq	%r15, %rbx
	xorl	%edi, %edi
	cmpl	$8, %eax
	jnb	.L256
	testb	$4, %al
	jne	.L330
	testl	%eax, %eax
	je	.L255
	movb	$0, (%rbx)
	testb	$2, %al
	jne	.L331
	.p2align 4,,10
	.p2align 3
.L255:
	movq	%r15, %rsi
	movl	$1, %edx
	shrq	$3, %r14
	movq	%r13, %r15
	leaq	128(%rsp), %rdi
	movq	%r12, %rbx
	call	sha512_block_data_order
	movl	$112, %eax
	movl	%eax, %ecx
	cmpl	$8, %eax
	jnb	.L264
.L335:
	testb	$4, %al
	jne	.L332
	testl	%ecx, %ecx
	je	.L263
	movb	$0, (%rbx)
	testb	$2, %cl
	jne	.L333
.L263:
	movq	200(%rsp), %rax
	movq	192(%rsp), %rdx
	movq	%r15, %rsi
	leaq	128(%rsp), %rdi
	bswap	%rdx
	bswap	%rax
	movq	%rdx, %xmm7
	movq	%rax, %xmm0
	punpcklqdq	%xmm7, %xmm0
	movl	$1, %edx
	movaps	%xmm0, 320(%rsp)
	call	sha512_block_data_order
	testq	%r14, %r14
	je	.L271
	movq	128(%rsp), %rax
	bswap	%rax
	movq	%rax, out(%rip)
	cmpq	$1, %r14
	je	.L271
	movq	136(%rsp), %rax
	bswap	%rax
	movq	%rax, out+8(%rip)
	cmpq	$2, %r14
	je	.L271
	movq	144(%rsp), %rax
	bswap	%rax
	movq	%rax, out+16(%rip)
	cmpq	$3, %r14
	je	.L271
	movq	152(%rsp), %rax
	bswap	%rax
	movq	%rax, out+24(%rip)
	cmpq	$4, %r14
	je	.L271
	movq	160(%rsp), %rax
	bswap	%rax
	movq	%rax, out+32(%rip)
	cmpq	$5, %r14
	je	.L271
	movq	168(%rsp), %rax
	bswap	%rax
	movq	%rax, out+40(%rip)
	cmpq	$6, %r14
	je	.L271
	movq	176(%rsp), %rax
	bswap	%rax
	movq	%rax, out+48(%rip)
	cmpq	$7, %r14
	je	.L271
	movq	184(%rsp), %rax
	bswap	%rax
	movq	%rax, out+56(%rip)
.L271:
	subl	$1, %ebp
	jne	.L269
	addq	$360, %rsp
	xorl	%eax, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
	.p2align 3
.L329:
	movq	%rbx, %rdx
	leaq	128(%rsp), %rdi
	shrq	$7, %rdx
	call	sha512_block_data_order
	movq	%rbx, %rax
	andl	$127, %eax
	jne	.L334
.L248:
	movl	$1, %ebx
	movb	$-128, 208(%rsp)
	movl	$111, %eax
	movl	$8, %r14d
.L272:
	addq	%r15, %rbx
	movl	%eax, %ecx
	cmpl	$8, %eax
	jb	.L335
	.p2align 4,,10
	.p2align 3
.L264:
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
	jmp	.L263
	.p2align 4,,10
	.p2align 3
.L256:
	leaq	8(%rbx), %rcx
	movl	%eax, %edx
	movq	$0, (%rbx)
	movq	$0, -8(%rbx,%rdx)
	andq	$-8, %rcx
	subq	%rcx, %rbx
	addl	%ebx, %eax
	andl	$-8, %eax
	cmpl	$8, %eax
	jb	.L255
	andl	$-8, %eax
	xorl	%edx, %edx
.L260:
	movl	%edx, %esi
	addl	$8, %edx
	movq	%rdi, (%rcx,%rsi)
	cmpl	%eax, %edx
	jb	.L260
	jmp	.L255
.L334:
	movq	%rbx, %rsi
	movq	%rax, %rbx
	subq	%rax, %rsi
	addq	$message, %rsi
	jmp	.L249
.L332:
	xorl	%eax, %eax
	movl	%eax, (%rbx)
	movl	%eax, -4(%rbx,%rcx)
	jmp	.L263
.L331:
	movl	%eax, %eax
	movw	$0, -2(%rbx,%rax)
	jmp	.L255
.L330:
	xorl	%edx, %edx
	movl	%eax, %eax
	xorl	%ecx, %ecx
	movl	%edx, (%rbx)
	movl	%ecx, -4(%rbx,%rax)
	jmp	.L255
.L333:
	movw	$0, -2(%rbx,%rcx)
	jmp	.L263
.L254:
	movl	$112, %eax
	shrq	$3, %r14
	subq	%rbx, %rax
	je	.L263
	jmp	.L272
	.size	main, .-main
	.globl	out
	.section	secret,"aw"
	.align 32
	.type	out, @object
	.size	out, 64
out:
	.zero	64
	.globl	message
	.align 32
	.type	message, @object
	.size	message, 256
message:
	.string	"\006\343\024\201\017\353`\233\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703\364\356\330|\005\030\375\351x+`\255D\326`\027\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253\361\276\342\0253)\360\206^\002c\252\005\264\364\341^\002c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347H<\b\035\267\370\255\235\253"
	.zero	127
	.section	.rodata
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
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
