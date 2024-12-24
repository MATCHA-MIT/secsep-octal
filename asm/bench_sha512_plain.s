	.text
	.file	"bench_sha512_plain.c"
	.globl	OPENSSL_cleanse                 # -- Begin function OPENSSL_cleanse
	.p2align	4, 0x90
	.type	OPENSSL_cleanse,@function
OPENSSL_cleanse:                        # @OPENSSL_cleanse
# %bb.0:
	pushq	%rbx
	movq	%rdi, %rbx
	testq	%rsi, %rsi
	je	.LBB0_2
# %bb.1:
	movq	%rsi, %rdx
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB0_2:
	#APP
	#NO_APP
	popq	%rbx
	retq
.Lfunc_end0:
	.size	OPENSSL_cleanse, .Lfunc_end0-OPENSSL_cleanse
                                        # -- End function
	.globl	CRYPTO_memcmp                   # -- Begin function CRYPTO_memcmp
	.p2align	4, 0x90
	.type	CRYPTO_memcmp,@function
CRYPTO_memcmp:                          # @CRYPTO_memcmp
# %bb.0:
	testq	%rdx, %rdx
	je	.LBB1_1
# %bb.2:
	cmpq	$8, %rdx
	jae	.LBB1_5
# %bb.3:
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	jmp	.LBB1_4
.LBB1_1:
	xorl	%eax, %eax
	retq
.LBB1_5:
	cmpq	$32, %rdx
	jae	.LBB1_7
# %bb.6:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	jmp	.LBB1_11
.LBB1_7:
	movq	%rdx, %rax
	andq	$-32, %rax
	pxor	%xmm0, %xmm0
	xorl	%ecx, %ecx
	pxor	%xmm1, %xmm1
	.p2align	4, 0x90
.LBB1_8:                                # =>This Inner Loop Header: Depth=1
	movdqu	(%rdi,%rcx), %xmm2
	movdqu	16(%rdi,%rcx), %xmm3
	movdqu	(%rsi,%rcx), %xmm4
	pxor	%xmm2, %xmm4
	por	%xmm4, %xmm0
	movdqu	16(%rsi,%rcx), %xmm2
	pxor	%xmm3, %xmm2
	por	%xmm2, %xmm1
	addq	$32, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_8
# %bb.9:
	por	%xmm0, %xmm1
	pshufd	$238, %xmm1, %xmm0              # xmm0 = xmm1[2,3,2,3]
	por	%xmm1, %xmm0
	pshufd	$85, %xmm0, %xmm1               # xmm1 = xmm0[1,1,1,1]
	por	%xmm0, %xmm1
	movdqa	%xmm1, %xmm0
	psrld	$16, %xmm0
	por	%xmm1, %xmm0
	movdqa	%xmm0, %xmm1
	psrlw	$8, %xmm1
	por	%xmm0, %xmm1
	movd	%xmm1, %ecx
	cmpq	%rdx, %rax
	je	.LBB1_14
# %bb.10:
	testb	$24, %dl
	je	.LBB1_4
.LBB1_11:
	movq	%rax, %r8
	movq	%rdx, %rax
	andq	$-8, %rax
	movzbl	%cl, %ecx
	movd	%ecx, %xmm0
	.p2align	4, 0x90
.LBB1_12:                               # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%r8), %rcx
	xorq	(%rdi,%r8), %rcx
	movq	%rcx, %xmm1
	por	%xmm1, %xmm0
	addq	$8, %r8
	cmpq	%r8, %rax
	jne	.LBB1_12
# %bb.13:
	pshufd	$85, %xmm0, %xmm1               # xmm1 = xmm0[1,1,1,1]
	por	%xmm0, %xmm1
	movdqa	%xmm1, %xmm0
	psrld	$16, %xmm0
	por	%xmm1, %xmm0
	movdqa	%xmm0, %xmm1
	psrlw	$8, %xmm1
	por	%xmm0, %xmm1
	movd	%xmm1, %ecx
	cmpq	%rdx, %rax
	je	.LBB1_14
	.p2align	4, 0x90
.LBB1_4:                                # =>This Inner Loop Header: Depth=1
	movzbl	(%rsi,%rax), %r8d
	xorb	(%rdi,%rax), %r8b
	orb	%r8b, %cl
	incq	%rax
	cmpq	%rax, %rdx
	jne	.LBB1_4
.LBB1_14:
	movzbl	%cl, %eax
	retq
.Lfunc_end1:
	.size	CRYPTO_memcmp, .Lfunc_end1-CRYPTO_memcmp
                                        # -- End function
	.globl	SHA512_Init                     # -- Begin function SHA512_Init
	.p2align	4, 0x90
	.type	SHA512_Init,@function
SHA512_Init:                            # @SHA512_Init
# %bb.0:
	movabsq	$7640891576956012808, %rax      # imm = 0x6A09E667F3BCC908
	movq	%rax, (%rdi)
	movabsq	$-4942790177534073029, %rax     # imm = 0xBB67AE8584CAA73B
	movq	%rax, 8(%rdi)
	movabsq	$4354685564936845355, %rax      # imm = 0x3C6EF372FE94F82B
	movq	%rax, 16(%rdi)
	movabsq	$-6534734903238641935, %rax     # imm = 0xA54FF53A5F1D36F1
	movq	%rax, 24(%rdi)
	movabsq	$5840696475078001361, %rax      # imm = 0x510E527FADE682D1
	movq	%rax, 32(%rdi)
	movabsq	$-7276294671716946913, %rax     # imm = 0x9B05688C2B3E6C1F
	movq	%rax, 40(%rdi)
	movabsq	$2270897969802886507, %rax      # imm = 0x1F83D9ABFB41BD6B
	movq	%rax, 48(%rdi)
	movabsq	$6620516959819538809, %rax      # imm = 0x5BE0CD19137E2179
	movq	%rax, 56(%rdi)
	movabsq	$274877906944, %rax             # imm = 0x4000000000
	movq	%rax, 208(%rdi)
	xorps	%xmm0, %xmm0
	movups	%xmm0, 64(%rdi)
	movl	$1, %eax
	retq
.Lfunc_end2:
	.size	SHA512_Init, .Lfunc_end2-SHA512_Init
                                        # -- End function
	.globl	SHA512_Update                   # -- Begin function SHA512_Update
	.p2align	4, 0x90
	.type	SHA512_Update,@function
SHA512_Update:                          # @SHA512_Update
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	testq	%rdx, %rdx
	je	.LBB3_12
# %bb.1:
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	leaq	(,%rdx,8), %rax
	movq	%rdx, %rcx
	shrq	$61, %rcx
	addq	%rax, 64(%rdi)
	adcq	%rcx, 72(%rdi)
	leaq	80(%rdi), %r12
	movl	208(%rdi), %edi
	testq	%rdi, %rdi
	je	.LBB3_7
# %bb.2:
	movl	$128, %r13d
	subq	%rdi, %r13
	cmpq	%rbx, %r13
	jbe	.LBB3_4
# %bb.3:
	addq	%rdi, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy@PLT
	addl	208(%r14), %ebx
	jmp	.LBB3_11
.LBB3_4:
	cmpl	$128, %edi
	je	.LBB3_6
# %bb.5:
	addq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	memcpy@PLT
.LBB3_6:
	movl	$0, 208(%r14)
	subq	%r13, %rbx
	addq	%r13, %r15
	movl	$1, %edx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	sha512_block_data_order
.LBB3_7:
	cmpq	$128, %rbx
	jb	.LBB3_9
# %bb.8:
	movq	%rbx, %rdx
	shrq	$7, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	addq	%rbx, %r15
	andl	$127, %ebx
	subq	%rbx, %r15
.LBB3_9:
	testq	%rbx, %rbx
	je	.LBB3_12
# %bb.10:
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy@PLT
.LBB3_11:
	movl	%ebx, 208(%r14)
.LBB3_12:
	movl	$1, %eax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Lfunc_end3:
	.size	SHA512_Update, .Lfunc_end3-SHA512_Update
                                        # -- End function
	.p2align	4, 0x90                         # -- Begin function sha512_block_data_order
	.type	sha512_block_data_order,@function
sha512_block_data_order:                # @sha512_block_data_order
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$200, %rsp
	movq	%rdi, 56(%rsp)                  # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB4_5
# %bb.1:
	movq	56(%rsp), %rax                  # 8-byte Reload
	movq	(%rax), %r13
	movq	8(%rax), %r14
	movq	16(%rax), %r10
	movq	24(%rax), %rdi
	movq	32(%rax), %rbp
	movq	40(%rax), %r12
	movq	48(%rax), %r11
	movq	56(%rax), %r8
	.p2align	4, 0x90
.LBB4_2:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_3 Depth 2
	movq	%rdx, 160(%rsp)                 # 8-byte Spill
	movq	(%rsi), %r9
	bswapq	%r9
	movq	%rbp, %rax
	rolq	$50, %rax
	movq	8(%rsi), %rbx
	movq	%rsi, %r15
	movq	%rbp, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%rbp, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r12, %rdx
	andq	%rbp, %rdx
	movq	%rbp, %rcx
	notq	%rcx
	andq	%r11, %rcx
	movq	%r8, 88(%rsp)                   # 8-byte Spill
	addq	%r8, %rdx
	addq	%rcx, %rdx
	addq	%rax, %rdx
	movq	%r13, %rax
	rolq	$36, %rax
	movabsq	$4794697086780616226, %rcx      # imm = 0x428A2F98D728AE22
	addq	%rcx, %rdx
	movq	%r13, %rcx
	rolq	$30, %rcx
	movq	%r9, -16(%rsp)                  # 8-byte Spill
	addq	%r9, %rdx
	movq	%r13, %rsi
	rolq	$25, %rsi
	xorq	%rax, %rcx
	xorq	%rcx, %rsi
	movq	%r10, %rax
	xorq	%r14, %rax
	andq	%r13, %rax
	movq	%r10, %r8
	andq	%r14, %r8
	xorq	%rax, %r8
	addq	%rsi, %r8
	addq	%rdx, %r8
	bswapq	%rbx
	movq	%rdi, 120(%rsp)                 # 8-byte Spill
	addq	%rdi, %rdx
	movq	%rdx, %rcx
	rolq	$50, %rcx
	movq	%r14, %rax
	movq	%rdx, %rsi
	rolq	$46, %rsi
	andq	%r13, %rax
	movq	%rdx, %rdi
	rolq	$23, %rdi
	xorq	%rcx, %rsi
	xorq	%rsi, %rdi
	movq	%rdx, %r9
	andq	%rbp, %r9
	movq	%rdx, %rcx
	notq	%rcx
	andq	%r12, %rcx
	movq	%r11, 96(%rsp)                  # 8-byte Spill
	movq	%rbx, 48(%rsp)                  # 8-byte Spill
	addq	%r11, %r9
	addq	%rbx, %r9
	addq	%rcx, %r9
	movabsq	$8158064640168781261, %rcx      # imm = 0x7137449123EF65CD
	addq	%rcx, %r9
	addq	%rdi, %r9
	movq	%r8, %rcx
	rolq	$36, %rcx
	movq	%r8, %rsi
	rolq	$30, %rsi
	xorq	%rcx, %rsi
	movq	%r8, %rcx
	rolq	$25, %rcx
	xorq	%rsi, %rcx
	movq	%r14, %rsi
	xorq	%r13, %rsi
	andq	%r8, %rsi
	xorq	%rax, %rsi
	addq	%rcx, %rsi
	addq	%r9, %rsi
	movq	16(%r15), %rbx
	bswapq	%rbx
	movq	%r10, 128(%rsp)                 # 8-byte Spill
	addq	%r10, %r9
	movq	%r9, %rcx
	rolq	$50, %rcx
	movq	%r8, %rax
	movq	%r9, %rdi
	rolq	$46, %rdi
	andq	%r13, %rax
	movq	%r9, %r10
	rolq	$23, %r10
	xorq	%rcx, %rdi
	xorq	%rdi, %r10
	movq	%r9, %r11
	andq	%rdx, %r11
	movq	%r9, %rcx
	notq	%rcx
	andq	%rbp, %rcx
	movq	%r12, 104(%rsp)                 # 8-byte Spill
	movq	%rbx, 40(%rsp)                  # 8-byte Spill
	addq	%r12, %r11
	addq	%rbx, %r11
	addq	%rcx, %r11
	movabsq	$-5349999486874862801, %rcx     # imm = 0xB5C0FBCFEC4D3B2F
	addq	%rcx, %r11
	addq	%r10, %r11
	movq	%rsi, %rcx
	rolq	$36, %rcx
	movq	%rsi, %rdi
	rolq	$30, %rdi
	xorq	%rcx, %rdi
	movq	%rsi, %rcx
	rolq	$25, %rcx
	xorq	%rdi, %rcx
	movq	%r8, %rdi
	xorq	%r13, %rdi
	andq	%rsi, %rdi
	xorq	%rax, %rdi
	addq	%rcx, %rdi
	addq	%r11, %rdi
	movq	%r15, %r12
	movq	24(%r15), %r15
	bswapq	%r15
	movq	%r14, 136(%rsp)                 # 8-byte Spill
	addq	%r14, %r11
	movq	%r11, %rax
	rolq	$50, %rax
	movq	%rsi, %rcx
	movq	%r11, %r10
	rolq	$46, %r10
	andq	%r8, %rcx
	movq	%r11, %rbx
	rolq	$23, %rbx
	xorq	%rax, %r10
	xorq	%r10, %rbx
	movq	%r11, %r10
	andq	%r9, %r10
	movq	%r11, %rax
	notq	%rax
	andq	%rdx, %rax
	movq	%rbp, 112(%rsp)                 # 8-byte Spill
	movq	%r15, 32(%rsp)                  # 8-byte Spill
	addq	%rbp, %r10
	addq	%r15, %r10
	addq	%rax, %r10
	movabsq	$-1606136188198331460, %rax     # imm = 0xE9B5DBA58189DBBC
	addq	%rax, %r10
	addq	%rbx, %r10
	movq	%rdi, %rax
	rolq	$36, %rax
	movq	%rdi, %rbx
	rolq	$30, %rbx
	xorq	%rax, %rbx
	movq	%rdi, %r14
	rolq	$25, %r14
	xorq	%rbx, %r14
	movq	%rsi, %rax
	xorq	%r8, %rax
	andq	%rdi, %rax
	xorq	%rcx, %rax
	addq	%r14, %rax
	addq	%r10, %rax
	movq	32(%r12), %r14
	bswapq	%r14
	movq	%r13, 144(%rsp)                 # 8-byte Spill
	addq	%r13, %r10
	movq	%r10, %rcx
	rolq	$50, %rcx
	movq	%r10, %rbx
	rolq	$46, %rbx
	xorq	%rcx, %rbx
	movq	%r10, %rcx
	rolq	$23, %rcx
	xorq	%rbx, %rcx
	movq	%r10, %rbx
	andq	%r11, %rbx
	movq	%r14, -32(%rsp)                 # 8-byte Spill
	addq	%r14, %rdx
	addq	%rbx, %rdx
	movq	%r10, %rbx
	notq	%rbx
	andq	%r9, %rbx
	addq	%rbx, %rdx
	movq	%rax, %rbx
	rolq	$36, %rbx
	movabsq	$4131703408338449720, %r14      # imm = 0x3956C25BF348B538
	addq	%r14, %rdx
	movq	%rax, %r14
	rolq	$30, %r14
	addq	%rcx, %rdx
	movq	%rax, %r15
	rolq	$25, %r15
	xorq	%rbx, %r14
	xorq	%r14, %r15
	movq	%rdi, %rbx
	andq	%rsi, %rbx
	movq	%rdi, %rcx
	xorq	%rsi, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rcx
	addq	%r15, %rcx
	movq	40(%r12), %r14
	bswapq	%r14
	addq	%rdx, %r8
	movq	%r8, %rbx
	rolq	$50, %rbx
	addq	%rdx, %rcx
	movq	%r8, %rdx
	rolq	$46, %rdx
	xorq	%rbx, %rdx
	movq	%r8, %rbx
	rolq	$23, %rbx
	xorq	%rdx, %rbx
	movq	%r8, %rdx
	andq	%r10, %rdx
	movq	%r14, -72(%rsp)                 # 8-byte Spill
	addq	%r14, %r9
	addq	%rdx, %r9
	movq	%r8, %rdx
	notq	%rdx
	andq	%r11, %rdx
	addq	%rdx, %r9
	movq	%rcx, %rdx
	rolq	$36, %rdx
	movabsq	$6480981068601479193, %r14      # imm = 0x59F111F1B605D019
	addq	%r14, %r9
	movq	%rcx, %r14
	rolq	$30, %r14
	addq	%rbx, %r9
	movq	%rcx, %rbx
	rolq	$25, %rbx
	xorq	%rdx, %r14
	xorq	%r14, %rbx
	movq	%rax, %r14
	andq	%rdi, %r14
	movq	%rax, %rdx
	xorq	%rdi, %rdx
	andq	%rcx, %rdx
	xorq	%r14, %rdx
	addq	%rbx, %rdx
	movq	48(%r12), %r14
	bswapq	%r14
	addq	%r9, %rsi
	movq	%rsi, %rbx
	rolq	$50, %rbx
	addq	%r9, %rdx
	movq	%rsi, %r9
	rolq	$46, %r9
	xorq	%rbx, %r9
	movq	%rsi, %rbx
	rolq	$23, %rbx
	xorq	%r9, %rbx
	movq	%rsi, %r9
	andq	%r8, %r9
	movq	%r14, -40(%rsp)                 # 8-byte Spill
	addq	%r14, %r11
	addq	%r9, %r11
	movq	%rsi, %r9
	notq	%r9
	andq	%r10, %r9
	addq	%r9, %r11
	movq	%rdx, %r9
	rolq	$36, %r9
	movabsq	$-7908458776815382629, %r14     # imm = 0x923F82A4AF194F9B
	addq	%r14, %r11
	movq	%rdx, %r14
	rolq	$30, %r14
	addq	%rbx, %r11
	movq	%rdx, %rbx
	rolq	$25, %rbx
	xorq	%r9, %r14
	xorq	%r14, %rbx
	movq	%rcx, %r9
	andq	%rax, %r9
	movq	%rcx, %r14
	xorq	%rax, %r14
	andq	%rdx, %r14
	xorq	%r9, %r14
	addq	%rbx, %r14
	movq	56(%r12), %rbx
	bswapq	%rbx
	addq	%r11, %rdi
	movq	%rdi, %r9
	rolq	$50, %r9
	addq	%r11, %r14
	movq	%rdi, %r11
	rolq	$46, %r11
	xorq	%r9, %r11
	movq	%rdi, %r9
	rolq	$23, %r9
	xorq	%r11, %r9
	movq	%rdi, %r11
	andq	%rsi, %r11
	movq	%rbx, 24(%rsp)                  # 8-byte Spill
	addq	%rbx, %r10
	addq	%r11, %r10
	movq	%rdi, %r11
	notq	%r11
	andq	%r8, %r11
	addq	%r11, %r10
	movq	%r14, %r11
	rolq	$36, %r11
	movabsq	$-6116909921290321640, %rbx     # imm = 0xAB1C5ED5DA6D8118
	addq	%rbx, %r10
	movq	%r14, %rbx
	rolq	$30, %rbx
	addq	%r9, %r10
	movq	%r14, %r9
	rolq	$25, %r9
	xorq	%r11, %rbx
	xorq	%rbx, %r9
	movq	%rdx, %r11
	andq	%rcx, %r11
	movq	%rdx, %r13
	xorq	%rcx, %r13
	andq	%r14, %r13
	xorq	%r11, %r13
	addq	%r9, %r13
	movq	64(%r12), %r11
	bswapq	%r11
	addq	%r10, %rax
	movq	%rax, %r9
	rolq	$50, %r9
	addq	%r10, %r13
	movq	%rax, %r10
	rolq	$46, %r10
	xorq	%r9, %r10
	movq	%rax, %r9
	rolq	$23, %r9
	xorq	%r10, %r9
	movq	%rax, %r10
	andq	%rdi, %r10
	movq	%r11, -24(%rsp)                 # 8-byte Spill
	addq	%r11, %r8
	addq	%r10, %r8
	movq	%rax, %r10
	notq	%r10
	andq	%rsi, %r10
	addq	%r10, %r8
	movq	%r13, %r10
	rolq	$36, %r10
	movabsq	$-2880145864133508542, %r11     # imm = 0xD807AA98A3030242
	addq	%r11, %r8
	movq	%r13, %r11
	rolq	$30, %r11
	addq	%r9, %r8
	movq	%r13, %r15
	rolq	$25, %r15
	xorq	%r10, %r11
	xorq	%r11, %r15
	movq	%r14, %r10
	andq	%rdx, %r10
	movq	%r14, %rbx
	xorq	%rdx, %rbx
	andq	%r13, %rbx
	xorq	%r10, %rbx
	addq	%r15, %rbx
	movq	72(%r12), %r9
	bswapq	%r9
	movq	%r9, 16(%rsp)                   # 8-byte Spill
	addq	%r8, %rcx
	movq	%rcx, %r10
	rolq	$50, %r10
	addq	%r8, %rbx
	movq	%rcx, %r8
	rolq	$46, %r8
	xorq	%r10, %r8
	movq	%rcx, %r10
	rolq	$23, %r10
	xorq	%r8, %r10
	movq	%rcx, %r8
	andq	%rax, %r8
	addq	%r9, %rsi
	addq	%r8, %rsi
	movq	%rcx, %r8
	notq	%r8
	andq	%rdi, %r8
	addq	%r8, %rsi
	movq	%rbx, %r8
	rolq	$36, %r8
	movabsq	$1334009975649890238, %r9       # imm = 0x12835B0145706FBE
	addq	%r9, %rsi
	movq	%rbx, %r11
	rolq	$30, %r11
	addq	%r10, %rsi
	movq	%rbx, %r10
	rolq	$25, %r10
	xorq	%r8, %r11
	xorq	%r11, %r10
	movq	%r13, %r8
	andq	%r14, %r8
	movq	%r13, %r15
	xorq	%r14, %r15
	andq	%rbx, %r15
	xorq	%r8, %r15
	addq	%r10, %r15
	movq	%r12, %r11
	movq	80(%r12), %r9
	bswapq	%r9
	movq	%r9, 8(%rsp)                    # 8-byte Spill
	addq	%rsi, %rdx
	movq	%rdx, %r8
	rolq	$50, %r8
	addq	%rsi, %r15
	movq	%rdx, %rsi
	rolq	$46, %rsi
	xorq	%r8, %rsi
	movq	%rdx, %r8
	rolq	$23, %r8
	xorq	%rsi, %r8
	movq	%rdx, %rsi
	andq	%rcx, %rsi
	addq	%r9, %rdi
	addq	%rsi, %rdi
	movq	%rdx, %rsi
	notq	%rsi
	andq	%rax, %rsi
	addq	%rsi, %rdi
	movq	%r15, %rsi
	rolq	$36, %rsi
	movabsq	$2608012711638119052, %r9       # imm = 0x243185BE4EE4B28C
	addq	%r9, %rdi
	movq	%r15, %r10
	rolq	$30, %r10
	addq	%r8, %rdi
	movq	%r15, %r8
	rolq	$25, %r8
	xorq	%rsi, %r10
	xorq	%r10, %r8
	movq	%rbx, %r10
	andq	%r13, %r10
	movq	%rbx, %r12
	xorq	%r13, %r12
	andq	%r15, %r12
	xorq	%r10, %r12
	addq	%r8, %r12
	movq	88(%r11), %rsi
	bswapq	%rsi
	movq	%rsi, -128(%rsp)                # 8-byte Spill
	addq	%rdi, %r14
	movq	%r14, %r8
	rolq	$50, %r8
	addq	%rdi, %r12
	movq	%r14, %rdi
	rolq	$46, %rdi
	xorq	%r8, %rdi
	movq	%r14, %r8
	rolq	$23, %r8
	xorq	%rdi, %r8
	movq	%r14, %rdi
	andq	%rdx, %rdi
	addq	%rsi, %rax
	addq	%rdi, %rax
	movq	%r14, %rdi
	notq	%rdi
	andq	%rcx, %rdi
	addq	%rdi, %rax
	movq	%r12, %rdi
	rolq	$36, %rdi
	movabsq	$6128411473006802146, %rsi      # imm = 0x550C7DC3D5FFB4E2
	addq	%rsi, %rax
	movq	%r12, %r10
	rolq	$30, %r10
	addq	%r8, %rax
	movq	%r12, %r8
	rolq	$25, %r8
	xorq	%rdi, %r10
	xorq	%r10, %r8
	movq	%r15, %rdi
	andq	%rbx, %rdi
	movq	%r15, %rbp
	xorq	%rbx, %rbp
	andq	%r12, %rbp
	xorq	%rdi, %rbp
	addq	%r8, %rbp
	movq	96(%r11), %rsi
	bswapq	%rsi
	addq	%rax, %r13
	movq	%r13, %rdi
	rolq	$50, %rdi
	addq	%rax, %rbp
	movq	%r13, %rax
	rolq	$46, %rax
	xorq	%rdi, %rax
	movq	%r13, %rdi
	rolq	$23, %rdi
	xorq	%rax, %rdi
	movq	%r13, %rax
	andq	%r14, %rax
	movq	%rsi, -120(%rsp)                # 8-byte Spill
	addq	%rsi, %rcx
	addq	%rax, %rcx
	movq	%r13, %rax
	notq	%rax
	andq	%rdx, %rax
	addq	%rax, %rcx
	movq	%rbp, %rax
	rolq	$36, %rax
	movabsq	$8268148722764581231, %rsi      # imm = 0x72BE5D74F27B896F
	addq	%rsi, %rcx
	movq	%rbp, %r8
	rolq	$30, %r8
	addq	%rdi, %rcx
	movq	%rbp, %r10
	rolq	$25, %r10
	xorq	%rax, %r8
	xorq	%r8, %r10
	movq	%r12, %rax
	andq	%r15, %rax
	movq	%r12, %rdi
	xorq	%r15, %rdi
	andq	%rbp, %rdi
	xorq	%rax, %rdi
	addq	%r10, %rdi
	movq	104(%r11), %r10
	bswapq	%r10
	addq	%rcx, %rbx
	movq	%rbx, %rax
	rolq	$50, %rax
	addq	%rcx, %rdi
	movq	%rbx, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%rbx, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%rbx, %rcx
	andq	%r13, %rcx
	addq	%r10, %rdx
	addq	%rcx, %rdx
	movq	%rbx, %rcx
	notq	%rcx
	andq	%r14, %rcx
	addq	%rcx, %rdx
	movq	%rdi, %rcx
	rolq	$36, %rcx
	movabsq	$-9160688886553864527, %rsi     # imm = 0x80DEB1FE3B1696B1
	addq	%rsi, %rdx
	movq	%rdi, %r8
	rolq	$30, %r8
	addq	%rax, %rdx
	movq	%rdi, %rax
	rolq	$25, %rax
	xorq	%rcx, %r8
	xorq	%r8, %rax
	movq	%rbp, %rcx
	andq	%r12, %rcx
	movq	%rbp, %r8
	xorq	%r12, %r8
	andq	%rdi, %r8
	xorq	%rcx, %r8
	addq	%rax, %r8
	movq	112(%r11), %r9
	bswapq	%r9
	addq	%rdx, %r15
	movq	%r15, %rax
	rolq	$50, %rax
	addq	%rdx, %r8
	movq	%r15, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%r15, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r15, %rcx
	andq	%rbx, %rcx
	addq	%r9, %r14
	addq	%rcx, %r14
	movq	%r15, %rcx
	notq	%rcx
	andq	%r13, %rcx
	addq	%rcx, %r14
	movq	%r8, %rcx
	rolq	$36, %rcx
	movabsq	$-7215885187991268811, %rdx     # imm = 0x9BDC06A725C71235
	addq	%rdx, %r14
	movq	%r8, %rdx
	rolq	$30, %rdx
	addq	%rax, %r14
	movq	%r8, %rax
	rolq	$25, %rax
	xorq	%rcx, %rdx
	xorq	%rdx, %rax
	movq	%rdi, %rcx
	andq	%rbp, %rcx
	movq	%rdi, %rsi
	xorq	%rbp, %rsi
	andq	%r8, %rsi
	xorq	%rcx, %rsi
	addq	%rax, %rsi
	movq	%r11, %rax
	movq	%r11, 152(%rsp)                 # 8-byte Spill
	movq	120(%r11), %rdx
	bswapq	%rdx
	addq	%r14, %r12
	movq	%r12, %rax
	rolq	$50, %rax
	addq	%r14, %rsi
	movq	%r12, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%r12, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r12, %rcx
	andq	%r15, %rcx
	movq	%rdx, -112(%rsp)                # 8-byte Spill
	addq	%rdx, %r13
	addq	%rcx, %r13
	movq	%r12, %rcx
	notq	%rcx
	andq	%rbx, %rcx
	addq	%rcx, %r13
	movq	%rsi, %rcx
	rolq	$36, %rcx
	movabsq	$-4495734319001033068, %rdx     # imm = 0xC19BF174CF692694
	addq	%rdx, %r13
	movq	%rsi, %rdx
	rolq	$30, %rdx
	addq	%rax, %r13
	movq	%rsi, %rax
	rolq	$25, %rax
	xorq	%rcx, %rdx
	xorq	%rdx, %rax
	movq	%r8, %rcx
	andq	%rdi, %rcx
	movq	%r8, %r11
	xorq	%rdi, %r11
	movq	%rsi, -80(%rsp)                 # 8-byte Spill
	andq	%rsi, %r11
	xorq	%rcx, %r11
	addq	%rax, %r11
	addq	%r13, %rbp
	addq	%r13, %r11
	xorl	%esi, %esi
	.p2align	4, 0x90
.LBB4_3:                                #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r10, -104(%rsp)                # 8-byte Spill
	movq	48(%rsp), %r14                  # 8-byte Reload
	movq	%r14, %rax
	rorq	%rax
	movq	%r14, %rcx
	rolq	$56, %rcx
	xorq	%rax, %rcx
	movq	%rbp, %rdx
	rolq	$50, %rdx
	movq	%r14, %rax
	shrq	$7, %rax
	movq	%rbp, %r10
	rolq	$46, %r10
	xorq	%rcx, %rax
	movq	%r11, %r13
	movq	%rbp, %r11
	rolq	$23, %r11
	xorq	%rdx, %r10
	xorq	%r10, %r11
	movq	%r12, %rdx
	andq	%rbp, %rdx
	addq	%rbx, %rdx
	movq	%rbp, %rcx
	notq	%rcx
	andq	%r15, %rcx
	addq	%rdx, %rcx
	movq	%r9, %rdx
	rolq	$45, %rdx
	movq	%r9, %r10
	movq	%r9, -96(%rsp)                  # 8-byte Spill
	rolq	$3, %r9
	xorq	%rdx, %r9
	addq	%r11, %rcx
	movq	%r10, %rdx
	shrq	$6, %rdx
	xorq	%r9, %rdx
	movq	%r13, %r9
	rolq	$36, %r9
	movq	%r13, %r10
	rolq	$30, %r10
	xorq	%r9, %r10
	movq	%r13, %r9
	movq	%r13, %r11
	movq	%r13, -88(%rsp)                 # 8-byte Spill
	rolq	$25, %r9
	xorq	%r10, %r9
	movq	%r8, %r10
	movq	-80(%rsp), %r13                 # 8-byte Reload
	xorq	%r13, %r10
	movq	%r8, %rbx
	andq	%r13, %rbx
	andq	%r11, %r10
	xorq	%r10, %rbx
	addq	%r9, %rbx
	movq	-16(%rsp), %r10                 # 8-byte Reload
	addq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rdx, %r10
	movq	40(%rsp), %r11                  # 8-byte Reload
	movq	%r11, %rdx
	rorq	%rdx
	movq	%r11, %r9
	rolq	$56, %r9
	xorq	%rdx, %r9
	addq	%rax, %r10
	movq	%r11, %rax
	shrq	$7, %rax
	xorq	%r9, %rax
	addq	%r10, %rcx
	addq	K512+128(,%rsi,8), %rcx
	movq	%rsi, %r9
	addq	%rcx, %rdi
	addq	%rcx, %rbx
	movq	-112(%rsp), %rsi                # 8-byte Reload
	movq	%rsi, %rcx
	rolq	$45, %rcx
	movq	%rsi, %rdx
	rolq	$3, %rdx
	xorq	%rcx, %rdx
	movq	%rsi, %rcx
	shrq	$6, %rcx
	xorq	%rdx, %rcx
	addq	8(%rsp), %r14                   # 8-byte Folded Reload
	addq	%rcx, %r14
	movq	%rdi, %rcx
	rolq	$50, %rcx
	movq	%rdi, %rdx
	rolq	$46, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %r14
	movq	%rdi, %rax
	rolq	$23, %rax
	xorq	%rdx, %rax
	movq	%rbx, %rcx
	rolq	$36, %rcx
	movq	%rbx, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%rbx, %rcx
	movq	%rbx, -48(%rsp)                 # 8-byte Spill
	rolq	$25, %rcx
	xorq	%rdx, %rcx
	movq	%r13, -80(%rsp)                 # 8-byte Spill
	movq	%r13, %rdx
	movq	-88(%rsp), %rsi                 # 8-byte Reload
	andq	%rsi, %rdx
	xorq	%rsi, %r13
	andq	%rbx, %r13
	xorq	%rdx, %r13
	addq	%rcx, %r13
	movq	%rdi, %rcx
	andq	%rbp, %rcx
	addq	%r14, %r15
	movq	%r9, %rsi
	movq	%r9, -64(%rsp)                  # 8-byte Spill
	addq	K512+136(,%r9,8), %r15
	addq	%rcx, %r15
	movq	%rdi, %rcx
	notq	%rcx
	andq	%r12, %rcx
	addq	%rcx, %r15
	movq	32(%rsp), %rbx                  # 8-byte Reload
	movq	%rbx, %rcx
	rorq	%rcx
	movq	%rbx, %rdx
	rolq	$56, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %r15
	movq	%r10, -16(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	rolq	$45, %rax
	movq	%rbx, %rcx
	shrq	$7, %rcx
	movq	%r10, %r9
	rolq	$3, %r9
	xorq	%rdx, %rcx
	xorq	%rax, %r9
	addq	%r15, %r8
	addq	%r15, %r13
	shrq	$6, %r10
	xorq	%r9, %r10
	addq	-128(%rsp), %r11                # 8-byte Folded Reload
	addq	%rcx, %r11
	movq	%r8, %rcx
	rolq	$50, %rcx
	addq	%r10, %r11
	movq	%r8, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	addq	%r11, %r12
	movq	%r8, %rcx
	andq	%rdi, %rcx
	addq	K512+144(,%rsi,8), %r12
	addq	%rcx, %r12
	movq	%r8, %rcx
	notq	%rcx
	andq	%rbp, %rcx
	addq	%rcx, %r12
	movq	-48(%rsp), %r10                 # 8-byte Reload
	movq	%r10, %rcx
	movq	-88(%rsp), %rdx                 # 8-byte Reload
	andq	%rdx, %rcx
	xorq	%rdx, %r10
	andq	%r13, %r10
	xorq	%rcx, %r10
	movq	%r13, %rcx
	rolq	$36, %rcx
	movq	%r13, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%r8, %rcx
	rolq	$23, %rcx
	xorq	%rax, %rcx
	movq	%r13, %rax
	movq	%r13, %r15
	rolq	$25, %rax
	xorq	%rdx, %rax
	movq	-32(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, %rdx
	rorq	%rdx
	movq	%rsi, %r9
	rolq	$56, %r9
	xorq	%rdx, %r9
	addq	%rcx, %r12
	movq	%rsi, %rcx
	shrq	$7, %rcx
	xorq	%r9, %rcx
	addq	%rax, %r10
	movq	%r14, 48(%rsp)                  # 8-byte Spill
	movq	%r14, %rax
	rolq	$45, %rax
	movq	%r14, %rdx
	rolq	$3, %rdx
	xorq	%rax, %rdx
	shrq	$6, %r14
	xorq	%rdx, %r14
	movq	-80(%rsp), %r13                 # 8-byte Reload
	addq	%r12, %r13
	addq	%r12, %r10
	addq	-120(%rsp), %rbx                # 8-byte Folded Reload
	addq	%rcx, %rbx
	addq	%r14, %rbx
	addq	%rbx, %rbp
	movq	-64(%rsp), %r12                 # 8-byte Reload
	addq	K512+152(,%r12,8), %rbp
	movq	%r13, %rax
	andq	%r8, %rax
	addq	%rax, %rbp
	movq	%r13, %rcx
	rolq	$50, %rcx
	movq	%r13, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	movq	%r13, %rcx
	notq	%rcx
	andq	%rdi, %rcx
	addq	%rcx, %rbp
	movq	%r15, 80(%rsp)                  # 8-byte Spill
	movq	%r15, %rcx
	movq	-48(%rsp), %rdx                 # 8-byte Reload
	andq	%rdx, %rcx
	movq	%r15, %r14
	xorq	%rdx, %r14
	movq	-72(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, %rdx
	rorq	%rdx
	movq	%r10, %r9
	andq	%r10, %r14
	xorq	%rcx, %r14
	movq	%rsi, %rcx
	movq	%rsi, %r10
	movq	%rsi, -72(%rsp)                 # 8-byte Spill
	rolq	$56, %rcx
	xorq	%rdx, %rcx
	movq	%r9, %rdx
	rolq	$36, %rdx
	movq	%r9, %rsi
	rolq	$30, %rsi
	xorq	%rdx, %rsi
	movq	%r13, %r15
	movq	%r13, %rdx
	rolq	$23, %rdx
	xorq	%rax, %rdx
	movq	%r9, %rax
	rolq	$25, %rax
	xorq	%rsi, %rax
	movq	%r10, %rsi
	shrq	$7, %rsi
	xorq	%rcx, %rsi
	addq	%rdx, %rbp
	movq	%r11, 40(%rsp)                  # 8-byte Spill
	movq	%r11, %rcx
	rolq	$45, %rcx
	movq	%r11, %rdx
	rolq	$3, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %r14
	movq	%r11, %rax
	shrq	$6, %rax
	xorq	%rdx, %rax
	movq	-32(%rsp), %rcx                 # 8-byte Reload
	addq	-104(%rsp), %rcx                # 8-byte Folded Reload
	addq	%rsi, %rcx
	addq	%rax, %rcx
	movq	%rcx, %rdx
	movq	%rcx, -32(%rsp)                 # 8-byte Spill
	movq	-88(%rsp), %r13                 # 8-byte Reload
	addq	%rbp, %r13
	addq	%rbp, %r14
	movq	%r13, %rcx
	rolq	$50, %rcx
	movq	%r13, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	addq	%rdx, %rdi
	movq	%r12, %r10
	addq	K512+160(,%r12,8), %rdi
	movq	%r13, %rcx
	andq	%r15, %rcx
	movq	%r15, %rbp
	movq	%r15, -80(%rsp)                 # 8-byte Spill
	addq	%rcx, %rdi
	movq	%r13, %rcx
	notq	%rcx
	andq	%r8, %rcx
	addq	%rcx, %rdi
	movq	%r9, %r11
	movq	%r9, (%rsp)                     # 8-byte Spill
	movq	%r9, %rcx
	movq	80(%rsp), %r12                  # 8-byte Reload
	andq	%r12, %rcx
	movq	-40(%rsp), %r9                  # 8-byte Reload
	movq	%r9, %rdx
	rorq	%rdx
	xorq	%r12, %r11
	movq	%r9, %rsi
	rolq	$56, %rsi
	andq	%r14, %r11
	xorq	%rcx, %r11
	xorq	%rdx, %rsi
	movq	%r9, %rcx
	shrq	$7, %rcx
	xorq	%rsi, %rcx
	movq	%r14, %rdx
	rolq	$36, %rdx
	movq	%r14, %rsi
	rolq	$30, %rsi
	xorq	%rdx, %rsi
	movq	%r13, -88(%rsp)                 # 8-byte Spill
	movq	%r13, %rdx
	rolq	$23, %rdx
	xorq	%rax, %rdx
	movq	%r14, %rax
	rolq	$25, %rax
	xorq	%rsi, %rax
	movq	%rbx, 32(%rsp)                  # 8-byte Spill
	movq	%rbx, %rsi
	rolq	$45, %rsi
	movq	%rbx, %r9
	rolq	$3, %r9
	xorq	%rsi, %r9
	addq	%rdx, %rdi
	movq	%rbx, %rdx
	shrq	$6, %rdx
	xorq	%r9, %rdx
	addq	%rax, %r11
	movq	-72(%rsp), %r12                 # 8-byte Reload
	addq	-96(%rsp), %r12                 # 8-byte Folded Reload
	addq	%rcx, %r12
	movq	-48(%rsp), %rbx                 # 8-byte Reload
	addq	%rdi, %rbx
	addq	%rdi, %r11
	addq	%rdx, %r12
	movq	%r12, -72(%rsp)                 # 8-byte Spill
	movq	%rbx, %rax
	rolq	$50, %rax
	movq	%rbx, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%rcx, -56(%rsp)                 # 8-byte Spill
	movq	K512+168(,%r10,8), %r15
	addq	%r12, %r15
	addq	%r8, %r15
	movq	%rbx, -48(%rsp)                 # 8-byte Spill
	movq	%rbx, %rax
	andq	%r13, %rax
	addq	%rax, %r15
	movq	%rbx, %rax
	notq	%rax
	movq	24(%rsp), %rcx                  # 8-byte Reload
	rolq	$56, %rcx
	movq	%rcx, 64(%rsp)                  # 8-byte Spill
	andq	%rbp, %rax
	addq	%rax, %r15
	movq	-24(%rsp), %rax                 # 8-byte Reload
	rolq	$56, %rax
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	16(%rsp), %r12                  # 8-byte Reload
	movq	%r12, %r10
	rolq	$56, %r10
	movq	8(%rsp), %rcx                   # 8-byte Reload
	movq	%rcx, %rax
	rolq	$56, %rax
	movq	%r14, %r8
	movq	%r14, 192(%rsp)                 # 8-byte Spill
	movq	%r14, %r9
	movq	(%rsp), %rsi                    # 8-byte Reload
	andq	%rsi, %r9
	movq	-128(%rsp), %rdx                # 8-byte Reload
	movq	%rdx, %r14
	rolq	$56, %r14
	movq	-120(%rsp), %rdi                # 8-byte Reload
	movq	%rdi, %rbp
	rolq	$56, %rbp
	xorq	%rsi, %r8
	movq	-104(%rsp), %r13                # 8-byte Reload
	movq	%r13, %rsi
	rolq	$56, %rsi
	andq	%r11, %r8
	xorq	%r9, %r8
	movq	24(%rsp), %rbx                  # 8-byte Reload
	movq	%rbx, %r9
	rorq	%r9
	xorq	%r9, 64(%rsp)                   # 8-byte Folded Spill
	movq	-24(%rsp), %r9                  # 8-byte Reload
	rorq	%r9
	xorq	%r9, 72(%rsp)                   # 8-byte Folded Spill
	rorq	%r12
	xorq	%r12, %r10
	movq	%r10, 184(%rsp)                 # 8-byte Spill
	rorq	%rcx
	xorq	%rcx, %rax
	movq	%rax, 176(%rsp)                 # 8-byte Spill
	movq	%rdx, %r9
	rorq	%r9
	xorq	%r9, %r14
	movq	%rdi, %r9
	rorq	%r9
	xorq	%r9, %rbp
	movq	%r13, %r9
	rorq	%r9
	xorq	%r9, %rsi
	movq	%r11, %rdx
	movq	%r11, -8(%rsp)                  # 8-byte Spill
	movq	%r11, %r9
	rolq	$36, %r9
	rolq	$30, %r11
	xorq	%r9, %r11
	movq	-48(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %r10
	rolq	$23, %r10
	xorq	-56(%rsp), %r10                 # 8-byte Folded Reload
	rolq	$25, %rdx
	xorq	%r11, %rdx
	movq	-96(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, %rcx
	rorq	%rcx
	movq	%rdi, %r9
	rolq	$56, %r9
	xorq	%rcx, %r9
	addq	%r10, %r15
	movq	-112(%rsp), %rax                # 8-byte Reload
	movq	%rax, %r11
	rorq	%r11
	movq	%rax, %r10
	rolq	$56, %r10
	xorq	%r11, %r10
	addq	%rdx, %r8
	movq	-16(%rsp), %rdx                 # 8-byte Reload
	movq	%rdx, %rcx
	rorq	%rcx
	movq	%rdx, %r11
	rolq	$56, %r11
	xorq	%rcx, %r11
	movq	%rdx, %rcx
	shrq	$7, %rcx
	xorq	%r11, %rcx
	movq	-40(%rsp), %r11                 # 8-byte Reload
	addq	%rax, %r11
	addq	%rax, %rcx
	movq	%rcx, 168(%rsp)                 # 8-byte Spill
	shrq	$7, %rax
	xorq	%r10, %rax
	addq	%rdi, %rax
	movq	%rax, -56(%rsp)                 # 8-byte Spill
	shrq	$7, %rdi
	xorq	%r9, %rdi
	movq	%r13, %rcx
	addq	%r13, %rdi
	movq	%rdi, -112(%rsp)                # 8-byte Spill
	shrq	$7, %rcx
	xorq	%rsi, %rcx
	movq	-120(%rsp), %rax                # 8-byte Reload
	addq	%rax, %rcx
	movq	%rcx, -96(%rsp)                 # 8-byte Spill
	movq	%rax, %rcx
	shrq	$7, %rcx
	xorq	%rbp, %rcx
	movq	-128(%rsp), %rax                # 8-byte Reload
	addq	%rax, %rcx
	movq	%rcx, -104(%rsp)                # 8-byte Spill
	shrq	$7, %rax
	xorq	%r14, %rax
	movq	8(%rsp), %rdx                   # 8-byte Reload
	addq	%rdx, %rax
	movq	%rax, -128(%rsp)                # 8-byte Spill
	shrq	$7, %rdx
	xorq	176(%rsp), %rdx                 # 8-byte Folded Reload
	movq	16(%rsp), %rcx                  # 8-byte Reload
	addq	%rcx, %rdx
	movq	%rdx, -120(%rsp)                # 8-byte Spill
	shrq	$7, %rcx
	xorq	184(%rsp), %rcx                 # 8-byte Folded Reload
	movq	-24(%rsp), %r10                 # 8-byte Reload
	addq	%r10, %rcx
	movq	%rcx, %rdx
	shrq	$7, %r10
	xorq	72(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rbx, %r10
	movq	-32(%rsp), %rcx                 # 8-byte Reload
	movq	%rcx, %rsi
	rolq	$45, %rsi
	shrq	$7, %rbx
	movq	%rcx, %rdi
	rolq	$3, %rdi
	xorq	64(%rsp), %rbx                  # 8-byte Folded Reload
	xorq	%rsi, %rdi
	movq	%rcx, %rsi
	shrq	$6, %rsi
	xorq	%rdi, %rsi
	movq	%r11, %rcx
	addq	%rbx, %rcx
	movq	80(%rsp), %r9                   # 8-byte Reload
	addq	%r15, %r9
	addq	%r15, %r8
	addq	%rsi, %rcx
	movq	%rcx, -40(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	rolq	$50, %rax
	movq	%r9, %rsi
	rolq	$46, %rsi
	xorq	%rax, %rsi
	movq	%r9, %rax
	rolq	$23, %rax
	xorq	%rsi, %rax
	movq	-64(%rsp), %r15                 # 8-byte Reload
	movq	K512+176(,%r15,8), %rsi
	addq	%rcx, %rsi
	addq	-80(%rsp), %rsi                 # 8-byte Folded Reload
	movq	%r9, %rdi
	movq	%r12, %rcx
	andq	%r12, %rdi
	addq	%rdi, %rsi
	movq	%r9, %rdi
	movq	%r9, %r13
	notq	%rdi
	movq	%r8, %r9
	rolq	$36, %r9
	movq	-88(%rsp), %r14                 # 8-byte Reload
	andq	%r14, %rdi
	addq	%rdi, %rsi
	movq	%r8, %rdi
	rolq	$30, %rdi
	xorq	%r9, %rdi
	movq	%r8, %r9
	rolq	$25, %r9
	xorq	%rdi, %r9
	movq	-8(%rsp), %rbp                  # 8-byte Reload
	movq	%rbp, %rdi
	movq	192(%rsp), %rbx                 # 8-byte Reload
	andq	%rbx, %rdi
	movq	%rbp, %r11
	xorq	%rbx, %r11
	andq	%r8, %r11
	xorq	%rdi, %r11
	addq	%rax, %rsi
	addq	%r9, %r11
	movq	-72(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rax
	rolq	$45, %rax
	movq	%r12, %rdi
	rolq	$3, %rdi
	movq	(%rsp), %r9                     # 8-byte Reload
	addq	%rsi, %r9
	addq	%rsi, %r11
	xorq	%rax, %rdi
	movq	%r12, %rax
	shrq	$6, %rax
	xorq	%rdi, %rax
	addq	-16(%rsp), %r10                 # 8-byte Folded Reload
	addq	%rax, %r10
	movq	K512+184(,%r15,8), %rsi
	addq	%r10, %rsi
	addq	%r14, %rsi
	movq	%r9, %rax
	andq	%r13, %rax
	addq	%rax, %rsi
	movq	%r9, %rax
	rolq	$50, %rax
	movq	%r9, %rdi
	rolq	$46, %rdi
	xorq	%rax, %rdi
	movq	%r9, %rax
	notq	%rax
	andq	%rcx, %rax
	addq	%rax, %rsi
	movq	%r9, %rax
	movq	%r9, %r14
	movq	%r9, (%rsp)                     # 8-byte Spill
	rolq	$23, %rax
	xorq	%rdi, %rax
	movq	%r11, %rdi
	rolq	$36, %rdi
	movq	%r11, %r9
	rolq	$30, %r9
	xorq	%rdi, %r9
	movq	%r11, %rdi
	rolq	$25, %rdi
	xorq	%r9, %rdi
	addq	%rax, %rsi
	movq	%r8, %r9
	andq	%rbp, %r9
	movq	%r8, %rax
	xorq	%rbp, %rax
	movq	-40(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %r15
	rolq	$45, %r15
	andq	%r11, %rax
	xorq	%r9, %rax
	movq	%r12, %r9
	rolq	$3, %r9
	addq	%rdi, %rax
	xorq	%r15, %r9
	addq	%rsi, %rbx
	addq	%rsi, %rax
	movq	%r12, %rsi
	shrq	$6, %rsi
	xorq	%r9, %rsi
	addq	48(%rsp), %rdx                  # 8-byte Folded Reload
	addq	%rsi, %rdx
	movq	%rbx, %rsi
	rolq	$50, %rsi
	movq	%rbx, %rdi
	rolq	$46, %rdi
	xorq	%rsi, %rdi
	movq	%rbx, %rsi
	rolq	$23, %rsi
	xorq	%rdi, %rsi
	movq	-64(%rsp), %r12                 # 8-byte Reload
	movq	K512+192(,%r12,8), %rdi
	addq	%rdx, %rdi
	addq	%rcx, %rdi
	movq	%rbx, %r9
	andq	%r14, %r9
	addq	%r9, %rdi
	movq	%rbx, %r9
	movq	%rbx, %r14
	notq	%r9
	movq	%rax, %r15
	rolq	$36, %r15
	andq	%r13, %r9
	addq	%r9, %rdi
	movq	%rax, %r9
	rolq	$30, %r9
	xorq	%r15, %r9
	movq	%rax, %r15
	rolq	$25, %r15
	xorq	%r9, %r15
	movq	%r11, %rbp
	andq	%r8, %rbp
	movq	%r11, %rbx
	xorq	%r8, %rbx
	andq	%rax, %rbx
	xorq	%rbp, %rbx
	addq	%rsi, %rdi
	addq	%r15, %rbx
	movq	%r10, %rsi
	rolq	$45, %rsi
	movq	%r10, %r15
	rolq	$3, %r15
	movq	-8(%rsp), %r9                   # 8-byte Reload
	addq	%rdi, %r9
	movq	%r9, -8(%rsp)                   # 8-byte Spill
	addq	%rdi, %rbx
	xorq	%rsi, %r15
	movq	%r10, %rsi
	shrq	$6, %rsi
	xorq	%r15, %rsi
	movq	-120(%rsp), %rcx                # 8-byte Reload
	addq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	%rsi, %rcx
	movq	%rcx, -120(%rsp)                # 8-byte Spill
	movq	K512+200(,%r12,8), %rsi
	addq	%rcx, %rsi
	addq	%r13, %rsi
	movq	%r9, %rdi
	andq	%r14, %rdi
	addq	%rdi, %rsi
	movq	%r9, %rdi
	rolq	$50, %rdi
	movq	%r9, %r15
	rolq	$46, %r15
	xorq	%rdi, %r15
	movq	%r9, %rdi
	notq	%rdi
	movq	(%rsp), %r13                    # 8-byte Reload
	andq	%r13, %rdi
	addq	%rdi, %rsi
	movq	%r9, %rdi
	rolq	$23, %rdi
	xorq	%r15, %rdi
	movq	%rbx, %r15
	rolq	$36, %r15
	movq	%rbx, %rbp
	rolq	$30, %rbp
	xorq	%r15, %rbp
	movq	%rbx, %r9
	rolq	$25, %r9
	xorq	%rbp, %r9
	addq	%rdi, %rsi
	movq	%rax, %rdi
	andq	%r11, %rdi
	movq	%rax, %r15
	xorq	%r11, %r15
	movq	%rdx, -88(%rsp)                 # 8-byte Spill
	movq	%rdx, %rbp
	rolq	$45, %rbp
	andq	%rbx, %r15
	xorq	%rdi, %r15
	movq	%rdx, %rdi
	rolq	$3, %rdi
	addq	%r9, %r15
	xorq	%rbp, %rdi
	addq	%rsi, %r8
	addq	%rsi, %r15
	movq	%rdx, %rsi
	shrq	$6, %rsi
	xorq	%rdi, %rsi
	movq	-128(%rsp), %rcx                # 8-byte Reload
	addq	32(%rsp), %rcx                  # 8-byte Folded Reload
	addq	%rsi, %rcx
	movq	%rcx, -128(%rsp)                # 8-byte Spill
	movq	%r8, %rsi
	rolq	$50, %rsi
	movq	%r8, %rdi
	rolq	$46, %rdi
	xorq	%rsi, %rdi
	movq	%r8, %r9
	rolq	$23, %r9
	xorq	%rdi, %r9
	movq	K512+208(,%r12,8), %rdi
	addq	%rcx, %rdi
	addq	%r13, %rdi
	movq	%r8, %rsi
	movq	-8(%rsp), %rdx                  # 8-byte Reload
	andq	%rdx, %rsi
	addq	%rsi, %rdi
	movq	%r8, %rsi
	notq	%rsi
	movq	%r15, %rbp
	rolq	$36, %rbp
	andq	%r14, %rsi
	movq	%r14, %r13
	addq	%rsi, %rdi
	movq	%r15, %rsi
	rolq	$30, %rsi
	xorq	%rbp, %rsi
	movq	%r15, %rbp
	rolq	$25, %rbp
	xorq	%rsi, %rbp
	movq	%rbx, %r14
	andq	%rax, %r14
	movq	%rbx, %r12
	xorq	%rax, %r12
	andq	%r15, %r12
	xorq	%r14, %r12
	addq	%r9, %rdi
	addq	%rbp, %r12
	movq	-120(%rsp), %rcx                # 8-byte Reload
	movq	%rcx, %r9
	rolq	$45, %r9
	movq	%rcx, %r14
	rolq	$3, %r14
	addq	%rdi, %r11
	addq	%rdi, %r12
	xorq	%r9, %r14
	movq	%rcx, %rdi
	shrq	$6, %rdi
	xorq	%r14, %rdi
	movq	-104(%rsp), %rcx                # 8-byte Reload
	addq	-32(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rdi, %rcx
	movq	%rcx, -104(%rsp)                # 8-byte Spill
	movq	-64(%rsp), %rsi                 # 8-byte Reload
	movq	K512+216(,%rsi,8), %rdi
	addq	%rcx, %rdi
	addq	%r13, %rdi
	movq	%r11, %r9
	andq	%r8, %r9
	addq	%r9, %rdi
	movq	%r11, %r9
	rolq	$50, %r9
	movq	%r11, %r14
	rolq	$46, %r14
	xorq	%r9, %r14
	movq	%r11, %r9
	notq	%r9
	andq	%rdx, %r9
	addq	%r9, %rdi
	movq	%r11, %r9
	rolq	$23, %r9
	xorq	%r14, %r9
	movq	%r12, %r14
	rolq	$36, %r14
	movq	%r12, %rbp
	rolq	$30, %rbp
	xorq	%r14, %rbp
	movq	%r12, %r14
	rolq	$25, %r14
	xorq	%rbp, %r14
	addq	%r9, %rdi
	movq	%r15, %r9
	andq	%rbx, %r9
	movq	%r15, %rbp
	xorq	%rbx, %rbp
	movq	-128(%rsp), %rcx                # 8-byte Reload
	movq	%rcx, %r13
	rolq	$45, %r13
	andq	%r12, %rbp
	xorq	%r9, %rbp
	movq	%rcx, %r9
	rolq	$3, %r9
	addq	%r14, %rbp
	xorq	%r13, %r9
	addq	%rdi, %rax
	addq	%rdi, %rbp
	movq	%rcx, %rdi
	shrq	$6, %rdi
	xorq	%r9, %rdi
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	addq	-72(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rdi, %rcx
	movq	%rcx, -96(%rsp)                 # 8-byte Spill
	movq	%rax, %rdi
	rolq	$50, %rdi
	movq	%rax, %r9
	rolq	$46, %r9
	xorq	%rdi, %r9
	movq	%rax, %r14
	rolq	$23, %r14
	xorq	%r9, %r14
	movq	K512+224(,%rsi,8), %r9
	addq	%rcx, %r9
	addq	%rdx, %r9
	movq	%rax, %rdi
	andq	%r11, %rdi
	addq	%rdi, %r9
	movq	%rax, %rdi
	notq	%rdi
	movq	%rbp, %r13
	rolq	$36, %r13
	andq	%r8, %rdi
	addq	%rdi, %r9
	movq	%rbp, %rdi
	rolq	$30, %rdi
	xorq	%r13, %rdi
	movq	%rbp, %r13
	rolq	$25, %r13
	xorq	%rdi, %r13
	movq	%r12, %rsi
	andq	%r15, %rsi
	movq	%r12, %rdi
	xorq	%r15, %rdi
	andq	%rbp, %rdi
	xorq	%rsi, %rdi
	addq	%r14, %r9
	addq	%r13, %rdi
	movq	-104(%rsp), %rcx                # 8-byte Reload
	movq	%rcx, %rsi
	rolq	$45, %rsi
	movq	%rcx, %r14
	rolq	$3, %r14
	addq	%r9, %rbx
	addq	%r9, %rdi
	xorq	%rsi, %r14
	movq	%rcx, %rsi
	shrq	$6, %rsi
	xorq	%r14, %rsi
	movq	-112(%rsp), %rcx                # 8-byte Reload
	addq	-40(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rsi, %rcx
	movq	%rcx, -112(%rsp)                # 8-byte Spill
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	movq	K512+232(,%rdx,8), %rsi
	addq	%rcx, %rsi
	addq	%r8, %rsi
	movq	%rbx, %r8
	andq	%rax, %r8
	addq	%r8, %rsi
	movq	%rbx, %r8
	rolq	$50, %r8
	movq	%rbx, %r9
	rolq	$46, %r9
	xorq	%r8, %r9
	movq	%rbx, %r8
	notq	%r8
	andq	%r11, %r8
	addq	%r8, %rsi
	movq	%rbx, %r8
	rolq	$23, %r8
	xorq	%r9, %r8
	movq	%rdi, %r9
	rolq	$36, %r9
	movq	%rdi, %r14
	rolq	$30, %r14
	xorq	%r9, %r14
	movq	%rdi, %r9
	rolq	$25, %r9
	xorq	%r14, %r9
	addq	%r8, %rsi
	movq	%rbp, %r14
	andq	%r12, %r14
	movq	%rbp, %r8
	xorq	%r12, %r8
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	movq	%rcx, %r13
	rolq	$45, %r13
	andq	%rdi, %r8
	xorq	%r14, %r8
	movq	%rcx, %r14
	rolq	$3, %r14
	addq	%r9, %r8
	xorq	%r13, %r14
	addq	%rsi, %r15
	addq	%rsi, %r8
	movq	%rcx, %rsi
	shrq	$6, %rsi
	xorq	%r14, %rsi
	movq	-56(%rsp), %rcx                 # 8-byte Reload
	addq	%r10, %rcx
	addq	%rsi, %rcx
	movq	%rcx, -56(%rsp)                 # 8-byte Spill
	movq	%r15, %rsi
	rolq	$50, %rsi
	movq	%r15, %r9
	rolq	$46, %r9
	xorq	%rsi, %r9
	movq	%r15, %rsi
	rolq	$23, %rsi
	xorq	%r9, %rsi
	movq	K512+240(,%rdx,8), %r9
	addq	%rcx, %r9
	addq	%r11, %r9
	movq	%r15, %r11
	andq	%rbx, %r11
	addq	%r11, %r9
	movq	%r15, %r11
	notq	%r11
	movq	%r8, %r14
	rolq	$36, %r14
	andq	%rax, %r11
	addq	%r11, %r9
	movq	%r8, %r11
	rolq	$30, %r11
	xorq	%r14, %r11
	movq	%r8, %r14
	rolq	$25, %r14
	xorq	%r11, %r14
	movq	%rdi, %r11
	andq	%rbp, %r11
	movq	%rdi, %r13
	xorq	%rbp, %r13
	andq	%r8, %r13
	xorq	%r11, %r13
	addq	%rsi, %r9
	addq	%r14, %r13
	movq	-112(%rsp), %rcx                # 8-byte Reload
	movq	%rcx, %rsi
	rolq	$45, %rsi
	movq	%rcx, %r11
	rolq	$3, %r11
	addq	%r9, %r12
	addq	%r9, %r13
	xorq	%rsi, %r11
	movq	%rcx, %rsi
	shrq	$6, %rsi
	xorq	%r11, %rsi
	movq	168(%rsp), %rcx                 # 8-byte Reload
	movq	-88(%rsp), %rdx                 # 8-byte Reload
	addq	%rdx, %rcx
	addq	%rsi, %rcx
	movq	-64(%rsp), %r14                 # 8-byte Reload
	movq	K512+248(,%r14,8), %rsi
	addq	%rcx, %rsi
	addq	%rax, %rsi
	movq	%r12, %rax
	andq	%r15, %rax
	addq	%rax, %rsi
	movq	%r12, %rax
	rolq	$50, %rax
	movq	%r12, %r9
	rolq	$46, %r9
	xorq	%rax, %r9
	movq	%r12, %rax
	notq	%rax
	andq	%rbx, %rax
	addq	%rax, %rsi
	movq	%r12, %rax
	rolq	$23, %rax
	xorq	%r9, %rax
	movq	%r13, %r9
	rolq	$36, %r9
	movq	%r13, %r11
	rolq	$30, %r11
	xorq	%r9, %r11
	movq	%r13, %r9
	rolq	$25, %r9
	xorq	%r11, %r9
	addq	%rax, %rsi
	movq	%r8, %rax
	andq	%rdi, %rax
	movq	%r8, %r11
	xorq	%rdi, %r11
	movq	%r13, -80(%rsp)                 # 8-byte Spill
	andq	%r13, %r11
	xorq	%rax, %r11
	addq	%r9, %r11
	addq	%rsi, %rbp
	addq	%rsi, %r11
	movq	%r10, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, -24(%rsp)                 # 8-byte Spill
	movq	-120(%rsp), %rax                # 8-byte Reload
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	-128(%rsp), %rax                # 8-byte Reload
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	-104(%rsp), %rax                # 8-byte Reload
	movq	%rax, -128(%rsp)                # 8-byte Spill
	movq	-96(%rsp), %rax                 # 8-byte Reload
	movq	%rax, -120(%rsp)                # 8-byte Spill
	movq	-112(%rsp), %r10                # 8-byte Reload
	movq	%r14, %rax
	addq	$16, %rax
	movq	-56(%rsp), %r9                  # 8-byte Reload
	movq	%rcx, -112(%rsp)                # 8-byte Spill
	movq	%rax, %rsi
	cmpq	$64, %rax
	jb	.LBB4_3
# %bb.4:                                #   in Loop: Header=BB4_2 Depth=1
	movq	144(%rsp), %r13                 # 8-byte Reload
	addq	%r11, %r13
	movq	136(%rsp), %r14                 # 8-byte Reload
	addq	-80(%rsp), %r14                 # 8-byte Folded Reload
	movq	128(%rsp), %r10                 # 8-byte Reload
	addq	%r8, %r10
	movq	120(%rsp), %rax                 # 8-byte Reload
	addq	%rdi, %rax
	movq	%rax, %rdi
	movq	112(%rsp), %rax                 # 8-byte Reload
	addq	%rbp, %rax
	movq	%rax, %rbp
	movq	104(%rsp), %rax                 # 8-byte Reload
	addq	%r12, %rax
	movq	%rax, %r12
	movq	96(%rsp), %r11                  # 8-byte Reload
	addq	%r15, %r11
	movq	88(%rsp), %r8                   # 8-byte Reload
	addq	%rbx, %r8
	movq	152(%rsp), %rsi                 # 8-byte Reload
	subq	$-128, %rsi
	movq	56(%rsp), %rax                  # 8-byte Reload
	movq	%r13, (%rax)
	movq	%r14, 8(%rax)
	movq	%r10, 16(%rax)
	movq	%rdi, 24(%rax)
	movq	%rbp, 32(%rax)
	movq	%r12, 40(%rax)
	movq	%r11, 48(%rax)
	movq	%r8, 56(%rax)
	movq	160(%rsp), %rdx                 # 8-byte Reload
	decq	%rdx
	jne	.LBB4_2
.LBB4_5:
	addq	$200, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end4:
	.size	sha512_block_data_order, .Lfunc_end4-sha512_block_data_order
                                        # -- End function
	.globl	SHA512_Final                    # -- Begin function SHA512_Final
	.p2align	4, 0x90
	.type	SHA512_Final,@function
SHA512_Final:                           # @SHA512_Final
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	pushq	%rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movl	208(%rsi), %eax
	movl	212(%rsi), %r12d
	leaq	80(%rsi), %r15
	leaq	1(%rax), %rdi
	movb	$-128, 80(%rsi,%rax)
	cmpq	$112, %rax
	jb	.LBB5_4
# %bb.1:
	cmpl	$127, %eax
	je	.LBB5_3
# %bb.2:
	movl	$127, %edx
	subq	%rax, %rdx
	addq	%r15, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB5_3:
	movl	$1, %edx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	xorl	%edi, %edi
	jmp	.LBB5_5
.LBB5_4:
	cmpq	$112, %rdi
	je	.LBB5_6
.LBB5_5:
	movl	$112, %edx
	subq	%rdi, %rdx
	addq	%r15, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB5_6:
	movq	64(%r14), %rax
	movq	72(%r14), %rcx
	bswapq	%rcx
	movq	%rcx, 192(%r14)
	bswapq	%rax
	movq	%rax, 200(%r14)
	movl	$1, %edx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	testq	%rbx, %rbx
	je	.LBB5_7
# %bb.8:
	movl	$1, %eax
	cmpl	$8, %r12d
	jb	.LBB5_16
# %bb.9:
	shrq	$3, %r12
	leaq	-1(%r12), %rdx
	movl	%r12d, %ecx
	andl	$3, %ecx
	cmpq	$3, %rdx
	jae	.LBB5_11
# %bb.10:
	xorl	%edx, %edx
	jmp	.LBB5_13
.LBB5_7:
	xorl	%eax, %eax
	jmp	.LBB5_16
.LBB5_11:
	andl	$-4, %r12d
	xorl	%edx, %edx
	.p2align	4, 0x90
.LBB5_12:                               # =>This Inner Loop Header: Depth=1
	movq	(%r14,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, (%rbx)
	movq	8(%r14,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 8(%rbx)
	movq	16(%r14,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 16(%rbx)
	movq	24(%r14,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 24(%rbx)
	addq	$32, %rbx
	addq	$4, %rdx
	cmpq	%rdx, %r12
	jne	.LBB5_12
.LBB5_13:
	testq	%rcx, %rcx
	je	.LBB5_16
# %bb.14:
	leaq	(%r14,%rdx,8), %rdx
	xorl	%esi, %esi
	.p2align	4, 0x90
.LBB5_15:                               # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rsi,8), %rdi
	bswapq	%rdi
	movq	%rdi, (%rbx,%rsi,8)
	incq	%rsi
	cmpq	%rsi, %rcx
	jne	.LBB5_15
.LBB5_16:
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Lfunc_end5:
	.size	SHA512_Final, .Lfunc_end5-SHA512_Final
                                        # -- End function
	.globl	SHA512                          # -- Begin function SHA512
	.p2align	4, 0x90
	.type	SHA512,@function
SHA512:                                 # @SHA512
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$224, %rsp
	movq	%rdx, %rbx
	movabsq	$7640891576956012808, %rax      # imm = 0x6A09E667F3BCC908
	movq	%rax, (%rsp)
	movabsq	$-4942790177534073029, %rax     # imm = 0xBB67AE8584CAA73B
	movq	%rax, 8(%rsp)
	movabsq	$4354685564936845355, %rax      # imm = 0x3C6EF372FE94F82B
	movq	%rax, 16(%rsp)
	movabsq	$-6534734903238641935, %rax     # imm = 0xA54FF53A5F1D36F1
	movq	%rax, 24(%rsp)
	movabsq	$5840696475078001361, %rax      # imm = 0x510E527FADE682D1
	movq	%rax, 32(%rsp)
	movabsq	$-7276294671716946913, %rax     # imm = 0x9B05688C2B3E6C1F
	movq	%rax, 40(%rsp)
	movabsq	$2270897969802886507, %rax      # imm = 0x1F83D9ABFB41BD6B
	movq	%rax, 48(%rsp)
	movabsq	$6620516959819538809, %rax      # imm = 0x5BE0CD19137E2179
	movq	%rax, 56(%rsp)
	movabsq	$274877906944, %rax             # imm = 0x4000000000
	movq	%rax, 208(%rsp)
	xorps	%xmm0, %xmm0
	movups	%xmm0, 64(%rsp)
	leaq	80(%rsp), %r14
	testq	%rsi, %rsi
	je	.LBB6_1
# %bb.2:
	movq	%rsi, %r12
	movq	%rdi, %r15
	leaq	(,%rsi,8), %rax
	movq	%rsi, %rcx
	shrq	$61, %rcx
	movq	%rcx, 72(%rsp)
	movq	%rax, 64(%rsp)
	cmpq	$128, %rsi
	jb	.LBB6_3
# %bb.4:
	movq	%r12, %rdx
	shrq	$7, %rdx
	movq	%rsp, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	movl	%r12d, %r13d
	andl	$127, %r13d
	je	.LBB6_23
# %bb.5:
	addq	%r12, %r15
	subq	%r13, %r15
	jmp	.LBB6_6
.LBB6_1:
	movl	212(%rsp), %r12d
	movb	$-128, 80(%rsp)
	movl	$1, %edi
	jmp	.LBB6_12
.LBB6_3:
	movq	%r12, %r13
.LBB6_6:
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	memcpy@PLT
	movl	%r13d, 208(%rsp)
	jmp	.LBB6_7
.LBB6_23:
	movl	208(%rsp), %r13d
.LBB6_7:
	movl	212(%rsp), %r12d
	movl	%r13d, %eax
	movb	$-128, 80(%rsp,%rax)
	leaq	1(%rax), %rdi
	cmpl	$112, %r13d
	jb	.LBB6_11
# %bb.8:
	cmpl	$127, %eax
	je	.LBB6_10
# %bb.9:
	movl	$127, %edx
	subq	%rax, %rdx
	addq	%r14, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB6_10:
	movq	%rsp, %rdi
	movl	$1, %edx
	movq	%r14, %rsi
	callq	sha512_block_data_order
	xorl	%edi, %edi
	jmp	.LBB6_12
.LBB6_11:
	cmpq	$112, %rdi
	je	.LBB6_13
.LBB6_12:
	movl	$112, %edx
	subq	%rdi, %rdx
	addq	%r14, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB6_13:
	movq	64(%rsp), %rax
	movq	72(%rsp), %rcx
	bswapq	%rcx
	movq	%rcx, 192(%rsp)
	bswapq	%rax
	movq	%rax, 200(%rsp)
	movq	%rsp, %r15
	movl	$1, %edx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	sha512_block_data_order
	testq	%rbx, %rbx
	je	.LBB6_22
# %bb.14:
	cmpl	$8, %r12d
	jb	.LBB6_22
# %bb.15:
	shrl	$3, %r12d
	leaq	-1(%r12), %rcx
	movl	%r12d, %eax
	andl	$3, %eax
	cmpq	$3, %rcx
	jae	.LBB6_17
# %bb.16:
	xorl	%edx, %edx
	movq	%rbx, %rcx
	jmp	.LBB6_19
.LBB6_17:
	andl	$-4, %r12d
	xorl	%edx, %edx
	movq	%rbx, %rcx
	.p2align	4, 0x90
.LBB6_18:                               # =>This Inner Loop Header: Depth=1
	movq	(%rsp,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, (%rcx)
	movq	8(%rsp,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 8(%rcx)
	movq	16(%rsp,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 16(%rcx)
	movq	24(%rsp,%rdx,8), %rsi
	bswapq	%rsi
	movq	%rsi, 24(%rcx)
	addq	$32, %rcx
	addq	$4, %rdx
	cmpq	%rdx, %r12
	jne	.LBB6_18
.LBB6_19:
	testq	%rax, %rax
	je	.LBB6_22
# %bb.20:
	leaq	(%rsp,%rdx,8), %rdx
	xorl	%esi, %esi
	.p2align	4, 0x90
.LBB6_21:                               # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rsi,8), %rdi
	bswapq	%rdi
	movq	%rdi, (%rcx,%rsi,8)
	incq	%rsi
	cmpq	%rsi, %rax
	jne	.LBB6_21
.LBB6_22:
	xorps	%xmm0, %xmm0
	movaps	%xmm0, 192(%rsp)
	movaps	%xmm0, 176(%rsp)
	movaps	%xmm0, 160(%rsp)
	movaps	%xmm0, 144(%rsp)
	movaps	%xmm0, 128(%rsp)
	movaps	%xmm0, 112(%rsp)
	movaps	%xmm0, 96(%rsp)
	movaps	%xmm0, 80(%rsp)
	movaps	%xmm0, 64(%rsp)
	movaps	%xmm0, 48(%rsp)
	movaps	%xmm0, 32(%rsp)
	movaps	%xmm0, 16(%rsp)
	movaps	%xmm0, (%rsp)
	movq	$0, 208(%rsp)
	#APP
	#NO_APP
	movq	%rbx, %rax
	addq	$224, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Lfunc_end6:
	.size	SHA512, .Lfunc_end6-SHA512
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
# %bb.0:
	pushq	%rbx
	subq	$64, %rsp
	movq	$1296236545, 16(%rsp)           # imm = 0x4D430001
	movq	$message, 24(%rsp)
	movq	$256, 32(%rsp)                  # imm = 0x100
	movq	$0, 40(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 56(%rsp)
	xorl	%ebx, %ebx
	leaq	16(%rsp), %rax
	xorl	%edx, %edx
	#APP
	rolq	$3, %rdi
	rolq	$13, %rdi
	rolq	$61, %rdi
	rolq	$51, %rdi
	xchgq	%rbx, %rbx
	#NO_APP
	movq	%rdx, 8(%rsp)
	movq	8(%rsp), %rax
	movq	$1296236545, 16(%rsp)           # imm = 0x4D430001
	movq	$out, 24(%rsp)
	movq	$64, 32(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 56(%rsp)
	leaq	16(%rsp), %rax
	xorl	%edx, %edx
	#APP
	rolq	$3, %rdi
	rolq	$13, %rdi
	rolq	$61, %rdi
	rolq	$51, %rdi
	xchgq	%rbx, %rbx
	#NO_APP
	movq	%rdx, 8(%rsp)
	movq	8(%rsp), %rax
	movq	$256, 16(%rsp)                  # imm = 0x100
	.p2align	4, 0x90
.LBB7_1:                                # =>This Inner Loop Header: Depth=1
	movl	%ebx, %eax
	andl	$15, %eax
	movl	$256, %esi                      # imm = 0x100
	subl	%eax, %esi
	movl	$message, %edi
	movl	$out, %edx
	callq	SHA512
	incl	%ebx
	cmpl	$100, %ebx
	jne	.LBB7_1
# %bb.2:
	xorl	%eax, %eax
	addq	$64, %rsp
	popq	%rbx
	retq
.Lfunc_end7:
	.size	main, .Lfunc_end7-main
                                        # -- End function
	.type	message,@object                 # @message
	.section	secret,"aw",@progbits
	.globl	message
	.p2align	4, 0x90
message:
	.ascii	"\006\343\024\201\017\353`\233\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703\364\356\330|\005\030\375\351x+`\255D\326`\027\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253\361\276\342\0253)\360\206^\002c\252\005\264\364\341^\002c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347H<\b\035\267\370\255\235\253"
	.zero	128
	.size	message, 256

	.type	out,@object                     # @out
	.globl	out
	.p2align	4, 0x90
out:
	.zero	64
	.size	out, 64

	.type	K512,@object                    # @K512
	.section	.rodata,"a",@progbits
	.p2align	4, 0x0
K512:
	.quad	4794697086780616226             # 0x428a2f98d728ae22
	.quad	8158064640168781261             # 0x7137449123ef65cd
	.quad	-5349999486874862801            # 0xb5c0fbcfec4d3b2f
	.quad	-1606136188198331460            # 0xe9b5dba58189dbbc
	.quad	4131703408338449720             # 0x3956c25bf348b538
	.quad	6480981068601479193             # 0x59f111f1b605d019
	.quad	-7908458776815382629            # 0x923f82a4af194f9b
	.quad	-6116909921290321640            # 0xab1c5ed5da6d8118
	.quad	-2880145864133508542            # 0xd807aa98a3030242
	.quad	1334009975649890238             # 0x12835b0145706fbe
	.quad	2608012711638119052             # 0x243185be4ee4b28c
	.quad	6128411473006802146             # 0x550c7dc3d5ffb4e2
	.quad	8268148722764581231             # 0x72be5d74f27b896f
	.quad	-9160688886553864527            # 0x80deb1fe3b1696b1
	.quad	-7215885187991268811            # 0x9bdc06a725c71235
	.quad	-4495734319001033068            # 0xc19bf174cf692694
	.quad	-1973867731355612462            # 0xe49b69c19ef14ad2
	.quad	-1171420211273849373            # 0xefbe4786384f25e3
	.quad	1135362057144423861             # 0xfc19dc68b8cd5b5
	.quad	2597628984639134821             # 0x240ca1cc77ac9c65
	.quad	3308224258029322869             # 0x2de92c6f592b0275
	.quad	5365058923640841347             # 0x4a7484aa6ea6e483
	.quad	6679025012923562964             # 0x5cb0a9dcbd41fbd4
	.quad	8573033837759648693             # 0x76f988da831153b5
	.quad	-7476448914759557205            # 0x983e5152ee66dfab
	.quad	-6327057829258317296            # 0xa831c66d2db43210
	.quad	-5763719355590565569            # 0xb00327c898fb213f
	.quad	-4658551843659510044            # 0xbf597fc7beef0ee4
	.quad	-4116276920077217854            # 0xc6e00bf33da88fc2
	.quad	-3051310485924567259            # 0xd5a79147930aa725
	.quad	489312712824947311              # 0x6ca6351e003826f
	.quad	1452737877330783856             # 0x142929670a0e6e70
	.quad	2861767655752347644             # 0x27b70a8546d22ffc
	.quad	3322285676063803686             # 0x2e1b21385c26c926
	.quad	5560940570517711597             # 0x4d2c6dfc5ac42aed
	.quad	5996557281743188959             # 0x53380d139d95b3df
	.quad	7280758554555802590             # 0x650a73548baf63de
	.quad	8532644243296465576             # 0x766a0abb3c77b2a8
	.quad	-9096487096722542874            # 0x81c2c92e47edaee6
	.quad	-7894198246740708037            # 0x92722c851482353b
	.quad	-6719396339535248540            # 0xa2bfe8a14cf10364
	.quad	-6333637450476146687            # 0xa81a664bbc423001
	.quad	-4446306890439682159            # 0xc24b8b70d0f89791
	.quad	-4076793802049405392            # 0xc76c51a30654be30
	.quad	-3345356375505022440            # 0xd192e819d6ef5218
	.quad	-2983346525034927856            # 0xd69906245565a910
	.quad	-860691631967231958             # 0xf40e35855771202a
	.quad	1182934255886127544             # 0x106aa07032bbd1b8
	.quad	1847814050463011016             # 0x19a4c116b8d2d0c8
	.quad	2177327727835720531             # 0x1e376c085141ab53
	.quad	2830643537854262169             # 0x2748774cdf8eeb99
	.quad	3796741975233480872             # 0x34b0bcb5e19b48a8
	.quad	4115178125766777443             # 0x391c0cb3c5c95a63
	.quad	5681478168544905931             # 0x4ed8aa4ae3418acb
	.quad	6601373596472566643             # 0x5b9cca4f7763e373
	.quad	7507060721942968483             # 0x682e6ff3d6b2b8a3
	.quad	8399075790359081724             # 0x748f82ee5defb2fc
	.quad	8693463985226723168             # 0x78a5636f43172f60
	.quad	-8878714635349349518            # 0x84c87814a1f0ab72
	.quad	-8302665154208450068            # 0x8cc702081a6439ec
	.quad	-8016688836872298968            # 0x90befffa23631e28
	.quad	-6606660893046293015            # 0xa4506cebde82bde9
	.quad	-4685533653050689259            # 0xbef9a3f7b2c67915
	.quad	-4147400797238176981            # 0xc67178f2e372532b
	.quad	-3880063495543823972            # 0xca273eceea26619c
	.quad	-3348786107499101689            # 0xd186b8c721c0c207
	.quad	-1523767162380948706            # 0xeada7dd6cde0eb1e
	.quad	-757361751448694408             # 0xf57d4f7fee6ed178
	.quad	500013540394364858              # 0x6f067aa72176fba
	.quad	748580250866718886              # 0xa637dc5a2c898a6
	.quad	1242879168328830382             # 0x113f9804bef90dae
	.quad	1977374033974150939             # 0x1b710b35131c471b
	.quad	2944078676154940804             # 0x28db77f523047d84
	.quad	3659926193048069267             # 0x32caab7b40c72493
	.quad	4368137639120453308             # 0x3c9ebe0a15c9bebc
	.quad	4836135668995329356             # 0x431d67c49c100d4c
	.quad	5532061633213252278             # 0x4cc5d4becb3e42b6
	.quad	6448918945643986474             # 0x597f299cfc657e2a
	.quad	6902733635092675308             # 0x5fcb6fab3ad6faec
	.quad	7801388544844847127             # 0x6c44198c4a475817
	.size	K512, 640

	.ident	"Debian clang version 16.0.6 (++20231112084702+7cbf1a259152-1~exp1~20231112084757.16)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym message
	.addrsig_sym out
