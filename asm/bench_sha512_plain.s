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
	subq	$192, %rsp
	movq	%rdi, 64(%rsp)                  # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB4_5
# %bb.1:
	movq	64(%rsp), %rax                  # 8-byte Reload
	movq	(%rax), %r15
	movq	8(%rax), %rbx
	movq	16(%rax), %r10
	movq	24(%rax), %rdi
	movq	32(%rax), %r13
	movq	40(%rax), %r14
	movq	48(%rax), %r8
	movq	56(%rax), %r9
	.p2align	4, 0x90
.LBB4_2:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_3 Depth 2
	movq	%rdx, 168(%rsp)                 # 8-byte Spill
	movq	(%rsi), %rdx
	bswapq	%rdx
	movq	%r13, %rax
	rolq	$50, %rax
	movq	8(%rsi), %rbp
	movq	%r13, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%r13, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r14, %r12
	andq	%r13, %r12
	movq	%r13, %rcx
	notq	%rcx
	andq	%r8, %rcx
	movq	%r9, 96(%rsp)                   # 8-byte Spill
	addq	%r9, %r12
	addq	%rcx, %r12
	addq	%rax, %r12
	movq	%r15, %rax
	rolq	$36, %rax
	movabsq	$4794697086780616226, %rcx      # imm = 0x428A2F98D728AE22
	addq	%rcx, %r12
	movq	%r15, %rcx
	rolq	$30, %rcx
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	addq	%rdx, %r12
	movq	%r15, %rdx
	rolq	$25, %rdx
	xorq	%rax, %rcx
	xorq	%rcx, %rdx
	movq	%r10, %rax
	xorq	%rbx, %rax
	andq	%r15, %rax
	movq	%r10, %r9
	andq	%rbx, %r9
	xorq	%rax, %r9
	addq	%rdx, %r9
	addq	%r12, %r9
	bswapq	%rbp
	movq	%rdi, 128(%rsp)                 # 8-byte Spill
	addq	%rdi, %r12
	movq	%r12, %rcx
	rolq	$50, %rcx
	movq	%rbx, %rax
	movq	%r12, %rdx
	rolq	$46, %rdx
	andq	%r15, %rax
	movq	%r12, %rdi
	rolq	$23, %rdi
	xorq	%rcx, %rdx
	xorq	%rdx, %rdi
	movq	%r12, %r11
	andq	%r13, %r11
	movq	%r12, %rcx
	notq	%rcx
	andq	%r14, %rcx
	movq	%r8, 104(%rsp)                  # 8-byte Spill
	movq	%rbp, 56(%rsp)                  # 8-byte Spill
	addq	%r8, %r11
	addq	%rbp, %r11
	addq	%rcx, %r11
	movabsq	$8158064640168781261, %rcx      # imm = 0x7137449123EF65CD
	addq	%rcx, %r11
	addq	%rdi, %r11
	movq	%r9, %rcx
	rolq	$36, %rcx
	movq	%r9, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%r9, %rcx
	rolq	$25, %rcx
	xorq	%rdx, %rcx
	movq	%rbx, %rdi
	xorq	%r15, %rdi
	andq	%r9, %rdi
	xorq	%rax, %rdi
	addq	%rcx, %rdi
	addq	%r11, %rdi
	movq	16(%rsi), %rbp
	bswapq	%rbp
	movq	%r10, 136(%rsp)                 # 8-byte Spill
	addq	%r10, %r11
	movq	%r11, %rcx
	rolq	$50, %rcx
	movq	%r9, %rax
	movq	%r11, %rdx
	rolq	$46, %rdx
	andq	%r15, %rax
	movq	%r11, %r8
	rolq	$23, %r8
	xorq	%rcx, %rdx
	xorq	%rdx, %r8
	movq	%r11, %r10
	andq	%r12, %r10
	movq	%r11, %rcx
	notq	%rcx
	andq	%r13, %rcx
	movq	%r14, 112(%rsp)                 # 8-byte Spill
	movq	%rbp, 48(%rsp)                  # 8-byte Spill
	addq	%r14, %r10
	addq	%rbp, %r10
	addq	%rcx, %r10
	movabsq	$-5349999486874862801, %rcx     # imm = 0xB5C0FBCFEC4D3B2F
	addq	%rcx, %r10
	addq	%r8, %r10
	movq	%rdi, %rcx
	rolq	$36, %rcx
	movq	%rdi, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%rdi, %rcx
	rolq	$25, %rcx
	xorq	%rdx, %rcx
	movq	%r9, %r8
	xorq	%r15, %r8
	andq	%rdi, %r8
	xorq	%rax, %r8
	addq	%rcx, %r8
	addq	%r10, %r8
	movq	24(%rsi), %r14
	bswapq	%r14
	movq	%rbx, 144(%rsp)                 # 8-byte Spill
	addq	%rbx, %r10
	movq	%r10, %rax
	rolq	$50, %rax
	movq	%rdi, %rdx
	movq	%r10, %rcx
	rolq	$46, %rcx
	andq	%r9, %rdx
	movq	%r10, %rbx
	rolq	$23, %rbx
	xorq	%rax, %rcx
	xorq	%rcx, %rbx
	movq	%r10, %rcx
	andq	%r11, %rcx
	movq	%r10, %rax
	notq	%rax
	andq	%r12, %rax
	movq	%r13, 120(%rsp)                 # 8-byte Spill
	movq	%r14, 8(%rsp)                   # 8-byte Spill
	addq	%r13, %r14
	addq	%r14, %rcx
	addq	%rax, %rcx
	movabsq	$-1606136188198331460, %rax     # imm = 0xE9B5DBA58189DBBC
	addq	%rax, %rcx
	addq	%rbx, %rcx
	movq	%r8, %rax
	rolq	$36, %rax
	movq	%r8, %rbx
	rolq	$30, %rbx
	xorq	%rax, %rbx
	movq	%r8, %r14
	rolq	$25, %r14
	xorq	%rbx, %r14
	movq	%rdi, %rax
	xorq	%r9, %rax
	andq	%r8, %rax
	xorq	%rdx, %rax
	addq	%r14, %rax
	addq	%rcx, %rax
	movq	32(%rsi), %r14
	bswapq	%r14
	movq	%r15, 152(%rsp)                 # 8-byte Spill
	addq	%r15, %rcx
	movq	%rcx, %rdx
	rolq	$50, %rdx
	movq	%rcx, %rbx
	rolq	$46, %rbx
	xorq	%rdx, %rbx
	movq	%rcx, %rdx
	rolq	$23, %rdx
	xorq	%rbx, %rdx
	movq	%rcx, %rbx
	andq	%r10, %rbx
	movq	%r14, -24(%rsp)                 # 8-byte Spill
	addq	%r14, %r12
	addq	%rbx, %r12
	movq	%rcx, %rbx
	notq	%rbx
	andq	%r11, %rbx
	addq	%rbx, %r12
	movq	%rax, %rbx
	rolq	$36, %rbx
	movabsq	$4131703408338449720, %r14      # imm = 0x3956C25BF348B538
	addq	%r14, %r12
	movq	%rax, %r14
	rolq	$30, %r14
	addq	%rdx, %r12
	movq	%rax, %r15
	rolq	$25, %r15
	xorq	%rbx, %r14
	xorq	%r14, %r15
	movq	%r8, %rbx
	andq	%rdi, %rbx
	movq	%r8, %rdx
	xorq	%rdi, %rdx
	andq	%rax, %rdx
	xorq	%rbx, %rdx
	addq	%r15, %rdx
	movq	40(%rsi), %r15
	bswapq	%r15
	addq	%r12, %r9
	movq	%r9, %rbx
	rolq	$50, %rbx
	addq	%r12, %rdx
	movq	%r9, %r14
	rolq	$46, %r14
	xorq	%rbx, %r14
	movq	%r9, %rbx
	rolq	$23, %rbx
	xorq	%r14, %rbx
	movq	%r9, %r14
	andq	%rcx, %r14
	movq	%r15, (%rsp)                    # 8-byte Spill
	addq	%r15, %r11
	addq	%r14, %r11
	movq	%r9, %r14
	notq	%r14
	andq	%r10, %r14
	addq	%r14, %r11
	movq	%rdx, %r15
	rolq	$36, %r15
	movabsq	$6480981068601479193, %r14      # imm = 0x59F111F1B605D019
	addq	%r14, %r11
	movq	%rdx, %r14
	rolq	$30, %r14
	addq	%rbx, %r11
	movq	%rdx, %rbx
	rolq	$25, %rbx
	xorq	%r15, %r14
	xorq	%r14, %rbx
	movq	%rax, %r14
	andq	%r8, %r14
	movq	%rax, %r13
	xorq	%r8, %r13
	andq	%rdx, %r13
	xorq	%r14, %r13
	addq	%rbx, %r13
	movq	48(%rsi), %r14
	bswapq	%r14
	addq	%r11, %rdi
	movq	%rdi, %rbx
	rolq	$50, %rbx
	addq	%r11, %r13
	movq	%rdi, %r11
	rolq	$46, %r11
	xorq	%rbx, %r11
	movq	%rdi, %rbx
	rolq	$23, %rbx
	xorq	%r11, %rbx
	movq	%rdi, %r11
	andq	%r9, %r11
	movq	%r14, 40(%rsp)                  # 8-byte Spill
	addq	%r14, %r10
	addq	%r11, %r10
	movq	%rdi, %r11
	notq	%r11
	andq	%rcx, %r11
	addq	%r11, %r10
	movq	%r13, %r11
	rolq	$36, %r11
	movabsq	$-7908458776815382629, %r14     # imm = 0x923F82A4AF194F9B
	addq	%r14, %r10
	movq	%r13, %r14
	rolq	$30, %r14
	addq	%rbx, %r10
	movq	%r13, %rbx
	rolq	$25, %rbx
	xorq	%r11, %r14
	xorq	%r14, %rbx
	movq	%rdx, %r11
	andq	%rax, %r11
	movq	%rsi, %r14
	movq	%rdx, %rsi
	xorq	%rax, %rsi
	andq	%r13, %rsi
	xorq	%r11, %rsi
	addq	%rbx, %rsi
	movq	56(%r14), %rbx
	bswapq	%rbx
	movq	%rbx, -16(%rsp)                 # 8-byte Spill
	addq	%r10, %r8
	movq	%r8, %r11
	rolq	$50, %r11
	addq	%r10, %rsi
	movq	%r8, %r10
	rolq	$46, %r10
	xorq	%r11, %r10
	movq	%r8, %r11
	rolq	$23, %r11
	xorq	%r10, %r11
	movq	%r8, %r10
	andq	%rdi, %r10
	addq	%rbx, %rcx
	addq	%r10, %rcx
	movq	%r8, %r10
	notq	%r10
	andq	%r9, %r10
	addq	%r10, %rcx
	movq	%rsi, %r10
	rolq	$36, %r10
	movabsq	$-6116909921290321640, %rbx     # imm = 0xAB1C5ED5DA6D8118
	addq	%rbx, %rcx
	movq	%rsi, %rbx
	rolq	$30, %rbx
	addq	%r11, %rcx
	movq	%rsi, %r11
	rolq	$25, %r11
	xorq	%r10, %rbx
	xorq	%rbx, %r11
	movq	%r13, %r10
	andq	%rdx, %r10
	movq	%r13, %rbx
	xorq	%rdx, %rbx
	andq	%rsi, %rbx
	xorq	%r10, %rbx
	addq	%r11, %rbx
	movq	64(%r14), %r11
	bswapq	%r11
	addq	%rcx, %rax
	movq	%rax, %r10
	rolq	$50, %r10
	addq	%rcx, %rbx
	movq	%rax, %rcx
	rolq	$46, %rcx
	xorq	%r10, %rcx
	movq	%rax, %r10
	rolq	$23, %r10
	xorq	%rcx, %r10
	movq	%rax, %rcx
	andq	%r8, %rcx
	movq	%r11, -8(%rsp)                  # 8-byte Spill
	addq	%r11, %r9
	addq	%rcx, %r9
	movq	%rax, %rcx
	notq	%rcx
	andq	%rdi, %rcx
	addq	%rcx, %r9
	movq	%rbx, %rcx
	rolq	$36, %rcx
	movabsq	$-2880145864133508542, %r11     # imm = 0xD807AA98A3030242
	addq	%r11, %r9
	movq	%rbx, %r11
	rolq	$30, %r11
	addq	%r10, %r9
	movq	%rbx, %r10
	rolq	$25, %r10
	xorq	%rcx, %r11
	xorq	%r11, %r10
	movq	%rsi, %rcx
	andq	%r13, %rcx
	movq	%rsi, %r12
	xorq	%r13, %r12
	andq	%rbx, %r12
	xorq	%rcx, %r12
	addq	%r10, %r12
	movq	72(%r14), %r10
	bswapq	%r10
	movq	%r10, -72(%rsp)                 # 8-byte Spill
	addq	%r9, %rdx
	movq	%rdx, %rcx
	rolq	$50, %rcx
	addq	%r9, %r12
	movq	%rdx, %r9
	rolq	$46, %r9
	xorq	%rcx, %r9
	movq	%rdx, %rcx
	rolq	$23, %rcx
	xorq	%r9, %rcx
	movq	%rdx, %r9
	andq	%rax, %r9
	addq	%r10, %rdi
	addq	%r9, %rdi
	movq	%rdx, %r9
	notq	%r9
	andq	%r8, %r9
	addq	%r9, %rdi
	movq	%r12, %r9
	rolq	$36, %r9
	movabsq	$1334009975649890238, %r10      # imm = 0x12835B0145706FBE
	addq	%r10, %rdi
	movq	%r12, %r10
	rolq	$30, %r10
	addq	%rcx, %rdi
	movq	%r12, %rcx
	rolq	$25, %rcx
	xorq	%r9, %r10
	xorq	%r10, %rcx
	movq	%rbx, %r9
	andq	%rsi, %r9
	movq	%rbx, %rbp
	xorq	%rsi, %rbp
	andq	%r12, %rbp
	xorq	%r9, %rbp
	addq	%rcx, %rbp
	movq	80(%r14), %rcx
	movq	%r14, %r10
	bswapq	%rcx
	movq	%rcx, %r9
	movq	%rcx, -80(%rsp)                 # 8-byte Spill
	addq	%rdi, %r13
	movq	%r13, %rcx
	rolq	$50, %rcx
	addq	%rdi, %rbp
	movq	%r13, %rdi
	rolq	$46, %rdi
	xorq	%rcx, %rdi
	movq	%r13, %rcx
	rolq	$23, %rcx
	xorq	%rdi, %rcx
	movq	%r13, %rdi
	andq	%rdx, %rdi
	addq	%r9, %r8
	addq	%rdi, %r8
	movq	%r13, %rdi
	notq	%rdi
	andq	%rax, %rdi
	addq	%rdi, %r8
	movq	%rbp, %rdi
	rolq	$36, %rdi
	movabsq	$2608012711638119052, %r9       # imm = 0x243185BE4EE4B28C
	addq	%r9, %r8
	movq	%rbp, %r9
	rolq	$30, %r9
	addq	%rcx, %r8
	movq	%rbp, %rcx
	rolq	$25, %rcx
	xorq	%rdi, %r9
	xorq	%r9, %rcx
	movq	%r12, %r9
	andq	%rbx, %r9
	movq	%r12, %r14
	xorq	%rbx, %r14
	andq	%rbp, %r14
	xorq	%r9, %r14
	addq	%rcx, %r14
	movq	88(%r10), %rdi
	bswapq	%rdi
	addq	%r8, %rsi
	movq	%rsi, %rcx
	rolq	$50, %rcx
	addq	%r8, %r14
	movq	%rsi, %r8
	rolq	$46, %r8
	xorq	%rcx, %r8
	movq	%rsi, %rcx
	rolq	$23, %rcx
	xorq	%r8, %rcx
	movq	%rsi, %r8
	andq	%r13, %r8
	movq	%rdi, -128(%rsp)                # 8-byte Spill
	addq	%rdi, %rax
	addq	%r8, %rax
	movq	%rsi, %r8
	notq	%r8
	andq	%rdx, %r8
	addq	%r8, %rax
	movq	%r14, %r8
	rolq	$36, %r8
	movabsq	$6128411473006802146, %rdi      # imm = 0x550C7DC3D5FFB4E2
	addq	%rdi, %rax
	movq	%r14, %r9
	rolq	$30, %r9
	addq	%rcx, %rax
	movq	%r14, %rcx
	rolq	$25, %rcx
	xorq	%r8, %r9
	xorq	%r9, %rcx
	movq	%rbp, %r8
	andq	%r12, %r8
	movq	%rbp, %r9
	xorq	%r12, %r9
	andq	%r14, %r9
	xorq	%r8, %r9
	addq	%rcx, %r9
	movq	96(%r10), %rdi
	bswapq	%rdi
	addq	%rax, %rbx
	movq	%rbx, %rcx
	rolq	$50, %rcx
	addq	%rax, %r9
	movq	%rbx, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	movq	%rbx, %rcx
	rolq	$23, %rcx
	xorq	%rax, %rcx
	movq	%rbx, %rax
	andq	%rsi, %rax
	movq	%rdi, -112(%rsp)                # 8-byte Spill
	addq	%rdi, %rdx
	addq	%rax, %rdx
	movq	%rbx, %rax
	notq	%rax
	andq	%r13, %rax
	addq	%rax, %rdx
	movq	%r9, %rax
	rolq	$36, %rax
	movabsq	$8268148722764581231, %rdi      # imm = 0x72BE5D74F27B896F
	addq	%rdi, %rdx
	movq	%r9, %r8
	rolq	$30, %r8
	addq	%rcx, %rdx
	movq	%r9, %rcx
	rolq	$25, %rcx
	xorq	%rax, %r8
	xorq	%r8, %rcx
	movq	%r14, %rax
	andq	%rbp, %rax
	movq	%r14, %r8
	xorq	%rbp, %r8
	andq	%r9, %r8
	xorq	%rax, %r8
	addq	%rcx, %r8
	movq	104(%r10), %rdi
	bswapq	%rdi
	addq	%rdx, %r12
	movq	%r12, %rax
	rolq	$50, %rax
	addq	%rdx, %r8
	movq	%r12, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%r12, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r12, %rcx
	andq	%rbx, %rcx
	movq	%rdi, -120(%rsp)                # 8-byte Spill
	addq	%rdi, %r13
	addq	%rcx, %r13
	movq	%r12, %rcx
	notq	%rcx
	andq	%rsi, %rcx
	addq	%rcx, %r13
	movq	%r8, %rcx
	rolq	$36, %rcx
	movabsq	$-9160688886553864527, %rdx     # imm = 0x80DEB1FE3B1696B1
	addq	%rdx, %r13
	movq	%r8, %rdx
	rolq	$30, %rdx
	addq	%rax, %r13
	movq	%r8, %rax
	rolq	$25, %rax
	xorq	%rcx, %rdx
	xorq	%rdx, %rax
	movq	%r9, %rcx
	andq	%r14, %rcx
	movq	%r9, %r11
	xorq	%r14, %r11
	andq	%r8, %r11
	xorq	%rcx, %r11
	addq	%rax, %r11
	movq	112(%r10), %r15
	bswapq	%r15
	addq	%r13, %rbp
	movq	%rbp, %rax
	rolq	$50, %rax
	addq	%r13, %r11
	movq	%rbp, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%rbp, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%rbp, %rcx
	andq	%r12, %rcx
	addq	%r15, %rsi
	addq	%rcx, %rsi
	movq	%rbp, %rcx
	notq	%rcx
	andq	%rbx, %rcx
	addq	%rcx, %rsi
	movq	%r11, %rcx
	rolq	$36, %rcx
	movabsq	$-7215885187991268811, %rdx     # imm = 0x9BDC06A725C71235
	addq	%rdx, %rsi
	movq	%r11, %rdx
	rolq	$30, %rdx
	addq	%rax, %rsi
	movq	%r11, %rax
	rolq	$25, %rax
	xorq	%rcx, %rdx
	xorq	%rdx, %rax
	movq	%r8, %rcx
	andq	%r9, %rcx
	movq	%r8, %rdi
	xorq	%r9, %rdi
	andq	%r11, %rdi
	xorq	%rcx, %rdi
	addq	%rax, %rdi
	movq	%r10, 160(%rsp)                 # 8-byte Spill
	movq	120(%r10), %rdx
	bswapq	%rdx
	addq	%rsi, %r14
	movq	%r14, %rax
	rolq	$50, %rax
	addq	%rsi, %rdi
	movq	%r14, %rcx
	rolq	$46, %rcx
	xorq	%rax, %rcx
	movq	%r14, %rax
	rolq	$23, %rax
	xorq	%rcx, %rax
	movq	%r14, %rcx
	andq	%rbp, %rcx
	movq	%rdx, -96(%rsp)                 # 8-byte Spill
	addq	%rdx, %rbx
	addq	%rcx, %rbx
	movq	%r14, %rcx
	notq	%rcx
	movq	%r12, -88(%rsp)                 # 8-byte Spill
	andq	%r12, %rcx
	addq	%rcx, %rbx
	movq	%rdi, %rcx
	rolq	$36, %rcx
	movabsq	$-4495734319001033068, %rdx     # imm = 0xC19BF174CF692694
	addq	%rdx, %rbx
	movq	%rdi, %rdx
	rolq	$30, %rdx
	addq	%rax, %rbx
	movq	%rdi, %rax
	rolq	$25, %rax
	xorq	%rcx, %rdx
	xorq	%rdx, %rax
	movq	%r11, %rcx
	andq	%r8, %rcx
	movq	%r11, -104(%rsp)                # 8-byte Spill
	movq	%r11, %rdx
	xorq	%r8, %rdx
	movq	%rdi, -64(%rsp)                 # 8-byte Spill
	andq	%rdi, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %rdx
	addq	%rbx, %r9
	addq	%rbx, %rdx
	movq	%rdx, %rbx
	xorl	%edi, %edi
	.p2align	4, 0x90
.LBB4_3:                                #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, -48(%rsp)                 # 8-byte Spill
	movq	56(%rsp), %r11                  # 8-byte Reload
	movq	%r11, %rax
	rorq	%rax
	movq	%r11, %rcx
	rolq	$56, %rcx
	xorq	%rax, %rcx
	movq	%r9, %rdx
	rolq	$50, %rdx
	movq	%r11, %rax
	shrq	$7, %rax
	movq	%r9, %rsi
	rolq	$46, %rsi
	xorq	%rcx, %rax
	movq	%r9, %r10
	rolq	$23, %r10
	xorq	%rdx, %rsi
	xorq	%rsi, %r10
	movq	%r14, %rdx
	andq	%r9, %rdx
	addq	-88(%rsp), %rdx                 # 8-byte Folded Reload
	movq	%r9, %rcx
	notq	%rcx
	andq	%rbp, %rcx
	addq	%rdx, %rcx
	movq	%r15, %rdx
	rolq	$45, %rdx
	movq	%r15, %rsi
	rolq	$3, %rsi
	xorq	%rdx, %rsi
	addq	%r10, %rcx
	movq	%r15, %rdx
	shrq	$6, %rdx
	xorq	%rsi, %rdx
	movq	%rbx, %rsi
	rolq	$36, %rsi
	movq	%rbx, %r10
	rolq	$30, %r10
	xorq	%rsi, %r10
	movq	%rbx, %rsi
	rolq	$25, %rsi
	xorq	%r10, %rsi
	movq	-104(%rsp), %r12                # 8-byte Reload
	movq	%r12, %r10
	movq	-64(%rsp), %r13                 # 8-byte Reload
	xorq	%r13, %r10
	movq	%rbx, %r15
	movq	%rbx, -32(%rsp)                 # 8-byte Spill
	movq	%r12, %rbx
	andq	%r13, %rbx
	andq	%r15, %r10
	xorq	%r10, %rbx
	addq	%rsi, %rbx
	movq	16(%rsp), %r15                  # 8-byte Reload
	addq	-72(%rsp), %r15                 # 8-byte Folded Reload
	addq	%rdx, %r15
	movq	48(%rsp), %r10                  # 8-byte Reload
	movq	%r10, %rdx
	rorq	%rdx
	movq	%r10, %rsi
	rolq	$56, %rsi
	xorq	%rdx, %rsi
	addq	%rax, %r15
	movq	%r10, %rax
	shrq	$7, %rax
	xorq	%rsi, %rax
	addq	%r15, %rcx
	movq	%rdi, -56(%rsp)                 # 8-byte Spill
	leaq	K512(%rip), %rdx
	addq	128(%rdx,%rdi,8), %rcx
	addq	%rcx, %r8
	addq	%rcx, %rbx
	movq	-96(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, %rcx
	rolq	$45, %rcx
	movq	%rsi, %rdx
	rolq	$3, %rdx
	xorq	%rcx, %rdx
	movq	%rsi, %rcx
	shrq	$6, %rcx
	xorq	%rdx, %rcx
	addq	-80(%rsp), %r11                 # 8-byte Folded Reload
	addq	%rcx, %r11
	movq	%r8, %rcx
	rolq	$50, %rcx
	movq	%r8, %rdx
	rolq	$46, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %r11
	movq	%r8, %rax
	rolq	$23, %rax
	xorq	%rdx, %rax
	movq	%rbx, %rcx
	rolq	$36, %rcx
	movq	%rbx, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%rbx, %rcx
	movq	%rbx, -88(%rsp)                 # 8-byte Spill
	rolq	$25, %rcx
	xorq	%rdx, %rcx
	movq	%r13, -64(%rsp)                 # 8-byte Spill
	movq	%r13, %rdx
	movq	-32(%rsp), %rdi                 # 8-byte Reload
	andq	%rdi, %rdx
	xorq	%rdi, %r13
	andq	%rbx, %r13
	xorq	%rdx, %r13
	addq	%rcx, %r13
	movq	%r13, %rdi
	movq	%r8, %rcx
	andq	%r9, %rcx
	addq	%r11, %rbp
	leaq	K512(%rip), %rdx
	movq	-56(%rsp), %rsi                 # 8-byte Reload
	addq	136(%rdx,%rsi,8), %rbp
	addq	%rcx, %rbp
	movq	%r8, %rcx
	notq	%rcx
	andq	%r14, %rcx
	addq	%rcx, %rbp
	movq	8(%rsp), %r13                   # 8-byte Reload
	movq	%r13, %rcx
	rorq	%rcx
	movq	%r13, %rdx
	rolq	$56, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %rbp
	movq	%r15, 16(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	rolq	$45, %rax
	movq	%r13, %rcx
	shrq	$7, %rcx
	movq	%r15, %rsi
	rolq	$3, %rsi
	xorq	%rdx, %rcx
	xorq	%rax, %rsi
	addq	%rbp, %r12
	addq	%rbp, %rdi
	shrq	$6, %r15
	xorq	%rsi, %r15
	addq	-128(%rsp), %r10                # 8-byte Folded Reload
	addq	%rcx, %r10
	movq	%r12, %rbx
	movq	%r12, %rcx
	rolq	$50, %rcx
	addq	%r15, %r10
	movq	%r12, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	addq	%r10, %r14
	movq	%r12, %rcx
	andq	%r8, %rcx
	leaq	K512(%rip), %r15
	movq	-56(%rsp), %rbp                 # 8-byte Reload
	addq	144(%r15,%rbp,8), %r14
	addq	%rcx, %r14
	movq	%r12, %rcx
	notq	%rcx
	andq	%r9, %rcx
	addq	%rcx, %r14
	movq	-88(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rcx
	movq	-32(%rsp), %rdx                 # 8-byte Reload
	andq	%rdx, %rcx
	xorq	%rdx, %r12
	movq	%rdi, 88(%rsp)                  # 8-byte Spill
	andq	%rdi, %r12
	xorq	%rcx, %r12
	movq	%rdi, %rcx
	rolq	$36, %rcx
	movq	%rdi, %rdx
	rolq	$30, %rdx
	xorq	%rcx, %rdx
	movq	%rbx, %rcx
	rolq	$23, %rcx
	xorq	%rax, %rcx
	movq	%rdi, %rax
	rolq	$25, %rax
	xorq	%rdx, %rax
	movq	-24(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, %rdx
	rorq	%rdx
	movq	%rdi, %rsi
	rolq	$56, %rsi
	xorq	%rdx, %rsi
	addq	%rcx, %r14
	movq	%rdi, %rcx
	shrq	$7, %rcx
	xorq	%rsi, %rcx
	addq	%rax, %r12
	movq	%r11, 56(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	rolq	$45, %rax
	movq	%r11, %rdx
	rolq	$3, %rdx
	xorq	%rax, %rdx
	shrq	$6, %r11
	xorq	%rdx, %r11
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	addq	%r14, %rdx
	addq	%r14, %r12
	addq	-112(%rsp), %r13                # 8-byte Folded Reload
	addq	%rcx, %r13
	addq	%r11, %r13
	movq	%r13, 8(%rsp)                   # 8-byte Spill
	addq	%r13, %r9
	addq	152(%r15,%rbp,8), %r9
	movq	%rdx, %r15
	movq	%rdx, %rax
	movq	%rbx, %rdi
	movq	%rbx, -104(%rsp)                # 8-byte Spill
	andq	%rbx, %rax
	addq	%rax, %r9
	movq	%rdx, %rcx
	rolq	$50, %rcx
	movq	%rdx, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	movq	%rdx, %rcx
	notq	%rcx
	andq	%r8, %rcx
	addq	%rcx, %r9
	movq	88(%rsp), %r13                  # 8-byte Reload
	movq	%r13, %rcx
	movq	-88(%rsp), %rbp                 # 8-byte Reload
	andq	%rbp, %rcx
	movq	%r13, %r11
	xorq	%rbp, %r11
	movq	(%rsp), %r14                    # 8-byte Reload
	movq	%r14, %rdx
	rorq	%rdx
	andq	%r12, %r11
	xorq	%rcx, %r11
	movq	%r14, %rcx
	rolq	$56, %rcx
	xorq	%rdx, %rcx
	movq	%r12, %rdx
	rolq	$36, %rdx
	movq	%r12, %rsi
	rolq	$30, %rsi
	xorq	%rdx, %rsi
	movq	%r15, %rdx
	movq	%r15, -64(%rsp)                 # 8-byte Spill
	rolq	$23, %rdx
	xorq	%rax, %rdx
	movq	%r12, %rax
	rolq	$25, %rax
	xorq	%rsi, %rax
	movq	%r14, %rsi
	shrq	$7, %rsi
	xorq	%rcx, %rsi
	addq	%rdx, %r9
	movq	%r10, 48(%rsp)                  # 8-byte Spill
	movq	%r10, %rcx
	rolq	$45, %rcx
	movq	%r10, %rdx
	rolq	$3, %rdx
	xorq	%rcx, %rdx
	addq	%rax, %r11
	shrq	$6, %r10
	xorq	%rdx, %r10
	movq	-24(%rsp), %rcx                 # 8-byte Reload
	addq	-120(%rsp), %rcx                # 8-byte Folded Reload
	addq	%rsi, %rcx
	addq	%r10, %rcx
	movq	%rcx, %rdx
	movq	%rcx, -24(%rsp)                 # 8-byte Spill
	movq	-32(%rsp), %rbx                 # 8-byte Reload
	addq	%r9, %rbx
	addq	%r9, %r11
	movq	%rbx, %rcx
	rolq	$50, %rcx
	movq	%rbx, %rax
	rolq	$46, %rax
	xorq	%rcx, %rax
	addq	%rdx, %r8
	leaq	K512(%rip), %rcx
	movq	-56(%rsp), %r9                  # 8-byte Reload
	addq	160(%rcx,%r9,8), %r8
	movq	%rbx, %rcx
	andq	%r15, %rcx
	addq	%rcx, %r8
	movq	%rbx, %rcx
	movq	%rbx, %r15
	notq	%rcx
	andq	%rdi, %rcx
	addq	%rcx, %r8
	movq	%r12, %rbx
	movq	%r12, 32(%rsp)                  # 8-byte Spill
	movq	%r12, %rcx
	andq	%r13, %rcx
	movq	40(%rsp), %r10                  # 8-byte Reload
	movq	%r10, %rdx
	rorq	%rdx
	xorq	%r13, %rbx
	movq	%r10, %rsi
	rolq	$56, %rsi
	andq	%r11, %rbx
	xorq	%rcx, %rbx
	xorq	%rdx, %rsi
	movq	%r10, %rcx
	shrq	$7, %rcx
	xorq	%rsi, %rcx
	movq	%r11, %rdx
	rolq	$36, %rdx
	movq	%r11, %rsi
	rolq	$30, %rsi
	xorq	%rdx, %rsi
	movq	%r15, -32(%rsp)                 # 8-byte Spill
	movq	%r15, %rdx
	rolq	$23, %rdx
	xorq	%rax, %rdx
	movq	%r11, %rax
	rolq	$25, %rax
	xorq	%rsi, %rax
	movq	8(%rsp), %r10                   # 8-byte Reload
	movq	%r10, %rsi
	rolq	$45, %rsi
	movq	%r10, %rdi
	rolq	$3, %rdi
	xorq	%rsi, %rdi
	addq	%rdx, %r8
	movq	%r10, %rdx
	shrq	$6, %rdx
	xorq	%rdi, %rdx
	addq	%rax, %rbx
	addq	-48(%rsp), %r14                 # 8-byte Folded Reload
	addq	%rcx, %r14
	addq	%r8, %rbp
	addq	%r8, %rbx
	addq	%rdx, %r14
	movq	%r14, %rdx
	movq	%r14, (%rsp)                    # 8-byte Spill
	movq	%rbp, %rcx
	rolq	$50, %rcx
	movq	%rbp, %rax
	movq	%rbp, -88(%rsp)                 # 8-byte Spill
	rolq	$46, %rax
	xorq	%rcx, %rax
	leaq	K512(%rip), %rcx
	movq	168(%rcx,%r9,8), %rsi
	addq	%r14, %rsi
	addq	-104(%rsp), %rsi                # 8-byte Folded Reload
	movq	%rbp, %rcx
	andq	%r15, %rcx
	addq	%rcx, %rsi
	movq	%rsi, %rdi
	movq	%rbp, %rcx
	notq	%rcx
	movq	-16(%rsp), %r8                  # 8-byte Reload
	movq	%r8, %rsi
	rolq	$56, %rsi
	movq	%rsi, -104(%rsp)                # 8-byte Spill
	andq	-64(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rcx, %rdi
	movq	%rdi, %r14
	movq	-8(%rsp), %rcx                  # 8-byte Reload
	rolq	$56, %rcx
	movq	%rcx, -40(%rsp)                 # 8-byte Spill
	movq	%r8, -16(%rsp)                  # 8-byte Spill
	movq	-72(%rsp), %rcx                 # 8-byte Reload
	rolq	$56, %rcx
	movq	%rcx, 80(%rsp)                  # 8-byte Spill
	movq	-80(%rsp), %r15                 # 8-byte Reload
	movq	%r15, %rbp
	rolq	$56, %rbp
	movq	%r11, 72(%rsp)                  # 8-byte Spill
	movq	%r11, %rsi
	movq	32(%rsp), %rdi                  # 8-byte Reload
	andq	%rdi, %rsi
	movq	-128(%rsp), %rdx                # 8-byte Reload
	movq	%rdx, %r13
	rolq	$56, %r13
	movq	-112(%rsp), %r10                # 8-byte Reload
	movq	%r10, %r9
	rolq	$56, %r9
	xorq	%rdi, %r11
	movq	-120(%rsp), %r12                # 8-byte Reload
	movq	%r12, %rdi
	rolq	$56, %rdi
	andq	%rbx, %r11
	xorq	%rsi, %r11
	rorq	%r8
	xorq	%r8, -104(%rsp)                 # 8-byte Folded Spill
	movq	-8(%rsp), %rcx                  # 8-byte Reload
	rorq	%rcx
	movq	-40(%rsp), %r8                  # 8-byte Reload
	xorq	%rcx, %r8
	movq	-72(%rsp), %rcx                 # 8-byte Reload
	rorq	%rcx
	xorq	%rcx, 80(%rsp)                  # 8-byte Folded Spill
	movq	%r15, %rcx
	rorq	%rcx
	xorq	%rcx, %rbp
	movq	%rdx, %rcx
	rorq	%rcx
	xorq	%rcx, %r13
	movq	%r10, %rcx
	rorq	%rcx
	xorq	%rcx, %r9
	movq	%r12, %rcx
	rorq	%rcx
	xorq	%rcx, %rdi
	movq	%rbx, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rcx
	rolq	$36, %rcx
	movq	%rbx, %r10
	rolq	$30, %r10
	xorq	%rcx, %r10
	movq	-88(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rsi
	rolq	$23, %rsi
	xorq	%rax, %rsi
	movq	%rbx, %rcx
	rolq	$25, %rcx
	xorq	%r10, %rcx
	movq	-48(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %r10
	rorq	%r10
	movq	%rbx, %rdx
	movq	%rbx, %r15
	rolq	$56, %rdx
	xorq	%r10, %rdx
	addq	%rsi, %r14
	movq	%r14, 176(%rsp)                 # 8-byte Spill
	movq	-96(%rsp), %rax                 # 8-byte Reload
	movq	%rax, %r10
	rorq	%r10
	movq	%rax, %rsi
	rolq	$56, %rsi
	xorq	%r10, %rsi
	addq	%rcx, %r11
	movq	16(%rsp), %rbx                  # 8-byte Reload
	movq	%rbx, %rcx
	rorq	%rcx
	movq	%rbx, %r10
	rolq	$56, %r10
	xorq	%rcx, %r10
	movq	%rbx, %rcx
	shrq	$7, %rcx
	xorq	%r10, %rcx
	movq	40(%rsp), %r14                  # 8-byte Reload
	addq	%rax, %r14
	addq	%rax, %rcx
	movq	%rcx, 184(%rsp)                 # 8-byte Spill
	shrq	$7, %rax
	xorq	%rsi, %rax
	addq	%r15, %rax
	movq	%rax, -40(%rsp)                 # 8-byte Spill
	shrq	$7, %r15
	xorq	%rdx, %r15
	movq	-120(%rsp), %rax                # 8-byte Reload
	addq	%rax, %r15
	movq	%r15, -96(%rsp)                 # 8-byte Spill
	movq	%rax, %rcx
	shrq	$7, %rcx
	xorq	%rdi, %rcx
	movq	-112(%rsp), %rax                # 8-byte Reload
	addq	%rax, %rcx
	movq	%rcx, -120(%rsp)                # 8-byte Spill
	movq	%rax, %rcx
	shrq	$7, %rcx
	xorq	%r9, %rcx
	movq	-128(%rsp), %rax                # 8-byte Reload
	addq	%rax, %rcx
	movq	%rcx, -48(%rsp)                 # 8-byte Spill
	shrq	$7, %rax
	xorq	%r13, %rax
	movq	-80(%rsp), %r10                 # 8-byte Reload
	addq	%r10, %rax
	movq	%rax, -112(%rsp)                # 8-byte Spill
	shrq	$7, %r10
	xorq	%rbp, %r10
	movq	-72(%rsp), %rsi                 # 8-byte Reload
	addq	%rsi, %r10
	movq	%r10, -80(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	shrq	$7, %rax
	xorq	80(%rsp), %rax                  # 8-byte Folded Reload
	movq	-8(%rsp), %rsi                  # 8-byte Reload
	addq	%rsi, %rax
	movq	%rax, -128(%rsp)                # 8-byte Spill
	shrq	$7, %rsi
	xorq	%r8, %rsi
	movq	-16(%rsp), %rcx                 # 8-byte Reload
	addq	%rcx, %rsi
	movq	%rsi, %r10
	movq	-24(%rsp), %rax                 # 8-byte Reload
	movq	%rax, %rdi
	rolq	$45, %rdi
	shrq	$7, %rcx
	movq	%rax, %r8
	rolq	$3, %r8
	xorq	-104(%rsp), %rcx                # 8-byte Folded Reload
	xorq	%rdi, %r8
	movq	%rax, %rdi
	shrq	$6, %rdi
	xorq	%r8, %rdi
	addq	%rcx, %r14
	movq	88(%rsp), %rsi                  # 8-byte Reload
	movq	176(%rsp), %rax                 # 8-byte Reload
	addq	%rax, %rsi
	addq	%rax, %r11
	addq	%rdi, %r14
	movq	%rsi, %rcx
	rolq	$50, %rcx
	movq	%rsi, %rdi
	rolq	$46, %rdi
	xorq	%rcx, %rdi
	movq	%rsi, %r8
	rolq	$23, %r8
	xorq	%rdi, %r8
	movq	-56(%rsp), %rax                 # 8-byte Reload
	leaq	K512(%rip), %rcx
	movq	176(%rcx,%rax,8), %rdi
	addq	%r14, %rdi
	addq	-64(%rsp), %rdi                 # 8-byte Folded Reload
	movq	%rsi, %rcx
	movq	%r12, %r13
	andq	%r12, %rcx
	addq	%rcx, %rdi
	movq	%rsi, %rcx
	notq	%rcx
	movq	%r11, %r9
	rolq	$36, %r9
	movq	-32(%rsp), %rbp                 # 8-byte Reload
	andq	%rbp, %rcx
	addq	%rcx, %rdi
	movq	%r11, %rcx
	rolq	$30, %rcx
	xorq	%r9, %rcx
	movq	%r11, %r9
	rolq	$25, %r9
	xorq	%rcx, %r9
	movq	24(%rsp), %rdx                  # 8-byte Reload
	movq	%rdx, %r15
	movq	72(%rsp), %rbx                  # 8-byte Reload
	andq	%rbx, %r15
	movq	%rdx, %rcx
	xorq	%rbx, %rcx
	andq	%r11, %rcx
	xorq	%r15, %rcx
	addq	%r8, %rdi
	addq	%r9, %rcx
	movq	(%rsp), %r15                    # 8-byte Reload
	movq	%r15, %r8
	rolq	$45, %r8
	movq	%r15, %r9
	rolq	$3, %r9
	movq	32(%rsp), %rbx                  # 8-byte Reload
	addq	%rdi, %rbx
	addq	%rdi, %rcx
	xorq	%r8, %r9
	movq	%r15, %rdi
	shrq	$6, %rdi
	xorq	%r9, %rdi
	addq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rdi, %r10
	movq	%r10, -72(%rsp)                 # 8-byte Spill
	leaq	K512(%rip), %rdi
	movq	184(%rdi,%rax,8), %rdi
	addq	%r10, %rdi
	addq	%rbp, %rdi
	movq	%rbx, %r8
	andq	%rsi, %r8
	addq	%r8, %rdi
	movq	%rbx, %r8
	rolq	$50, %r8
	movq	%rbx, %r9
	rolq	$46, %r9
	xorq	%r8, %r9
	movq	%rbx, %r8
	notq	%r8
	andq	%r12, %r8
	addq	%r8, %rdi
	movq	%rbx, %r8
	movq	%rbx, 32(%rsp)                  # 8-byte Spill
	rolq	$23, %r8
	xorq	%r9, %r8
	movq	%rcx, %r9
	rolq	$36, %r9
	movq	%rcx, %r15
	rolq	$30, %r15
	xorq	%r9, %r15
	movq	%rcx, %r9
	rolq	$25, %r9
	xorq	%r15, %r9
	addq	%r8, %rdi
	movq	%r11, %r8
	movq	%rdx, %rax
	andq	%rdx, %r8
	movq	%r11, %r15
	xorq	%rdx, %r15
	movq	%r14, %r12
	rolq	$45, %r12
	andq	%rcx, %r15
	xorq	%r8, %r15
	movq	%r14, %r8
	movq	%r14, 40(%rsp)                  # 8-byte Spill
	rolq	$3, %r8
	addq	%r9, %r15
	xorq	%r12, %r8
	movq	72(%rsp), %rax                  # 8-byte Reload
	addq	%rdi, %rax
	addq	%rdi, %r15
	shrq	$6, %r14
	xorq	%r8, %r14
	movq	-128(%rsp), %rdx                # 8-byte Reload
	addq	56(%rsp), %rdx                  # 8-byte Folded Reload
	addq	%r14, %rdx
	movq	%rdx, -128(%rsp)                # 8-byte Spill
	movq	%rax, %rdi
	rolq	$50, %rdi
	movq	%rax, %r8
	rolq	$46, %r8
	xorq	%rdi, %r8
	movq	%rax, %rdi
	rolq	$23, %rdi
	xorq	%r8, %rdi
	leaq	K512(%rip), %rbp
	movq	-56(%rsp), %r10                 # 8-byte Reload
	movq	192(%rbp,%r10,8), %r8
	addq	%rdx, %r8
	addq	%r13, %r8
	movq	%rax, %r9
	andq	%rbx, %r9
	addq	%r9, %r8
	movq	%rax, %r9
	movq	%rax, %r13
	notq	%r9
	movq	%r15, %rbx
	rolq	$36, %rbx
	andq	%rsi, %r9
	addq	%r9, %r8
	movq	%r15, %r9
	rolq	$30, %r9
	xorq	%rbx, %r9
	movq	%r15, %r12
	rolq	$25, %r12
	xorq	%r9, %r12
	movq	%rcx, %r9
	andq	%r11, %r9
	movq	%rcx, %rdx
	xorq	%r11, %rdx
	andq	%r15, %rdx
	xorq	%r9, %rdx
	addq	%rdi, %r8
	addq	%r12, %rdx
	movq	-72(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %rdi
	rolq	$45, %rdi
	movq	%rbx, %r9
	rolq	$3, %r9
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	%r8, %rax
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	addq	%r8, %rdx
	xorq	%rdi, %r9
	movq	%rbx, %rdi
	shrq	$6, %rdi
	xorq	%r9, %rdi
	movq	-80(%rsp), %r14                 # 8-byte Reload
	addq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rdi, %r14
	movq	%r10, %rbx
	movq	200(%rbp,%r10,8), %rdi
	addq	%r14, %rdi
	movq	%r14, %r10
	addq	%rsi, %rdi
	movq	%rax, %r8
	andq	%r13, %r8
	addq	%r8, %rdi
	movq	%rax, %r8
	rolq	$50, %r8
	movq	%rax, %r9
	rolq	$46, %r9
	xorq	%r8, %r9
	movq	%rax, %r8
	notq	%r8
	movq	32(%rsp), %r14                  # 8-byte Reload
	andq	%r14, %r8
	addq	%r8, %rdi
	movq	%rax, %r8
	rolq	$23, %r8
	xorq	%r9, %r8
	movq	%rdx, %r9
	rolq	$36, %r9
	movq	%rdx, %r12
	rolq	$30, %r12
	xorq	%r9, %r12
	movq	%rdx, %r9
	rolq	$25, %r9
	xorq	%r12, %r9
	addq	%r8, %rdi
	movq	%r15, %r8
	andq	%rcx, %r8
	movq	%r15, %rbp
	xorq	%rcx, %rbp
	movq	-128(%rsp), %rsi                # 8-byte Reload
	movq	%rsi, %rax
	rolq	$45, %rax
	andq	%rdx, %rbp
	xorq	%r8, %rbp
	movq	%rsi, %r8
	rolq	$3, %r8
	addq	%r9, %rbp
	xorq	%rax, %r8
	addq	%rdi, %r11
	addq	%rdi, %rbp
	movq	%rsi, %rax
	shrq	$6, %rax
	xorq	%r8, %rax
	movq	-112(%rsp), %rsi                # 8-byte Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	addq	%rax, %rsi
	movq	%rsi, -112(%rsp)                # 8-byte Spill
	movq	%r11, %rax
	rolq	$50, %rax
	movq	%r11, %rdi
	rolq	$46, %rdi
	xorq	%rax, %rdi
	movq	%r11, %rax
	rolq	$23, %rax
	xorq	%rdi, %rax
	movq	%rbx, %r12
	leaq	K512(%rip), %rdi
	movq	208(%rdi,%rbx,8), %r8
	addq	%rsi, %r8
	addq	%r14, %r8
	movq	%r11, %rdi
	movq	24(%rsp), %rsi                  # 8-byte Reload
	andq	%rsi, %rdi
	addq	%rdi, %r8
	movq	%r11, %rdi
	notq	%rdi
	movq	%rbp, %r9
	rolq	$36, %r9
	andq	%r13, %rdi
	addq	%rdi, %r8
	movq	%rbp, %rdi
	rolq	$30, %rdi
	xorq	%r9, %rdi
	movq	%rbp, %r9
	rolq	$25, %r9
	xorq	%rdi, %r9
	movq	%rdx, %rbx
	andq	%r15, %rbx
	movq	%rdx, %r14
	xorq	%r15, %r14
	andq	%rbp, %r14
	xorq	%rbx, %r14
	addq	%rax, %r8
	addq	%r9, %r14
	movq	%r10, %rdi
	movq	%r10, -80(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	rolq	$45, %rax
	movq	%r10, %r9
	rolq	$3, %r9
	addq	%r8, %rcx
	addq	%r8, %r14
	xorq	%rax, %r9
	movq	%r10, %rax
	shrq	$6, %rax
	xorq	%r9, %rax
	movq	-48(%rsp), %rdi                 # 8-byte Reload
	addq	-24(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%rax, %rdi
	leaq	K512(%rip), %rax
	movq	216(%rax,%r12,8), %rax
	addq	%rdi, %rax
	movq	%rdi, %r10
	addq	%r13, %rax
	movq	%rcx, %r8
	andq	%r11, %r8
	addq	%r8, %rax
	movq	%rcx, %r8
	rolq	$50, %r8
	movq	%rcx, %r9
	rolq	$46, %r9
	xorq	%r8, %r9
	movq	%rcx, %r8
	notq	%r8
	andq	%rsi, %r8
	addq	%r8, %rax
	movq	%rcx, %r8
	rolq	$23, %r8
	xorq	%r9, %r8
	movq	%r14, %r9
	rolq	$36, %r9
	movq	%r14, %rbx
	rolq	$30, %rbx
	xorq	%r9, %rbx
	movq	%r14, %rdi
	rolq	$25, %rdi
	xorq	%rbx, %rdi
	addq	%r8, %rax
	movq	%rbp, %r8
	andq	%rdx, %r8
	movq	%rbp, %r12
	xorq	%rdx, %r12
	movq	-112(%rsp), %r9                 # 8-byte Reload
	movq	%r9, %rbx
	rolq	$45, %rbx
	andq	%r14, %r12
	xorq	%r8, %r12
	movq	%r9, %r8
	rolq	$3, %r8
	addq	%rdi, %r12
	xorq	%rbx, %r8
	addq	%rax, %r15
	addq	%rax, %r12
	movq	%r9, %rax
	shrq	$6, %rax
	xorq	%r8, %rax
	movq	-120(%rsp), %r8                 # 8-byte Reload
	addq	(%rsp), %r8                     # 8-byte Folded Reload
	addq	%rax, %r8
	movq	%r8, -120(%rsp)                 # 8-byte Spill
	movq	%r15, %rax
	rolq	$50, %rax
	movq	%r15, %rdi
	rolq	$46, %rdi
	xorq	%rax, %rdi
	movq	%r15, %rax
	rolq	$23, %rax
	xorq	%rdi, %rax
	movq	-56(%rsp), %r9                  # 8-byte Reload
	leaq	K512(%rip), %rdi
	movq	224(%rdi,%r9,8), %rdi
	addq	%r8, %rdi
	addq	%rsi, %rdi
	movq	%r15, %r8
	andq	%rcx, %r8
	addq	%r8, %rdi
	movq	%r15, %r8
	notq	%r8
	movq	%r12, %rbx
	rolq	$36, %rbx
	andq	%r11, %r8
	addq	%r8, %rdi
	movq	%r12, %r8
	rolq	$30, %r8
	xorq	%rbx, %r8
	movq	%r12, %rbx
	rolq	$25, %rbx
	xorq	%r8, %rbx
	movq	%r14, %r13
	andq	%rbp, %r13
	movq	%r14, %r8
	xorq	%rbp, %r8
	andq	%r12, %r8
	xorq	%r13, %r8
	addq	%rax, %rdi
	addq	%rbx, %r8
	movq	%r10, %r13
	movq	%r10, -48(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	rolq	$45, %rax
	movq	%r10, %rbx
	rolq	$3, %rbx
	addq	%rdi, %rdx
	addq	%rdi, %r8
	xorq	%rax, %rbx
	movq	%r10, %rax
	shrq	$6, %rax
	xorq	%rbx, %rax
	movq	-96(%rsp), %rsi                 # 8-byte Reload
	addq	40(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%rax, %rsi
	movq	%rsi, -96(%rsp)                 # 8-byte Spill
	leaq	K512(%rip), %rax
	movq	232(%rax,%r9,8), %rax
	addq	%rsi, %rax
	addq	%r11, %rax
	movq	%rdx, %rdi
	andq	%r15, %rdi
	addq	%rdi, %rax
	movq	%rdx, %rdi
	rolq	$50, %rdi
	movq	%rdx, %r11
	rolq	$46, %r11
	xorq	%rdi, %r11
	movq	%rdx, %rdi
	notq	%rdi
	andq	%rcx, %rdi
	addq	%rdi, %rax
	movq	%rdx, %rdi
	rolq	$23, %rdi
	xorq	%r11, %rdi
	movq	%r8, %r11
	rolq	$36, %r11
	movq	%r8, %rbx
	rolq	$30, %rbx
	xorq	%r11, %rbx
	movq	%r8, %r13
	rolq	$25, %r13
	xorq	%rbx, %r13
	addq	%rdi, %rax
	movq	%r12, %rdi
	andq	%r14, %rdi
	movq	%r12, %r9
	xorq	%r14, %r9
	movq	-120(%rsp), %rsi                # 8-byte Reload
	movq	%rsi, %rbx
	rolq	$45, %rbx
	andq	%r8, %r9
	xorq	%rdi, %r9
	movq	%rsi, %rdi
	rolq	$3, %rdi
	addq	%r13, %r9
	leaq	K512(%rip), %r13
	xorq	%rbx, %rdi
	addq	%rax, %rbp
	addq	%rax, %r9
	movq	%rsi, %rax
	shrq	$6, %rax
	xorq	%rdi, %rax
	movq	-40(%rsp), %r11                 # 8-byte Reload
	movq	-72(%rsp), %r10                 # 8-byte Reload
	addq	%r10, %r11
	addq	%rax, %r11
	movq	%r11, -40(%rsp)                 # 8-byte Spill
	movq	%rbp, %rax
	rolq	$50, %rax
	movq	%rbp, %rdi
	rolq	$46, %rdi
	xorq	%rax, %rdi
	movq	%rbp, %rax
	rolq	$23, %rax
	xorq	%rdi, %rax
	movq	-56(%rsp), %rsi                 # 8-byte Reload
	movq	240(%r13,%rsi,8), %rdi
	addq	%r11, %rdi
	addq	%rcx, %rdi
	movq	%rbp, %rcx
	andq	%rdx, %rcx
	addq	%rcx, %rdi
	movq	%rbp, %rcx
	notq	%rcx
	movq	%r9, %rbx
	rolq	$36, %rbx
	andq	%r15, %rcx
	addq	%rcx, %rdi
	movq	%r9, %rcx
	rolq	$30, %rcx
	xorq	%rbx, %rcx
	movq	%r9, %r11
	rolq	$25, %r11
	xorq	%rcx, %r11
	movq	%r8, %rcx
	andq	%r12, %rcx
	movq	%r8, %rbx
	xorq	%r12, %rbx
	andq	%r9, %rbx
	xorq	%rcx, %rbx
	addq	%rax, %rdi
	addq	%r11, %rbx
	movq	-96(%rsp), %r11                 # 8-byte Reload
	movq	%r11, %rax
	rolq	$45, %rax
	movq	%r11, %rcx
	rolq	$3, %rcx
	addq	%rdi, %r14
	addq	%rdi, %rbx
	xorq	%rax, %rcx
	movq	%r11, %rax
	shrq	$6, %rax
	xorq	%rcx, %rax
	movq	184(%rsp), %r13                 # 8-byte Reload
	addq	-128(%rsp), %r13                # 8-byte Folded Reload
	addq	%rax, %r13
	leaq	K512(%rip), %rax
	movq	248(%rax,%rsi,8), %rax
	addq	%r13, %rax
	addq	%r15, %rax
	movq	%r14, %rcx
	andq	%rbp, %rcx
	addq	%rcx, %rax
	movq	%r14, %rcx
	rolq	$50, %rcx
	movq	%r14, %rdi
	rolq	$46, %rdi
	xorq	%rcx, %rdi
	movq	%r14, %rcx
	notq	%rcx
	movq	%rdx, -88(%rsp)                 # 8-byte Spill
	andq	%rdx, %rcx
	addq	%rcx, %rax
	movq	%r14, %rcx
	rolq	$23, %rcx
	xorq	%rdi, %rcx
	movq	%rbx, %rdi
	rolq	$36, %rdi
	movq	%rbx, %r11
	rolq	$30, %r11
	xorq	%rdi, %r11
	movq	%rbx, %rdi
	rolq	$25, %rdi
	xorq	%r11, %rdi
	addq	%rcx, %rax
	movq	%r9, %rcx
	andq	%r8, %rcx
	movq	%r9, -104(%rsp)                 # 8-byte Spill
	movq	%r9, %r15
	movq	%r12, %r9
	xorq	%r8, %r15
	movq	%rbx, -64(%rsp)                 # 8-byte Spill
	andq	%rbx, %r15
	movq	%r15, %rbx
	xorq	%rcx, %rbx
	addq	%rdi, %rbx
	addq	%rax, %r9
	addq	%rax, %rbx
	movq	%r10, -16(%rsp)                 # 8-byte Spill
	movq	-128(%rsp), %rax                # 8-byte Reload
	movq	%rax, -8(%rsp)                  # 8-byte Spill
	movq	-80(%rsp), %rax                 # 8-byte Reload
	movq	%rax, -72(%rsp)                 # 8-byte Spill
	movq	-112(%rsp), %rax                # 8-byte Reload
	movq	%rax, -80(%rsp)                 # 8-byte Spill
	movq	-48(%rsp), %rax                 # 8-byte Reload
	movq	%rax, -128(%rsp)                # 8-byte Spill
	movq	-120(%rsp), %rax                # 8-byte Reload
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movq	-96(%rsp), %rax                 # 8-byte Reload
	movq	%rax, -120(%rsp)                # 8-byte Spill
	addq	$16, %rsi
	movq	-40(%rsp), %r15                 # 8-byte Reload
	movq	%r13, -96(%rsp)                 # 8-byte Spill
	movq	%rsi, %rdi
	cmpq	$64, %rsi
	jb	.LBB4_3
# %bb.4:                                #   in Loop: Header=BB4_2 Depth=1
	movq	152(%rsp), %r15                 # 8-byte Reload
	addq	%rbx, %r15
	movq	144(%rsp), %rbx                 # 8-byte Reload
	addq	-64(%rsp), %rbx                 # 8-byte Folded Reload
	movq	136(%rsp), %r10                 # 8-byte Reload
	addq	-104(%rsp), %r10                # 8-byte Folded Reload
	movq	128(%rsp), %rdi                 # 8-byte Reload
	addq	%r8, %rdi
	movq	120(%rsp), %r13                 # 8-byte Reload
	addq	%r9, %r13
	movq	112(%rsp), %rax                 # 8-byte Reload
	addq	%r14, %rax
	movq	%rax, %r14
	movq	104(%rsp), %r8                  # 8-byte Reload
	addq	%rbp, %r8
	movq	96(%rsp), %r9                   # 8-byte Reload
	addq	-88(%rsp), %r9                  # 8-byte Folded Reload
	movq	160(%rsp), %rsi                 # 8-byte Reload
	subq	$-128, %rsi
	movq	64(%rsp), %rax                  # 8-byte Reload
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%r10, 16(%rax)
	movq	%rdi, 24(%rax)
	movq	%r13, 32(%rax)
	movq	%r14, 40(%rax)
	movq	%r8, 48(%rax)
	movq	%r9, 56(%rax)
	movq	168(%rsp), %rdx                 # 8-byte Reload
	decq	%rdx
	jne	.LBB4_2
.LBB4_5:
	addq	$192, %rsp
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
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$64, %rsp
	movq	$1296236545, 16(%rsp)           # imm = 0x4D430001
	leaq	message(%rip), %rbx
	movq	%rbx, 24(%rsp)
	movq	$256, 32(%rsp)                  # imm = 0x100
	movq	$0, 40(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 56(%rsp)
	xorl	%r15d, %r15d
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
	leaq	out(%rip), %r14
	movq	%r14, 24(%rsp)
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
	movl	%r15d, %eax
	andl	$15, %eax
	movl	$256, %esi                      # imm = 0x100
	subl	%eax, %esi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	SHA512
	incl	%r15d
	cmpl	$100, %r15d
	jne	.LBB7_1
# %bb.2:
	xorl	%eax, %eax
	addq	$64, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
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
