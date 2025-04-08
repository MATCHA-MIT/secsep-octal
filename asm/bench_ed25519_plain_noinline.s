	.text
	.file	"bench_ed25519_plain_noinline.c"
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
.Lfunc_end0:
	.size	SHA512_Init, .Lfunc_end0-SHA512_Init
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
	jb	.LBB1_4
# %bb.1:
	cmpl	$127, %eax
	je	.LBB1_3
# %bb.2:
	movl	$127, %edx
	subq	%rax, %rdx
	addq	%r15, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB1_3:
	movl	$1, %edx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	xorl	%edi, %edi
	jmp	.LBB1_5
.LBB1_4:
	cmpq	$112, %rdi
	je	.LBB1_6
.LBB1_5:
	movl	$112, %edx
	subq	%rdi, %rdx
	addq	%r15, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB1_6:
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
	je	.LBB1_7
# %bb.8:
	movl	$1, %eax
	cmpl	$8, %r12d
	jb	.LBB1_16
# %bb.9:
	shrq	$3, %r12
	leaq	-1(%r12), %rdx
	movl	%r12d, %ecx
	andl	$3, %ecx
	cmpq	$3, %rdx
	jae	.LBB1_11
# %bb.10:
	xorl	%edx, %edx
	jmp	.LBB1_13
.LBB1_7:
	xorl	%eax, %eax
	jmp	.LBB1_16
.LBB1_11:
	andl	$-4, %r12d
	xorl	%edx, %edx
	.p2align	4, 0x90
.LBB1_12:                               # =>This Inner Loop Header: Depth=1
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
	jne	.LBB1_12
.LBB1_13:
	testq	%rcx, %rcx
	je	.LBB1_16
# %bb.14:
	leaq	(%r14,%rdx,8), %rdx
	xorl	%esi, %esi
	.p2align	4, 0x90
.LBB1_15:                               # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rsi,8), %rdi
	bswapq	%rdi
	movq	%rdi, (%rbx,%rsi,8)
	incq	%rsi
	cmpq	%rsi, %rcx
	jne	.LBB1_15
.LBB1_16:
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Lfunc_end1:
	.size	SHA512_Final, .Lfunc_end1-SHA512_Final
                                        # -- End function
	.globl	OPENSSL_cleanse                 # -- Begin function OPENSSL_cleanse
	.p2align	4, 0x90
	.type	OPENSSL_cleanse,@function
OPENSSL_cleanse:                        # @OPENSSL_cleanse
# %bb.0:
	pushq	%rbx
	movq	%rdi, %rbx
	testq	%rsi, %rsi
	je	.LBB2_2
# %bb.1:
	movq	%rsi, %rdx
	movq	%rbx, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
.LBB2_2:
	#APP
	#NO_APP
	popq	%rbx
	retq
.Lfunc_end2:
	.size	OPENSSL_cleanse, .Lfunc_end2-OPENSSL_cleanse
                                        # -- End function
	.globl	x25519_ge_p1p1_to_p2            # -- Begin function x25519_ge_p1p1_to_p2
	.p2align	4, 0x90
	.type	x25519_ge_p1p1_to_p2,@function
x25519_ge_p1p1_to_p2:                   # @x25519_ge_p1p1_to_p2
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	leaq	120(%rsi), %r15
	movq	%r15, %rdx
	callq	fe_mul_impl
	leaq	40(%r14), %rdi
	leaq	40(%rbx), %rsi
	addq	$80, %rbx
	movq	%rbx, %rdx
	callq	fe_mul_impl
	addq	$80, %r14
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	fe_mul_impl
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Lfunc_end3:
	.size	x25519_ge_p1p1_to_p2, .Lfunc_end3-x25519_ge_p1p1_to_p2
                                        # -- End function
	.globl	x25519_ge_p1p1_to_p3            # -- Begin function x25519_ge_p1p1_to_p3
	.p2align	4, 0x90
	.type	x25519_ge_p1p1_to_p3,@function
x25519_ge_p1p1_to_p3:                   # @x25519_ge_p1p1_to_p3
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	leaq	120(%rsi), %r15
	movq	%r15, %rdx
	callq	fe_mul_impl
	leaq	40(%r14), %rdi
	leaq	40(%rbx), %r12
	leaq	80(%rbx), %r13
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	fe_mul_impl
	leaq	80(%r14), %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	fe_mul_impl
	addq	$120, %r14
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	fe_mul_impl
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Lfunc_end4:
	.size	x25519_ge_p1p1_to_p3, .Lfunc_end4-x25519_ge_p1p1_to_p3
                                        # -- End function
	.globl	CRYPTO_memcmp                   # -- Begin function CRYPTO_memcmp
	.p2align	4, 0x90
	.type	CRYPTO_memcmp,@function
CRYPTO_memcmp:                          # @CRYPTO_memcmp
# %bb.0:
	testq	%rdx, %rdx
	je	.LBB5_1
# %bb.2:
	cmpq	$8, %rdx
	jae	.LBB5_5
# %bb.3:
	xorl	%eax, %eax
	xorl	%ecx, %ecx
	jmp	.LBB5_4
.LBB5_1:
	xorl	%eax, %eax
	retq
.LBB5_5:
	cmpq	$32, %rdx
	jae	.LBB5_7
# %bb.6:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	jmp	.LBB5_11
.LBB5_7:
	movq	%rdx, %rax
	andq	$-32, %rax
	pxor	%xmm0, %xmm0
	xorl	%ecx, %ecx
	pxor	%xmm1, %xmm1
	.p2align	4, 0x90
.LBB5_8:                                # =>This Inner Loop Header: Depth=1
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
	jne	.LBB5_8
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
	je	.LBB5_14
# %bb.10:
	testb	$24, %dl
	je	.LBB5_4
.LBB5_11:
	movq	%rax, %r8
	movq	%rdx, %rax
	andq	$-8, %rax
	movzbl	%cl, %ecx
	movd	%ecx, %xmm0
	.p2align	4, 0x90
.LBB5_12:                               # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%r8), %rcx
	xorq	(%rdi,%r8), %rcx
	movq	%rcx, %xmm1
	por	%xmm1, %xmm0
	addq	$8, %r8
	cmpq	%r8, %rax
	jne	.LBB5_12
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
	je	.LBB5_14
	.p2align	4, 0x90
.LBB5_4:                                # =>This Inner Loop Header: Depth=1
	movzbl	(%rsi,%rax), %r8d
	xorb	(%rdi,%rax), %r8b
	orb	%r8b, %cl
	incq	%rax
	cmpq	%rax, %rdx
	jne	.LBB5_4
.LBB5_14:
	movzbl	%cl, %eax
	retq
.Lfunc_end5:
	.size	CRYPTO_memcmp, .Lfunc_end5-CRYPTO_memcmp
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
	je	.LBB6_12
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
	je	.LBB6_7
# %bb.2:
	movl	$128, %r13d
	subq	%rdi, %r13
	cmpq	%rbx, %r13
	jbe	.LBB6_4
# %bb.3:
	addq	%rdi, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy@PLT
	addl	208(%r14), %ebx
	jmp	.LBB6_11
.LBB6_4:
	cmpl	$128, %edi
	je	.LBB6_6
# %bb.5:
	addq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	memcpy@PLT
.LBB6_6:
	movl	$0, 208(%r14)
	subq	%r13, %rbx
	addq	%r13, %r15
	movl	$1, %edx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	sha512_block_data_order
.LBB6_7:
	cmpq	$128, %rbx
	jb	.LBB6_9
# %bb.8:
	movq	%rbx, %rdx
	shrq	$7, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	sha512_block_data_order
	addq	%rbx, %r15
	andl	$127, %ebx
	subq	%rbx, %r15
.LBB6_9:
	testq	%rbx, %rbx
	je	.LBB6_12
# %bb.10:
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy@PLT
.LBB6_11:
	movl	%ebx, 208(%r14)
.LBB6_12:
	movl	$1, %eax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Lfunc_end6:
	.size	SHA512_Update, .Lfunc_end6-SHA512_Update
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
	je	.LBB7_5
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
.LBB7_2:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_3 Depth 2
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
.LBB7_3:                                #   Parent Loop BB7_2 Depth=1
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
	jb	.LBB7_3
# %bb.4:                                #   in Loop: Header=BB7_2 Depth=1
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
	jne	.LBB7_2
.LBB7_5:
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end7:
	.size	sha512_block_data_order, .Lfunc_end7-sha512_block_data_order
                                        # -- End function
	.globl	SHA512                          # -- Begin function SHA512
	.p2align	4, 0x90
	.type	SHA512,@function
SHA512:                                 # @SHA512
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$216, %rsp
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%rsp, %r12
	movq	%r12, %rdi
	callq	SHA512_Init
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	SHA512_Update
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	SHA512_Final
	xorps	%xmm0, %xmm0
	movaps	%xmm0, (%rsp)
	movaps	%xmm0, 16(%rsp)
	movaps	%xmm0, 32(%rsp)
	movaps	%xmm0, 48(%rsp)
	movaps	%xmm0, 64(%rsp)
	movaps	%xmm0, 80(%rsp)
	movaps	%xmm0, 96(%rsp)
	movaps	%xmm0, 112(%rsp)
	movaps	%xmm0, 128(%rsp)
	movaps	%xmm0, 144(%rsp)
	movaps	%xmm0, 160(%rsp)
	movaps	%xmm0, 176(%rsp)
	movaps	%xmm0, 192(%rsp)
	movq	$0, 208(%rsp)
	#APP
	#NO_APP
	movq	%rbx, %rax
	addq	$216, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Lfunc_end8:
	.size	SHA512, .Lfunc_end8-SHA512
                                        # -- End function
	.globl	x25519_sc_reduce                # -- Begin function x25519_sc_reduce
	.p2align	4, 0x90
	.type	x25519_sc_reduce,@function
x25519_sc_reduce:                       # @x25519_sc_reduce
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	movq	%rdi, %r13
	movzwl	(%rdi), %eax
	movzbl	2(%rdi), %ecx
	andl	$31, %ecx
	shlq	$16, %rcx
	orq	%rax, %rcx
	movq	%rcx, -88(%rsp)                 # 8-byte Spill
	movzwl	5(%rdi), %eax
	movzbl	7(%rdi), %ecx
	shll	$16, %ecx
	orl	%eax, %ecx
	movq	%rcx, -96(%rsp)                 # 8-byte Spill
	movzwl	13(%rdi), %eax
	movzbl	15(%rdi), %ecx
	shll	$16, %ecx
	orl	%eax, %ecx
	movq	%rcx, -72(%rsp)                 # 8-byte Spill
	movzwl	18(%rdi), %eax
	movzbl	20(%rdi), %r8d
	shlq	$16, %r8
	orq	%rax, %r8
	movzwl	21(%rdi), %eax
	movzbl	23(%rdi), %r9d
	andl	$31, %r9d
	shlq	$16, %r9
	orq	%rax, %r9
	movzwl	26(%rdi), %eax
	movzbl	28(%rdi), %r10d
	shll	$16, %r10d
	orl	%eax, %r10d
	movzwl	34(%rdi), %ecx
	movzbl	36(%rdi), %eax
	shll	$16, %eax
	orl	%ecx, %eax
	shrl	%eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movl	36(%rdi), %ecx
	shrl	$6, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	movzwl	39(%rdi), %edx
	movzbl	41(%rdi), %r11d
	shlq	$16, %r11
	orq	%rdx, %r11
	shrq	$3, %r11
	movzwl	42(%rdi), %edx
	movzbl	44(%rdi), %esi
	andl	$31, %esi
	shlq	$16, %rsi
	orq	%rdx, %rsi
	movq	%rsi, -16(%rsp)                 # 8-byte Spill
	movzwl	47(%rdi), %edx
	movzbl	49(%rdi), %r14d
	shll	$16, %r14d
	orl	%edx, %r14d
	movzwl	55(%rdi), %edx
	movzbl	57(%rdi), %ebx
	shll	$16, %ebx
	orl	%edx, %ebx
	movl	60(%rdi), %r12d
	shrq	$3, %r12
	imulq	$654183, %r12, %rsi             # imm = 0x9FB67
	addq	%rax, %rsi
	imulq	$-997805, %r12, %rdx            # imm = 0xFFF0C653
	addq	%rcx, %rdx
	imulq	$136657, %r12, %rdi             # imm = 0x215D1
	addq	%r11, %rdi
	movl	57(%r13), %ecx
	shrl	$6, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	imulq	$666643, %r12, %rax             # imm = 0xA2C13
	movq	%r12, -24(%rsp)                 # 8-byte Spill
	imulq	$470296, %rcx, %r11             # imm = 0x72D18
	addq	%rax, %r11
	imulq	$136657, %rcx, %rax             # imm = 0x215D1
	addq	%rdx, %rax
	movq	%rax, -104(%rsp)                # 8-byte Spill
	imulq	$-683901, %rcx, %rax            # imm = 0xFFF59083
	addq	%rdi, %rax
	movq	%rax, -80(%rsp)                 # 8-byte Spill
	shrl	%ebx
	andl	$2097151, %ebx                  # imm = 0x1FFFFF
	imulq	$654183, %rbx, %rax             # imm = 0x9FB67
	addq	%r11, %rax
	movq	%rax, -48(%rsp)                 # 8-byte Spill
	movl	52(%r13), %edx
	shrl	$4, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	imulq	$470296, %r12, %r11             # imm = 0x72D18
	imulq	$136657, %rdx, %rdi             # imm = 0x215D1
	addq	%r11, %rdi
	imulq	$-683901, %rdx, %r12            # imm = 0xFFF59083
	addq	%rsi, %r12
	imulq	$-997805, %rcx, %rsi            # imm = 0xFFF0C653
	addq	%rsi, %r12
	shrq	$3, %r8
	movl	49(%r13), %eax
	shrl	$7, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	imulq	$666643, %rax, %r11             # imm = 0xA2C13
	addq	%r8, %r11
	imulq	$470296, %rax, %rbp             # imm = 0x72D18
	addq	%r9, %rbp
	movl	23(%r13), %r8d
	shrl	$5, %r8d
	andl	$2097151, %r8d                  # imm = 0x1FFFFF
	imulq	$654183, %rax, %rsi             # imm = 0x9FB67
	addq	%r8, %rsi
	imulq	$470296, %rdx, %r8              # imm = 0x72D18
	addq	%r8, %rsi
	shrl	$2, %r10d
	andl	$2097151, %r10d                 # imm = 0x1FFFFF
	imulq	$-997805, %rax, %r8             # imm = 0xFFF0C653
	addq	%r10, %r8
	imulq	$654183, %rdx, %r9              # imm = 0x9FB67
	addq	%r9, %r8
	movl	28(%r13), %r9d
	shrl	$7, %r9d
	andl	$2097151, %r9d                  # imm = 0x1FFFFF
	imulq	$136657, %rax, %r15             # imm = 0x215D1
	addq	%r9, %r15
	imulq	$-997805, %rdx, %r9             # imm = 0xFFF0C653
	addq	%r9, %r15
	movl	31(%r13), %r9d
	shrl	$4, %r9d
	andl	$2097151, %r9d                  # imm = 0x1FFFFF
	imulq	$-683901, %rax, %r10            # imm = 0xFFF59083
	addq	%r9, %r10
	addq	%rdi, %r10
	imulq	$654183, %rcx, %rax             # imm = 0x9FB67
	addq	%rax, %r10
	shrl	$2, %r14d
	andl	$2097151, %r14d                 # imm = 0x1FFFFF
	imulq	$470296, %r14, %rax             # imm = 0x72D18
	addq	%r11, %rax
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	imulq	$666643, %rdx, %rax             # imm = 0xA2C13
	addq	%rax, %rbp
	imulq	$-997805, %r14, %rdi            # imm = 0xFFF0C653
	addq	%rsi, %rdi
	imulq	$136657, %r14, %rax             # imm = 0x215D1
	addq	%r8, %rax
	imulq	$666643, %rcx, %rcx             # imm = 0xA2C13
	addq	%rcx, %rax
	imulq	$-683901, %r14, %r9             # imm = 0xFFF59083
	addq	%r15, %r9
	movl	15(%r13), %ecx
	movq	%r13, -8(%rsp)                  # 8-byte Spill
	shrl	$6, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	imulq	$666643, %r14, %rdx             # imm = 0xA2C13
	leaq	(%rdx,%rcx), %r8
	addq	%rdx, %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	movq	%rcx, %rsi
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %r8
	movq	%r8, -40(%rsp)                  # 8-byte Spill
	imulq	$654183, %r14, %rcx             # imm = 0x9FB67
	leaq	(%rcx,%rbp), %rdx
	leaq	(%rcx,%rbp), %r14
	addq	$1048576, %r14                  # imm = 0x100000
	imulq	$666643, %rbx, %rcx             # imm = 0xA2C13
	addq	%rcx, %rdi
	movabsq	$17592183947264, %rcx           # imm = 0xFFFFFE00000
	andq	%r14, %rcx
	subq	%rcx, %rdx
	movq	%rdx, -56(%rsp)                 # 8-byte Spill
	imulq	$470296, %rbx, %rcx             # imm = 0x72D18
	leaq	(%rax,%rcx), %rdx
	movq	%rdx, -32(%rsp)                 # 8-byte Spill
	leaq	(%rcx,%rax), %rbp
	addq	$1048576, %rbp                  # imm = 0x100000
	addq	-48(%rsp), %r9                  # 8-byte Folded Reload
	imulq	$-997805, %rbx, %rax            # imm = 0xFFF0C653
	leaq	(%r10,%rax), %r15
	addq	%r10, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	imulq	$136657, %rbx, %rcx             # imm = 0x215D1
	addq	%rcx, %r12
	movq	%rax, %rcx
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r15
	imulq	$-683901, %rbx, %rax            # imm = 0xFFF59083
	movq	-104(%rsp), %rdx                # 8-byte Reload
	leaq	(%rdx,%rax), %r8
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %rdx
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r8
	imulq	$-683901, -24(%rsp), %rax       # 8-byte Folded Reload
                                        # imm = 0xFFF59083
	movq	-16(%rsp), %r10                 # 8-byte Reload
	leaq	(%rax,%r10), %r11
	addq	%rax, %r10
	addq	$1048576, %r10                  # imm = 0x100000
	movl	44(%r13), %ebx
	shrl	$5, %ebx
	andl	$2097151, %ebx                  # imm = 0x1FFFFF
	movq	%r10, %rax
	sarq	$21, %rax
	addq	%rbx, %rax
	andq	$-2097152, %r10                 # imm = 0xFFE00000
	subq	%r10, %r11
	shrq	$21, %rsi
	movq	-64(%rsp), %r10                 # 8-byte Reload
	leaq	(%r10,%rsi), %rbx
	addq	%r10, %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%rsi, %r13
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %rbx
	shrq	$21, %r14
	leaq	(%rdi,%r14), %rsi
	movq	%rsi, -104(%rsp)                # 8-byte Spill
	addq	%r14, %rdi
	addq	$1048576, %rdi                  # imm = 0x100000
	movq	%rbp, %r10
	sarq	$21, %r10
	leaq	(%r9,%r10), %rsi
	addq	%r10, %r9
	addq	$1048576, %r9                   # imm = 0x100000
	movq	%r9, %r10
	sarq	$21, %r10
	addq	%r15, %r10
	andq	$-2097152, %r9                  # imm = 0xFFE00000
	subq	%r9, %rsi
	movq	%rsi, -16(%rsp)                 # 8-byte Spill
	sarq	$21, %rcx
	leaq	(%r12,%rcx), %r15
	addq	%r12, %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	movq	%rcx, %r12
	sarq	$21, %r12
	addq	%r8, %r12
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %r15
	sarq	$21, %rdx
	movq	-80(%rsp), %rcx                 # 8-byte Reload
	leaq	(%rcx,%rdx), %r14
	addq	%rdx, %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	movq	%rcx, %r9
	sarq	$21, %r9
	addq	%r11, %r9
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %r14
	movq	-72(%rsp), %rcx                 # 8-byte Reload
	shrl	%ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	imulq	$666643, %rax, %rsi             # imm = 0xA2C13
	addq	%rcx, %rsi
	imulq	$470296, %rax, %rdx             # imm = 0x72D18
	addq	-40(%rsp), %rdx                 # 8-byte Folded Reload
	imulq	$654183, %rax, %r8              # imm = 0x9FB67
	addq	%rbx, %r8
	imulq	$-997805, %rax, %rcx            # imm = 0xFFF0C653
	addq	-56(%rsp), %rcx                 # 8-byte Folded Reload
	shrq	$21, %r13
	addq	%r13, %rcx
	imulq	$136657, %rax, %r11             # imm = 0x215D1
	addq	-104(%rsp), %r11                # 8-byte Folded Reload
	movq	%rdi, -72(%rsp)                 # 8-byte Spill
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r11
	movq	%r11, -104(%rsp)                # 8-byte Spill
	imulq	$-683901, %rax, %rax            # imm = 0xFFF59083
	addq	-32(%rsp), %rax                 # 8-byte Folded Reload
	andq	$-2097152, %rbp                 # imm = 0xFFE00000
	subq	%rbp, %rax
	movq	%rax, -80(%rsp)                 # 8-byte Spill
	movq	-8(%rsp), %rbx                  # 8-byte Reload
	movl	10(%rbx), %eax
	shrl	$4, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	imulq	$666643, %r9, %rdi              # imm = 0xA2C13
	addq	%rax, %rdi
	imulq	$470296, %r9, %rax              # imm = 0x72D18
	addq	%rsi, %rax
	imulq	$654183, %r9, %rsi              # imm = 0x9FB67
	addq	%rdx, %rsi
	imulq	$-997805, %r9, %rdx             # imm = 0xFFF0C653
	addq	%r8, %rdx
	imulq	$136657, %r9, %rbp              # imm = 0x215D1
	addq	%rcx, %rbp
	movl	7(%rbx), %ecx
	shrl	$7, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	imulq	$666643, %r14, %r13             # imm = 0xA2C13
	addq	%rcx, %r13
	imulq	$470296, %r14, %r8              # imm = 0x72D18
	addq	%rdi, %r8
	imulq	$654183, %r14, %r11             # imm = 0x9FB67
	addq	%rax, %r11
	imulq	$-997805, %r14, %rax            # imm = 0xFFF0C653
	addq	%rsi, %rax
	imulq	$136657, %r14, %rsi             # imm = 0x215D1
	addq	%rdx, %rsi
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	shrl	$2, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	movq	%rcx, %rdx
	imulq	$666643, %r12, %rcx             # imm = 0xA2C13
	addq	%rdx, %rcx
	imulq	$470296, %r12, %rdx             # imm = 0x72D18
	addq	%r13, %rdx
	imulq	$136657, %r12, %rdi             # imm = 0x215D1
	addq	%rax, %rdi
	imulq	$-683901, %r12, %rax            # imm = 0xFFF59083
	addq	%rsi, %rax
	movq	%rax, -40(%rsp)                 # 8-byte Spill
	movl	2(%rbx), %eax
	shrl	$5, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	imulq	$666643, %r15, %rsi             # imm = 0xA2C13
	addq	%rax, %rsi
	imulq	$470296, %r15, %r13             # imm = 0x72D18
	addq	%rcx, %r13
	imulq	$654183, %r15, %rcx             # imm = 0x9FB67
	addq	%rdx, %rcx
	imulq	$654183, %r12, %rdx             # imm = 0x9FB67
	imulq	$-997805, %r15, %rax            # imm = 0xFFF0C653
	addq	%rdx, %rax
	addq	%r8, %rax
	imulq	$-997805, %r12, %rdx            # imm = 0xFFF0C653
	imulq	$136657, %r15, %r8              # imm = 0x215D1
	addq	%rdx, %r8
	addq	%r11, %r8
	imulq	$470296, %r10, %r11             # imm = 0x72D18
	addq	%rsi, %r11
	imulq	$-997805, %r10, %rdx            # imm = 0xFFF0C653
	addq	%rcx, %rdx
	movq	%rdx, -96(%rsp)                 # 8-byte Spill
	imulq	$-683901, %r10, %r12            # imm = 0xFFF59083
	addq	%r8, %r12
	movq	%r12, -24(%rsp)                 # 8-byte Spill
	imulq	$666643, %r10, %rcx             # imm = 0xA2C13
	movq	-88(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rcx,%rdx), %rsi
	addq	%rcx, %rdx
	addq	$1048576, %rdx                  # imm = 0x100000
	movq	%rdx, %rcx
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rsi
	movq	%rsi, -56(%rsp)                 # 8-byte Spill
	imulq	$654183, %r10, %rdx             # imm = 0x9FB67
	leaq	(%rdx,%r13), %rsi
	movq	%rsi, -48(%rsp)                 # 8-byte Spill
	addq	%rdx, %r13
	addq	$1048576, %r13                  # imm = 0x100000
	imulq	$136657, %r10, %rdx             # imm = 0x215D1
	leaq	(%rax,%rdx), %rsi
	movq	%rsi, -32(%rsp)                 # 8-byte Spill
	leaq	(%rdx,%rax), %r8
	addq	$1048576, %r8                   # imm = 0x100000
	imulq	$-683901, %r15, %rax            # imm = 0xFFF59083
	leaq	(%rdi,%rax), %r10
	addq	%rdi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %r15
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r10
	imulq	$-683901, %r14, %rax            # imm = 0xFFF59083
	leaq	(%rax,%rbp), %rdx
	addq	%rbp, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	imulq	$-683901, %r9, %r9              # imm = 0xFFF59083
	addq	-104(%rsp), %r9                 # 8-byte Folded Reload
	movq	%rax, %r14
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rdx
	movq	-72(%rsp), %rax                 # 8-byte Reload
	sarq	$21, %rax
	movq	-80(%rsp), %rsi                 # 8-byte Reload
	leaq	(%rsi,%rax), %rbp
	addq	%rsi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %rbx
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rbp
	sarq	$21, %rcx
	leaq	(%r11,%rcx), %rax
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	leaq	(%rcx,%r11), %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%r8, %rcx
	sarq	$21, %rcx
	movq	%rcx, -88(%rsp)                 # 8-byte Spill
	leaq	(%rcx,%r12), %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%rsi, %rdi
	sarq	$21, %rdi
	addq	%r10, %rdi
	sarq	$21, %r15
	movq	-40(%rsp), %r10                 # 8-byte Reload
	leaq	(%r10,%r15), %rcx
	addq	%r15, %r10
	addq	$1048576, %r10                  # imm = 0x100000
	movq	%r10, %r11
	sarq	$21, %r11
	addq	%rdx, %r11
	andq	$-2097152, %r10                 # imm = 0xFFE00000
	subq	%r10, %rcx
	movq	%rcx, -104(%rsp)                # 8-byte Spill
	sarq	$21, %r14
	leaq	(%r9,%r14), %rcx
	leaq	(%r14,%r9), %rdx
	addq	$1048576, %rdx                  # imm = 0x100000
	movq	%rdx, %r10
	sarq	$21, %r10
	addq	%rbp, %r10
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rcx
	movq	%rcx, -80(%rsp)                 # 8-byte Spill
	sarq	$21, %rbx
	movq	-16(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rbx,%rdx), %rcx
	addq	%rbx, %rdx
	addq	$1048576, %rdx                  # imm = 0x100000
	movq	%rdx, %rbx
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rcx
	movq	%rcx, -72(%rsp)                 # 8-byte Spill
	sarq	$21, %rbx
	imulq	$666643, %rbx, %r9              # imm = 0xA2C13
	addq	-56(%rsp), %r9                  # 8-byte Folded Reload
	imulq	$470296, %rbx, %r14             # imm = 0x72D18
	addq	-64(%rsp), %r14                 # 8-byte Folded Reload
	movq	%rax, %r15
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r14
	imulq	$654183, %rbx, %rbp             # imm = 0x9FB67
	addq	-48(%rsp), %rbp                 # 8-byte Folded Reload
	movq	%r13, %rdx
	andq	$-2097152, %r13                 # imm = 0xFFE00000
	subq	%r13, %rbp
	sarq	$21, %r15
	addq	%r15, %rbp
	imulq	$136657, %rbx, %rax             # imm = 0x215D1
	addq	-32(%rsp), %rax                 # 8-byte Folded Reload
	andq	$-2097152, %r8                  # imm = 0xFFE00000
	subq	%r8, %rax
	sarq	$21, %rdx
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	leaq	(%rdx,%rcx), %r15
	addq	$1048576, %r15                  # imm = 0x100000
	movq	%r15, %r8
	sarq	$21, %r8
	addq	%r8, %rax
	movq	%r9, %r8
	sarq	$21, %r8
	addq	%r14, %r8
	movq	%r8, %r12
	sarq	$21, %r12
	addq	%rbp, %r12
	imulq	$-997805, %rbx, %r13            # imm = 0xFFF0C653
	addq	%rcx, %r13
	addq	%rdx, %r13
	andq	$-2097152, %r15                 # imm = 0xFFE00000
	subq	%r15, %r13
	movq	%r12, %rdx
	sarq	$21, %rdx
	addq	%rdx, %r13
	movq	%r13, %r15
	sarq	$21, %r15
	addq	%rax, %r15
	imulq	$-683901, %rbx, %rdx            # imm = 0xFFF59083
	addq	-24(%rsp), %rdx                 # 8-byte Folded Reload
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	andl	$2097151, %r9d                  # imm = 0x1FFFFF
	andl	$2097151, %r8d                  # imm = 0x1FFFFF
	andl	$2097151, %r12d                 # imm = 0x1FFFFF
	andl	$2097151, %r13d                 # imm = 0x1FFFFF
	addq	-88(%rsp), %rdx                 # 8-byte Folded Reload
	movq	%r15, %rax
	sarq	$21, %rax
	subq	%rsi, %rdx
	addq	%rax, %rdx
	andl	$2097151, %r15d                 # imm = 0x1FFFFF
	movq	%rdx, %rbx
	sarq	$21, %rbx
	addq	%rdi, %rbx
	movq	%rbx, -88(%rsp)                 # 8-byte Spill
	sarq	$21, %rbx
	addq	-104(%rsp), %rbx                # 8-byte Folded Reload
	movq	%rbx, -96(%rsp)                 # 8-byte Spill
	sarq	$21, %rbx
	addq	%r11, %rbx
	movq	%rbx, %rdi
	sarq	$21, %rdi
	addq	-80(%rsp), %rdi                 # 8-byte Folded Reload
	movq	%rdi, %r14
	sarq	$21, %r14
	addq	%r10, %r14
	movq	%r14, %rbp
	sarq	$21, %rbp
	addq	-72(%rsp), %rbp                 # 8-byte Folded Reload
	movq	%rbp, %r10
	sarq	$21, %r10
	imulq	$666643, %r10, %rax             # imm = 0xA2C13
	addq	%r9, %rax
	imulq	$470296, %r10, %r9              # imm = 0x72D18
	addq	%r8, %r9
	imulq	$654183, %r10, %r8              # imm = 0x9FB67
	addq	%r12, %r8
	imulq	$-997805, %r10, %r12            # imm = 0xFFF0C653
	addq	%r13, %r12
	imulq	$136657, %r10, %rsi             # imm = 0x215D1
	addq	%r15, %rsi
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	-88(%rsp), %rcx                 # 8-byte Reload
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	movq	%rcx, -88(%rsp)                 # 8-byte Spill
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	movq	%rcx, -96(%rsp)                 # 8-byte Spill
	andl	$2097151, %ebx                  # imm = 0x1FFFFF
	andl	$2097151, %edi                  # imm = 0x1FFFFF
	andl	$2097151, %r14d                 # imm = 0x1FFFFF
	andl	$2097151, %ebp                  # imm = 0x1FFFFF
	imulq	$-683901, %r10, %rcx            # imm = 0xFFF59083
	addq	%rdx, %rcx
	movq	%rax, %r13
	sarq	$21, %r13
	addq	%r9, %r13
	movq	%r13, %r15
	sarq	$21, %r15
	addq	%r8, %r15
	movq	%r15, %r11
	sarq	$21, %r11
	addq	%r12, %r11
	movq	%r11, %r10
	sarq	$21, %r10
	addq	%rsi, %r10
	movq	%r10, %r9
	sarq	$21, %r9
	addq	%rcx, %r9
	movq	%r9, %r8
	sarq	$21, %r8
	addq	-88(%rsp), %r8                  # 8-byte Folded Reload
	movq	%r8, %r12
	sarq	$21, %r12
	addq	-96(%rsp), %r12                 # 8-byte Folded Reload
	movq	%r12, %rdx
	sarq	$21, %rdx
	addq	%rbx, %rdx
	movq	%rdx, %rbx
	sarq	$21, %rbx
	addq	%rdi, %rbx
	movq	%rbx, %rcx
	sarq	$21, %rcx
	addq	%r14, %rcx
	movq	%rcx, %rsi
	sarq	$21, %rsi
	addq	%rbp, %rsi
	movq	-8(%rsp), %rbp                  # 8-byte Reload
	movb	%al, (%rbp)
	movb	%ah, 1(%rbp)
	shrl	$16, %eax
	andl	$31, %eax
	movl	%r13d, %edi
	shll	$5, %edi
	orl	%eax, %edi
	movb	%dil, 2(%rbp)
	movq	%r13, %rax
	shrq	$3, %rax
	movb	%al, 3(%rbp)
	movq	%r13, %rax
	shrq	$11, %rax
	movb	%al, 4(%rbp)
	shrl	$19, %r13d
	andl	$3, %r13d
	leal	(%r13,%r15,4), %eax
	movb	%al, 5(%rbp)
	movq	%r15, %rax
	shrq	$6, %rax
	movb	%al, 6(%rbp)
	movl	%r11d, %eax
	shll	$7, %eax
	shrl	$14, %r15d
	andl	$127, %r15d
	orl	%eax, %r15d
	movb	%r15b, 7(%rbp)
	movq	%r11, %rax
	shrq	%rax
	movb	%al, 8(%rbp)
	movq	%r11, %rax
	shrq	$9, %rax
	movb	%al, 9(%rbp)
	movl	%r10d, %eax
	shll	$4, %eax
	shrl	$17, %r11d
	andl	$15, %r11d
	orl	%eax, %r11d
	movb	%r11b, 10(%rbp)
	movq	%r10, %rax
	shrq	$4, %rax
	movb	%al, 11(%rbp)
	movq	%r10, %rax
	shrq	$12, %rax
	movb	%al, 12(%rbp)
	shrl	$20, %r10d
	andl	$1, %r10d
	leal	(%r10,%r9,2), %eax
	movb	%al, 13(%rbp)
	movq	%r9, %rax
	shrq	$7, %rax
	movb	%al, 14(%rbp)
	movl	%r8d, %eax
	shll	$6, %eax
	shrl	$15, %r9d
	andl	$63, %r9d
	orl	%eax, %r9d
	movb	%r9b, 15(%rbp)
	movq	%r8, %rax
	shrq	$2, %rax
	movb	%al, 16(%rbp)
	movq	%r8, %rax
	shrq	$10, %rax
	movb	%al, 17(%rbp)
	shrl	$18, %r8d
	andl	$7, %r8d
	leal	(%r8,%r12,8), %eax
	movb	%al, 18(%rbp)
	movq	%r12, %rax
	shrq	$5, %rax
	movb	%al, 19(%rbp)
	shrq	$13, %r12
	movb	%r12b, 20(%rbp)
	movb	%dl, 21(%rbp)
	movb	%dh, 22(%rbp)
	shrl	$16, %edx
	andl	$31, %edx
	movl	%ebx, %eax
	shll	$5, %eax
	orl	%edx, %eax
	movb	%al, 23(%rbp)
	movq	%rbx, %rax
	shrq	$3, %rax
	movb	%al, 24(%rbp)
	movq	%rbx, %rax
	shrq	$11, %rax
	movb	%al, 25(%rbp)
	shrl	$19, %ebx
	andl	$3, %ebx
	leal	(%rbx,%rcx,4), %eax
	movb	%al, 26(%rbp)
	movq	%rcx, %rax
	shrq	$6, %rax
	movb	%al, 27(%rbp)
	movl	%esi, %eax
	shll	$7, %eax
	shrl	$14, %ecx
	andl	$127, %ecx
	orl	%eax, %ecx
	movb	%cl, 28(%rbp)
	movq	%rsi, %rax
	shrq	%rax
	movb	%al, 29(%rbp)
	movq	%rsi, %rax
	shrq	$9, %rax
	movb	%al, 30(%rbp)
	shrq	$17, %rsi
	movb	%sil, 31(%rbp)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end9:
	.size	x25519_sc_reduce, .Lfunc_end9-x25519_sc_reduce
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function x25519_ge_scalarmult_base
.LCPI10_0:
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.byte	15                              # 0xf
	.zero	1
	.zero	1
	.zero	1
	.zero	1
	.zero	1
	.zero	1
	.zero	1
	.zero	1
.LCPI10_1:
	.zero	16,15
	.text
	.globl	x25519_ge_scalarmult_base
	.p2align	4, 0x90
	.type	x25519_ge_scalarmult_base,@function
x25519_ge_scalarmult_base:              # @x25519_ge_scalarmult_base
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$488, %rsp                      # imm = 0x1E8
	movq	%rdi, %rbx
	movq	(%rsi), %xmm1                   # xmm1 = mem[0],zero
	movdqa	.LCPI10_0(%rip), %xmm0          # xmm0 = <15,15,15,15,15,15,15,15,u,u,u,u,u,u,u,u>
	movdqa	%xmm1, %xmm2
	pand	%xmm0, %xmm2
	psrlw	$4, %xmm1
	movdqa	.LCPI10_1(%rip), %xmm3          # xmm3 = [15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15]
	pand	%xmm3, %xmm1
	punpcklbw	%xmm1, %xmm2            # xmm2 = xmm2[0],xmm1[0],xmm2[1],xmm1[1],xmm2[2],xmm1[2],xmm2[3],xmm1[3],xmm2[4],xmm1[4],xmm2[5],xmm1[5],xmm2[6],xmm1[6],xmm2[7],xmm1[7]
	movdqa	%xmm2, 16(%rsp)
	movq	8(%rsi), %xmm1                  # xmm1 = mem[0],zero
	movdqa	%xmm1, %xmm2
	pand	%xmm0, %xmm2
	psrlw	$4, %xmm1
	pand	%xmm3, %xmm1
	punpcklbw	%xmm1, %xmm2            # xmm2 = xmm2[0],xmm1[0],xmm2[1],xmm1[1],xmm2[2],xmm1[2],xmm2[3],xmm1[3],xmm2[4],xmm1[4],xmm2[5],xmm1[5],xmm2[6],xmm1[6],xmm2[7],xmm1[7]
	movdqa	%xmm2, 32(%rsp)
	movq	16(%rsi), %xmm1                 # xmm1 = mem[0],zero
	movdqa	%xmm1, %xmm2
	pand	%xmm0, %xmm2
	psrlw	$4, %xmm1
	pand	%xmm3, %xmm1
	punpcklbw	%xmm1, %xmm2            # xmm2 = xmm2[0],xmm1[0],xmm2[1],xmm1[1],xmm2[2],xmm1[2],xmm2[3],xmm1[3],xmm2[4],xmm1[4],xmm2[5],xmm1[5],xmm2[6],xmm1[6],xmm2[7],xmm1[7]
	movdqa	%xmm2, 48(%rsp)
	movq	24(%rsi), %xmm1                 # xmm1 = mem[0],zero
	pand	%xmm1, %xmm0
	psrlw	$4, %xmm1
	pand	%xmm3, %xmm1
	punpcklbw	%xmm1, %xmm0            # xmm0 = xmm0[0],xmm1[0],xmm0[1],xmm1[1],xmm0[2],xmm1[2],xmm0[3],xmm1[3],xmm0[4],xmm1[4],xmm0[5],xmm1[5],xmm0[6],xmm1[6],xmm0[7],xmm1[7]
	movdqa	%xmm0, 64(%rsp)
	movl	$1, %eax
	xorl	%ecx, %ecx
	.p2align	4, 0x90
.LBB10_1:                               # =>This Inner Loop Header: Depth=1
	addb	15(%rsp,%rax), %cl
	leal	8(%rcx), %esi
	movl	%esi, %edx
	sarb	$4, %dl
	andb	$-16, %sil
	subb	%sil, %cl
	movb	%cl, 15(%rsp,%rax)
	cmpq	$63, %rax
	je	.LBB10_3
# %bb.2:                                #   in Loop: Header=BB10_1 Depth=1
	addb	16(%rsp,%rax), %dl
	leal	8(%rdx), %esi
	movl	%esi, %ecx
	sarb	$4, %cl
	andb	$-16, %sil
	subb	%sil, %dl
	movb	%dl, 16(%rsp,%rax)
	addq	$2, %rax
	jmp	.LBB10_1
.LBB10_3:
	addb	%dl, 79(%rsp)
	pxor	%xmm0, %xmm0
	movdqu	%xmm0, 16(%rbx)
	movdqu	%xmm0, (%rbx)
	movq	$0, 32(%rbx)
	leaq	40(%rbx), %r13
	movdqu	%xmm0, 48(%rbx)
	movdqu	%xmm0, 64(%rbx)
	movq	$1, 40(%rbx)
	leaq	80(%rbx), %rax
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movdqu	%xmm0, 88(%rbx)
	movdqu	%xmm0, 104(%rbx)
	movq	$1, 80(%rbx)
	movdqu	%xmm0, 120(%rbx)
	movdqu	%xmm0, 136(%rbx)
	movq	$0, 152(%rbx)
	movq	$-1, %r12
	xorl	%ebp, %ebp
	leaq	208(%rsp), %r14
	leaq	328(%rsp), %r15
	.p2align	4, 0x90
.LBB10_4:                               # =>This Inner Loop Header: Depth=1
	movsbl	18(%rsp,%r12), %edx
	movq	%r14, %rdi
	movl	%ebp, %esi
	callq	table_select
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	ge_madd
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	x25519_ge_p1p1_to_p3
	addq	$2, %r12
	incl	%ebp
	cmpq	$62, %r12
	jb	.LBB10_4
# %bb.5:
	movq	32(%rbx), %rax
	movq	%rax, 112(%rsp)
	movups	(%rbx), %xmm0
	movups	16(%rbx), %xmm1
	movaps	%xmm1, 96(%rsp)
	movaps	%xmm0, 80(%rsp)
	movups	(%r13), %xmm0
	movups	16(%r13), %xmm1
	movups	%xmm0, 120(%rsp)
	movups	%xmm1, 136(%rsp)
	movq	32(%r13), %rax
	movq	%rax, 152(%rsp)
	movq	8(%rsp), %rax                   # 8-byte Reload
	movups	(%rax), %xmm0
	movups	16(%rax), %xmm1
	movaps	%xmm0, 160(%rsp)
	movaps	%xmm1, 176(%rsp)
	movq	32(%rax), %rax
	movq	%rax, 192(%rsp)
	leaq	328(%rsp), %r14
	leaq	80(%rsp), %rsi
	movq	%r14, %rdi
	callq	ge_p2_dbl
	leaq	80(%rsp), %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_p1p1_to_p2
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	ge_p2_dbl
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_p1p1_to_p2
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	ge_p2_dbl
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_p1p1_to_p2
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	ge_p2_dbl
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_p1p1_to_p3
	movq	$-2, %r12
	xorl	%ebp, %ebp
	leaq	208(%rsp), %r15
	.p2align	4, 0x90
.LBB10_6:                               # =>This Inner Loop Header: Depth=1
	movsbl	18(%rsp,%r12), %edx
	movq	%r15, %rdi
	movl	%ebp, %esi
	callq	table_select
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	ge_madd
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_p1p1_to_p3
	addq	$2, %r12
	incl	%ebp
	cmpq	$62, %r12
	jb	.LBB10_6
# %bb.7:
	addq	$488, %rsp                      # imm = 0x1E8
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end10:
	.size	x25519_ge_scalarmult_base, .Lfunc_end10-x25519_ge_scalarmult_base
                                        # -- End function
	.p2align	4, 0x90                         # -- Begin function table_select
	.type	table_select,@function
table_select:                           # @table_select
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
	movslq	%edx, %rax
	movq	%rax, %rcx
	sarq	$63, %rcx
	movq	%rcx, 32(%rsp)                  # 8-byte Spill
                                        # kill: def $cl killed $cl killed $rcx
	andb	%dl, %cl
	addb	%cl, %cl
	subb	%cl, %dl
	xorps	%xmm0, %xmm0
	movaps	%xmm0, -128(%rsp)
	movaps	%xmm0, -96(%rsp)
	movaps	%xmm0, -48(%rsp)
	movaps	%xmm0, -64(%rsp)
	movaps	%xmm0, -80(%rsp)
	movaps	%xmm0, -112(%rsp)
	movq	%rax, %rcx
	notq	%rcx
	decq	%rax
	andq	%rcx, %rax
	shrq	$63, %rax
	movb	%al, -128(%rsp)
	movb	%al, -96(%rsp)
	#APP
	#NO_APP
	movslq	%esi, %rax
	movzbl	%dl, %esi
	movl	%esi, %ecx
	xorl	$1, %ecx
	decq	%rcx
	sarq	$63, %rcx
	#APP
	#NO_APP
	movd	%ecx, %xmm0
	punpcklbw	%xmm0, %xmm0            # xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0]
	leaq	(%rax,%rax,2), %rcx
	shlq	$8, %rcx
	leaq	k25519Precomp(%rip), %rdx
	movdqa	(%rcx,%rdx), %xmm2
	pand	%xmm0, %xmm2
	pxor	-128(%rsp), %xmm2
	movdqa	%xmm2, -128(%rsp)
	movdqa	16(%rcx,%rdx), %xmm3
	pand	%xmm0, %xmm3
	pxor	-112(%rsp), %xmm3
	movdqa	%xmm3, -112(%rsp)
	movdqa	32(%rcx,%rdx), %xmm4
	pand	%xmm0, %xmm4
	pxor	-96(%rsp), %xmm4
	movdqa	%xmm4, -96(%rsp)
	movdqa	48(%rcx,%rdx), %xmm5
	pand	%xmm0, %xmm5
	pxor	-80(%rsp), %xmm5
	movdqa	%xmm5, -80(%rsp)
	movdqa	64(%rcx,%rdx), %xmm6
	pand	%xmm0, %xmm6
	pxor	-64(%rsp), %xmm6
	movq	%rdi, 24(%rsp)                  # 8-byte Spill
	movdqa	%xmm6, -64(%rsp)
	pand	80(%rcx,%rdx), %xmm0
	pxor	-48(%rsp), %xmm0
	movdqa	%xmm0, -48(%rsp)
	movl	%esi, %eax
	xorl	$2, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm1
	punpcklbw	%xmm1, %xmm1            # xmm1 = xmm1[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0]
	movdqa	96(%rcx,%rdx), %xmm7
	pand	%xmm1, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -128(%rsp)
	movdqa	112(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm3, %xmm2
	movdqa	%xmm2, -112(%rsp)
	movdqa	128(%rcx,%rdx), %xmm3
	pand	%xmm1, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm3, -96(%rsp)
	movdqa	144(%rcx,%rdx), %xmm4
	pand	%xmm1, %xmm4
	pxor	%xmm5, %xmm4
	movdqa	%xmm4, -80(%rsp)
	movdqa	160(%rcx,%rdx), %xmm5
	pand	%xmm1, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm5, -64(%rsp)
	pand	176(%rcx,%rdx), %xmm1
	pxor	%xmm0, %xmm1
	movdqa	%xmm1, -48(%rsp)
	movl	%esi, %eax
	xorl	$3, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm0
	punpcklbw	%xmm0, %xmm0            # xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0]
	movdqa	192(%rcx,%rdx), %xmm6
	pand	%xmm0, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm6, -128(%rsp)
	movdqa	208(%rcx,%rdx), %xmm7
	pand	%xmm0, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -112(%rsp)
	movdqa	224(%rcx,%rdx), %xmm2
	pand	%xmm0, %xmm2
	pxor	%xmm3, %xmm2
	movdqa	%xmm2, -96(%rsp)
	movdqa	240(%rcx,%rdx), %xmm3
	pand	%xmm0, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm3, -80(%rsp)
	movdqa	256(%rcx,%rdx), %xmm4
	pand	%xmm0, %xmm4
	pxor	%xmm5, %xmm4
	movdqa	%xmm4, -64(%rsp)
	pand	272(%rcx,%rdx), %xmm0
	pxor	%xmm1, %xmm0
	movdqa	%xmm0, -48(%rsp)
	movl	%esi, %eax
	xorl	$4, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm1
	punpcklbw	%xmm1, %xmm1            # xmm1 = xmm1[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0]
	movdqa	288(%rcx,%rdx), %xmm5
	pand	%xmm1, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm5, -128(%rsp)
	movdqa	304(%rcx,%rdx), %xmm6
	pand	%xmm1, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm6, -112(%rsp)
	movdqa	320(%rcx,%rdx), %xmm7
	pand	%xmm1, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -96(%rsp)
	movdqa	336(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm3, %xmm2
	movdqa	%xmm2, -80(%rsp)
	movdqa	352(%rcx,%rdx), %xmm3
	pand	%xmm1, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm3, -64(%rsp)
	pand	368(%rcx,%rdx), %xmm1
	pxor	%xmm0, %xmm1
	movdqa	%xmm1, -48(%rsp)
	movl	%esi, %eax
	xorl	$5, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm0
	punpcklbw	%xmm0, %xmm0            # xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0]
	movdqa	384(%rcx,%rdx), %xmm4
	pand	%xmm0, %xmm4
	pxor	%xmm5, %xmm4
	movdqa	%xmm4, -128(%rsp)
	movdqa	400(%rcx,%rdx), %xmm5
	pand	%xmm0, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm5, -112(%rsp)
	movdqa	416(%rcx,%rdx), %xmm6
	pand	%xmm0, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm6, -96(%rsp)
	movdqa	432(%rcx,%rdx), %xmm7
	pand	%xmm0, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -80(%rsp)
	movdqa	448(%rcx,%rdx), %xmm2
	pand	%xmm0, %xmm2
	pxor	%xmm3, %xmm2
	pand	464(%rcx,%rdx), %xmm0
	movdqa	%xmm2, -64(%rsp)
	pxor	%xmm1, %xmm0
	movdqa	%xmm0, -48(%rsp)
	movl	%esi, %eax
	xorl	$6, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm1
	punpcklbw	%xmm1, %xmm1            # xmm1 = xmm1[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0]
	movdqa	480(%rcx,%rdx), %xmm3
	pand	%xmm1, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm3, -128(%rsp)
	movdqa	496(%rcx,%rdx), %xmm4
	pand	%xmm1, %xmm4
	pxor	%xmm5, %xmm4
	movdqa	%xmm4, -112(%rsp)
	movdqa	512(%rcx,%rdx), %xmm5
	pand	%xmm1, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm5, -96(%rsp)
	movdqa	528(%rcx,%rdx), %xmm6
	pand	%xmm1, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm6, -80(%rsp)
	movdqa	544(%rcx,%rdx), %xmm7
	pand	%xmm1, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -64(%rsp)
	pand	560(%rcx,%rdx), %xmm1
	pxor	%xmm0, %xmm1
	movdqa	%xmm1, -48(%rsp)
	movl	%esi, %eax
	xorl	$7, %eax
	decq	%rax
	sarq	$63, %rax
	#APP
	#NO_APP
	movd	%eax, %xmm0
	punpcklbw	%xmm0, %xmm0            # xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm0, %xmm0                # xmm0 = xmm0[0,0,0,0]
	movdqa	576(%rcx,%rdx), %xmm2
	pand	%xmm0, %xmm2
	pxor	%xmm3, %xmm2
	movdqa	%xmm2, -128(%rsp)
	movdqa	592(%rcx,%rdx), %xmm3
	pand	%xmm0, %xmm3
	pxor	%xmm4, %xmm3
	movdqa	%xmm3, -112(%rsp)
	movdqa	608(%rcx,%rdx), %xmm4
	pand	%xmm0, %xmm4
	pxor	%xmm5, %xmm4
	movdqa	%xmm4, -96(%rsp)
	movdqa	624(%rcx,%rdx), %xmm5
	pand	%xmm0, %xmm5
	pxor	%xmm6, %xmm5
	movdqa	%xmm5, -80(%rsp)
	movdqa	640(%rcx,%rdx), %xmm6
	pand	%xmm0, %xmm6
	pxor	%xmm7, %xmm6
	movdqa	%xmm6, -64(%rsp)
	pand	656(%rcx,%rdx), %xmm0
	pxor	%xmm1, %xmm0
	movdqa	%xmm0, -48(%rsp)
	xorl	$8, %esi
	decq	%rsi
	sarq	$63, %rsi
	#APP
	#NO_APP
	movd	%esi, %xmm1
	punpcklbw	%xmm1, %xmm1            # xmm1 = xmm1[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	pshuflw	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0,4,5,6,7]
	pshufd	$0, %xmm1, %xmm1                # xmm1 = xmm1[0,0,0,0]
	movdqa	672(%rcx,%rdx), %xmm7
	pand	%xmm1, %xmm7
	pxor	%xmm2, %xmm7
	movdqa	%xmm7, -128(%rsp)
	movdqa	688(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm3, %xmm2
	movdqa	%xmm2, -112(%rsp)
	movdqa	704(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm4, %xmm2
	movdqa	%xmm2, -96(%rsp)
	movdqa	720(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm5, %xmm2
	movdqa	%xmm2, -80(%rsp)
	movdqa	736(%rcx,%rdx), %xmm2
	pand	%xmm1, %xmm2
	pxor	%xmm6, %xmm2
	movdqa	%xmm2, -64(%rsp)
	pand	752(%rcx,%rdx), %xmm1
	pxor	%xmm0, %xmm1
	movdqa	%xmm1, -48(%rsp)
	movl	-100(%rsp), %eax
	shlq	$20, %rax
	movzbl	-101(%rsp), %ecx
	shlq	$12, %rcx
	orq	%rax, %rcx
	movzbl	-102(%rsp), %r11d
	shlq	$4, %r11
	orq	%rcx, %r11
	movl	-112(%rsp), %eax
	movl	-106(%rsp), %edi
	movq	%rdi, %r10
	shlq	$23, %r10
	movzbl	-107(%rsp), %ecx
	shlq	$15, %rcx
	movzbl	-108(%rsp), %ebx
	shlq	$7, %rbx
	movzbl	-113(%rsp), %edx
	shlq	$18, %rdx
	movzbl	-114(%rsp), %esi
	shlq	$10, %rsi
	movzbl	-116(%rsp), %r14d
	movzbl	-117(%rsp), %r15d
	shlq	$37, %r15
	movzbl	-122(%rsp), %r12d
	movzbl	-123(%rsp), %r8d
	shlq	$40, %r8
	movl	%r12d, %r9d
	andl	$7, %r9d
	shlq	$48, %r9
	orq	%r8, %r9
	movl	-128(%rsp), %r8d
	orq	%r8, %r9
	movzbl	-124(%rsp), %r8d
	shlq	$32, %r8
	orq	%r8, %r9
	movl	%r14d, %r8d
	andl	$63, %r8d
	shlq	$45, %r8
	orq	%r15, %r8
	movzbl	-118(%rsp), %r15d
	shlq	$29, %r15
	orq	%r15, %r8
	movzbl	-119(%rsp), %r15d
	shlq	$21, %r15
	orq	%r15, %r8
	movzbl	-120(%rsp), %r15d
	shlq	$13, %r15
	orq	%r15, %r8
	movzbl	-121(%rsp), %r15d
	shlq	$5, %r15
	orq	%r15, %r8
	movzbl	-115(%rsp), %r15d
	shrq	$3, %r12
	orq	%r12, %r8
	shrq	$6, %r14
	movl	%eax, %r12d
	andl	$33554431, %r12d                # imm = 0x1FFFFFF
	shlq	$26, %r12
	orq	%rdx, %r12
	orq	%rsi, %r12
	leaq	(%r12,%r15,4), %rdx
	orq	%r14, %rdx
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	shrq	$25, %rax
	movabsq	$2251799746576384, %rdx         # imm = 0x7FFFFFC000000
	orq	$58720256, %rdx                 # imm = 0x3800000
	andq	%rdx, %r10
	movq	%rdx, %r14
	orq	%rcx, %r10
	orq	%rbx, %r10
	orq	%rax, %r10
	movq	%r10, (%rsp)                    # 8-byte Spill
	shrq	$28, %rdi
	orq	%r11, %rdi
	movq	%rdi, 8(%rsp)                   # 8-byte Spill
	movl	-68(%rsp), %eax
	shlq	$20, %rax
	movzbl	-69(%rsp), %ecx
	shlq	$12, %rcx
	orq	%rax, %rcx
	movzbl	-70(%rsp), %r13d
	shlq	$4, %r13
	orq	%rcx, %r13
	movl	-80(%rsp), %eax
	movzbl	-84(%rsp), %ecx
	movzbl	-85(%rsp), %edx
	shlq	$37, %rdx
	movzbl	-90(%rsp), %esi
	movzbl	-91(%rsp), %ebx
	shlq	$40, %rbx
	movl	%esi, %r11d
	andl	$7, %r11d
	shlq	$48, %r11
	orq	%rbx, %r11
	movl	-96(%rsp), %ebx
	orq	%rbx, %r11
	movzbl	-92(%rsp), %ebx
	shlq	$32, %rbx
	orq	%rbx, %r11
	movq	%r11, -24(%rsp)                 # 8-byte Spill
	movl	%ecx, %ebx
	andl	$63, %ebx
	shlq	$45, %rbx
	orq	%rdx, %rbx
	movzbl	-86(%rsp), %edx
	shlq	$29, %rdx
	orq	%rdx, %rbx
	movzbl	-87(%rsp), %edx
	shlq	$21, %rdx
	orq	%rdx, %rbx
	movzbl	-88(%rsp), %edx
	shlq	$13, %rdx
	orq	%rdx, %rbx
	movzbl	-89(%rsp), %edx
	shlq	$5, %rdx
	orq	%rdx, %rbx
	movzbl	-81(%rsp), %edx
	shlq	$18, %rdx
	shrq	$3, %rsi
	orq	%rsi, %rbx
	movl	%eax, %esi
	andl	$33554431, %esi                 # imm = 0x1FFFFFF
	shlq	$26, %rsi
	orq	%rdx, %rsi
	movzbl	-82(%rsp), %edx
	shlq	$10, %rdx
	orq	%rdx, %rsi
	movzbl	-83(%rsp), %edx
	leaq	(%rsi,%rdx,4), %rsi
	movl	-74(%rsp), %edx
	shrq	$6, %rcx
	orq	%rcx, %rsi
	movq	%rsi, -32(%rsp)                 # 8-byte Spill
	movq	%rdx, %r12
	shlq	$23, %r12
	movzbl	-75(%rsp), %ecx
	shlq	$15, %rcx
	andq	%r14, %r12
	orq	%rcx, %r12
	movzbl	-76(%rsp), %ecx
	shlq	$7, %rcx
	orq	%rcx, %r12
	shrq	$25, %rax
	orq	%rax, %r12
	shrq	$28, %rdx
	orq	%r13, %rdx
	movq	%rdx, -8(%rsp)                  # 8-byte Spill
	movl	-36(%rsp), %eax
	shlq	$20, %rax
	movzbl	-37(%rsp), %ecx
	shlq	$12, %rcx
	orq	%rax, %rcx
	movzbl	-38(%rsp), %ebp
	shlq	$4, %rbp
	orq	%rcx, %rbp
	movzbl	-59(%rsp), %eax
	shlq	$40, %rax
	movzbl	-58(%rsp), %ecx
	movl	%ecx, %r13d
	andl	$7, %r13d
	shlq	$48, %r13
	orq	%rax, %r13
	movl	-64(%rsp), %eax
	orq	%rax, %r13
	movzbl	-60(%rsp), %eax
	shlq	$32, %rax
	orq	%rax, %r13
	movzbl	-53(%rsp), %esi
	shlq	$37, %rsi
	movzbl	-52(%rsp), %edi
	movl	%edi, %r10d
	andl	$63, %r10d
	shlq	$45, %r10
	orq	%rsi, %r10
	movzbl	-54(%rsp), %esi
	shlq	$29, %rsi
	orq	%rsi, %r10
	movzbl	-55(%rsp), %esi
	shlq	$21, %rsi
	orq	%rsi, %r10
	movzbl	-56(%rsp), %esi
	shlq	$13, %rsi
	orq	%rsi, %r10
	movzbl	-57(%rsp), %esi
	shlq	$5, %rsi
	orq	%rsi, %r10
	shrq	$3, %rcx
	orq	%rcx, %r10
	movzbl	-49(%rsp), %ecx
	shlq	$18, %rcx
	movl	-48(%rsp), %edx
	movl	%edx, %esi
	andl	$33554431, %esi                 # imm = 0x1FFFFFF
	shlq	$26, %rsi
	orq	%rcx, %rsi
	movzbl	-50(%rsp), %ecx
	shlq	$10, %rcx
	orq	%rcx, %rsi
	movzbl	-51(%rsp), %ecx
	leaq	(%rsi,%rcx,4), %rax
	shrq	$6, %rdi
	orq	%rdi, %rax
	movq	%rax, -16(%rsp)                 # 8-byte Spill
	movl	-42(%rsp), %r15d
	movq	%r15, %rcx
	shlq	$23, %rcx
	andq	%r14, %rcx
	movzbl	-43(%rsp), %edi
	shlq	$15, %rdi
	orq	%rdi, %rcx
	movzbl	-44(%rsp), %edi
	shlq	$7, %rdi
	orq	%rdi, %rcx
	shrq	$25, %rdx
	orq	%rdx, %rcx
	shrq	$28, %r15
	orq	%rbp, %r15
	movq	%r11, %rbp
	xorq	%r9, %rbp
	movq	32(%rsp), %rsi                  # 8-byte Reload
	andq	%rsi, %rbp
	xorq	%rbp, %r9
	movq	24(%rsp), %r11                  # 8-byte Reload
	movq	%r9, (%r11)
	movq	%r8, %rdi
	xorq	%rbx, %rdi
	andq	%rsi, %rdi
	xorq	%rdi, %r8
	movq	%r8, 8(%r11)
	movq	16(%rsp), %r8                   # 8-byte Reload
	movq	%r8, %rdx
	xorq	-32(%rsp), %rdx                 # 8-byte Folded Reload
	andq	%rsi, %rdx
	xorq	%rdx, %r8
	movq	%r8, 16(%r11)
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rax, %r8
	xorq	%r12, %r8
	andq	%rsi, %r8
	xorq	%r8, %rax
	movq	%rax, 24(%r11)
	movq	8(%rsp), %rax                   # 8-byte Reload
	movq	%rax, %r9
	movq	-8(%rsp), %r14                  # 8-byte Reload
	xorq	%r14, %r9
	andq	%rsi, %r9
	xorq	%r9, %rax
	movq	%rax, 32(%r11)
	xorq	-24(%rsp), %rbp                 # 8-byte Folded Reload
	movq	%rbp, 40(%r11)
	xorq	%rbx, %rdi
	movq	%rdi, 48(%r11)
	xorq	-32(%rsp), %rdx                 # 8-byte Folded Reload
	movq	%rdx, 56(%r11)
	xorq	%r12, %r8
	movq	%r8, 64(%r11)
	xorq	%r14, %r9
	movabsq	$4503599627370494, %rdi         # imm = 0xFFFFFFFFFFFFE
	movq	%r9, 72(%r11)
	movq	%rdi, %rdx
	subq	%r13, %rdx
	addq	$-36, %rdx
	xorq	%r13, %rdx
	andq	%rsi, %rdx
	xorq	%r13, %rdx
	movq	%rdx, 80(%r11)
	movq	%rdi, %rdx
	subq	%r10, %rdx
	xorq	%r10, %rdx
	andq	%rsi, %rdx
	xorq	%r10, %rdx
	movq	%rdx, 88(%r11)
	movq	%rdi, %rax
	movq	-16(%rsp), %rdx                 # 8-byte Reload
	subq	%rdx, %rax
	xorq	%rdx, %rax
	andq	%rsi, %rax
	xorq	%rdx, %rax
	movq	%rax, 96(%r11)
	movq	%rdi, %rax
	subq	%rcx, %rax
	xorq	%rcx, %rax
	andq	%rsi, %rax
	xorq	%rcx, %rax
	movq	%rax, 104(%r11)
	subq	%r15, %rdi
	xorq	%r15, %rdi
	andq	%rsi, %rdi
	xorq	%r15, %rdi
	movq	%rdi, 112(%r11)
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end11:
	.size	table_select, .Lfunc_end11-table_select
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function ge_madd
.LCPI12_0:
	.quad	4503599627370458                # 0xfffffffffffda
	.quad	4503599627370494                # 0xffffffffffffe
.LCPI12_1:
	.quad	4503599627370494                # 0xffffffffffffe
	.quad	4503599627370494                # 0xffffffffffffe
	.text
	.p2align	4, 0x90
	.type	ge_madd,@function
ge_madd:                                # @ge_madd
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$152, %rsp
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	32(%rsi), %rax
	addq	72(%rsi), %rax
	movdqu	40(%rsi), %xmm0
	movdqu	(%rsi), %xmm1
	paddq	%xmm0, %xmm1
	movdqu	16(%rsi), %xmm0
	movdqu	56(%rsi), %xmm2
	paddq	%xmm0, %xmm2
	movdqu	%xmm1, (%rdi)
	movdqu	%xmm2, 16(%rdi)
	movq	%rax, 32(%rdi)
	movabsq	$4503599627370494, %r13         # imm = 0xFFFFFFFFFFFFE
	movq	72(%rsi), %rax
	addq	%r13, %rax
	subq	32(%rsi), %rax
	movdqu	(%rsi), %xmm0
	movdqu	16(%rsi), %xmm1
	movdqu	40(%rsi), %xmm2
	psubq	%xmm0, %xmm2
	movdqu	56(%rsi), %xmm0
	psubq	%xmm1, %xmm0
	paddq	.LCPI12_0(%rip), %xmm2
	paddq	.LCPI12_1(%rip), %xmm0
	leaq	40(%rdi), %r12
	movdqu	%xmm2, 40(%rdi)
	movdqu	%xmm0, 56(%rdi)
	movq	%rax, 72(%rdi)
	leaq	72(%rsp), %rdi
	movq	%rbx, %rsi
	callq	fe_mul_impl
	leaq	40(%r15), %rdx
	leaq	112(%rsp), %rdi
	movq	%r12, %rsi
	callq	fe_mul_impl
	addq	$80, %r15
	leaq	120(%r14), %rdx
	leaq	32(%rsp), %rdi
	movq	%r15, %rsi
	callq	fe_mul_impl
	movq	80(%r14), %rsi
	movq	88(%r14), %rax
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	96(%r14), %rdi
	movq	104(%r14), %rax
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	112(%r14), %rax
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	72(%rsp), %r11
	movq	80(%rsp), %r10
	leaq	(%r11,%r13), %r14
	addq	$-36, %r14
	movq	112(%rsp), %r8
	movq	120(%rsp), %r9
	subq	%r8, %r14
	movq	88(%rsp), %r15
	movq	128(%rsp), %r12
	movq	96(%rsp), %rbp
	movq	136(%rsp), %rcx
	movq	104(%rsp), %rdx
	movq	144(%rsp), %rax
	movq	%r14, (%rbx)
	leaq	(%r10,%r13), %r14
	subq	%r9, %r14
	movq	%r14, 8(%rbx)
	leaq	(%r15,%r13), %r14
	subq	%r12, %r14
	movq	%r14, 16(%rbx)
	movq	%rbp, %r14
	addq	%r13, %r14
	subq	%rcx, %r14
	movq	%r14, 24(%rbx)
	leaq	(%rdx,%r13), %r14
	subq	%rax, %r14
	movq	%r14, 32(%rbx)
	addq	%r11, %r8
	addq	%r10, %r9
	addq	%r15, %r12
	addq	%rbp, %rcx
	addq	%rsi, %rsi
	addq	%rdx, %rax
	movq	%r8, 40(%rbx)
	movq	%r9, 48(%rbx)
	movq	%r12, 56(%rbx)
	movq	%rcx, 64(%rbx)
	movq	%rax, 72(%rbx)
	movabsq	$2251799813685247, %r8          # imm = 0x7FFFFFFFFFFFF
	leaq	-1(%r8), %r9
	andq	%rsi, %r9
	shrq	$51, %rsi
	movq	24(%rsp), %rax                  # 8-byte Reload
	leaq	(%rsi,%rax,2), %r10
	movq	%r10, %rax
	shrq	$51, %rax
	leaq	(%rax,%rdi,2), %rax
	movq	%rax, %rcx
	shrq	$51, %rcx
	movq	8(%rsp), %rdx                   # 8-byte Reload
	leaq	(%rcx,%rdx,2), %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	movq	16(%rsp), %rsi                  # 8-byte Reload
	leaq	(%rdx,%rsi,2), %rdx
	movq	%rdx, %rsi
	shrq	$51, %rsi
	leaq	(%rsi,%rsi,8), %rdi
	leaq	(%rsi,%rdi,2), %rdi
	addq	%r9, %rdi
	movq	%rdi, %rsi
	shrq	$51, %rsi
	andq	%r8, %r10
	addq	%rsi, %r10
	andq	%r8, %rdi
	movq	%r10, %rsi
	andq	%r8, %rsi
	shrq	$51, %r10
	andq	%r8, %rax
	addq	%r10, %rax
	andq	%r8, %rcx
	andq	%r8, %rdx
	movq	32(%rsp), %r8
	movq	40(%rsp), %r9
	leaq	(%r8,%rdi), %r10
	movq	48(%rsp), %r11
	movq	56(%rsp), %r14
	movq	64(%rsp), %r15
	movq	%r10, 80(%rbx)
	leaq	(%r9,%rsi), %r10
	movq	%r10, 88(%rbx)
	leaq	(%r11,%rax), %r10
	movq	%r10, 96(%rbx)
	leaq	(%r14,%rcx), %r10
	movq	%r10, 104(%rbx)
	leaq	(%r15,%rdx), %r10
	movq	%r10, 112(%rbx)
	addq	%r13, %rdi
	addq	$-36, %rdi
	subq	%r8, %rdi
	addq	%r13, %rsi
	subq	%r9, %rsi
	addq	%r13, %rax
	subq	%r11, %rax
	addq	%r13, %rcx
	subq	%r14, %rcx
	addq	%r13, %rdx
	subq	%r15, %rdx
	movq	%rdi, 120(%rbx)
	movq	%rsi, 128(%rbx)
	movq	%rax, 136(%rbx)
	movq	%rcx, 144(%rbx)
	movq	%rdx, 152(%rbx)
	addq	$152, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end12:
	.size	ge_madd, .Lfunc_end12-ge_madd
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function ge_p2_dbl
.LCPI13_0:
	.quad	2251799813685247                # 0x7ffffffffffff
	.quad	2251799813685247                # 0x7ffffffffffff
.LCPI13_1:
	.quad	4503599627370494                # 0xffffffffffffe
	.quad	4503599627370494                # 0xffffffffffffe
	.text
	.p2align	4, 0x90
	.type	ge_p2_dbl,@function
ge_p2_dbl:                              # @ge_p2_dbl
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$168, %rsp
	movq	%rsi, %r15
	movq	32(%rsi), %rcx
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rax
	imulq	$38, %rcx, %r8
	movq	%rcx, %r9
	movq	24(%rsi), %r11
	leaq	(%r11,%r11,8), %rcx
	leaq	(%r11,%rcx,2), %rcx
	imulq	$38, %r11, %r10
	mulq	%r9
	movq	%r9, %rsi
	movq	%r9, 152(%rsp)                  # 8-byte Spill
	movq	%rdx, -56(%rsp)                 # 8-byte Spill
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r8
	movq	%rdx, -80(%rsp)                 # 8-byte Spill
	movq	%rax, -120(%rsp)                # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r11
	movq	%r11, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	16(%r15), %rbx
	movq	%rbx, %rax
	mulq	%r8
	movq	%rdx, %rcx
	movq	%rax, %r9
	movq	%rbx, %rax
	mulq	%r10
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, -96(%rsp)                 # 8-byte Spill
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rbx, 72(%rsp)                  # 8-byte Spill
	movq	%rax, -128(%rsp)                # 8-byte Spill
	movq	%rdx, -72(%rsp)                 # 8-byte Spill
	movq	8(%r15), %r12
	movq	%r12, %rax
	mulq	%r8
	movq	%rax, %r8
	movq	%rdx, %r14
	leaq	(%r11,%r11), %r13
	movq	%r12, %rax
	mulq	%r13
	movq	%rax, 48(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	leaq	(%rbx,%rbx), %rbp
	movq	%r12, %rax
	mulq	%rbp
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%r12, %rax
	mulq	%r12
	movq	%r12, 144(%rsp)                 # 8-byte Spill
	movq	%rax, %r11
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	leaq	(%rsi,%rsi), %rdx
	movq	(%r15), %rsi
	movq	%rsi, 136(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	mulq	%rdx
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	%rax, -104(%rsp)                # 8-byte Spill
	movq	%rsi, %rax
	mulq	%r13
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%rax, %r13
	movq	%rsi, %rax
	mulq	%rbp
	movq	%rax, -88(%rsp)                 # 8-byte Spill
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	leaq	(%r12,%r12), %rdx
	movq	%rsi, %rax
	mulq	%rdx
	movq	%rdx, %rbp
	movq	%rax, 160(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	mulq	%rsi
	addq	-96(%rsp), %r8                  # 8-byte Folded Reload
	adcq	56(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r8
	adcq	%rdx, %r14
	shldq	$13, %r8, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r8
	movq	48(%rsp), %rsi                  # 8-byte Reload
	addq	-128(%rsp), %rsi                # 8-byte Folded Reload
	adcq	-72(%rsp), %r10                 # 8-byte Folded Reload
	addq	-104(%rsp), %rsi                # 8-byte Folded Reload
	adcq	32(%rsp), %r10                  # 8-byte Folded Reload
	addq	-64(%rsp), %rbx                 # 8-byte Folded Reload
	movq	104(%rsp), %r12                 # 8-byte Reload
	adcq	-56(%rsp), %r12                 # 8-byte Folded Reload
	addq	%r13, %rbx
	adcq	40(%rsp), %r12                  # 8-byte Folded Reload
	addq	-120(%rsp), %r11                # 8-byte Folded Reload
	movq	112(%rsp), %rax                 # 8-byte Reload
	adcq	-80(%rsp), %rax                 # 8-byte Folded Reload
	addq	-88(%rsp), %r11                 # 8-byte Folded Reload
	adcq	64(%rsp), %rax                  # 8-byte Folded Reload
	addq	88(%rsp), %r9                   # 8-byte Folded Reload
	adcq	96(%rsp), %rcx                  # 8-byte Folded Reload
	addq	160(%rsp), %r9                  # 8-byte Folded Reload
	adcq	%rbp, %rcx
	addq	%r14, %r9
	adcq	$0, %rcx
	shldq	$13, %r9, %rcx
	andq	%rdx, %r9
	addq	%r11, %rcx
	adcq	$0, %rax
	shldq	$13, %rcx, %rax
	andq	%rdx, %rcx
	addq	%rbx, %rax
	movq	%r12, %rbx
	adcq	$0, %rbx
	shldq	$13, %rax, %rbx
	andq	%rdx, %rax
	movq	%rdx, %r11
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	addq	%rsi, %rbx
	adcq	$0, %r10
	shldq	$13, %rbx, %r10
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %rax
	addq	%r8, %rax
	movq	%rax, -56(%rsp)                 # 8-byte Spill
	shrq	$51, %rax
	addq	%r9, %rax
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	movq	%rax, %rdx
	shrq	$51, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 160(%rsp)                 # 8-byte Spill
	movq	72(%r15), %rcx
	movq	%rcx, -120(%rsp)                # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rax
	mulq	%rcx
	movq	%rax, -96(%rsp)                 # 8-byte Spill
	movq	%rdx, -88(%rsp)                 # 8-byte Spill
	imulq	$38, %rcx, %r8
	movq	64(%r15), %rcx
	movq	%rcx, %rax
	mulq	%r8
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rax
	mulq	%rcx
	movq	%rcx, %r9
	movq	%rax, -128(%rsp)                # 8-byte Spill
	movq	%rdx, -72(%rsp)                 # 8-byte Spill
	movq	56(%r15), %rsi
	movq	%rsi, %rax
	mulq	%r8
	movq	%rax, %rbp
	movq	%rdx, %rcx
	andq	%r11, %rbx
	movq	%rbx, 104(%rsp)                 # 8-byte Spill
	imulq	$38, %r9, %rdx
	movq	%r9, 48(%rsp)                   # 8-byte Spill
	movq	%rsi, %rax
	mulq	%rdx
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, -8(%rsp)                  # 8-byte Spill
	movq	%rsi, %rax
	mulq	%rsi
	movq	%rsi, 40(%rsp)                  # 8-byte Spill
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	movq	48(%r15), %r11
	movq	%r15, %rbx
	movq	%r11, %rax
	mulq	%r8
	movq	%rax, %r8
	movq	%rdx, %r13
	addq	%r9, %r9
	movq	%r11, %rax
	mulq	%r9
	movq	%rax, -16(%rsp)                 # 8-byte Spill
	movq	%rdx, %r10
	leaq	(%rsi,%rsi), %r12
	movq	%r11, %rax
	mulq	%r12
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	movq	%rax, %rsi
	movq	%r11, %rax
	mulq	%r11
	movq	%r11, 32(%rsp)                  # 8-byte Spill
	movq	%rax, %r15
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	-120(%rsp), %rax                # 8-byte Reload
	leaq	(%rax,%rax), %rdx
	movq	40(%rbx), %r14
	movq	%r14, -80(%rsp)                 # 8-byte Spill
	movq	%r14, %rax
	mulq	%rdx
	movq	%rdx, -40(%rsp)                 # 8-byte Spill
	movq	%rax, -48(%rsp)                 # 8-byte Spill
	movq	%r14, %rax
	mulq	%r9
	movq	%rdx, -32(%rsp)                 # 8-byte Spill
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movq	%r14, %rax
	mulq	%r12
	movq	%rax, -24(%rsp)                 # 8-byte Spill
	movq	%rdx, (%rsp)                    # 8-byte Spill
	movabsq	$2251799813685247, %r9          # imm = 0x7FFFFFFFFFFFF
	andq	%r9, -56(%rsp)                  # 8-byte Folded Spill
	leaq	(%r11,%r11), %rdx
	movq	%r14, %rax
	mulq	%rdx
	movq	%rdx, %r12
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%r14, %rax
	mulq	%r14
	andq	%r9, -64(%rsp)                  # 8-byte Folded Spill
	addq	-8(%rsp), %r8                   # 8-byte Folded Reload
	adcq	8(%rsp), %r13                   # 8-byte Folded Reload
	addq	%rax, %r8
	adcq	%rdx, %r13
	shldq	$13, %r8, %r13
	andq	%r9, %r8
	movq	-16(%rsp), %rdx                 # 8-byte Reload
	addq	24(%rsp), %rdx                  # 8-byte Folded Reload
	adcq	-104(%rsp), %r10                # 8-byte Folded Reload
	addq	-48(%rsp), %rdx                 # 8-byte Folded Reload
	adcq	-40(%rsp), %r10                 # 8-byte Folded Reload
	addq	-96(%rsp), %rsi                 # 8-byte Folded Reload
	movq	88(%rsp), %r11                  # 8-byte Reload
	adcq	-88(%rsp), %r11                 # 8-byte Folded Reload
	addq	-112(%rsp), %rsi                # 8-byte Folded Reload
	adcq	-32(%rsp), %r11                 # 8-byte Folded Reload
	addq	56(%rsp), %r15                  # 8-byte Folded Reload
	movq	96(%rsp), %rax                  # 8-byte Reload
	adcq	64(%rsp), %rax                  # 8-byte Folded Reload
	addq	-24(%rsp), %r15                 # 8-byte Folded Reload
	adcq	(%rsp), %rax                    # 8-byte Folded Reload
	addq	-128(%rsp), %rbp                # 8-byte Folded Reload
	adcq	-72(%rsp), %rcx                 # 8-byte Folded Reload
	addq	16(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	%r12, %rcx
	addq	%r13, %rbp
	adcq	$0, %rcx
	shldq	$13, %rbp, %rcx
	andq	%r9, %rbp
	addq	%r15, %rcx
	adcq	$0, %rax
	shldq	$13, %rcx, %rax
	andq	%r9, %rcx
	addq	%rsi, %rax
	movq	%r11, %rsi
	adcq	$0, %rsi
	shldq	$13, %rax, %rsi
	andq	%r9, %rax
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	addq	%rdx, %rsi
	adcq	$0, %r10
	shldq	$13, %rsi, %r10
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %rdx
	addq	%r8, %rdx
	movq	%rdx, %rax
	movq	%rdx, %r8
	shrq	$51, %rax
	addq	%rbp, %rax
	movq	%rax, -72(%rsp)                 # 8-byte Spill
	movq	%rax, %rdx
	shrq	$51, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	andq	%r9, %rsi
	movq	%rsi, 88(%rsp)                  # 8-byte Spill
	andq	%r9, %r8
	movq	%r8, 56(%rsp)                   # 8-byte Spill
	movq	%rbx, %rsi
	movq	112(%rbx), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	mulq	%rbx
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%rdx, 24(%rsp)                  # 8-byte Spill
	movq	104(%rsi), %r10
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %rcx
	imulq	$38, %rbx, %r14
	movq	%r10, %rax
	mulq	%r14
	movq	%rdx, -96(%rsp)                 # 8-byte Spill
	movq	%rax, -104(%rsp)                # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, -88(%rsp)                 # 8-byte Spill
	movq	%rdx, -128(%rsp)                # 8-byte Spill
	movq	96(%rsi), %r11
	movq	%r11, %rax
	mulq	%r14
	movq	%rax, %r8
	movq	%rdx, %rcx
	imulq	$38, %r10, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, -24(%rsp)                 # 8-byte Spill
	movq	%rdx, -16(%rsp)                 # 8-byte Spill
	addq	%r10, %r10
	movq	%r10, -112(%rsp)                # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, (%rsp)                    # 8-byte Spill
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	88(%rsi), %r9
	movq	%r9, %rax
	mulq	%r14
	movq	%rax, %r14
	movq	%rdx, %r15
	leaq	(%r11,%r11), %r13
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %r11
	movq	%rax, -32(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r13
	movq	%rdx, %rbp
	movq	%rax, -8(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, %r12
	movq	%rdx, %r10
	addq	%rbx, %rbx
	movq	80(%rsi), %rsi
	movq	%rsi, %rax
	mulq	%rbx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	mulq	-112(%rsp)                      # 8-byte Folded Reload
	movq	%rax, -48(%rsp)                 # 8-byte Spill
	movq	%rdx, -40(%rsp)                 # 8-byte Spill
	addq	%r9, %r9
	movq	%rsi, %rax
	mulq	%r13
	movq	%rdx, %r13
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movq	%rsi, %rax
	mulq	%r9
	movq	%rdx, %r9
	movq	%rax, %rbx
	movq	%rsi, %rax
	mulq	%rsi
	movabsq	$2251799813685247, %rsi         # imm = 0x7FFFFFFFFFFFF
	andq	%rsi, -72(%rsp)                 # 8-byte Folded Spill
	addq	-24(%rsp), %r14                 # 8-byte Folded Reload
	adcq	-16(%rsp), %r15                 # 8-byte Folded Reload
	addq	%rax, %r14
	adcq	%rdx, %r15
	shldq	$13, %r14, %r15
	andq	%rsi, %r14
	movq	%rsi, %rax
	movq	-32(%rsp), %rdx                 # 8-byte Reload
	addq	(%rsp), %rdx                    # 8-byte Folded Reload
	adcq	8(%rsp), %r11                   # 8-byte Folded Reload
	addq	120(%rsp), %rdx                 # 8-byte Folded Reload
	adcq	128(%rsp), %r11                 # 8-byte Folded Reload
	movq	-8(%rsp), %rsi                  # 8-byte Reload
	addq	16(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	24(%rsp), %rbp                  # 8-byte Folded Reload
	addq	-48(%rsp), %rsi                 # 8-byte Folded Reload
	adcq	-40(%rsp), %rbp                 # 8-byte Folded Reload
	addq	-104(%rsp), %r12                # 8-byte Folded Reload
	adcq	-96(%rsp), %r10                 # 8-byte Folded Reload
	addq	-112(%rsp), %r12                # 8-byte Folded Reload
	adcq	%r13, %r10
	addq	-88(%rsp), %r8                  # 8-byte Folded Reload
	adcq	-128(%rsp), %rcx                # 8-byte Folded Reload
	addq	%rbx, %r8
	adcq	%r9, %rcx
	addq	%r15, %r8
	adcq	$0, %rcx
	shldq	$13, %r8, %rcx
	movq	%rax, %r15
	andq	%rax, %r8
	addq	%r12, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rax, %rcx
	addq	%rsi, %r10
	adcq	$0, %rbp
	shldq	$13, %r10, %rbp
	addq	%rdx, %rbp
	adcq	$0, %r11
	shldq	$13, %rbp, %r11
	leaq	(%r11,%r11,8), %rax
	leaq	(%r11,%rax,2), %rax
	addq	%r14, %rax
	movq	%rax, %rsi
	shrq	$51, %rsi
	addq	%r8, %rsi
	movq	%rsi, %rdx
	movq	%rsi, -88(%rsp)                 # 8-byte Spill
	shrq	$51, %rdx
	addq	%rcx, %rdx
	andq	%r15, %r10
	leaq	(%rdx,%rdx), %r8
	shrq	$50, %rdx
	leaq	(%rdx,%r10,2), %rdx
	andq	%r15, %rbp
	movq	%rdx, %rcx
	movq	%rdx, %r9
	shrq	$51, %rcx
	leaq	(%rcx,%rbp,2), %rcx
	movq	%rcx, -128(%rsp)                # 8-byte Spill
	shrq	$51, %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %r10
	leaq	(%rax,%rax), %rcx
	leaq	-1(%r15), %rdx
	andq	%rdx, %rcx
	addq	%rcx, %r10
	shrq	$50, %rax
	andl	$1, %eax
	leaq	(%rsi,%rsi), %rcx
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movq	%r10, %rax
	shrq	$51, %rax
	addq	%rax, %rcx
	andq	%rdx, %r8
	movq	%r8, -96(%rsp)                  # 8-byte Spill
	movq	-80(%rsp), %rax                 # 8-byte Reload
	addq	136(%rsp), %rax                 # 8-byte Folded Reload
	movq	%rax, -80(%rsp)                 # 8-byte Spill
	movq	32(%rsp), %r14                  # 8-byte Reload
	addq	144(%rsp), %r14                 # 8-byte Folded Reload
	movq	40(%rsp), %rbx                  # 8-byte Reload
	addq	72(%rsp), %rbx                  # 8-byte Folded Reload
	movq	48(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	movq	-120(%rsp), %r11                # 8-byte Reload
	addq	152(%rsp), %r11                 # 8-byte Folded Reload
	movq	%r15, %rax
	andq	%r15, %r10
	movq	%r10, 48(%rsp)                  # 8-byte Spill
	movq	%rcx, %rdx
	andq	%rax, %rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	shrq	$51, %rcx
	movq	%rcx, 144(%rsp)                 # 8-byte Spill
	andq	%rax, %r9
	movq	%r9, 152(%rsp)                  # 8-byte Spill
	leaq	(%r11,%r11,8), %rax
	leaq	(%r11,%rax,2), %rax
	leaq	(%rsi,%rsi,8), %rcx
	leaq	(%rsi,%rcx,2), %rcx
	mulq	%r11
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	imulq	$38, %r11, %r10
	movq	%rsi, %rax
	mulq	%r10
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%rsi
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	imulq	$38, %rsi, %rcx
	leaq	(%rsi,%rsi), %r8
	movq	%rbx, %rax
	mulq	%r10
	movq	%rdx, %rsi
	movq	%rax, %rbp
	movq	%rbx, %rax
	mulq	%rcx
	movq	%rdx, -24(%rsp)                 # 8-byte Spill
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, -8(%rsp)                  # 8-byte Spill
	movq	%rdx, (%rsp)                    # 8-byte Spill
	leaq	(%rbx,%rbx), %r9
	movq	%r14, %rcx
	movq	%r14, %rax
	mulq	%r10
	movq	%rdx, %r15
	movq	%rax, -120(%rsp)                # 8-byte Spill
	movq	%r14, %rax
	mulq	%r8
	movq	%rdx, %r13
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%r14, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%r11, %r11
	movq	%r14, %rax
	mulq	%r14
	movq	%rdx, %r14
	movq	%rax, %r12
	movq	-80(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %rax
	mulq	%r11
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, %r11
	movq	%rbx, %rax
	mulq	%r8
	movq	%rax, 128(%rsp)                 # 8-byte Spill
	movq	%rdx, -48(%rsp)                 # 8-byte Spill
	leaq	(%rcx,%rcx), %r8
	movq	%rbx, %rax
	mulq	%r9
	movq	%rdx, -32(%rsp)                 # 8-byte Spill
	movq	%rax, -40(%rsp)                 # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r8
	movq	%rdx, %r9
	movq	%rax, -16(%rsp)                 # 8-byte Spill
	movq	%rbx, %rax
	mulq	%rbx
	movabsq	$2251799813685247, %rcx         # imm = 0x7FFFFFFFFFFFF
	andq	%rcx, -128(%rsp)                # 8-byte Folded Spill
	movq	-120(%rsp), %r8                 # 8-byte Reload
	addq	-112(%rsp), %r8                 # 8-byte Folded Reload
	adcq	-24(%rsp), %r15                 # 8-byte Folded Reload
	addq	%rax, %r8
	adcq	%rdx, %r15
	shldq	$13, %r8, %r15
	andq	%rcx, %r8
	movq	%r8, -120(%rsp)                 # 8-byte Spill
	movq	80(%rsp), %rax                  # 8-byte Reload
	addq	-8(%rsp), %rax                  # 8-byte Folded Reload
	adcq	(%rsp), %r13                    # 8-byte Folded Reload
	addq	%r11, %rax
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	adcq	120(%rsp), %r13                 # 8-byte Folded Reload
	movq	72(%rsp), %rax                  # 8-byte Reload
	addq	8(%rsp), %rax                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	128(%rsp), %rax                 # 8-byte Folded Reload
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	adcq	-48(%rsp), %r10                 # 8-byte Folded Reload
	addq	24(%rsp), %r12                  # 8-byte Folded Reload
	adcq	-104(%rsp), %r14                # 8-byte Folded Reload
	addq	-40(%rsp), %r12                 # 8-byte Folded Reload
	adcq	-32(%rsp), %r14                 # 8-byte Folded Reload
	addq	32(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	40(%rsp), %rsi                  # 8-byte Folded Reload
	addq	-16(%rsp), %rbp                 # 8-byte Folded Reload
	adcq	%r9, %rsi
	addq	%r15, %rbp
	adcq	$0, %rsi
	shldq	$13, %rbp, %rsi
	andq	%rcx, %rbp
	addq	%r12, %rsi
	adcq	$0, %r14
	movabsq	$4503599627370494, %rax         # imm = 0xFFFFFFFFFFFFE
	leaq	-36(%rax), %r8
	movq	-56(%rsp), %rcx                 # 8-byte Reload
	subq	%rcx, %r8
	movq	56(%rsp), %rdx                  # 8-byte Reload
	leaq	(%rdx,%rcx), %r15
	addq	%rdx, %r8
	movq	%rax, %r9
	movq	-64(%rsp), %rcx                 # 8-byte Reload
	subq	%rcx, %r9
	movq	-72(%rsp), %rdx                 # 8-byte Reload
	addq	%rdx, %rcx
	addq	%rdx, %r9
	movq	%r9, -80(%rsp)                  # 8-byte Spill
	movq	%rax, %r11
	movq	160(%rsp), %rdx                 # 8-byte Reload
	subq	%rdx, %r11
	movq	64(%rsp), %r9                   # 8-byte Reload
	leaq	(%r9,%rdx), %rbx
	addq	%r9, %r11
	movq	%r11, -56(%rsp)                 # 8-byte Spill
	movq	%rax, %r9
	movq	112(%rsp), %rdx                 # 8-byte Reload
	subq	%rdx, %r9
	movq	96(%rsp), %r11                  # 8-byte Reload
	addq	%r11, %rdx
	movq	%rdx, -64(%rsp)                 # 8-byte Spill
	addq	%r11, %r9
	movq	%r9, 112(%rsp)                  # 8-byte Spill
	movq	%rax, %r12
	movq	104(%rsp), %rdx                 # 8-byte Reload
	subq	%rdx, %r12
	movq	88(%rsp), %r11                  # 8-byte Reload
	addq	%r11, %rdx
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	addq	%r11, %r12
	movq	%r15, 40(%rdi)
	movq	%rcx, 48(%rdi)
	movq	%r15, %r11
	shrq	$51, %r11
	addq	%rcx, %r11
	shldq	$13, %rsi, %r14
	movabsq	$2251799813685247, %r9          # imm = 0x7FFFFFFFFFFFF
	andq	%r9, %rsi
	movq	%r11, %rcx
	shrq	$51, %rcx
	addq	%rbx, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	andq	%r9, %r15
	andq	%r9, %r11
	andq	%r9, %rcx
	addq	72(%rsp), %r14                  # 8-byte Folded Reload
	adcq	$0, %r10
	movq	%rbx, 56(%rdi)
	shldq	$13, %r14, %r10
	addq	80(%rsp), %r10                  # 8-byte Folded Reload
	adcq	$0, %r13
	shldq	$13, %r10, %r13
	leaq	(,%r13,8), %rbx
	addq	%r13, %rbx
	leaq	(%r13,%rbx,2), %r13
	addq	-120(%rsp), %r13                # 8-byte Folded Reload
	movq	%r13, %rbx
	shrq	$51, %rbx
	addq	%rbp, %rbx
	movq	%r10, %xmm1
	movq	%r14, %xmm0
	movq	-64(%rsp), %r9                  # 8-byte Reload
	movq	%r9, 64(%rdi)
	addq	%r9, %rdx
	movq	104(%rsp), %r9                  # 8-byte Reload
	movq	%r9, 72(%rdi)
	movq	%rdx, %xmm2
	shrq	$51, %rdx
	addq	%r9, %rdx
	movq	%rdx, %xmm3
	shrq	$51, %rdx
	leaq	(%rdx,%rdx,8), %r9
	leaq	(%rdx,%r9,2), %rdx
	addq	%r15, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	addq	%r11, %r9
	movq	%r9, %r10
	shrq	$51, %r9
	addq	%rcx, %r9
	movabsq	$2251799813685247, %r14         # imm = 0x7FFFFFFFFFFFF
	andq	%r14, %r13
	andq	%r14, %rdx
	leaq	(%rax,%r13), %rcx
	addq	$-36, %rcx
	subq	%rdx, %rcx
	movq	%rbx, %rdx
	andq	%r14, %rbx
	andq	%r14, %r10
	addq	%rax, %rbx
	subq	%r10, %rbx
	shrq	$51, %rdx
	addq	%rax, %rsi
	addq	%rdx, %rsi
	subq	%r9, %rsi
	movq	%r8, 80(%rdi)
	movq	-80(%rsp), %rdx                 # 8-byte Reload
	movq	%rdx, 88(%rdi)
	movq	-56(%rsp), %r10                 # 8-byte Reload
	movq	%r10, 96(%rdi)
	movq	112(%rsp), %r11                 # 8-byte Reload
	movq	%r11, 104(%rdi)
	movq	%r12, 112(%rdi)
	movq	%rcx, (%rdi)
	movq	%rbx, 8(%rdi)
	movq	%rsi, 16(%rdi)
	movq	%r8, %rcx
	shrq	$51, %rcx
	addq	%rdx, %rcx
	movq	%rcx, %r9
	shrq	$51, %r9
	addq	%r10, %r9
	movq	%r9, %rdx
	shrq	$51, %rdx
	addq	%r11, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rsi
	addq	%r12, %rsi
	movq	%rsi, %r10
	shrq	$51, %r10
	leaq	(%r10,%r10,8), %r11
	leaq	(%r10,%r11,2), %r10
	andq	%r14, %r8
	addq	%r8, %r10
	movq	%r10, %r8
	shrq	$51, %r8
	andq	%r14, %rcx
	addq	%r8, %rcx
	andq	%r14, %r10
	movq	%rcx, %r8
	andq	%r14, %r8
	andq	%r14, %r9
	andq	%r14, %rdx
	andq	%r14, %rsi
	negq	%r10
	addq	%rax, %r10
	addq	$-36, %r10
	addq	48(%rsp), %r10                  # 8-byte Folded Reload
	movq	%rax, %r11
	subq	%r8, %r11
	addq	136(%rsp), %r11                 # 8-byte Folded Reload
	movq	%rax, %r8
	subq	%r9, %r8
	shrq	$51, %rcx
	btq	$50, -88(%rsp)                  # 8-byte Folded Reload
	adcq	-96(%rsp), %r8                  # 8-byte Folded Reload
	subq	%rcx, %r8
	addq	144(%rsp), %r8                  # 8-byte Folded Reload
	movq	%rax, %rcx
	subq	%rdx, %rcx
	addq	152(%rsp), %rcx                 # 8-byte Folded Reload
	subq	%rsi, %rax
	addq	-128(%rsp), %rax                # 8-byte Folded Reload
	punpcklqdq	%xmm1, %xmm0            # xmm0 = xmm0[0],xmm1[0]
	movdqa	.LCPI13_0(%rip), %xmm1          # xmm1 = [2251799813685247,2251799813685247]
	pand	%xmm1, %xmm0
	punpcklqdq	%xmm3, %xmm2            # xmm2 = xmm2[0],xmm3[0]
	pand	%xmm1, %xmm2
	psubq	%xmm2, %xmm0
	paddq	.LCPI13_1(%rip), %xmm0
	movdqu	%xmm0, 24(%rdi)
	movq	%r10, 120(%rdi)
	movq	%r11, 128(%rdi)
	movq	%r8, 136(%rdi)
	movq	%rcx, 144(%rdi)
	movq	%rax, 152(%rdi)
	addq	$168, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end13:
	.size	ge_p2_dbl, .Lfunc_end13-ge_p2_dbl
                                        # -- End function
	.globl	ED25519_sign                    # -- Begin function ED25519_sign
	.p2align	4, 0x90
	.type	ED25519_sign,@function
ED25519_sign:                           # @ED25519_sign
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$584, %rsp                      # imm = 0x248
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %rbx
	leaq	16(%rsp), %rdx
	movl	$32, %esi
	movq	%rcx, %rdi
	callq	SHA512
	andb	$-8, 16(%rsp)
	movzbl	47(%rsp), %eax
	andb	$63, %al
	orb	$64, %al
	movb	%al, 47(%rsp)
	leaq	368(%rsp), %rbp
	movq	%rbp, %rdi
	callq	SHA512_Init
	leaq	48(%rsp), %rsi
	movl	$32, %edx
	movq	%rbp, %rdi
	callq	SHA512_Update
	movq	%rbp, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	SHA512_Update
	leaq	144(%rsp), %r14
	movq	%r14, %rdi
	movq	%rbp, %rsi
	callq	SHA512_Final
	movq	%r14, %rdi
	callq	x25519_sc_reduce
	leaq	208(%rsp), %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	x25519_ge_scalarmult_base
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	ge_p3_tobytes
	movq	%rbp, %rdi
	callq	SHA512_Init
	movl	$32, %edx
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	callq	SHA512_Update
	addq	$32, %r13
	movl	$32, %edx
	movq	%rbp, %rdi
	movq	%r13, %rsi
	callq	SHA512_Update
	movq	%rbp, %rdi
	movq	%r12, %rsi
	movq	8(%rsp), %rdx                   # 8-byte Reload
	callq	SHA512_Update
	leaq	80(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbp, %rsi
	callq	SHA512_Final
	movq	%r15, %rdi
	callq	x25519_sc_reduce
	addq	$32, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	leaq	16(%rsp), %rdx
	movq	%r14, %rcx
	callq	sc_muladd
	movl	$1, %eax
	addq	$584, %rsp                      # imm = 0x248
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end14:
	.size	ED25519_sign, .Lfunc_end14-ED25519_sign
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function ge_p3_tobytes
.LCPI15_0:
	.quad	2251799813685247                # 0x7ffffffffffff
	.quad	2251799813685247                # 0x7ffffffffffff
	.text
	.p2align	4, 0x90
	.type	ge_p3_tobytes,@function
ge_p3_tobytes:                          # @ge_p3_tobytes
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$440, %rsp                      # imm = 0x1B8
	movq	112(%rsi), %r15
	movq	%r15, 384(%rsp)
	movups	80(%rsi), %xmm0
	movq	%rsi, 344(%rsp)                 # 8-byte Spill
	movups	96(%rsi), %xmm1
	movaps	%xmm1, 368(%rsp)
	movaps	%xmm0, 352(%rsp)
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %rax
	imulq	$38, %r15, %r9
	movq	368(%rsp), %rcx
	movq	376(%rsp), %rsi
	leaq	(%rsi,%rsi,8), %rdx
	leaq	(%rsi,%rdx,2), %r8
	mulq	%r15
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rsi, %rax
	mulq	%r9
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	imulq	$38, %rsi, %r10
	movq	%r8, %rax
	mulq	%rsi
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r9
	movq	%rdx, %r11
	movq	%rax, %r8
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	addq	%rsi, %rsi
	movq	%rcx, %rax
	mulq	%rcx
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	360(%rsp), %r14
	movq	%r14, %rax
	mulq	%r9
	movq	%rax, %r9
	movq	%rdx, %r13
	leaq	(%rcx,%rcx), %rbx
	movq	%r14, %rax
	mulq	%rsi
	movq	%rdx, %r10
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%r14, %rax
	mulq	%rbx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, %r12
	movq	%r14, %rax
	mulq	%r14
	movq	%rax, %rbp
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	leaq	(%r15,%r15), %rdx
	movq	352(%rsp), %rcx
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 184(%rsp)                 # 8-byte Spill
	movq	%rax, %r15
	movq	%rcx, %rax
	mulq	%rsi
	movq	%rax, 136(%rsp)                 # 8-byte Spill
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	leaq	(%r14,%r14), %rsi
	movq	%rcx, %rax
	mulq	%rbx
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%rsi
	movq	%rdx, %rsi
	movq	%rax, %r14
	movq	%rcx, %rax
	mulq	%rcx
	addq	32(%rsp), %r9                   # 8-byte Folded Reload
	adcq	40(%rsp), %r13                  # 8-byte Folded Reload
	addq	%rax, %r9
	adcq	%rdx, %r13
	shldq	$13, %r9, %r13
	movabsq	$2251799813685247, %rax         # imm = 0x7FFFFFFFFFFFF
	andq	%rax, %r9
	movq	16(%rsp), %rbx                  # 8-byte Reload
	addq	56(%rsp), %rbx                  # 8-byte Folded Reload
	adcq	64(%rsp), %r10                  # 8-byte Folded Reload
	addq	%r15, %rbx
	adcq	184(%rsp), %r10                 # 8-byte Folded Reload
	addq	72(%rsp), %r12                  # 8-byte Folded Reload
	movq	120(%rsp), %rcx                 # 8-byte Reload
	adcq	80(%rsp), %rcx                  # 8-byte Folded Reload
	addq	136(%rsp), %r12                 # 8-byte Folded Reload
	adcq	8(%rsp), %rcx                   # 8-byte Folded Reload
	addq	88(%rsp), %rbp                  # 8-byte Folded Reload
	movq	128(%rsp), %rdx                 # 8-byte Reload
	adcq	96(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	48(%rsp), %rdx                  # 8-byte Folded Reload
	addq	104(%rsp), %r8                  # 8-byte Folded Reload
	adcq	112(%rsp), %r11                 # 8-byte Folded Reload
	addq	%r14, %r8
	adcq	%rsi, %r11
	addq	%r13, %r8
	adcq	$0, %r11
	shldq	$13, %r8, %r11
	andq	%rax, %r8
	addq	%rbp, %r11
	adcq	$0, %rdx
	shldq	$13, %r11, %rdx
	andq	%rax, %r11
	addq	%r12, %rdx
	adcq	$0, %rcx
	shldq	$13, %rdx, %rcx
	andq	%rax, %rdx
	movq	%rdx, %rsi
	addq	%rbx, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rax, %rcx
	movq	%rax, %r15
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %r14
	addq	%r9, %r14
	movq	%r14, %r9
	shrq	$51, %r9
	addq	%r8, %r9
	movq	%r9, %r10
	shrq	$51, %r10
	addq	%r11, %r10
	movq	%rcx, %r8
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %rcx
	movq	%r8, %rax
	movq	%rsi, %r8
	leaq	(%rsi,%rsi,8), %rsi
	movq	%rax, %rbx
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	mulq	%rdx
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	leaq	(%r8,%rsi,2), %rsi
	movq	%r8, %rax
	mulq	%rcx
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r8, %rax
	mulq	%rsi
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%rcx
	movq	%rax, %r12
	movq	%rdx, %r11
	imulq	$38, %r8, %rdx
	movq	%r8, 128(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, 184(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	mulq	%r10
	movq	%r10, 112(%rsp)                 # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	andq	%r15, %r9
	movq	%r9, %rax
	mulq	%rcx
	movq	%rax, 248(%rsp)                 # 8-byte Spill
	movq	%rdx, 256(%rsp)                 # 8-byte Spill
	leaq	(%r8,%r8), %rsi
	movq	%r9, %rax
	mulq	%rsi
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	leaq	(%r10,%r10), %rcx
	movq	%rcx, %rax
	mulq	%r9
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, %r10
	movq	%rdx, %r8
	andq	%r15, %r14
	movq	%r14, 104(%rsp)                 # 8-byte Spill
	leaq	(%rbx,%rbx), %rdx
	movq	%r14, %rax
	mulq	%rdx
	movq	%rdx, %rbp
	movq	%rax, %r15
	movq	%r14, %rax
	mulq	%rsi
	movq	%rdx, %rbx
	movq	%rax, %r13
	movq	%rcx, %rax
	mulq	%r14
	movq	%rax, %rcx
	movq	%rdx, %rsi
	leaq	(%r9,%r9), %rax
	mulq	%r14
	movq	%rdx, 240(%rsp)                 # 8-byte Spill
	movq	%rax, 232(%rsp)                 # 8-byte Spill
	movq	%r14, %rax
	mulq	%r14
	addq	248(%rsp), %rax                 # 8-byte Folded Reload
	adcq	256(%rsp), %rdx                 # 8-byte Folded Reload
	addq	184(%rsp), %rax                 # 8-byte Folded Reload
	adcq	136(%rsp), %rdx                 # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r14         # imm = 0x7FFFFFFFFFFFF
	andq	%r14, %rax
	addq	8(%rsp), %r10                   # 8-byte Folded Reload
	adcq	16(%rsp), %r8                   # 8-byte Folded Reload
	addq	%rcx, %r10
	adcq	%rsi, %r8
	addq	24(%rsp), %r12                  # 8-byte Folded Reload
	adcq	32(%rsp), %r11                  # 8-byte Folded Reload
	addq	232(%rsp), %r12                 # 8-byte Folded Reload
	adcq	240(%rsp), %r11                 # 8-byte Folded Reload
	addq	%rdx, %r12
	adcq	$0, %r11
	movq	%r11, %rcx
	shrq	$51, %rcx
	shldq	$13, %r12, %r11
	andq	%r14, %r12
	addq	%r10, %r11
	adcq	%r8, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	shldq	$13, %r11, %rcx
	andq	%r14, %r11
	addq	72(%rsp), %r13                  # 8-byte Folded Reload
	adcq	80(%rsp), %rbx                  # 8-byte Folded Reload
	addq	40(%rsp), %r13                  # 8-byte Folded Reload
	adcq	48(%rsp), %rbx                  # 8-byte Folded Reload
	addq	%rcx, %r13
	adcq	%rdx, %rbx
	movq	%rbx, %rcx
	shrq	$51, %rcx
	shldq	$13, %r13, %rbx
	andq	%r14, %r13
	addq	56(%rsp), %r15                  # 8-byte Folded Reload
	adcq	64(%rsp), %rbp                  # 8-byte Folded Reload
	addq	88(%rsp), %r15                  # 8-byte Folded Reload
	adcq	96(%rsp), %rbp                  # 8-byte Folded Reload
	addq	%rbx, %r15
	adcq	%rcx, %rbp
	shldq	$13, %r15, %rbp
	andq	%r14, %r15
	leaq	(,%rbp,8), %rcx
	addq	%rbp, %rcx
	leaq	(,%rcx,2), %r8
	addq	%rbp, %r8
	addq	%rax, %r8
	movq	%r8, %rbp
	shrq	$51, %rbp
	addq	%r12, %rbp
	andq	%r14, %r8
	movq	%rbp, %rcx
	shrq	$51, %rcx
	addq	%r11, %rcx
	andq	%r14, %rbp
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %rdx
	movq	%r15, %rax
	mulq	%rdx
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	leaq	(,%r13,8), %rax
	addq	%r13, %rax
	leaq	(,%rax,2), %rsi
	addq	%r13, %rsi
	imulq	$38, %r15, %r10
	movq	%r13, %rax
	mulq	%r10
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r13, %rax
	mulq	%rsi
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	leaq	(%r15,%r15), %rbx
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, %r15
	movq	%rdx, %r11
	imulq	$38, %r13, %rdx
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rax, 184(%rsp)                 # 8-byte Spill
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%r13, %r14
	addq	%r13, %r14
	movq	%rcx, %rax
	mulq	%rcx
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r10
	movq	%rdx, 256(%rsp)                 # 8-byte Spill
	movq	%rax, 248(%rsp)                 # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r14
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %rsi
	movq	%rsi, %rax
	mulq	%rbp
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%rbp
	movq	%rdx, %r10
	movq	%rax, %rcx
	movq	%r8, %rax
	mulq	%rbx
	movq	%rax, %r13
	movq	%rdx, %rbx
	addq	%rbp, %rbp
	movq	%r8, %rax
	mulq	%r14
	movq	%rdx, %r12
	movq	%rax, %r14
	movq	%rsi, %rax
	mulq	%r8
	movq	%rdx, 232(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%rbp, %rax
	mulq	%r8
	movq	%rdx, 240(%rsp)                 # 8-byte Spill
	movq	%rax, %rbp
	movq	%r8, %rax
	mulq	%r8
	addq	248(%rsp), %rax                 # 8-byte Folded Reload
	adcq	256(%rsp), %rdx                 # 8-byte Folded Reload
	addq	184(%rsp), %rax                 # 8-byte Folded Reload
	adcq	136(%rsp), %rdx                 # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r8          # imm = 0x7FFFFFFFFFFFF
	andq	%r8, %rax
	addq	8(%rsp), %rcx                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rsi, %rcx
	adcq	232(%rsp), %r10                 # 8-byte Folded Reload
	movq	%rdi, 184(%rsp)                 # 8-byte Spill
	addq	24(%rsp), %r15                  # 8-byte Folded Reload
	adcq	32(%rsp), %r11                  # 8-byte Folded Reload
	movq	104(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, 264(%rsp)
	addq	%rbp, %r15
	adcq	240(%rsp), %r11                 # 8-byte Folded Reload
	movq	%r9, 272(%rsp)
	addq	%rdx, %r15
	adcq	$0, %r11
	movq	%r11, %rdx
	shrq	$51, %rdx
	shldq	$13, %r15, %r11
	andq	%r8, %r15
	addq	%rcx, %r11
	adcq	%r10, %rdx
	movq	%rdx, %rcx
	shrq	$51, %rcx
	shldq	$13, %r11, %rdx
	andq	%r8, %r11
	addq	88(%rsp), %r14                  # 8-byte Folded Reload
	adcq	96(%rsp), %r12                  # 8-byte Folded Reload
	movq	112(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, 280(%rsp)
	addq	40(%rsp), %r14                  # 8-byte Folded Reload
	adcq	48(%rsp), %r12                  # 8-byte Folded Reload
	movq	128(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, 288(%rsp)
	addq	56(%rsp), %r13                  # 8-byte Folded Reload
	adcq	64(%rsp), %rbx                  # 8-byte Folded Reload
	movq	120(%rsp), %rsi                 # 8-byte Reload
	movq	%rsi, 296(%rsp)
	addq	72(%rsp), %r13                  # 8-byte Folded Reload
	adcq	80(%rsp), %rbx                  # 8-byte Folded Reload
	addq	%rdx, %r14
	adcq	%rcx, %r12
	movq	%r12, %rcx
	shrq	$51, %rcx
	shldq	$13, %r14, %r12
	addq	%r13, %r12
	adcq	%rbx, %rcx
	shldq	$13, %r12, %rcx
	movq	%r12, %xmm0
	movq	%r14, %xmm1
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rcx
	addq	%rax, %rcx
	movq	%rcx, %rax
	shrq	$51, %rax
	addq	%r15, %rax
	movq	%rax, %rdx
	shrq	$51, %rdx
	addq	%r11, %rdx
	andq	%r8, %rcx
	movq	%rcx, 192(%rsp)
	andq	%r8, %rax
	movq	%rax, 200(%rsp)
	movq	%rdx, 208(%rsp)
	punpcklqdq	%xmm0, %xmm1            # xmm1 = xmm1[0],xmm0[0]
	pand	.LCPI15_0(%rip), %xmm1
	movdqu	%xmm1, 216(%rsp)
	leaq	352(%rsp), %rsi
	leaq	192(%rsp), %rbx
	movq	%rbx, %rdi
	movq	%rbx, %rdx
	callq	fe_mul_impl
	leaq	264(%rsp), %rdi
	movq	%rdi, %rsi
	movq	%rbx, %rdx
	callq	fe_mul_impl
	movq	296(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r9
	movq	288(%rsp), %rdi
	leaq	(%rdi,%rdi,8), %rcx
	leaq	(%rdi,%rcx,2), %rcx
	imulq	$38, %rdi, %r10
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r9
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	280(%rsp), %r8
	movq	%rcx, %rax
	mulq	%rdi
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r8, %rax
	mulq	%r9
	movq	%rdx, %rcx
	movq	%rax, %rsi
	movq	%r8, %rax
	mulq	%r10
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rdi), %r12
	movq	%r8, %rax
	mulq	%r8
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	272(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%r9
	movq	%rax, %rdi
	movq	%rdx, %r14
	leaq	(%r8,%r8), %r10
	movq	%rbp, %rax
	mulq	%r12
	movq	%rdx, %r8
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r10
	movq	%rdx, %r9
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%rbp
	movq	%rax, %r13
	movq	%rdx, %r11
	leaq	(%rbx,%rbx), %rdx
	movq	264(%rsp), %r15
	movq	%r15, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rbp, %r12
	addq	%rbp, %r12
	movq	%r15, %rax
	mulq	%r10
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %rbp
	movq	%r15, %rax
	mulq	%r15
	addq	40(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %rdi
	adcq	%rdx, %r14
	shldq	$13, %rdi, %r14
	movabsq	$2251799813685247, %r10         # imm = 0x7FFFFFFFFFFFF
	andq	%r10, %rdi
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r8                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r8                  # 8-byte Folded Reload
	movq	64(%rsp), %rdx                  # 8-byte Reload
	addq	88(%rsp), %rdx                  # 8-byte Folded Reload
	adcq	96(%rsp), %r9                   # 8-byte Folded Reload
	addq	8(%rsp), %rdx                   # 8-byte Folded Reload
	adcq	16(%rsp), %r9                   # 8-byte Folded Reload
	addq	104(%rsp), %r13                 # 8-byte Folded Reload
	adcq	112(%rsp), %r11                 # 8-byte Folded Reload
	addq	32(%rsp), %r13                  # 8-byte Folded Reload
	adcq	56(%rsp), %r11                  # 8-byte Folded Reload
	addq	120(%rsp), %rsi                 # 8-byte Folded Reload
	adcq	128(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbp, %rsi
	adcq	%r12, %rcx
	addq	%r14, %rsi
	adcq	$0, %rcx
	shldq	$13, %rsi, %rcx
	andq	%r10, %rsi
	addq	%r13, %rcx
	adcq	$0, %r11
	shldq	$13, %rcx, %r11
	andq	%r10, %rcx
	addq	%rdx, %r11
	adcq	$0, %r9
	shldq	$13, %r11, %r9
	addq	%rax, %r9
	adcq	$0, %r8
	shldq	$13, %r9, %r8
	movq	%r9, %xmm0
	movq	%r11, %xmm1
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rax
	addq	%rdi, %rax
	movq	%rax, %rdx
	shrq	$51, %rdx
	addq	%rsi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rsi
	addq	%rcx, %rsi
	andq	%r10, %rax
	movq	%rax, 144(%rsp)
	andq	%r10, %rdx
	movq	%rdx, 152(%rsp)
	movq	%rsi, 160(%rsp)
	punpcklqdq	%xmm0, %xmm1            # xmm1 = xmm1[0],xmm0[0]
	pand	.LCPI15_0(%rip), %xmm1
	movdqu	%xmm1, 168(%rsp)
	leaq	144(%rsp), %rdx
	leaq	192(%rsp), %rdi
	movq	%rdi, %rsi
	callq	fe_mul_impl
	movq	224(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r9
	movq	216(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r10
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r9
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	208(%rsp), %r8
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r8, %rax
	mulq	%r9
	movq	%rdx, %rsi
	movq	%rax, %rdi
	movq	%r8, %rax
	mulq	%r10
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	movq	%r8, %rax
	mulq	%r8
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	200(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%r9
	movq	%rax, %r9
	movq	%rdx, %r14
	leaq	(%r8,%r8), %r11
	movq	%rbp, %rax
	mulq	%r12
	movq	%rdx, %r10
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, %r8
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%rbp
	movq	%rax, %r13
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	192(%rsp), %r15
	movq	%r15, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rbp, %r12
	addq	%rbp, %r12
	movq	%r15, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %rbp
	movq	%r15, %rax
	mulq	%r15
	addq	40(%rsp), %r9                   # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r9
	adcq	%rdx, %r14
	shldq	$13, %r9, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r9
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r10                 # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %r8                   # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %r8                   # 8-byte Folded Reload
	addq	104(%rsp), %r13                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r13                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %rdi                 # 8-byte Folded Reload
	adcq	128(%rsp), %rsi                 # 8-byte Folded Reload
	addq	%rbp, %rdi
	adcq	%r12, %rsi
	addq	%r14, %rdi
	adcq	$0, %rsi
	shldq	$13, %rdi, %rsi
	andq	%rdx, %rdi
	addq	%r13, %rsi
	adcq	$0, %rcx
	shldq	$13, %rsi, %rcx
	andq	%rdx, %rsi
	addq	%r11, %rcx
	adcq	$0, %r8
	shldq	$13, %rcx, %r8
	andq	%rdx, %rcx
	addq	%rax, %r8
	adcq	$0, %r10
	shldq	$13, %r8, %r10
	andq	%rdx, %r8
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %r15
	addq	%r9, %r15
	movq	%r15, %r12
	shrq	$51, %r12
	addq	%rdi, %r12
	andq	%rdx, %r15
	movq	%r12, %rdi
	shrq	$51, %rdi
	addq	%rsi, %rdi
	andq	%rdx, %r12
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rdx
	imulq	$38, %r8, %r9
	leaq	(%r8,%r8), %rbx
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rsi
	movq	%r8, %rax
	mulq	%rdx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r9
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	imulq	$38, %rcx, %r10
	movq	%rcx, %rax
	mulq	%rsi
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r9
	movq	%rdx, %rsi
	movq	%rax, %r8
	movq	%rdi, %rax
	mulq	%r10
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r14
	movq	%rdi, %rax
	mulq	%rdi
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r12, %rax
	mulq	%r9
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r12, %rax
	mulq	%r14
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rdi), %r11
	movq	%r11, %rax
	mulq	%r12
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r12, %rax
	mulq	%r12
	movq	%rdx, %r9
	movq	%rax, %r10
	movq	%r15, %rax
	mulq	%rbx
	movq	%rax, %rcx
	movq	%rdx, %r13
	leaq	(%r12,%r12), %rbx
	movq	%r15, %rax
	mulq	%r14
	movq	%rdx, %r12
	movq	%rax, %rdi
	movq	%r11, %rax
	mulq	%r15
	movq	%rdx, %r14
	movq	%rax, %r11
	movq	%rbx, %rax
	mulq	%r15
	movq	%rdx, %rbp
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r15
	addq	8(%rsp), %rax                   # 8-byte Folded Reload
	adcq	16(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rax                  # 8-byte Folded Reload
	adcq	32(%rsp), %rdx                  # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r15         # imm = 0x7FFFFFFFFFFFF
	andq	%r15, %rax
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r9                   # 8-byte Folded Reload
	addq	%r11, %r10
	adcq	%r14, %r9
	addq	56(%rsp), %r8                   # 8-byte Folded Reload
	adcq	64(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%rbx, %r8
	adcq	%rbp, %rsi
	addq	%rdx, %r8
	adcq	$0, %rsi
	movq	%rsi, %rdx
	shrq	$51, %rdx
	shldq	$13, %r8, %rsi
	andq	%r15, %r8
	addq	%r10, %rsi
	adcq	%r9, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	shldq	$13, %rsi, %rdx
	andq	%r15, %rsi
	addq	104(%rsp), %rdi                 # 8-byte Folded Reload
	adcq	120(%rsp), %r12                 # 8-byte Folded Reload
	addq	72(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	80(%rsp), %r12                  # 8-byte Folded Reload
	addq	%rdx, %rdi
	adcq	%r9, %r12
	movq	%r12, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %r12
	movq	%r15, %r9
	andq	%r15, %rdi
	addq	88(%rsp), %rcx                  # 8-byte Folded Reload
	adcq	96(%rsp), %r13                  # 8-byte Folded Reload
	addq	112(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	128(%rsp), %r13                 # 8-byte Folded Reload
	addq	%r12, %rcx
	adcq	%rdx, %r13
	shldq	$13, %rcx, %r13
	andq	%r15, %rcx
	leaq	(,%r13,8), %rdx
	addq	%r13, %rdx
	leaq	(,%rdx,2), %r15
	addq	%r13, %r15
	addq	%rax, %r15
	movq	%r15, %r12
	shrq	$51, %r12
	addq	%r8, %r12
	movq	%r9, %rax
	andq	%r9, %r15
	movq	%r12, %r9
	shrq	$51, %r9
	addq	%rsi, %r9
	andq	%rax, %r12
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %r10
	leaq	(%rcx,%rcx), %rsi
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %r8
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r10
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	imulq	$38, %rdi, %r11
	movq	%rdi, %rax
	mulq	%r8
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rcx
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rdi), %r14
	movq	%r9, %rax
	mulq	%r9
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r12, %rax
	mulq	%r10
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r12, %rax
	mulq	%r14
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	leaq	(%r9,%r9), %r11
	movq	%r11, %rax
	mulq	%r12
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r12, %rax
	mulq	%r12
	movq	%rdx, %r9
	movq	%rax, %r10
	movq	%r15, %rax
	mulq	%rsi
	movq	%rax, %rdi
	movq	%rdx, %r13
	leaq	(%r12,%r12), %rbx
	movq	%r15, %rax
	mulq	%r14
	movq	%rdx, %r12
	movq	%rax, %rsi
	movq	%r11, %rax
	mulq	%r15
	movq	%rdx, %r14
	movq	%rax, %r11
	movq	%rbx, %rax
	mulq	%r15
	movq	%rdx, %rbp
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r15
	addq	8(%rsp), %rax                   # 8-byte Folded Reload
	adcq	16(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rax                  # 8-byte Folded Reload
	adcq	32(%rsp), %rdx                  # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r15         # imm = 0x7FFFFFFFFFFFF
	andq	%r15, %rax
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r9                   # 8-byte Folded Reload
	addq	%r11, %r10
	adcq	%r14, %r9
	addq	56(%rsp), %r8                   # 8-byte Folded Reload
	adcq	64(%rsp), %rcx                  # 8-byte Folded Reload
	addq	%rbx, %r8
	adcq	%rbp, %rcx
	addq	%rdx, %r8
	adcq	$0, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	shldq	$13, %r8, %rcx
	andq	%r15, %r8
	addq	%r10, %rcx
	adcq	%r9, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	shldq	$13, %rcx, %rdx
	andq	%r15, %rcx
	addq	104(%rsp), %rsi                 # 8-byte Folded Reload
	adcq	120(%rsp), %r12                 # 8-byte Folded Reload
	addq	72(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	80(%rsp), %r12                  # 8-byte Folded Reload
	addq	%rdx, %rsi
	adcq	%r9, %r12
	movq	%r12, %rdx
	shrq	$51, %rdx
	shldq	$13, %rsi, %r12
	movq	%r15, %r9
	andq	%r15, %rsi
	addq	88(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	96(%rsp), %r13                  # 8-byte Folded Reload
	addq	112(%rsp), %rdi                 # 8-byte Folded Reload
	adcq	128(%rsp), %r13                 # 8-byte Folded Reload
	addq	%r12, %rdi
	adcq	%rdx, %r13
	shldq	$13, %rdi, %r13
	andq	%r15, %rdi
	leaq	(,%r13,8), %rdx
	addq	%r13, %rdx
	leaq	(,%rdx,2), %r15
	addq	%r13, %r15
	addq	%rax, %r15
	movq	%r15, %r12
	shrq	$51, %r12
	addq	%r8, %r12
	andq	%r9, %r15
	movq	%r12, %r8
	shrq	$51, %r8
	addq	%rcx, %r8
	andq	%r9, %r12
	leaq	(%rdi,%rdi,8), %rax
	leaq	(%rdi,%rax,2), %rcx
	imulq	$38, %rdi, %r9
	leaq	(%rdi,%rdi), %rbx
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %r10
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	mulq	%r9
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	imulq	$38, %rsi, %r11
	movq	%rsi, %rax
	mulq	%r10
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%r9
	movq	%rdx, %rcx
	movq	%rax, %rdi
	movq	%r8, %rax
	mulq	%r11
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	addq	%rsi, %rsi
	movq	%r8, %rax
	mulq	%r8
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r12, %rax
	mulq	%r9
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r12, %rax
	mulq	%rsi
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	leaq	(%r8,%r8), %r11
	movq	%r11, %rax
	mulq	%r12
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r12, %rax
	mulq	%r12
	movq	%rdx, %r9
	movq	%rax, %r10
	movq	%r15, %rax
	mulq	%rbx
	movq	%rax, %r8
	movq	%rdx, %r13
	leaq	(%r12,%r12), %rbx
	movq	%r15, %rax
	mulq	%rsi
	movq	%rdx, %r12
	movq	%rax, %rsi
	movq	%r11, %rax
	mulq	%r15
	movq	%rdx, %r14
	movq	%rax, %r11
	movq	%rbx, %rax
	mulq	%r15
	movq	%rdx, %rbp
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r15
	addq	8(%rsp), %rax                   # 8-byte Folded Reload
	adcq	16(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rax                  # 8-byte Folded Reload
	adcq	32(%rsp), %rdx                  # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r15         # imm = 0x7FFFFFFFFFFFF
	andq	%r15, %rax
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r9                   # 8-byte Folded Reload
	addq	%r11, %r10
	adcq	%r14, %r9
	addq	56(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	64(%rsp), %rcx                  # 8-byte Folded Reload
	addq	%rbx, %rdi
	adcq	%rbp, %rcx
	addq	%rdx, %rdi
	adcq	$0, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %rcx
	andq	%r15, %rdi
	addq	%r10, %rcx
	adcq	%r9, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	shldq	$13, %rcx, %rdx
	andq	%r15, %rcx
	addq	104(%rsp), %rsi                 # 8-byte Folded Reload
	adcq	120(%rsp), %r12                 # 8-byte Folded Reload
	addq	72(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	80(%rsp), %r12                  # 8-byte Folded Reload
	addq	%rdx, %rsi
	adcq	%r9, %r12
	movq	%r12, %rdx
	shrq	$51, %rdx
	shldq	$13, %rsi, %r12
	movq	%r15, %r9
	andq	%r15, %rsi
	addq	88(%rsp), %r8                   # 8-byte Folded Reload
	adcq	96(%rsp), %r13                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %r13                 # 8-byte Folded Reload
	addq	%r12, %r8
	adcq	%rdx, %r13
	shldq	$13, %r8, %r13
	andq	%r15, %r8
	leaq	(,%r13,8), %rdx
	addq	%r13, %rdx
	leaq	(,%rdx,2), %r15
	addq	%r13, %r15
	addq	%rax, %r15
	movq	%r15, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r9, %rax
	andq	%r9, %r15
	movq	%r11, %r9
	shrq	$51, %r9
	addq	%rcx, %r9
	andq	%rax, %r11
	leaq	(%r8,%r8,8), %rax
	leaq	(%r8,%rax,2), %rcx
	imulq	$38, %r8, %rbx
	leaq	(%r8,%r8), %r10
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rdi
	movq	%r8, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%rsi, %rax
	mulq	%rbx
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	imulq	$38, %rsi, %r8
	movq	%rsi, %rax
	mulq	%rdi
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%rbx
	movq	%rdx, %rcx
	movq	%rax, %rdi
	movq	%r9, %rax
	mulq	%r8
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	leaq	(%rsi,%rsi), %r14
	movq	%r9, %rax
	mulq	%r9
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%rbx
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r11, %rax
	mulq	%r14
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	leaq	(%r9,%r9), %r8
	movq	%r8, %rax
	mulq	%r11
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, %r9
	movq	%r15, %rax
	mulq	%r10
	movq	%rax, %r13
	movq	%rdx, %rbx
	leaq	(%r11,%r11), %r10
	movq	%r15, %rax
	mulq	%r14
	movq	%rdx, %r14
	movq	%rax, %r11
	movq	%r8, %rax
	mulq	%r15
	movq	%rdx, %r12
	movq	%rax, %r8
	movq	%r10, %rax
	mulq	%r15
	movq	%rdx, %rbp
	movq	%rax, %r10
	movq	%r15, %rax
	mulq	%r15
	addq	8(%rsp), %rax                   # 8-byte Folded Reload
	adcq	16(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rax                  # 8-byte Folded Reload
	adcq	32(%rsp), %rdx                  # 8-byte Folded Reload
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %r15         # imm = 0x7FFFFFFFFFFFF
	andq	%r15, %rax
	addq	40(%rsp), %r9                   # 8-byte Folded Reload
	adcq	48(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%r8, %r9
	adcq	%r12, %rsi
	addq	56(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	64(%rsp), %rcx                  # 8-byte Folded Reload
	addq	%r10, %rdi
	adcq	%rbp, %rcx
	addq	%rdx, %rdi
	adcq	$0, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %rcx
	andq	%r15, %rdi
	addq	%r9, %rcx
	adcq	%rsi, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rsi
	shldq	$13, %rcx, %rdx
	andq	%r15, %rcx
	addq	120(%rsp), %r11                 # 8-byte Folded Reload
	adcq	128(%rsp), %r14                 # 8-byte Folded Reload
	addq	72(%rsp), %r11                  # 8-byte Folded Reload
	adcq	80(%rsp), %r14                  # 8-byte Folded Reload
	addq	88(%rsp), %r13                  # 8-byte Folded Reload
	adcq	96(%rsp), %rbx                  # 8-byte Folded Reload
	addq	104(%rsp), %r13                 # 8-byte Folded Reload
	adcq	112(%rsp), %rbx                 # 8-byte Folded Reload
	addq	%rdx, %r11
	adcq	%rsi, %r14
	movq	%r14, %rdx
	shrq	$51, %rdx
	shldq	$13, %r11, %r14
	addq	%r13, %r14
	adcq	%rbx, %rdx
	shldq	$13, %r14, %rdx
	movq	%r14, %xmm0
	movq	%r11, %xmm1
	leaq	(%rdx,%rdx,8), %rsi
	leaq	(%rdx,%rsi,2), %rdx
	addq	%rax, %rdx
	movq	%rdx, %rax
	shrq	$51, %rax
	addq	%rdi, %rax
	movq	%rax, %rsi
	shrq	$51, %rsi
	addq	%rcx, %rsi
	punpcklqdq	%xmm0, %xmm1            # xmm1 = xmm1[0],xmm0[0]
	pand	.LCPI15_0(%rip), %xmm1
	movdqu	%xmm1, 168(%rsp)
	movq	%rsi, 160(%rsp)
	andq	%r15, %rax
	movq	%rax, 152(%rsp)
	andq	%r15, %rdx
	movq	%rdx, 144(%rsp)
	leaq	192(%rsp), %rdi
	leaq	144(%rsp), %rsi
	movq	%rdi, %rdx
	callq	fe_mul_impl
	movq	224(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	216(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	208(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	200(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	192(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$9, %eax
	.p2align	4, 0x90
.LBB15_1:                               # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_1
# %bb.2:
	movq	%rsi, 176(%rsp)
	movq	%rcx, 168(%rsp)
	movq	%r11, 160(%rsp)
	movq	%rbx, 152(%rsp)
	movq	%r9, 144(%rsp)
	leaq	144(%rsp), %rdi
	leaq	192(%rsp), %rdx
	movq	%rdi, %rsi
	callq	fe_mul_impl
	movq	176(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	168(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	160(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	152(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	144(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$19, %eax
	.p2align	4, 0x90
.LBB15_3:                               # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_3
# %bb.4:
	movq	%rsi, 336(%rsp)
	movq	%rcx, 328(%rsp)
	movq	%r11, 320(%rsp)
	movq	%rbx, 312(%rsp)
	movq	%r9, 304(%rsp)
	leaq	304(%rsp), %rsi
	leaq	144(%rsp), %rdi
	movq	%rdi, %rdx
	callq	fe_mul_impl
	movq	176(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	168(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	160(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	152(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	144(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$9, %eax
	.p2align	4, 0x90
.LBB15_5:                               # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_5
# %bb.6:
	movq	%rsi, 176(%rsp)
	movq	%rcx, 168(%rsp)
	movq	%r11, 160(%rsp)
	movq	%rbx, 152(%rsp)
	movq	%r9, 144(%rsp)
	leaq	144(%rsp), %rsi
	leaq	192(%rsp), %rdi
	movq	%rdi, %rdx
	callq	fe_mul_impl
	movq	224(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	216(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	208(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	200(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	192(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$49, %eax
	.p2align	4, 0x90
.LBB15_7:                               # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_7
# %bb.8:
	movq	%rsi, 176(%rsp)
	movq	%rcx, 168(%rsp)
	movq	%r11, 160(%rsp)
	movq	%rbx, 152(%rsp)
	movq	%r9, 144(%rsp)
	leaq	144(%rsp), %rdi
	leaq	192(%rsp), %rdx
	movq	%rdi, %rsi
	callq	fe_mul_impl
	movq	176(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	168(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	160(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	152(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	144(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$99, %eax
	.p2align	4, 0x90
.LBB15_9:                               # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_9
# %bb.10:
	movq	%rsi, 336(%rsp)
	movq	%rcx, 328(%rsp)
	movq	%r11, 320(%rsp)
	movq	%rbx, 312(%rsp)
	movq	%r9, 304(%rsp)
	leaq	304(%rsp), %rsi
	leaq	144(%rsp), %rdi
	movq	%rdi, %rdx
	callq	fe_mul_impl
	movq	176(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r10
	movq	168(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r11
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	160(%rsp), %r9
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, %rdi
	movq	%rax, %r8
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r15
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	152(%rsp), %r13
	movq	%r13, %rax
	mulq	%r10
	movq	%rax, %r10
	movq	%rdx, %r14
	leaq	(%r9,%r9), %r11
	movq	%r13, %rax
	mulq	%r15
	movq	%rdx, %r9
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r11
	movq	%rdx, %rsi
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r13
	movq	%rax, %r12
	movq	%rdx, %rcx
	leaq	(%rbx,%rbx), %rdx
	movq	144(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbp, %rax
	mulq	%r15
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %r15
	addq	%r13, %r15
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r15
	movq	%rdx, %r15
	movq	%rax, %r13
	movq	%rbp, %rax
	mulq	%rbp
	addq	40(%rsp), %r10                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r10
	adcq	%rdx, %r14
	shldq	$13, %r10, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r10
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	72(%rsp), %rax                  # 8-byte Folded Reload
	adcq	80(%rsp), %r9                   # 8-byte Folded Reload
	addq	%rbx, %rax
	adcq	136(%rsp), %r9                  # 8-byte Folded Reload
	movq	64(%rsp), %r11                  # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %rsi                  # 8-byte Folded Reload
	addq	8(%rsp), %r11                   # 8-byte Folded Reload
	adcq	16(%rsp), %rsi                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	32(%rsp), %r12                  # 8-byte Folded Reload
	adcq	56(%rsp), %rcx                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r15, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%r12, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%r11, %rcx
	adcq	$0, %rsi
	shldq	$13, %rcx, %rsi
	andq	%rdx, %rcx
	addq	%rax, %rsi
	adcq	$0, %r9
	shldq	$13, %rsi, %r9
	leaq	(%r9,%r9,8), %rax
	leaq	(%r9,%rax,2), %r9
	andq	%rdx, %rsi
	addq	%r10, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	andq	%rdx, %r9
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	andq	%rdx, %rbx
	movl	$49, %eax
	.p2align	4, 0x90
.LBB15_11:                              # =>This Inner Loop Header: Depth=1
	movl	%eax, 128(%rsp)                 # 4-byte Spill
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	imulq	$38, %rsi, %r10
	mulq	%rsi
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdi
	movq	%rcx, %rax
	mulq	%r10
	movq	%rax, 96(%rsp)                  # 8-byte Spill
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%rcx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r10
	movq	%rax, %r8
	movq	%rdx, %rdi
	imulq	$38, %rcx, %rdx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	leaq	(%r11,%r11), %r13
	movq	%rbx, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r14
	movq	%rbx, %rax
	mulq	%r12
	movq	%rdx, %r15
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbx, %rax
	mulq	%r13
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%rdx, %r10
	addq	%rsi, %rsi
	movq	%rbx, %rax
	mulq	%rbx
	movq	%rax, %rbp
	movq	%rdx, %rcx
	movq	%r9, %rax
	mulq	%rsi
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rsi
	movq	%r9, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%rbx,%rbx), %r12
	movq	%r9, %rax
	mulq	%r13
	movq	%rax, %rbx
	movq	%rdx, 40(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %r13
	movq	%r9, %rax
	mulq	%r9
	addq	32(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r11
	adcq	%rdx, %r14
	shldq	$13, %r11, %r14
	movabsq	$2251799813685247, %rdx         # imm = 0x7FFFFFFFFFFFF
	andq	%rdx, %r11
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	64(%rsp), %rax                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	%rsi, %rax
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	movq	56(%rsp), %rsi                  # 8-byte Reload
	addq	80(%rsp), %rsi                  # 8-byte Folded Reload
	adcq	88(%rsp), %r10                  # 8-byte Folded Reload
	addq	8(%rsp), %rsi                   # 8-byte Folded Reload
	adcq	16(%rsp), %r10                  # 8-byte Folded Reload
	addq	96(%rsp), %rbp                  # 8-byte Folded Reload
	adcq	104(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rbx, %rbp
	adcq	40(%rsp), %rcx                  # 8-byte Folded Reload
	addq	112(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %rdi                 # 8-byte Folded Reload
	addq	%r13, %r8
	adcq	%r12, %rdi
	addq	%r14, %r8
	adcq	$0, %rdi
	shldq	$13, %r8, %rdi
	andq	%rdx, %r8
	addq	%rbp, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rdx, %rdi
	addq	%rsi, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rdx, %rcx
	addq	%rax, %r10
	adcq	$0, %r15
	shldq	$13, %r10, %r15
	leaq	(%r15,%r15,8), %rax
	leaq	(%r15,%rax,2), %r9
	movl	128(%rsp), %eax                 # 4-byte Reload
	addq	%r11, %r9
	movq	%r9, %rbx
	shrq	$51, %rbx
	addq	%r8, %rbx
	movq	%rbx, %r11
	shrq	$51, %r11
	addq	%rdi, %r11
	movq	%r10, %rsi
	andq	%rdx, %rsi
	andq	%rdx, %r9
	andq	%rdx, %rbx
	decl	%eax
	jne	.LBB15_11
# %bb.12:
	movq	%rsi, 176(%rsp)
	movq	%rcx, 168(%rsp)
	movq	%r11, 160(%rsp)
	movq	%rbx, 152(%rsp)
	movq	%r9, 144(%rsp)
	leaq	144(%rsp), %rsi
	leaq	192(%rsp), %rdi
	movq	%rdi, %rdx
	callq	fe_mul_impl
	movq	224(%rsp), %rbx
	leaq	(%rbx,%rbx,8), %rax
	leaq	(%rbx,%rax,2), %rax
	imulq	$38, %rbx, %r9
	movq	216(%rsp), %rcx
	leaq	(%rcx,%rcx,8), %rdx
	leaq	(%rcx,%rdx,2), %rsi
	imulq	$38, %rcx, %r10
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rcx, %rax
	mulq	%r9
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	208(%rsp), %rdi
	movq	%rsi, %rax
	mulq	%rcx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r9
	movq	%rdx, %rsi
	movq	%rax, %r8
	movq	%rdi, %rax
	mulq	%r10
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rcx,%rcx), %r12
	movq	%rdi, %rax
	mulq	%rdi
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	200(%rsp), %rbp
	movq	%rbp, %rax
	mulq	%r9
	movq	%rax, %r9
	movq	%rdx, %r14
	leaq	(%rdi,%rdi), %r11
	movq	%rbp, %rax
	mulq	%r12
	movq	%rdx, %r10
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%r11
	movq	%rdx, %rcx
	movq	%rax, 64(%rsp)                  # 8-byte Spill
	movq	%rbp, %rax
	mulq	%rbp
	movq	%rax, %r13
	movq	%rdx, %rdi
	leaq	(%rbx,%rbx), %rdx
	movq	192(%rsp), %r15
	movq	%r15, %rax
	mulq	%rdx
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, %rbx
	movq	%r15, %rax
	mulq	%r12
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	movq	%rbp, %r12
	addq	%rbp, %r12
	movq	%r15, %rax
	mulq	%r11
	movq	%rdx, 56(%rsp)                  # 8-byte Spill
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r12
	movq	%rdx, %r12
	movq	%rax, %rbp
	movq	%r15, %rax
	mulq	%r15
	addq	40(%rsp), %r9                   # 8-byte Folded Reload
	adcq	48(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rax, %r9
	adcq	%rdx, %r14
	shldq	$13, %r9, %r14
	movabsq	$2251799813685247, %rax         # imm = 0x7FFFFFFFFFFFF
	andq	%rax, %r9
	movq	24(%rsp), %r11                  # 8-byte Reload
	addq	72(%rsp), %r11                  # 8-byte Folded Reload
	adcq	80(%rsp), %r10                  # 8-byte Folded Reload
	addq	%rbx, %r11
	adcq	136(%rsp), %r10                 # 8-byte Folded Reload
	movq	64(%rsp), %rbx                  # 8-byte Reload
	addq	88(%rsp), %rbx                  # 8-byte Folded Reload
	adcq	96(%rsp), %rcx                  # 8-byte Folded Reload
	addq	8(%rsp), %rbx                   # 8-byte Folded Reload
	adcq	16(%rsp), %rcx                  # 8-byte Folded Reload
	addq	104(%rsp), %r13                 # 8-byte Folded Reload
	adcq	112(%rsp), %rdi                 # 8-byte Folded Reload
	addq	32(%rsp), %r13                  # 8-byte Folded Reload
	adcq	56(%rsp), %rdi                  # 8-byte Folded Reload
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	adcq	128(%rsp), %rsi                 # 8-byte Folded Reload
	addq	%rbp, %r8
	adcq	%r12, %rsi
	addq	%r14, %r8
	adcq	$0, %rsi
	shldq	$13, %r8, %rsi
	movq	%rax, %rdx
	andq	%rax, %r8
	addq	%r13, %rsi
	adcq	$0, %rdi
	shldq	$13, %rsi, %rdi
	andq	%rax, %rsi
	addq	%rbx, %rdi
	adcq	$0, %rcx
	shldq	$13, %rdi, %rcx
	andq	%rax, %rdi
	addq	%r11, %rcx
	adcq	$0, %r10
	shldq	$13, %rcx, %r10
	andq	%rax, %rcx
	leaq	(%r10,%r10,8), %rax
	leaq	(%r10,%rax,2), %r13
	addq	%r9, %r13
	movq	%r13, %r10
	shrq	$51, %r10
	addq	%r8, %r10
	andq	%rdx, %r13
	movq	%r10, %r9
	shrq	$51, %r9
	addq	%rsi, %r9
	andq	%rdx, %r10
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %r11
	leaq	(%rdi,%rdi,8), %rsi
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rsi,2), %rdx
	movq	%rdi, %rax
	mulq	%rdx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, %r8
	movq	%rdx, %rsi
	imulq	$38, %rdi, %rdx
	addq	%rdi, %rdi
	movq	%r9, %rax
	mulq	%rdx
	movq	%rdx, %rbp
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r9
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	mulq	%r11
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%rdx, 24(%rsp)                  # 8-byte Spill
	leaq	(%r9,%r9), %r15
	movq	%r10, %rax
	mulq	%rdi
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r10
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r9
	addq	%rcx, %rcx
	movq	%r13, %rax
	mulq	%rcx
	movq	%rdx, %rbx
	movq	%rax, %rcx
	movq	%r13, %rax
	mulq	%rdi
	movq	%rdx, %r14
	movq	%rax, %rdi
	movq	%r15, %rax
	mulq	%r13
	movq	%rax, %r15
	movq	%rdx, %r12
	leaq	(%r10,%r10), %rax
	mulq	%r13
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, %r10
	movq	%r13, %rax
	mulq	%r13
	addq	16(%rsp), %rax                  # 8-byte Folded Reload
	adcq	24(%rsp), %rdx                  # 8-byte Folded Reload
	addq	32(%rsp), %rax                  # 8-byte Folded Reload
	adcq	%rbp, %rdx
	movq	%rdx, %r13
	shrq	$51, %r13
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %rbp         # imm = 0x7FFFFFFFFFFFF
	andq	%rbp, %rax
	addq	40(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r9                   # 8-byte Folded Reload
	addq	%r15, %r11
	adcq	%r12, %r9
	addq	56(%rsp), %r8                   # 8-byte Folded Reload
	adcq	64(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%r10, %r8
	adcq	8(%rsp), %rsi                   # 8-byte Folded Reload
	addq	%rdx, %r8
	adcq	%r13, %rsi
	movq	%rsi, %rdx
	shrq	$51, %rdx
	shldq	$13, %r8, %rsi
	movabsq	$2251799813685247, %r10         # imm = 0x7FFFFFFFFFFFF
	andq	%r10, %r8
	addq	%r11, %rsi
	adcq	%r9, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	shldq	$13, %rsi, %rdx
	andq	%r10, %rsi
	addq	104(%rsp), %rdi                 # 8-byte Folded Reload
	adcq	120(%rsp), %r14                 # 8-byte Folded Reload
	addq	72(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	80(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rdx, %rdi
	adcq	%r9, %r14
	movq	%r14, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %r14
	andq	%r10, %rdi
	addq	88(%rsp), %rcx                  # 8-byte Folded Reload
	adcq	96(%rsp), %rbx                  # 8-byte Folded Reload
	addq	112(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	128(%rsp), %rbx                 # 8-byte Folded Reload
	addq	%r14, %rcx
	adcq	%rdx, %rbx
	shldq	$13, %rcx, %rbx
	andq	%r10, %rcx
	leaq	(%rbx,%rbx,8), %rdx
	leaq	(%rbx,%rdx,2), %r13
	addq	%rax, %r13
	movq	%r13, %r9
	shrq	$51, %r9
	addq	%r8, %r9
	movq	%r10, %rax
	andq	%r10, %r13
	movq	%r9, %r10
	shrq	$51, %r10
	addq	%rsi, %r10
	andq	%rax, %r9
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %r11
	leaq	(%rdi,%rdi,8), %rsi
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rsi,2), %rdx
	movq	%rdi, %rax
	mulq	%rdx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%r11
	movq	%rax, %r8
	movq	%rdx, %rsi
	imulq	$38, %rdi, %rdx
	addq	%rdi, %rdi
	movq	%r10, %rax
	mulq	%rdx
	movq	%rdx, %rbp
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%r10
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r9, %rax
	mulq	%r11
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%rdx, 24(%rsp)                  # 8-byte Spill
	leaq	(%r10,%r10), %r15
	movq	%r9, %rax
	mulq	%rdi
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r9
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r9
	movq	%rax, %r11
	movq	%rdx, %r10
	addq	%rcx, %rcx
	movq	%r13, %rax
	mulq	%rcx
	movq	%rdx, %rbx
	movq	%rax, %rcx
	movq	%r13, %rax
	mulq	%rdi
	movq	%rdx, %r14
	movq	%rax, %rdi
	movq	%r15, %rax
	mulq	%r13
	movq	%rax, %r15
	movq	%rdx, %r12
	leaq	(%r9,%r9), %rax
	mulq	%r13
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, %r9
	movq	%r13, %rax
	mulq	%r13
	addq	16(%rsp), %rax                  # 8-byte Folded Reload
	adcq	24(%rsp), %rdx                  # 8-byte Folded Reload
	addq	32(%rsp), %rax                  # 8-byte Folded Reload
	adcq	%rbp, %rdx
	movq	%rdx, %r13
	shrq	$51, %r13
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %rbp         # imm = 0x7FFFFFFFFFFFF
	andq	%rbp, %rax
	addq	40(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r10                  # 8-byte Folded Reload
	addq	%r15, %r11
	adcq	%r12, %r10
	addq	56(%rsp), %r8                   # 8-byte Folded Reload
	adcq	64(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%r9, %r8
	adcq	8(%rsp), %rsi                   # 8-byte Folded Reload
	addq	%rdx, %r8
	adcq	%r13, %rsi
	movq	%rsi, %rdx
	shrq	$51, %rdx
	shldq	$13, %r8, %rsi
	movabsq	$2251799813685247, %r15         # imm = 0x7FFFFFFFFFFFF
	andq	%r15, %r8
	addq	%r11, %rsi
	adcq	%r10, %rdx
	movq	%rdx, %r9
	shrq	$51, %r9
	shldq	$13, %rsi, %rdx
	andq	%r15, %rsi
	addq	104(%rsp), %rdi                 # 8-byte Folded Reload
	adcq	120(%rsp), %r14                 # 8-byte Folded Reload
	addq	72(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	80(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rdx, %rdi
	adcq	%r9, %r14
	movq	%r14, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %r14
	andq	%r15, %rdi
	addq	88(%rsp), %rcx                  # 8-byte Folded Reload
	adcq	96(%rsp), %rbx                  # 8-byte Folded Reload
	addq	112(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	128(%rsp), %rbx                 # 8-byte Folded Reload
	addq	%r14, %rcx
	adcq	%rdx, %rbx
	shldq	$13, %rcx, %rbx
	andq	%r15, %rcx
	leaq	(%rbx,%rbx,8), %rdx
	leaq	(%rbx,%rdx,2), %r13
	addq	%rax, %r13
	movq	%r13, %r10
	shrq	$51, %r10
	addq	%r8, %r10
	andq	%r15, %r13
	movq	%r10, %r8
	shrq	$51, %r8
	addq	%rsi, %r8
	andq	%r15, %r10
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %r11
	leaq	(%rdi,%rdi,8), %rsi
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%rdi, %rax
	mulq	%r11
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%rdi,%rsi,2), %rdx
	movq	%rdi, %rax
	mulq	%rdx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%r11
	movq	%rax, %r9
	movq	%rdx, %rsi
	imulq	$38, %rdi, %rdx
	leaq	(%rdi,%rdi), %r14
	movq	%r8, %rax
	mulq	%rdx
	movq	%rdx, %rbp
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%r8
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	mulq	%r11
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%rdx, 24(%rsp)                  # 8-byte Spill
	leaq	(%r8,%r8), %r15
	movq	%r10, %rax
	mulq	%r14
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%r15, %rax
	mulq	%r10
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %rdi
	addq	%rcx, %rcx
	movq	%r13, %rax
	mulq	%rcx
	movq	%rdx, %rbx
	movq	%rax, %rcx
	movq	%r13, %rax
	mulq	%r14
	movq	%rdx, %r14
	movq	%rax, %r8
	movq	%r15, %rax
	mulq	%r13
	movq	%rax, %r15
	movq	%rdx, %r12
	leaq	(%r10,%r10), %rax
	mulq	%r13
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, %r10
	movq	%r13, %rax
	mulq	%r13
	addq	16(%rsp), %rax                  # 8-byte Folded Reload
	adcq	24(%rsp), %rdx                  # 8-byte Folded Reload
	addq	32(%rsp), %rax                  # 8-byte Folded Reload
	adcq	%rbp, %rdx
	movq	%rdx, %r13
	shrq	$51, %r13
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %rbp         # imm = 0x7FFFFFFFFFFFF
	andq	%rbp, %rax
	addq	40(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %rdi                  # 8-byte Folded Reload
	addq	%r15, %r11
	adcq	%r12, %rdi
	addq	56(%rsp), %r9                   # 8-byte Folded Reload
	adcq	64(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%r10, %r9
	adcq	8(%rsp), %rsi                   # 8-byte Folded Reload
	addq	%rdx, %r9
	adcq	%r13, %rsi
	movq	%rsi, %rdx
	shrq	$51, %rdx
	shldq	$13, %r9, %rsi
	movabsq	$2251799813685247, %r10         # imm = 0x7FFFFFFFFFFFF
	andq	%r10, %r9
	addq	%r11, %rsi
	adcq	%rdi, %rdx
	movq	%rdx, %rdi
	shrq	$51, %rdi
	shldq	$13, %rsi, %rdx
	andq	%r10, %rsi
	movq	%r10, %r11
	addq	104(%rsp), %r8                  # 8-byte Folded Reload
	adcq	120(%rsp), %r14                 # 8-byte Folded Reload
	addq	72(%rsp), %r8                   # 8-byte Folded Reload
	adcq	80(%rsp), %r14                  # 8-byte Folded Reload
	addq	%rdx, %r8
	adcq	%rdi, %r14
	movq	%r14, %rdx
	shrq	$51, %rdx
	shldq	$13, %r8, %r14
	andq	%r10, %r8
	addq	88(%rsp), %rcx                  # 8-byte Folded Reload
	adcq	96(%rsp), %rbx                  # 8-byte Folded Reload
	addq	112(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	128(%rsp), %rbx                 # 8-byte Folded Reload
	addq	%r14, %rcx
	adcq	%rdx, %rbx
	shldq	$13, %rcx, %rbx
	andq	%r10, %rcx
	leaq	(%rbx,%rbx,8), %rdx
	leaq	(%rbx,%rdx,2), %rbp
	addq	%rax, %rbp
	movq	%rbp, %r10
	shrq	$51, %r10
	addq	%r9, %r10
	movq	%r11, %rax
	andq	%r11, %rbp
	movq	%r10, %r11
	shrq	$51, %r11
	addq	%rsi, %r11
	andq	%rax, %r10
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rdx
	imulq	$38, %rcx, %r9
	leaq	(%r8,%r8,8), %rsi
	movq	%rcx, %rax
	mulq	%rdx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r8, %rax
	mulq	%r9
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%rdx, 48(%rsp)                  # 8-byte Spill
	leaq	(%r8,%rsi,2), %rdx
	movq	%r8, %rax
	mulq	%rdx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r9
	movq	%rax, %rdi
	movq	%rdx, %rsi
	imulq	$38, %r8, %rdx
	leaq	(%r8,%r8), %rbx
	movq	%r11, %rax
	mulq	%rdx
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%r11, %rax
	mulq	%r11
	movq	%rdx, 112(%rsp)                 # 8-byte Spill
	movq	%rax, 104(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	mulq	%r9
	movq	%rax, %r13
	movq	%rdx, 16(%rsp)                  # 8-byte Spill
	leaq	(%r11,%r11), %r9
	movq	%r10, %rax
	mulq	%rbx
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%r9, %rax
	mulq	%r10
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	%r10, %rax
	mulq	%r10
	movq	%rax, %r11
	movq	%rdx, %r8
	addq	%rcx, %rcx
	movq	%rbp, %rax
	mulq	%rcx
	movq	%rdx, %rcx
	movq	%rax, %r12
	movq	%rbp, %rax
	mulq	%rbx
	movq	%rdx, %r14
	movq	%rax, %rbx
	movq	%r9, %rax
	mulq	%rbp
	movq	%rax, %r9
	movq	%rdx, %r15
	leaq	(%r10,%r10), %rax
	mulq	%rbp
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, %r10
	movq	%rbp, %rax
	mulq	%rbp
	addq	%r13, %rax
	adcq	16(%rsp), %rdx                  # 8-byte Folded Reload
	addq	24(%rsp), %rax                  # 8-byte Folded Reload
	adcq	32(%rsp), %rdx                  # 8-byte Folded Reload
	movq	%rdx, %r13
	shrq	$51, %r13
	shldq	$13, %rax, %rdx
	movabsq	$2251799813685247, %rbp         # imm = 0x7FFFFFFFFFFFF
	andq	%rbp, %rax
	addq	40(%rsp), %r11                  # 8-byte Folded Reload
	adcq	48(%rsp), %r8                   # 8-byte Folded Reload
	addq	%r9, %r11
	adcq	%r15, %r8
	addq	56(%rsp), %rdi                  # 8-byte Folded Reload
	adcq	64(%rsp), %rsi                  # 8-byte Folded Reload
	addq	%r10, %rdi
	adcq	8(%rsp), %rsi                   # 8-byte Folded Reload
	addq	%rdx, %rdi
	adcq	%r13, %rsi
	movq	%rsi, %rdx
	shrq	$51, %rdx
	shldq	$13, %rdi, %rsi
	andq	%rbp, %rdi
	addq	%r11, %rsi
	adcq	%r8, %rdx
	movq	%rdx, %r8
	shrq	$51, %r8
	shldq	$13, %rsi, %rdx
	andq	%rbp, %rsi
	addq	120(%rsp), %rbx                 # 8-byte Folded Reload
	adcq	128(%rsp), %r14                 # 8-byte Folded Reload
	addq	72(%rsp), %rbx                  # 8-byte Folded Reload
	adcq	80(%rsp), %r14                  # 8-byte Folded Reload
	addq	88(%rsp), %r12                  # 8-byte Folded Reload
	adcq	96(%rsp), %rcx                  # 8-byte Folded Reload
	addq	104(%rsp), %r12                 # 8-byte Folded Reload
	adcq	112(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rdx, %rbx
	adcq	%r8, %r14
	movq	%r14, %rdx
	shrq	$51, %rdx
	shldq	$13, %rbx, %r14
	addq	%r12, %r14
	adcq	%rcx, %rdx
	shldq	$13, %r14, %rdx
	movq	%r14, %xmm0
	movq	%rbx, %xmm1
	leaq	(%rdx,%rdx,8), %rcx
	leaq	(%rdx,%rcx,2), %rcx
	addq	%rax, %rcx
	movq	%rcx, %rax
	shrq	$51, %rax
	addq	%rdi, %rax
	movq	%rax, %rdx
	shrq	$51, %rdx
	addq	%rsi, %rdx
	andq	%rbp, %rcx
	andq	%rbp, %rax
	punpcklqdq	%xmm0, %xmm1            # xmm1 = xmm1[0],xmm0[0]
	pand	.LCPI15_0(%rip), %xmm1
	movdqu	%xmm1, 216(%rsp)
	movq	%rdx, 208(%rsp)
	movq	%rax, 200(%rsp)
	movq	%rcx, 192(%rsp)
	leaq	400(%rsp), %rbx
	leaq	264(%rsp), %rdx
	movq	%rbx, %rdi
	leaq	192(%rsp), %rsi
	callq	fe_mul_impl
	leaq	264(%rsp), %r14
	movq	%r14, %rdi
	movq	344(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	fe_mul_impl
	addq	$40, %r12
	leaq	192(%rsp), %r15
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	fe_mul_impl
	movq	184(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	fe_tobytes
	leaq	144(%rsp), %rdi
	movq	%r14, %rsi
	callq	fe_tobytes
	movzbl	144(%rsp), %eax
	shlb	$7, %al
	xorb	%al, 31(%rbx)
	addq	$440, %rsp                      # imm = 0x1B8
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end15:
	.size	ge_p3_tobytes, .Lfunc_end15-ge_p3_tobytes
                                        # -- End function
	.p2align	4, 0x90                         # -- Begin function sc_muladd
	.type	sc_muladd,@function
sc_muladd:                              # @sc_muladd
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$232, %rsp
	movq	%rdx, %rbx
	movq	%rdi, 224(%rsp)                 # 8-byte Spill
	movzwl	(%rsi), %eax
	movzbl	2(%rsi), %edi
	andl	$31, %edi
	shlq	$16, %rdi
	orq	%rax, %rdi
	movq	%rdi, %r11
	movzwl	5(%rsi), %eax
	movzbl	7(%rsi), %r9d
	shll	$16, %r9d
	orl	%eax, %r9d
	movzwl	13(%rsi), %eax
	movzbl	15(%rsi), %edi
	shll	$16, %edi
	orl	%eax, %edi
	movq	%rdi, -112(%rsp)                # 8-byte Spill
	movzwl	18(%rsi), %eax
	movzbl	20(%rsi), %edi
	shlq	$16, %rdi
	orq	%rax, %rdi
	movq	%rdi, -88(%rsp)                 # 8-byte Spill
	movzwl	21(%rsi), %eax
	movzbl	23(%rsi), %edi
	andl	$31, %edi
	shlq	$16, %rdi
	orq	%rax, %rdi
	movq	%rdi, -24(%rsp)                 # 8-byte Spill
	movzwl	26(%rsi), %eax
	movzbl	28(%rsi), %edi
	movq	%rsi, %rbp
	shll	$16, %edi
	orl	%eax, %edi
	movq	%rdi, -16(%rsp)                 # 8-byte Spill
	movzwl	(%rdx), %eax
	movzbl	2(%rdx), %r13d
	andl	$31, %r13d
	shlq	$16, %r13
	orq	%rax, %r13
	movzwl	5(%rdx), %eax
	movzbl	7(%rdx), %r8d
	shll	$16, %r8d
	orl	%eax, %r8d
	movzwl	13(%rdx), %eax
	movzbl	15(%rdx), %edx
	shll	$16, %edx
	orl	%eax, %edx
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	movzwl	18(%rbx), %eax
	movzbl	20(%rbx), %edx
	shlq	$16, %rdx
	orq	%rax, %rdx
	movq	%rdx, -96(%rsp)                 # 8-byte Spill
	movzbl	23(%rbx), %edx
	andl	$31, %edx
	shlq	$16, %rdx
	movzwl	21(%rbx), %eax
	orq	%rax, %rdx
	movq	%rdx, 8(%rsp)                   # 8-byte Spill
	movzbl	28(%rbx), %edx
	shll	$16, %edx
	movzwl	26(%rbx), %eax
	orl	%eax, %edx
	movq	%rdx, -48(%rsp)                 # 8-byte Spill
	movzbl	2(%rcx), %edx
	andl	$31, %edx
	shlq	$16, %rdx
	movzwl	(%rcx), %eax
	orq	%rax, %rdx
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movzbl	7(%rcx), %edx
	shll	$16, %edx
	movzwl	5(%rcx), %eax
	orl	%eax, %edx
	movq	%rdx, 120(%rsp)                 # 8-byte Spill
	movq	%rcx, %rdx
	movzbl	15(%rcx), %ecx
	shll	$16, %ecx
	movzwl	13(%rdx), %eax
	movq	%rdx, %rdi
	orl	%eax, %ecx
	movzbl	20(%rdx), %eax
	shlq	$16, %rax
	movzwl	18(%rdx), %edx
	movq	%rdi, %r10
	orq	%rdx, %rax
	movq	%rax, -32(%rsp)                 # 8-byte Spill
	movzbl	28(%rdi), %edi
	shll	$16, %edi
	movzwl	26(%r10), %edx
	movq	%r10, %r14
	movq	%r10, 56(%rsp)                  # 8-byte Spill
	orl	%edx, %edi
	movq	%rdi, 112(%rsp)                 # 8-byte Spill
	movl	2(%rsi), %eax
	shrl	$5, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	%rax, %r15
	movl	2(%rbx), %eax
	shrl	$5, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	%r11, %rdi
	imulq	%rax, %rdi
	movq	%r13, %rdx
	imulq	%r15, %rdx
	addq	%rdi, %rdx
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	shrl	$2, %r9d
	andl	$2097151, %r9d                  # imm = 0x1FFFFF
	movq	%rax, %rdi
	movq	%rax, %rsi
	imulq	%r15, %rdi
	movq	%r13, %r10
	imulq	%r9, %r10
	movq	%r9, %rax
	addq	%rdi, %r10
	movq	%r8, %rdx
	shrl	$2, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%rdx, %rdi
	imulq	%r11, %rdi
	addq	%rdi, %r10
	movq	%r10, 96(%rsp)                  # 8-byte Spill
	movl	7(%r14), %edi
	shrl	$7, %edi
	andl	$2097151, %edi                  # imm = 0x1FFFFF
	movq	%rdx, %r8
	movq	%rdx, %r10
	imulq	%r15, %r8
	movq	%r15, %r12
	addq	%rdi, %r8
	movq	%r8, 216(%rsp)                  # 8-byte Spill
	movq	%rbp, 48(%rsp)                  # 8-byte Spill
	movl	7(%rbp), %r15d
	shrl	$7, %r15d
	andl	$2097151, %r15d                 # imm = 0x1FFFFF
	movq	%rbx, -40(%rsp)                 # 8-byte Spill
	movl	7(%rbx), %edx
	shrl	$7, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%r11, %r8
	movq	%r11, %r14
	imulq	%rdx, %r8
	movq	%rdx, %rdi
	movq	%r13, %rdx
	imulq	%r15, %rdx
	addq	%r8, %rdx
	movq	%r9, %r8
	movq	%rsi, %r9
	movq	%rsi, -120(%rsp)                # 8-byte Spill
	imulq	%rsi, %r8
	addq	%r8, %rdx
	movq	%rdx, 88(%rsp)                  # 8-byte Spill
	movq	%rdi, %r8
	movq	%rdi, %rsi
	imulq	%r12, %r8
	imulq	%r15, %r9
	addq	%r8, %r9
	movl	10(%rbp), %edi
	shrl	$4, %edi
	andl	$2097151, %edi                  # imm = 0x1FFFFF
	movq	%r13, %rdx
	imulq	%rdi, %rdx
	movq	%rdi, %r8
	addq	%r9, %rdx
	movl	10(%rbx), %edi
	shrl	$4, %edi
	andl	$2097151, %edi                  # imm = 0x1FFFFF
	movq	%rdi, -128(%rsp)                # 8-byte Spill
	movq	%r11, %r9
	imulq	%rdi, %r9
	addq	%r9, %rdx
	movq	%r10, %rbx
	movq	%r10, %r9
	imulq	%rax, %r9
	addq	%r9, %rdx
	movq	%rdx, 208(%rsp)                 # 8-byte Spill
	movq	%rdi, %r9
	imulq	%r12, %r9
	movq	%rax, %r10
	movq	%rax, %r11
	movq	%rsi, %rbp
	imulq	%rsi, %r10
	addq	%r9, %r10
	movq	-104(%rsp), %rdx                # 8-byte Reload
	shrl	%edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%rdx, %r9
	movq	%rdx, %rsi
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	imulq	%r14, %r9
	movq	%rbx, %rdx
	movq	%rbx, -72(%rsp)                 # 8-byte Spill
	imulq	%r15, %rdx
	addq	%r9, %rdx
	shrl	%ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	addq	%rcx, %rdx
	movq	%rdx, 200(%rsp)                 # 8-byte Spill
	movq	-112(%rsp), %r9                 # 8-byte Reload
	shrl	%r9d
	andl	$2097151, %r9d                  # imm = 0x1FFFFF
	movq	%r9, -112(%rsp)                 # 8-byte Spill
	movq	-120(%rsp), %rax                # 8-byte Reload
	movq	%rax, %rcx
	movq	%r8, -64(%rsp)                  # 8-byte Spill
	imulq	%r8, %rcx
	movq	%r13, %rdx
	imulq	%r9, %rdx
	addq	%rcx, %rdx
	addq	%r10, %rdx
	movq	%rdx, 80(%rsp)                  # 8-byte Spill
	movq	-40(%rsp), %rcx                 # 8-byte Reload
	movl	15(%rcx), %edx
	shrl	$6, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%rdx, -80(%rsp)                 # 8-byte Spill
	movq	%r14, %rcx
	movq	%r14, -8(%rsp)                  # 8-byte Spill
	imulq	%rdx, %rcx
	movq	%rsi, %r9
	imulq	%r12, %r9
	addq	%rcx, %r9
	movq	%r11, %rcx
	movq	%r11, (%rsp)                    # 8-byte Spill
	imulq	-128(%rsp), %rcx                # 8-byte Folded Reload
	imulq	%r8, %rbx
	addq	%rcx, %rbx
	movq	48(%rsp), %rcx                  # 8-byte Reload
	movl	15(%rcx), %r10d
	shrl	$6, %r10d
	andl	$2097151, %r10d                 # imm = 0x1FFFFF
	movq	%rbp, %rcx
	movq	%rbp, %rsi
	movq	%rbp, -56(%rsp)                 # 8-byte Spill
	movq	%r15, %r8
	imulq	%r15, %rcx
	movq	%r13, %rdx
	movq	%r13, %r15
	movq	%r13, 16(%rsp)                  # 8-byte Spill
	imulq	%r10, %rdx
	addq	%rcx, %rdx
	movq	-112(%rsp), %rdi                # 8-byte Reload
	movq	%rdi, %rcx
	imulq	%rax, %rcx
	addq	%rcx, %rdx
	addq	%rbx, %rdx
	addq	%r9, %rdx
	movq	%rdx, 192(%rsp)                 # 8-byte Spill
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	shrq	$3, %rcx
	movq	%rcx, -96(%rsp)                 # 8-byte Spill
	imulq	%r14, %rcx
	movq	-104(%rsp), %rbx                # 8-byte Reload
	movq	%rbx, %rdx
	imulq	%r11, %rdx
	addq	%rcx, %rdx
	movq	-32(%rsp), %rax                 # 8-byte Reload
	shrq	$3, %rax
	addq	%rax, %rdx
	movq	%rdx, 184(%rsp)                 # 8-byte Spill
	movq	-80(%rsp), %r14                 # 8-byte Reload
	movq	%r14, %rax
	imulq	%r12, %rax
	movq	%r12, %r13
	movq	-72(%rsp), %rbp                 # 8-byte Reload
	movq	%rbp, %rcx
	imulq	%rdi, %rcx
	addq	%rax, %rcx
	imulq	-64(%rsp), %rsi                 # 8-byte Folded Reload
	movq	-120(%rsp), %rdi                # 8-byte Reload
	movq	%rdi, %r9
	imulq	%r10, %r9
	movq	%r10, %r12
	movq	%r10, 32(%rsp)                  # 8-byte Spill
	addq	%rsi, %r9
	movq	-88(%rsp), %rax                 # 8-byte Reload
	shrq	$3, %rax
	movq	%rax, -88(%rsp)                 # 8-byte Spill
	movq	%r15, %rdx
	imulq	%rax, %rdx
	addq	%r9, %rdx
	movq	-128(%rsp), %rsi                # 8-byte Reload
	movq	%rsi, %rax
	imulq	%r8, %rax
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	movq	8(%rsp), %r10                   # 8-byte Reload
	movq	%r10, %rax
	movq	-8(%rsp), %r15                  # 8-byte Reload
	imulq	%r15, %rax
	movq	56(%rsp), %r11                  # 8-byte Reload
	movzwl	21(%r11), %ecx
	addq	%rcx, %rax
	movq	(%rsp), %rcx                    # 8-byte Reload
	imulq	%r14, %rcx
	movq	%rbx, %r9
	imulq	%r8, %r9
	movq	%r8, 40(%rsp)                   # 8-byte Spill
	addq	%rcx, %r9
	movq	-96(%rsp), %rcx                 # 8-byte Reload
	movq	%r13, 24(%rsp)                  # 8-byte Spill
	imulq	%r13, %rcx
	addq	%rcx, %r9
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	imulq	%rdx, %rsi
	movq	%rbp, %rbx
	imulq	%r12, %rbx
	addq	%rsi, %rbx
	movq	-88(%rsp), %rbp                 # 8-byte Reload
	movq	%rbp, %rcx
	imulq	%rdi, %rcx
	movq	16(%rsp), %r12                  # 8-byte Reload
	movq	%r12, %r14
	movq	-24(%rsp), %rsi                 # 8-byte Reload
	imulq	%rsi, %r14
	addq	%rcx, %r14
	movq	-112(%rsp), %rdi                # 8-byte Reload
	movq	%rdi, %rcx
	imulq	-56(%rsp), %rcx                 # 8-byte Folded Reload
	addq	%rcx, %r14
	addq	%rbx, %r14
	addq	%r9, %r14
	addq	%rax, %r14
	movq	%r14, 176(%rsp)                 # 8-byte Spill
	movq	-40(%rsp), %rax                 # 8-byte Reload
	movl	23(%rax), %ecx
	shrl	$5, %ecx
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	movq	%rcx, -32(%rsp)                 # 8-byte Spill
	movq	%r15, %rax
	imulq	%rcx, %rax
	imulq	%r13, %r10
	addq	%rax, %r10
	movl	23(%r11), %eax
	shrl	$5, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	addq	%rax, %r10
	movq	%r10, 168(%rsp)                 # 8-byte Spill
	movq	-80(%rsp), %rax                 # 8-byte Reload
	imulq	%r8, %rax
	movq	-104(%rsp), %rbx                # 8-byte Reload
	imulq	%rdx, %rbx
	addq	%rax, %rbx
	movq	-96(%rsp), %r8                  # 8-byte Reload
	movq	%r8, %rax
	movq	(%rsp), %r15                    # 8-byte Reload
	imulq	%r15, %rax
	addq	%rax, %rbx
	movq	%rdi, %rax
	movq	-128(%rsp), %r10                # 8-byte Reload
	imulq	%r10, %rax
	movq	-72(%rsp), %r11                 # 8-byte Reload
	movq	%r11, %r14
	imulq	%rbp, %r14
	addq	%rax, %r14
	movq	48(%rsp), %rax                  # 8-byte Reload
	movl	23(%rax), %edx
	shrl	$5, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%rsi, %rax
	movq	-120(%rsp), %r9                 # 8-byte Reload
	imulq	%r9, %rax
	imulq	%rdx, %r12
	movq	%rdx, %rbp
	addq	%rax, %r12
	movq	-56(%rsp), %rdx                 # 8-byte Reload
	movq	%rdx, %rax
	imulq	32(%rsp), %rax                  # 8-byte Folded Reload
	addq	%rax, %r12
	addq	%r14, %r12
	addq	%rbx, %r12
	movq	%r12, 64(%rsp)                  # 8-byte Spill
	movq	-32(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rbx
	imulq	%r13, %rbx
	movq	8(%rsp), %rcx                   # 8-byte Reload
	imulq	%r15, %rcx
	addq	%rbx, %rcx
	movq	-48(%rsp), %rbx                 # 8-byte Reload
	shrl	$2, %ebx
	andl	$2097151, %ebx                  # imm = 0x1FFFFF
	movq	%rbx, -48(%rsp)                 # 8-byte Spill
	imulq	-8(%rsp), %rbx                  # 8-byte Folded Reload
	addq	%rbx, %rcx
	movq	-80(%rsp), %r13                 # 8-byte Reload
	movq	%r13, %rbx
	imulq	-64(%rsp), %rbx                 # 8-byte Folded Reload
	movq	-104(%rsp), %r15                # 8-byte Reload
	movq	%r15, %r14
	imulq	%rdi, %r14
	addq	%rbx, %r14
	movq	%r8, %rbx
	movq	40(%rsp), %r8                   # 8-byte Reload
	imulq	%r8, %rbx
	addq	%rbx, %r14
	movq	%r10, %rbx
	movq	32(%rsp), %r10                  # 8-byte Reload
	imulq	%r10, %rbx
	movq	%r11, %rax
	imulq	%rsi, %rax
	addq	%rbx, %rax
	movq	-88(%rsp), %r11                 # 8-byte Reload
	movq	%r11, %rbx
	imulq	%rdx, %rbx
	movq	%r9, %rsi
	imulq	%rbp, %rsi
	movq	%rbp, 136(%rsp)                 # 8-byte Spill
	addq	%rbx, %rsi
	movq	-16(%rsp), %rdx                 # 8-byte Reload
	shrl	$2, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	%rdx, -16(%rsp)                 # 8-byte Spill
	movq	16(%rsp), %r9                   # 8-byte Reload
	movq	%r9, %rdi
	imulq	%rdx, %rdi
	addq	%rsi, %rdi
	addq	%rax, %rdi
	addq	%r14, %rdi
	addq	%rcx, %rdi
	movq	%rdi, 160(%rsp)                 # 8-byte Spill
	movq	(%rsp), %rax                    # 8-byte Reload
	imulq	%r12, %rax
	movq	8(%rsp), %rbx                   # 8-byte Reload
	imulq	%r8, %rbx
	movq	%r8, %r12
	addq	%rax, %rbx
	movq	-40(%rsp), %rax                 # 8-byte Reload
	movl	28(%rax), %edx
	shrq	$7, %rdx
	movq	%rdx, -40(%rsp)                 # 8-byte Spill
	movq	%r9, %rcx
	movq	%r9, %r14
	movq	-8(%rsp), %rax                  # 8-byte Reload
	imulq	%rax, %rcx
	movq	%rcx, 152(%rsp)                 # 8-byte Spill
	imulq	%rdx, %rax
	addq	%rax, %rbx
	movq	-48(%rsp), %rax                 # 8-byte Reload
	imulq	24(%rsp), %rax                  # 8-byte Folded Reload
	addq	%rax, %rbx
	movq	-112(%rsp), %rdx                # 8-byte Reload
	movq	%rdx, %rax
	imulq	%r13, %rax
	movq	%r15, %rcx
	imulq	%r10, %rcx
	addq	%rax, %rcx
	movq	-96(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, %rax
	movq	-64(%rsp), %r8                  # 8-byte Reload
	imulq	%r8, %rax
	addq	%rax, %rcx
	movq	%r11, %r9
	movq	%r11, %rax
	imulq	-128(%rsp), %rax                # 8-byte Folded Reload
	movq	-72(%rsp), %rsi                 # 8-byte Reload
	imulq	%rbp, %rsi
	addq	%rax, %rsi
	movq	48(%rsp), %rax                  # 8-byte Reload
	movl	28(%rax), %ebp
	shrq	$7, %rbp
	movq	-24(%rsp), %rax                 # 8-byte Reload
	movq	-56(%rsp), %r15                 # 8-byte Reload
	imulq	%r15, %rax
	movq	%r14, %r13
	imulq	%rbp, %r13
	addq	%rax, %r13
	movq	-16(%rsp), %r14                 # 8-byte Reload
	movq	%r14, %rax
	movq	-120(%rsp), %r11                # 8-byte Reload
	imulq	%r11, %rax
	addq	%rax, %r13
	addq	%rsi, %r13
	addq	%rcx, %r13
	addq	%rbx, %r13
	movq	%r13, 16(%rsp)                  # 8-byte Spill
	movq	-32(%rsp), %rax                 # 8-byte Reload
	imulq	%r12, %rax
	movq	%rdi, %rcx
	imulq	%rdx, %rcx
	addq	%rax, %rcx
	movq	8(%rsp), %rbx                   # 8-byte Reload
	movq	%rbx, %rax
	imulq	%r8, %rax
	addq	%rax, %rcx
	movq	-80(%rsp), %r12                 # 8-byte Reload
	movq	%r12, %rax
	imulq	%r10, %rax
	movq	-104(%rsp), %r8                 # 8-byte Reload
	movq	%r8, %rsi
	imulq	%r9, %rsi
	addq	%rax, %rsi
	movq	%r15, %rax
	movq	136(%rsp), %r9                  # 8-byte Reload
	imulq	%r9, %rax
	movq	%r11, %rdx
	imulq	%rbp, %rdx
	addq	%rax, %rdx
	movq	-24(%rsp), %r11                 # 8-byte Reload
	movq	%r11, %rax
	movq	-128(%rsp), %r10                # 8-byte Reload
	imulq	%r10, %rax
	addq	%rax, %rdx
	movq	-72(%rsp), %r15                 # 8-byte Reload
	movq	%r15, %rax
	movq	%r14, %rdi
	imulq	%r14, %rax
	addq	%rax, %rdx
	addq	%rsi, %rdx
	addq	%rcx, %rdx
	movq	-40(%rsp), %rsi                 # 8-byte Reload
	movq	24(%rsp), %rax                  # 8-byte Reload
	imulq	%rsi, %rax
	addq	%rax, %rdx
	movq	%rdx, -120(%rsp)                # 8-byte Spill
	movq	-48(%rsp), %rdx                 # 8-byte Reload
	movq	%rdx, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	imulq	%rax, %rcx
	movq	%rcx, -8(%rsp)                  # 8-byte Spill
	imulq	%rsi, %rax
	movq	%rbx, %rcx
	movq	-112(%rsp), %r13                # 8-byte Reload
	imulq	%r13, %rcx
	addq	%rax, %rcx
	movq	%rdx, %rax
	imulq	40(%rsp), %rax                  # 8-byte Folded Reload
	addq	%rax, %rcx
	movq	%rcx, 24(%rsp)                  # 8-byte Spill
	movq	-96(%rsp), %rax                 # 8-byte Reload
	movq	32(%rsp), %r14                  # 8-byte Reload
	imulq	%r14, %rax
	movq	%r8, %rcx
	imulq	%r11, %rcx
	addq	%rax, %rcx
	movq	-32(%rsp), %r11                 # 8-byte Reload
	movq	%r11, %rax
	imulq	-64(%rsp), %rax                 # 8-byte Folded Reload
	addq	%rax, %rcx
	imulq	%r9, %r10
	movq	%rdi, %rsi
	movq	-56(%rsp), %rdi                 # 8-byte Reload
	imulq	%rdi, %rsi
	addq	%r10, %rsi
	movq	-88(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %rax
	imulq	%r12, %rax
	imulq	%rbp, %r15
	addq	%rax, %r15
	addq	%rsi, %r15
	addq	%rcx, %r15
	movq	%r15, -72(%rsp)                 # 8-byte Spill
	movq	%r13, %rax
	imulq	%r11, %rax
	movq	8(%rsp), %r15                   # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %r10
	imulq	%r14, %rdx
	addq	%rax, %rdx
	movq	40(%rsp), %rax                  # 8-byte Reload
	movq	-40(%rsp), %r14                 # 8-byte Reload
	imulq	%r14, %rax
	addq	%rax, %rdx
	movq	-96(%rsp), %r13                 # 8-byte Reload
	movq	%r13, %rax
	imulq	%rbx, %rax
	movq	%r8, %rsi
	imulq	%r9, %rsi
	addq	%rax, %rsi
	movq	-24(%rsp), %rbx                 # 8-byte Reload
	movq	%rbx, %rax
	imulq	%r12, %rax
	movq	%rdi, %rcx
	imulq	%rbp, %rcx
	addq	%rax, %rcx
	movq	-16(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, %rax
	movq	-128(%rsp), %r8                 # 8-byte Reload
	imulq	%r8, %rax
	addq	%rax, %rcx
	addq	%rsi, %rcx
	addq	%rdx, %rcx
	movq	%rcx, -56(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	imulq	%r10, %rax
	movq	%r15, %rcx
	movq	-88(%rsp), %r11                 # 8-byte Reload
	imulq	%r11, %rcx
	addq	%rax, %rcx
	movq	-48(%rsp), %r10                 # 8-byte Reload
	movq	%r10, %rax
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	imulq	%rdx, %rax
	movq	%rax, (%rsp)                    # 8-byte Spill
	imulq	%r14, %rdx
	addq	%rdx, %rcx
	movq	%r12, %rax
	imulq	%r9, %rax
	movq	%r8, %rdx
	imulq	%rbp, %rdx
	addq	%rax, %rdx
	movq	%r13, %rax
	imulq	%rbx, %rax
	addq	%rax, %rdx
	movq	-104(%rsp), %r8                 # 8-byte Reload
	movq	%r8, %rax
	imulq	%rdi, %rax
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	%rdx, -128(%rsp)                # 8-byte Spill
	movq	%r11, %rdx
	movq	%r11, %rax
	movq	-32(%rsp), %r11                 # 8-byte Reload
	imulq	%r11, %rax
	movq	%r13, %rcx
	imulq	%r9, %rcx
	addq	%rax, %rcx
	movq	%r15, %rax
	imulq	%rbx, %rax
	movq	%rbx, %rsi
	addq	%rax, %rcx
	movq	%rdi, %rax
	imulq	%r12, %rax
	imulq	%rbp, %r8
	addq	%rax, %r8
	addq	%rcx, %r8
	movq	%r10, %rcx
	movq	-112(%rsp), %rax                # 8-byte Reload
	imulq	%rax, %rcx
	movq	%rcx, 144(%rsp)                 # 8-byte Spill
	imulq	%r14, %rax
	addq	%rax, %r8
	movq	%r8, -104(%rsp)                 # 8-byte Spill
	movq	%r10, %rax
	movq	%r10, %rbx
	movq	32(%rsp), %rcx                  # 8-byte Reload
	imulq	%rcx, %rax
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	imulq	%r14, %rcx
	movq	%rcx, %rax
	movq	%r15, %rcx
	imulq	%r9, %rcx
	addq	%rax, %rcx
	movq	%r10, %rax
	imulq	%rdx, %rax
	addq	%rax, %rcx
	movq	%rsi, %rax
	imulq	%r11, %rax
	imulq	%rbp, %r12
	addq	%rax, %r12
	movq	%r13, %rax
	imulq	%rdi, %rax
	addq	%rax, %r12
	movq	%r12, -80(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	imulq	%r9, %rax
	movq	%r13, %r10
	imulq	%rbp, %r10
	addq	%rax, %r10
	movq	%r15, %rax
	movq	%r15, %r12
	imulq	%rdi, %rax
	addq	%rax, %r10
	movq	%rdx, %rax
	imulq	%r14, %rax
	addq	%rax, %r10
	movq	%r10, -96(%rsp)                 # 8-byte Spill
	movq	%rbx, %r13
	movq	%rsi, %rax
	imulq	%rsi, %r13
	imulq	%r14, %rax
	imulq	%rbp, %r12
	addq	%rax, %r12
	movq	%rdi, %rdx
	movq	%rdi, %rax
	movq	%r11, %rsi
	imulq	%r11, %rax
	addq	%rax, %r12
	movq	%rbx, %r8
	imulq	%r9, %r8
	movq	%r8, -24(%rsp)                  # 8-byte Spill
	imulq	%r14, %r9
	imulq	%rbp, %rsi
	addq	%r9, %rsi
	movq	%rsi, %r9
	movq	%rbx, %r11
	imulq	%rdi, %r11
	imulq	%r14, %rdx
	imulq	%rbp, %rbx
	addq	%rdx, %rbx
	movq	%rbx, -48(%rsp)                 # 8-byte Spill
	imulq	%rbp, %r14
	movq	128(%rsp), %rax                 # 8-byte Reload
	movq	152(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rax,%rdx), %r8
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	56(%rsp), %rsi                  # 8-byte Reload
	movl	2(%rsi), %edx
	shrl	$5, %edx
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	movq	104(%rsp), %rbx                 # 8-byte Reload
	addq	%rdx, %rbx
	movq	%rax, %r10
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r8
	movq	%r8, 8(%rsp)                    # 8-byte Spill
	movq	120(%rsp), %rax                 # 8-byte Reload
	shrl	$2, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	96(%rsp), %rdx                  # 8-byte Reload
	leaq	(%rdx,%rax), %rdi
	movq	%rdi, -16(%rsp)                 # 8-byte Spill
	leaq	(%rax,%rdx), %r15
	addq	$1048576, %r15                  # imm = 0x100000
	movq	216(%rsp), %rax                 # 8-byte Reload
	addq	%rax, 88(%rsp)                  # 8-byte Folded Spill
	movl	10(%rsi), %eax
	shrl	$4, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	208(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rdx,%rax), %rbp
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	200(%rsp), %rdx                 # 8-byte Reload
	addq	%rdx, 80(%rsp)                  # 8-byte Folded Spill
	movq	%rax, %rdx
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rbp
	movl	15(%rsi), %eax
	shrl	$6, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	192(%rsp), %r8                  # 8-byte Reload
	leaq	(%r8,%rax), %rdi
	movq	%rdi, -40(%rsp)                 # 8-byte Spill
	addq	%r8, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	movq	184(%rsp), %rax                 # 8-byte Reload
	addq	%rax, 72(%rsp)                  # 8-byte Folded Spill
	movzbl	23(%rsi), %eax
	andl	$31, %eax
	shlq	$16, %rax
	movq	176(%rsp), %r8                  # 8-byte Reload
	leaq	(%r8,%rax), %rdi
	movq	%rdi, 104(%rsp)                 # 8-byte Spill
	addq	%r8, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -88(%rsp)                 # 8-byte Spill
	movq	168(%rsp), %rax                 # 8-byte Reload
	addq	%rax, 64(%rsp)                  # 8-byte Folded Spill
	movq	112(%rsp), %rax                 # 8-byte Reload
	shrl	$2, %eax
	andl	$2097151, %eax                  # imm = 0x1FFFFF
	movq	160(%rsp), %rdi                 # 8-byte Reload
	leaq	(%rdi,%rax), %r8
	movq	%r8, 48(%rsp)                   # 8-byte Spill
	addq	%rdi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movl	28(%rsi), %eax
	shrq	$7, %rax
	addq	%rax, 16(%rsp)                  # 8-byte Folded Spill
	movq	-120(%rsp), %rax                # 8-byte Reload
	movq	-8(%rsp), %rsi                  # 8-byte Reload
	leaq	(%rax,%rsi), %rdi
	movq	%rdi, 56(%rsp)                  # 8-byte Spill
	addq	%rsi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -120(%rsp)                # 8-byte Spill
	movq	24(%rsp), %rax                  # 8-byte Reload
	addq	%rax, -72(%rsp)                 # 8-byte Folded Spill
	movq	144(%rsp), %rax                 # 8-byte Reload
	addq	%rax, -128(%rsp)                # 8-byte Folded Spill
	addq	%rcx, -80(%rsp)                 # 8-byte Folded Spill
	movq	-96(%rsp), %rax                 # 8-byte Reload
	leaq	(%rax,%r13), %rsi
	addq	%r13, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	addq	-24(%rsp), %r12                 # 8-byte Folded Reload
	movq	%rax, %rcx
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rsi
	movq	%rsi, 96(%rsp)                  # 8-byte Spill
	leaq	(%r9,%r11), %rsi
	leaq	(%r11,%r9), %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %r9
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rsi
	leaq	1048576(%r14), %rdi
	movq	%rdi, %r8
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r14
	shrq	$21, %r10
	leaq	(%rbx,%r10), %r11
	leaq	(%r10,%rbx), %rdi
	addq	$1048576, %rdi                  # imm = 0x100000
	movq	%rdi, %rax
	shrq	$21, %rax
	addq	-16(%rsp), %rax                 # 8-byte Folded Reload
	movq	%r15, %r10
	andq	$-2097152, %r15                 # imm = 0xFFE00000
	subq	%r15, %rax
	movq	%rax, -24(%rsp)                 # 8-byte Spill
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r11
	movq	%r11, -16(%rsp)                 # 8-byte Spill
	shrq	$21, %r10
	movq	88(%rsp), %rax                  # 8-byte Reload
	leaq	(%rax,%r10), %r11
	leaq	(%r10,%rax), %rdi
	addq	$1048576, %rdi                  # imm = 0x100000
	movq	%rdi, %rax
	shrq	$21, %rax
	addq	%rbp, %rax
	movq	%rax, 32(%rsp)                  # 8-byte Spill
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r11
	movq	%r11, -32(%rsp)                 # 8-byte Spill
	shrq	$21, %rdx
	movq	80(%rsp), %rax                  # 8-byte Reload
	leaq	(%rax,%rdx), %rdi
	addq	%rax, %rdx
	addq	$1048576, %rdx                  # imm = 0x100000
	movq	%rdx, -8(%rsp)                  # 8-byte Spill
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rdi
	movq	%rdi, -96(%rsp)                 # 8-byte Spill
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	shrq	$21, %rdx
	movq	72(%rsp), %rax                  # 8-byte Reload
	leaq	(%rax,%rdx), %rdi
	movq	%rdi, 120(%rsp)                 # 8-byte Spill
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, 128(%rsp)                 # 8-byte Spill
	shrq	$21, %rax
	addq	104(%rsp), %rax                 # 8-byte Folded Reload
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	-88(%rsp), %rdx                 # 8-byte Reload
	shrq	$21, %rdx
	movq	64(%rsp), %rax                  # 8-byte Reload
	leaq	(%rax,%rdx), %rdi
	movq	%rdi, 80(%rsp)                  # 8-byte Spill
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, 72(%rsp)                  # 8-byte Spill
	movq	-112(%rsp), %rdx                # 8-byte Reload
	shrq	$21, %rdx
	movq	16(%rsp), %rax                  # 8-byte Reload
	leaq	(%rax,%rdx), %rdi
	movq	%rdi, 88(%rsp)                  # 8-byte Spill
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	-120(%rsp), %rdx                # 8-byte Reload
	shrq	$21, %rdx
	movq	-72(%rsp), %rax                 # 8-byte Reload
	leaq	(%rax,%rdx), %rdi
	movq	%rdi, 64(%rsp)                  # 8-byte Spill
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	-56(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rax,%rdx), %r10
	addq	$1048576, %r10                  # imm = 0x100000
	movq	%r10, %rdi
	shrq	$21, %rdi
	movq	-128(%rsp), %rax                # 8-byte Reload
	leaq	(%rax,%rdi), %rdx
	movq	%rdx, -72(%rsp)                 # 8-byte Spill
	leaq	(%rdi,%rax), %rbx
	addq	$1048576, %rbx                  # imm = 0x100000
	movq	-104(%rsp), %rax                # 8-byte Reload
	movq	40(%rsp), %rdx                  # 8-byte Reload
	leaq	(%rdx,%rax), %r11
	addq	$1048576, %r11                  # imm = 0x100000
	movq	%r11, %rdi
	shrq	$21, %rdi
	movq	-80(%rsp), %r13                 # 8-byte Reload
	leaq	(%rdi,%r13), %r15
	addq	%r13, %rdi
	addq	$1048576, %rdi                  # imm = 0x100000
	movq	%rdi, %r13
	shrq	$21, %r13
	addq	96(%rsp), %r13                  # 8-byte Folded Reload
	movq	%r13, -80(%rsp)                 # 8-byte Spill
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r15
	movq	%r15, -128(%rsp)                # 8-byte Spill
	shrq	$21, %rcx
	leaq	(%r12,%rcx), %rbp
	leaq	(%rcx,%r12), %r15
	addq	$1048576, %r15                  # imm = 0x100000
	movq	%r15, %rcx
	shrq	$21, %rcx
	addq	%rsi, %rcx
	andq	$-2097152, %r15                 # imm = 0xFFE00000
	subq	%r15, %rbp
	shrq	$21, %r9
	movq	-48(%rsp), %rdi                 # 8-byte Reload
	leaq	(%rdi,%r9), %rsi
	leaq	(%r9,%rdi), %r15
	addq	$1048576, %r15                  # imm = 0x100000
	movq	%r15, %r9
	shrq	$21, %r9
	addq	%r14, %r9
	andq	$-2097152, %r15                 # imm = 0xFFE00000
	subq	%r15, %rsi
	shrq	$21, %r8
	imulq	$136657, %r8, %r14              # imm = 0x215D1
	addq	-72(%rsp), %r14                 # 8-byte Folded Reload
	imulq	$-683901, %r8, %r15             # imm = 0xFFF59083
	addq	%rdx, %r15
	addq	%rax, %r15
	andq	$-2097152, %r11                 # imm = 0xFFE00000
	subq	%r11, %r15
	imulq	$666643, %r8, %r11              # imm = 0xA2C13
	imulq	$470296, %r9, %r13              # imm = 0x72D18
	addq	%r11, %r13
	imulq	$470296, %r8, %r11              # imm = 0x72D18
	imulq	$654183, %r9, %rax              # imm = 0x9FB67
	addq	%r11, %rax
	imulq	$654183, %r8, %r11              # imm = 0x9FB67
	imulq	$-997805, %r9, %rdx             # imm = 0xFFF0C653
	addq	%r11, %rdx
	imulq	$-683901, %r9, %rdi             # imm = 0xFFF59083
	addq	%r14, %rdi
	movq	%rbx, -48(%rsp)                 # 8-byte Spill
	andq	$-2097152, %rbx                 # imm = 0xFFE00000
	subq	%rbx, %rdi
	movq	%rdi, -72(%rsp)                 # 8-byte Spill
	imulq	$666643, %r9, %rbx              # imm = 0xA2C13
	imulq	$470296, %rsi, %r14             # imm = 0x72D18
	addq	%rbx, %r14
	imulq	$654183, %rsi, %r12             # imm = 0x9FB67
	addq	%r13, %r12
	imulq	$-997805, %rsi, %rbx            # imm = 0xFFF0C653
	addq	56(%rsp), %rbx                  # 8-byte Folded Reload
	addq	%rax, %rbx
	imulq	$136657, %rsi, %rdi             # imm = 0x215D1
	addq	%rdx, %rdi
	imulq	$-997805, %r8, %rax             # imm = 0xFFF0C653
	addq	(%rsp), %rax                    # 8-byte Folded Reload
	addq	-56(%rsp), %rax                 # 8-byte Folded Reload
	imulq	$136657, %r9, %rdx              # imm = 0x215D1
	addq	%rdx, %rax
	imulq	$-683901, %rsi, %rdx            # imm = 0xFFF59083
	addq	%rdx, %rax
	andq	$-2097152, %r10                 # imm = 0xFFE00000
	subq	%r10, %rax
	imulq	$666643, %rsi, %rdx             # imm = 0xA2C13
	imulq	$470296, %rcx, %r9              # imm = 0x72D18
	addq	%rdx, %r9
	imulq	$654183, %rcx, %rdx             # imm = 0x9FB67
	addq	%r14, %rdx
	imulq	$-997805, %rcx, %rsi            # imm = 0xFFF0C653
	addq	%r12, %rsi
	imulq	$136657, %rcx, %r12             # imm = 0x215D1
	addq	%rbx, %r12
	imulq	$-683901, %rcx, %r13            # imm = 0xFFF59083
	addq	%rdi, %r13
	addq	64(%rsp), %r13                  # 8-byte Folded Reload
	movq	112(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, %r11
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %r13
	imulq	$666643, %rbp, %r8              # imm = 0xA2C13
	addq	120(%rsp), %r8                  # 8-byte Folded Reload
	imulq	$666643, %rcx, %rcx             # imm = 0xA2C13
	imulq	$470296, %rbp, %r10             # imm = 0x72D18
	addq	%rcx, %r10
	imulq	$654183, %rbp, %r14             # imm = 0x9FB67
	addq	%r9, %r14
	imulq	$-997805, %rbp, %rbx            # imm = 0xFFF0C653
	addq	%rdx, %rbx
	imulq	$136657, %rbp, %r9              # imm = 0x215D1
	addq	%rsi, %r9
	imulq	$-683901, %rbp, %rcx            # imm = 0xFFF59083
	addq	%r12, %rcx
	movq	-120(%rsp), %rdx                # 8-byte Reload
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rcx
	movq	-80(%rsp), %rbp                 # 8-byte Reload
	imulq	$666643, %rbp, %rdi             # imm = 0xA2C13
	addq	-40(%rsp), %rdi                 # 8-byte Folded Reload
	movq	-64(%rsp), %rdx                 # 8-byte Reload
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %rdi
	imulq	$470296, %rbp, %rdx             # imm = 0x72D18
	addq	%r8, %rdx
	movq	128(%rsp), %rsi                 # 8-byte Reload
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %rdx
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	imulq	$654183, %rbp, %r12             # imm = 0x9FB67
	addq	%r10, %r12
	addq	24(%rsp), %r12                  # 8-byte Folded Reload
	movq	-88(%rsp), %rdx                 # 8-byte Reload
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %r12
	imulq	$-997805, %rbp, %r8             # imm = 0xFFF0C653
	addq	80(%rsp), %r8                   # 8-byte Folded Reload
	addq	%r14, %r8
	movq	72(%rsp), %rdx                  # 8-byte Reload
	movq	%rdx, %rsi
	andq	$-2097152, %rdx                 # imm = 0xFFE00000
	subq	%rdx, %r8
	shrq	$21, %rsi
	imulq	$136657, %rbp, %rdx             # imm = 0x215D1
	addq	%rsi, %rdx
	addq	%rbx, %rdx
	addq	48(%rsp), %rdx                  # 8-byte Folded Reload
	movq	-112(%rsp), %rsi                # 8-byte Reload
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %rdx
	imulq	$-683901, %rbp, %rbx            # imm = 0xFFF59083
	addq	88(%rsp), %rbx                  # 8-byte Folded Reload
	addq	%r9, %rbx
	movq	16(%rsp), %r9                   # 8-byte Reload
	movq	%r9, %rsi
	andq	$-2097152, %r9                  # imm = 0xFFE00000
	subq	%r9, %rbx
	movq	-8(%rsp), %r9                   # 8-byte Reload
	shrq	$21, %r9
	leaq	(%rdi,%r9), %r10
	movq	%r10, -112(%rsp)                # 8-byte Spill
	addq	%r9, %rdi
	addq	$1048576, %rdi                  # imm = 0x100000
	movq	%rdi, -88(%rsp)                 # 8-byte Spill
	shrq	$21, %rsi
	leaq	(%rcx,%rsi), %r10
	addq	%rcx, %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%rsi, %rcx
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %r10
	shrq	$21, %r11
	leaq	(%rax,%r11), %rbp
	leaq	(%rax,%r11), %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%rsi, %rdi
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %rbp
	movq	-48(%rsp), %rax                 # 8-byte Reload
	shrq	$21, %rax
	leaq	(%r15,%rax), %r9
	leaq	(%r15,%rax), %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%rsi, %rax
	sarq	$21, %rax
	addq	-128(%rsp), %rax                # 8-byte Folded Reload
	movq	%rax, %r14
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %r9
	leaq	1048576(%r12), %rsi
	movq	%rsi, -56(%rsp)                 # 8-byte Spill
	sarq	$21, %rsi
	leaq	(%r8,%rsi), %rax
	movq	%rax, -120(%rsp)                # 8-byte Spill
	leaq	(%r8,%rsi), %r15
	addq	$1048576, %r15                  # imm = 0x100000
	leaq	1048576(%rdx), %rsi
	movq	%rsi, %r8
	sarq	$21, %r8
	leaq	(%rbx,%r8), %rax
	addq	%rbx, %r8
	addq	$1048576, %r8                   # imm = 0x100000
	movq	%r8, %r11
	sarq	$21, %r11
	addq	%r10, %r11
	andq	$-2097152, %r8                  # imm = 0xFFE00000
	subq	%r8, %rax
	movq	%rax, -48(%rsp)                 # 8-byte Spill
	sarq	$21, %rcx
	leaq	(%rcx,%r13), %rbx
	addq	%r13, %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	movq	%rcx, %r13
	sarq	$21, %r13
	addq	%rbp, %r13
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %rbx
	sarq	$21, %rdi
	movq	-72(%rsp), %rax                 # 8-byte Reload
	leaq	(%rax,%rdi), %rbp
	addq	%rdi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %r8
	sarq	$21, %r8
	addq	%r9, %r8
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rbp
	imulq	$666643, %r14, %rax             # imm = 0xA2C13
	addq	-96(%rsp), %rax                 # 8-byte Folded Reload
	imulq	$470296, %r14, %rcx             # imm = 0x72D18
	addq	-112(%rsp), %rcx                # 8-byte Folded Reload
	movq	-88(%rsp), %rdi                 # 8-byte Reload
	movq	%rdi, -128(%rsp)                # 8-byte Spill
	andq	$-2097152, %rdi                 # imm = 0xFFE00000
	subq	%rdi, %rcx
	imulq	$136657, %r14, %rdi             # imm = 0x215D1
	addq	-120(%rsp), %rdi                # 8-byte Folded Reload
	imulq	$-683901, %r14, %r9             # imm = 0xFFF59083
	movq	%r14, -80(%rsp)                 # 8-byte Spill
	addq	%rdx, %r9
	movq	%r15, %rdx
	sarq	$21, %rdx
	addq	%rdx, %r9
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %r9
	movq	%r9, -96(%rsp)                  # 8-byte Spill
	imulq	$666643, %r8, %rdx              # imm = 0xA2C13
	addq	32(%rsp), %rdx                  # 8-byte Folded Reload
	imulq	$470296, %r8, %rsi              # imm = 0x72D18
	addq	%rax, %rsi
	imulq	$654183, %r8, %rax              # imm = 0x9FB67
	addq	%rcx, %rax
	imulq	$-997805, %r14, %rcx            # imm = 0xFFF0C653
	imulq	$136657, %r8, %r9               # imm = 0x215D1
	addq	%rcx, %r9
	imulq	$-683901, %r8, %rcx             # imm = 0xFFF59083
	addq	%rdi, %rcx
	andq	$-2097152, %r15                 # imm = 0xFFE00000
	subq	%r15, %rcx
	movq	%rcx, -112(%rsp)                # 8-byte Spill
	imulq	$666643, %rbp, %rdi             # imm = 0xA2C13
	addq	-32(%rsp), %rdi                 # 8-byte Folded Reload
	imulq	$470296, %rbp, %rcx             # imm = 0x72D18
	addq	%rdx, %rcx
	imulq	$654183, %rbp, %rdx             # imm = 0x9FB67
	addq	%rsi, %rdx
	imulq	$-997805, %rbp, %r10            # imm = 0xFFF0C653
	addq	%rax, %r10
	movq	-128(%rsp), %rsi                # 8-byte Reload
	sarq	$21, %rsi
	movq	%rsi, -128(%rsp)                # 8-byte Spill
	movq	-104(%rsp), %rax                # 8-byte Reload
	addq	%rsi, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -120(%rsp)                # 8-byte Spill
	sarq	$21, %rax
	imulq	$-683901, %rbp, %r15            # imm = 0xFFF59083
	addq	%rax, %r15
	addq	%r9, %r15
	addq	%r12, %r15
	movq	-56(%rsp), %rax                 # 8-byte Reload
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r15
	imulq	$666643, %r13, %rsi             # imm = 0xA2C13
	addq	-24(%rsp), %rsi                 # 8-byte Folded Reload
	imulq	$470296, %r13, %rax             # imm = 0x72D18
	addq	%rdi, %rax
	imulq	$654183, %r13, %r9              # imm = 0x9FB67
	addq	%rcx, %r9
	imulq	$-997805, %r13, %r12            # imm = 0xFFF0C653
	addq	%rdx, %r12
	imulq	$666643, %rbx, %rdx             # imm = 0xA2C13
	addq	-16(%rsp), %rdx                 # 8-byte Folded Reload
	imulq	$654183, %rbx, %rdi             # imm = 0x9FB67
	addq	%rax, %rdi
	imulq	$136657, %rbx, %r14             # imm = 0x215D1
	addq	%r12, %r14
	imulq	$136657, %r13, %rcx             # imm = 0x215D1
	addq	%r10, %rcx
	imulq	$654183, %r11, %r10             # imm = 0x9FB67
	addq	%rsi, %r10
	imulq	$136657, %r11, %rax             # imm = 0x215D1
	addq	%r9, %rax
	imulq	$666643, %r11, %rsi             # imm = 0xA2C13
	movq	8(%rsp), %r9                    # 8-byte Reload
	leaq	(%rsi,%r9), %r12
	addq	%rsi, %r9
	addq	$1048576, %r9                   # imm = 0x100000
	imulq	$470296, %r11, %rsi             # imm = 0x72D18
	addq	%rdx, %rsi
	movq	%r9, %rdx
	andq	$-2097152, %r9                  # imm = 0xFFE00000
	subq	%r9, %r12
	movq	%r12, -88(%rsp)                 # 8-byte Spill
	imulq	$470296, %rbx, %r9              # imm = 0x72D18
	leaq	(%r10,%r9), %r12
	movq	%r12, -56(%rsp)                 # 8-byte Spill
	leaq	(%r9,%r10), %r12
	addq	$1048576, %r12                  # imm = 0x100000
	imulq	$-997805, %r11, %r9             # imm = 0xFFF0C653
	addq	%rdi, %r9
	imulq	$-997805, %rbx, %rdi            # imm = 0xFFF0C653
	leaq	(%rax,%rdi), %r10
	movq	%r10, -72(%rsp)                 # 8-byte Spill
	leaq	(%rdi,%rax), %r10
	addq	$1048576, %r10                  # imm = 0x100000
	movq	%r10, -64(%rsp)                 # 8-byte Spill
	imulq	$-683901, %r11, %r11            # imm = 0xFFF59083
	addq	%r14, %r11
	imulq	$-683901, %rbx, %rax            # imm = 0xFFF59083
	leaq	(%rcx,%rax), %rdi
	addq	%rax, %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	imulq	$654183, -80(%rsp), %rax        # 8-byte Folded Reload
                                        # imm = 0x9FB67
	addq	-104(%rsp), %rax                # 8-byte Folded Reload
	addq	-128(%rsp), %rax                # 8-byte Folded Reload
	imulq	$-997805, %r8, %r8              # imm = 0xFFF0C653
	addq	%r8, %rax
	imulq	$136657, %rbp, %r8              # imm = 0x215D1
	addq	%r8, %rax
	movq	-120(%rsp), %r8                 # 8-byte Reload
	andq	$-2097152, %r8                  # imm = 0xFFE00000
	subq	%r8, %rax
	imulq	$-683901, %r13, %r8             # imm = 0xFFF59083
	addq	%r8, %rax
	movq	%rcx, %r8
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %rdi
	leaq	1048576(%r15), %rcx
	movq	%rcx, %rbp
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %r15
	sarq	$21, %rdx
	leaq	(%rsi,%rdx), %r14
	leaq	(%rdx,%rsi), %rcx
	addq	$1048576, %rcx                  # imm = 0x100000
	movq	%r12, %rdx
	sarq	$21, %rdx
	leaq	(%r9,%rdx), %rsi
	movq	%rsi, -120(%rsp)                # 8-byte Spill
	leaq	(%rdx,%r9), %rsi
	addq	$1048576, %rsi                  # imm = 0x100000
	movq	%r10, %rdx
	sarq	$21, %rdx
	leaq	(%r11,%rdx), %r9
	movq	%r9, -80(%rsp)                  # 8-byte Spill
	leaq	(%rdx,%r11), %rbx
	addq	$1048576, %rbx                  # imm = 0x100000
	movq	%rbx, %r11
	sarq	$21, %r11
	addq	%rdi, %r11
	sarq	$21, %r8
	leaq	(%rax,%r8), %r10
	addq	%r8, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %r9
	sarq	$21, %r9
	addq	%r15, %r9
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r10
	sarq	$21, %rbp
	movq	-112(%rsp), %rax                # 8-byte Reload
	leaq	(%rax,%rbp), %r13
	addq	%rbp, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, -128(%rsp)                # 8-byte Spill
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %r13
	movq	-96(%rsp), %rax                 # 8-byte Reload
	leaq	1048576(%rax), %r8
	movq	%r8, %rax
	sarq	$21, %rax
	movq	-48(%rsp), %rdx                 # 8-byte Reload
	leaq	(%rdx,%rax), %rdi
	addq	%rdx, %rax
	addq	$1048576, %rax                  # imm = 0x100000
	movq	%rax, %rbp
	andq	$-2097152, %rax                 # imm = 0xFFE00000
	subq	%rax, %rdi
	movq	%rdi, -104(%rsp)                # 8-byte Spill
	sarq	$21, %rbp
	imulq	$666643, %rbp, %rdx             # imm = 0xA2C13
	addq	-88(%rsp), %rdx                 # 8-byte Folded Reload
	imulq	$470296, %rbp, %rax             # imm = 0x72D18
	addq	%r14, %rax
	movq	%rcx, %r14
	andq	$-2097152, %rcx                 # imm = 0xFFE00000
	subq	%rcx, %rax
	imulq	$654183, %rbp, %rdi             # imm = 0x9FB67
	addq	-56(%rsp), %rdi                 # 8-byte Folded Reload
	andq	$-2097152, %r12                 # imm = 0xFFE00000
	subq	%r12, %rdi
	sarq	$21, %r14
	addq	%r14, %rdi
	imulq	$-997805, %rbp, %r15            # imm = 0xFFF0C653
	addq	-120(%rsp), %r15                # 8-byte Folded Reload
	movq	%rsi, %rcx
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %r15
	imulq	$136657, %rbp, %r12             # imm = 0x215D1
	addq	-72(%rsp), %r12                 # 8-byte Folded Reload
	movq	-64(%rsp), %rsi                 # 8-byte Reload
	andq	$-2097152, %rsi                 # imm = 0xFFE00000
	subq	%rsi, %r12
	sarq	$21, %rcx
	addq	%rcx, %r12
	imulq	$-683901, %rbp, %r14            # imm = 0xFFF59083
	addq	-80(%rsp), %r14                 # 8-byte Folded Reload
	andq	$-2097152, %rbx                 # imm = 0xFFE00000
	subq	%rbx, %r14
	movq	%rdx, %rcx
	sarq	$21, %rcx
	addq	%rax, %rcx
	movq	%rcx, %rsi
	sarq	$21, %rsi
	addq	%rdi, %rsi
	movq	%rsi, %rdi
	sarq	$21, %rdi
	addq	%r15, %rdi
	movq	%rdi, %r15
	sarq	$21, %r15
	addq	%r12, %r15
	movq	%r15, %rbp
	sarq	$21, %rbp
	addq	%r14, %rbp
	movq	%rbp, %rbx
	sarq	$21, %rbx
	addq	%r11, %rbx
	movq	%rbx, %r14
	sarq	$21, %r14
	addq	%r10, %r14
	movq	%r14, %r11
	sarq	$21, %r11
	addq	%r9, %r11
	movq	%r11, %r12
	sarq	$21, %r12
	addq	%r13, %r12
	andq	$-2097152, %r8                  # imm = 0xFFE00000
	movq	-128(%rsp), %r9                 # 8-byte Reload
	sarq	$21, %r9
	andl	$2097151, %edx                  # imm = 0x1FFFFF
	andl	$2097151, %ecx                  # imm = 0x1FFFFF
	andl	$2097151, %esi                  # imm = 0x1FFFFF
	andl	$2097151, %edi                  # imm = 0x1FFFFF
	andl	$2097151, %r15d                 # imm = 0x1FFFFF
	addq	-96(%rsp), %r9                  # 8-byte Folded Reload
	movq	%r12, %rax
	sarq	$21, %rax
	subq	%r8, %r9
	addq	%rax, %r9
	movq	%r9, %r13
	movq	%r9, %r10
	sarq	$21, %r13
	addq	-104(%rsp), %r13                # 8-byte Folded Reload
	movq	%r13, %r8
	sarq	$21, %r8
	imulq	$666643, %r8, %rax              # imm = 0xA2C13
	addq	%rdx, %rax
	imulq	$470296, %r8, %rdx              # imm = 0x72D18
	addq	%rcx, %rdx
	imulq	$654183, %r8, %rcx              # imm = 0x9FB67
	addq	%rsi, %rcx
	imulq	$-997805, %r8, %r9              # imm = 0xFFF0C653
	addq	%rdi, %r9
	imulq	$136657, %r8, %rdi              # imm = 0x215D1
	addq	%r15, %rdi
	andl	$2097151, %ebp                  # imm = 0x1FFFFF
	andl	$2097151, %ebx                  # imm = 0x1FFFFF
	andl	$2097151, %r14d                 # imm = 0x1FFFFF
	andl	$2097151, %r11d                 # imm = 0x1FFFFF
	andl	$2097151, %r12d                 # imm = 0x1FFFFF
	andl	$2097151, %r10d                 # imm = 0x1FFFFF
	movq	%r10, -128(%rsp)                # 8-byte Spill
	andl	$2097151, %r13d                 # imm = 0x1FFFFF
	imulq	$-683901, %r8, %rsi             # imm = 0xFFF59083
	addq	%rbp, %rsi
	movq	%rax, %rbp
	sarq	$21, %rbp
	addq	%rdx, %rbp
	movq	%rbp, %r15
	sarq	$21, %r15
	addq	%rcx, %r15
	movq	%r15, %r10
	sarq	$21, %r10
	addq	%r9, %r10
	movq	%r10, %r9
	sarq	$21, %r9
	addq	%rdi, %r9
	movq	%r9, %r8
	sarq	$21, %r8
	addq	%rsi, %r8
	movq	%r8, %rdi
	sarq	$21, %rdi
	addq	%rbx, %rdi
	movq	%rdi, %rsi
	sarq	$21, %rsi
	addq	%r14, %rsi
	movq	%rsi, %rbx
	sarq	$21, %rbx
	addq	%r11, %rbx
	movq	%rbx, %rdx
	sarq	$21, %rdx
	addq	%r12, %rdx
	movq	%rdx, %r12
	sarq	$21, %r12
	addq	-128(%rsp), %r12                # 8-byte Folded Reload
	movq	%r12, %r11
	sarq	$21, %r11
	addq	%r13, %r11
	movq	224(%rsp), %rcx                 # 8-byte Reload
	movb	%al, (%rcx)
	movb	%ah, 1(%rcx)
	shrl	$16, %eax
	andl	$31, %eax
	movl	%ebp, %r14d
	shll	$5, %r14d
	orl	%eax, %r14d
	movb	%r14b, 2(%rcx)
	movq	%rbp, %rax
	shrq	$3, %rax
	movb	%al, 3(%rcx)
	movq	%rbp, %rax
	shrq	$11, %rax
	movb	%al, 4(%rcx)
	shrl	$19, %ebp
	andl	$3, %ebp
	leal	(%rbp,%r15,4), %eax
	movb	%al, 5(%rcx)
	movq	%r15, %rax
	shrq	$6, %rax
	movb	%al, 6(%rcx)
	movl	%r10d, %eax
	shll	$7, %eax
	shrl	$14, %r15d
	andl	$127, %r15d
	orl	%eax, %r15d
	movb	%r15b, 7(%rcx)
	movq	%r10, %rax
	shrq	%rax
	movb	%al, 8(%rcx)
	movq	%r10, %rax
	shrq	$9, %rax
	movb	%al, 9(%rcx)
	movl	%r9d, %eax
	shll	$4, %eax
	shrl	$17, %r10d
	andl	$15, %r10d
	orl	%eax, %r10d
	movb	%r10b, 10(%rcx)
	movq	%r9, %rax
	shrq	$4, %rax
	movb	%al, 11(%rcx)
	movq	%r9, %rax
	shrq	$12, %rax
	movb	%al, 12(%rcx)
	shrl	$20, %r9d
	andl	$1, %r9d
	leal	(%r9,%r8,2), %eax
	movb	%al, 13(%rcx)
	movq	%r8, %rax
	shrq	$7, %rax
	movb	%al, 14(%rcx)
	movl	%edi, %eax
	shll	$6, %eax
	shrl	$15, %r8d
	andl	$63, %r8d
	orl	%eax, %r8d
	movb	%r8b, 15(%rcx)
	movq	%rdi, %rax
	shrq	$2, %rax
	movb	%al, 16(%rcx)
	movq	%rdi, %rax
	shrq	$10, %rax
	movb	%al, 17(%rcx)
	shrl	$18, %edi
	andl	$7, %edi
	leal	(%rdi,%rsi,8), %eax
	movb	%al, 18(%rcx)
	movq	%rsi, %rax
	shrq	$5, %rax
	movb	%al, 19(%rcx)
	shrq	$13, %rsi
	movb	%sil, 20(%rcx)
	movb	%bl, 21(%rcx)
	movb	%bh, 22(%rcx)
	shrl	$16, %ebx
	andl	$31, %ebx
	movl	%edx, %eax
	shll	$5, %eax
	orl	%ebx, %eax
	movb	%al, 23(%rcx)
	movq	%rdx, %rax
	shrq	$3, %rax
	movb	%al, 24(%rcx)
	movq	%rdx, %rax
	shrq	$11, %rax
	movb	%al, 25(%rcx)
	shrl	$19, %edx
	andl	$3, %edx
	leal	(%rdx,%r12,4), %eax
	movb	%al, 26(%rcx)
	movq	%r12, %rax
	shrq	$6, %rax
	movb	%al, 27(%rcx)
	movl	%r11d, %eax
	shll	$7, %eax
	shrl	$14, %r12d
	andl	$127, %r12d
	orl	%eax, %r12d
	movb	%r12b, 28(%rcx)
	movq	%r11, %rax
	shrq	%rax
	movb	%al, 29(%rcx)
	movq	%r11, %rax
	shrq	$9, %rax
	movb	%al, 30(%rcx)
	shrq	$17, %r11
	movb	%r11b, 31(%rcx)
	addq	$232, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end16:
	.size	sc_muladd, .Lfunc_end16-sc_muladd
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	xorl	%ebp, %ebp
	leaq	signature(%rip), %rbx
	leaq	message(%rip), %r14
	leaq	private_key(%rip), %r15
	.p2align	4, 0x90
.LBB17_1:                               # =>This Inner Loop Header: Depth=1
	movl	%ebp, %eax
	andl	$15, %eax
	movl	$256, %edx                      # imm = 0x100
	subl	%eax, %edx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	callq	ED25519_sign
	incl	%ebp
	cmpl	$100, %ebp
	jne	.LBB17_1
# %bb.2:
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end17:
	.size	main, .Lfunc_end17-main
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function fe_mul_impl
.LCPI18_0:
	.quad	2251799813685247                # 0x7ffffffffffff
	.quad	2251799813685247                # 0x7ffffffffffff
	.text
	.p2align	4, 0x90
	.type	fe_mul_impl,@function
fe_mul_impl:                            # @fe_mul_impl
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$192, %rsp
	movq	%rdx, %r11
	movq	32(%rsi), %r15
	movq	%rsi, %r9
	movq	32(%rdx), %rcx
	movq	%rcx, -104(%rsp)                # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %rbx
	movq	%rbx, %rax
	mulq	%r15
	movq	%rax, 176(%rsp)                 # 8-byte Spill
	movq	%rdx, 184(%rsp)                 # 8-byte Spill
	movq	24(%r11), %rcx
	movq	%rcx, -120(%rsp)                # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %r13
	movq	%r13, %rax
	mulq	%r15
	movq	%rax, 112(%rsp)                 # 8-byte Spill
	movq	%rdx, 128(%rsp)                 # 8-byte Spill
	movq	16(%r11), %rcx
	movq	%rcx, -128(%rsp)                # 8-byte Spill
	leaq	(%rcx,%rcx,8), %rax
	leaq	(%rcx,%rax,2), %r8
	movq	%r8, %rax
	mulq	%r15
	movq	%rax, 88(%rsp)                  # 8-byte Spill
	movq	%rdx, 96(%rsp)                  # 8-byte Spill
	movq	8(%r11), %rsi
	leaq	(%rsi,%rsi,8), %rax
	leaq	(%rsi,%rax,2), %rax
	mulq	%r15
	movq	%rax, (%rsp)                    # 8-byte Spill
	movq	%rdx, 24(%rsp)                  # 8-byte Spill
	movq	%r9, -112(%rsp)                 # 8-byte Spill
	movq	24(%r9), %r14
	movq	%r14, %rax
	mulq	%rbx
	movq	%rdx, %rbp
	movq	%rax, 8(%rsp)                   # 8-byte Spill
	movq	%r14, %rax
	mulq	%r13
	movq	%rdx, %r10
	movq	%rax, -96(%rsp)                 # 8-byte Spill
	movq	%r14, %rax
	mulq	%r8
	movq	%rax, -24(%rsp)                 # 8-byte Spill
	movq	%rdx, %r12
	movq	16(%r9), %r8
	movq	%r8, %rax
	mulq	%rbx
	movq	%rdx, 64(%rsp)                  # 8-byte Spill
	movq	%rax, 48(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%r13
	movq	%rax, -48(%rsp)                 # 8-byte Spill
	movq	%rdx, -40(%rsp)                 # 8-byte Spill
	movq	8(%r9), %r13
	movq	%r13, %rax
	mulq	%rbx
	movq	%rax, -64(%rsp)                 # 8-byte Spill
	movq	%rdx, -56(%rsp)                 # 8-byte Spill
	movq	(%r11), %r11
	movq	%r11, %rax
	mulq	%r15
	movq	%rdx, 168(%rsp)                 # 8-byte Spill
	movq	%rax, 160(%rsp)                 # 8-byte Spill
	movq	%r14, %rax
	mulq	%rsi
	movq	%rdx, 152(%rsp)                 # 8-byte Spill
	movq	%rax, 144(%rsp)                 # 8-byte Spill
	movq	%r11, %rax
	mulq	%r14
	movq	%rdx, 136(%rsp)                 # 8-byte Spill
	movq	%rax, 120(%rsp)                 # 8-byte Spill
	movq	%r8, %rax
	movq	-128(%rsp), %r9                 # 8-byte Reload
	mulq	%r9
	movq	%rdx, %rbx
	movq	%rax, -8(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%rsi
	movq	%rdx, %r15
	movq	%rax, %r14
	movq	%r11, %rax
	mulq	%r8
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	movq	-120(%rsp), %rcx                # 8-byte Reload
	mulq	%rcx
	movq	%rdx, 104(%rsp)                 # 8-byte Spill
	movq	%rax, 80(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%r9
	movq	%rdx, 72(%rsp)                  # 8-byte Spill
	movq	%rax, 56(%rsp)                  # 8-byte Spill
	movq	%r13, %rax
	mulq	%rsi
	movq	%rax, -32(%rsp)                 # 8-byte Spill
	movq	%rdx, -16(%rsp)                 # 8-byte Spill
	movq	-112(%rsp), %rax                # 8-byte Reload
	movq	(%rax), %r8
	movq	%r11, %rax
	mulq	%r13
	movq	%rdx, -72(%rsp)                 # 8-byte Spill
	movq	%rax, -80(%rsp)                 # 8-byte Spill
	movq	%r8, %rax
	mulq	-104(%rsp)                      # 8-byte Folded Reload
	movq	%rdx, -104(%rsp)                # 8-byte Spill
	movq	%rax, -112(%rsp)                # 8-byte Spill
	movq	%r8, %rax
	mulq	%rcx
	movq	%rdx, -120(%rsp)                # 8-byte Spill
	movq	%rax, 40(%rsp)                  # 8-byte Spill
	movq	%r8, %rax
	mulq	%r9
	movq	%rdx, %r9
	movq	%rax, -128(%rsp)                # 8-byte Spill
	movq	%r8, %rax
	mulq	%rsi
	movq	%rdx, -88(%rsp)                 # 8-byte Spill
	movq	%rax, %r13
	movq	%r8, %rax
	mulq	%r11
	movq	-24(%rsp), %r8                  # 8-byte Reload
	addq	(%rsp), %r8                     # 8-byte Folded Reload
	adcq	24(%rsp), %r12                  # 8-byte Folded Reload
	addq	-48(%rsp), %r8                  # 8-byte Folded Reload
	adcq	-40(%rsp), %r12                 # 8-byte Folded Reload
	addq	-64(%rsp), %r8                  # 8-byte Folded Reload
	adcq	-56(%rsp), %r12                 # 8-byte Folded Reload
	addq	%rax, %r8
	adcq	%rdx, %r12
	shldq	$13, %r8, %r12
	movabsq	$2251799813685247, %rax         # imm = 0x7FFFFFFFFFFFF
	andq	%rax, %r8
	movq	8(%rsp), %rcx                   # 8-byte Reload
	addq	112(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	128(%rsp), %rbp                 # 8-byte Folded Reload
	movq	-96(%rsp), %r11                 # 8-byte Reload
	addq	88(%rsp), %r11                  # 8-byte Folded Reload
	adcq	96(%rsp), %r10                  # 8-byte Folded Reload
	addq	48(%rsp), %r11                  # 8-byte Folded Reload
	adcq	64(%rsp), %r10                  # 8-byte Folded Reload
	addq	-80(%rsp), %r11                 # 8-byte Folded Reload
	adcq	-72(%rsp), %r10                 # 8-byte Folded Reload
	addq	%r13, %r11
	adcq	-88(%rsp), %r10                 # 8-byte Folded Reload
	addq	%r12, %r11
	adcq	$0, %r10
	shldq	$13, %r11, %r10
	andq	%rax, %r11
	addq	-32(%rsp), %rcx                 # 8-byte Folded Reload
	adcq	-16(%rsp), %rbp                 # 8-byte Folded Reload
	addq	16(%rsp), %rcx                  # 8-byte Folded Reload
	adcq	32(%rsp), %rbp                  # 8-byte Folded Reload
	addq	-128(%rsp), %rcx                # 8-byte Folded Reload
	adcq	%r9, %rbp
	addq	%r10, %rcx
	adcq	$0, %rbp
	shldq	$13, %rcx, %rbp
	andq	%rax, %rcx
	addq	176(%rsp), %r14                 # 8-byte Folded Reload
	adcq	184(%rsp), %r15                 # 8-byte Folded Reload
	addq	56(%rsp), %r14                  # 8-byte Folded Reload
	adcq	72(%rsp), %r15                  # 8-byte Folded Reload
	addq	120(%rsp), %r14                 # 8-byte Folded Reload
	adcq	136(%rsp), %r15                 # 8-byte Folded Reload
	addq	40(%rsp), %r14                  # 8-byte Folded Reload
	adcq	-120(%rsp), %r15                # 8-byte Folded Reload
	movq	-8(%rsp), %rdx                  # 8-byte Reload
	addq	144(%rsp), %rdx                 # 8-byte Folded Reload
	adcq	152(%rsp), %rbx                 # 8-byte Folded Reload
	addq	80(%rsp), %rdx                  # 8-byte Folded Reload
	adcq	104(%rsp), %rbx                 # 8-byte Folded Reload
	addq	160(%rsp), %rdx                 # 8-byte Folded Reload
	adcq	168(%rsp), %rbx                 # 8-byte Folded Reload
	addq	-112(%rsp), %rdx                # 8-byte Folded Reload
	adcq	-104(%rsp), %rbx                # 8-byte Folded Reload
	addq	%rbp, %r14
	adcq	$0, %r15
	shldq	$13, %r14, %r15
	addq	%rdx, %r15
	adcq	$0, %rbx
	shldq	$13, %r15, %rbx
	movq	%r15, %xmm0
	movq	%r14, %xmm1
	leaq	(%rbx,%rbx,8), %rdx
	leaq	(%rbx,%rdx,2), %rdx
	addq	%r8, %rdx
	movq	%rdx, %rsi
	shrq	$51, %rsi
	addq	%r11, %rsi
	movq	%rsi, %r8
	shrq	$51, %r8
	addq	%rcx, %r8
	andq	%rax, %rdx
	andq	%rax, %rsi
	movq	%rdx, (%rdi)
	movq	%rsi, 8(%rdi)
	movq	%r8, 16(%rdi)
	punpcklqdq	%xmm0, %xmm1            # xmm1 = xmm1[0],xmm0[0]
	pand	.LCPI18_0(%rip), %xmm1
	movdqu	%xmm1, 24(%rdi)
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end18:
	.size	fe_mul_impl, .Lfunc_end18-fe_mul_impl
                                        # -- End function
	.p2align	4, 0x90                         # -- Begin function fe_tobytes
	.type	fe_tobytes,@function
fe_tobytes:                             # @fe_tobytes
# %bb.0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	movq	(%rsi), %rax
	movabsq	$-2251799813685247, %r9         # imm = 0xFFF8000000000001
	leaq	(%rax,%r9), %r11
	addq	$18, %r11
	movq	%r11, %rcx
	shrq	$51, %rcx
	movabsq	$2251799813685247, %rax         # imm = 0x7FFFFFFFFFFFF
	andq	%rax, %r11
	negl	%ecx
	movzbl	%cl, %edx
	movq	8(%rsi), %rcx
	addq	%r9, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rdx
	shrq	$51, %rdx
	andq	%rax, %rcx
	negl	%edx
	movzbl	%dl, %edx
	movq	16(%rsi), %r8
	addq	%r9, %r8
	subq	%rdx, %r8
	movq	%r8, %rdx
	shrq	$51, %rdx
	andq	%rax, %r8
	negl	%edx
	movzbl	%dl, %edx
	movq	24(%rsi), %r10
	addq	%r9, %r10
	subq	%rdx, %r10
	movq	%r10, %rdx
	shrq	$51, %rdx
	andq	%rax, %r10
	negl	%edx
	movzbl	%dl, %edx
	addq	32(%rsi), %r9
	subq	%rdx, %r9
	movabsq	$574208952489738240, %rsi       # imm = 0x7F8000000000000
	andq	%r9, %rsi
	movl	$255, %edx
	cmoveq	%rsi, %rdx
	#APP
	#NO_APP
	leaq	-18(%rax), %r14
	andq	%rdx, %r14
	addq	%r11, %r14
	movq	%r14, %rsi
	movq	%r14, %rbx
	shrq	$51, %rbx
	andq	%rdx, %rax
	addq	%rax, %rcx
	addq	%rbx, %rcx
	movq	%rcx, %r11
	shrq	$51, %r11
	addq	%rax, %r8
	addq	%r11, %r8
	movq	%r8, %r11
	shrq	$51, %r11
	addq	%r10, %rax
	addq	%r11, %rax
	movq	%rax, %r10
	shrq	$51, %r10
	addq	%r9, %rdx
	addq	%r10, %rdx
	movl	%edx, %ebp
	shll	$4, %ebp
	movl	%r8d, %r9d
	shll	$6, %r9d
	shrq	$48, %rsi
	andl	$7, %esi
	leal	(%rsi,%rcx,8), %esi
	movl	%esi, -68(%rsp)                 # 4-byte Spill
	movq	%rcx, -8(%rsp)                  # 8-byte Spill
	movq	%rcx, -16(%rsp)                 # 8-byte Spill
	movq	%rcx, -24(%rsp)                 # 8-byte Spill
	movq	%rcx, -32(%rsp)                 # 8-byte Spill
	movq	%rcx, -40(%rsp)                 # 8-byte Spill
	shrq	$45, %rcx
	andl	$63, %ecx
	orl	%r9d, %ecx
	movq	%r8, -48(%rsp)                  # 8-byte Spill
	movq	%r8, -56(%rsp)                  # 8-byte Spill
	movq	%r8, -64(%rsp)                  # 8-byte Spill
	movq	%r8, %r13
	movq	%r8, %r12
	movq	%r8, %r15
	shrq	$50, %r8
	andl	$1, %r8d
	leal	(%r8,%rax,2), %esi
	movl	%esi, -72(%rsp)                 # 4-byte Spill
	movq	%rax, %r11
	movq	%rax, %r10
	movq	%rax, %r9
	movq	%rax, %r8
	movq	%rax, %rsi
	shrq	$47, %rax
	andl	$15, %eax
	orl	%ebp, %eax
	movq	%r14, %rbx
	movb	%bl, (%rdi)
	movb	%bh, 1(%rdi)
	shrq	$16, %rbx
	movb	%bl, 2(%rdi)
	movq	%r14, %rbx
	shrq	$24, %rbx
	movb	%bl, 3(%rdi)
	movq	%r14, %rbx
	shrq	$32, %rbx
	movb	%bl, 4(%rdi)
	movq	%r14, %rbx
	shrq	$40, %rbx
	movb	%bl, 5(%rdi)
	movl	-68(%rsp), %ebx                 # 4-byte Reload
	movb	%bl, 6(%rdi)
	movq	-8(%rsp), %rbx                  # 8-byte Reload
	shrq	$5, %rbx
	movb	%bl, 7(%rdi)
	movq	-16(%rsp), %rbx                 # 8-byte Reload
	shrq	$13, %rbx
	movb	%bl, 8(%rdi)
	movq	-24(%rsp), %rbx                 # 8-byte Reload
	shrq	$21, %rbx
	movb	%bl, 9(%rdi)
	movq	-32(%rsp), %rbx                 # 8-byte Reload
	shrq	$29, %rbx
	movb	%bl, 10(%rdi)
	movq	-40(%rsp), %rbx                 # 8-byte Reload
	shrq	$37, %rbx
	movb	%bl, 11(%rdi)
	movb	%cl, 12(%rdi)
	movq	-48(%rsp), %rcx                 # 8-byte Reload
	shrq	$2, %rcx
	movb	%cl, 13(%rdi)
	movq	-56(%rsp), %rcx                 # 8-byte Reload
	shrq	$10, %rcx
	movb	%cl, 14(%rdi)
	movq	-64(%rsp), %rcx                 # 8-byte Reload
	shrq	$18, %rcx
	movb	%cl, 15(%rdi)
	shrq	$26, %r13
	movb	%r13b, 16(%rdi)
	shrq	$34, %r12
	movb	%r12b, 17(%rdi)
	shrq	$42, %r15
	movb	%r15b, 18(%rdi)
	movl	-72(%rsp), %ecx                 # 4-byte Reload
	movb	%cl, 19(%rdi)
	shrq	$7, %r11
	movb	%r11b, 20(%rdi)
	shrq	$15, %r10
	movb	%r10b, 21(%rdi)
	shrq	$23, %r9
	movb	%r9b, 22(%rdi)
	shrq	$31, %r8
	movb	%r8b, 23(%rdi)
	shrq	$39, %rsi
	movb	%sil, 24(%rdi)
	movb	%al, 25(%rdi)
	movq	%rdx, %rax
	shrq	$4, %rax
	movb	%al, 26(%rdi)
	movq	%rdx, %rax
	shrq	$12, %rax
	movb	%al, 27(%rdi)
	movq	%rdx, %rax
	shrq	$20, %rax
	movb	%al, 28(%rdi)
	movq	%rdx, %rax
	shrq	$28, %rax
	movb	%al, 29(%rdi)
	movq	%rdx, %rax
	shrq	$36, %rax
	movb	%al, 30(%rdi)
	shrq	$44, %rdx
	andb	$127, %dl
	movb	%dl, 31(%rdi)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end19:
	.size	fe_tobytes, .Lfunc_end19-fe_tobytes
                                        # -- End function
	.type	k25519Precomp,@object           # @k25519Precomp
	.section	.rodata,"a",@progbits
	.globl	k25519Precomp
	.p2align	4, 0x0
k25519Precomp:
	.ascii	"\205;\214\365\306\223\274/\031\016\214\373\306-\223\317\302B=d\230H\013'e\272\3243:\235\317\007"
	.ascii	">\221@\327\0059\020\235\263\276@\321\005\2379\375\t\212\217h4\204\301\245g\022\370\230\222/\375D"
	.ascii	"h\252z\207\005\022\311\253\236\304\252\314#\350\331&\214YC\335\313}\033Z\250e\f\237h{\021o"
	.ascii	"\327q<\223\374\347$\222\265\365\017z\226\235F\237\002\007\326\341e\232\246Z..}\250?\006\fY"
	.ascii	"\250\325\264B`\245\231\212\366\254`N\f\201+\217\2527n\261k#\236\340U%\311i\246\225\265k"
	.ascii	"_z\233\245\263\250\372Cx\317\232]\335k\30161j=\013\204\240\017Ps\013\245>\261\365\032p"
	.ascii	"0\227\356L\250\260%\257\212K\206\3500\204Z\0022g\001\237\002P\033\301\364\370\200\232\033N\026z"
	.ascii	"e\322\374\244\350\037aV}\272\301\345\375S\323;\275\326K!\032\3631\201b\332[U\207\025\271*"
	.ascii	"\211\330\320\r?\223\256\024b\3325\034\"#\224XL\333\362\214E\345p\321\306\264\271\022\257&(Z"
	.ascii	"\237\t\374\216\271Qs(8%\375}\364\306ege\222\n\373=\2154\312'\207\345!\003\221\016h"
	.ascii	"\277\030h\005\n\005\376\225\251\372`Vq\211~2sP\240\006\315\343\350\303\232\244EtL?\223'"
	.ascii	"\t\377v\304\351\373\023Zr\301\\{E9\236n\224D+\020\371\334\333]+>Uc\277\f\235\177"
	.ascii	"3\273\245\bD\274\022\242\002\355^\307\303HP\215D\354\277Z\f\353\033\335\353\006\342F\361\314E)"
	.ascii	"\272\326G\244\303\202\221\177\267)'K\321\024\000\325\207\240d\270\034\361<\343\363U\033\353s~J\025"
	.ascii	"\205\202*\201\361\333\273\274\374\321\275\320\007\b\016'-\247\275\033\013g\033\264\232\266;ki\276\252C"
	.ascii	"1q\025w\353\356\f:\210\257\310\000\211\025'\2336\247Y\332h\266e\200\2758\314\242\266{\345Q"
	.ascii	"\244\214}{\266\006\230I9'\322'\204\342[W\271SE \347\\\b\273\204xA\256AL\2668"
	.ascii	"qK\352\002g2\254\205\001\273\241A\003\340p\276D\301;\bK\242\344S\343a\r\237\032\351\270\020"
	.ascii	"\277\243N\224\320\\\032k\322\300\235\263:5ptI.T(\202R\262q~\222<(i\352\033F"
	.ascii	"\261!2\252\232,o\272\247#\272;S!\240l:,\031\222Ov\352\235\340\027S.]\335n\035"
	.ascii	"\242\263\270\001\310m\203\361\232\244>\005G_\003\263\363\255wX\272A\234R\247\220\017j\034\273\237z"
	.ascii	"\217>\335\004fY\267Y,p\210\342w\003\263l#\303\331^f\2343\261/\345\274a`\347\025\t"
	.ascii	"\3314\222\363\355]\247\342\371X\265\341\200v=\226\373#<n\254A',\303\001\0162\241$\220:"
	.ascii	"\032\221\242\311\331\365\301\347\327\247\314\213xq\243\2702*\266\016\031\022dc\225N\314.\\|\220&"
	.ascii	"\035\234/c\016\335\314.\0251\211v\226\266\320QXzc\250k\267\337R9\357\016\240I}\323m"
	.ascii	"^Q\252ITc[\355:\202\306\013\237\304e\250\304\321B[\351\037\f\205\271\025\323\003om\3270"
	.ascii	"\307\344\006!\027DDli\177\215\222\200\326S\373&?Mi\244\236s\264\260K\206.\021\227\306\020"
	.ascii	"\005\310X\203\240*\246\fGB z\343J=j\334\355\021;\246\323dt\357\006\bU\257\233\277\003"
	.ascii	"\336_\276}'\304\223d\242~\255\031\255O]&\220E0F\310\337\000\016\t\376f\355\253\034\346%"
	.ascii	"\004fX\314(\341\023?~tY\264\354sXo\365h\022\314\355=\266\240,\342\206EcxmV"
	.ascii	"\320/Z\306\205B\005\241\303g\026\363*\021dlX\356\032s@\342\nh*\262\223G\363\245\373\024"
	.ascii	"4\b\301\234\237\2447\026Q\304\233\250\325V\216\274\333\322\177\177\017\354\265\034\3315\314^\312[\2273"
	.ascii	"\324\367\205i\026F\327<W\000\310\311\204^>Y\036\023a{\266\362\303/lR\374\203\352\234\202\024"
	.ascii	"\270\354qN/\013\347!\343w\244@\271\335V\346\200O\035\316\316Ve\277~{]S\304;\374\005"
	.ascii	"\302\225\335\227\204{C\377\247\265N\2520Ntl\213\350\205<a]\f\236s\201u_\036\307\331/"
	.ascii	"\335\336\257R\256\263\270$\3170;\355\214c\2254\225\201\276\251\203\274\2443\004\037e\\Gg77"
	.ascii	"\220e$\024\313\225@c5U\301\026@\024\022\357`\274\020\211\f\0248\236\214|\2200W\220\365k"
	.ascii	"\331\255\321@\375\231\272/'\320\364\226o\026\007\263\256;\360\025R\360cC\231\371\030;l\245\276\037"
	.ascii	"\212[A\341\361x\247\017~\247\303\272\367\237@\006P\232\242\232\270\327RoVZcz\366\034R\002"
	.asciz	"\344^/w g\024\261\316\232\007\226\261\224\370\350J\202\254\000M\"\370J\304l\315\367\331S\027"
	.ascii	"\224R\235\n\013\356?QfZ\337\017\\\347\230\217\316\007\341\277\210\206a\324\355,8q~\n\240?"
	.ascii	"4\333=\226-#i<X8\227\264\332\207\336\035\205\362\221\240\371\321\327\252\266\355H\240/\376\265\022"
	.ascii	"\222\036o\255&|+\337\023\211KP#\323fK\303\213\034u\300\235@\214\270\307\226\007\302\223~o"
	.ascii	"M\343\374\226\304\373\360q\355[\363\255k\202\271sa\305(\377ar\004\322o \261o\371v\233t"
	.ascii	"\005\256\246\256\004\366Z\037\231\234\344\276\361Q#\301fk\377\356\265\b\250aQ!\340\001\017\301\316\017"
	.ascii	"EN$\304\235\322\362=\n\336\330\223t\016\002+M!\f\202~\006\310l\n\271\352o\026y7A"
	.ascii	"D\036\376I\246XMd~w\2551\242\256\374!\322\320\177\210Z\034D\002\363\021\305\203q\252\001I"
	.ascii	"\360\370\032\214T\267\261\b\264\231b$|z\017\3169\331\006\036\371\260`\367\023\022mr{\210\273A"
	.ascii	"\256\221f|YL#~\310\264\205\n=\235\210d\347\372J5\f\311\342\332\035\236j\f\007\036\207\n"
	.ascii	"\276FCtD}\350@%+\265\025\324\332H\035>`;\241\030\212:|\367\275\315/\301(\267N"
	.ascii	"\211\211\274K\231\265\0013`B\335[:\256ks<\236\325\031\342\255a\rd\324\205&\0170\347>"
	.ascii	"\030u\036\204Gy\372C\327F\234cY\372\306\345t+\005\343\035^\006\2410\220\270\317\242\306G}"
	.ascii	"\267\326}\236\344U\322\365\254\036\013a\\\021\026\200\312\207\341\222]\227\231<\302%\221\227bW\201\023"
	.ascii	"\340\326\360\216\024\320\332?<oT\221\232t>\235W\201\273&\020b\354q\200\354\3114\215\365\214\024"
	.ascii	"mu\344\232}/W\342\177H\363\210\273E\303V\215\250`im\013\321\237\271\241\256N\255\353\217'"
	.ascii	"'\3604y\366\222\244F\251\n\204\366\276\204\231FT\030a\211*\274\241\\\324\273]\275\036\372\362?"
	.ascii	"f9\223\214\037h\252\261\230\f) \234\224!\214R<\235!\221R\0219{g\234\376\002\335\004A"
	.ascii	"\270j\t\333\006N!\2015O\344\f\311\266\250!\365*\236@*\301$e\201\244\374\216\244\265e\001"
	.ascii	"*B$\021^\277\262r\265:\243\2303\f\372\241f\266R\372\001a\313\224\325S\257\257\000;\206,"
	.ascii	"vj\204\240t\244\220\361\300|/\315\204\371\357\022\217+\252X\006)^i\270\310\376\277\331g\033Y"
	.ascii	"]\265\030\237q\263\271\231\036d\214\241\372\345e\344\355\005\237\3026\021\ba\213\0220p\206O\233H"
	.ascii	"\372\233\264\200\034\r/1\212\354\363\253^QyY\210\034\360\236\3003pr\313{\217\312\307.\340="
	.ascii	"\357\222\353:-\0202\322a\250\026a\264Sb\341$\252\013\031\347\253~=\277\276lI\272\373\365I"
	.ascii	".W\234\036\214b]\025AG\210\305\254\206M\212\353cWQ\366R\243\221[Qg\210\302\246\241\006"
	.ascii	"\324\317[\212\020\232\2240\353sd\274p\335@\334\034\r|0\301\224\302\222tn\372\313m\250\004V"
	.ascii	"\266d\027|\324\321\210rQ\213A\340@\021Tr\321\366\254\030`\032\003\237\306B'\376\211\236\230 "
	.ascii	".\354\352\205\213't\026\337+\313z\007\334!VZ\364\313a\026L\nd\323\225\005\367P\231\013s"
	.ascii	"\177\314-:\375w\227I\222\330O\245,|\2052\240\343\007\322d\330y\242)~\246\f\035\355\003\004"
	.ascii	"R\305N\2075-K\311\215o$\230\317\310\346\305\3165\300\026\372F\313\367\314=0\bCE\327["
	.ascii	"*y\347\025!\223\304\205\311\335\315\275\242\211L\306b\327\243\255\250=\036\235,\370g0\022\333\267["
	.ascii	"\302L\262(\225\321\232\177\201\3015ceTk\1776r\300On\266\270f\203\255\200s\000x:\023"
	.ascii	"\276b\312\306g\364a\t\356R\031!\326!\354\004pG\325\233w`#\030\322\340\360Xm\312\rt"
	.ascii	"<Cx\004W\214\032#\235C\201\302\016'\265\267\237\007\331\343\352\231\252\333\331\003+l%\365\003,"
	.ascii	"N\316\317R\007\356H\337\267\b\354\006\363\372\377\303\304YT\271*\013q\005\215\243>\226\372%\035\026"
	.ascii	"}\244S{u\030\017yyX\f\3170\001{0\371\367~%w=\2201\257\273\226\275\275h\224i"
	.ascii	"H\031\251j\346=\335\330\314\322\300/\302dPH/\352\3754f$H\233:.JlN\034>)"
	.ascii	"\317\376\332\364F/\037\275\367\326\177\244\024\001\357|\177\263GJ\332\375\037\323\205W\220s\244\031RR"
	.ascii	"\341\022Q\222K\023n7\240]\241\334\265x7p\0211\034F\257\211E\260#(\003\177D\\`["
	.ascii	"L\360\347\360\306\376\351;bI\343u\236Wj\206\032\346\035\036\026\357BU\325\275Z\314\364\376\022/"
	.ascii	"\211|\304 Y\200e\271\314\217;\222\f\020\360\347w\357\342\002e%\001\000\356\263\256\250\316m\247$"
	.ascii	"@\307\300\337\262\"E\n\007\244\311@\177n\320\020h\366\317xA\024\317\306\2207\244\030%{`^"
	.ascii	"\024\317\226\245\034C,\240\000\344\323\256@-\304\343\333&\017.\200&E\322hpE\236\0233\037 "
	.ascii	"\030\030\337l\217\035\263X\242Xb\303O\247\3175n\035\346fO\377\263\341\367\325\315l\253\254gP"
	.ascii	"Q\235\003\bk\177R\375\006\000|\001dI\261\030\250\244%.\260\016\"\325u\003Fb\210\272|9"
	.ascii	"\347y\023\310\373\303\025x\361*\341\335 \224a\246\325\375\250\205\370\300\251\377R\302\341\301\"@\033w"
	.ascii	"\262YY\360\2230\3010vy\251\351\215\241:\342&^\035r\221\324/\":lnv \3239#"
	.ascii	"\247/:Q\206\331}\330\b\317\324\371q\233\254\365\263\203\242\036\033\303k\320v\032\227\031\222\030\0323"
	.ascii	"\257ru\235:/Q&\236J\007h\210\342\313[\304\367\200\021\301\301\355\204{\246I\366\237a\311\032"
	.ascii	"\306\200O\373Eo\026\365\317u\307a\336\3076\234\034\331A\220\033\350\324\343!\376\275\203k|\0261"
	.ascii	"h\020KRB8+\362\207\351\234\356;4hP\310PbJ\204q\235\374\021\261\b\03746$a"
	.ascii	"8&-\032\343Ic\2135\375\323\233\000\267\337\235\244k\240\243\270\361\213\177E\004\331x1\252\"\025"
	.ascii	"\215\211N\207\333A\235\331 \334\007l\361\245\376\t\274\233\017\320g,=y@\377^\2360\342\353F"
	.ascii	"8IaiS/8,\020m-\267\232@\376\332'\362F\266\2213\310\350l0$\005\365p\376E"
	.ascii	"\221\024\225\310 I\362b\242\fc?\310\007\360\005\270\324\311\365\322E\273oE\"z\265m\237a\026"
	.ascii	"\214\013\f\226\246uH\332 /\016\357v\320h[\324\217\013=\317Q\373\007\324\222\343\240#\026\215B"
	.ascii	"\375\b\243\001DJO\b\254\312\245v\303\031\"\250}\274\321CF\336\270\336\3068\275`-Y\201\035"
	.ascii	"\350\305\205{\237\266e\207\262\272h\321\213g\360o\233\0173\035|\347p:|\216\257\260Qm_:"
	.ascii	"_\254\r\246V\2076aW\334\253\353j/\340\027}\017\316L-?\031\177\360\334\354\211wJ# "
	.ascii	"R\262xq\266\r\322v`\321\036\325\3714\034\007p\021\344\263 J*\366f\343\377<5\202\326|"
	.ascii	"\363\364\254h`\315e\246\323\343\327<\030-\331B\331%`3\2358YW\377\330,+;%\360>"
	.ascii	"\266\372\207\330[\244\341\013n;@\2722j\204*\000`n\351\022\020\222\331C\t\334;\206\3108("
	.ascii	"0PFJ\317\260k\321\253w\305\025AkI\372\235A\253\364\212\256\317\202\022(\250\006\246\270\334!"
	.ascii	"\2721w\276\372\000\215\232\211\030\236b~`\003\202\177\331\363C7\002\314\262\213gol\277\r\204]"
	.ascii	"\310\237\235\214F\004`\\\313\243*\324n\t@%\234/\356\022LM[\022\253\035\243\224\201\320\303\013"
	.ascii	"\213\341\2370\r8np\307e\341\271\246-\260n\253 \256}\231\272\273W\335\226\301*#vB:"
	.ascii	"\313~D\333r\301\370;\275-(\306\037\304\317_\376\025\252u\300\377\254\200\371\251\341$\350\311p\007"
	.ascii	"\372\204p\212,CBKE\345\271\337\343\031\212\211]\344X\234!\000\237\276\321\353m\241\316w\361\037"
	.ascii	"\375\265\265E\232\331a\317$y:\033\351\204\t\206\211>>0\031\t0\347\036\013PA\375d\3629"
	.ascii	"\341{\t\376\253J\233\321)\031\340\337\341\374m\244\377\361\246,\224\b\311\303N\3615,'!\306e"
	.ascii	"\234\342\347\333\0274\255\247\234\023\234+j7\224\275\251{Y\223\216\033\351\240@\230\210h4\327\022\027"
	.ascii	"\335\2231\316\370\211+\347\273\300%\241V3\020M\203\376\034.=\251\031\004r\342\234\261\n\200\371\""
	.ascii	"\254\375n\232\335\237\002BAI\2454\276\316\022\271{\363\275\207\271d\017d\264\312\230\205\323\244qA"
	.ascii	"\313\370\236>\2126Z`\025GP\245\"\300\351\343\217$$_\260H=U\345&vd\315\026\364\023"
	.ascii	"\214L\311\231\252X'\372\007\270\000\260oo\000#\222S\332\255\335\221\322\373\253\321KW\372\024\202P"
	.ascii	"\326\003\320S\273\025\032Fe\311\363\274\210(\020\262Z:hluv\305'G\264l\310\244Xw:"
	.ascii	"K\376\326>\025i\002\302\304w\035Q9gZ\246\224\257\024,F&\336\313K\247\253o\354`\371\""
	.ascii	"vP\256\223\366\021\201T\246T\375\035\337!\256\035e^\021\363\220\214$\022\224\364\347\215_\321\237]"
	.ascii	"\036R\327\356*M$?\025\226.C(\220:\216\324\026\234.w\272d\341\330\230\353G\372\207\301;"
	.ascii	"\177rcm\323\b\024\0033\265\307\327\357\2327jK\342\256\314\305\217\341\251\323\276\217O\2215/3"
	.ascii	"\f\302\206\352\025\001Gm%\321Fl\313\267\212\231\210\001f:\2652x\327\003\272o\220\316\201\rE"
	.ascii	"?t\256\034\226\330t\320\355c\034\356\365\030m\370)\355\364\347[\305\275\227\b\261:fy\322\272L"
	.ascii	"uR \246\241\266{n\203\216<A\327!O\252\262\\\217\350U\321Vo\341[4\246K]\342-"
	.ascii	"\315\037\327\240$\220\321\200\370\212(\373\n\302%\305\031d:_K\227\243\2613r\000\342\357\274\177}"
	.ascii	"\224\220\302\363\305]|\315\253\005\221*\232\242\201\307X0\034B6\035\306\200\327\324\330\334\226\321\234O"
	.ascii	"\001(k&j\036\357\372\026\237s\325\304hl\206,v\003\033\274/\212\366\215Z\267\207^CuY"
	.ascii	"h7{j\330\227\222\031cz\321\032$X\320\320\027\f\034\\\255\234\002\272\007\003z8\204\320\315|"
	.ascii	"\223\314`g\030\204\f\233\231*\263\032z\000\256\315\030\332\013b\206\354\215\250D\312\220\201\204\312\2235"
	.ascii	"\027\004&m,B\246\334\275@\202\224P=\025\256w\306h\373\264\301\300\251S\317\320a\355\320\213B"
	.ascii	"\247\232\204^\232\030\023\222\315\372\330e5\303\330\324\321\273\375S[TR\214\346c-\332\b\2039'"
	.ascii	"S$p\nL\016\241\271\336\033}\325fX\242\017\367\332'\315\265\331\271\377\3753,IE),W"
	.ascii	"\023\324^C(\215\303B\311\314x2`\363P\275\357\003\332y\032\253\007\273U3\214\276\256\227\225&"
	.ascii	"\2760\315\326E\307\177\307\373\256\272\343\323\350\337\344\f\332]\2520\210,\242\200\312[\300\230T\230\177"
	.ascii	"cc\277\017R\025V\323\246\373M\317EZ\004\b\302\240?\207\274O\302\356\347\022\233\326<e\3620"
	.ascii	"\027\341\013\237\210\316I8\210\242T{\033\255\005\200\034\222\374#\237\303\243=\004\3631\nG\354\302v"
	.ascii	"\205\f\301\2528\311\b\212\313k'\333`\233\027Fp\254o\016\036\300 \251\332sdY\361s\022/"
	.ascii	"\300\013\247U\327\213H0\347B\324\361\244\265\326\006baY\274\236\246\321\352\204\367\305\355\227\031\2548"
	.ascii	"\021\036\340\212|\3749G\237\253jJ\220tR\375.\217r\207\202\212\331A\362i[\330*W\236]"
	.ascii	";\261Q\247\027\265f\006\214\205\233~\206\006}tI\336ME\021\300\254\254\234\346\351\277\234\315\337\""
	.ascii	"\241\340;\020\264Y\354Vi\371Y\322\354\272\343.2\315\365\023\224\262|yr\344\315$x\207\351\017"
	.ascii	"\331\f\r\303\340\322\333\2153C\273\254_f\216\255\037\226*2\214%k\217\307\301HT\300\026)k"
	.ascii	";\221\272\n\3214\333~\016\254m.\202\315\243N\025\370xe\377=\bf\027\n\360\1770?0L"
	.ascii	"\000E\331\rX\003\374)\223\354\273o\244z\322\354\370\247\342\302_\025\n\023\325\241\006\267\032\025kA"
	.ascii	"\205\214\262\027\326;\n\323\352;w9\267w\323\305\277\\j\036\214\347\306\306\304\267*\213\367\270a\r"
	.ascii	"\2606\301\351\357\327\250V K\344X\315\345\007\275\253\340W\033\332/\346\257\322\350wB\367*\032\031"
	.ascii	"\373\016FOC+\346\237\326\0076\246\324\003\323\336$\332\240\267\016!R\360\223[T\000\276}~#"
	.ascii	"1\024<\305K\367\026\316\336\355r \316%\227+\347>\262\265o\303\271\270\b\311\\\013E\016.~"
	.ascii	"0\264\001g\355u5\001\020\375\013\237\346\224\020#\"\177\344\203\025\0172u\343U\021\261\231\246\257q"
	.asciz	"\326P;G\034<B\352\020\3578;\037z\350Q\225\276\311\262_\277\204\233\034\232\370x\274\037s"
	.ascii	"\035\266S9\233o\316e\346A\241\257\3529X\306\376Y\367\251\375_C\017\216\302\261\302\351B\021\002"
	.ascii	"\200\030\370H\030\3070\344\031\301\316^\"\f\226\277\343\025\272k\203\340\332\266\bX\341G3oML"
	.ascii	"p\031\217\230\374\335\f/\033\365\271\260'b\221k\276v\221w\304\266\307n\250\237\217\250\000\225\2778"
	.ascii	"\311\037}\301\317\354\367\030\024<@Q\246\365ul\337\f\356\367+q\336\333\"z\344\247\252\335?\031"
	.ascii	"o\207\3507<\311\322\037,F\321\030Z\036\366\242v\022$9\202\365\200PiI\r\277\236\271oj"
	.ascii	"\306#\344\266\265\"\261\356\216\377\206\362\020p\235\223\214]\317\035\203*\251\220\020\353\305B\237\332o\023"
	.ascii	"\353U\bV\273\301Fj\235\360\223\3708\273\026$\301\254q\2177\021\035\327\352\226\030\243\024i\367u"
	.ascii	"\321\275\005\243\261\337L\371\b,\370\237\235K6\017\212X\273\303\245\330\207*\272\334\350\013Q\203!\002"
	.ascii	"\177z0C\001qZ\235_\244}\304\236\336c\260\323z\222\276R\376\273\"lB@\375A\304\207\023"
	.ascii	"\024-\255^8f\367J0X|\312\200\330\216\240=\036!\020\346\246\023\r\003l\200{\341\034\007j"
	.ascii	"\370\212\227\207\321\303\323\265\023D\016\177=Z+r\240|G\273HH{\r\222\334\036\257j\262q1"
	.ascii	"\321G\212\262\330\267\r\246\361\244p\027\326\024\277\246X\275\335S\223\370\241\324\351CB4cJQl"
	.ascii	"\250LV\227\2201/\251\031\341u\"L\270{\377PQ\207\2447\376UOZ\203\360<\207\324\037\""
	.ascii	"Ac\025:O \"#-\003\n\272\351\340s\373\016\003\017AL\335\340\374\252J\222\373\226\245\332H"
	.ascii	"\223\227L\310]\035\366\024\006\202A\357\343\371A\231\254wb4\217\270\365\315\251y\212\016\3727\310X"
	.ascii	"\307\234\245\\f\216\312n\240\2548.K%G\250\316\027\036\322\b\307\2571\367J\330\312\374\326mg"
	.ascii	"X\220\374\226\205h\371\f\033\240V{\363\273\334\035j\3265I}\347\302\334\n\177\245\306\362sO\034"
	.ascii	"\2044|\374npn\263a\317\301\303\264\311\337s\345\307\034x\311y\035\353\\g\257}\333\232Ep"
	.ascii	"\273\240_0\275Oz\016\255c\306T\340L\235\202H8\343/\203\303!\364BL\366\033\r\310Zy"
	.ascii	"\263+\264\221I\333\221\033\312\334\002K#\226&W\334x\214\037\345\236\337\237\323\037\342\214\204b\341_"
	.ascii	"\b\262|]-\205y(\347\362}hp\335\336\270\221xh!\253\377\013\3345\252}gC\300D+"
	.ascii	"\032\226\224\341O!YNO\315q\r\307}\276I-\362P;\322\317\000\2232r\221\374F\324\211G"
	.ascii	"\216\267N\007\253\207\034\032g\364\332\231\216\321\306\372g\220OH\315\273\254>\344\244\271+\357.\305`"
	.ascii	"\021m\256|\302\305+p\253\214\244T\233i\307D\262.I\272V@\274\357mg\266\331Hr\327p"
	.ascii	"\361\213\375;\274\211]\013\032U\363\3117\222k\260\365(0\325\260\026L\016\253\312\317,1\234\274\020"
	.ascii	"[\240\302>K\350\212\252\340\201\027\355\364\236i\230\321\205\216p\344\023Ey\023\364v\251\323[uc"
	.ascii	"\267\254\361\227\030\020\307=\330\273e\301^}\332]\017\002\241\017\234[\216PV*\3057\027uc'"
	.ascii	"S\b\321*>\240_\265i5\346\236\220uo5\220\270i\276\375\361\371\237\204o\301\213\304\301\214\r"
	.ascii	"\251\031\264n\323\002\224\002\245`\264w~N\264\360VI<\3240b\250\317\347f\321z\212\335\302p"
	.ascii	"\023~\355\270}\226\324\221z\201v\327\n/%td%\205\r\340\202\t\344\345<\245\0268a\2702"
	.ascii	"\016\354o\237P\224ae\215Q\306F\251~.\356\\\233\340g\363\3013\227\225\204\224cc\254\017."
	.ascii	"d\315H\344\276\367\347y\320\206x\bg:\310j.\333\344\240\331\324\237\370AOZs\\!yA"
	.ascii	"4\315k(\2713\256\344\334\326\235U\266~\357\267\037\216\323\263\037\024\213'\206\302A\"f\205\3721"
	.ascii	"*\355\334\327\347\224p\214p\234\323G\303\212\373\227\002\331\006\2513\340;\341v\235\331\f\243D\003p"
	.ascii	"\364\"6.Bl\202\257-P3\230\207) \301#\2218+\341\267\301\233\211$\225\251\022#\273$"
	.ascii	"k\\\370\365*\f\370A\224g\372\004\303\204rh\255\033\272\243\231\337E\211\026]\353\377\371*\035\r"
	.ascii	"\303g\3362\027\355\250\261HI\033F\030\224\264<\322\274\317vCC\275\216\b\200\030\036\207>\356\017"
	.ascii	"\337\036b2\241\212\332\251ye\"Y\241\"\2700\223\301\232\247{\031\004@v\035S\030\227\327\254\026"
	.ascii	"\255\266\207x\305\306Y\311\272\376\220_\255\236\341\224\004\365B\243bN\342\026\000\027\026\030K\323N\026"
	.ascii	"=\035\233-\257r\337rZ$2\2446*Fc7\226\263\026y\240\316>\t#0\271\366\016>\022"
	.ascii	"\232\346/\031L\331~H\023\025\221:\352,\256a'\336\244\271\323\366{\207\353\363s\020\306\017\332x"
	.ascii	"\224:\fh\361\200\237\242\346\347\351\032\025~\367qsy\001HX\361\000\021\335\215\263\026\263\244J\005"
	.ascii	"j\306+\345(]\361[\216\032\360p\030\343G,\335\213\302\006\274\257\031$:\027k%\353\336%-"
	.ascii	"\270|&\031\215F\310\337\257M\345f\234x(\013\027\354nf*\035\353*`\247}\253\246\020F\023"
	.ascii	"\025\365\321w\347e*\315\361`\252\217\207\221\211T\345\006\274\332\274;\267\261\373\311|\251\313xHe"
	.ascii	"\376\260\366\215\307\216\023Q\033\365u\345\211\332\227S\271\361zq\035z \tP\326 +\272\375\002!"
	.ascii	"\241\346\\\005\005\344\236\226)\255Q\022h\247\2746\025\244}\252\027\365\032:\272\262\354)\333%\327\n"
	.ascii	"\205o\005\233\f\274\307\376\327\377\365\347hR}S\372\256\022Cb\306\257w\331\2379\002S_gO"
	.ascii	"W$N\203\261gB\334\305\033\316p\265Du\266\327^\321\367\013z\360\032P6\240q\373\317\357J"
	.ascii	"\036\027\025\00466-\303;H\230\211\021\357+\315\020Q\224\320\255n\n\207ae\250\242r\273\314\013"
	.ascii	"\226\022\376PL^m\030~\237\350\376\202{9\340\2601pP\305\366\307;\3027\217\020i\375xf"
	.ascii	"\310\251\261\352/\226^\030\315}\024e5\346\347\206\362m[\2731\340\222\260>\267\326Y\253\360$@"
	.ascii	"\302chc1\372\206\025\3623-WH\214\366\007\374\256\236x\237\314sO\001G\255\216\020\342B-"
	.ascii	"\223uS\017\r{q!L\006\036\023\013iN\221\237\340*u\256\207\266\033n<B\233\247\363\013B"
	.ascii	"\233\322\337\224\025\023\365\227jL?1]\230Ua\020PE\b\007?\241\353\"\323\322\270\b&kg"
	.ascii	"G+[\034e\2728\201\200\033\0331\354\266q\206\26051\274\261\f\377{\340\361\f\234\372/]t"
	.ascii	"jN\323!W\3376`\320\263{\231'\210\333\261\372ju\310\303\t\302\3239\310\035L\345[\341\006"
	.ascii	"\275\310\311+\036ZR\277\201\235G&\b&[\352\333U\001\337\016\307\021\325\320\365\f\226\353<\342\032"
	.ascii	"J\2312\031\207]r[\260\332\261\316\265\03452\005\312\267\332I\025\304}\367\301\216'a\330\336X"
	.ascii	"\250\311\302\266\250[\373-\214Y,\365\216\357\356Hs\025-\361\007\221\2003\330[\035Ski\272\b"
	.ascii	"\\\305f\362\2237\027\330INE\314\305v\311\310\250\303&\274\370\202\343\\\371\366\205T\350\235\363/"
	.ascii	"z\305\357\303\356>\355w\021H\377\324\027U\340\004\313q\246\361?z=\352T\376|\224\2643\006\022"
	.ascii	"\n\020\022IG1\275\202\006\276o~m{#\336\306y\352\021\031v\036\341\336;9\313\343;C\007"
	.ascii	"B\000a\221x\230\224\013\350\372\353\354<\261\347N\300\244\360\224\225s\276p\205\221\325\264\231\n\3235"
	.ascii	"\364\227\351\\\300Dy\377\243Q\\\260\344=]W|\204vZ\375\2013X\237\332\366z\336>\207-"
	.ascii	"\201\371]N\341\002b\252\365\341\025P\027Y\r\242l\035\342\272\323u\242\030S\002`\001\212aC\005"
	.ascii	"\t47Cd1z\025\331\201\252\364\356\267\270\372\006H\246\365\346\376\223\260\266\247\177pT6w."
	.ascii	"\301#L\227\364\275\352\r\223F\316\235%\no\252,\272\232\242\270, \004\r\226\007-6C\024K"
	.ascii	"\313\234R\034\351T|\226\3735\306d\222&\3660e\031\022x\364\257G'\\o\366\352\030\204\003\027"
	.ascii	"z\037n\266\307\267\304\314~/\f\365%~\025D\034\257>q\374m\360>\367c\332RgD/X"
	.ascii	"\344L2 \323{1\306\304\213H\244\350B\020\250d\023ZN\213\361\036\262\311\215\242\315K\034*\f"
	.ascii	"Ei\275iH\201\304\355\"\215\034\276}\220m\r\253\305\\\325\022\322;\306\203\334\024\2430\233jZ"
	.ascii	"G\004\037o\320\307M\322Y\300\207\333>\236&\262\217\322\262\373r\002[\321wH\366\306\321\213U|"
	.ascii	"=F\226\323$\025\354\320\360$Z\303\212b\273\022\244_\274\034y:\f\245\303\257\373\n\312\245\004\004"
	.ascii	"\321oA*\033\236\274b\213YP\343(\367\306\265gi]=\330?4\004\230\356\370\347\026uR9"
	.ascii	"\326C\247\n\007@\037\214\350^&[\313\320\272\314\336\322\217fk\004KW3\226\335\312\375[9F"
	.ascii	"\234\232]\032-\333\177\021*\\\000\321\274Ew\234\352o\325T\361\276\324\357\026\320\"\350)\232Wv"
	.ascii	"\3624\264R\023\265<3\341\200\336\223I(2\330\3165\ru\207(Q\265\301w'*\273\024\305\002"
	.ascii	"\027*\300I~\216\266E\177\243\251\274\242Q\315#\033L\"\354\021_\326>\261\275\005\236\334\204\243C"
	.ascii	"E\266\361\213\332\325KhSK\265\366~\323\213\373S\322\260\251\327\02691Y\200Ta\t\222`\021"
	.ascii	"\315M\2336\026V8zc5\\e\247,\300u!\200\361\324\371\033\302}B\340\346\221t}c/"
	.ascii	"\252\317\332)i\026M\264\217Y\023\204L\237R\332YU=E\312c\357\351\013\216i\305[\022\0365"
	.ascii	"\276{\366\032F\233\264\324a\211\253\310z\003\003\326\373\231\246\371\237\341\336q\232*\316\347\006-\030\177"
	.ascii	"\"u!\216rKE\t\330\270\204\324\364\350X\252<\220F\177M%X\323\027R\034$C\300\254D"
	.ascii	"\354h\001\253d\216|zC\305\355\025UJZ\313\332\016\315G\323\031U\t\260\223>4\214\254\324g"
	.ascii	"wWzO\273k}\034\341\023\203\221\324\3765\213\204Fk\311\306\241\334J\275q\255\022\203\034mU"
	.ascii	"!\350\033\261Vg\360\201\335\363\243\020#\370\257\017]F\231jU\320\262\370\005\177\214\3148\276z\t"
	.ascii	"\2029\215\f\343@\357\0274\372\243\025>\007\3671nds\007\313\363!O\377N\202\035mllt"
	.ascii	"\244-\245~\207\311I\fC\035\334\233UiCL\322\353\314\367\t8,\002\275\204\356K\243\024~W"
	.ascii	"+\327M\275\276\316\376\224\021\"\017\006\332Oj\364\377\321\310\300wYJ\022\225\222\000\373\270\004Sp"
	.ascii	"\n;\247a\254h\342\360\365\245\2217\020\372\372\362\351\000mk\202>\341\301B\217\327o\351~\372`"
	.ascii	"\306n)M5\035=\266\3301\255_>\005\303\363\354B\275\264\214\225\013g\375Sc\241\f\2169!"
	.ascii	"\001V\267\264\371\252\230'r\255\215\\\023r\254^#\240\267aa\252\316\322N}\217\351\204\262\277\033"
	.ascii	"\3633+8\212\005\365\211\264\300H\255\013\272\342Zn\263=\245\003\265\223\217\3462\242\225\235\355\243Z"
	.ascii	"ae\331\307\351wge6\200\307rT\022+\313\356nP\331\2312\005e\314W\211^N\341\007J"
	.ascii	"\233\244w\304\315X\013$\027\360Gd\336\3328\375\255j\310\2472\215\222\031\201\240\257\204\355z\257P"
	.ascii	"\231\371\r\230\313\022\344Nq\307n<o\327\025\243\375w\\\222\336\355\245\273\00241\0359\254\013?"
	.ascii	"\345[\366\025\001\336On\262\ta!!&\230)\331\326\255\013\201\005\002x\006\320\353\272\026\243!\031"
	.ascii	"\213\301\363\331\232\255Z\327\234\301\261`\357\016jV\331\016\\%\254\013\232>\365\307b\240\354\235\004{"
	.ascii	"\374p\270\337~/B\211\275\263vO\353k),\367M\3026\324\3618\007\260\256s\342A\337Xd"
	.ascii	"\203DD5z\343\313\334\223\276\355\0173y\210u\207\335\305\022\303\004`xd\016\225\302\313\334\223`"
	.ascii	"K\003\204`\276\356\336kT\270\017x\266\302\2311\225\006-\266\253v3\227\220}d\213\311\2001n"
	.ascii	"mp\340\205\205\232\363\03739\347\263\330\245\3206;E\217q\341\362\271C|\251'H\b\352\321W"
	.ascii	"q\260(\241\347\266z\356\252\213\250\223mY\301\2440a!\262\202\336\264\367\030\275\227\335\235\231>6"
	.ascii	"\306\256K\342\334H\030/`\257\274\272Ur\233v1\351\357<n<\313\220U\263\371\306\233\227\037#"
	.ascii	"\304\037\3565\301C\250\226\317\310\344\bU\263n\2270\323\214\265\001h/\264+\005:ix\233\356H"
	.ascii	"\306\363*\314K\3361\\\037\215 \3760\260K\260f\264O\301\tp\215\267\023$y\b\233\372\233\007"
	.ascii	"EB\325\242\200\355\311\363R9\366wx\213\240\nuT\b\321c\254m\327kcp\224\025\373\364\036"
	.ascii	"\364\r0\332Q:\220\343\260Z\251=#d9\204\200d5\013-\361<\355\224q\201\204\366w\214\003"
	.ascii	"\354{\026[\346^N\205\302\315\320\226B\nYY\231!\020\2304\337\262rV\377\013J*\351^W"
	.ascii	"\001\330\244\nE\274F]\330\2713\245'\022\257\303\302\006\211+&;\2368\033X/8~\036\n "
	.ascii	"\317/\030\212\220\200\300\324\275\235H\231\302p\3410\3363\367RW\275\272\005\000\375\323,\021\347\324C"
	.ascii	"\305:\371\352g\271\215Q\300Rf\005\233\230\274q\365\227qV\331\205+\3768N\036eR\312\016\005"
	.ascii	"\352h\346`v9\254\227\227\264:\025\376\273\031\233\237\247\3544\265y\261LW\2561\241\237\300Qa"
	.ascii	"\234\f?E\336\032C\303\233;p\377^\004\365\351={\204\355\311z\331\374\306\364X\034\302\346\016K"
	.ascii	"\226]\360\375\r\\\365:z\356\264*\340.&\335\t\027\027\022\207\273\262\021\013\003\017\200\372$\357\037"
	.ascii	"\206k\2270\365\257\322\"\004F\322\302\006\270\220\215\345\272\345Ml\211\241\334\027\f4\310\346_\000("
	.ascii	"\2261\247\032\373S\3267\030d\327?0\225\224\017\262\027:\373\t\013 \255>a\310/)IMT"
	.ascii	"\210\206R4\237\272\357j\241}\020%\224\377\033\\6K\331f\315\273[\367\372m1\017\223r\344r"
	.ascii	"'v*\3235\366\363\007\360fe_\206M\252zPD\320(\227\347\205<8d\340\017\000\177\356\037"
	.ascii	"O\b\201\227\214 \225&\341\016E#\013*P\261\002\336\357\003\246\256\235\375L\2433'\214.\235Z"
	.asciz	"\345\367\333\003\332\005Sv\275\3154\024I\362\332\244\354\210J\322\315\325J{C\005\004\356Q@\371"
	.ascii	"S\227\257\007\273\223\357\327\247f\267=\317\320>X\305\036\013n\277\230i\316R\004\324]\322\377\267G"
	.ascii	"\2620\323\303#k5\215\006\033G\260\233\213\034\362<\270Bnl1l\263\r\261\352\213~\234\327\007"
	.ascii	"\022\335\b\274\234\373\373\207\233\302\356\341:k\006\212\277\301\037\333+$W\r\266K\246^\243 5\034"
	.ascii	"Y\300k!@o\250\315~\330\274\022\035#\273\037\220\t\307\027\236j\225\264U.\321f;\fu8"
	.ascii	"J\243\313\274\246S\322\200\233!88\241\303a>\226\343\202\230\001\266\303\220o\346\016]w\005=\034"
	.ascii	"\032\345\"\224@\361.iq\366]+<\307\300\313)\340Lt\347O\001!|H0\323\307\342!\006"
	.ascii	"\363\360\333\260\226\027\256\267\226\341|\341\271\257\337T\264\243\252\351q0\222%\235.\000\241\234X\216]"
	.asciz	"\215\203Y\202\314`\230\257\334\232\237\306\301H\352\2200\036Xe7H&e\274\245\323{\t\326\007"
	.ascii	"K\251B\b\225\035\277\300>.\217Xc\303\323\262\357\342Q\2738\024\226\n\206\277\034<x\327\203\025"
	.ascii	"\307(\235\314\004G\003\220\217\305,\367\236g\033\035&\207[\276_+\341\026\nX\305\203N\006XI"
	.ascii	"\341z\242]\357\242\356\354t\001gU\024:|Yz\026\tf\022*\246\311p\217\355\201._*%"
	.ascii	"\r\350fP&\224(\rk\214|0\205\367\303\374\375\022\021\fx\332S\033\210\263C\330\013\027\234\007"
	.ascii	"V\320\325\300P\315\326\315;W\003\273mh\367\232H\357\303\363?r\246<\314\212{1\327\300hg"
	.ascii	"\377o\372d\344\354\006\005#\345\005b\036C\343\276B\352\270Q$By5\000\373\311J\343\005\354m"
	.ascii	"\263\301U\361\345%\266\224\221{{\231\247\363{A\000&km\334\275,\302\364R\315\335\024^DQ"
	.ascii	"U\244\276+\253G1\211)\221\007\222O\242S\214\247\3670\276H\371IK=\324On\b\220\351\022"
	.ascii	"QI\024;K+PW\263\274KDk\377g\216\333\205c\026'i\275\270\310\225\222\3431o\030\023"
	.ascii	".\273\337\177\263\226\f\361\371\352\034\022^\223\232\237?\230[:\3046\021\337\257\231>]\360\343\262w"
	.ascii	"\244\260\335\022\234c\230\325k\206$\3000\237\321\245`\344\374X\003/|\321\212^\t.\025\225\241\007"
	.ascii	"\336\304.\234\305\251o)\313\363\204O\277a\213\274\b\371\250\027\331\006w\034]%\323z\374\225\267c"
	.ascii	"\310_\2368\002\2176\250;\344\215\317\002;C\220C&A\305]\375\241\2577\001/\003=\350\217>"
	.ascii	"<\321\357\350\215Lp\b17\3403\216\032\305\337\343\315`\022\245]\235\245\206\214%\246\231\b\326\""
	.ascii	"\224\242p\005\271\025\213/IE\bgpB\362\224\204\375\273a\341Z\034\336\007@\254\177y;\272u"
	.ascii	"\226\321\315p\300\3339b\232\212}l\213\212\376``\022@\353\274G\210\263^\236w\207{\320\004\t"
	.ascii	"\271@\371Hf-2\3649\f-\275\f/\225\0061\371\201\240\255\227v\026l*\367\272\316\252@b"
	.ascii	"\234\221\272\335\324\037\316\264\252\215L\307>\3331\317Q\314\206\255c\314c,\007\336\035\274?\024\342C"
	.ascii	"\240\225\242[\234t4\370Z\3227\312[|\224\326j1\311\347\247;\361f\254\f\264\215#\257\275V"
	.ascii	"\262;\235\301l\323\020\023\271\206#b\267k*\006\\O\241\327\221\205\233|TW\036~P1\252\003"
	.ascii	"\35335\365\343\271*6@=\271n\325h\2053rUZ\035R\024\016\236\030\023t\203m\250$\035"
	.ascii	"\037\316\324\377Hv\354\364\034\214\254T\360\352E\340|5\t\035\202%\322\210YH\353\232\334a\262C"
	.ascii	"d\023\225l\213=Q\031{\364\013\000&q\376\224g\225O\325\335\020\215\002d\t\224B\342\325\264\002"
	.ascii	"\273y\273\210\031\036[\345\2355z\301}\320\236\2403\352=`\342.,\260\302k'[\317U`2"
	.ascii	"\362\215\321(\313U\241\264\b\345l\030FF\314\352\211C\202l\223\364\234\304\0204]\256\t\310\246'"
	.ascii	"Ti=\304\n',\315\262\312fjW>J\335l\003\327i$Y\372y\231%\214=`\003\025\""
	.ascii	"\210\261\r\037\315\353\246\213\350[Zg:\327\3237ZX\365\025\243\337.\362~\241`\377tq\266,"
	.ascii	"\320\341\0139\371\315\356Y\361\343\214rD B\251\364\360\224zf\034\211\2026\364\2208\267\364\035{"
	.ascii	"\214\365\370\007\030\"._\324\t\224\324\237\\U\3430\246\266\037\215\250\252\262=\340R\323E\202ih"
	.ascii	"$\242\262\263\340\362\222\344`\021U+\006\236l|\016{\177\r\342\217\353\025\222Y\374X&\357\374a"
	.ascii	"z\030\030*\205]\261\333\327\254\335\206\323\252\344\363\202\304\366\017\201\342\272D\317\001\257=GL\317F"
	.ascii	"@\201I\361\247n<!TH+9\370~\036|\272\316)V\214\303\210$\273\305\214\r\345\252e\020"
	.ascii	"\371\345\304\236\355%eB\0033\220\026\001\332^\016\334\312\345\313\362\247\261r@_\353\024\315{8)"
	.ascii	"W\r \337%E,\034Jg\312\277\326-;\\0@\203\341\261\347\007\n\026\347\034O\346\230\241i"
	.ascii	"\355\312\305\3344D\001\3413\373\204<\226]\355G\347\240\206\355v\225\001p\344\371g\322{i\262%"
	.ascii	"\274x\032\331\340\262b\220g\226P\310\234\210\311G\270pP@fJ\365\235\277\241\223$\251\346is"
	.ascii	"dh\230\023\373?g\235\270\307]A\331\373\245<^;'\337;\314N\340\322LN\265=h \024"
	.ascii	"\320Z\314\301o\273\3564\213\254F\226\351\f\033jS\336k\246I\332\260\323\301\201\320aA;\3501"
	.ascii	"\227\321\235$\036\275x\264\002\301X^\0005\fb\\\254\272\314/\323\002\373-\247\b\365\353;\266`"
	.ascii	"O+\006\236\022\307\350\227\330\n2)O\217\344I?h\030oK\341\354[\027\003U-\266\036\317U"
	.ascii	"R\214\365}\343\265v06\314\231\347\335\271:\327 \356\023I\343\034\203\2753\001\272b\252\373V\032"
	.ascii	"X=\302e\020\020yX\234\201\224Pm\b\235\213\247_\305\022\251/@\342\324\221\bWde\232f"
	.ascii	"\354\311\235\\Pk>\224\0327|\247\273W%0Qv4AV\256s\230\\\212\305\231g\203\304\023"
	.ascii	"\200\320\213]j\373\334\304BH\032W\354\304\353\336eS\345\270\203\350\262\324'\270\345\310}\310\275P"
	.ascii	"\271\341\263ZF]:Ba?\361\307\207\301\023\374\266\271\265\354d6\370\031\007\2667\246\223\f\370f"
	.ascii	"\021\341\337n\2037m`\331\253\021\360\025>52\226;\267%\303:\260d\256\325_rDd\325\035"
	.ascii	"\232\310\272\b\000\346\227\302\340\303\341\352\021\352L}|\227\347\237\341\213\343\363\315\005\243c\017E::"
	.ascii	"}\022b3\370\177\244\217\025|\315q\304j\237\274\213\f\"ICEqn.s\237!\022Yd\016"
	.ascii	"'F9\3301/\217\007\020\245\224\336\2031\2358\200o\231\027ml\343\321{\250\251\223\223\215\2141"
	.ascii	"\230\323\035\253)\236f];\236-4X\026\222\374\315sY\363\375\035\205U\366\n\225%\303A\232P"
	.ascii	"\031\376\377*\003]t\362f\333$\177I<\237\f\357\230\205\272\343\323\230\274\024S\035\232g|L\""
	.ascii	"\351%\371\246\334n\300\2753\037\033d\364\363>y\211>\203\235\200\022\354\202\211\023\241(#\360\277\005"
	.ascii	"\344\022\305\r\335\240\201h\376\372\245D\310\r\347O@RJ\217k\216t\037\352\243\001\356\315wbW"
	.ascii	"\013\340\312#p\02326Y\317\254\321\n\317JT\210\034\032\322I\020t\226\247D*\372\303\214\013x"
	.ascii	"_0O#\274\212\363\036\b\336\005\024\275\177W\232\r*\3464\024\245\202^\241\267qbr\030\364_"
	.ascii	"@\225\266\023\350G\333\345\341\020&C;*]\363v\022x8\351&\037\254i\313\240\240\214\333\324)"
	.ascii	"\235\333\211\027\f\b\2169\365x\347\363% `\247]\003\275\006L\211\230\372\276f\251%\334\003j\020"
	.ascii	"\320S33\257\n\255\331\345\t\323\254\245\235f8\360\367\210\310\212eW<\372\276,\005Q\212\263J"
	.ascii	"\234\300\335_\357\321\317\326\316]W\367\375>+\350\3024\026 ]k\325%\233+\355\004\273\306A0"
	.ascii	"\223\325hg%+|\332\023\312\"DW\300\301\230\035\316\n\312\325\013\250\361\220\246\210\300\255\321\315)"
	.ascii	"H\341V\331\371\362\362\017.k5\237u\227\347\255\\\002l_\273\230F\032{\232\004\024h\275K\020"
	.ascii	"c\361\177\326_\232]\251\201V\307L\235\346+\351W\362 \336L\002\370\267\365-\007\373 *O "
	.ascii	"g\355\361h1\375\360Q\302;o\330\315\035\201,\336\362\322\004C\\\334DIq*\tW\314\350["
	.ascii	"y\260\3530=;\024\3100.e\275Z\025\211u1\\m\2171<<e\037\026y\302\027\373p%"
	.ascii	"Z$\270\013U\251.\031\321P\220\217\250\373\346\3105\311\244\210-\352\206yh\206\001\336\221_\034$"
	.ascii	"u\025\266,\1776\372>l\002\326\034vo\371\365b%\265e*\024\307\350\315\n\003S\352e\313="
	.ascii	"\252l\336@)\027\330(:s\331\"\360,\277\217\321\001[#\335\374\327\026\345\360\315_\335\016B\b"
	.ascii	"\316\020\364\004N\303X\003\205\006n'Z[\023\266!\025\271\353\307p\226]\234\210\333!\363T\326\004"
	.ascii	"J\372b\203\253 \377\315n>\032\342\324\030\341W+\3469\374\027\226\027\343\375i\027\274\357S\232\r"
	.ascii	"\325\265\275\335\026\301}^-\335\245\215\266\336T)\222\24243\027\b\266\034\327\032\231\030&OzJ"
	.ascii	"K*7\257\221\262\303$\362G\201qp\202\332\223\362\236\211\206d\205\204\3353\356\340#B1\226J"
	.ascii	"\225_\261_\002\030\247\364\217\033\\k4_\366=\022\021\340\000\205\360\374\315H\030\323\335L\f\265\021"
	.ascii	"\326\377\244\bD'\350\246\331v\025\234~\027\216s\362\263\002=\266H3wQ\314k\316M\316KO"
	.ascii	"o\013\235\304na\3420\027#\354\312\217qV\344\246Ok\362\233@\353H7_Ya\345\316B0"
	.ascii	"\204%$\342Z\316\037\247\236\212\365\222Vr\352&\364<\352\034\327\t\032\322\346\001\034\267\024\335\374s"
	.ascii	"A\254\233Dyp~B\n1\342\274m\343Z\205|\032\204_!v\256L\326\341\234\232\ft\2368"
	.ascii	"(\254\016W\366x\275\311\341\234\221'2\013[\345\355\221\233\241\253>\374e\2206&\326\345%\304%"
	.ascii	"\316\271\3344\256\263\374d\255\320H\343#\003P\227\0338\306b}\360\263E\210gZFySTa"
	.ascii	"n\336\327\361\246\006>?\b#\006\216'v\371>wl\212N&\366\024\214YGH\025\211\2409e"
	.ascii	"\031J\273\024\324\333\304\335\216OB\230<\274\262\031iq\3126\327\237\250H\220\275\031\360\0162e\017"
	.ascii	"s\367\322\303t\037\322\351Eh\304%ATP\3013\236\271\371\350\\Nbl\030\315\305\252\344\305\021"
	.ascii	"\306\340\375\312\261\321\206\324\201Q;\026\343\346?O\232\223\362\372\r\257\250Y*\0073\354\275\307\253L"
	.ascii	"\211\322x?\217x\217\300\237M@\241,\2470\376\235\314e\317\374\213w\362! \313Z\026\230\344~"
	.ascii	".\n\234\b$\226\236#8G\376:\300\304H\307*\241Ov*\355\333\027\202\205\0342\360\223\233c"
	.ascii	"\303\241\021\221\343\b\325{\211t\220\200\324\220++\031\375r\256\302\256\322\347\246\002\266\205<I\337\016"
	.ascii	"\023Av\204\322\304gg5\370\365\367?@\220\240\336\276\346\312\372\317\217\034i\243\337\321T\f\300\004"
	.ascii	"hZ\233YX\201\314\256\016\342\255\353\017OW\352\007\177\266\"t\035\344O\264O\235\001\343\222;@"
	.ascii	"\370\\F\213\201/\302M\370\357\200\024Z\363\240qW\326\307\004\255\277\350\256\364va\262*\261[5"
	.ascii	"\030s\214Z\307\332\001\243\021\252\316\263\235\003\220\355-?\256;\277|\007o\216\255R\340\370\352\030u"
	.ascii	"\364\273\223t\314d\036\247\303\260\243\354\331\204\275\345\205\347\005\372\f\305k\n\022\303.\0302\201\233\017"
	.ascii	"2l\177\033\304Y\210\244\23028\364\274`-\017\331\321\261\311)\251\025\030\304U\027\273\033\207\303G"
	.ascii	"\260fP\310P]\346\373\260\231\242\263\260\304\354b\340\350\032D\352T7\345_\215\324\350,\240\376\b"
	.asciz	"HO\354q\227SDQn]\214\311}\261\005\370k\306\303G\032\301b\367\334\231Fv\205\233\270"
	.ascii	"\320\352\336hv\335M\202#]hK Ed\310e\326\211]\315\317\024\2657\325uO\247)8G"
	.ascii	"\311\0029\255:S\331#\217X\003\357\316\335\302d\264/\341\317\220s%\025\220\323\344DM\213fl"
	.ascii	"\030\304yFu\332\322\202\360\215a\262\330\327;\346\n\353G\254$\357^5\264\3063HLhx "
	.ascii	"\f\202xz!\317H;\227>'\201\262\nj\367{\355\216\214\247el\251?C\212O\005\246\021t"
	.ascii	"\264u\261\030=\345\232W\002\241\222\363Y1qh\3655\357\036\272\354U\204\2179\214Er\250\311\036"
	.ascii	"m\310\235\2712\235eM\025\361:`u\334L\004\210\344\302\334,qL\263\3774\201\373te\023|"
	.ascii	"\233P\242\000\324\244\346\270\264\202\310\013\002\327\201\233au\225\361\233\314\347W`d\315\307\245\210\335:"
	.ascii	"F09Y\324\230\302\205\354Y\366_\2305~\217:n\366\362*\242,\035 \247\006\2441\021\272a"
	.ascii	"\362\3345\266pW\211\253\274\037l\366l\357\337\002\207\321\266\276h\002S\205t\236\207\314\374)\231$"
	.ascii	")\220\225\026\361\240\320\243\211\275~\272lk;\002\0073x&>Z\361{\347\354\330\273\f1 V"
	.ascii	"\326\205\342w\364\265Ff\223a\217lg\377\350@\335\224\265\253\021s\354\246M\354\214e\363F\310~"
	.ascii	"C\3264IC\223\211R\365\"\022\245\006\370\333\271\"\034\364\303\217\207m\2170\227\235M*jg7"
	.ascii	"\307.\242\035?\217^\233\023\315\001lw\035\017\023\270\237\230\242\317\217L!\325\235\2339#\367\252m"
	.ascii	"\242\216\255\254\277\004;X\204\350\213\024\350C\267)\333\305\020\b;X\036+\252\273\263\216\345IT+"
	.ascii	"G\276=\353bu:_\270\240\275\216T8\352\367\231rtE1\345\303\000Q\325'\026\347\351\004\023"
	.ascii	"\376\234\334j\322\024\230x\013\335H\213?\253\033<\n\306y\371\377\341\017\332\223\326-|-\336hD"
	.ascii	"\316\007c\370\306\330\232K(\f]C15\021!,wze\305f\250\324Rs$c~B\246]"
	.ascii	"\236F\031\224^5\273QT\307\335#L\334\3463b\231\177D\326\266\245\223c\275D\373o|\316l"
	.ascii	"\312\"\254\336\210\306\224\032\370\037\256\273\367n\006\271\017XY\2158\214\255\210\250,\237\347\277\232\362X"
	.ascii	"\366\315\016q\277dZK<),F8\345L\261\271:\013\325V\320C6pH[\030$7\371j"
	.ascii	"h>\347\215\253\317\016\351\245v~7\237o\003T\202Y\001\276\013[I\3606\036\364\247\304)vW"
	.ascii	"\210\250\306\tE\002 2s\211UK\0236\340\322\237(3<#6\342\203\217\301\256\f\273%\037p"
	.ascii	"\023\301\276|\331\366\030\235\344\333\277t\346\006J\204\326`N\254\"\265\365 Q^\225P\300[\nr"
	.ascii	"\355la\344\370\260\250\303}\250%\236\016f\000\367\234\245\274\364\037\006\343a\351\013\304\275\277\222\f."
	.ascii	"5Z\200\233C\t?\f\374\253Bb7\213N\350F\223\"\\\363\027\024i\354\360N\024\273\234\233\016"
	.ascii	"\356\276\261]\325\233\356\215\271?r\n7\253\303\311\221\327h\034\277\361\250D\336<\375\034\031Dm6"
	.ascii	"\255 W\373\217\324\272\373\016\r\371\333k\221\201\356\277CUcR1\201\324\330{3?\353\004\021\""
	.ascii	"\024\214\274\362C\027<\236;l\205\265\374&\332.\227\373\247h\016/\270\314D2Y\274\346\244gA"
	.ascii	"\356\217\316\370e&\276\302,\326\200\350\024\377g\351\356N6/~n.\361\366\322~\313p3\2634"
	.ascii	"\000'\366v(\235;d\353hv\016@\235\035]\204\006\374!\003CK\033j$U\"~\2738y"
	.ascii	"\314\326\201\206\356\221\305\315S\247\205\355\234\020\002\316\203\210\200X\301\205t\355\344e\376-n\374v\021"
	.ascii	"\270\016wI\211\342\220\333\243@\364\254*\314\373\230\233\207\327\336\376O5!\266\006i\362T>j\037"
	.ascii	"\233a\234[\320l\257\264\200\204\245\262\364\311\337-\304M\351\353\002\245O=4_}gL:\374\b"
	.ascii	"\3524\007\323\231\301\244`\326\\\0261\266\205\300@\225\202Y\367#>3\342\321\000\271\026\001\255/O"
	.ascii	"8\266;\267\035\331,\226\b\234\022\374\252w\005\346\211\026\266\3639\233ao\201\356D)_\231Q4"
	.ascii	"TN\256\224A\262\276Dl\357W\030Q\034T_\230\004\2156-k\036\246\253\367.\227\244\204TD"
	.ascii	"|}\352\237\320\374R\221\366\\\223\260\224l\201J@\\(G\252\232\216%\267\223(\004\246\234\270\020"
	.ascii	"n\360EZ\276A9ue_\234m\355\256|\320\266Q\377r\234kw\021\251M\r\357\331\321\322\027"
	.ascii	"\234(\030\227IGY=&?S$\305\370\353\022\025\357\303\024\313\277b\002\216Q\267w\325x\270 "
	.ascii	"j>?\007\030\257\362'i\020R\327\031\345?\375\"\000\246<,\267\343\"\247\306e\314cO!r"
	.ascii	"\311);\364\271\267\235\035u\217QOJ\202\005\326\304\235/1\275r\300\362\260E\025Z\205\254$\037"
	.ascii	"\223\246\007S@\177\343\264\225g3/\327\024\247\253\231\020vs\247\320\373\326\311\313q\201\305H\337_"
	.asciz	"\252\005\225\2162\b\326$\356 \024\f\321\301HG\242%\373\006\\\344\377\307\346\225\343*\236s\272"
	.ascii	"&\273\210\352\365&D\256\373;\227\204\331y\0066PNi&\f\003\237\\&\322\030\325\347})r"
	.ascii	"\326\220\207\\\336\230.Y\337\242\302E\323\267\277\345\"\231\264\371`;Z\021\363x\255g>:(\003"
	.ascii	"9\271\f\276\307\035$H\2000c\213M\233\3612\b\223(\002\r\311\337\323E\031'Fh)\341\005"
	.ascii	"PE,$\310\273\277\255\331\2010\320\354\f\310\274\222\337\310\365\246f5\204L\316X\202\323%\317x"
	.ascii	"ZI\234-\263\356\202\272|\271+\361\374\310\357\316\340\321\265\223\256\253-\260\233\215i\023\234\f\3009"
	.asciz	"h\235H1\216k\256\025\207\360+\234\253\034\205\252\005\372N\360\227Z\247\3112\370?k\007Rk"
	.ascii	"-\b\316\271\026~\313\365)\274zAL\361\0074\253\247\364+\316k\263\324\316u\237\032V\351\342}"
	.ascii	"\034x\225\235\341\317\340)\342\020c\226\030\337\201\2669kQp\3239\337W\"a\307;D\343WM"
	.ascii	"\313^\245\266\364\324p\336\231\333\205]\177R\001H\201\232\356\323@\304\311\333\355)`\032\257\220*k"
	.ascii	"\n\330\262[$\363\353w\233\007\271/G\0330\3303s\356L\362\346G\306\t!l'\310\022XF"
	.ascii	"\227\036\346\232\374\364#i\321_?\340\035(5W-\321\355\346C\256d\247J>-\321\351\364\330_"
	.ascii	"\331b\020*\262\276CM\026\33418u\373ep\327h)\336{J\r\030\220g\261\034+,\263\005"
	.ascii	"\225\201\325z,\244\374\367\314\3633Cn(\0242\235\227\0134\r\235\302\266\341\007sVH\032w1"
	.ascii	"\375\250M\322\314^\300\310\203\357\337\005\254\032\317\241a\315\371}\362\357\276\333\231\036G{\243VU;"
	.ascii	"\202\324M\341$\305\2602\266\244+\032TQ\263\355\363Z+(H`\321\243\3536sz\322y\300O"
	.ascii	"\r\305\206\fD\2134\334Q\346\224\314\311\3137\023\271<>dM\367\"d\b\315\343\272\302p\021$"
	.ascii	"\177/\277\211\2608\311Q\247\351\337\002e\275\227$S\344\200x\234\300\377\377\222\216\371\312\316gE\022"
	.ascii	"\264s\304\n\206\253\371?5\344\023\001\356\035\221\360\257\304\306\353`P\347J\r\000\207l\226\022\206?"
	.ascii	"\023\215\0046\372\374\030\234\335\235\211s\263\235\025)\252\320\222\237\0135\237\334\324\031\212\207\356~\365&"
	.ascii	"\336\r*x\311\f\232U\205\203q\352\262\315\035U\214#\3571[\206b\177=asyv\247JP"
	.ascii	"\261\357\207V\325,\253\f{\361z$b\321\200Qg$ZO4Z\301\205i0\272\235=\224A@"
	.ascii	"\335\252l\242Cw!K\316\267\212d$\264\246G\343\311\373\003zO\035\313\031\320\000\230B1\331\022"
	.ascii	"\226\314\353C\272\356\300\303\257\234\352&\234\234t\215\306\314w\034\356\225\372\331\0174\204v\331\241 \024"
	.ascii	"OY7\323\231w\306\000{\244:\262@Q<^\225\363_\343T(\030D\022\240YC1\222O\033"
	.ascii	"\261f\230\24400\3173YH_!\322s\037%\366\364\336Q@\252\202\253\366#\232o\325\221\361_"
	.ascii	"Q\t\025\211\235\020\\>ji\351-\221\372\3169 0_\227?\344\352 \256-\023\177*W\233#"
	.ascii	"h\220-\2543\324\236\201#\205\311_y\253\203(=\353\223U\200rE\357\3136\217ujR\f\002"
	.ascii	"\211\314B\360Y\3571\351\266K\022\216\235\234X,\227Y\307\256\212\341\310\255\f\305\002V\n\376,E"
	.ascii	"\274\333\330\236\3704\230wl\244|\334\371\252\362\310t\260\341\243\334LR\251w81\025F\314\252\002"
	.ascii	"\337wxd\240\367\240\206\237|`\016'd\304\273\311\021\373\361%\352\027\253{\207K0{}\373L"
	.ascii	"\022\357\211\227\302\231\206\342\r\031W\337q\315n+\320p\311\354W\310C\303\305:MC\274L\035["
	.ascii	"\376u\233\270l=\264r\200\334j\234\331\224\306T\237L\343>7\252\303\270dS\0079+b\264\024"
	.ascii	"&\237\n\314\025&\373\266\345\314\215\270+\016O:\005\247i3\213I\001\023\321-YX\022\367\230/"
	.ascii	"\001\247TOD\256\022.\336\327\313\251\360>\376\374\340]\203u\r\211\277\316TEa\347\351b\200\035"
	.ascii	"V\236\017\265L\247\224\f \023\216\216\251\364\037[g\0170\202!\314*\232\371\252\006\330I\342j:"
	.ascii	"Z|\220\251\205\332zeb\017\271\221\265\250\016\032\351\2644\337\373\035\016\215\363_\362\256\350\214\213)"
	.ascii	"\336e!\n\352rz\203\366y\317\013\264\007\253?p\2568w\3076\026R\334\327\247\003\030'\246k"
	.ascii	"\262\f\367\357Sy\222*vp\025y*\311\211Kj\317\2470zE\030\224\205\344\\M@\250\2704"
	.ascii	"53i\203\265\354n\302\375\376\265c\337\023\250\325s%\262\244\232\252\223\242j\034^F\335+\326q"
	.ascii	"\365^\367\261\332\265-\315\365e\260\026\317\225\177\327\205\360I?\352\037W\024=++&!63\034"
	.ascii	"\200\337x\323(\3143e\264\244\017\nyC\333\366Z\332\001\367\371_d\343\244+\027\363\027\363\325t"
	.ascii	"\201\312\331gT\345o\2507\214)+u|\2139;b\254\343\222\bm\332\214\331\351GE\314\353J"
	.ascii	"\020\266Ts\236\215@\013n[\250[S2k\200\007\242XJ\003:\346\333,\337\241\311\335\331;\027"
	.ascii	"\311\001m'\033\007\360\022p\214\304\206\305\272\270\347\251\373\326q\233\022\bS\222\267=Z\371\373\210]"
	.ascii	"\337rX\376\036\017P+\301\0309\324.X\326X\340:g\311\216'\355\346\031\243\236\261\023\315\341\006"
	.ascii	"S\003[\236b\257+GG\004\215'\220\013\252;'\277C\226F_x\f\023{\203\215\032j:\177"
	.ascii	"#o\026oQ\255\320@\276j\253\037\2232\216\021\216\bM\240\024^\343?fb\341&5`\2000"
	.ascii	"\013\200=]9D\346\367\366\355\001\311U\325\250\2259c,Y0x\315h~0Q.\355\375\3200"
	.ascii	"PG\270h\036\227\264\234\317\273df)r\225\240+A\372r&\347\215\\\331\211\305QC\b\025F"
	.ascii	"\2633\022\362\032MY\340\234M\314\360\216\347\333\033w\232I\217\177\030eih\230\t, \024\222\n"
	.ascii	".\240\271\256\300\031\220\274\256L\003\026\r\021\307U\3542\231e\001\365m\016\376]\312\225(\r\312;"
	.ascii	"\277\001\314\236\266\216h\234o\211D\246\255\203\274\360\342\237z__\225-\312A\202\362\215\003\264\250N"
	.ascii	"\244b]<\2741\360@`z\360\317>\213\374\031E\265\017\023\242=\030\230\315\023\217\256\335\3361V"
	.ascii	"\002\322\312\361\nF\355*\203\356\214\244\005S0F_\032\361IEw!\221c\244,T0\t\316$"
	.ascii	"\205\013\363\375U\241\317?\244.76\216\026\367\322D\370\222d\336d\340\262\200BO2\247(\231T"
	.ascii	"\006\301\006\375\365\220\350\037\362\020\210]5h\304\265>\257\214n\376\bx\202K\327\006\212\302\343\324A"
	.ascii	".\032\356c\2472n\362\352\375_\322\267\344\221\256iM\177\321;\323;\274j\377\334\300\336f\033I"
	.ascii	"\241d\332\320\216J\360uK(\342g\257,\"\355\244{{\037y\2434\202g\213\001\267\260\270\366L"
	.ascii	"\2472\352\307=\261\365\230\230\333\026~\314\370\325\343G\331\370\313R\277\n\254\254\344^\310\3208\363\b"
	.ascii	"\275s\032\231!\250\203\303z\f2\337\001\274'\253cpw\204\0333=\301\231\212\007\353\202J\rS"
	.ascii	"\236\277\232lEsim\200\250\000I\374\262\177%P\270\317\310\022\364\254+[\275\277\f\340\347\263\r"
	.ascii	"%H\371\34106L\000ZS\253\214&x-~\213\377\204\314##H\307\271p\027\020?u\352e"
	.ascii	"cc\t\342>\374f=k\313\265a\177,\326\201\032;D\023B\004\276\017\333\241\341!\031\354\244\002"
	.ascii	"_y\317\361ba\310\365\362W\356&\031\206\214\021x5\006\034\205$!\027\317\177\006\354]+\3216"
	.ascii	"\242\270$;\232%\346\\\270\240\257E\314zW\2707p\240\213\350\346\313\314\277\tx\022Q<\024="
	.ascii	"WE\025y\221'm\022\n:x\374\\\217\344\325\254\233\027\337\350\266\2756Y(\250[\210\027\365."
	.ascii	"Q/[0\373\277\356\226\270\226\225\210\2558\371\323%\335\325F\307-\365\360\225\000:\273\220\202\226W"
	.ascii	"\334\256X\214N\2277F\244A\360\253\373\"\357\271\212q\200\351V\331\205\341\246\250C\261\372x\033/"
	.ascii	"\001\341 \nC\270\032\367G\354\360$\215e\223\363\321\356\342n\250\tu\317\341\243*\3345>\304}"
	.ascii	"\030\227>'\\*xZ\224\375N^\231\306v5>}#\037\005\330.\017\231\n\325\202\035\270O\004"
	.ascii	"\303\331}\210ef\226\205US\260K1\233\017\311\261y \357\370\215\340\306/\301\214u\026 \367~"
	.ascii	"\331\343\007\251\305\030\337\301YcL\316\0357\263WI\273\001\2624Ep\312.\3350\234?\202y\177"
	.ascii	"\272\207\365h\360\037\234j\336\310P\000N\211'\b\347[\355}U\231\277<\360\326\006\034C\260\251d"
	.ascii	"\350\023\265\2439\3224\203\330\250\037\271\324p6\3013\275\220\3656A\265\022\264\331\204\327s\003N\n"
	.ascii	"\031)}[\241\326\263.5\202:\325\240\366\264\260G]\244\211C\316Vql4\030\316\n}\032\007"
	.ascii	"1D\341 R5\f\314AQ\261\t\007\225e\r6_\235 \033b\365\232\323Uwa\367\274i|"
	.ascii	"\013\272\207\310\252-\007\323\356b\245\277\005)&\001\213v\357\300\0020T\317\234~\352Fq\314;,"
	.ascii	"_)\350\004\353\327\360\007}\363P/%\030\333\020\327\230\027\027\243\251Q\351\035\245\254\"s\232Zo"
	.ascii	"\276D\331\243\353\324)\347\236\257x\200@\t\236\215\003\234\206GzV%E$;\215\356\200\226\253\002"
	.ascii	"\305\306A/\f\000\241\213\233\373\376\f\301y\237\304\237\034\305<pG\372N\312\257G\341\242!NI"
	.ascii	"\232\r\345\335\205\212\244\357I\242\271\017N\"\232!\331\366\036\331\035\037\t\3724\273F\352\313v]k"
	.ascii	"\"%x\036\027A\371\340\3236i\003t\256\346\361F\307\374\320\242>\213@>1\335\003\234\206\373\026"
	.ascii	"\224\331\f\354lUW\210\272\035\320\\o\334rdw\264B\217\024i\001\257Ts'\205\3663\343\n"
	.ascii	"b\t\2663\227\031\216(3\341\253\330\264r\374$>\320\221\t\355\367\021Hu\320p\217\213\343\201?"
	.ascii	"$\310\027_5\177\333\n\244\231B\327\303#\271t\367\352\370\313\213>|\325=\334\336L\323\342\323\n"
	.ascii	"\376\257\331~\314\017\221\177K\207e$\241\270\\T\004G\fK\322~9\250\223\t\365\004\301\017QP"
	.ascii	"\235$n3\305\017\fo\331\3171\303\031\336^t\034\376\356\t\000\375\326\362\276\036\372\360\213\025|\022"
	.ascii	"t\271Q\256\304\217\242\336\226\376Mt\323s\231\035\250H8\207\013h@b\225\337g\321y$\330N"
	.ascii	"\242y\230.B|\031\366G6\312R\324\335J\244\313\254NK\301?A\233hO\357\007}\370N5"
	.ascii	"u\331\305`\"\265\343\376\270\260A\353\374.5P<e\366\2510\254\b\210m#9\005\322\222-0"
	.ascii	"w\361\340\344\266o\274-\223j\275\244)\277\341\004\350\366zx\324f\031^`\320&\264^_\334\016"
	.ascii	"=(\244\274\242\301\023x\331=\206\241\221\360b\355\206\372h\302\270\274\307\256L\256\034o\267\323\345\020"
	.ascii	"g\216\332S\326\277STA\366\251$\354\036\334\351#\212W\003;&\207\277r\272\0346Ql\264E"
	.ascii	"\344\343\177\212\335M\235\3160\016bvVd\023\253X\231\016\263{OYK\337)\0222\357\n\034\\"
	.ascii	"\241\177O1\277*@\251P\364\214\216\334\361W\342\204\276\250#K\325\273\035;q\313m\243\277w!"
	.ascii	"\217\333y\372\274\033\b7\263Y_\302\036\201H`\207$\203\234evz\b\273\265\212}8\031\346J"
	.ascii	"\203\373[\230D~\021a61\226q*F\340\374K\220%\324H4\254\203d=\244[\276Zhu"
	.ascii	".\243DS\252\366\333\215x@\033\264\264\352\210}`\r\023J\227\353\260^\003>\277\027\033\331\000\032"
	.ascii	"\262\362a\3533\t\226nRI\377\311\250\017=Tie\366z\020ur\337\252\346\260#\266)U\023"
	.ascii	"\376\203.\342\274\026\307\365\301\205\t\350\031\353+\264\256J%\0247\246\235\354\023\246\220\025\005\352rY"
	.ascii	"\030\325\321\255\327\333\360\030\021\037\301\317\210x\237\227\233u\024q\360\3412\207\001:\312e\032\270\265y"
	.ascii	"\021x\217\334 \254\324\017\250OM\254\224\322\232\2324\0046\263d-\033\300\333;_\220\225\234~O"
	.ascii	"\376\231R5=D\310q\327\352\353\333\034;\315\213f\224\244\361\236I\222\200\310\255D\241\304\356B\031"
	.ascii	".0\201W\274Kgb\017\334\255\2119\017R\330\306\331\373S\256\231)\214L\216c.\331:\2311"
	.ascii	"\222I#\256\031S\254}\222>\352\f\221=\033,\"\021<%\224\344<Uu\312\371N1e\n*"
	.ascii	":y\034<\315\0326\317;\2745Z\254\274\236/\253\246\315\250\351`\350`\023\032\352m\233\303]\005"
	.ascii	"\302'\371\367\177\223\267-5\246\320\027\006\037t\333v\257U\021\242\363\202Y\355-|d\030\342\366L"
	.ascii	"\266[\215\302|\"\031\261\253\377Mw\274N\342\007\211,\243\344\316x<\250\266$\252\020w0\032\022"
	.ascii	"\311\203t\307>qY\326\257\226+\270w\340\277\210\323\274\227\020#(\236(\233:\355lJ\271{R"
	.ascii	"\227J\003\237^]\333\344-\27440\t\374S\341\261\323Q\225\221F\005F-\345@zl\307?3"
	.ascii	".H[\231*\231=V\00188n|\320\0054\345\330d/\3365PH\367\251\247 \233\006\211k"
	.ascii	"w\333\307\265\214\372\202@U\3014\307\370\206\206\006~\245\347\366\331\310\346)\317\233c\247\b\323s\004"
	.ascii	"\r\"pbA\240*\201N[$\371\372\211Z\231\005\357rP\316\304\255\377s\353s\252\003!\274#"
	.ascii	"\005\236X\003&y\356\312\222\304\334F\022BK+O\251\001\346t\357\241\002\0324\004\336\277s/\020"
	.ascii	"\232\034Q\265\340\332\264\242\006\377\377+)`\310z4BP\365]7\037\230-\241N\332%\327k?"
	.ascii	"\306EW\177\253\271\030\353\220\306\207W\356\212:\002\251\257\367-\332\022'\267=\001\\\352%}Y6"
	.ascii	"\254X`\020{\215Ms_\220\306o\236W@\331-\223\002\222\371\370fd\320\326`\332\031\314~{"
	.ascii	"\233\372|\247QJ\256mP\206\243\347T6&\202\333\202-\217\315\377\273\t\272\312\365\033f\334\276\003"
	.ascii	"\ri\\i<7\302xn\220B\006f.%\335\322+\341JDD\035\225V9t\001v\2555B"
	.ascii	"\365u\211\007\r\313Xb\230\362\211\221TB)I\344n\343\342#\264\312\240\241f\360\315\260\342|\016"
	.ascii	"\371pK\331\337\376\246\376-\272\374\301Q\3000\361\211\253/\177~\324\202H\265\356\354\212\023VRa"
	.ascii	"\243\205\214\304:d\224\304\2559a<\364\0356\375HM\351:\335\027\333\tJg\264\217]\nnf"
	.ascii	"\r\313pHN\366\273*k\213E\252\360\274e\315]\230\350u\272N\276\232\344\336\024\325\020\310\013\177"
	.ascii	"\240\023rs\255\235\254\203\230.\367.\272\370\366\237Wi\354C\335.\0361u\253\305\336}\220:\035"
	.ascii	"o\023\364&\244k\000\27150\340W\2366g\215(<FO\331\337\310\313\365\333\356\370\274\215\037\r"
	.asciz	"\334\201\320>1\223\026\272\2004\033\205\255\2372)\313!\003\003<\001(\001\343\375\033\243D\033\001"
	.ascii	"\\\247\nji\037V\026j\275RX\\r\277\301\255fy\232\177\335\250\021&\020\205\322\242\210\331c"
	.ascii	"\fl\306?l\240\337?\322\r\326M\216\343@]qM\216&8\213\343z\341W\203n\221\215\304:"
	.ascii	".#\275\257S\007\022\000\203\366\330\375\270\316+\351\221+\347\204\263i\026\370f\240h#+\325\3723"
	.ascii	"\350\317\"\304\320\310,\215\313:\241\005{O+\007o\245\366\354\346\266\376\243\342q\n\271\314U\303<"
	.ascii	"\026\036\344\305\306I\006T5w?30d\370\nF\347\005\363\322\374\254\262\247\334V\242)\364\300\026"
	.ascii	"1\221>\220C\224\266\351\3167Vz\313\224\244\270D\222\272\272\244\321|\310hu\256kB\257\036c"
	.ascii	"\350\rp\243\271u\331GR\005\370\342\373\305\200r\341]\3442'\217eS\265\200_f\177,\037C"
	.ascii	"\237\376f\332\020\004\351\263\246\345\026lRK\335\205\203\277\371\036a\227=\274\265\031\251\036\213d\231U"
	.ascii	"\031{\217\205Dc\002\326JQ\352\241/5\253\024\327\251\220 \032D\000\211&;%\221_q\004{"
	.ascii	"\306\272\346\304\200\302v\263\013\233\035m\335\323\016\227D\371\013EX\225\232\260#\342\315W\372\254\320H"
	.ascii	"C\256\366\254(\275\355\203\264z\\}\213|5\206D,\353\267iG@\300?X\366\302\365{\263Y"
	.ascii	"q\346\253}\344&\017\2667:/b\227\241\321\361\224\003\226\351~\316\bB\333;m3\221A#\026"
	.ascii	"@\206\363\037\326\234I\335\240%6\006\303\233\315)\303=\327=\002\330\342Q1\222; zp%J"
	.ascii	"\366\177&\366\336\231\344\271C\b,t{\312rw\261\362\244\351?\025\240#\006P\320\325\354\337\337,"
	.ascii	"j\355\366S\212f\267*\241p\321\035XBB0a\001\342:L\024\000@\374I\216$m\211!W"
	.ascii	"N\332\320\241\221P](\b>\376\265\247o\252K\263\223\223\341|\027\345c\3750\260\304\2575\311\003"
	.ascii	"\256\033\030\375\027Un\013\264c\271+\237b\"\220%F\0062\351\274\tU\332\023<\366t\335\216W"
	.ascii	"=\f+I\306vr\231\374\005\342\337\304\302\314G<:b\335\204\233\322\334\242\307\210\002Y\253\302>"
	.ascii	"\313\3212\256\t:!\247\325\302\365@\337\207+\017)\253\036\350\306\244\256\013^\254\333jl\366\033\016"
	.ascii	"\271{\330\344{\322\240\241\355\0329a\353M\213\251\203\233\313s\320\335\240\231\316\312\017 Z\302\325-"
	.ascii	"~\210,y\351\325\253\342]m\222\313\030\000\002\032\036_\256\272\315i\272\277_\217\350Z\263H\005s"
	.ascii	"4\343\326\241K\t[\200\031?5\tw\361>\277+p\"\006\313\006?B\335Ex\330w\"ZX"
	.ascii	"\356\270\250\313\243Q5\304\026_\021\262\035o\242eP8\214\253RO\017v\312\270\035A;DC0"
	.ascii	"b\211\3243\202_\212\241\177%x\354\265\304\230f\377A>7\245o\216\247\037\230\357P\211'Vv"
	.ascii	"\235\317\206\352\243sp\341\334_\025\007\267\373\214:\216\212\2031\374\347SH\026\366\023\266\204\364\273("
	.ascii	"\300\310\037\325Y\317\3038\362\266\006\005\375\322\355\233\217\016W\253\237\020\277&\246F\270\301\250`A?"
	.ascii	"|l\023o\\/a\362\276\021\335\366\007\321\352\2573o\336\023\322\232~R]\367\210\2015\313y\036"
	.ascii	"\201\201\340\365\330S\351w\331\336\235)D\f\245\204\345%E\206\f-l\334\364\362\3219-\265\212G"
	.ascii	"\361\343\367\356\30364\001\370\020\236\376\177j\213\202\374\336\371\274\345\b\371\17718;:\033\225\327e"
	.ascii	"Y\321R\222\323\244\246f\007\310\032\207\274\341\335\345o\311\301\246@k,\270\024\"!\032Az\330\026"
	.ascii	"\203\005N\325\342\325\244\373\372\231\275.\327\257\037\342\217w\351ns\302zI\336mZzW\013\231\037"
	.ascii	"\025b\006BZ~\275\263\301$Z\f\315\343\233\207\267\224\371\326\261]\300W\246\214\363e\201|\370("
	.ascii	"\326\367\350\033\255N4\243\217y\352\254\353P\036}R\340\rR\236V\306w>mMS\341/\210E"
	.ascii	"\344o<\224)\231\254\330\242\222\203\243a\361\371\265\363\232\310\276\023\333\231&t\360\005\344<\204\317}"
	.ascii	"\326\203yu]4if\246\021\252\027\021\355\266b\217\022^\230W\030\335}\335\366&\366\270\345\217h"
	.ascii	"\3002GJH\326\220l\2312V\312\375C!\325\341\306]\221\303(\276\263\033\031's~h9g"
	.ascii	"\300\032\f\310\235\314m\2466\2448\033\364\\\240\227\306\327\333\225\276\363\353\247\253}~\215\366\270\240}"
	.ascii	"\246uV8\024 x\357\350\251\375\2520\237d\242\313\250\337\\P\353\321L\263\300M\035\272Z\021F"
	.ascii	"v\332\265\303S\031\017\324\233\236\021!so\254\035`Y\262\376!`\314\003KKg\203~\210_Z"
	.ascii	"\271C\246\240\323(\226\236d \303\346\000\313\303\2652\354-|\211\002S\233\f\307\321\325\342z\343C"
	.ascii	"\021=\241p\317\001c\217\304\320\r5\025\270\316\317~\244\274\244\324\227\002\3674\024M\344V\266i6"
	.ascii	"3\341\246\355\006?~8\300:\241\231Q\0350g\0218&6\370\330Z\275\276\351\325O\315\346!j"
	.ascii	"\343\262\231f\022)A\357\001\023\215pG\b\323q\275\260\202\021\3202T26\213\036\000\007\0337E"
	.ascii	"_\346F0\n\027\306\361$5\322\000**qXU\267\202\214<\275\333iW\377\225\241\361\371kX"
	.ascii	"\013y\370^\215\b\333\246\3457\ta\334\360xR\270n\241a\322I\003\254y!\345\2207\260\257\016"
	.ascii	"\035\256u\017^\200@Q0\314b&\343\373\002\354m9\222\352\036\337\353,\263[C\305D3\256D"
	.ascii	"/\004H7\301U\005\226\021\252\013\202\346A\232!\fmHs8\367\201\034a\306\002Zg\314\2320"
	.ascii	"\356C\245\273\271\211\362\234Bq\311Z\235\016v\363\252`\223O\306\345\202\035\217g\224\177\033\"\325b"
	.ascii	"<z\367:&\324\205uM\024\351\376\021{\256\337=\031\367Y\200p\006\2457 \222\203S\232\362\024"
	.ascii	"m\223\320\030\234)LR\f\032\f\212l\265k\3101\206J\333.\005u\243bEu\274\344\375\016\\"
	.ascii	"\365\327\262%\334~q\337@0\265\231\333p\371!bL\355\303\2674\222\332>\t\356{\\6r^"
	.ascii	">\263\b/\0069\223}\2762\237\337\345Y\226[\375\275\236\037\255=\377\254\267Is\313U\005\262p"
	.ascii	"\177!qE\007\374[W[\331\224\006]gy73\036\031\364\2737\n\232\274\352\264GL\020\361w"
	.ascii	"L,\021U\305\023Q\276\315\037\210\232:B\210fG;P^\205wfDJ@\006J\21794\016"
	.ascii	"(\031K>\t\013\223\030@\366\363s\016\341\343}o]9s\332\0272\364>\2347\312\326\336\212o"
	.ascii	"\350\275\316>\331\"}\266\007/\202'A\350\263\t\215m[\260\037\246?tr#6\2126\005T^"
	.ascii	"\232\262\267\375=\022@\343\221\262\032\242\341\227{H\236\224\346\375\002}\226\371\227\336\323\310.\347\rx"
	.ascii	"r'\364\000\363\352\037g\252A\214**\353r\217\22227\227\327\177\241)\246\207\2652\255\306\357\035"
	.ascii	"\274\347\232\bE\205\342\n\006M\177\034\317\336\2158\270\021H\nQ\025\2548\344\214\222q\366\213\262\016"
	.ascii	"\247\225Q\357\032\276[\257\355\025{\221w\022\214\024.\332\345z\373\367\221)g(\335\370\033 }F"
	.ascii	"\251\347zV\275\364\036\274\275\230D\326\262Lb?\310N\037,\322d\020\344\001@8\272\245\305\371."
	.ascii	"\255O\357t\232\221\376\225\242\b\243\366\354{\202:\001{\244\t\323\001N\226\227\307\243[O<\304q"
	.ascii	"\315t\236\372\366m\375\266z&\257\344\274x\202\361\016\231\357\361\320\263U\202\223\362\305\220\243\214uZ"
	.ascii	"\224\334a\035\213\221\340\214f0\201\232F6\355\215\323\252\350\257)\250\346\324?\3249\366'\200s\n"
	.ascii	"\225$F\331\020'\267\242\003P}\325\322\306\250:\312\207\264\240\277\000\324\343\354r\353\263D\342\272-"
	.ascii	"\314\341\377W/J\017\230C\230\203\341\r\rg\000\375\025\373IJ?\\\020\234\246&Qc\312\230&"
	.ascii	"\016\331=^/p=.\206S\322\344\030\t?\236j\251M\002\366>w^23\372J\fK\000<"
	.ascii	"x\272\2602\2101e\347\213\377\\\222\3671\0308\314\037)\240\221\033\250\b\007\353\312I\314=\264\037"
	.ascii	"+\270\364\006\254F\251\232\363\304\006\250\245\204\242\034\207G\315\306_&\323>\027\322\037\315\001\375Ck"
	.ascii	"\363\016v>XB\307\265\220\271\n\356\271R\334u?\222+\007\302'\024\277\360\331\360o-\013Bs"
	.ascii	"D\305\227FK]\247\307\277\377\017\337H\370\375\025ZxF\252\353\271h(\024\367R[\020\327hZ"
	.ascii	"\006\036\205\236\313\366,\257\3048\"\306\0239Y\217s\363\373\231\226\270\212\332\236\2744\352/c\265="
	.ascii	"\325%\230\202\261\220I.\221\211\232>\207\353\352\355\370JpL9=\360\356\016+\337\225\244~\031Y"
	.ascii	"\330\331]\367+\356n\364\245Yg9\366\261\027\rsr\236I1\321\362\033\023_\327I\337\0322\004"
	.ascii	"\256Z\345\344\031`\341\004\351\222/~zC{\347\244\232\025o\301-\316\307\300\f\327\364\301\375\352E"
	.ascii	"\355\261\314\317$F\016\266\225\003\\\275\222\302\333Y\311\201\004\334\035\235\2401@\331V]\352\316s?"
	.ascii	"+\327E\200\205\001\204iQ\006/\317\242\372\"L\306-\"ke6\032\224\336\332b\003\310\353^Z"
	.ascii	"\306\215N\n\321\277\247\2679\263\311D~\000W\276\372\256W\025\177 \301`\333\030b&\221\210\005&"
	.ascii	"B\345v\306<\216\201L\255\314\316\003\223,B^\b\237\022\264\312\314\007\354\270CD\262\020\372\355\r"
	.ascii	"\004\377`\203\246\004\367Y\364\346av\336?\331\303Q5\207\022s*\033\203W]aN.\f\255T"
	.ascii	"*R+\270\325g;\356\353\301\245\237Fc\3616\323\237\301n\362\322\264\245\b\224z\247\272\262\354b"
	.ascii	"t(\266\2576(\007\222\245\004\341y\205^\315_J\2410\306\255\001\255Z\230?fuP=\221a"
	.ascii	"=+\025aRy\355\345\321\327\335\016}5bIqLk\271\320\310\202t\276\330f\251\031\371Y."
	.ascii	"\33212\0326-\306\rp\002 \2242XG\372\316\224\225?Q\001\330\002\\]\3001\241\302\333="
	.ascii	"\024\273\226'\242W\252\363!\332\007\233\267\272:\210\0349\2401\030\342K\345\371\0052\3308\373\347^"
	.ascii	"K\305^\316\371\017\334\232\r\023/\214k*\234\003\025\225\370\360\307\007\200\002k\263\004\254\024\203\226x"
	.ascii	"\216jDA\313\375\215S\3717IC\251\375\254\245x\214<&\215\220\257F\t\r\312\233<c\320a"
	.ascii	"\337s\374\370\274(\243\255\3747\360\246]i\204\356\t\251\3028\333\264\177c\334{\006\370-\254#["
	.ascii	"f%\333\3775Itc\273h\013x\211k\275\305\003\354>U\2002\033o\365\327\256G\330_\226n"
	.ascii	"{R\200\356S\271\322\232\215m\336\372\252\031\217\350\317\202\016\025\004\027q\016\334\336\225\335\271\273\271y"
	.ascii	"ts\237\216\256}\231\321\026\b\273\317\370\2422\240\n_Dm\022\272l\3154\270\314\nF\021\250\033"
	.ascii	"\302&1j@U\263\353\223\303\310h\250\203c\322\202z\271\345)d\flG!\375\311X\361eP"
	.ascii	"T\231B\f\373i\201pg\317n\327\254\000F\341\272E\346p\212\271\252.\362\372\244X\236\363\2019"
	.ascii	"\336o\346m\245\337E\310:H@,\000\245R\3412\366\264\307c\341\322\351e\033\274\334.E\3640"
	.ascii	"\223\n#Yu\212\373\030]\364\346`i\217\026\035\265<\251\024E\251\205:\375\320\254\0057\b\3348"
	.ascii	"@\227u\305\202'm\205\314\276\234\371iE\023\372qN\352\300s\374D\210i$?Y\032\232-c"
	.ascii	"\247\204\f\355\021\375\t\277:i\237\r\201q\360cy\207\317W-\214\220!\242K\366\212\362}Z:"
	.ascii	"\246\313\007\270\025k\273\366\327\360T\274\337\307#\030\013g)n\003\227\035\273WJ\355G\210\364$\013"
	.ascii	"\307\352\033Q\276\324\332\334\362\314&\355u\200S\244e\232_\000\237\377\234\341c\037HuD\367\3744"
	.ascii	"\230\252\317x\253\035\273\245\362r\013\031g\242\355\\\216`\222\n\021\311\t\223\260t\263/\004\243\031\001"
	.ascii	"\312g\227xL\340\227\301}F\3318\313Mq\270\250_\371\203\202\210\336U\367c\372M\026\334;="
	.ascii	"}\027\302\350\234\330\242g\301\320\225h\366\245\235f\260\242\202\262\345\230e\365s\n\342\355\361\210\300V"
	.ascii	"\002\217\363$\254_\033X\275\f\343\272\376\351\013\251\360\222\317\212\002i!\232\217\003Y\203\244~\213\003"
	.ascii	"\027n\250\020\021=m3\372\262u\0132\210\363\327\210)\007%v3\025\371\207\213\020\231kLg\t"
	.ascii	"\370o1\231!\370N\237O\215\247\352\202\322I/t1\357Z\253\245q\te\353iY\0021^n"
	.ascii	"\"b\006c\016\373\0043?\272\254\207\211\0065\373\243a\020\214w$\031\275 \206\203\321C\255X0"
	.ascii	"\373\223\345\207\365bl\261q>]\312\336\355\231Im>\314\024\340\301\221\264\250\333\250\211G\021\365\b"
	.ascii	"\320cv\345\375\017<2\020\246.\2428\337\303\005\232O\231\254\275\212\307\275\231\334\343\357\244\237T&"
	.ascii	"nf?\257I\205F\333\245\016J\361\004\317\177\327G\f\272\244\367?\362=\205<\3162\341\337\020:"
	.ascii	"\326\371k\036FZ\035t\201\245ww\374\263\005#\331\323td\242tU\324\377\340\001d\334\341&\031"
	.ascii	"\240\316\027\352\212N\177\340\375\301\037:F\025\325/\361\300\3621\375\"S\027\025]\036\206\035\320\241\037"
	.ascii	"\253\224\337\321\000\254\3348\351\r\b\321\335+q.b\342\325\375>\351\023\177\345\001\232\356\030\355\374s"
	.ascii	"2\230Y}\224U\200\314 U\3617\332VF\036 \223\005Nt\367\366\2313\317uj\274c5w"
	.ascii	"\263\234\023c\b\351\261\006\315>\240\305g\332\223\2442\211c\255\310\316w\215DO\206\033pkB\037"
	.ascii	"R%\241\221\3105~\361v\234^WS\201k\267>r\233\ro@\203\3728\344\247?\033\273v\013"
	.ascii	"\001\034\221AL&\311\357%,\242\027\270\267\243\361G\024\017\363k\332uX\220\2601\035'\365\032N"
	.ascii	"\233\223\222\177\371\301\270\bn\253D\324\313qg\276\027\200\273\231cd\345\"U\251r\267\036\326m{"
	.ascii	"\307\322\001\253\371\2530W\030;\024@\334v\373\026\201\262\313\240e\276l\206\376j\377\233e\233\372S"
	.ascii	"\222=\363P\350\301\255\267\317\325\214`O\372\230y\333[\374\215\275-\226\255O/\035\257\316\233>p"
	.ascii	"UT\210\224\351\310\024l\345\324\256ef]:\204\361Z\326\274>\267\033\030P\037\306\304\345\223\2159"
	.ascii	"\362\343\347\322`|\207\303\261\213\2020\240\2524;8\361\236s\347&>(w\005\303\002\220\234\234i"
	.ascii	"\363H\3423g\321K\034_\n\277\025\207\022\236\275v\003\013\241\360\214?\324\023\033\031\337]\233\260S"
	.ascii	"\314\361FY#\247\006\363}\331\345\314\265\030\027\222u\351\264\201G\322\315(\007\331\315o\f\363\312Q"
	.ascii	"\307T\254\030\232\371zs\017\263\034\305\334x3\220\307\f\341L3\274\211+\232\351\370\211\301)\256\022"
	.ascii	"\n\340tvB\247\013\246\363{z\241p\205\016c\314$3\317=VX7\252\375\203#)\252\004U"
	.ascii	"\317\001\r\037\313\300\236\251\256\3674:\314\357\321\r\"N\234\320!u\312U\352\245\353X\351O\321_"
	.ascii	"\216\313\223\277^\376B<_V\3246Q\250\337\276\350 B\210\236\205\360\340(\321%\007\226?\327}"
	.ascii	",\253E(\337-\334\265\223\351\177\n\261\221\224\006F\343\002@\326\363\252M\321tdXn\362?\t"
	.ascii	")\230\005h\376$\r\261\345#\257\333r\006su)\254W\264:%g\023\244p\264\206\274\274Y/"
	.ascii	"\001\303\221\266`\325Ap\036\347\327\255?\033 \205\205U3\021c\341\302\026\261(\b\001=^\245*"
	.ascii	"_\023\027\231B}\204\203\327\003}V\037\221\033\255\321\252w\276\331Hw~J\257Q..\264XT"
	.ascii	"OD\007\f\346\222Q\355\020\035Bt-N\305Bd\310\265\375\202L+5d\206v\212J\000\351\023"
	.ascii	"\177\207;\031\311\000.\273kP\334\340\220\250\343\354\237d\3366\300\267\363\354\032\236\336\230\b\004F_"
	.ascii	"\333\316/\203E\210\235sc\370k\256\311\3268\372\367\376O\267\312\r\2742^\344\274\024\210~\223s"
	.ascii	"\215\364{)\026q\003\2714h\360\324\";\321\251\306\275\226FW\025\227\3415\350\325\221\350\244\370,"
	.ascii	"\242k\320\027~H\265,k\031P9\0348\322$0\212\227\205\201\234e\327\366\244\326\221(\177oz"
	.ascii	"g\017\021\007\207\375\223mI\2658|\323\tL\335\206js\302Lj\261|\t*%Xn\275I "
	.ascii	"I\357\232j\215\375\t}\013\271=[\276`\356\360\324\277\236Q,\265!L\035\224E\305\337\252\021`"
	.ascii	"\220\370\313\002\310\320\336c\252j\377\r\312\230\320\373\231\355\266\271\375\nMb\036\0134y\267\030\316i"
	.ascii	"<\370\225\317m\222g_q\220(qa\205~|[z\217\231\363\347\241\326\340\371b\013\033\314\305o"
	.ascii	"\313y\230\262(U\357\321\222\220~\324<\256\032\335R#\237\030B\004~\022\361\001q\345:kY\025"
	.ascii	"\312$Q~\0261\377\t\337E\307\331\213\025\344\013\345V\365~\"}+)8\321\266\257A\342\244:"
	.ascii	"\242y\221?\3229'F\317\335\326\2271\022\203\377\212\024\362S\265\336\007\023\332M_{h7\"\r"
	.ascii	"\365\0053*\2778\301,\303&\351\242\217?XH\353\322IU\242\261:\bl\243\207Fn\252\3742"
	.ascii	"\337\314\207's\244\0072\370\343\023\362\b\031\343\027N\226\r\366\327\354\262\325\351\013`\3026cot"
	.ascii	"\365\232}\305\215n\305{\362\275\360\235\355\322\013>\243\344\357\"\336\024\300\252\\j\275\376\316\351'F"
	.ascii	"\034\227l\253E\363J?\037sC\231r\353\210\342m\030D\003\212jY3\223b\326~\000\027I{"
	.ascii	"\335\242S\335(\0334T?\374B\337[\220\027\252\364\370\322M\331\222\365\017}\323\214\340\017b\003\035"
	.ascii	"d\260\204\253\\\373\205-\024\274\363\211\322\020xI\f\316\025{D\334jG{\375D\370v\243+\022"
	.ascii	"T\345\264\242\3152\002\302\177\030]\021B\375\320\236\331y\324}\276\264\253.L\354h+\365\013\307\002"
	.ascii	"\341r\215E\2772\345\254\265<\267|\340h\347[\347\275\213\356\224}\317V\003:\264\376\343\227\006k"
	.ascii	"\273/\013]K\354\207\242\312\202H\007\220W\\A\\\201\320\301\036\246D\340\340\365\236@\nO3&"
	.ascii	"\300\243b\337J\360\310\266]\244m\007\357\000\360>\251\322\360IX\271\234\234\256/\033DC\177\303\034"
	.ascii	"\271\256\316\311\361Vf\327je\345\030\370\025[\0344#L\2042(\347&8h\031/wo4:"
	.ascii	"O2\307\\ZV\217P\"\251\006\345\300\304a\320\031\254E\\\333\253\030\373J1\200\003\301\thl"
	.ascii	"\310j\332\342\022Q\325\322\355Q\350\2611\003\275\351br\306\216\335F\007\226\320\305\367n\237\033\221\005"
	.ascii	"\357\352.Q\363\254ISI\313\301\034\323A\301 \215h\232\251\007\f\030$\027-K\306\321\371^U"
	.ascii	"\273\016\337\365\203\2313\301\254L,Q\217u\363\300\341\230\263\013\n\023\361,b\f'\252\371\354<k"
	.ascii	"\b\275s;\272p\2476\f\277\257\243\b\357Jb\362F\t\264\230\3777W\235t\2013\341M_g"
	.ascii	"\035\263\332;\331\366/\241\376-e\235\017\330%\007\207\224\276\232\363O\234\001C<\315\202\270P\364`"
	.ascii	"\374\202\027k\003R,\016\264\203\255l\201l\201d>\007di\331\275\334\320 \305d\001\367\235\331\023"
	.ascii	"\312\300\345!\303^K\001\242\277\031\327\311i\313O\240#\000u\030\034_N\200\254\355U\236\336\006\034"
	.ascii	"\252im\377@+\325\377\273I@\334\030\013S4\227\230M\243/\\J^-\2722}\216o\tx"
	.ascii	"\342\304>\243\326z\017\231\216\340.\2768\371\bf\025E(c\305C\241\234\r\266-\354\037\212\363L"
	.ascii	"\347\\\372\re\252\252\240\214G\265H*\236\304\371[r\003p}\314\tO\276\032\t&:\255<7"
	.ascii	"\255\273\335\211\373\250\276\361\313\256\256a\274,\313;\235\215\233\037\273\247X\217\206\246\022Q\332~T!"
	.asciz	"|\365\311\202Mc\224\2626E\223$\341\375\313\037Z\333\214A\263M\234\236\374\031DE\331\363@"
	.ascii	"\323\206Y\3759\351\375\336\f8\nQ\211,'\364\271\0311\273\007\244+\267\364M%J3\nUc"
	.ascii	"I{TrEX\272\233\340\b\304\342\372\306\005\363\215\3614\307i\372\350`zv}\252\257+\2519"
	.ascii	"7\317i\265\355\326\007e\341.\245\f\260)\204\027]\326k\353\220\000|\352Q\217\367\332\307b\352>"
	.ascii	"N'\223\346\023\307$\235u\323\333hw\205c_\232\263\212\353`URp\315\304\311e\006jCh"
	.ascii	"|\020 \350\027\323V\036e\351\n\204Dh&\305z\374\0172\306\241\340\301r\024a\221\234fsS"
	.ascii	"'?/ \3505\002\274\260u\371d\342\000\\\307\026$\214\243\325\351\244\221\371\211\267\212\366\347\266\027"
	.ascii	"WR\016\232\253\024(]\374\263\312\311\204 \217\220\312\036-[\210\365\312\257\021}\370x\246\265\264\034"
	.ascii	"\347\007\240\242b\252tk\261\307q\360\260\340\021\363#\342\013\0008\344\007W\254n\357\202-\375\300-"
	.ascii	"l\374J9k\300d\266\261_\332\230$\336\210\f4\330\312K\026\003\215O\2424t\336x\312\0133"
	.ascii	"Nt\031\021\204\377.\230$G\007+\226^i\371\373S\311\277O\301\212\305\365\034\2376\033\2761<"
	.ascii	"rB\313\371\223\274h\301\230\333\316\307\037q\270\256z\215\2544\252R\016\177\273U}~\t\301\316A"
	.ascii	"\356\212\224\bM\206\364\260o\034\272\221\356\031\334\007X\241\254\246\256\315uy\273\324bB\023a\0133"
	.ascii	"\212\200m\242\327\031\226\367m\025\236\035\236\324\037\273'\337\241\333l\303\327s}w(\037\331L\264&"
	.ascii	"\203\003sb\223\362\267\341,\212\312\353\377yRK\024\023\324\277\212w\374\332\017ar\234\024\020\353}"
	.ascii	"ut8\217GH\360Q<\313\276\234\364\274]\262U \237\331D\022\253\232\326\245\020\034l\236p,"
	.ascii	"z\356f\207j\257b\313\016\315SU\004\354\313f\265\344\013\0178\001\200X\352\342,\366\237\216\346\b"
	.ascii	"\371\362\270\n\325\t-/\337#Y\305\215!\271\254\271lvs&4\217J\365\031\3678\327;\261L"
	.ascii	"\2550\301K\nP\2554\234\324\013=I\3338\215\276\211\nP\230=\\\242\t;\272\356\207?\037/"
	.ascii	"J\266\025\345u\214\204\3678\220J\333\272\001\225\245P\033u??1\r\302\350.\256\300S\343\241\031"
	.ascii	"\275\275\226\325\315r!\264@\374\356\230CE\340\223\265\tA\264GS\261\2374\256f\002\231\323ks"
	.ascii	"\303\005\372\272`u\034}a^\345\306\240\240\341\263sd\326\300\030\227R\343\2064\f\302\021kTA"
	.ascii	"\264\2634\223P-S\205se\201`K\021\375Fu\203\\B0__\314\\\253\177\270\242\225\"A"
	.ascii	"\306\352\223\342aRe.\333\2543!\003\222Z\204k\231\000y\313u\tF\200\335Z\031\215\273`\007"
	.ascii	"\351\326~\365\210\233\311\031%\310\370m&\313\223Ss\322\n\263\0232\356\\4.-\265\353S\341\024"
	.ascii	"\212\201\346\315\027\032>A\204\240i\355\251m\025W\261\314\312F\217&\277,\362\305:\303\233\2764k"
	.ascii	"\323\362qeei\374\021zs\016SE\350\311\3065P\376\324\242\347:\343\013\323m.\266\307\271\001"
	.ascii	"\262\300x:d/\337\363|\002.\362\036\227>L\243\265\301I^\034}\354-\335\"\t\217\301\022 "
	.ascii	")\235\310Z\345U\013\210c\247\240E\037$\203\024\037l\347\302\337\3576=\350\255KNx[\257\b"
	.ascii	"K,\314\211\322\024s\342\215\027\207\242\021\275\344K\316d3\372\326(\325\030n\202\331\257\325\301#d"
	.ascii	"3%\037\210\334\2314(\266#\223w\332%\005\235\364A4g\373\335z\211\215\026:\026q\235\2672"
	.ascii	"j\263\374\355\331\370\205\314\371\345F7\217\302\274\"\315\323\345\3718\343\235\344\314->\301\373^\nH"
	.ascii	"\037\"\316B\344La\266(9\005L\314\235\031n\003\276\034\334\244\264?f\006\216\034iG\035\263$"
	.ascii	"q b\001\013\347Q\013\305\257\035\213\317\005\265\006\315\253Z\357a\260k,1\277\267\f`'\252G"
	.ascii	"\303\370\025\300\355\036T*|?i|~\376\244\021\326x\242N\023f\257\360\224\240\335\024]X[T"
	.ascii	"\341!\263\343\320\344\004b\225\036\377(zc\252;\236\275\231[\375\317\f\013q\320\310d>\334\"M"
	.ascii	"\017:\324\240^'\277g\276\356\233\b4\216\346\255.\347y\324L\023\211BTT\2722\303\371b\017"
	.ascii	"9_;\326\211e\264\374a\317\313W?j\256\\\005\372:\225\322\302\272\3766\02476\032\240\017\034"
	.ascii	"Pj\223\214\016+\bi\266\305\332\3015\240\311\3714\266\337\304T>\267o@\301+\035\233A\005@"
	.ascii	"\377=\224\"\266\004\306\322\240\263\317D\316\276\214\274x\206\200\227\363O%]\277\246\034;Oa\243\017"
	.ascii	"\360\202\276\271\275\376\003\240\220\254D:\257\301\211 \216\372T\031\221\237I\370B\253@\357\212!\272\037"
	.ascii	"\224\001{>\004W>O\177\257\332\b\356>\035\250\361\336\334\231\253\3069\310\325aw\377\023]Sl"
	.ascii	">\365\310\372H\224T\253A7\246{\232\350\366\201\001^+l}l\375tBn\310\250\312:.9"
	.ascii	"\2575\212>\3514\275L\026\350\207XD\201\007.\253\260\232\362v\2341\031;\301\n\325\344\177\341%"
	.ascii	"\247!\361v\365\177_\221\343\207\315/'2J\303&\345\033M\336/\272\314\233\211i\211\217\202\272k"
	.ascii	"v\366\004\036\327\233(\n\225\017B\326R\034\216 \253\037i4\260\330\206QQ\263\237*DQW%"
	.ascii	"\0019\376\220f\274\321\342\325z\231\240\030J\265L\324`\204\257\024i\035\227\344{k\177OP\235U"
	.ascii	"\375f\322\366\347\221H\234\033x\007\003\233\241D\007;\342a`\035\2178\210\016\325K5\243\246>\022"
	.ascii	"\325T\353\263x\203s\247|<U\245f\323i\035\272\000(\371b\317&\n\0272~\200\325\022\253\001"
	.ascii	"\226-\343A\220\030\215\021HX1\330\302\343\355\271\331E2\330qB\253\036T\241\030\311\342a9J"
	.ascii	"\036?#\363D\326'\003\026\360\3744\016&\232Iy\271\332\362\026\247\265\203\037\021\324\233\255\356\254h"
	.ascii	"\240\273\346\370\340;\334q\n\343\377~4\370\316\326jG:\341_B\222\251c\267\035\373\343\274\326,"
	.ascii	"\020\302\327\363\016\311\2648\f\004\255\267$n\2160#>\347\267\361\331`8\227\365\b\265\325`WY"
	.ascii	"\220'\002\375\353\313*\210`W\021\304\0053\257\211\364s4}\343\222\364e+ZQT\337\305\262,"
	.ascii	"\227c\252\004\341\277)a\313\374\247\244\b\000\226\217X\224\220}\211\300\213?\251\221\262\334>\244\237p"
	.ascii	"\312*\375c\214]\n\353\377Ni.f\301+\322:\260\313\370n\363#'\037\023\310\360\354)\360p"
	.ascii	"\271\260\020^\252\257j*\251\032\004\357p\243\360x\037\326:\252w\373>w\341\331K\247\242\245\354D"
	.ascii	"3>\355.\263\007\023F\347\201U\2443/\004\256f\003_\031\323ID\311XH1l\212]}\013"
	.ascii	"C\325\225{2H\324%\035\0174\243\000\203\323p+\305\341`\034S\034\336\344\351},Q$\"'"
	.ascii	"\374u\251B\212\273{\277X\243\255\226w9\\\214H\252\355\315o\307\177\342\246 \274\366\327_s\031"
	.ascii	".4\305I\257\222\274\032\320\372\346\262\021\330\356\377)N\310\374\215\214\242\357C\305L\244\030\337\265\021"
	.ascii	"fB\310B\320\220\253\343~T\031\177\017\216\204\353\271\227\244e\320\241\003%_\211\337\221\021\221\357\017"
	.size	k25519Precomp, 24576

	.type	message,@object                 # @message
	.section	secret,"aw",@progbits
	.globl	message
	.p2align	4, 0x90
message:
	.ascii	"\006\343\024\201\017\353`\233\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703\364\356\330|\005\030\375\351x+`\255D\326`\027\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253\361\276\342\0253)\360\206^\002c\252\005\264\364\341^\002c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347H<\b\035\267\370\255\235\253"
	.zero	128
	.size	message, 256

	.type	signature,@object               # @signature
	.globl	signature
	.p2align	4, 0x90
signature:
	.zero	64
	.size	signature, 64

	.type	public_key,@object              # @public_key
	.section	public,"aw",@progbits
	.globl	public_key
	.p2align	4, 0x90
public_key:
	.ascii	"\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253"
	.size	public_key, 32

	.type	private_key,@object             # @private_key
	.section	secret,"aw",@progbits
	.globl	private_key
	.p2align	4, 0x90
private_key:
	.ascii	"\006\343\024\201\017\353`\233\361\276\342\0253)\360\206^\002c\252\005\264\364\341x+`\255D\326`\027\331\021\372X\376m\347H\364\356\330|\005\030\375\351\211\022\240\023\340\353\2703<\b\035\267\370\255\235\253"
	.size	private_key, 64

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
	.addrsig_sym signature
	.addrsig_sym private_key
