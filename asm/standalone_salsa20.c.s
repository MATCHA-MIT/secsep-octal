	.file	"standalone_salsa20.c"
	.text
	.p2align 4
	.globl	salsa20_words
	.type	salsa20_words, @function
salsa20_words:
.LFB2:
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
	movl	8(%rsi), %eax
	movl	(%rsi), %r8d
	movl	16(%rsi), %ebp
	movl	44(%rcx), %edx
	movl	4(%rsi), %r15d
	movl	20(%rsi), %r13d
	movl	36(%rcx), %r11d
	movl	%eax, -56(%rsp)
	movl	%r8d, %r14d
	movl	12(%rsi), %eax
	movl	56(%rcx), %ebx
	movl	%edx, -44(%rsp)
	movl	%r8d, -20(%rsp)
	movl	48(%rcx), %edx
	movl	%eax, -52(%rsp)
	movl	28(%rcx), %eax
	movl	24(%rsi), %esi
	movl	32(%rcx), %r12d
	movq	%rdi, -16(%rsp)
	movl	%edx, %r10d
	movl	%eax, -48(%rsp)
	movl	40(%rcx), %eax
	movl	%ebx, %edx
	movl	%r11d, %edi
	movl	$10, -24(%rsp)
	movl	52(%rcx), %r9d
	movl	%eax, -36(%rsp)
	movl	60(%rcx), %eax
	movl	%ebp, -40(%rsp)
	movl	-36(%rsp), %r8d
	movq	%rcx, -8(%rsp)
	.p2align 4,,10
	.p2align 3
.L2:
	movl	-40(%rsp), %ebx
	leal	(%r14,%r10), %ebp
	leal	0(%r13,%r15), %r11d
	roll	$7, %ebp
	roll	$7, %r11d
	xorl	%ebx, %ebp
	xorl	%edi, %r11d
	leal	0(%rbp,%r14), %ecx
	roll	$9, %ecx
	xorl	%r12d, %ecx
	movl	-48(%rsp), %r12d
	movl	%ecx, %ebx
	leal	0(%rbp,%rcx), %ecx
	roll	$13, %ecx
	movl	%ebx, -36(%rsp)
	xorl	%r10d, %ecx
	leal	(%rbx,%rcx), %r10d
	movl	%ecx, -32(%rsp)
	leal	(%r11,%r13), %ecx
	roll	$9, %ecx
	leal	(%r8,%rsi), %ebx
	rorl	$14, %r10d
	xorl	%r9d, %ecx
	xorl	%r14d, %r10d
	roll	$7, %ebx
	leal	(%r11,%rcx), %r14d
	xorl	%edx, %ebx
	movl	%ecx, %edi
	movl	%ecx, -28(%rsp)
	roll	$13, %r14d
	leal	(%rbx,%r8), %r9d
	xorl	%r15d, %r14d
	movl	-56(%rsp), %r15d
	roll	$9, %r9d
	addl	%r14d, %edi
	xorl	%r15d, %r9d
	rorl	$14, %edi
	movl	-44(%rsp), %r15d
	xorl	%r13d, %edi
	leal	(%rbx,%r9), %r13d
	roll	$13, %r13d
	xorl	%esi, %r13d
	leal	(%r15,%rax), %esi
	leal	(%r9,%r13), %edx
	roll	$7, %esi
	rorl	$14, %edx
	xorl	%r8d, %edx
	movl	-52(%rsp), %r8d
	xorl	%r8d, %esi
	leal	(%rsi,%rax), %r8d
	roll	$9, %r8d
	xorl	%r12d, %r8d
	leal	(%rsi,%r8), %r12d
	roll	$13, %r12d
	xorl	%r15d, %r12d
	leal	(%r10,%rsi), %r15d
	leal	(%r8,%r12), %ecx
	rorl	$14, %ecx
	xorl	%eax, %ecx
	roll	$7, %r15d
	xorl	%r14d, %r15d
	leal	(%r10,%r15), %eax
	roll	$9, %eax
	movl	%eax, %r14d
	xorl	%r9d, %r14d
	leal	(%r15,%r14), %eax
	movl	%r14d, -56(%rsp)
	roll	$13, %eax
	movl	%eax, %r9d
	xorl	%esi, %r9d
	leal	0(%rbp,%rdi), %esi
	roll	$7, %esi
	addl	%r9d, %r14d
	movl	%r9d, -52(%rsp)
	xorl	%r13d, %esi
	rorl	$14, %r14d
	leal	(%rdi,%rsi), %eax
	xorl	%r10d, %r14d
	roll	$9, %eax
	movl	%eax, %r9d
	leal	(%r11,%rdx), %eax
	xorl	%r8d, %r9d
	roll	$7, %eax
	leal	(%rsi,%r9), %r8d
	movl	%r9d, -48(%rsp)
	roll	$13, %r8d
	movl	%r8d, %r10d
	movl	%eax, %r8d
	movl	-36(%rsp), %eax
	xorl	%r12d, %r8d
	xorl	%ebp, %r10d
	leal	(%rdx,%r8), %r12d
	leal	(%r9,%r10), %r13d
	movl	%r8d, -44(%rsp)
	roll	$9, %r12d
	rorl	$14, %r13d
	movl	%r10d, -40(%rsp)
	leal	(%rbx,%rcx), %r10d
	xorl	%eax, %r12d
	xorl	%edi, %r13d
	roll	$7, %r10d
	leal	(%r8,%r12), %edi
	roll	$13, %edi
	xorl	%r11d, %edi
	leal	(%r12,%rdi), %eax
	rorl	$14, %eax
	movl	%eax, %r8d
	movl	-28(%rsp), %eax
	xorl	%edx, %r8d
	movl	-32(%rsp), %edx
	xorl	%edx, %r10d
	leal	(%rcx,%r10), %r9d
	roll	$9, %r9d
	xorl	%eax, %r9d
	leal	(%r10,%r9), %edx
	roll	$13, %edx
	xorl	%ebx, %edx
	leal	(%r9,%rdx), %eax
	rorl	$14, %eax
	xorl	%ecx, %eax
	subl	$1, -24(%rsp)
	jne	.L2
	movl	%r8d, -36(%rsp)
	movl	-20(%rsp), %r8d
	movl	%edi, %r11d
	movl	%edx, %ebx
	movq	-16(%rsp), %rdi
	movq	-8(%rsp), %rcx
	movl	%r10d, %edx
	movl	%eax, %r10d
	leal	(%r14,%r8), %eax
	movl	-40(%rsp), %ebp
	movl	%eax, (%rdi)
	addl	4(%rcx), %r15d
	movl	%r15d, 4(%rdi)
	movl	-56(%rsp), %eax
	addl	8(%rcx), %eax
	movl	%eax, 8(%rdi)
	movl	-52(%rsp), %eax
	addl	12(%rcx), %eax
	movl	%eax, 12(%rdi)
	addl	16(%rcx), %ebp
	movl	%ebp, 16(%rdi)
	addl	20(%rcx), %r13d
	movl	%r13d, 20(%rdi)
	addl	24(%rcx), %esi
	movl	-48(%rsp), %eax
	movl	%esi, 24(%rdi)
	addl	28(%rcx), %eax
	movl	%eax, 28(%rdi)
	addl	32(%rcx), %r12d
	movl	%r12d, 32(%rdi)
	addl	36(%rcx), %r11d
	movl	-36(%rsp), %eax
	movl	%r11d, 36(%rdi)
	addl	40(%rcx), %eax
	movl	%eax, 40(%rdi)
	movl	-44(%rsp), %eax
	addl	44(%rcx), %eax
	movl	%eax, 44(%rdi)
	addl	48(%rcx), %edx
	movl	%edx, 48(%rdi)
	addl	52(%rcx), %r9d
	movl	%r9d, 52(%rdi)
	addl	56(%rcx), %ebx
	movl	%ebx, 56(%rdi)
	movl	60(%rcx), %r15d
	addl	%r10d, %r15d
	movl	%r15d, 60(%rdi)
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
.LFE2:
	.size	salsa20_words, .-salsa20_words
	.p2align 4
	.globl	salsa20_block
	.type	salsa20_block, @function
salsa20_block:
.LFB3:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movq	%rdi, %rbx
	addq	$-128, %rsp
	.cfi_def_cfa_offset 144
	movq	(%rsi), %rax
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
	call	salsa20_words
	xorl	%eax, %eax
	.p2align 4,,10
	.p2align 3
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
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE3:
	.size	salsa20_block, .-salsa20_block
	.p2align 4
	.globl	salsa20
	.type	salsa20, @function
salsa20:
.LFB4:
	.cfi_startproc
	testq	%rsi, %rsi
	je	.L19
	pushq	%r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	movq	%rsi, %r14
	pushq	%r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	movq	%rcx, %r13
	pushq	%r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	movq	%rdx, %r12
	pushq	%rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	xorl	%ebx, %ebx
	subq	$64, %rsp
	.cfi_def_cfa_offset 112
	.p2align 4,,10
	.p2align 3
.L14:
	movl	%ebx, %eax
	andl	$63, %eax
	je	.L22
	cltq
	movzbl	(%rsp,%rax), %eax
	xorb	%al, 0(%rbp,%rbx)
	addq	$1, %rbx
	cmpq	%rbx, %r14
	jne	.L14
	addq	$64, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L22:
	.cfi_restore_state
	movl	%ebx, %ecx
	movq	%r13, %rdx
	movq	%r12, %rsi
	movq	%rsp, %rdi
	sarl	$6, %ecx
	movslq	%ecx, %rcx
	call	salsa20_block
	movzbl	(%rsp), %eax
	xorb	%al, 0(%rbp,%rbx)
	addq	$1, %rbx
	cmpq	%rbx, %r14
	jne	.L14
	addq	$64, %rsp
	.cfi_def_cfa_offset 48
	popq	%rbx
	.cfi_def_cfa_offset 40
	popq	%rbp
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L19:
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	ret
	.cfi_endproc
.LFE4:
	.size	salsa20, .-salsa20
	.p2align 4
	.globl	_start
	.type	_start, @function
_start:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	xorl	%ecx, %ecx
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	movl	%ecx, %edx
	subq	$248, %rsp
	.cfi_def_cfa_offset 272
	movq	$1296236545, 32(%rsp)
	leaq	32(%rsp), %rax
	movq	$key, 40(%rsp)
	movq	$32, 48(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
#APP
# 107 "/root/benchmarks/src/salsa20/standalone_salsa20.c" 1
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
	movq	$nonce, 88(%rsp)
	movq	$8, 96(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 120(%rsp)
#APP
# 108 "/root/benchmarks/src/salsa20/standalone_salsa20.c" 1
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
	movq	$msg, 136(%rsp)
	movq	$64, 144(%rsp)
	movq	$0, 152(%rsp)
	movq	$0, 160(%rsp)
	movq	$0, 168(%rsp)
#APP
# 109 "/root/benchmarks/src/salsa20/standalone_salsa20.c" 1
	rolq $3,  %rdi ; rolq $13, %rdi
	rolq $61, %rdi ; rolq $51, %rdi
	xchgq %rbx,%rbx
# 0 "" 2
#NO_APP
	movq	nonce(%rip), %rbp
	movq	%rdx, 24(%rsp)
	movl	$50, %ebx
	movq	24(%rsp), %rax
	.p2align 4,,10
	.p2align 3
.L25:
	xorl	%ecx, %ecx
	movq	%rbp, %rdx
	movl	$key, %esi
	leaq	176(%rsp), %rdi
	call	salsa20_block
	movzbl	176(%rsp), %eax
	xorb	%al, msg(%rip)
	movl	$1, %eax
.L24:
	movzbl	msg(%rax), %edx
	xorb	176(%rsp,%rax), %dl
	addq	$1, %rax
	movb	%dl, msg-1(%rax)
	cmpq	$64, %rax
	jne	.L24
	subl	$1, %ebx
	jne	.L25
#APP
# 114 "/root/benchmarks/src/salsa20/standalone_salsa20.c" 1
	movl $60, %eax
xorl %edi, %edi
syscall
hlt

# 0 "" 2
#NO_APP
	addq	$248, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE5:
	.size	_start, .-_start
	.globl	msg
	.data
	.align 32
	.type	msg, @object
	.size	msg, 64
msg:
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.globl	nonce
	.bss
	.align 8
	.type	nonce, @object
	.size	nonce, 8
nonce:
	.zero	8
	.globl	key
	.data
	.align 32
	.type	key, @object
	.size	key, 32
key:
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.ascii	"\b\001\002\003\004\005\006\007\007\006\005\004\003\002\001\b"
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
