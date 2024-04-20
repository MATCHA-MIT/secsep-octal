table_select:
.LFB82:
# Reg read: rdx: k1, rsi: k2
# Reg write: r9
# Reg unused:
movl %edx, %r9d
movslq %esi, %rsi
leaq (%rsi,%rsi,2), %rsi
salq $8, %rsi
addq $k25519Precomp, %rsi
movl $1, %edi
# rdx: k1, rdi: 1, rsi: k25519Precomp + k2 * 768, r9: k1
.L80:
# Reg read: r9: k3, rdi: k4
# Reg write: rax, rdx
# Reg unused: rsi: k5
movq %r9, %rax
xorq %rdi, %rax
movq %rax, %rdx
subq $1, %rax
notq %rdx
andq %rax, %rdx
xorl %eax, %eax
sarq $63, %rdx
# To L79:
# rax: 0, rdx: sth(0/1), rdi: k4, rsi: k5, r9: k3
.L79:
# Reg read: rax: k6, rdx: k7, rsi: k8, rdi: k9
# Reg write: rcx
# Reg unused:
movzbl (%rsi,%rax), %ecx
# Minimum req: k8=ptr, k6=idx or rsi=idx, rax=ptr
andl %edx, %ecx
xorb %cl, -88(%rsp,%rax)
# Minimum req: k6=idx
addq $1, %rax
cmpq $96, %rax
jne .L79
# To L79:
# rax: k6+1{96}, rcx: any/data, rdx: k7, rsi: k8, rdi: k9
# To next:
# rax: {96}, rcx: any/data, rdx: k7, rsi: k8, rdi: k9
addq $1, %rdi
movzbl -88(%rsp), %eax
addq $96, %rsi
cmpq $9, %rdi
jne .L80
# To L80:
# rax: any/data, rcx: any/data, rdi: k9+1{9}, rsi: k8+96,
# To next:
# rax: any/data, rcx: any/data, rdi: {9}, rsi: k8+96,
# ...
ret
