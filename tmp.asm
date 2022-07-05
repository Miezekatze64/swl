global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	call main
	jmp _end
main:
	push rbp
	mov rbp,rsp
	mov rdx, this_is_a_string
	mov [rbp-16], rdx
	mov rdx, 69
	mov rax, rdx
	pop rbp
	ret
_end:
	mov rdi, rax
	mov rax, 60
	syscall
section .bss
this_is_a_string:
	dq 16
	dd "this is a string"
