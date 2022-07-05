global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	call main
	jmp _end
syscall3: 
	jmp syscall
syscall: 
	syscall
	ret
str2addr: 
	jmp str_to_ptr
str_to_ptr: 
	add rax, 8
	ret

strlen: 
	jmp dereference
dereference: 
	mov rax, [rax]
	ret

int2char: 
	jmp convert
convert: 
	ret

write:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 1
	push rax
	mov rax, 1
	push rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call str2addr
	push rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call strlen
	push rax
	pop rdx
	pop rsi
	pop rdi
	pop rax
	call syscall3
	leave
	ret
main:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, str_hello__world__n
	push rax
	pop rax
	call write
	mov rax, 69
	leave
	ret
_end:
	mov rdi, rax
	mov rax, 60
	syscall
section .data
str_hello__world__n:
	dq 15
	db `Hello, World!\n`
