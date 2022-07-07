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
	push rax
	push rbx
	push rcx
	push rdx
	pop rdx
	pop rsi
	pop rdi
	pop rax
	syscall
	ret
arr2addr: 
	jmp str_to_ptr
str_to_ptr: 
	add rax, 8
	ret

arrlen: 
	jmp dereference
dereference: 
	mov rax, [rax]
	ret

char2int: 
	jmp convert
convert: 
	ret

bool2int: 
	jmp convert
deref: 
	jmp dereference
main:
	push rbp
	mov rbp,rsp
	mov rax, arr_0
	sub rsp, 8
	mov qword [rbp-8], rax
	mov rax, 98
	mov rbx, 0
	push rax
	mov rax, [rbp-8]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, 0
	mov rbx, [rbp-8]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	push rax
	mov rbx, 98
	pop rax
	cmp al, bl
	sete al
	mov al, al
	push rax
	pop rax
	call bool2int
	leave
	ret
_end:
	mov rdi, rax
	mov rax, 60
	syscall
section .data
arr_0:
	dq 3
	resb 3
