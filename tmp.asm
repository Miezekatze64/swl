global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	call main
	jmp _end
syscall1: 
	jmp intrinsic_syscall
intrinsic_syscall: 
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
syscall3: 
	jmp intrinsic_syscall
arr2addr: 
	jmp intrinsic_str_to_ptr
intrinsic_str_to_ptr: 
	add rax, 8
	ret

arrlen: 
	jmp intrinsic_dereference
intrinsic_dereference: 
	mov rax, [rax]
	ret

arr2ptr: 
	jmp intrinsic_convert
intrinsic_convert: 
	ret

ptr2arr: 
	jmp intrinsic_convert
ptr2str: 
	jmp intrinsic_convert
set_ptr: 
	jmp intrinsic_set_ptr
intrinsic_set_ptr: 
	mov [rax], rbx
	ret

brk:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 12
	push rax
	mov rbx, [rbp-8]
	push rbx
	pop rbx
	pop rax
	call syscall1
	leave
	ret
sbrk:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 0
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, 0
	sub rsp, 32
	mov qword [rbp-32], rax
	mov rax, [rbp-24]
	push rax
	mov rbx, 0
	pop rax
	cmp al, bl
	sete al
	mov al, al
	cmp rax, 1
	jne .l2_16
.l1_16:
	mov rax, 0
	push rax
	pop rax
	call brk
	sub rsp, 32
	mov qword [rbp-32], rax
	mov rax, [rbp-32]
	sub rsp, 24
	mov qword [rbp-24], rax
.l2_16:
	mov rax, [rbp-8]
	push rax
	mov rbx, 0
	pop rax
	cmp al, bl
	sete al
	mov al, al
	cmp rax, 1
	jne .l2_17
.l1_17:
	mov rax, [rbp-32]
	leave
	ret
.l2_17:
	mov rax, [rbp-32]
	push rax
	mov rbx, [rbp-8]
	pop rax
	add rax, rbx
	sub rsp, 16
	mov qword [rbp-16], rax
	mov rax, [rbp-16]
	push rax
	pop rax
	call brk
	push rax
	mov rbx, [rbp-32]
	pop rax
	cmp al, bl
	sete al
	mov al, al
	cmp rax, 1
	jne .l2_19
.l1_19:
	mov rax, -1
	leave
	ret
.l2_19:
	mov rax, [rbp-32]
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, [rbp-16]
	sub rsp, 32
	mov qword [rbp-32], rax
	mov rax, [rbp-24]
	leave
	ret
_malloc:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, [global_blocks]
	push rax
	pop rax
	call arr2ptr
	push rax
	mov rbx, 0
	pop rax
	cmp al, bl
	sete al
	mov al, al
	cmp rax, 1
	jne .l2_16
.l1_16:
	mov rax, 100
	push rax
	mov rbx, 8
	pop rax
	imul rax, rbx
	push rax
	pop rax
	call sbrk
	push rax
	pop rax
	call ptr2arr
	mov qword [global_blocks], rax
.l2_16:
	mov rax, [rbp-8]
	push rax
	pop rax
	call sbrk
	sub rsp, 16
	mov qword [rbp-16], rax
	mov rax, [rbp-16]
	leave
	ret
ptr_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 16
	mov [rbp-16], rbx
	mov rax, [rbp-8]
	push rax
	mov rax, [rbp-16]
	push rax
	pop rbx
	pop rax
	call set_ptr
	mov rax, [rbp-8]
	push rax
	pop rax
	call ptr2arr
	leave
	ret
strcpy:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 16
	mov [rbp-16], rbx
	mov rax, [rbp-8]
	push rax
	mov rax, [rbp-16]
	push rax
	mov rax, 0
	push rax
	pop rcx
	pop rbx
	pop rax
	call _strcpy
	leave
	ret
_strcpy:
	push rbp
	mov rbp,rsp
	sub rsp, 24
	mov [rbp-24], rax
	sub rsp, 16
	mov [rbp-16], rbx
	sub rsp, 8
	mov [rbp-8], rcx
	mov rax, [rbp-24]
	push rax
	mov rax, [rbp-16]
	push rax
	pop rax
	call arrlen
	mov rbx, rax
	pop rax
	cmp al, bl
	sete al
	mov al, al
	cmp rax, 1
	jne .l2_19
.l1_19:
	leave
	ret
.l2_19:
	mov rax, [rbp-24]
	mov rbx, [rbp-16]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	mov rbx, [rbp-24]
	push rax
	mov rax, [rbp-8]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, [rbp-8]
	push rax
	mov rax, [rbp-16]
	push rax
	mov rax, [rbp-24]
	push rax
	mov rbx, 1
	pop rax
	add rax, rbx
	push rax
	pop rcx
	pop rbx
	pop rax
	call _strcpy
	leave
	ret
bool_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], rax
	mov rax, [rbp-1]
	cmp rax, 1
	jne .l2_20
.l1_20:
	mov rax, str_true
	leave
	ret
.l2_20:
	mov rax, str_false
	leave
	ret
exit:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 60
	push rax
	mov rax, [rbp-8]
	push rax
	pop rbx
	pop rax
	call syscall1
	leave
	ret
print:
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
	call arr2addr
	push rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call arrlen
	push rax
	pop rdx
	pop rcx
	pop rbx
	pop rax
	call syscall3
	leave
	ret
read:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 0
	push rax
	mov rax, 0
	push rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call arr2addr
	push rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call arrlen
	push rax
	pop rdx
	pop rcx
	pop rbx
	pop rax
	call syscall3
	leave
	ret
main:
	push rbp
	mov rbp,rsp
	mov rax, 0
	leave
	ret
_end:
	mov rdi, rax
	mov rax, 60
	syscall
section .data
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
global_blocks:
	resb 8
global_is_block:
	resb 8
