global _start
section .text
_start:
	push rbp
	mov rbp, rsp
	mov rax, 0
	mov qword [global__m_heap_start], rax
	mov rax, 0
	mov qword [global__m_location], rax
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

ptr2arr: 
	jmp intrinsic_convert
intrinsic_convert: 
	ret

__unsafe_convert: 
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
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_15
.l1_15:
	mov rax, 0
	push rax
	pop rax
	call brk
	sub rsp, 32
	mov qword [rbp-32], rax
	mov rax, [rbp-32]
	sub rsp, 24
	mov qword [rbp-24], rax
.l2_15:
	mov rax, [rbp-8]
	push rax
	mov rbx, 0
	pop rax
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_16
.l1_16:
	mov rax, [rbp-32]
	leave
	ret
.l2_16:
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
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_18
.l1_18:
	mov rax, -1
	leave
	ret
.l2_18:
	mov rax, [rbp-32]
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, [rbp-16]
	sub rsp, 32
	mov qword [rbp-32], rax
	mov rax, [rbp-24]
	leave
	ret
	mov rax, 0
	mov qword [global__m_heap_start], rax
	mov rax, 0
	mov qword [global__m_location], rax
_malloc:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, [global__m_heap_start]
	push rax
	mov rbx, 0
	pop rax
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_18
.l1_18:
	mov rax, 0
	push rax
	pop rax
	call sbrk
	mov qword [global__m_heap_start], rax
	mov rax, 1000
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
	mov qword [global__m_ptrs], rax
	mov rax, 1000
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
	mov qword [global__m_size], rax
	mov rax, 1000
	push rax
	pop rax
	call sbrk
	push rax
	pop rax
	call ptr2arr
	mov qword [global__m_is_block], rax
.l2_18:
	mov rax, [rbp-8]
	push rax
	pop rax
	call sbrk
	sub rsp, 16
	mov qword [rbp-16], rax
	mov rax, [rbp-16]
	push rax
	mov rbx, [rbp-8]
	pop rax
	add rax, rbx
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, 0
	sub rsp, 32
	mov qword [rbp-32], rax
.while_22:
	mov rax, [rbp-32]
	push rax
	mov rbx, [global__m_location]
	pop rax
	cmp rax, rbx
	setl al
	mov al, al
	cmp rax, 1
	jne .while_22_end
	mov rax, [rbp-32]
	mov rbx, [global__m_is_block]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	push rax
	test al, al
	setz al
	cmp al, 1
	jne .l2_24
.l1_24:
	mov rax, str_found_hole_n
	push rax
	pop rax
	call print
	mov rax, 1
	mov rbx, [rbp-32]
	push rax
	mov rax, [global__m_is_block]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, [rbp-32]
	mov rbx, [global__m_ptrs]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	leave
	ret
.l2_24:
	mov rax, 1
	mov rbx, [rbp-32]
	add rax, rbx
	sub rsp, 32
	mov qword [rbp-32], rax
	jmp .while_22
.while_22_end:
	mov rax, [rbp-16]
	mov rbx, [global__m_location]
	push rax
	mov rax, [global__m_ptrs]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, [rbp-8]
	mov rbx, [global__m_location]
	push rax
	mov rax, [global__m_size]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, 1
	mov rbx, [global__m_location]
	push rax
	mov rax, [global__m_is_block]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
	mov rax, 1
	mov rbx, [global__m_location]
	add rax, rbx
	mov qword [global__m_location], rax
	mov rax, [rbp-16]
	leave
	ret
_free:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, 0
	sub rsp, 16
	mov qword [rbp-16], rax
.while_20:
	mov rax, [rbp-16]
	push rax
	mov rbx, [global__m_location]
	pop rax
	cmp rax, rbx
	setl al
	mov al, al
	cmp rax, 1
	jne .while_20_end
	mov rax, str_searching____n
	push rax
	pop rax
	call print
	mov rax, [rbp-16]
	mov rbx, [global__m_ptrs]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	push rax
	pop rax
	call exit
	mov rax, [rbp-8]
	push rax
	mov rbx, [rbp-16]
	mov rcx, [global__m_ptrs]
	add rcx, 8
	add rcx, rbx
	mov rbx, [rcx]
	pop rax
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_24
.l1_24:
	mov rax, str_freed_n
	push rax
	pop rax
	call print
	mov rax, 0
	mov rbx, [rbp-16]
	push rax
	mov rax, [global__m_is_block]
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
.l2_24:
	mov rax, 1
	mov rbx, [rbp-16]
	add rax, rbx
	sub rsp, 16
	mov qword [rbp-16], rax
	jmp .while_20
.while_20_end:
	leave
	ret
malloc:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call _malloc
	sub rsp, 16
	mov qword [rbp-16], rax
	mov rax, [rbp-16]
	push rax
	mov rax, [rbp-8]
	push rax
	pop rbx
	pop rax
	call set_ptr
	mov rax, [rbp-16]
	push rax
	pop rax
	call ptr2arr
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, [rbp-24]
	leave
	ret
free:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call __unsafe_convert
	push rax
	pop rax
	call _free
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
	cmp rax, rbx
	sete al
	mov al, al
	cmp al, 1
	jne .l2_24
.l1_24:
	leave
	ret
.l2_24:
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
bool_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], al
	mov rax, [rbp-1]
	cmp al, 1
	jne .l2_28
.l1_28:
	mov rax, str_true
	leave
	ret
.l2_28:
	mov rax, str_false
	leave
	ret
main:
	push rbp
	mov rbp,rsp
	mov rax, 10
	push rax
	pop rax
	call malloc
	sub rsp, 8
	mov qword [rbp-8], rax
	mov rax, 10
	push rax
	pop rax
	call malloc
	sub rsp, 16
	mov qword [rbp-16], rax
	mov rax, [rbp-8]
	push rax
	pop rax
	call free
	mov rax, 10
	push rax
	pop rax
	call malloc
	sub rsp, 24
	mov qword [rbp-24], rax
	mov rax, [rbp-24]
	push rax
	pop rax
	call read
	mov rax, [rbp-24]
	push rax
	pop rax
	call print
	mov rax, 0
	mov rbx, [global__m_is_block]
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
	push rax
	pop rax
	call __unsafe_convert
	leave
	ret
_end:
	mov rdi, rax
	mov rax, 60
	syscall
section .data
str_found_hole_n:
	dq 11
	db `FOUND HOLE\n`
str_searching____n:
	dq 13
	db `searching...\n`
str_freed_n:
	dq 6
	db `freed\n`
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
global__m_location:
	resb 8
global__m_is_block:
	resb 8
global__m_ptrs:
	resb 8
global__m_heap_start:
	resb 8
global__m_size:
	resb 8
