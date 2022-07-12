global _start
section .text
_start:
	push rbp
	mov rbp, rsp
;; FUNCTION CALL main
	call main
;; JUMP
	jmp _end
;; INTRINSIC syscall
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
;; INTRINSIC syscall
syscall3: 
	jmp intrinsic_syscall
;; INTRINSIC str_to_ptr
arr2addr: 
	jmp intrinsic_str_to_ptr
intrinsic_str_to_ptr: 
	add rax, 8
	ret

;; INTRINSIC dereference
arrlen: 
	jmp intrinsic_dereference
intrinsic_dereference: 
	mov rax, [rax]
	ret

;; INTRINSIC convert
ptr2arr: 
	jmp intrinsic_convert
intrinsic_convert: 
	ret

;; INTRINSIC convert
__unsafe_convert: 
	jmp intrinsic_convert
;; INTRINSIC set_ptr
set_ptr: 
	jmp intrinsic_set_ptr
intrinsic_set_ptr: 
	mov [rax], rbx
	ret

;; FUNCTION DECL brk
brk:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 12
	mov rax, 12
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; PUSH
	push rbx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL syscall1
	call syscall1
;; RETURN
	leave
	ret
;; FUNCTION DECL sbrk
sbrk:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 15 START
	cmp al, 1
	jne .l2_15
.l1_15:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL brk
	call brk
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; IF 15 END
.l2_15:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 16 START
	cmp al, 1
	jne .l2_16
.l1_16:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; IF 16 END
.l2_16:
;; GET VAR 32
	mov rax, [rbp-32]
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL brk
	call brk
;; PUSH
	push rax
;; GET VAR 32
	mov rbx, [rbp-32]
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 18 START
	cmp al, 1
	jne .l2_18
.l1_18:
;; VALUE -1
	mov rax, -1
;; RETURN
	leave
	ret
;; IF 18 END
.l2_18:
;; GET VAR 32
	mov rax, [rbp-32]
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; GET VAR 16
	mov rax, [rbp-16]
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; RETURN
	leave
	ret
;; VALUE 0
	mov rax, 0
;; SET GLOBAL
	mov qword [global__m_heap_start+0], rax
;; VALUE 0
	mov rax, 0
;; SET GLOBAL
	mov qword [global__m_location+0], rax
;; FUNCTION DECL _malloc
_malloc:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET GLOBAL
	mov rax, [global__m_heap_start+0]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 18 START
	cmp al, 1
	jne .l2_18
.l1_18:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL sbrk
	call sbrk
;; SET GLOBAL
	mov qword [global__m_heap_start+0], rax
;; VALUE 1000
	mov rax, 1000
;; PUSH
	push rax
;; VALUE 8
	mov rbx, 8
;; PUSH
	push rbx
;; VALUE 8
	mov rcx, 8
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL sbrk
	call sbrk
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL ptr2arr
	call ptr2arr
;; SET GLOBAL
	mov qword [global__m_blocks+0], rax
;; VALUE 1000
	mov rax, 1000
;; PUSH
	push rax
;; VALUE 8
	mov rbx, 8
;; PUSH
	push rbx
;; VALUE 8
	mov rcx, 8
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL sbrk
	call sbrk
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL ptr2arr
	call ptr2arr
;; SET GLOBAL
	mov qword [global__m_holes+0], rax
;; IF 18 END
.l2_18:
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 20 START
.while_20:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_location+0]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 20 CHECK
	cmp rax, 1
	jne .while_20_end
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 16
	mov rbx, 16
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_holes+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; GET STRUCT FIELD
	add rax, 8
	mov rax, [rax]
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setge al
	mov al, al
;; IF 22 START
	cmp al, 1
	jne .l2_22
.l1_22:
;; VALUE FOUND HOLE\n
	mov rax, str_found_hole_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 16
	mov rbx, 16
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_holes+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; GET STRUCT FIELD
	add rax, 0
	mov rax, [rax]
;; RETURN
	leave
	ret
;; IF 22 END
.l2_22:
;; VALUE 1
	mov rax, 1
;; GET VAR 16
	mov rbx, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 20 END
	jmp .while_20
.while_20_end:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL sbrk
	call sbrk
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; GET VAR 8
	mov rbx, [rbp-8]
;; GET GLOBAL
	mov rcx, [global__m_location+0]
;; VALUE 16
	mov rdx, 16
;; MUL
	imul rcx, rdx
;; GET GLOBAL
	mov rdx, [global__m_blocks+0]
;; SET ARRAY INDEX
	push rbx
	push rax
	mov rax, rdx
	add rax, 8
	add rax, rcx
	pop rbx
	mov [rax+0], rbx
	pop rcx
	mov [rax+8], rcx
;; VALUE 1
	mov rax, 1
;; GET GLOBAL
	mov rbx, [global__m_location+0]
;; ADD
	add rax, rbx
;; SET GLOBAL
	mov qword [global__m_location+0], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; RETURN
	leave
	ret
;; FUNCTION DECL _free
_free:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 20 START
.while_20:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_location+0]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 20 CHECK
	cmp rax, 1
	jne .while_20_end
;; VALUE searching...\n
	mov rax, str_searching____n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 16
	mov rbx, 16
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_blocks+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; GET STRUCT FIELD
	add rax, 0
	mov rax, [rax]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL exit
	call exit
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; GET VAR 16
	mov rbx, [rbp-16]
;; VALUE 16
	mov rcx, 16
;; MUL
	imul rbx, rcx
;; PUSH
	push rbx
;; GET GLOBAL
	mov rcx, [global__m_blocks+0]
;; POP
	pop rbx
;; ARRAY INDEX
	add rcx, 8
	add rcx, rbx
	mov rbx, rcx
;; GET STRUCT FIELD
	add rbx, 0
	mov rbx, [rbx]
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 24 START
	cmp al, 1
	jne .l2_24
.l1_24:
;; VALUE freed\n
	mov rax, str_freed_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; IF 24 END
.l2_24:
;; VALUE 1
	mov rax, 1
;; GET VAR 16
	mov rbx, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 20 END
	jmp .while_20
.while_20_end:
;; RETURN
	leave
	ret
;; FUNCTION DECL malloc
malloc:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _malloc
	call _malloc
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL set_ptr
	call set_ptr
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL ptr2arr
	call ptr2arr
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; RETURN
	leave
	ret
;; FUNCTION DECL free
free:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _free
	call _free
;; RETURN
	leave
	ret
;; FUNCTION DECL ptr_to_string
ptr_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 16
	mov [rbp-16], rbx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL set_ptr
	call set_ptr
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL ptr2arr
	call ptr2arr
;; RETURN
	leave
	ret
;; FUNCTION DECL strcpy
strcpy:
	push rbp
	mov rbp,rsp
	sub rsp, 16
	mov [rbp-16], rax
	sub rsp, 8
	mov [rbp-8], rbx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL _strcpy
	call _strcpy
;; RETURN
	leave
	ret
;; FUNCTION DECL _strcpy
_strcpy:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 24
	mov [rbp-24], rbx
	sub rsp, 16
	mov [rbp-16], rcx
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arrlen
	call arrlen
;; GET RETURN VALUE
	mov rbx, rax
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 24 START
	cmp al, 1
	jne .l2_24
.l1_24:
;; RETURN
	leave
	ret
;; IF 24 END
.l2_24:
;; GET VAR 24
	mov rax, [rbp-24]
;; VALUE 1
	mov rbx, 1
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET VAR 16
	mov rbx, [rbp-16]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, [rbx]
;; GET VAR 24
	mov rbx, [rbp-24]
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 8
	mov rcx, [rbp-8]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL _strcpy
	call _strcpy
;; RETURN
	leave
	ret
;; FUNCTION DECL exit
exit:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 60
	mov rax, 60
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL syscall1
	call syscall1
;; RETURN
	leave
	ret
;; FUNCTION DECL read
read:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arr2addr
	call arr2addr
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arrlen
	call arrlen
;; PUSH
	push rax
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL syscall3
	call syscall3
;; RETURN
	leave
	ret
;; FUNCTION DECL print
print:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 1
	mov rax, 1
;; PUSH
	push rax
;; VALUE 1
	mov rax, 1
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arr2addr
	call arr2addr
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arrlen
	call arrlen
;; PUSH
	push rax
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL syscall3
	call syscall3
;; RETURN
	leave
	ret
;; FUNCTION DECL bool_to_string
bool_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], al
;; GET VAR 1
	mov al, [rbp-1]
;; IF 28 START
	cmp al, 1
	jne .l2_28
.l1_28:
;; VALUE true
	mov rax, str_true
;; RETURN
	leave
	ret
;; IF 28 END
.l2_28:
;; VALUE false
	mov rax, str_false
;; RETURN
	leave
	ret
;; FUNCTION DECL f
f:
	push rbp
	mov rbp,rsp
;; GET GLOBAL
	mov rax, [global_test+0]
;; GET GLOBAL
	mov rbx, [global_test+8]
;; RETURN
	leave
	ret
;; FUNCTION DECL ident
ident:
	push rbp
	mov rbp,rsp
	sub rsp, 16
	mov [rbp-16], rax
	sub rsp, 24
	mov [rbp-24], rax
;; GET VAR 16
	mov rax, [rbp-16]
	mov rbx, [rbp-24]
;; RETURN
	leave
	ret
;; FUNCTION DECL str
str:
	push rbp
	mov rbp,rsp
;; VALUE 2
	mov rax, 2
;; VALUE 1
	mov rbx, 1
;; RETURN
	leave
	ret
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; VALUE 10
	mov rax, 10
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL malloc
	call malloc
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; VALUE 10
	mov rax, 10
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL malloc
	call malloc
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL free
	call free
;; VALUE 0
	mov rax, 0
;; RETURN
	leave
	ret
;; VALUE 10
	mov rax, 10
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL malloc
	call malloc
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL read
	call read
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
_end:
;; EXIT
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
global__m_heap_start:
	resb 8
global__m_blocks:
	resb 8
global__m_holes:
	resb 8
global_test:
	resb 16
