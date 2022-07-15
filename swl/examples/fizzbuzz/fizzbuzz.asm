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

;; INTRINSIC set_ptr
set_addr: 
	jmp intrinsic_set_ptr
;; FUNCTION DECL _brk
_brk:
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
;; FUNCTION DECL _sbrk
_sbrk:
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
;; IF 196608 START
	cmp al, 1
	jne .l2_196608
.l1_196608:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _brk
	call _brk
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; IF 196608 END
.l2_196608:
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
;; IF 393216 START
	cmp al, 1
	jne .l2_393216
.l1_393216:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; IF 393216 END
.l2_393216:
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
;; FUNCTION CALL _brk
	call _brk
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
;; IF 1572864 START
	cmp al, 1
	jne .l2_1572864
.l1_1572864:
;; VALUE -1
	mov rax, -1
;; RETURN
	leave
	ret
;; IF 1572864 END
.l2_1572864:
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
;; IF 786432 START
	cmp al, 1
	jne .l2_786432
.l1_786432:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _sbrk
	call _sbrk
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
;; FUNCTION CALL _sbrk
	call _sbrk
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL ptr2arr
	call ptr2arr
;; SET GLOBAL
	mov qword [global__m_tcache+0], rax
;; IF 786432 END
.l2_786432:
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3145728 START
.loop_3145728:
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
;; WHILE 3145728 CHECK
	cmp al, 1
	jne .loop_3145728_end
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rdx, rbx
	mov al, [rdx+0]
	mov rbx, [rdx+1]
	mov rcx, [rdx+9]
;; SET VAR 33
	sub rsp, 33
	mov byte [rbp-33], al
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rcx
;; GET VAR REF 33
	mov rax, rbp
	sub rax, 33
;; GET STRUCT FIELD
	add rax, 0
	mov rax, [rax]
;; PUSH
	push rax
;; NOT
	test al, al
	setz al
;; PUSH
	push rax
;; GET VAR REF 33
	mov rbx, rbp
	sub rbx, 33
;; GET STRUCT FIELD
	add rbx, 9
	mov rbx, [rbx]
;; PUSH
	push rbx
;; GET VAR 8
	mov rcx, [rbp-8]
;; POP
	pop rbx
;; LESS
	cmp rbx, rcx
	setge al
	mov bl, al
;; POP
	pop rax
;; AND
	and al, bl
;; IF 37748736 START
	cmp al, 1
	jne .l2_37748736
.l1_37748736:
;; VALUE 0
	mov rax, 0
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; VALUE true
	mov rbx, 1
;; SET STRUCT FIELD
	add rax, 0
	mov [rax], bl
;; GET VAR REF 33
	mov rax, rbp
	sub rax, 33
;; GET STRUCT FIELD
	add rax, 1
	mov rax, [rax]
;; RETURN
	leave
	ret
;; IF 37748736 END
.l2_37748736:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3145728 END
	jmp .loop_3145728
.loop_3145728_end:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _sbrk
	call _sbrk
;; SET VAR 41
	sub rsp, 41
	mov qword [rbp-41], rax
;; GET VAR 41
	mov rax, [rbp-41]
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; SET VAR 49
	sub rsp, 49
	mov qword [rbp-49], rax
;; VALUE true
	mov rax, 1
;; GET VAR 41
	mov rbx, [rbp-41]
;; GET VAR 8
	mov rcx, [rbp-8]
;; GET GLOBAL
	mov rdx, [global__m_location+0]
;; VALUE 17
	mov rdi, 17
;; MUL
	imul rdx, rdi
;; GET GLOBAL
	mov rdi, [global__m_tcache+0]
;; SET ARRAY INDEX
	push rcx
	push rbx
	push rax
	mov rax, rdi
	add rax, 8
	add rax, rdx
	pop rbx
	mov [rax+0], bl
	pop rcx
	mov [rax+1], rcx
	pop rdx
	mov [rax+9], rdx
;; VALUE 1
	mov rbx, 1
;; GET GLOBAL
	mov rax, [global__m_location+0]
;; ADD
	add rax, rbx
;; SET GLOBAL
	mov qword [global__m_location+0], rax
;; GET VAR 41
	mov rax, [rbp-41]
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
;; WHILE 3145728 START
.loop_3145728:
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
;; WHILE 3145728 CHECK
	cmp al, 1
	jne .loop_3145728_end
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rdx, rbx
	mov al, [rdx+0]
	mov rbx, [rdx+1]
	mov rcx, [rdx+9]
;; SET VAR 33
	sub rsp, 33
	mov byte [rbp-33], al
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rcx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; GET VAR REF 33
	mov rbx, rbp
	sub rbx, 33
;; GET STRUCT FIELD
	add rbx, 1
	mov rbx, [rbx]
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; PUSH
	push rax
;; GET VAR REF 33
	mov rbx, rbp
	sub rbx, 33
;; GET STRUCT FIELD
	add rbx, 0
	mov rbx, [rbx]
;; POP
	pop rax
;; AND
	and al, bl
;; IF 37748736 START
	cmp al, 1
	jne .l2_37748736
.l1_37748736:
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; VALUE false
	mov rbx, 0
;; SET STRUCT FIELD
	add rax, 0
	mov [rax], bl
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; GREATER
	cmp rax, rbx
	setg al
	mov al, al
;; IF 452984832 START
	cmp al, 1
	jne .l2_452984832
.l1_452984832:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; SUB
	sub rax, rbx
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
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
;; NOT
	test al, al
	setz al
;; IF 2717908992 START
	cmp al, 1
	jne .l2_2717908992
.l1_2717908992:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; SUB
	sub rax, rbx
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; GET VAR 16
	mov rbx, [rbp-16]
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; SUB
	sub rbx, rcx
;; VALUE 17
	mov rcx, 17
;; MUL
	imul rbx, rcx
;; PUSH
	push rbx
;; GET GLOBAL
	mov rcx, [global__m_tcache+0]
;; POP
	pop rbx
;; ARRAY INDEX
	add rcx, 8
	add rcx, rbx
	mov rbx, rcx
;; GET STRUCT FIELD
	add rbx, 9
	mov rbx, [rbx]
;; PUSH
	push rbx
;; GET VAR 16
	mov rcx, [rbp-16]
;; VALUE 17
	mov rdx, 17
;; MUL
	imul rcx, rdx
;; PUSH
	push rcx
;; GET GLOBAL
	mov rdx, [global__m_tcache+0]
;; POP
	pop rcx
;; ARRAY INDEX
	add rdx, 8
	add rdx, rcx
	mov rcx, rdx
;; GET STRUCT FIELD
	add rcx, 9
	mov rcx, [rcx]
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; SET STRUCT FIELD
	add rax, 9
	mov [rax], rbx
;; IF 2717908992 END
.l2_2717908992:
;; IF 452984832 END
.l2_452984832:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_location+0]
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; SUB
	sub rbx, rcx
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 905969664 START
	cmp al, 1
	jne .l2_905969664
.l1_905969664:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; ADD
	add rax, rbx
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
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
;; NOT
	test al, al
	setz al
;; IF 5435817984 START
	cmp al, 1
	jne .l2_5435817984
.l1_5435817984:
;; GET VAR 16
	mov rax, [rbp-16]
;; VALUE 17
	mov rbx, 17
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_tcache+0]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rax, rbx
;; GET VAR 16
	mov rbx, [rbp-16]
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; VALUE 17
	mov rcx, 17
;; MUL
	imul rbx, rcx
;; PUSH
	push rbx
;; GET GLOBAL
	mov rcx, [global__m_tcache+0]
;; POP
	pop rbx
;; ARRAY INDEX
	add rcx, 8
	add rcx, rbx
	mov rbx, rcx
;; GET STRUCT FIELD
	add rbx, 9
	mov rbx, [rbx]
;; PUSH
	push rbx
;; GET VAR 16
	mov rcx, [rbp-16]
;; VALUE 17
	mov rdx, 17
;; MUL
	imul rcx, rdx
;; PUSH
	push rcx
;; GET GLOBAL
	mov rdx, [global__m_tcache+0]
;; POP
	pop rcx
;; ARRAY INDEX
	add rdx, 8
	add rdx, rcx
	mov rcx, rdx
;; GET STRUCT FIELD
	add rcx, 9
	mov rcx, [rcx]
;; POP
	pop rbx
;; ADD
	add rbx, rcx
;; SET STRUCT FIELD
	add rax, 9
	mov [rax], rbx
;; IF 5435817984 END
.l2_5435817984:
;; IF 905969664 END
.l2_905969664:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET GLOBAL
	mov rbx, [global__m_location+0]
;; PUSH
	push rbx
;; VALUE 1
	mov rcx, 1
;; POP
	pop rbx
;; SUB
	sub rbx, rcx
;; POP
	pop rax
;; EQ
	cmp rax, rbx
	sete al
	mov al, al
;; IF 1811939328 START
	cmp al, 1
	jne .l2_1811939328
.l1_1811939328:
;; VALUE -1
	mov rax, -1
;; PUSH
	push rax
;; GET VAR 16
	mov rbx, [rbp-16]
;; VALUE 17
	mov rcx, 17
;; MUL
	imul rbx, rcx
;; PUSH
	push rbx
;; GET GLOBAL
	mov rcx, [global__m_tcache+0]
;; POP
	pop rbx
;; ARRAY INDEX
	add rcx, 8
	add rcx, rbx
	mov rbx, rcx
;; GET STRUCT FIELD
	add rbx, 9
	mov rbx, [rbx]
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL _sbrk
	call _sbrk
;; IF 1811939328 END
.l2_1811939328:
;; RETURN
	leave
	ret
;; IF 37748736 END
.l2_37748736:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3145728 END
	jmp .loop_3145728
.loop_3145728_end:
;; VALUE free: double free\n
	mov rax, str_free__double_free_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; RETURN
	leave
	ret
;; FUNCTION DECL alloc_array
alloc_array:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 8
	mov rax, 8
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
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
;; FUNCTION CALL set_addr
	call set_addr
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
;; FUNCTION DECL free_array
free_array:
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
;; FUNCTION DECL strcpy
strcpy:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 16
	mov [rbp-16], rbx
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
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
;; PUSH
	push rbx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL min
	call min
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; WHILE 50331648 START
.loop_50331648:
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 32
	mov rbx, [rbp-32]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 50331648 CHECK
	cmp al, 1
	jne .loop_50331648_end
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
	mov rbx, rbx
	mov al, [rbx+0]
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
	add al, bl
	pop rbx
	mov [rax], bl
;; VALUE 1
	mov rbx, 1
;; GET VAR 24
	mov rax, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 50331648 END
	jmp .loop_50331648
.loop_50331648_end:
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
;; FUNCTION DECL println
println:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; VALUE 

	mov rbx, 10
;; PUSH
	push rbx
;; POP
	pop rbx
;; FUNCTION CALL append
	call append
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; RETURN
	leave
	ret
;; FUNCTION DECL char_code
char_code:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], al
;; GET VAR 1
	mov al, [rbp-1]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; RETURN
	leave
	ret
;; FUNCTION DECL to_char
to_char:
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
;; RETURN
	leave
	ret
;; FUNCTION DECL digit_to_ascii
digit_to_ascii:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 48
;; FUNCTION CALL char_code
	call char_code
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; FUNCTION CALL to_char
	call to_char
;; RETURN
	leave
	ret
;; FUNCTION DECL itoa
itoa:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 12884901888 START
.loop_12884901888:
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; VALUE 10
	mov rbx, 10
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setge al
	mov al, al
;; WHILE 12884901888 CHECK
	cmp al, 1
	jne .loop_12884901888_end
;; VALUE 10
	mov rbx, 10
;; GET VAR 24
	mov rax, [rbp-24]
;; DIV
	xor rdx, rdx
	mov rax, rax
	idiv rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 12884901888 END
	jmp .loop_12884901888
.loop_12884901888_end:
;; GET VAR 16
	mov rax, [rbp-16]
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
	pop rax
;; FUNCTION CALL alloc_array
	call alloc_array
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 16
	mov rax, [rbp-16]
;; SET VAR 40
	sub rsp, 40
	mov qword [rbp-40], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; SET VAR 48
	sub rsp, 48
	mov qword [rbp-48], rax
;; WHILE 206158430208 START
.loop_206158430208:
;; GET VAR 40
	mov rax, [rbp-40]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setge al
	mov al, al
;; WHILE 206158430208 CHECK
	cmp al, 1
	jne .loop_206158430208_end
;; GET VAR 48
	mov rax, [rbp-48]
;; PUSH
	push rax
;; VALUE 10
	mov rbx, 10
;; POP
	pop rax
;; MOD
	mov rax, rax
	xor rdx, rdx
	idiv rbx
	mov rax, rdx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL digit_to_ascii
	call digit_to_ascii
;; GET VAR 40
	mov rbx, [rbp-40]
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 32
	mov rcx, [rbp-32]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add al, bl
	pop rbx
	mov [rax], bl
;; VALUE 10
	mov rbx, 10
;; GET VAR 48
	mov rax, [rbp-48]
;; DIV
	xor rdx, rdx
	mov rax, rax
	idiv rbx
;; SET VAR 48
	sub rsp, 48
	mov qword [rbp-48], rax
;; VALUE 1
	mov rbx, 1
;; GET VAR 40
	mov rax, [rbp-40]
;; SUB
	sub rax, rbx
;; SET VAR 40
	sub rsp, 40
	mov qword [rbp-40], rax
;; WHILE 206158430208 END
	jmp .loop_206158430208
.loop_206158430208_end:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; FUNCTION DECL to_string
to_string:
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
;; FUNCTION CALL itoa
	call itoa
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
;; IF 12884901888 START
	cmp al, 1
	jne .l2_12884901888
.l1_12884901888:
;; VALUE true
	mov rax, str_true
;; RETURN
	leave
	ret
;; IF 12884901888 END
.l2_12884901888:
;; VALUE false
	mov rax, str_false
;; RETURN
	leave
	ret
;; FUNCTION DECL len
len:
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
;; FUNCTION CALL arrlen
	call arrlen
;; RETURN
	leave
	ret
;; FUNCTION DECL append
append:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
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
	pop rax
;; FUNCTION CALL alloc_array
	call alloc_array
;; SET VAR 17
	sub rsp, 17
	mov qword [rbp-17], rax
;; GET VAR 17
	mov rax, [rbp-17]
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
;; FUNCTION CALL strcpy
	call strcpy
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; SET VAR 25
	sub rsp, 25
	mov qword [rbp-25], rax
;; GET VAR 9
	mov al, [rbp-9]
;; GET VAR 25
	mov rbx, [rbp-25]
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 17
	mov rcx, [rbp-17]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add al, bl
	pop rbx
	mov [rax], bl
;; GET VAR 17
	mov rax, [rbp-17]
;; RETURN
	leave
	ret
;; FUNCTION DECL startsWith
startsWith:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 103079215104 START
	cmp al, 1
	jne .l2_103079215104
.l1_103079215104:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 103079215104 END
.l2_103079215104:
;; VALUE 0
	mov rax, 0
;; VALUE 1
	mov rbx, 1
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rbx, rbx
	mov al, [rbx+0]
;; PUSH
	push rax
;; GET VAR 9
	mov bl, [rbp-9]
;; POP
	pop rax
;; EQ
	cmp al, bl
	sete al
	mov al, al
;; RETURN
	leave
	ret
;; FUNCTION DECL endsWith
endsWith:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 206158430208 START
	cmp al, 1
	jne .l2_206158430208
.l1_206158430208:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 206158430208 END
.l2_206158430208:
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; SUB
	sub rax, rbx
;; VALUE 1
	mov rbx, 1
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ARRAY INDEX
	add rbx, 8
	add rbx, rax
	mov rbx, rbx
	mov al, [rbx+0]
;; PUSH
	push rax
;; GET VAR 9
	mov bl, [rbp-9]
;; POP
	pop rax
;; EQ
	cmp al, bl
	sete al
	mov al, al
;; RETURN
	leave
	ret
;; FUNCTION DECL min
min:
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
	mov rbx, [rbp-16]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 412316860416 START
	cmp al, 1
	jne .l2_412316860416
.l1_412316860416:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 412316860416 END
.l2_412316860416:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL max
max:
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
	mov rbx, [rbp-16]
;; POP
	pop rax
;; GREATER
	cmp rax, rbx
	setg al
	mov al, al
;; IF 824633720832 START
	cmp al, 1
	jne .l2_824633720832
.l1_824633720832:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 824633720832 END
.l2_824633720832:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL fizzbuzz_
fizzbuzz_:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; VALUE 15
	mov rbx, 15
;; POP
	pop rax
;; MOD
	mov rax, rax
	xor rdx, rdx
	idiv rbx
	mov rax, rdx
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
;; IF 24 START
	cmp al, 1
	jne .l2_24
.l1_24:
;; VALUE FizzBuzz
	mov rax, str_fizzbuzz
;; RETURN
	leave
	ret
;; IF 24 END
.l2_24:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; VALUE 5
	mov rbx, 5
;; POP
	pop rax
;; MOD
	mov rax, rax
	xor rdx, rdx
	idiv rbx
	mov rax, rdx
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
;; IF 48 START
	cmp al, 1
	jne .l2_48
.l1_48:
;; VALUE Buzz
	mov rax, str_buzz
;; RETURN
	leave
	ret
;; IF 48 END
.l2_48:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; VALUE 3
	mov rbx, 3
;; POP
	pop rax
;; MOD
	mov rax, rax
	xor rdx, rdx
	idiv rbx
	mov rax, rdx
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
;; IF 96 START
	cmp al, 1
	jne .l2_96
.l1_96:
;; VALUE Fizz
	mov rax, str_fizz
;; RETURN
	leave
	ret
;; IF 96 END
.l2_96:
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL to_string
	call to_string
;; RETURN
	leave
	ret
;; FUNCTION DECL fizzbuzz
fizzbuzz:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 1
	mov rax, 1
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 96 START
.loop_96:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 96 CHECK
	cmp al, 1
	jne .loop_96_end
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL fizzbuzz_
	call fizzbuzz_
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; VALUE , 
	mov rax, str___
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 96 END
	jmp .loop_96
.loop_96_end:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL fizzbuzz_
	call fizzbuzz_
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL println
	call println
;; RETURN
	leave
	ret
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; VALUE 1000
	mov rax, 1000
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL fizzbuzz
	call fizzbuzz
;; VALUE 0
	mov rax, 0
;; RETURN
	leave
	ret
_end:
;; EXIT
	mov rdi, rax
	mov rax, 60
	syscall
section .data
str_free__double_free_n:
	dq 18
	db `free: double free\n`
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
str_fizzbuzz:
	dq 8
	db `FizzBuzz`
str_buzz:
	dq 4
	db `Buzz`
str_fizz:
	dq 4
	db `Fizz`
str___:
	dq 2
	db `, `
global__m_tcache:
	resb 8
global__m_heap_start:
	resb 8
global__m_location:
	resb 8
