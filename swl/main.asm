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
;; IF 49152 START
	cmp al, 1
	jne .l2_49152
.l1_49152:
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
;; IF 49152 END
.l2_49152:
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
;; IF 98304 START
	cmp al, 1
	jne .l2_98304
.l1_98304:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; IF 98304 END
.l2_98304:
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
;; IF 393216 START
	cmp al, 1
	jne .l2_393216
.l1_393216:
;; VALUE -1
	mov rax, -1
;; RETURN
	leave
	ret
;; IF 393216 END
.l2_393216:
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
;; IF 196608 END
.l2_196608:
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 786432 START
.loop_786432:
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
;; WHILE 786432 CHECK
	cmp rax, 1
	jne .loop_786432_end
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
;; IF 9437184 START
	cmp al, 1
	jne .l2_9437184
.l1_9437184:
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
;; IF 9437184 END
.l2_9437184:
;; VALUE 1
	mov rax, 1
;; GET VAR 16
	mov rbx, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 786432 END
	jmp .loop_786432
.loop_786432_end:
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
	mov rax, 1
;; GET GLOBAL
	mov rbx, [global__m_location+0]
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
;; WHILE 786432 START
.loop_786432:
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
;; WHILE 786432 CHECK
	cmp rax, 1
	jne .loop_786432_end
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
;; IF 9437184 START
	cmp al, 1
	jne .l2_9437184
.l1_9437184:
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
;; IF 113246208 START
	cmp al, 1
	jne .l2_113246208
.l1_113246208:
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
;; IF 679477248 START
	cmp al, 1
	jne .l2_679477248
.l1_679477248:
;; VALUE UNIFY LEFT\n
	mov rax, str_unify_left_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
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
;; IF 679477248 END
.l2_679477248:
;; IF 113246208 END
.l2_113246208:
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
;; IF 226492416 START
	cmp al, 1
	jne .l2_226492416
.l1_226492416:
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
;; IF 1358954496 START
	cmp al, 1
	jne .l2_1358954496
.l1_1358954496:
;; VALUE UNIFY RIGHT\n
	mov rax, str_unify_right_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
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
;; IF 1358954496 END
.l2_1358954496:
;; IF 226492416 END
.l2_226492416:
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
;; IF 452984832 START
	cmp al, 1
	jne .l2_452984832
.l1_452984832:
;; VALUE DECREMENT\n
	mov rax, str_decrement_n
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
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
;; IF 452984832 END
.l2_452984832:
;; RETURN
	leave
	ret
;; IF 9437184 END
.l2_9437184:
;; VALUE 1
	mov rax, 1
;; GET VAR 16
	mov rbx, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 786432 END
	jmp .loop_786432
.loop_786432_end:
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
;; WHILE 12582912 START
.loop_12582912:
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
;; WHILE 12582912 CHECK
	cmp rax, 1
	jne .loop_12582912_end
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
	mov rax, 1
;; GET VAR 24
	mov rbx, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 12582912 END
	jmp .loop_12582912
.loop_12582912_end:
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
;; IF 50331648 START
	cmp al, 1
	jne .l2_50331648
.l1_50331648:
;; VALUE true
	mov rax, str_true
;; RETURN
	leave
	ret
;; IF 50331648 END
.l2_50331648:
;; VALUE false
	mov rax, str_false
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
;; IF 201326592 START
	cmp al, 1
	jne .l2_201326592
.l1_201326592:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 201326592 END
.l2_201326592:
;; GET VAR 16
	mov rax, [rbp-16]
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
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; VALUE string
	mov rax, str_string
;; FUNCTION CALL len
	call len
;; RETURN
	leave
	ret
_end:
;; EXIT
	mov rdi, rax
	mov rax, 60
	syscall
section .data
str_unify_left_n:
	dq 11
	db `UNIFY LEFT\n`
str_unify_right_n:
	dq 12
	db `UNIFY RIGHT\n`
str_decrement_n:
	dq 10
	db `DECREMENT\n`
str_free__double_free_n:
	dq 18
	db `free: double free\n`
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
str_string:
	dq 6
	db `string`
global__m_location:
	resb 8
global__m_tcache:
	resb 8
global__m_heap_start:
	resb 8
