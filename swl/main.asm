global _start
section .text
_start:
	mov [ARGS], rsp
	push rbp
	mov rbp, rsp
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; SET GLOBAL
	mov byte [global_NULL+0], al
;; FUNCTION CALL main
	call main
;; JUMP
	jmp _end
;; INTRINSIC syscall
syscall0: 
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
syscall1: 
	jmp intrinsic_syscall
;; INTRINSIC syscall
syscall2: 
	jmp intrinsic_syscall
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
;; INTRINSIC get_args
__get_args: 
	jmp intrinsic_get_args
intrinsic_get_args: 
	mov rax, [ARGS]
	ret

;; FUNCTION DECL cstrlen
cstrlen:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 196608 START
.loop_196608:
;; VALUE true
	mov rax, 1
;; WHILE 196608 CHECK
	cmp al, 1
	jne .loop_196608_end
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
;; GET VAR 16
	mov rbx, [rbp-16]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; GET VAR 24
	mov rax, [rbp-24]
;; DEREFERENCE
	mov rax, [rax]
;; PUSH
	push rax
;; NOT
	test al, al
	setz al
;; IF 2359296 START
	cmp al, 1
	jne .l2_2359296
.l1_2359296:
;; BREAK 196608
	jmp .loop_196608_end
;; IF 2359296 END
.l2_2359296:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 196608 END
	jmp .loop_196608
.loop_196608_end:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL cstr_to_str
cstr_to_str:
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
;; FUNCTION CALL cstrlen
	call cstrlen
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL alloc_array
	call alloc_array
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 786432 START
.loop_786432:
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL cstrlen
	call cstrlen
;; GET RETURN VALUE
	mov rbx, rax
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 786432 CHECK
	cmp al, 1
	jne .loop_786432_end
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
;; GET VAR 24
	mov rbx, [rbp-24]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; DEREFERENCE
	mov rax, [rax]
;; SET VAR 33
	sub rsp, 33
	mov byte [rbp-33], al
;; GET VAR 33
	mov al, [rbp-33]
;; GET VAR 24
	mov rbx, [rbp-24]
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 16
	mov rcx, [rbp-16]
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
;; WHILE 786432 END
	jmp .loop_786432
.loop_786432_end:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL to_cstr
to_cstr:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
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
;; FUNCTION CALL _malloc
	call _malloc
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 1572864 START
.loop_1572864:
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; GET RETURN VALUE
	mov rbx, rax
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 1572864 CHECK
	cmp al, 1
	jne .loop_1572864_end
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 24
	mov rbx, [rbp-24]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; GET VAR 24
	mov rax, [rbp-24]
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
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL set_addr
	call set_addr
;; VALUE 1
	mov rbx, 1
;; GET VAR 24
	mov rax, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 1572864 END
	jmp .loop_1572864
.loop_1572864_end:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL len
	call len
;; GET RETURN VALUE
	mov rbx, rax
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; VALUE 0
	mov rax, 0
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
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; RETURN
	leave
	ret
;; FUNCTION DECL args
args:
	push rbp
	mov rbp,rsp
;; FUNCTION CALL __get_args
	call __get_args
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; FUNCTION DECL env
env:
	push rbp
	mov rbp,rsp
;; FUNCTION CALL __get_args
	call __get_args
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arrlen
	call arrlen
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 12582912 START
.loop_12582912:
;; VALUE true
	mov rax, 1
;; WHILE 12582912 CHECK
	cmp al, 1
	jne .loop_12582912_end
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 16
	mov rbx, [rbp-16]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; ADD
	add rax, rbx
;; VALUE 8
	mov rbx, 8
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
	mov rax, [rbx+0]
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; SET VAR 40
	sub rsp, 40
	mov qword [rbp-40], rax
;; GET VAR 40
	mov rax, [rbp-40]
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
;; IF 301989888 START
	cmp al, 1
	jne .l2_301989888
.l1_301989888:
;; BREAK 12582912
	jmp .loop_12582912_end
;; IF 301989888 END
.l2_301989888:
;; VALUE 1
	mov rbx, 1
;; GET VAR 24
	mov rax, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 12582912 END
	jmp .loop_12582912
.loop_12582912_end:
;; GET VAR 24
	mov rax, [rbp-24]
;; SET VAR 48
	sub rsp, 48
	mov qword [rbp-48], rax
;; GET VAR 48
	mov rax, [rbp-48]
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
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL alloc_array
	call alloc_array
;; SET VAR 56
	sub rsp, 56
	mov qword [rbp-56], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 64
	sub rsp, 64
	mov qword [rbp-64], rax
;; WHILE 201326592 START
.loop_201326592:
;; GET VAR 64
	mov rax, [rbp-64]
;; PUSH
	push rax
;; GET VAR 48
	mov rbx, [rbp-48]
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; WHILE 201326592 CHECK
	cmp al, 1
	jne .loop_201326592_end
;; GET VAR 64
	mov rax, [rbp-64]
;; PUSH
	push rax
;; GET VAR 16
	mov rbx, [rbp-16]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; VALUE 1
	mov rbx, 1
;; POP
	pop rax
;; ADD
	add rax, rbx
;; VALUE 8
	mov rbx, 8
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
	mov rax, [rbx+0]
;; SET VAR 72
	sub rsp, 72
	mov qword [rbp-72], rax
;; GET VAR 72
	mov rax, [rbp-72]
;; GET VAR 64
	mov rbx, [rbp-64]
;; VALUE 8
	mov rcx, 8
;; MUL
	imul rbx, rcx
;; GET VAR 56
	mov rcx, [rbp-56]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; VALUE 1
	mov rbx, 1
;; GET VAR 64
	mov rax, [rbp-64]
;; ADD
	add rax, rbx
;; SET VAR 64
	sub rsp, 64
	mov qword [rbp-64], rax
;; WHILE 201326592 END
	jmp .loop_201326592
.loop_201326592_end:
;; GET VAR 56
	mov rax, [rbp-56]
;; RETURN
	leave
	ret
;; FUNCTION DECL envp
envp:
	push rbp
	mov rbp,rsp
;; FUNCTION CALL env
	call env
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; VALUE 8
	mov rbx, 8
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; RETURN
	leave
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
;; IF 100663296 START
	cmp al, 1
	jne .l2_100663296
.l1_100663296:
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
;; IF 100663296 END
.l2_100663296:
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
;; IF 201326592 START
	cmp al, 1
	jne .l2_201326592
.l1_201326592:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; IF 201326592 END
.l2_201326592:
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
;; IF 805306368 START
	cmp al, 1
	jne .l2_805306368
.l1_805306368:
;; VALUE -1
	mov rax, -1
;; RETURN
	leave
	ret
;; IF 805306368 END
.l2_805306368:
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
;; IF 402653184 START
	cmp al, 1
	jne .l2_402653184
.l1_402653184:
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
;; IF 402653184 END
.l2_402653184:
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 1610612736 START
.loop_1610612736:
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
;; WHILE 1610612736 CHECK
	cmp al, 1
	jne .loop_1610612736_end
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
;; IF 19327352832 START
	cmp al, 1
	jne .l2_19327352832
.l1_19327352832:
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
;; IF 19327352832 END
.l2_19327352832:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 1610612736 END
	jmp .loop_1610612736
.loop_1610612736_end:
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
;; WHILE 1610612736 START
.loop_1610612736:
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
;; WHILE 1610612736 CHECK
	cmp al, 1
	jne .loop_1610612736_end
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
;; IF 19327352832 START
	cmp al, 1
	jne .l2_19327352832
.l1_19327352832:
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
;; IF 231928233984 START
	cmp al, 1
	jne .l2_231928233984
.l1_231928233984:
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
;; IF 1391569403904 START
	cmp al, 1
	jne .l2_1391569403904
.l1_1391569403904:
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
;; IF 1391569403904 END
.l2_1391569403904:
;; IF 231928233984 END
.l2_231928233984:
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
;; IF 463856467968 START
	cmp al, 1
	jne .l2_463856467968
.l1_463856467968:
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
;; IF 2783138807808 START
	cmp al, 1
	jne .l2_2783138807808
.l1_2783138807808:
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
;; IF 2783138807808 END
.l2_2783138807808:
;; IF 463856467968 END
.l2_463856467968:
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
;; IF 927712935936 START
	cmp al, 1
	jne .l2_927712935936
.l1_927712935936:
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
;; IF 927712935936 END
.l2_927712935936:
;; RETURN
	leave
	ret
;; IF 19327352832 END
.l2_19327352832:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 1610612736 END
	jmp .loop_1610612736
.loop_1610612736_end:
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
;; WHILE 25769803776 START
.loop_25769803776:
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
;; WHILE 25769803776 CHECK
	cmp al, 1
	jne .loop_25769803776_end
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
;; WHILE 25769803776 END
	jmp .loop_25769803776
.loop_25769803776_end:
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
	mov rbx, 0
;; PUSH
	push rbx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arr2addr
	call arr2addr
;; GET RETURN VALUE
	mov rcx, rax
;; PUSH
	push rcx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL arrlen
	call arrlen
;; GET RETURN VALUE
	mov rdx, rax
;; PUSH
	push rdx
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
;; FUNCTION DECL fork
fork:
	push rbp
	mov rbp,rsp
;; VALUE 57
	mov rax, 57
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL syscall0
	call syscall0
;; RETURN
	leave
	ret
;; FUNCTION DECL execve
execve:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 16
	mov [rbp-16], rbx
	sub rsp, 24
	mov [rbp-24], rcx
;; VALUE 59
	mov rax, 59
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL to_cstr
	call to_cstr
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; VALUE 8
	mov rbx, 8
;; POP
	pop rax
;; ADD
	add rax, rbx
;; PUSH
	push rax
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; PUSH
	push rax
;; VALUE 8
	mov rbx, 8
;; POP
	pop rax
;; ADD
	add rax, rbx
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
;; FUNCTION DECL system
system:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 8
	mov rax, 8
;; PUSH
	push rax
;; VALUE 4
	mov rbx, 4
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL alloc_array
	call alloc_array
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE /usr/bin/sh
	mov rax, str__usr_bin_sh
;; FUNCTION CALL to_cstr
	call to_cstr
;; VALUE 0
	mov rbx, 0
;; VALUE 8
	mov rcx, 8
;; MUL
	imul rbx, rcx
;; GET VAR 16
	mov rcx, [rbp-16]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; VALUE -c
	mov rax, str__c
;; FUNCTION CALL to_cstr
	call to_cstr
;; VALUE 1
	mov rbx, 1
;; VALUE 8
	mov rcx, 8
;; MUL
	imul rbx, rcx
;; GET VAR 16
	mov rcx, [rbp-16]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL to_cstr
	call to_cstr
;; VALUE 2
	mov rbx, 2
;; VALUE 8
	mov rcx, 8
;; MUL
	imul rbx, rcx
;; GET VAR 16
	mov rcx, [rbp-16]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; VALUE 3
	mov rbx, 3
;; VALUE 8
	mov rcx, 8
;; MUL
	imul rbx, rcx
;; GET VAR 16
	mov rcx, [rbp-16]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add rax, rbx
	pop rbx
	mov [rax], rbx
;; VALUE INIT PID = 
	mov rax, str_init_pid___
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL print
	call print
;; VALUE 39
	mov rax, 39
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL syscall0
	call syscall0
;; FUNCTION CALL to_string
	call to_string
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL println
	call println
;; FUNCTION CALL fork
	call fork
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
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
;; IF 211106232532992 START
	cmp al, 1
	jne .l2_211106232532992
.l1_211106232532992:
;; VALUE /usr/bin/sh
	mov rax, str__usr_bin_sh
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION CALL env
	call env
;; PUSH
	push rax
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL execve
	call execve
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL exit
	call exit
;; IF 211106232532992 END
.l2_211106232532992:
;; WHILE 422212465065984 START
.loop_422212465065984:
;; VALUE true
	mov rax, 1
;; WHILE 422212465065984 CHECK
	cmp al, 1
	jne .loop_422212465065984_end
;; VALUE 10
	mov rax, 10
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL sleep
	call sleep
;; VALUE 121
	mov rax, 121
;; PUSH
	push rax
;; GET VAR 24
	mov rbx, [rbp-24]
;; PUSH
	push rbx
;; POP
	pop rbx
;; POP
	pop rax
;; FUNCTION CALL syscall1
	call syscall1
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; PUSH
	push rax
;; VALUE 0
	mov rbx, 0
;; POP
	pop rax
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 10133099161583616 START
	cmp al, 1
	jne .l2_10133099161583616
.l1_10133099161583616:
;; BREAK 422212465065984
	jmp .loop_422212465065984_end
;; IF 10133099161583616 END
.l2_10133099161583616:
;; GET VAR 32
	mov rax, [rbp-32]
;; FUNCTION CALL to_string
	call to_string
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL println
	call println
;; WHILE 422212465065984 END
	jmp .loop_422212465065984
.loop_422212465065984_end:
;; RETURN
	leave
	ret
;; FUNCTION DECL sleep
sleep:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; VALUE 1000
	mov rbx, 1000
;; POP
	pop rax
;; DIV
	xor rdx, rdx
	mov rax, rax
	idiv rbx
;; GET VAR 8
	mov rbx, [rbp-8]
;; PUSH
	push rbx
;; VALUE 1000
	mov rcx, 1000
;; POP
	pop rbx
;; MOD
	mov rax, rbx
	xor rdx, rdx
	idiv rcx
	mov rbx, rdx
;; PUSH
	push rbx
;; VALUE 1000000
	mov rcx, 1000000
;; POP
	pop rbx
;; MUL
	imul rbx, rcx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rbx
;; VALUE 35
	mov rax, 35
;; PUSH
	push rax
;; GET VAR REF 24
	mov rax, rbp
	sub rax, 24
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
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
;; FUNCTION CALL syscall2
	call syscall2
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
;; WHILE 211106232532992 START
.loop_211106232532992:
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
;; WHILE 211106232532992 CHECK
	cmp al, 1
	jne .loop_211106232532992_end
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
;; WHILE 211106232532992 END
	jmp .loop_211106232532992
.loop_211106232532992_end:
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
;; WHILE 3377699720527872 START
.loop_3377699720527872:
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
;; WHILE 3377699720527872 CHECK
	cmp al, 1
	jne .loop_3377699720527872_end
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
;; WHILE 3377699720527872 END
	jmp .loop_3377699720527872
.loop_3377699720527872_end:
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
;; IF 211106232532992 START
	cmp al, 1
	jne .l2_211106232532992
.l1_211106232532992:
;; VALUE true
	mov rax, str_true
;; RETURN
	leave
	ret
;; IF 211106232532992 END
.l2_211106232532992:
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
;; IF 1688849860263936 START
	cmp al, 1
	jne .l2_1688849860263936
.l1_1688849860263936:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 1688849860263936 END
.l2_1688849860263936:
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
;; IF 3377699720527872 START
	cmp al, 1
	jne .l2_3377699720527872
.l1_3377699720527872:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 3377699720527872 END
.l2_3377699720527872:
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
;; IF 6755399441055744 START
	cmp al, 1
	jne .l2_6755399441055744
.l1_6755399441055744:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 6755399441055744 END
.l2_6755399441055744:
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
;; IF 13510798882111488 START
	cmp al, 1
	jne .l2_13510798882111488
.l1_13510798882111488:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 13510798882111488 END
.l2_13510798882111488:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL __unsafe_convert
	call __unsafe_convert
;; SET GLOBAL
	mov byte [global_NULL+0], al
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; GET ARRAY
	mov rax, arr_0
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; WHILE 96 START
.loop_96:
;; VALUE true
	mov rax, 1
;; WHILE 96 CHECK
	cmp al, 1
	jne .loop_96_end
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL read
	call read
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; GET GLOBAL
	mov rax, [global_NULL+0]
;; GET VAR 16
	mov rbx, [rbp-16]
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
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; POP
	pop rax
;; FUNCTION CALL system
	call system
;; WHILE 96 END
	jmp .loop_96
.loop_96_end:
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
ARGS:
	resb 8
str_free__double_free_n:
	dq 18
	db `free: double free\n`
str__usr_bin_sh:
	dq 11
	db `/usr/bin/sh`
str__c:
	dq 2
	db `-c`
str_init_pid___:
	dq 11
	db `INIT PID = `
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
arr_0:
	dq 100
	resb 100
global__m_tcache:
	resb 8
global__m_location:
	resb 8
global_NULL:
	resb 1
global__m_heap_start:
	resb 8
