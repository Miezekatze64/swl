global _start
section .text
_start:
	mov [ARGS], rsp
	push rbp
	mov rbp, rsp
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
;; INTRINSIC syscall
syscall4: 
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
;; WHILE 393216 START
.loop_393216:
;; VALUE true
	mov rax, 1
;; WHILE 393216 CHECK
	cmp al, 1
	jne .loop_393216_end
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; IF 4718592 START
	cmp al, 1
	jne .l2_4718592
.l1_4718592:
;; BREAK 393216
	jmp .loop_393216_end
;; IF 4718592 END
.l2_4718592:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 393216 END
	jmp .loop_393216
.loop_393216_end:
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
;; FUNCTION ADRESS
	mov rbx, cstrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, alloc_array
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, cstrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; WHILE 1572864 END
	jmp .loop_1572864
.loop_1572864_end:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__11876854719037224982_to_cstr
__member__11876854719037224982_to_cstr:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; FUNCTION ADRESS
	mov rbx, _malloc
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 3145728 START
.loop_3145728:
;; GET VAR 24
	mov rax, [rbp-24]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
;; GET RETURN VALUE
	mov rbx, rax
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rcx, set_addr
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
;; VALUE 1
	mov rbx, 1
;; GET VAR 24
	mov rax, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 3145728 END
	jmp .loop_3145728
.loop_3145728_end:
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; FUNCTION ADRESS
	mov rcx, set_addr
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; RETURN
	leave
	ret
;; FUNCTION DECL args
args:
	push rbp
	mov rbp,rsp
;; FUNCTION ADRESS
	mov rax, __get_args
;; CALL PTR
	call rax
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
;; FUNCTION ADRESS
	mov rax, __get_args
;; CALL PTR
	call rax
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 25165824 START
.loop_25165824:
;; VALUE true
	mov rax, 1
;; WHILE 25165824 CHECK
	cmp al, 1
	jne .loop_25165824_end
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; IF 603979776 START
	cmp al, 1
	jne .l2_603979776
.l1_603979776:
;; BREAK 25165824
	jmp .loop_25165824_end
;; IF 603979776 END
.l2_603979776:
;; VALUE 1
	mov rbx, 1
;; GET VAR 24
	mov rax, [rbp-24]
;; ADD
	add rax, rbx
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; WHILE 25165824 END
	jmp .loop_25165824
.loop_25165824_end:
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
;; FUNCTION ADRESS
	mov rbx, alloc_array
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 56
	sub rsp, 56
	mov qword [rbp-56], rax
;; VALUE 0
	mov rax, 0
;; SET VAR 64
	sub rsp, 64
	mov qword [rbp-64], rax
;; WHILE 402653184 START
.loop_402653184:
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
;; WHILE 402653184 CHECK
	cmp al, 1
	jne .loop_402653184_end
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
;; WHILE 402653184 END
	jmp .loop_402653184
.loop_402653184_end:
;; GET VAR 56
	mov rax, [rbp-56]
;; RETURN
	leave
	ret
;; FUNCTION DECL envp
envp:
	push rbp
	mov rbp,rsp
;; FUNCTION ADRESS
	mov rax, env
;; CALL PTR
	call rax
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rcx, syscall1
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
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
;; IF 201326592 START
	cmp al, 1
	jne .l2_201326592
.l1_201326592:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, _brk
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; GET VAR 32
	mov rax, [rbp-32]
;; SET VAR 24
	sub rsp, 24
	mov qword [rbp-24], rax
;; IF 201326592 END
.l2_201326592:
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
;; IF 402653184 START
	cmp al, 1
	jne .l2_402653184
.l1_402653184:
;; GET VAR 32
	mov rax, [rbp-32]
;; RETURN
	leave
	ret
;; IF 402653184 END
.l2_402653184:
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
;; FUNCTION ADRESS
	mov rbx, _brk
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; IF 1610612736 START
	cmp al, 1
	jne .l2_1610612736
.l1_1610612736:
;; VALUE -1
	mov rax, -1
;; RETURN
	leave
	ret
;; IF 1610612736 END
.l2_1610612736:
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
;; IF 805306368 START
	cmp al, 1
	jne .l2_805306368
.l1_805306368:
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, _sbrk
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, _sbrk
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, ptr2arr
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET GLOBAL
	mov qword [global__m_tcache+0], rax
;; IF 805306368 END
.l2_805306368:
;; VALUE 0
	mov rax, 0
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3221225472 START
.loop_3221225472:
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
;; WHILE 3221225472 CHECK
	cmp al, 1
	jne .loop_3221225472_end
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
;; IF 38654705664 START
	cmp al, 1
	jne .l2_38654705664
.l1_38654705664:
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
;; IF 38654705664 END
.l2_38654705664:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3221225472 END
	jmp .loop_3221225472
.loop_3221225472_end:
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, _sbrk
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; WHILE 3221225472 START
.loop_3221225472:
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
;; WHILE 3221225472 CHECK
	cmp al, 1
	jne .loop_3221225472_end
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
;; IF 38654705664 START
	cmp al, 1
	jne .l2_38654705664
.l1_38654705664:
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
;; IF 2783138807808 START
	cmp al, 1
	jne .l2_2783138807808
.l1_2783138807808:
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
;; LESS
	cmp rax, rbx
	setl al
	mov al, al
;; IF 927712935936 START
	cmp al, 1
	jne .l2_927712935936
.l1_927712935936:
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
;; IF 5566277615616 START
	cmp al, 1
	jne .l2_5566277615616
.l1_5566277615616:
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
;; IF 5566277615616 END
.l2_5566277615616:
;; IF 927712935936 END
.l2_927712935936:
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
;; IF 1855425871872 START
	cmp al, 1
	jne .l2_1855425871872
.l1_1855425871872:
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
;; FUNCTION ADRESS
	mov rbx, _sbrk
;; POP
	pop rax
;; CALL PTR
	call rbx
;; IF 1855425871872 END
.l2_1855425871872:
;; RETURN
	leave
	ret
;; IF 38654705664 END
.l2_38654705664:
;; VALUE 1
	mov rbx, 1
;; GET VAR 16
	mov rax, [rbp-16]
;; ADD
	add rax, rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; WHILE 3221225472 END
	jmp .loop_3221225472
.loop_3221225472_end:
;; VALUE free: double free\n
	mov rax, str_free__double_free_n
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, print
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, _malloc
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rcx, set_addr
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, ptr2arr
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, _free
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; GET RETURN VALUE
	mov rbx, rax
;; PUSH
	push rbx
;; FUNCTION ADRESS
	mov rcx, min
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
;; SET VAR 32
	sub rsp, 32
	mov qword [rbp-32], rax
;; WHILE 51539607552 START
.loop_51539607552:
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
;; WHILE 51539607552 CHECK
	cmp al, 1
	jne .loop_51539607552_end
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
;; WHILE 51539607552 END
	jmp .loop_51539607552
.loop_51539607552_end:
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
;; FUNCTION ADRESS
	mov rcx, syscall1
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
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
;; FUNCTION ADRESS
	mov rbx, arr2addr
;; POP
	pop rax
;; CALL PTR
	call rbx
;; GET RETURN VALUE
	mov rcx, rax
;; PUSH
	push rcx
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; GET RETURN VALUE
	mov rdx, rax
;; PUSH
	push rdx
;; FUNCTION ADRESS
	mov rdi, syscall3
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdi
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
;; FUNCTION ADRESS
	mov rbx, arr2addr
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rdi, syscall3
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdi
;; RETURN
	leave
	ret
;; FUNCTION DECL eprint
eprint:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE 1
	mov rax, 1
;; PUSH
	push rax
;; VALUE 2
	mov rax, 2
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arr2addr
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rdi, syscall3
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdi
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
;; FUNCTION CALL __member__11876854719037224982_append
	call __member__11876854719037224982_append
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, print
;; POP
	pop rax
;; CALL PTR
	call rbx
;; RETURN
	leave
	ret
;; FUNCTION DECL eprintln
eprintln:
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
;; FUNCTION CALL __member__11876854719037224982_append
	call __member__11876854719037224982_append
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, eprint
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, syscall0
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION CALL __member__11876854719037224982_to_cstr
	call __member__11876854719037224982_to_cstr
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rdi, syscall3
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdi
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
;; FUNCTION ADRESS
	mov rbx, alloc_array
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 16
	sub rsp, 16
	mov qword [rbp-16], rax
;; VALUE /usr/bin/sh
	mov rax, str__usr_bin_sh
;; FUNCTION CALL __member__11876854719037224982_to_cstr
	call __member__11876854719037224982_to_cstr
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
;; FUNCTION CALL __member__11876854719037224982_to_cstr
	call __member__11876854719037224982_to_cstr
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
;; FUNCTION CALL __member__11876854719037224982_to_cstr
	call __member__11876854719037224982_to_cstr
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rax, fork
;; CALL PTR
	call rax
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
;; IF 422212465065984 START
	cmp al, 1
	jne .l2_422212465065984
.l1_422212465065984:
;; VALUE /usr/bin/sh
	mov rax, str__usr_bin_sh
;; PUSH
	push rax
;; GET VAR 16
	mov rax, [rbp-16]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rax, env
;; CALL PTR
	call rax
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rdx, execve
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdx
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, exit
;; POP
	pop rax
;; CALL PTR
	call rbx
;; IF 422212465065984 END
.l2_422212465065984:
;; WHILE 844424930131968 START
.loop_844424930131968:
;; VALUE true
	mov rax, 1
;; WHILE 844424930131968 CHECK
	cmp al, 1
	jne .loop_844424930131968_end
;; VALUE 61
	mov rax, 61
;; PUSH
	push rax
;; GET VAR 24
	mov rbx, [rbp-24]
;; PUSH
	push rbx
;; VALUE 0
	mov rcx, 0
;; PUSH
	push rcx
;; VALUE 0
	mov rdx, 0
;; PUSH
	push rdx
;; VALUE 0
	mov rdi, 0
;; PUSH
	push rdi
;; FUNCTION ADRESS
	mov rsi, syscall4
;; POP
	pop rdi
;; POP
	pop rdx
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rsi
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
;; LESS OR EQUAL
	cmp rax, rbx
	setle al
	mov al, al
;; IF 10133099161583616 START
	cmp al, 1
	jne .l2_10133099161583616
.l1_10133099161583616:
;; BREAK 844424930131968
	jmp .loop_844424930131968_end
;; IF 10133099161583616 END
.l2_10133099161583616:
;; VALUE 10
	mov rax, 10
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, sleep
;; POP
	pop rax
;; CALL PTR
	call rbx
;; WHILE 844424930131968 END
	jmp .loop_844424930131968
.loop_844424930131968_end:
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
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; PUSH
	push rax
;; VALUE 0
	mov rax, 0
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rdx, syscall2
;; POP
	pop rcx
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rdx
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__11876854719037224982_char_code
__member__11876854719037224982_char_code:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], al
;; GET VAR 1
	mov al, [rbp-1]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__13646096770106105413_to_char
__member__13646096770106105413_to_char:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, __unsafe_convert
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION CALL __member__11876854719037224982_char_code
	call __member__11876854719037224982_char_code
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; ADD
	add rax, rbx
;; FUNCTION CALL __member__13646096770106105413_to_char
	call __member__13646096770106105413_to_char
;; RETURN
	leave
	ret
;; FUNCTION DECL itoa
itoa:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; VALUE false
	mov rax, 0
;; SET VAR 9
	sub rsp, 9
	mov byte [rbp-9], al
;; GET VAR 8
	mov rax, [rbp-8]
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
;; IF 844424930131968 START
	cmp al, 1
	jne .l2_844424930131968
.l1_844424930131968:
;; VALUE true
	mov rax, 1
;; SET VAR 9
	sub rsp, 9
	mov byte [rbp-9], al
;; VALUE -1
	mov rax, -1
;; PUSH
	push rax
;; GET VAR 8
	mov rbx, [rbp-8]
;; POP
	pop rax
;; MUL
	imul rax, rbx
;; SET VAR 8
	sub rsp, 8
	mov qword [rbp-8], rax
;; IF 844424930131968 END
.l2_844424930131968:
;; VALUE 0
	mov rax, 0
;; SET VAR 17
	sub rsp, 17
	mov qword [rbp-17], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; SET VAR 25
	sub rsp, 25
	mov qword [rbp-25], rax
;; WHILE 6755399441055744 START
.loop_6755399441055744:
;; GET VAR 25
	mov rax, [rbp-25]
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
;; WHILE 6755399441055744 CHECK
	cmp al, 1
	jne .loop_6755399441055744_end
;; VALUE 10
	mov rbx, 10
;; GET VAR 25
	mov rax, [rbp-25]
;; DIV
	xor rdx, rdx
	mov rax, rax
	idiv rbx
;; SET VAR 25
	sub rsp, 25
	mov qword [rbp-25], rax
;; VALUE 1
	mov rbx, 1
;; GET VAR 17
	mov rax, [rbp-17]
;; ADD
	add rax, rbx
;; SET VAR 17
	sub rsp, 17
	mov qword [rbp-17], rax
;; WHILE 6755399441055744 END
	jmp .loop_6755399441055744
.loop_6755399441055744_end:
;; GET VAR 9
	mov al, [rbp-9]
;; IF 13510798882111488 START
	cmp al, 1
	jne .l2_13510798882111488
.l1_13510798882111488:
;; VALUE 1
	mov rbx, 1
;; GET VAR 17
	mov rax, [rbp-17]
;; ADD
	add rax, rbx
;; SET VAR 17
	sub rsp, 17
	mov qword [rbp-17], rax
;; IF 13510798882111488 END
.l2_13510798882111488:
;; GET VAR 17
	mov rax, [rbp-17]
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
;; FUNCTION ADRESS
	mov rbx, alloc_array
;; POP
	pop rax
;; CALL PTR
	call rbx
;; SET VAR 33
	sub rsp, 33
	mov qword [rbp-33], rax
;; GET VAR 17
	mov rax, [rbp-17]
;; SET VAR 41
	sub rsp, 41
	mov qword [rbp-41], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; SET VAR 49
	sub rsp, 49
	mov qword [rbp-49], rax
;; WHILE 216172782113783808 START
.loop_216172782113783808:
;; GET VAR 41
	mov rax, [rbp-41]
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
;; WHILE 216172782113783808 CHECK
	cmp al, 1
	jne .loop_216172782113783808_end
;; GET VAR 49
	mov rax, [rbp-49]
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
;; FUNCTION ADRESS
	mov rbx, digit_to_ascii
;; POP
	pop rax
;; CALL PTR
	call rbx
;; GET VAR 41
	mov rbx, [rbp-41]
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 33
	mov rcx, [rbp-33]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add al, bl
	pop rbx
	mov [rax], bl
;; VALUE 10
	mov rbx, 10
;; GET VAR 49
	mov rax, [rbp-49]
;; DIV
	xor rdx, rdx
	mov rax, rax
	idiv rbx
;; SET VAR 49
	sub rsp, 49
	mov qword [rbp-49], rax
;; VALUE 1
	mov rbx, 1
;; GET VAR 41
	mov rax, [rbp-41]
;; SUB
	sub rax, rbx
;; SET VAR 41
	sub rsp, 41
	mov qword [rbp-41], rax
;; WHILE 216172782113783808 END
	jmp .loop_216172782113783808
.loop_216172782113783808_end:
;; GET VAR 9
	mov al, [rbp-9]
;; IF 432345564227567616 START
	cmp al, 1
	jne .l2_432345564227567616
.l1_432345564227567616:
;; VALUE -
	mov rax, 45
;; VALUE 0
	mov rbx, 0
;; VALUE 1
	mov rcx, 1
;; MUL
	imul rbx, rcx
;; GET VAR 33
	mov rcx, [rbp-33]
;; SET ARRAY INDEX
	push rax
	mov rax, rcx
	add rax, 8
	add al, bl
	pop rbx
	mov [rax], bl
;; IF 432345564227567616 END
.l2_432345564227567616:
;; GET VAR 33
	mov rax, [rbp-33]
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__13646096770106105413_to_string
__member__13646096770106105413_to_string:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, itoa
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; IF 1688849860263936 START
	cmp al, 1
	jne .l2_1688849860263936
.l1_1688849860263936:
;; VALUE true
	mov rax, str_true
;; RETURN
	leave
	ret
;; IF 1688849860263936 END
.l2_1688849860263936:
;; VALUE false
	mov rax, str_false
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__11876854719037224982_len
__member__11876854719037224982_len:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, arrlen
;; POP
	pop rax
;; CALL PTR
	call rbx
;; RETURN
	leave
	ret
;; FUNCTION DECL __member__11876854719037224982_append
__member__11876854719037224982_append:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; FUNCTION ADRESS
	mov rbx, alloc_array
;; POP
	pop rax
;; CALL PTR
	call rbx
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
;; FUNCTION ADRESS
	mov rcx, strcpy
;; POP
	pop rbx
;; POP
	pop rax
;; CALL PTR
	call rcx
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; FUNCTION DECL __member__11876854719037224982_startsWith
__member__11876854719037224982_startsWith:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; IF 13510798882111488 START
	cmp al, 1
	jne .l2_13510798882111488
.l1_13510798882111488:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 13510798882111488 END
.l2_13510798882111488:
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
;; FUNCTION DECL __member__11876854719037224982_endsWith
__member__11876854719037224982_endsWith:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
	sub rsp, 9
	mov [rbp-9], bl
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; IF 27021597764222976 START
	cmp al, 1
	jne .l2_27021597764222976
.l1_27021597764222976:
;; VALUE false
	mov rax, 0
;; RETURN
	leave
	ret
;; IF 27021597764222976 END
.l2_27021597764222976:
;; GET VAR 8
	mov rax, [rbp-8]
;; FUNCTION CALL __member__11876854719037224982_len
	call __member__11876854719037224982_len
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
;; IF 54043195528445952 START
	cmp al, 1
	jne .l2_54043195528445952
.l1_54043195528445952:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 54043195528445952 END
.l2_54043195528445952:
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
;; IF 108086391056891904 START
	cmp al, 1
	jne .l2_108086391056891904
.l1_108086391056891904:
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; IF 108086391056891904 END
.l2_108086391056891904:
;; GET VAR 16
	mov rax, [rbp-16]
;; RETURN
	leave
	ret
;; FUNCTION DECL assert
assert:
	push rbp
	mov rbp,rsp
	sub rsp, 1
	mov [rbp-1], al
	sub rsp, 9
	mov [rbp-9], rbx
;; GET VAR 1
	mov al, [rbp-1]
;; PUSH
	push rax
;; NOT
	test al, al
	setz al
;; IF 216172782113783808 START
	cmp al, 1
	jne .l2_216172782113783808
.l1_216172782113783808:
;; VALUE Assertion failed: 
	mov rax, str_assertion_failed__
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, eprint
;; POP
	pop rax
;; CALL PTR
	call rbx
;; GET VAR 9
	mov rax, [rbp-9]
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, eprintln
;; POP
	pop rax
;; CALL PTR
	call rbx
;; VALUE 1
	mov rax, 1
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, exit
;; POP
	pop rax
;; CALL PTR
	call rbx
;; IF 216172782113783808 END
.l2_216172782113783808:
;; RETURN
	leave
	ret
;; FUNCTION DECL id
id:
	push rbp
	mov rbp,rsp
	sub rsp, 8
	mov [rbp-8], rax
;; GET VAR 8
	mov rax, [rbp-8]
;; RETURN
	leave
	ret
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; VALUE 1
	mov rax, 1
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, id
;; POP
	pop rax
;; CALL PTR
	call rbx
;; FUNCTION CALL __member__13646096770106105413_to_string
	call __member__13646096770106105413_to_string
;; PUSH
	push rax
;; FUNCTION ADRESS
	mov rbx, println
;; POP
	pop rax
;; CALL PTR
	call rbx
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
str_true:
	dq 4
	db `true`
str_false:
	dq 5
	db `false`
str_assertion_failed__:
	dq 18
	db `Assertion failed: `
global__m_tcache:
	resb 8
global__m_location:
	resb 8
global__m_heap_start:
	resb 8
