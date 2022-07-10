global _start
section .text
_start:
	push rbp
	mov rbp, rsp
;; FUNCTION CALL main
	call main
;; JUMP
	jmp _end
;; FUNCTION DECL main
main:
	push rbp
	mov rbp,rsp
;; VALUE 0
	mov rax, 0
;; SET VAR 4
	sub rsp, 4
	mov dword [rbp-4], eax
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
