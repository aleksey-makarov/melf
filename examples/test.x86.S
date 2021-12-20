/*
    https://en.wikibooks.org/wiki/X86_Assembly/Interfacing_with_Linux
*/

.global _start
_start:
    movq $1, %rax   // use the `write` [fast] syscall
    movq $1, %rdi   // write to stdout
    lea msg(%rip), %rsi // use string "Hello World"
    movq $len, %rdx // write 12 characters
    syscall         // make syscall
    movq $60, %rax  // use the `_exit` [fast] syscall
    movq $0, %rdi   // error code 0
    syscall         // make syscall

msg:
    .ascii "Hello World!\n"
len = . - msg
