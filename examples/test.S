/*
    https://stackoverflow.com/questions/25431095/how-to-implement-system-call-in-arm64
    https://eastrivervillage.com/Anatomy-of-Linux-system-call-in-ARM64/
    https://reverseengineering.stackexchange.com/questions/16917/arm64-syscalls-table
*/

.global _start
_start:

    mov x0, #1
    adr x1, msg
    mov x2, len
    mov x8, #64 // write()
    svc #0

    mov x0, #0
    mov x8, #93 // exit()
    svc #0
msg:
    .ascii "Hello World!\n"
len = . - msg
