/*
    https://stackoverflow.com/questions/25431095/how-to-implement-system-call-in-arm64
    https://eastrivervillage.com/Anatomy-of-Linux-system-call-in-ARM64/
    https://reverseengineering.stackexchange.com/questions/16917/arm64-syscalls-table/18834
*/

.global _start
_start:
    mov x0, #42
    mov x8, #93
    svc #0
