.global _start
_start:

    mov x0, #0
    mov x1, #0
l1:
    mov x2, #0
    mov x3, #0
l2:
    mov x4, #0
    mov x5, #0

    mov x0, #0
    mov x8, #93 // exit()
    svc #0
