// because Windows
usleep(us) {
    __asm__(
        "ldarg.0",
        "conv.i4",
        "ldc.i4 1000",
        "div",
        "call void [mscorlib]System.Threading.Thread::Sleep(int32)",
        "ldc.i8 0",
        "ret"
    );
}

char(s,n) {
    __asm__(
        "ldarg 0",
        "ldarg 1",
        "add",
        "ldind.i1",
        "conv.i8",
        "ret"
    );
}

extrn printf;
__variadic__(printf, 1);
extrn putchar;
extrn getchar;
extrn exit;