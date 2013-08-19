/************************************************************************************************************************
 * Simple application that tries to execute machine code from many different memory locations.
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <signal.h>
#include <errno.h>

// Execute the given function pointer.
#define EXECUTE_FUNC(FROM, FUNC) \
    printf("Executing machine code from "FROM": "); \
    (FUNC)();                          \
    printf("Executed successfully.\n");


/****************************************************************************************************
 * Machine code that returns the stack pointer upon entry on the function.
 * Since this is defined globally and it is initialized it will end up in our data segment.
 ****************************************************************************************************/
unsigned char code[] = {
    0x48,         // 64 bit operand size prefix
    0x89, 0xe0,   // mov %rsp, %rax
    0xc3          // ret
};

/****************************************************************************************************
 * This is defined globally but it is NOT initialized so it will end up in our bss segment.
 ****************************************************************************************************/
unsigned char bss_code[sizeof(code)];

/* Segmentation fault handler */
void sigsegv_handler (int sig) {
    printf("SIGSEGV recieved.\n");
    exit(0);
}

/* Data bus error handler (usually occurs when accessing unmapped addressing space) */
void sigbus_handler (int sig) {
    printf("SIGBUS recieved.\n");
    exit(0);
}

/* Execute code off the data segment. */
void execute_from_data_segment () {
    EXECUTE_FUNC("data segment", (unsigned long (*)()) code);
}

/* Execute code off the bss segment. */
void execute_from_bss_segment () {
    memcpy(bss_code, code, sizeof(code));
    EXECUTE_FUNC("bss segment", (unsigned long (*)()) bss_code);
}

/* Execute code off the stack. */
void execute_from_stack () {
    unsigned char stack_code[sizeof(code)];

    memcpy(stack_code, code, sizeof(code));

    EXECUTE_FUNC("stack", (unsigned long (*)()) stack_code);
}

/* Execute code off malloc'd memory. */
void execute_from_malloc () {
    char *ptr = malloc(sizeof(code));

    memcpy(ptr, code, sizeof(code));

    EXECUTE_FUNC("malloc memory", (unsigned long (*)()) ptr);
}

/* Execute code off mmap'd memory with write permissions. */
void execute_from_mmap_write () {
    char *ptr = mmap(NULL, sizeof(code), PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

    memcpy(ptr, code, sizeof(code));

    EXECUTE_FUNC("mmap (write) memory", (unsigned long (*)()) ptr);
}

/* Execute code off mmap'd memory with execute permissions. */
void execute_from_mmap_exec () {
    char *ptr = mmap(NULL, sizeof(code), PROT_WRITE | PROT_EXEC, MAP_ANON | MAP_PRIVATE, -1, 0);

    memcpy(ptr, code, sizeof(code));

    EXECUTE_FUNC("mmap (write|exec) memory", (unsigned long (*)()) ptr);
}

int main (int argc, char *argv[]) {

    /* Trap invalid memory related signals. */
    signal(SIGSEGV, sigsegv_handler);
    signal(SIGBUS, sigbus_handler);

    if (argc < 2) {
        printf("Usage: %s [data|bss|stack|malloc|mmap-write|mmap-exec]\n", argv[0]);
        exit(1);
    }

    if (!strcmp("data", argv[1]))
        execute_from_data_segment();
    else if (!strcmp("bss", argv[1]))
        execute_from_bss_segment();
    else if (!strcmp("stack", argv[1]))
        execute_from_stack();
    else if (!strcmp("malloc", argv[1]))
        execute_from_malloc();
    else if (!strcmp("mmap-write", argv[1]))
        execute_from_mmap_write();
    else if (!strcmp("mmap-exec", argv[1]))
        execute_from_mmap_exec();
    else {
        printf("Usage: %s [data|bss|stack|malloc|mmap-write|mmap-exec]\n", argv[0]);
        exit(1);
    }

    return 0;
}
