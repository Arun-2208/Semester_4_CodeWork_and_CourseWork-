
/*
 * cwe121_showcase.c
 *
 * One-file demo of two classic memory-safety bugs:
 *  1) memcpy overrun in a struct field (bad vs good)
 *  2) array index without upper-bound check (bad vs good)
 *
 * Build (recommended, with sanitizers):
 *   gcc -O0 -g -Wall -Wextra -Wshadow -Wconversion -fno-omit-frame-pointer \
 *       -fsanitize=address,undefined cwe121_showcase.c -o cwe121_showcase
 *
 * Run examples:
 *   ./cwe121_showcase memcpy-bad
 *   ./cwe121_showcase memcpy-good
 *   ./cwe121_showcase index-bad 10
 *   ./cwe121_showcase index-good 10
 *
 * Notes:
 * - AddressSanitizer will loudly report overflows on the 'bad' cases when triggered.
 * - On some platforms, you may need to use clang:  CC=clang (macOS).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ---------- Case 1: memcpy overrun inside struct ---------- */
typedef struct {
    char  charFirst[16];
    void *voidSecond;
    void *voidThird;
} CharVoid;

static void print_struct(const CharVoid *s, const char *label) {
    printf("[%s] charFirst=\"%s\"  voidSecond=%p  voidThird=%p\n",
           label, s->charFirst, s->voidSecond, s->voidThird);
}

/* BAD: Copies sizeof(CharVoid) bytes into charFirst[16], overrunning into the pointers */
static void demo_memcpy_bad(void) {
    puts("== memcpy BAD demo ==");
    CharVoid s;
    memset(&s, 0, sizeof(s));

    /* Set pointers to known values so we can observe corruption if it happens */
    const char *message = "ORIGINAL_POINTER_DATA";
    s.voidSecond = (void*)message;
    s.voidThird  = (void*)(message + 1);

    print_struct(&s, "before");

    /* Prepare a large enough source so memcpy doesn't read past SRC */
    char src[64];
    memset(src, 'A', sizeof(src));
    src[sizeof(src)-1] = '\0';

    /* BUG: size equals sizeof(CharVoid), but destination is only 16 bytes */
    memcpy(s.charFirst, src, sizeof(CharVoid));

    /* In the bad case, AddressSanitizer should report a stack-buffer-overflow here.
       Even if it continues, our pointers likely got clobbered. */
    print_struct(&s, "after");
}

/* GOOD: Copies only what fits into charFirst[], and NUL-terminates */
static void demo_memcpy_good(void) {
    puts("== memcpy GOOD demo ==");
    CharVoid s;
    memset(&s, 0, sizeof(s));
    const char *message = "ORIGINAL_POINTER_DATA";
    s.voidSecond = (void*)message;
    s.voidThird  = (void*)(message + 1);

    print_struct(&s, "before");

    const char *SRC = "SAFE_COPY_DEMO";
    /* Correct: limit by destination field size minus 1, then NUL-terminate */
    size_t dstsz = sizeof(s.charFirst);
    size_t n = strlen(SRC);
    if (n >= dstsz) n = dstsz - 1;
    memcpy(s.charFirst, SRC, n);
    s.charFirst[n] = '\0';

    print_struct(&s, "after");
}

/* ---------- Case 2: array index without proper bounds ---------- */
static void dump_buffer(const int *buf, size_t n) {
    fputs("buffer: [", stdout);
    for (size_t i = 0; i < n; i++) {
        printf("%s%d", (i ? ", " : ""), buf[i]);
    }
    puts("]");
}

/* BAD: checks only lower bound; any idx >= 10 will write out of bounds */
static void demo_index_bad(int idx) {
    puts("== index BAD demo ==");
    int buffer[10] = {0};
    dump_buffer(buffer, 10);

    if (idx >= 0) { /* BUG: no upper bound check */
        printf("Writing buffer[%d] = 1 (BAD path)\n", idx);
        buffer[idx] = 1; /* OOB write if idx >= 10 */
    } else {
        puts("Index is negative; skipping write.");
    }

    dump_buffer(buffer, 10);
}

/* GOOD: checks both 0 <= idx < 10 before writing */
static void demo_index_good(int idx) {
    puts("== index GOOD demo ==");
    int buffer[10] = {0};
    dump_buffer(buffer, 10);

    if (idx >= 0 && idx < 10) {
        printf("Writing buffer[%d] = 1 (GOOD path)\n", idx);
        buffer[idx] = 1;
    } else {
        printf("Index %d out of range [0, 9]; not writing.\n", idx);
    }

    dump_buffer(buffer, 10);
}

/* ---------- CLI harness ---------- */
static void usage(const char *prog) {
    fprintf(stderr,
        "Usage:\n"
        "  %s memcpy-bad\n"
        "  %s memcpy-good\n"
        "  %s index-bad <n>\n"
        "  %s index-good <n>\n"
        "\n"
        "Examples:\n"
        "  %s memcpy-bad\n"
        "  %s memcpy-good\n"
        "  %s index-bad 10     # triggers out-of-bounds write\n"
        "  %s index-good 10    # safe guarded write\n",
        prog, prog, prog, prog, prog, prog, prog, prog
    );
}

int main(int argc, char **argv) {
    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }
    if (strcmp(argv[1], "memcpy-bad") == 0) {
        demo_memcpy_bad();
        return 0;
    }
    if (strcmp(argv[1], "memcpy-good") == 0) {
        demo_memcpy_good();
        return 0;
    }
    if ((strcmp(argv[1], "index-bad") == 0 || strcmp(argv[1], "index-good") == 0)) {
        if (argc < 3) {
            fputs("Please provide an integer index.\n", stderr);
            usage(argv[0]);
            return 1;
        }
        char *end = NULL;
        long val = strtol(argv[2], &end, 10);
        if (!end || *end != '\0') {
            fputs("Index must be an integer.\n", stderr);
            return 1;
        }
        int idx = (int)val;
        if (strcmp(argv[1], "index-bad") == 0) {
            demo_index_bad(idx);
        } else {
            demo_index_good(idx);
        }
        return 0;
    }
    usage(argv[0]);
    return 1;
}
