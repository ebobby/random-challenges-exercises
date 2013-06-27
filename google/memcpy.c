#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void naivememcpy (void *dst, void *src, size_t n) {
    uint8_t *d =(uint8_t *)dst, *s = (uint8_t *)src;
    int i;

    for (i = 0; i < n; i++) {
        *(d++) = *(s++);
    }
}

void fastmemcpy (void *dst, void *src, size_t n) {
    uint8_t *byte_d =(uint8_t *)dst, *byte_s = (uint8_t *)src, byte;
    uint64_t constructed = 0, buffer = 0, *dword_d, *dword_s;
    int i, j, read = 0, buffered = 0, written = 0;
    long tmp;

    if (n < 8) {
        for (i = 0; i < n; i++) {
            *(byte_d++) = *(byte_s++);
            written++;
        }
    }
    else {
        tmp = ((long) byte_s) & 0x7;
        for (i = 0; i < tmp; i++) {
            constructed += (*byte_s & 0xFF) << (read << 3);
            read        += 1;
            byte_s      += 1;
            n--;
        }
        dword_s = (uint64_t *)byte_s; // now read pointer is aligned.

        tmp = ((long) byte_d) & 0x7;
        for (i = 0; i < tmp; i++) {
            if (read == 0) {
                if (n < 8) {
                    tmp = n;
                    for (i = 0; i < tmp; i++) {
                        constructed += (*byte_s & 0xFF) << (read << 3);
                        read        += 1;
                        byte_s      += 1;
                        n--;
                    }
                    dword_s = (uint64_t *)byte_s;
                }
                else {
                    constructed = *dword_s;
                    read        = 8;
                    dword_s    += 1;
                    n -= 8;
                }
            }

            byte = constructed & 0xFF;
            constructed = constructed >> 8;
            read--;
            *(byte_d++) = byte;
            written += 1;
        }
        dword_d = (uint64_t *)byte_d; // write pointer is aligned.

        while (n >= 8) {
            if (read < 8) {
                tmp = 8 - read;
                for (j = 0; j < tmp; j++) {
                    if (buffered == 0) {
                        buffer   = *dword_s;
                        buffered = 8;
                        dword_s += 1;
                        n -= 8;
                    }

                    constructed += (buffer & 0xFF) << (read << 3);
                    buffer = buffer >> 8;
                    read++;
                    buffered--;
                }
            }
            *(dword_d++) = constructed;
            written += 8;
            constructed = 0;
            read = 0;
        }

        byte_s = (uint8_t *)dword_s;
        byte_d = (uint8_t *)dword_d;

        tmp = read;
        for (i = 0; i < tmp; i++) {
            *(byte_d++) = constructed & 0xFF;
            constructed = constructed >> 8;
            read--;
            written += 1;
        }

        tmp = buffered;
        for (i = 0; i < tmp; i++) {
            *(byte_d++) = buffer & 0xFF;
            buffer = buffer >> 8;
            written += 1;
            buffered--;
        }

        for (i = 0; i < n; i++) {
            *(byte_d++) = *(byte_s++);
            written += 1;
        }
    }
}

typedef int bool;

#define HOWMANY 412345
#define TRUE  1
#define FALSE 0

int main (void) {
    int i = 0, j = 0;
    unsigned long stuff1[HOWMANY] = {  };
    unsigned long stuff2[HOWMANY] = {  };
    bool equals = TRUE;

    for (i = 0; i < 5000; i++) {
        stuff1[i] = i + 1;
    }
    for (j = 0; j < 1000; j++) {
        fastmemcpy(&stuff2, &stuff1, sizeof(stuff1));
    }

    return 0;
}
