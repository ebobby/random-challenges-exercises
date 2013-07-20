/************************************************************************************************************************
 * Signs strings so finding an anagram is easie. The signature is the string with the characters sorted.
 *
 * Programming Pearls
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 1000
#define MAX_LEN_WIDTH_FORMAT "1000"

int char_compare (const void *c1, const void *c2) {
    return *((char*)c1) - *((char *) c2);
}

int main (int argc, char *argv[]) {
    char str[MAX_LEN+1], signature[MAX_LEN+1];

    while (scanf("%"MAX_LEN_WIDTH_FORMAT"s", str) != EOF) {
        memcpy(signature, str, sizeof(str));
        qsort(signature, strlen(signature) - 1, sizeof(char), &char_compare);
        printf("%s %s\n", signature, str);
    }

    return 0;
}
