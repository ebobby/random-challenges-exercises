/************************************************************************************************************************
 * Flattens a list of strings with the same signature into the same line.
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

int main (int argc, char *argv[]) {
    char str[MAX_LEN+1], signature[MAX_LEN+1], previous[MAX_LEN+1] = { 0 };

    while (scanf("%"MAX_LEN_WIDTH_FORMAT"s %"MAX_LEN_WIDTH_FORMAT"s", signature, str) != EOF) {
        if (strcmp(signature, previous) != 0) {
            memcpy(previous, signature, sizeof(previous));
            printf("\n");
        }

        printf("%s ", str);
    }
    printf("\n");

    return 0;
}
