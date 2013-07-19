/******************************************************************************************************************************************************
 * Bit vector set sort
 *
 * Exercise 1.3 from Programming Pearls
 *
 * Francisco Soto <ebobby@ebobby.org>
 ******************************************************************************************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "bitvectorset.h"

#define NUMBER_FILE "numbers.txt"
#define SORTED_FILE "set-sorted.txt"
#define SET_MAX     100000000
#define MAX_ITEMS   10000000

int main (void) {
    FILE *fp = fopen(NUMBER_FILE, "r");
    unsigned int i,  number;
    bit_vector_set *set = bit_vector_set_create(SET_MAX);

    if (fp == NULL) {
        printf("File %s does not exist.\n", NUMBER_FILE);
        return -1;
    }

    for (i = 0; i < MAX_ITEMS; i++) {
        fscanf(fp, "%u", &number);
        bit_vector_set_add(set, number);
    }

    fclose(fp);

    fp = fopen(SORTED_FILE, "w");

    if (fp == NULL) {
        printf("Cannot create file %s.\n", SORTED_FILE);
        return -1;
    }

    for (i = 1; i <= SET_MAX; i++) {
        if (bit_vector_set_is_element_of(set, i)) {
            fprintf(fp, "%u\n", i);
        }
    }

    fclose(fp);

    bit_vector_set_destroy(set);

    return 0;
}
