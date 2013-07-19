/******************************************************************************************************************************************************
 * Set bitmap structure
 *
 * Exercise 1.2 from Programming Pearls
 *
 * Francisco Soto <ebobby@ebobby.org>
 ******************************************************************************************************************************************************/

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include "bitvectorset.h"

#define SET_SIZE 10000000000

int main (void) {
    bit_vector_set *set = bit_vector_set_create(SET_SIZE);
    unsigned long i;

    if (set == NULL) {
        printf("No memory for set.\n");
        return -1;
    }

    srand(time(NULL));

    for (i = 0; i < rand()%10000; i++)
        bit_vector_set_add(set, rand()%SET_SIZE);

    for (i = 1; i <= SET_SIZE; i++) {
        if (bit_vector_set_is_element_of(set, i)) {
            printf("%lu is element of the set.\n", i);
            bit_vector_set_remove(set, i);
        }
    }
    printf("\nShould be empty now:");

    for (i = 1; i <= SET_SIZE; i++) {
        if (bit_vector_set_is_element_of(set, i)) {
            printf("%lu is element of the set.\n", i);
            bit_vector_set_remove(set, i);
        }
    }
    printf("\n");

    bit_vector_set_destroy(set);

    return 0;
}
