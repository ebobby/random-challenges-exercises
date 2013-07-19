/******************************************************************************************************************************************************
 * Set bitmap structure
 *
 * Exercise 1.2 from Programming Pearls
 *
 * Francisco Soto <ebobby@ebobby.org>
 ******************************************************************************************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define BIT_VECTOR_SET_BITS_PER_ELEMENT    32
#define BIT_VECTOR_SET_SHIFT               5
#define BIT_VECTOR_SET_POSITION(EL)        (EL >> 5)
#define BIT_VECTOR_SET_SET_BIT_MASK(EL)    (1 << (EL & 0x1F))
#define BIT_VECTOR_SET_UNSET_BIT_MASK(EL) ~(1 << (EL & 0x1F))

typedef struct {
    uint32_t *bitmap;  // bit vector
    unsigned long n;    // number of elements in the set universe
} bit_vector_set;

bit_vector_set *bit_vector_set_create (unsigned long elements) {
    bit_vector_set *set = (bit_vector_set *)malloc(sizeof(bit_vector_set));
    int size;

    if (set == NULL)
        return NULL;

    size = sizeof(uint32_t) * (BIT_VECTOR_SET_POSITION(elements) + 1);
    set->bitmap = (uint32_t *)malloc(size);
    memset(set->bitmap, 0, size);

    if (set->bitmap == NULL) {
        free(set);
        return NULL;
    }

    set->n = elements;

    return set;
}

void bit_vector_set_destroy (bit_vector_set *set) {
    if (set == NULL)
        return;
    free(set->bitmap);
    free(set);
}

void bit_vector_set_add (bit_vector_set *set, unsigned long element) {
    if (element > set->n)
        return;
    element--;
    set->bitmap[BIT_VECTOR_SET_POSITION(element)] |= BIT_VECTOR_SET_SET_BIT_MASK(element);
}

void bit_vector_set_remove (bit_vector_set *set, unsigned long element) {
    if (element > set->n)
        return;
    element--;
    set->bitmap[BIT_VECTOR_SET_POSITION(element)] &= BIT_VECTOR_SET_UNSET_BIT_MASK(element);
}

int bit_vector_set_is_element_of (bit_vector_set *set, unsigned long element) {
    if (element > set->n)
        return 0;
    element--;
    return set->bitmap[BIT_VECTOR_SET_POSITION(element)] & BIT_VECTOR_SET_SET_BIT_MASK(element);
}
