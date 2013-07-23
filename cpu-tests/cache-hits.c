/************************************************************************************************************************
 * Test cache hit/misses performance
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "timers.h"

#define ARRAY_SIZE 64*1024*1024

int memory_block[ARRAY_SIZE];

int main (void) {
    int i;

    CREATE_CLOCK(cache_hits);

    // Just in case
    for (i = 0; i < ARRAY_SIZE; i++) memory_block[i] = 8;

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 1) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 1, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 2) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 2, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 4) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 4, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 8) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 8, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 16) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 16, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 32) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 32, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 64) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 64, CLOCK_MICROSECS_ELAPSED(cache_hits));

    START_CLOCK(cache_hits);
    for (i = 0; i < ARRAY_SIZE; i += 64) memory_block[i] *= 3;
    STOP_CLOCK(cache_hits);
    printf("increment %i, microseconds elapsed: %lu\n", 64, CLOCK_MICROSECS_ELAPSED(cache_hits));

    return 0;
}
