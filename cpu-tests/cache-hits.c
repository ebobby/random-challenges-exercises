/************************************************************************************************************************
 * Test cache hit/misses performance
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "timers.h"

#define CACHE_LINE_SIZE 32
#define NUM_OPS         ((unsigned long) 5242880)
#define PASSES          5
#define ARRAY_SIZE      64L * NUM_OPS

char memory_block[ARRAY_SIZE];

#define LOOP_ARRAY(start, ops, inc)               \
    for (unsigned long i = start; i < ops * inc; i += inc)      \
        memory_block[i] <<= 1;

#define PRINT_RESULTS(count, index, elapsed)                            \
    printf("%lu memory accesses, index %i, microseconds elapsed: %lu\n", \
           count,                                                       \
           index,                                                       \
           elapsed)

int main (void) {

    CREATE_CLOCK(cache_hits);

    for (int j = 0; j < PASSES; j++) {
        printf("\n\nPass #%i:\n", j);

        // Just in case
        for (unsigned long i = 0; i < ARRAY_SIZE; i++) memory_block[i] = -1;
        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 1);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 1, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 2);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 2, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 4);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 4, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 8);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 8, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 16);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 16, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 32);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 32, CLOCK_MICROSECS_ELAPSED(cache_hits));

        START_CLOCK(cache_hits);
        LOOP_ARRAY(0, NUM_OPS, 64);
        STOP_CLOCK(cache_hits);
        PRINT_RESULTS(NUM_OPS, 64, CLOCK_MICROSECS_ELAPSED(cache_hits));

    }

    return 0;
}
