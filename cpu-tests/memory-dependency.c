/************************************************************************************************************************
 * Test memory dependency
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "timers.h"

#define STEPS 1024*1024*64

int memory_block[2];

int main (void) {
    int i;

    CREATE_CLOCK(memory_dependency);

    START_CLOCK(memory_dependency);
    for (i = 0; i < STEPS; i++) { memory_block[0]++; memory_block[0]++; }
    STOP_CLOCK(memory_dependency);
    printf("same address, microseconds elapsed: %lu\n", CLOCK_MICROSECS_ELAPSED(memory_dependency));

    START_CLOCK(memory_dependency);
    for (i = 0; i < STEPS; i++) { memory_block[0]++; memory_block[1]++; }
    STOP_CLOCK(memory_dependency);
    printf("next address, microseconds elapsed: %lu\n", CLOCK_MICROSECS_ELAPSED(memory_dependency));

    return 0;
}
