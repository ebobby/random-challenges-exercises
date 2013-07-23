/************************************************************************************************************************
 * Test branch prediction performance
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "timers.h"

#define STEPS 1024*1024*512

int main (void) {
    int i, sum, j;

    CREATE_CLOCK(branch_prediction);

    START_CLOCK(branch_prediction);
    for (i = 0; i < STEPS; i++) { if ((i & 0x80000000) == 0) sum++; }
    STOP_CLOCK(branch_prediction);
    printf("i & 0x80000000 = 0 elapsed:\t %lu\n", CLOCK_MICROSECS_ELAPSED(branch_prediction));

    START_CLOCK(branch_prediction);
    for (i = 0; i < STEPS; i++) { if ((i & 0xffffffff) == 0) sum++; }
    STOP_CLOCK(branch_prediction);
    printf("i & 0xffffffff = 0 elapsed:\t %lu\n", CLOCK_MICROSECS_ELAPSED(branch_prediction));

    START_CLOCK(branch_prediction);
    for (i = 0; i < STEPS; i++) { if ((i & 3) == 0) sum++; }
    STOP_CLOCK(branch_prediction);
    printf("i & 3 = 0 elapsed:\t %lu\n", CLOCK_MICROSECS_ELAPSED(branch_prediction));

    for (j = 1; j <= 16; j <<= 1) {
        START_CLOCK(branch_prediction);
        for (i = 0; i < STEPS; i++) { if ((i & j) == 0) sum++; }
        STOP_CLOCK(branch_prediction);
        printf("i & %d = 0 elapsed:\t\t %lu\n", j, CLOCK_MICROSECS_ELAPSED(branch_prediction));
    }

    return 0;
}
