/************************************************************************************************************************
 * Given a range of numbers (inclusive) how many exact (integer) cubes are there in it?
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

int solution (int A, int B) {
    int i, result = 0,
        first  = (int)cbrt(A),
        cube   = first * first * first;

    if (cube < A) {
        first++;
        cube = first * first * first;
    }

    for (i = cube; i <= B; result++) {
        first++;
        i = first * first * first;
    }

    return result;
}

int main (void) {
    // -100^3 to 100^3 so there are 201 cubes. (0 is a cube as well).
    printf("%d\n", solution(-1000000, 1000000));
    return 0;
}
