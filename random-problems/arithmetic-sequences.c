/************************************************************************************************************************
 * Count the number of slices where the difference between neighbors is the same ({0, 2, 4}) in the given array.
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int solution (int A[], int N) {
    int result = 0, i;
    long long difference, previous = -1, current = 0;

    // No arithmetic sequence possible
    if (N < 3) {
        return 0;
    }

    for (i = 0; i < N - 1; i++) {
        // get the absolute value of the difference
        difference = A[i] - A[i + 1];

        if (difference < 0)
            difference *= -1;

        // we are on a roll
        if (difference == previous) {
            current++;
        }
        else {
            // The number of different slices is the summation of the number of contiguous differences minus one
            if (current != 0) {
                current--;
                result += (current * (current + 1)) >> 1;
            }

            current = 1; // reset current
            previous = difference;
        }

        // If the result is already larger there is no point in calculating the rest.
        if (result > 1000000000)
            return -1;
    }

    // process the last slice if any
    if (current != 0) {
        current--;
        result += (current * (current + 1)) >> 1;
    }

    if (result > 1000000000)
        return -1;

    return result;
}

int main (void) {
//    int A[] =  { -1, 1, 3, 3, 3, 2, 1, 0 };
    int A[] =  { 0, 1, 2, 3, 4, 50, 0, 1, 2, 3, 4 };

    printf("%i\n", solution(A, 11));

    return 0;
}
