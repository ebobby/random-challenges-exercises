/************************************************************************************************************************
 * Given an array of N elements find an index where the sum of all elements up to that index is the same as the sum of
 * all elements after that index.
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int solution(int A[], int N) {
    long long *forwardSum, *backwardSum;
    int i, result = -1;

    if (N == 0)
        return -1;

    if (N == 1)
        return 0;

    forwardSum  = (long long *)malloc(sizeof(long long) * N);
    backwardSum = (long long *)malloc(sizeof(long long) * N);

    forwardSum[0] = backwardSum[0] = A[0];

    for (i = 1; i < N; i++) {
        backwardSum[i] = A[i];
        forwardSum[i] = A[i] + forwardSum[i - 1];
    }

    for (i = N - 2; i >= 0; i--)
        backwardSum[i] += backwardSum[i + 1];

    /* if the backward sum of all elements up to the first index is 0 then 0 is a correct
       answer because the forward sum up to index 0 is 0 because there are no elements. */
    if (backwardSum[1] == 0)
        result = 0;

    /* if the forward sum of all elements up to the last index is 0 then the last index is correct
       answer because the backward sum up to the last index is 0 because there are no elements. */
    if (forwardSum[N - 2] == 0)
        result = N - 1;

    if (result == -1) {
        for (i = 1; i < N-1; i++) {
            if (forwardSum[i - 1] == backwardSum[i + 1]) {
                result = i;
                break;
            }
        }
    }

    free(forwardSum);
    free(backwardSum);

    return result;
}

int main (void) {
    int a[] = { -7, 1, 5, 2, -4, 3, 0 };

    printf("Solution %d\n", solution(a, 7));

    return 0;
}
