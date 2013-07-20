/************************************************************************************************************************
 * Find out if a random string can be arranged (anagram) into a  palindrome.
 *
 * Codility programming test
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>

int solution (char *s) {
    unsigned char histogram[UCHAR_MAX] = { 0 };
    int i = 0, odds = 0;

    for ( ; *s != '\0'; s++)
        histogram[*s]++;

    // if the count of this character is odd...
    for (i = 0; i < UCHAR_MAX; i++) {
        if (histogram[i] & 0x1)
            odds++;
    }

    // If we have zero or one odd histogram count this can be a palindrome.
    if (odds <= 1) {
        return 1;
    }

    return 0;
}

int main (void) {
    printf("%i\n", solution("helloXX|hello"));
    return 0;
}
