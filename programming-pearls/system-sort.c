/************************************************************************************************************************
 * System sort
 *
 * Exercise 1.3 from Programming Pearls
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#define NUMBER_FILE "numbers.txt"
#define SORTED_FILE "system-sorted.txt"
#define SET_MAX     100000000
#define MAX_ITEMS   10000000

unsigned int item_list[MAX_ITEMS];

int compare (const void *a, const void *b) {
    return *((int *) a) - *((int *) b);
}

int main (void) {
    FILE *fp = fopen(NUMBER_FILE, "r");
    int i;

    if (fp == NULL) {
        printf("File %s does not exist.\n", NUMBER_FILE);
        return -1;
    }

    for (i = 0; i < MAX_ITEMS; i++) {
        fscanf(fp, "%u", &item_list[i]);
    }

    fclose(fp);

    qsort(&item_list, MAX_ITEMS, sizeof(unsigned int), &compare);

    fp = fopen(SORTED_FILE, "w");

    if (fp == NULL) {
        printf("Cannot create file %s.\n", SORTED_FILE);
        return -1;
    }

    for (i = 0; i < MAX_ITEMS; i++) {
        fprintf(fp, "%u\n", item_list[i]);
    }

    fclose(fp);

    return 0;
}
