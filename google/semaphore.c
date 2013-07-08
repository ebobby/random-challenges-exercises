////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// semaphore implementation
//
// Practice for Google's interviews
//
// Francisco Soto <ebobby@ebobby.org>
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/time.h>

#include "semaphore.h"

void *thread_function (void *thread_data) {
    int id = *((int *)thread_data);

    while (1) {
        semaphore_acquire(&semaphore);

        printf("In thread %d.\n", id);
        sleep(1); // simulate a long running thread.

        semaphore_release(&semaphore);
    }

    pthread_exit(NULL);
}

int main (void) {
    int ids[NUM_THREADS], i;

    semaphore_init(&semaphore, NUM_THREADS>>1);

    // Create the threads.
    for (i = 0; i < NUM_THREADS; i++) {
        ids[i] = i;
        pthread_create(&threads[i], NULL, thread_function, &ids[i]);
    }

    // We go offline for a bit...
    sleep(10);

    semaphore_destroy(&semaphore);

    // Return and let the other threads die.
    return 0;
}
