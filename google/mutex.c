////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// simple mutex implementation using peterson's algorithm
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
#include <pthread.h>

typedef struct {
    int interested[2];
    int victim;
} peterson_lock_t;

pthread_t threads[2];
peterson_lock_t mutex;

void peterson_lock_init (peterson_lock_t *lock) {
    lock->interested[0] = 0;
    lock->interested[1] = 0;
    lock->victim = -1;
}

void peterson_lock_grab (int id, peterson_lock_t *lock) {
    int other = 1 - id;           // who's the other thread

    lock->interested[id] = 1;     // we are interested
    lock->victim = id;            // the magic to prevent race conditions

    while (lock->victim == id &&
           lock->interested[other] == 1); // wait for our turn.
}

void peterson_lock_release (int id, peterson_lock_t *lock) {
    lock->interested[id] = 0;
}

void *thread_function (void *thread_data) {
    int id = *((int *)thread_data);
    while (1) {
        peterson_lock_grab(id, &mutex);

        printf("In thread %d.\n", id);
        sleep(1); // simulate a long running thread.

        peterson_lock_release(id, &mutex);
    }

    pthread_exit(NULL);
}

int main (void) {
    int ids[2] = { 0, 1 };

    peterson_lock_init(&mutex);

    // Create the threads.
    pthread_create(&threads[0], NULL, thread_function, &ids[0]);
    pthread_create(&threads[1], NULL, thread_function, &ids[1]);

    pthread_detach(threads[0]);
    pthread_detach(threads[1]);

    // We go offline for a bit...
    sleep(5);

    // Return and let the other threads die.
    return 0;
}
