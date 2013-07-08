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
#include <pthread.h>

typedef struct {
    int capacity;
    int current;
    pthread_mutex_t mutex;
    pthread_cond_t conditional;
} semaphore_t;

#define NUM_THREADS 10

pthread_t threads[NUM_THREADS];
semaphore_t semaphore;

void semaphore_init (semaphore_t *semaphore, int capacity) {
    semaphore->capacity = capacity;
    semaphore->current = 0;
    pthread_mutex_init(&semaphore->mutex, NULL);
    pthread_cond_init(&semaphore->conditional, NULL);
}

void semaphore_acquire (semaphore_t *semaphore) {
    pthread_mutex_lock(&semaphore->mutex);
    while (semaphore->current == semaphore->capacity)
        pthread_cond_wait(&semaphore->conditional, &semaphore->mutex);
    semaphore->current++;
    pthread_mutex_unlock(&semaphore->mutex);
}

void semaphore_release (semaphore_t *semaphore) {
    pthread_mutex_lock(&semaphore->mutex);
    semaphore->current--;
    pthread_cond_broadcast(&semaphore->conditional);
    pthread_mutex_unlock(&semaphore->mutex);
}

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

    // Return and let the other threads die.
    return 0;
}
