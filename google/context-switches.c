////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// measure (very roughly) the number of context switches in a second.
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

// timeval to micro seconds
#define USEC(X) (1000000 * X.tv_sec + X.tv_usec)

uint32_t counter;
pthread_t threads[2];

pthread_mutex_t _lock  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t _start = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t _cond   = PTHREAD_COND_INITIALIZER;

void *thread_function (void *thread_data) {
    // This lock is so the main thread can "schedule" when these threads start.
    pthread_mutex_lock(&_start);
    pthread_mutex_unlock(&_start);

    // Grab the mutex
    pthread_mutex_lock(&_lock);

    // If the other thread ran first and increased the counter is now waiting for the signal
    if (counter > 0) {
        pthread_cond_signal(&_cond);
    }

    while (1) {
        counter++;

        // Wake up the other thread.
        pthread_cond_signal(&_cond);

        // Wait for it to increase the counter
        pthread_cond_wait(&_cond, &_lock);
    }

    pthread_mutex_unlock(&_lock);
    pthread_exit(NULL);
}

int main (void) {
    struct timeval start_time, end_time;

    // Grab the lock so the threads start when we want them to.
    pthread_mutex_lock(&_start);

    // Create the threads.
    pthread_create(&threads[0], NULL, thread_function, NULL);
    pthread_create(&threads[1], NULL, thread_function, NULL);

    pthread_detach(threads[0]);
    pthread_detach(threads[1]);

    gettimeofday(&start_time, NULL);

    // Off they go
    pthread_mutex_unlock(&_start);

    // We go offline for a bit...
    sleep(1);

    // Grab the lock so the other threads stop.
    pthread_mutex_lock(&_lock);

    gettimeofday(&end_time, NULL);

    double context_switch_speed = ((double) USEC(end_time) - USEC(start_time)) / (double) counter;

    printf("Context switch takes %f microseconds, %u elapsed in %lu microseconds.\n", context_switch_speed, counter, USEC(end_time) - USEC(start_time));

    return 0;
}
