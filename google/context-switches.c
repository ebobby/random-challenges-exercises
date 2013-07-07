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
    // for debugging
    int thread_id[2];

    thread_id[0] = 1;
    thread_id[1] = 2;

    struct timeval start_time, end_time;

    // Grab the lock so the threads start when we want them to.
    pthread_mutex_lock(&_start);

    // Create the threads.
    pthread_create(&threads[0], NULL, thread_function, &thread_id[0]);
    pthread_create(&threads[1], NULL, thread_function, &thread_id[1]);

    pthread_detach(threads[0]);
    pthread_detach(threads[1]);

    printf("Context switches: %u\n", counter);

    gettimeofday(&start_time, NULL);

    // Off they go
    pthread_mutex_unlock(&_start);

    // We go offline for a bit...
    sleep(1);

    // Grab the lock so the other threads stop.
    pthread_mutex_lock(&_lock);

    gettimeofday(&end_time, NULL);

    float context_switch_speed = (((float) end_time.tv_usec - start_time.tv_usec) / (float) counter) * 1000000.0;

    printf("Context switch takes %f nanoseconds, %u elapsed in %u milliseconds.\n", context_switch_speed, counter, end_time.tv_usec - start_time.tv_usec);

    return 0;
}
