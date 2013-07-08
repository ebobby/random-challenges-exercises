////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// semaphore implementation
//
// Practice for Google's interviews
//
// Francisco Soto <ebobby@ebobby.org>
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <pthread.h>

typedef struct {
    int capacity;
    pthread_mutex_t mutex;
    pthread_cond_t conditional;
} semaphore_t;

void semaphore_init (semaphore_t *semaphore, int capacity) {
    semaphore->capacity = capacity;
    pthread_mutex_init(&semaphore->mutex, NULL);
    pthread_cond_init(&semaphore->conditional, NULL);
}

void semaphore_destroy (semaphore_t *semaphore) {
    pthread_mutex_destroy(&semaphore->mutex);
    pthread_cond_destroy(&semaphore->conditional);
}

void semaphore_acquire (semaphore_t *semaphore) {
    pthread_mutex_lock(&semaphore->mutex);
    while (0 == semaphore->capacity)
        pthread_cond_wait(&semaphore->conditional, &semaphore->mutex);
    semaphore->capacity--;
    pthread_mutex_unlock(&semaphore->mutex);
}

void semaphore_release (semaphore_t *semaphore) {
    pthread_mutex_lock(&semaphore->mutex);
    semaphore->capacity++;
    pthread_cond_broadcast(&semaphore->conditional);
    pthread_mutex_unlock(&semaphore->mutex);
}
