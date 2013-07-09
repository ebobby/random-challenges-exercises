////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// producers-consumers problem
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

#define PRODUCERS 3
#define CONSUMERS 5
#define QUEUE_SIZE 3

uint32_t buffer = 0;
semaphore_t fill_count;
semaphore_t empty_count;
pthread_mutex_t queue_mutex;

int produce_item (int id) {
    printf("Producer %d producing item.\n", id);
    sleep(1);
    return 1;
}

void consume_item (int id, int i) {
    printf("Consumer %d consuming item %d.\n", id, i);
    sleep(1);
}

void put_item (int id, int i) {
    buffer += i;
    printf("Producer %d putting item %u.\n", id, buffer);
}

int remove_item (int id) {
    int i = buffer;
    buffer--;
    printf("Consumer %d removing item %u.\n", id, i);
    return i;
}

void *producer (void *td) {
    int producer_id = *((int *) td);
    int item;
    while (1) {
        item = produce_item(producer_id);           // produce item
        semaphore_acquire(&empty_count);            // is the buffer full? if this semaphore gets down to 0 it means
                                                    // the buffer is full and we want to wait on it.

        pthread_mutex_lock(&queue_mutex);           // buffer is not full anymore, enter critical section
        put_item(producer_id, item);                // put item into buffer
        pthread_mutex_unlock(&queue_mutex);         // exit critical section

        semaphore_release(&fill_count);             // increase the fill count and wake asleep consumers
    }
}

void *consumer (void *td) {
    int consumer_id = *((int *) td);
    int item;
    while (1) {
        semaphore_acquire(&fill_count);             // if the buffer empty? if this semaphore is 0 it means the buffer
                                                    // is empty and we want to wait on it.
        pthread_mutex_lock(&queue_mutex);           // buffer has data, enter critical section
        item = remove_item(consumer_id);            // get item
        pthread_mutex_unlock(&queue_mutex);         // exit critical section

        semaphore_release(&empty_count);            // increase empty count and wake asleep producers
        consume_item(consumer_id, item);            // consume item
    }
}

int main (void) {
    int i, producer_ids[PRODUCERS], consumer_ids[CONSUMERS];
    pthread_t producers[PRODUCERS], consumers[CONSUMERS];

    printf("%d producers, %d consumers, %d queue size.\n", PRODUCERS, CONSUMERS, QUEUE_SIZE);

    semaphore_init(&fill_count, 0);
    semaphore_init(&empty_count, QUEUE_SIZE);
    pthread_mutex_init(&queue_mutex, NULL);

    for (i = 0; i < PRODUCERS; i++) {
        producer_ids[i] = i;
        pthread_create(&producers[i], NULL, producer, &producer_ids[i]);
    }

    for (i = 0; i < CONSUMERS; i++) {
        consumer_ids[i] = i;
        pthread_create(&consumers[i], NULL, consumer, &consumer_ids[i]);
    }

    sleep(5);

    pthread_mutex_destroy(&queue_mutex);
    semaphore_destroy(&empty_count);
    semaphore_destroy(&fill_count);

    return 0;
}
