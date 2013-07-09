////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// readers-writers problem
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

#define WRITERS 3
#define READERS 5

semaphore_t queue, exclusive_access;
pthread_mutex_t reader_mutex;
uint32_t readers = 0;

void write_db (int id) {
    printf("Writer %d writing to database.\n", id);
    sleep(2);
    printf("Writer %d done writing to database.\n", id);
}

void read_db (int id) {
    printf("Reader %d reading from database.\n", id);
    sleep(1);
    printf("Reader %d done reading from database.\n", id);
}

void *writer (void *td) {
    int writer_id = *((int *) td);

    while (1) {
        printf("Writer %d intends to write...\n", writer_id);

        semaphore_acquire(&queue);                // Put us in the queue.
        semaphore_acquire(&exclusive_access);     // Is our turn, get exclusive access or wait for it.
        semaphore_release(&queue);                // We have exclusive access, take us out of the queue.
        write_db(writer_id);                      // Write to the db.
        semaphore_release(&exclusive_access);     // Release exclusive access.
        sleep(1);                                 // Get more data to write...
    }
}

void *reader (void *td) {
    int reader_id = *((int *) td);

    while (1) {
        printf("Reader %d intends to read...\n", reader_id);

        semaphore_acquire(&queue);                    // Put us in the queue
        pthread_mutex_lock(&reader_mutex);            // Critical section, checking readers count.
        if (readers == 0)                             // Are we the first reader on the queue?
            semaphore_acquire(&exclusive_access);     // Get exclusive access for this batch of readers.
        readers++;                                    // Increase the number of readers.
        semaphore_release(&queue);                    // We got the exclusive access, take us out of the queue.
        pthread_mutex_unlock(&reader_mutex);          // Release the lock on the readers count so others read can proceed.
        read_db(reader_id);                           // Read the db.
        pthread_mutex_lock(&reader_mutex);            // Critical section we want to modify the readers count
        readers--;                                    // We are done reading.
        if (readers == 0)                             // Are we the last of this batch?
            semaphore_release(&exclusive_access);     // Then release the exclusive access.
        pthread_mutex_unlock(&reader_mutex);          // End of readers count critical section
        sleep(1);                                     // Do something with the data read....
    }
}

int main (void) {
    int i, writer_ids[WRITERS], reader_ids[READERS];
    pthread_t writers[WRITERS], readers[READERS];

    printf("%d writers, %d readers.\n", WRITERS, READERS);

    semaphore_init(&queue, 1);
    semaphore_init(&exclusive_access, 1);
    pthread_mutex_init(&reader_mutex, NULL);

    for (i = 0; i < WRITERS; i++) {
        writer_ids[i] = i;
        pthread_create(&writers[i], NULL, writer, &writer_ids[i]);
    }

    for (i = 0; i < READERS; i++) {
        reader_ids[i] = i;
        pthread_create(&readers[i], NULL, reader, &reader_ids[i]);
    }

    sleep(10);

    pthread_mutex_destroy(&reader_mutex);
    semaphore_destroy(&exclusive_access);
    semaphore_destroy(&queue);

    return 0;
}
