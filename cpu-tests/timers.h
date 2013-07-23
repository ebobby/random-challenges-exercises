/************************************************************************************************************************
 * clock helpers
 *
 * Not really timers or clocks, just simple wrappers to measure elapsed microseconds.
 *
 * Francisco Soto <ebobby@ebobby.org>
 ************************************************************************************************************************/

#include <sys/time.h>

#define USEC(TIMEV) (1000000 * TIMEV.tv_sec + TIMEV.tv_usec)

#define CREATE_CLOCK(NAME) \
    struct timeval NAME ## _start_timer, NAME ## _end_timer;

#define START_CLOCK(NAME) \
    gettimeofday(&NAME ## _start_timer, NULL);

#define STOP_CLOCK(NAME) \
    gettimeofday(&NAME ## _end_timer, NULL);

#define CLOCK_MICROSECS_ELAPSED(NAME) \
    ((unsigned long)(1000000 * NAME ## _end_timer.tv_sec + NAME ## _end_timer.tv_usec) - (1000000 * NAME ## _start_timer.tv_sec + NAME ## _start_timer.tv_usec))

#define CLOCK_NANOSECS_ELAPSED(NAME) \
    (CLOCK_MICROSECS_ELAPSED(NAME) * 1000)
