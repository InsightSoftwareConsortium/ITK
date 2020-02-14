#include "v3p_f2c.h"
#undef abs
#undef min
#undef max

#include "stdio.h"
#include "signal.h"

#ifndef SIGIOT
#ifdef SIGABRT
#define SIGIOT SIGABRT
#endif
#endif

#ifdef KR_headers
void sig_die(s, kill) register char *s; int kill;
#else
#include "stdlib.h"
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus
extern "C" {
#endif

void sig_die(register char *s, int kill)
#endif
{
        /* print error message, then clear buffers */
        fprintf(stderr, "%s\n", s);

        if(kill)
                {
                abort();
                }
        else {
                exit(1);
                }
        }
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
}
#endif
