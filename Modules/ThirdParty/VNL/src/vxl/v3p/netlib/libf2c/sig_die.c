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
void sig_die(s, killsignal) register char *s; int killsignal;
#else
#include "stdlib.h"
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus
extern "C" {
#endif

void sig_die(register char *s, int killsignal)
#endif
{
        /* print error message, then clear buffers */
        fprintf(stderr, "%s\n", s);

        if(killsignal)
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
