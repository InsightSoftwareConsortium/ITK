#ifndef vcl_sys_time_h_
#define vcl_sys_time_h_

#include <vcl_compiler.h>

#if defined(VCL_WIN32) && !defined(__CYGWIN__)
# include <sys/timeb.h>
extern "C" int gettimeofday(struct timeval*, struct timezone*);

#elif defined(__CYGWIN__)
# include <sys/time.h>
# include <sys/timeb.h>
# include <sys/times.h>

#elif defined(GNU_LIBSTDCXX_V3)
# define __restrict /* */
# include <sys/time.h>
# undef __restrict

#elif !defined(VCL_NO_SYS_TIME_H)
# include <sys/time.h>

#elif defined(SYSV)
extern "C" int gettimeofday(struct timeval *tp);

#else
extern "C" int gettimeofday(struct timeval*, struct timezone*);
#endif

//struct timeval:
// time_t         tv_sec      seconds
// suseconds_t    tv_usec     microseconds

//struct itimerval:
// struct timeval it_interval timer interval
// struct timeval it_value    current value

// int   getitimer(int, struct itimerval *);
// int   setitimer(int, const struct itimerval *, struct itimerval *);
// int   gettimeofday(struct timeval *, void *);
// int   select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
// int   utimes(const char *, const struct timeval [2]);

#endif // vcl_sys_time_h_
