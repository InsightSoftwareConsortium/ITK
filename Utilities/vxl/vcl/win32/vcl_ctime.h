#ifndef vcl_win32_ctime_h_
#define vcl_win32_ctime_h_

// 1. include system header
#include <ctime>

// size_t
#ifndef vcl_size_t
#define vcl_size_t  ::size_t
#endif
// clock_t
#ifndef vcl_clock_t
#define vcl_clock_t  ::clock_t
#endif
// time_t
#ifndef vcl_time_t
#define vcl_time_t ::time_t
#endif
// tm
#ifndef vcl_tm
#define vcl_tm ::tm
#endif
// asctime
#ifndef vcl_asctime
#define vcl_asctime ::asctime
#endif
// clock
#ifndef vcl_clock
#define vcl_clock ::clock
#endif
// difftime
#ifndef vcl_difftime
#define vcl_difftime ::difftime
#endif
// localtime
#ifndef vcl_localtime
#define vcl_localtime ::localtime
#endif
// strftime
#ifndef vcl_strftime
#define vcl_strftime ::strftime
#endif
// ctime
#ifndef vcl_ctime
#define vcl_ctime ::ctime
#endif
// gmtime
#ifndef vcl_gmtime
#define vcl_gmtime ::gmtime
#endif
// mktime
#ifndef vcl_mktime
#define vcl_mktime ::mktime
#endif
// time
#ifndef vcl_time
#define vcl_time ::time
#endif

#endif // vcl_ctime_h_
