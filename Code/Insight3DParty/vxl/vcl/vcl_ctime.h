#ifndef vcl_ctime_h_
#define vcl_ctime_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

// Purpose: provide access to the <time.h> standard C header.

#include <time.h>

#ifndef VCL_WIN32
#include <sys/times.h>
#endif

#endif // vcl_ctime_h_
