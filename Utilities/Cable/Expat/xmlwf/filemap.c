#if (defined(WIN32) || defined(_WIN32)) && !defined(__CYGWIN__)
#include "win32filemap.c"
#else
#include "unixfilemap.c"
#endif

