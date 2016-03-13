#ifndef vcl_cstdio_h_
#define vcl_cstdio_h_

#include <cstdio>
#include <cstdarg>
#include "vcl_compiler.h"

// Adapted from
// http://stackoverflow.com/questions/2915672/snprintf-and-visual-studio-2010
//
// Microsoft has finally implemented snprintf in Visual Studio 2015. On earlier
// versions you can simulate it as below.
#if defined(_MSC_VER) && _MSC_VER < 1900
__inline int c99_vsnprintf(char *outBuf, size_t size, const char *format, va_list ap)
{
    int count = -1;

    if (size != 0)
        count = _vsnprintf_s(outBuf, size, _TRUNCATE, format, ap);
    if (count == -1)
        count = _vscprintf(format, ap);

    return count;
}

__inline int vcl_snprintf(char *outBuf, size_t size, const char *format, ...)
{
    int count;
    va_list ap;

    va_start(ap, format);
    count = c99_vsnprintf(outBuf, size, format, ap);
    va_end(ap);

    return count;
}
#else
#define vcl_snprintf std::snprintf
#endif

// scanf() family
#define vcl_scanf std::scanf
#define vcl_sscanf std::sscanf
#define vcl_fscanf std::fscanf
#define vcl_vscanf std::vscanf
#define vcl_vsscanf std::vsscanf
#define vcl_vfscanf std::vfscanf

#endif // vcl_cstdio_h_
