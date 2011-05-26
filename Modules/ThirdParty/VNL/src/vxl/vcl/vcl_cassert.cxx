// @author fsm
#include <vcl_cstdio.h>
#include <vcl_cstdlib.h>

void vcl_cassert_failure(char const *FILE, int LINE, char const *expr)
{
  vcl_fprintf(stderr, "%s:%d assertion failure \'%s\'\n",
              FILE, LINE, expr);
  vcl_fflush(stderr);
  vcl_abort();
}

#ifndef __GNUC__
// fsm: This is a silly hack to enable us to link code compiled
// with SunPro against a library (such as libMesaGL) compiled with gcc.
// The gcc assert macro uses a function call __eprintf() which is defined
// in gcc/libgcc2.c in the gcc sources.
//
// Note that this is fixing a problem with *gcc*, not the SunPro or
// KAI compilers.
extern "C" void
__eprintf(char const *string, char const *expression,
          unsigned int line, char const *filename)
{
  vcl_fprintf(stderr, string, expression, line, filename);
  vcl_fflush(stderr);
  vcl_abort();
}
#endif
