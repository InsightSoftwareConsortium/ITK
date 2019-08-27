#include <fenv.h>

int
main()
{
  /** Test whether fegetenv is available. This depends on the C library
   * implementation. */
  static fenv_t fenv;
  fegetenv(&fenv);

  return 0;
}
