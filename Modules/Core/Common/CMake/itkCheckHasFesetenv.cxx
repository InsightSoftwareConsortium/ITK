#include <fenv.h>

int
main()
{
  /** Test whether fesetenv is available. This depends on the C library
   * implementation. */
  static fenv_t fenv;
  fesetenv(&fenv);

  return 0;
}
