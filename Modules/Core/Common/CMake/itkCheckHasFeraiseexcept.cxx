#include <fenv.h>

int
main()
{
  /** Test whether feraiseexcept is available. This depends on the C library
   * implementation. */
  feraiseexcept(FE_DIVBYZERO);

  return 0;
}
