#include <fenv.h>

int main()
{
  /** Test whether feenableexcept is available. This depends on the C library
   * implementation. */
  feenableexcept (FE_DIVBYZERO);

  return 0;
}
