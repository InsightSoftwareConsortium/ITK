#include "f2c.h"
#include "netlib.h"
#undef abs
#undef min
#undef max
#include <stdlib.h> /* for abort() */

int i_dnnt(const double *x)
{
  (void)x;
  abort();
  return 0;
}
