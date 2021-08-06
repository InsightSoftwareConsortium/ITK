#include <malloc.h>

int
main()
{
  /** Test whether mallinfo is available. This depends on the C library
   * implementation. */

  struct mallinfo2 minfo = mallinfo2();

  if (minfo.uordblks > 0)
  {
    return 0;
  }
  return 1;
}
