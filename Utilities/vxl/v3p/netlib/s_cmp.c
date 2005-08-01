#include "f2c.h"
#include "netlib.h"

/* compare two strings */

#ifdef KR_headers
integer s_cmp(a0, b0, la, lb) const char *a0, *b0; ftnlen la, lb;
#else
integer s_cmp(const char *a0, const char *b0, ftnlen la, ftnlen lb)
#endif
{
  register const unsigned char *a, *aend, *b, *bend;
  a = (const unsigned char *)a0;
  b = (const unsigned char *)b0;
  aend = a + la;
  bend = b + lb;

  if (la <= lb)
  {
    while (a < aend)
      if (*a != *b)
        return *a - *b;
      else
        ++a, ++b;

    while (b < bend)
      if (*b != ' ')
        return ' ' - *b;
      else
        ++b;
    }

  else
  {
    while (b < bend)
      if (*a == *b)
        ++a, ++b;
      else
        return *a - *b;

    while (a < aend)
      if (*a != ' ')
        return *a - ' ';
      else
        ++a;
  }
  return 0;
}
