#include <stdio.h>
#include <float.h>

int
main(int argc, char *argv[])
{
  char *me;
  float qnan, zero;
  int i;

  me = argv[0];
  if (sizeof(float) != sizeof(int))
    {
    fprintf(stderr, "%s: MADNESS:  sizeof(float)=%d != sizeof(int)=%d\n",
            me, (int)sizeof(float), (int)sizeof(int));
    return -1;
    }

  zero = 0;
  qnan=zero/zero;
  i=*(int*)(&qnan);
  printf("-DTEEM_QNANHIBIT=%d\n", (i >> 22) & 1);
  return (int)((i >> 22) & 1);
}
