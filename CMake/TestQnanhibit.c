#include <stdio.h>
#include <float.h>

#if defined(__BORLANDC__)
# include <math.h>
# include <float.h>
#endif


int
main(int argc, char *argv[])
{
   const char * const me=argv[0];
   const float zero=0.0F;
   union {
     float flt32bit;
     int   int32bit;
   } qnan;

#if defined(__BORLANDC__)
   // Disable floating point exceptions in Borland
   _control87(MCW_EM, MCW_EM);
#endif // defined(__BORLANDC__)

   if (sizeof(float) != sizeof(int))
     {
     fprintf(stderr, "%s: MADNESS:  sizeof(float)=%d != sizeof(int)=%d\n",
           me, (int)sizeof(float), (int)sizeof(int));
     return -1;
     }
   qnan.flt32bit=zero/zero;
   printf("-DTEEM_QNANHIBIT=%d\n", (qnan.int32bit >> 22) & 1);
   return (int)((qnan.int32bit >> 22) & 1);
}
