#include "itpack_f2c.h"
#undef abs
#include "math.h"


integer do_fio(ftnint *number, char *ptr, ftnlen len)
{
  return 0;
}

integer e_wsfe(Void)
{
  return 0;
}

integer s_wsfe(cilist *a)
{
  return 0;
}

double pow_dd(doublereal *ap, doublereal *bp)
{
  return(pow(*ap, *bp) );
}

integer i_sign(integer *a, integer *b)
{
integer x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}

etime_(float *tarray)
{
  tarray[0]=0.0f;
  tarray[1]=0.0f;
}
