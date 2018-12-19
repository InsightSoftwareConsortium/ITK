// File extracted from freebsd to provide local version of drand48 functionality
// in a way that can be supported on all platforms (i.e. MSVC does not provide drand48()
/*
 * Copyright (c) 1993 Martin Birgmeier
 * All rights reserved.
 *
 * You may redistribute unmodified or modified versions of this source
 * code provided that the above copyright notice and this and the
 * following conditions are retained.
 *
 * This software is provided ``as is'', and comes with no warranties
 * of any kind. I shall in no event be liable for anything that happens
 * to anyone/anything when using this software.
 */

#include <math.h>
#include <cstdint>
#include "vxl_config.h"
#include "vnl_drand48.h"

static constexpr unsigned short	RAND48_SEED_0 = 0x330e;
static constexpr unsigned short	RAND48_SEED_1	= 0xabcd;
static constexpr unsigned short	RAND48_SEED_2	= 0x1234;
static constexpr unsigned short	RAND48_MULT_0	= 0xe66d;
static constexpr unsigned short	RAND48_MULT_1	= 0xdeec;
static constexpr unsigned short	RAND48_MULT_2	= 0x0005;
static constexpr unsigned short	RAND48_ADD	  = 0x000b;

//===== local_ldexp.c ===
/* Bit fiddling routines copied from msun/src/math_private.h,v 1.15 */
#if VXL_BIG_ENDIAN
typedef union
{
  double value;
  struct
  {
    std::uint32_t msw;
    std::uint32_t lsw;
  } parts;
} ieee_double_shape_type;
#elif VXL_LITTLE_ENDIAN
typedef union
{
  double value;
  struct
  {
    std::uint32_t lsw;
    std::uint32_t msw;
  } parts;
} ieee_double_shape_type;
#else
# error "ENDIAN of system not configured"
#endif

/* Get two 32 bit ints from a double.  */

#define EXTRACT_WORDS(ix0,ix1,d)				\
do {								\
  ieee_double_shape_type ew_u;					\
  ew_u.value = (d);						\
  (ix0) = ew_u.parts.msw;					\
  (ix1) = ew_u.parts.lsw;					\
} while (0)

/* Get the more significant 32 bit int from a double.  */

#define GET_HIGH_WORD(i,d)					\
do {								\
  ieee_double_shape_type gh_u;					\
  gh_u.value = (d);						\
  (i) = gh_u.parts.msw;						\
} while (0)

/* Set the more significant 32 bits of a double from an int.  */

#define SET_HIGH_WORD(d,v)					\
do {								\
  ieee_double_shape_type sh_u;					\
  sh_u.value = (d);						\
  sh_u.parts.msw = (v);						\
  (d) = sh_u.value;						\
} while (0)


static constexpr double two54   =  1.80143985094819840000e+16; /* 0x43500000, 0x00000000 */
static constexpr double twom54  =  5.55111512312578270212e-17; /* 0x3C900000, 0x00000000 */
static constexpr double huge   = 1.0e+300;
static constexpr double tiny   = 1.0e-300;

static double
local_copysign(double x, double y)
{
  std::uint32_t hx,hy;
  GET_HIGH_WORD(hx,x);
  GET_HIGH_WORD(hy,y);
  SET_HIGH_WORD(x,(hx&0x7fffffff)|(hy&0x80000000));
  return x;
}

static double
local_ldexp(double x, int n)
{
  std::int32_t k,hx,lx;
  EXTRACT_WORDS(hx,lx,x);
  k = (hx&0x7ff00000)>>20;		/* extract exponent */
  if (k==0) {				/* 0 or subnormal x */
    if ((lx|(hx&0x7fffffff))==0) return x; /* +-0 */
    x *= two54;
    GET_HIGH_WORD(hx,x);
    k = ((hx&0x7ff00000)>>20) - 54;
    if (n< -50000) return tiny*x; 	/*underflow*/
  }
  if (k==0x7ff) return x+x;		/* NaN or Inf */
  k = k+n;
  if (k >  0x7fe) return huge*local_copysign(huge,x); /* overflow  */
  if (k > 0) 				/* normal result */
  {SET_HIGH_WORD(x,(hx&0x800fffff)|(k<<20)); return x;}
  if (k <= -54) {
    if (n > 50000) 	/* in case integer overflow in n+k */
      return huge*local_copysign(huge,x);	/*overflow*/
    else return tiny*local_copysign(tiny,x); 	/*underflow*/
  }
  k += 54;				/* subnormal result */
  SET_HIGH_WORD(x,(hx&0x800fffff)|(k<<20));
  return x*twom54;
}

// ^^^^^ local_ldexp.c ^^^^

static unsigned short _rand48_seed[3] = {
		RAND48_SEED_0,
		RAND48_SEED_1,
		RAND48_SEED_2
};
static unsigned short _rand48_mult[3] = {
		RAND48_MULT_0,
		RAND48_MULT_1,
		RAND48_MULT_2
};
static unsigned short _rand48_add = RAND48_ADD;

static void
_dorand48(unsigned short xseed[3])
{
	unsigned long accu;
	unsigned short temp[2];

	accu = (unsigned long) _rand48_mult[0] * (unsigned long) xseed[0] +
				 (unsigned long) _rand48_add;
	temp[0] = (unsigned short) accu;        /* lower 16 bits */
	accu >>= sizeof(unsigned short) * 8;
	accu += (unsigned long) _rand48_mult[0] * (unsigned long) xseed[1] +
					(unsigned long) _rand48_mult[1] * (unsigned long) xseed[0];
	temp[1] = (unsigned short) accu;        /* middle 16 bits */
	accu >>= sizeof(unsigned short) * 8;
	accu += _rand48_mult[0] * xseed[2] + _rand48_mult[1] * xseed[1] + _rand48_mult[2] * xseed[0];
	xseed[0] = temp[0];
	xseed[1] = temp[1];
	xseed[2] = (unsigned short) accu;
}

static double
local_erand48(unsigned short xseed[3])
{
	_dorand48(xseed);
	return local_ldexp((double) xseed[0], -48) +
				 local_ldexp((double) xseed[1], -32) +
				 local_ldexp((double) xseed[2], -16);
}

//======================================================================================
// The public interface
double vnl_drand48()
{
	return local_erand48(_rand48_seed);
}

void vnl_srand48(long seed)
{
  _rand48_seed[0] = RAND48_SEED_0;
  _rand48_seed[1] = (unsigned short) seed;
  _rand48_seed[2] = (unsigned short) (seed >> 16);
  _rand48_mult[0] = RAND48_MULT_0;
  _rand48_mult[1] = RAND48_MULT_1;
  _rand48_mult[2] = RAND48_MULT_2;
  _rand48_add = RAND48_ADD;
}
