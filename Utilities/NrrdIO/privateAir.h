
#include "teemEndian.h"
#include "teemQnanhibit.h"
#include "teemBigbitfield.h"

/*
** _airFloat, _airDouble
**
** these unions facilitate converting amonst
** i: unsigned integral type
** c: (sign,exp,frac) triples of unsigned integral components
** v: the floating point numbers these bit-patterns represent
*/
typedef union {
  unsigned int i;
  struct {
#if TEEM_ENDIAN == 1234
    unsigned int frac : 23;
    unsigned int exp : 8;
    unsigned int sign : 1;
#else
    unsigned int sign : 1;
    unsigned int exp : 8;
    unsigned int frac : 23;
#endif
  } c;
  float v;
} _airFloat;

typedef union {
  airULLong i;
#if TEEM_BIGBITFIELD == 1
  /* #ifndef __sparc */
  struct {
# if TEEM_ENDIAN == 1234
    airULLong frac : 52;
    unsigned int exp : 11;
    unsigned int sign : 1;
# else
    unsigned int sign : 1;
    unsigned int exp : 11;
    airULLong frac : 52;
# endif
  } c;
#endif
  /* these next two members are used for printing in airFPFprintf_d */
  struct { /* access to whole double as two unsigned ints */
#if TEEM_ENDIAN == 1234
    unsigned int half0 : 32;
    unsigned int half1 : 32;
#else
    unsigned int half1 : 32;
    unsigned int half0 : 32;
#endif
  } h;
  struct { /* access to fraction with two unsigned ints */
#if TEEM_ENDIAN == 1234
    unsigned int frac0 : 32;
    unsigned int frac1 : 20;
    unsigned int exp : 11;
    unsigned int sign : 1;
#else
    unsigned int sign : 1;
    unsigned int exp : 11;
    unsigned int frac1 : 20;
    unsigned int frac0 : 32;
#endif
  } c2;
  double v;
} _airDouble;

