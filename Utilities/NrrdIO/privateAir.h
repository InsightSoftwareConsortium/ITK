
#include "teemEndian.h"
#include "teemQnanhibit.h"

/* miscAir.c */
extern double _airSanityHelper(double val);

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
#if TEEM_ENDIAN == 1234        /* little endian */
    unsigned int mant : 23;
    unsigned int expo : 8;
    unsigned int sign : 1;
#else                          /* big endian */
    unsigned int sign : 1;
    unsigned int expo : 8;
    unsigned int mant : 23;
#endif
  } c;
  float v;
} _airFloat;

typedef union {
  airULLong i;
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
#if TEEM_ENDIAN == 1234        /* little endian */
    unsigned int mant1 : 32;
    unsigned int mant0 : 20;
    unsigned int expo : 11;
    unsigned int sign : 1;
#else                          /* big endian */
    unsigned int sign : 1;
    unsigned int expo : 11;
    unsigned int mant0 : 20;
    unsigned int mant1 : 32;
#endif
  } c;
  double v;
} _airDouble;


