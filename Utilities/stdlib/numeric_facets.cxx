/*
 * Copyright (c) 1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#include <iterator>
#include <string.h>
#include <stdlib.h>
#include <locale>
#include <limits.h>
#include <math.h>               /* Needed for HUGE_VAL macro */

#if defined(_MSC_VER)
#include <float.h>
#elif defined(__GNUC__) && !defined(__sun__)
#include <values.h>
#else
#include <values.h>
#include <nan.h>
#endif

__STL_BEGIN_NAMESPACE

_Locale_numeric* __acquire_numeric(const char* name);
void __release_numeric(_Locale_numeric* cat);


//----------------------------------------------------------------------
// numpunct<char> and numpunct<wchar_t>

numpunct<char>::numpunct(size_t refs)
  : locale::facet(refs),
    _M_truename("true"),
    _M_falsename("false")
{
}

numpunct<char>::~numpunct() 
{}

numpunct<wchar_t>::numpunct(size_t refs)
  : locale::facet(refs),
    _M_truename(L"true"),
    _M_falsename(L"false")
{
}

numpunct<wchar_t>::~numpunct() 
{}

wchar_t numpunct<wchar_t>::do_decimal_point() const
{
  return L'.';
}

wchar_t numpunct<wchar_t>::do_thousands_sep() const
{
  return L',';
}

string numpunct<wchar_t>::do_grouping() const
{
  return string();
}


basic_string<wchar_t> numpunct<wchar_t>::do_truename() const
{
  return _M_truename;
}
 
basic_string<wchar_t> numpunct<wchar_t>::do_falsename() const
{
  return _M_falsename;
} 


// numpunct_byname<char> and numpunct_byname<wchar_t> 

numpunct_byname<char>::numpunct_byname(const char* name, size_t refs)
  : numpunct<char>(refs),
    _M_numeric(__acquire_numeric(name))
{
  if (!_M_numeric)
    locale::_M_throw_runtime_error();

  _M_truename  = _Locale_true(_M_numeric);
  _M_falsename = _Locale_false(_M_numeric);
}

numpunct_byname<char>::~numpunct_byname()
{
  __release_numeric(_M_numeric);
}

char numpunct_byname<char>::do_decimal_point() const {
  return _Locale_decimal_point(_M_numeric);
}

char numpunct_byname<char>::do_thousands_sep() const {
  return _Locale_thousands_sep(_M_numeric);
}

string numpunct_byname<char>::do_grouping() const {
  const char * __grouping = _Locale_grouping(_M_numeric);
  if (__grouping != NULL && __grouping[0] == CHAR_MAX)
    __grouping = "";
  return __grouping;
}

numpunct_byname<wchar_t>::numpunct_byname(const char* name, size_t refs)
  : numpunct<wchar_t>(refs),
    _M_numeric(__acquire_numeric(name))
{
  if (!_M_numeric)
    locale::_M_throw_runtime_error();

  const char* truename  = _Locale_true(_M_numeric);
  const char* falsename = _Locale_false(_M_numeric);

  // The use of assign() would be more appropriate here,
  // but some compilers have trouble with template member
  // functions, so we use copy instead
  int __truelen = strlen(truename);
  _M_truename.resize(__truelen);
  copy(truename, truename + __truelen, _M_truename.begin());
  int __falselen = strlen(falsename);
  _M_falsename.resize(__falselen);
  copy(falsename, falsename + __falselen, _M_falsename.begin());
}

numpunct_byname<wchar_t>::~numpunct_byname()
{
  __release_numeric(_M_numeric);
}

wchar_t numpunct_byname<wchar_t>::do_decimal_point() const {
  return (wchar_t) _Locale_decimal_point(_M_numeric);
}

wchar_t numpunct_byname<wchar_t>::do_thousands_sep() const {
  return (wchar_t) _Locale_thousands_sep(_M_numeric);
}

string numpunct_byname<wchar_t>::do_grouping() const {
  const char * __grouping = _Locale_grouping(_M_numeric);
  if (__grouping != NULL && __grouping[0] == CHAR_MAX)
    __grouping = "";
  return __grouping;
}

//----------------------------------------------------------------------
// num_get

// Helper functions for _M_do_get_integer.

void __initialize_get_digit(wchar_t* digits, wchar_t* xdigits,
                            const ctype<wchar_t>& ct)
{
  char narrow_digits[11]  = "0123456789";
  char narrow_xdigits[13] = "aAbBcCdDeEfF";

  ct.widen(narrow_digits + 0,  narrow_digits + 10,  digits);
  ct.widen(narrow_xdigits + 0, narrow_xdigits + 10, xdigits);
}

// Return either the digit corresponding to c, or a negative number if
// if c isn't a digit.  We return -1 if c is the separator character, and
// -2 if it's some other non-digit.
int __get_digit(wchar_t c,
                const wchar_t* digits, const wchar_t* xdigits,
                wchar_t separator)
{
  // Test if it's the separator.
  if (c == separator)
    return -1;

  const wchar_t* p;

  // Test if it's a decimal digit.
  p = find(digits, digits + 10, c);
  if (p != digits + 10)
    return p - digits;

  // Test if it's a hex digit.
  p = find(xdigits, xdigits + 12, c);
  if (p != xdigits + 12)
    return 10 + (xdigits - p) / 2;
  else
    return -2;                  // It's not a digit and not the separator.
}

// Similar, except return the character itself instead of the numeric
// value.  Used for floating-point input.
pair<char, bool> __get_fdigit(wchar_t c, const wchar_t* digits)
{
  const wchar_t* p = find(digits, digits + 10, c);
  if (p != digits + 10)
    return make_pair(char('0' + (p - digits)), true);
  else
    return make_pair('\0', false);
}

pair<char, bool> __get_fdigit_or_sep(wchar_t c, wchar_t sep,
                                     const wchar_t * digits)
{
  if (c == sep)
    return make_pair(',', true);
  else
    return __get_fdigit(c, digits);
}


// Helper functions for _M_do_get_float.

void __initialize_get_float(const locale& loc,
                            char& plus_char, char& minus_char, char& dot_char,
                            char& pow_e, char& pow_E,
                            char& sep, string& grouping,
                            char* /* digits */)
{
  plus_char = '+';
  minus_char = '-';
  dot_char = use_facet<numpunct<char> >(loc).decimal_point();
  pow_e = 'e';
  pow_E = 'E';
  sep = use_facet<numpunct<char> >(loc).thousands_sep();
  grouping = use_facet<numpunct<char> >(loc).grouping();
}

void __initialize_get_float(const locale& loc,
                            wchar_t& plus_char, wchar_t& minus_char,
			    wchar_t& dot_char,
                            wchar_t& pow_e, wchar_t& pow_E,
                            wchar_t& sep, string&  grouping,
                            wchar_t* digits)
{
  char ndigits[11] = "0123456789";
  const ctype<wchar_t>& ct = use_facet<ctype<wchar_t> >(loc);

  plus_char  = ct.widen('+');
  minus_char = ct.widen('-');
  dot_char   = use_facet<numpunct<wchar_t> >(loc).decimal_point();
  pow_e = ct.widen('e');
  pow_E = ct.widen('E');
  sep = use_facet<numpunct<wchar_t> >(loc).thousands_sep();
  grouping = use_facet<numpunct<wchar_t> >(loc).grouping();
  ct.widen(ndigits + 0, ndigits + 10, digits);
}


/*
 * __string_to_double is just lifted from atof, the difference being
 * that we just use '.' for the decimal point, rather than let it
 * be taken from the current C locale, which of course is not accessible
 * to us.
 */

namespace {

typedef unsigned int uint32;
#if defined(_MSC_VER)
typedef unsigned __int64  uint64;
#define SUFFIX64(X) X##uI64
#else
typedef unsigned long long uint64;
#define SUFFIX64(X) X##ull
#endif

// Multiplication of two 64-bit integers, giving a 128-bit result.
// Taken from Algorithm M in Knuth section 4.3.1, with the loop 
// hand-unrolled.
inline void mult64(const uint64 u, const uint64 v,
                   uint64& high, uint64& low)
{
  const uint64 low_mask = SUFFIX64(0xffffffff);
  const uint64 u0 = u & low_mask;
  const uint64 u1 = u >> 32;
  const uint64 v0 = v & low_mask;
  const uint64 v1 = v >> 32;

  uint64 t = u0 * v0;
  low = t & low_mask;

  t = u1 * v0 + (t >> 32);
  uint64 w1 = t & low_mask;
  uint64 w2 = t >> 32;

  uint64 x = u0 * v1 + w1;
  low += (x & low_mask) << 32;
  high = u1 * v1 + w2 + (x >> 32);
}

inline void set_exponent(uint64& val, uint64 exp)
{
  static const uint64 bit11 = SUFFIX64(0x7ff);
  static const uint64 exponent_mask = bit11 << 52;
  val = (val & ~exponent_mask) | ((exp & bit11) << 52);
}

/* Power of ten fractions for tenscale*/
/* The constants are factored so that at most two constants
 * and two multiplies are needed. Furthermore, one of the constants
 * is represented exactly - 10**n where 1<= n <= 27.
 */

static const uint64 tenpow[80] = {
SUFFIX64(0xa000000000000000), /* tenpow[0]=(10**1)/(2**4) */
SUFFIX64(0xc800000000000000), /* tenpow[1]=(10**2)/(2**7) */
SUFFIX64(0xfa00000000000000), /* tenpow[2]=(10**3)/(2**10) */
SUFFIX64(0x9c40000000000000), /* tenpow[3]=(10**4)/(2**14) */
SUFFIX64(0xc350000000000000), /* tenpow[4]=(10**5)/(2**17) */
SUFFIX64(0xf424000000000000), /* tenpow[5]=(10**6)/(2**20) */
SUFFIX64(0x9896800000000000), /* tenpow[6]=(10**7)/(2**24) */
SUFFIX64(0xbebc200000000000), /* tenpow[7]=(10**8)/(2**27) */
SUFFIX64(0xee6b280000000000), /* tenpow[8]=(10**9)/(2**30) */
SUFFIX64(0x9502f90000000000), /* tenpow[9]=(10**10)/(2**34) */
SUFFIX64(0xba43b74000000000), /* tenpow[10]=(10**11)/(2**37) */
SUFFIX64(0xe8d4a51000000000), /* tenpow[11]=(10**12)/(2**40) */
SUFFIX64(0x9184e72a00000000), /* tenpow[12]=(10**13)/(2**44) */
SUFFIX64(0xb5e620f480000000), /* tenpow[13]=(10**14)/(2**47) */
SUFFIX64(0xe35fa931a0000000), /* tenpow[14]=(10**15)/(2**50) */
SUFFIX64(0x8e1bc9bf04000000), /* tenpow[15]=(10**16)/(2**54) */
SUFFIX64(0xb1a2bc2ec5000000), /* tenpow[16]=(10**17)/(2**57) */
SUFFIX64(0xde0b6b3a76400000), /* tenpow[17]=(10**18)/(2**60) */
SUFFIX64(0x8ac7230489e80000), /* tenpow[18]=(10**19)/(2**64) */
SUFFIX64(0xad78ebc5ac620000), /* tenpow[19]=(10**20)/(2**67) */
SUFFIX64(0xd8d726b7177a8000), /* tenpow[20]=(10**21)/(2**70) */
SUFFIX64(0x878678326eac9000), /* tenpow[21]=(10**22)/(2**74) */
SUFFIX64(0xa968163f0a57b400), /* tenpow[22]=(10**23)/(2**77) */
SUFFIX64(0xd3c21bcecceda100), /* tenpow[23]=(10**24)/(2**80) */
SUFFIX64(0x84595161401484a0), /* tenpow[24]=(10**25)/(2**84) */
SUFFIX64(0xa56fa5b99019a5c8), /* tenpow[25]=(10**26)/(2**87) */
SUFFIX64(0xcecb8f27f4200f3a), /* tenpow[26]=(10**27)/(2**90) */

SUFFIX64(0xd0cf4b50cfe20766), /* tenpow[27]=(10**55)/(2**183) */
SUFFIX64(0xd2d80db02aabd62c), /* tenpow[28]=(10**83)/(2**276) */
SUFFIX64(0xd4e5e2cdc1d1ea96), /* tenpow[29]=(10**111)/(2**369) */
SUFFIX64(0xd6f8d7509292d603), /* tenpow[30]=(10**139)/(2**462) */
SUFFIX64(0xd910f7ff28069da4), /* tenpow[31]=(10**167)/(2**555) */
SUFFIX64(0xdb2e51bfe9d0696a), /* tenpow[32]=(10**195)/(2**648) */
SUFFIX64(0xdd50f1996b947519), /* tenpow[33]=(10**223)/(2**741) */
SUFFIX64(0xdf78e4b2bd342cf7), /* tenpow[34]=(10**251)/(2**834) */
SUFFIX64(0xe1a63853bbd26451), /* tenpow[35]=(10**279)/(2**927) */
SUFFIX64(0xe3d8f9e563a198e5), /* tenpow[36]=(10**307)/(2**1020) */

SUFFIX64(0xfd87b5f28300ca0e), /* tenpow[37]=(10**-28)/(2**-93) */
SUFFIX64(0xfb158592be068d2f), /* tenpow[38]=(10**-56)/(2**-186) */
SUFFIX64(0xf8a95fcf88747d94), /* tenpow[39]=(10**-84)/(2**-279) */
SUFFIX64(0xf64335bcf065d37d), /* tenpow[40]=(10**-112)/(2**-372) */
SUFFIX64(0xf3e2f893dec3f126), /* tenpow[41]=(10**-140)/(2**-465) */
SUFFIX64(0xf18899b1bc3f8ca2), /* tenpow[42]=(10**-168)/(2**-558) */
SUFFIX64(0xef340a98172aace5), /* tenpow[43]=(10**-196)/(2**-651) */
SUFFIX64(0xece53cec4a314ebe), /* tenpow[44]=(10**-224)/(2**-744) */
SUFFIX64(0xea9c227723ee8bcb), /* tenpow[45]=(10**-252)/(2**-837)     */
SUFFIX64(0xe858ad248f5c22ca), /* tenpow[46]=(10**-280)/(2**-930) */
SUFFIX64(0xe61acf033d1a45df), /* tenpow[47]=(10**-308)/(2**-1023)    */
SUFFIX64(0xe3e27a444d8d98b8), /* tenpow[48]=(10**-336)/(2**-1116) */
SUFFIX64(0xe1afa13afbd14d6e)  /* tenpow[49]=(10**-364)/(2**-1209) */
};

static const short twoexp[80] = {
4,7,10,14,17,20,24,27,30,34,37,40,44,47,50,54,57,60,64,67,70,74,77,80,84,87,90,
183,276,369,462,555,648,741,834,927,1020,
-93,-186,-279,-372,-465,-558,-651,-744,-837,-930,-1023,-1116,-1209
};

const int TEN_1    = 0;         /* offset to 10 **   1 */
const int TEN_27   = 26;        /* offset to 10 **  27 */
const int TEN_M28  = 37;        /* offset to 10 ** -28 */
const int NUM_HI_P = 11;
const int NUM_HI_N = 13;

const uint64 HIBITULL = SUFFIX64(1) << 63;

void norm_and_round(uint64& p, int& norm, uint64 prodhi, uint64 prodlo)
{
  norm = 0;
  if( ! (prodhi & HIBITULL) ) { 
                                /* leading bit is a zero 
                                 * may have to normalize 
                                 */
    if(prodhi == ~HIBITULL &&
       prodlo >> 62 == 0x3 ) {  /* normalization followed by round
                                 * would cause carry to create
                                 * extra bit, so don't normalize 
                                 */
      p = HIBITULL;
      return;
    }
    p = prodhi<<1 | prodlo>>63; /* normalize */
    norm=1;
    prodlo <<= 1;
  }
  else {
    p = prodhi;
  }

  if( prodlo & HIBITULL ) {     /* first guard bit a one */
    if( (p & SUFFIX64(0x1)) ||         /* LSB on, round to even */
       prodlo != HIBITULL) {    /* not borderline for round to even */

      /* round */
      p++;
      if(p==0)
        p++;
    }
  }

  return;
}

// Convert a 64-bitb fraction * 10^exp to a 64-bit fraction * 2^bexp.
// p:    64-bit fraction
// exp:  base-10 exponent
// bexp: base-2 exponent (output parameter)

void tenscale(uint64& p, int exp, int& bexp)
{
  uint64 prodhi, prodlo;        /* 128b product */
  int exp_hi, exp_lo;           /* exp = exp_hi*32 + exp_lo */
  int hi, lo, tlo, thi;         /* offsets in power of ten table */
  int norm;                     /* number of bits of normalization */
  int num_hi;                   /* number of high exponent powers */

  bexp = 0;
  if(exp > 0) {                 /* split exponent */
    exp_lo = exp;
    exp_hi = 0;
    if(exp_lo>27) {
      exp_lo++;
      while(exp_lo>27) {
        exp_hi++;
        exp_lo-=28;
      }
    }
    tlo = TEN_1;
    thi = TEN_27;
    num_hi = NUM_HI_P;
  }
  else if(exp < 0) {
    exp_lo = exp;
    exp_hi = 0;
    while(exp_lo<0) {
      exp_hi++;
      exp_lo+=28;
    }
    tlo = TEN_1;
    thi = TEN_M28;
    num_hi = NUM_HI_N;
  }
  else {                        /* no scaling needed */
    return;
  }
  while(exp_hi) {               /* scale */
    hi = min(exp_hi,num_hi);    /* only a few large powers of 10 */
    exp_hi -= hi;               /* could iterate in extreme case */
    hi += thi-1;
    mult64(p, tenpow[hi], prodhi, prodlo);
    norm_and_round(p, norm, prodhi, prodlo);
    bexp += twoexp[hi] - norm;
  }
  if(exp_lo) {
    lo = tlo + exp_lo -1;
    mult64(p, tenpow[lo], prodhi, prodlo);
    norm_and_round(p, norm, prodhi, prodlo);
    bexp += twoexp[lo] - norm;
  }

  return;
}

// First argument is a buffer of values from 0 to 9, NOT ascii.
// Second argument is number of digits in buffer, 1 <= digits <= 17.
// Third argument is base-10 exponent.
double _atod(char *buffer, int ndigit, int dexp)
{
  uint64 value;         /* Value develops as follows:
                                 * 1) decimal digits as an integer
                                 * 2) left adjusted fraction
                                 * 3) right adjusted fraction
                                 * 4) exponent and fraction
                                 */
  uint32 guard;         /* First guard bit */
  uint64 rest;          /* Remaining guard bits */

  int bexp;             /* binary exponent */
  int nzero;            /* number of non-zero bits */
  int sexp;             /* scaling exponent */

  char *bufferend;              /* pointer to char after last digit */
  
  /* Check for zero and treat it as a special case */

  if (buffer == 0){
    return 0.0; 
  }

  /* Convert the decimal digits to a binary integer. */

  bufferend = buffer + ndigit;
  value = 0;                    

  while( buffer < bufferend ) {
    value *= 10;
    value += *buffer++;
  }

  /* Check for zero and treat it as a special case */

  if (value == 0){
    return 0.0; 
  }

  /* Normalize value */

  bexp = 64;                    /* convert from 64b int to fraction */

  /* Count number of non-zeroes in value */
  nzero = 0;
  if ( value >> (32        ) ){ nzero  = 32; }
  if ( value >> (16 + nzero) ){ nzero += 16; }
  if ( value >> ( 8 + nzero) ){ nzero +=  8; }
  if ( value >> ( 4 + nzero) ){ nzero +=  4; }
  if ( value >> ( 2 + nzero) ){ nzero +=  2; }
  if ( value >> ( 1 + nzero) ){ nzero +=  1; }
  if ( value >> (     nzero) ){ nzero +=  1; }

  /* Normalize */
  value <<= (uint64) (64-nzero);
  bexp -= 64-nzero;

  /* At this point we have a 64b fraction and a binary exponent 
   * but have yet to incorporate the decimal exponent.
   */

  /* multiply by 10^dexp */

  tenscale(value, dexp, sexp);
  bexp += sexp;

  if (bexp <= -1022) {          /* HI denorm or underflow */
    bexp += 1022;
    if( bexp < -53 ) {          /* guaranteed underflow */
      value = 0;
    }
    else {                      /* denorm or possible underflow */
      int lead0;

      lead0 = 12-bexp;          /* 12 sign and exponent bits */

      /* we must special case right shifts of more than 63 */

      if ( lead0 > 64 )
      {
           rest = value;
           guard = 0;
           value = 0;
      }
      else if ( lead0 == 64 )
      {
           rest = value & (SUFFIX64(1) << 63)-1;
           guard = (uint32) ((value>> 63) & SUFFIX64(1));
           value = 0;
      }
      else
      {
          rest = value & (SUFFIX64(1) << lead0-1)-1;
          guard = (uint32) ((value>> lead0-1) & SUFFIX64(1));
          value >>= (uint64) lead0; /* exponent is zero */
      }
      
      /* Round */
      if( guard ) {
        if( value&1 || rest ) {
          value++;
      
          if( value == (SUFFIX64(1) << 52) ) { /* carry created normal number */
            value = 0;
            set_exponent(value, 1);
          }
        }
      }
    }
  }
  else {                        /* not zero or denorm */
    /* Round to 53 bits */

    rest = value & (SUFFIX64(1)<<10)-1;
    value >>= 10;
    guard = (uint32) value & 1;
    value >>= 1;
    
    /*  value&1 guard   rest    Action
     *  
     *  dc      0       dc      none
     *  1       1       dc      round
     *  0       1       0       none
     *  0       1       !=0     round
     */

    if(guard) {
      if(value&1 || rest) {
        value++;                        /* round */
        if(value>>53) {         /* carry all the way across */
          value >>= 1;          /* renormalize */
          bexp ++;
        }
      }
    }

    /*
     * Check for overflow
     * IEEE Double Precision Format
     * (From Table 7-8 of Kane and Heinrich)
     * 
     * Fraction bits               52
     * Emax                     +1023
     * Emin                     -1022
     * Exponent bias            +1023
     * Exponent bits               11
     * Integer bit             hidden
     * Total width in bits         64
     */
  
    if (bexp > 1024) {          /* overflow */
      return numeric_limits<double>::infinity();
    }
    else {                      /* value is normal */
      value &= ~(SUFFIX64(1) << 52);   /* hide hidden bit */
      set_exponent(value, bexp + 1022); /* add bias */
    }
  }
  return *((double *) &value);
}

double string_to_double(const char * s) {
  const int max_digits = 17;
  unsigned c;
  unsigned negate, decimal_point;
  char *d;
  int exp;
  double x;
  int dpchar;
  char digits[max_digits];

  // Skip leading whitespace, if any.
  const ctype<char>& ct = use_facet<ctype<char> >(locale::classic());
  while (c = *s++, ct.is(ctype_base::space, char(c)))
    ;

  /* process sign */
  negate = 0;
  if (c == '+') {
    c = *s++;
  }
  else if (c == '-') {
    negate = 1;
    c = *s++;
  }
  d = digits;
  dpchar = '.' - '0';
  decimal_point = 0;
  exp = 0;
  for (;;) {
    c -= '0';
    if (c < 10) {
      if (d == digits+max_digits) {
        /* ignore more than 17 digits, but adjust exponent */
        exp += (decimal_point ^ 1);
      }
      else {
        if (c == 0 && d == digits) {
          /* ignore leading zeros */
        }
        else {
          *d++ = (char) c;
        }
        exp -= decimal_point;
      }
    }
    else if (c == (unsigned int) dpchar && !decimal_point) {    /* INTERNATIONAL */
      decimal_point = 1;
    }
    else {
      break;
    }
    c = *s++;
  }
  /* strtod cant return until it finds the end of the exponent */
  if (d == digits) {
    return 0.0;
  }
  if (c == 'e'-'0' || c == 'E'-'0') {
    register unsigned negate_exp = 0;
    register int e = 0;
    c = *s++;
    if (c == '+' || c == ' ') {
      c = *s++;
    }
    else if (c == '-') {
      negate_exp = 1;
      c = *s++;
    }
    if (c -= '0', c < 10) {
      do {
        if (e <= 340) 
          e = e * 10 + (int)c;
        else break;
        c = *s++;
      }
      while (c -= '0', c < 10);
      if (negate_exp) {
        e = -e;
      }
      if (e < -340 || e > 340) 
        exp = e;
      else 
        exp += e;
    }
  }

  if (exp < -340) {
    x = 0;
  }
  else if (exp > 308) {
    x = numeric_limits<double>::infinity();
  }
  else {
    /* let _atod diagnose under- and over-flows */
    /* if the input was == 0.0, we have already returned,
       so retval of +-Inf signals OVERFLOW, 0.0 UNDERFLOW
    */
    x = _atod (digits, (int)(d - digits), exp);
  }
  if (negate) {
    x = -x;
  }
  return x;
}

/*
 * string_to_long_double is just lifted from atold, the difference being
 * that we just use '.' for the decimal point, rather than let it
 * be taken from the current C locale, which of course is not accessible
 * to us.
 */

long double string_to_long_double(const char * s) {
  const int max_digits = 34;
  register unsigned c;
  register unsigned negate, decimal_point;
  register char *d;
  register int exp;
  long double x;
  register int dpchar;
  char digits[max_digits];

  const ctype<char>& ct = use_facet<ctype<char> >(locale::classic());
  while (c = *s++, ct.is(ctype_base::space, char(c)))
    ;

    /* process sign */
  negate = 0;
  if (c == '+') {
    c = *s++;
  }
  else if (c == '-') {
    negate = 1;
    c = *s++;
  }
  d = digits;
  dpchar = '.' -'0';
  decimal_point = 0;
  exp = 0;
  for (;;) {
    c -= '0';
    if (c < 10) {
      if (d == digits+max_digits) {
        /* ignore more than 34 digits, but adjust exponent */
        exp += (decimal_point ^ 1);
      }
      else {
        if (c == 0 && d == digits) {
          /* ignore leading zeros */
          ;
        }
        else {
          *d++ = c;
        }
        exp -= decimal_point;
      }
    }
    else if (int(c) == dpchar && !decimal_point) {    /* INTERNATIONAL */
      decimal_point = 1;
    }
    else {
      break;
    }
    c = *s++;
  }
  if (d == digits) {
    return 0.0L;
  }
  if (c == 'e'-'0' || c == 'E'-'0') {
    register unsigned negate_exp = 0;
    register int e = 0;
    c = *s++;
    if (c == '+' || c == ' ') {
      c = *s++;
    }
    else if (c == '-') {
      negate_exp = 1;
      c = *s++;
    }
    if (c -= '0', c < 10) {
      do {
        if (e <= 340) 
          e = e * 10 + c;
        else break;
        c = *s++;
      }
      while (c -= '0', c < 10);
      if (negate_exp) {
        e = -e;
      }
      if (e < -(323+max_digits) || e > 308) 
        exp = e;
      else 
        exp += e;
    }
  }

  if (exp < -(324+max_digits)) {
    x = 0;
  }
  else if (exp > 308) {
    x = numeric_limits<long double>::infinity();
  }
  else {
    /* let _atod diagnose under- and over-flows */
    /* if the input was == 0.0, we have already returned,
           so retval of +-Inf signals OVERFLOW, 0.0 UNDERFLOW
        */

    double tmp = _atod (digits, (int)(d - digits), exp); // TEMPORARY!!:1
    x = tmp == numeric_limits<double>::infinity()
      ? numeric_limits<long double>::infinity()
      : tmp;
  }
  if (negate) {
    x = -x;
  }
  return x;
}

} // Close unnamed namespace

void __string_to_float(const string& v, float& val) {
  val = string_to_double(v.begin());
}

void __string_to_float(const string& v, double& val) {
  val = string_to_double(v.begin());
}

void __string_to_float(const string& v, long double& val) {
  val = string_to_long_double(v.begin());
}

// __valid_grouping compares two strings, one representing the
// group sizes encountered when reading an integer, and the other
// representing the valid group sizes as returned by the numpunct
// grouping() member function.  Both are interpreted right-to-left.
// The grouping string is treated as if it were extended indefinitely
// with its last value.  For a grouping to be valid, each term in
// the first string must be equal to the corresponding term in the
// second, except for the last, which must be less than or equal.

bool __valid_grouping(const string& group_sizes,
                      const string& prescribed_grouping)
{
  const char * first1 = group_sizes.begin();
  const char * last1  = group_sizes.end();
  const char * first2 = prescribed_grouping.begin();
  const char * last2  = prescribed_grouping.end();

  if (first1 == last1 || first2 == last2) return true;

  --last1; --last2;

  while (first1 != last1) {
    if (*first1 != *first2)
      return false;
    ++first1;
    if (first2 != last2) ++first2;
  }
  return *first1 <= *first2;
}

    
    




//----------------------------------------------------------------------
// num_put

#define NDIG 82
#define MAXECVT 17
#define MAXFCVT 18
#define MAXFSIG MAXECVT
#define MAXESIZ 5


// Helper functions for _M_do_put_float

#define todigit(x) ((x)+'0')

// __format_float formats a mantissa and exponent as returned by
// one of the conversion functions (ecvt_r, fcvt_r, qecvt_r, qfcvt_r)
// according to the specified precision and format flags.  This is
// based on doprnt but is much simpler since it is concerned only
// with floating point input and does not consider all formats.  It
// also does not deal with blank padding, which is handled by
// __copy_float_and_fill. 

void __format_float_scientific(char * buf, const char * bp, 
                              int decpt, int sign, double x,
                              ios_base::fmtflags flags,
                              int precision, bool islong)
{
  char * suffix;
  char expbuf[MAXESIZ + 2];
  // sign if required
  if (sign)
    *buf++ = '-';
  else if (flags & ios_base::showpos)
    *buf++ = '+';
  
  // first digit of mantissa
  *buf++ = *bp++;

  // decimal point if required
  if (precision != 0 || flags & ios_base::showpoint)
    *buf++ = '.';
  // rest of mantissa
  int rz = precision;
  while (rz-- > 0 && *bp != 0)
    *buf++ = *bp++;

  // exponent
  *(suffix = &expbuf[MAXESIZ]) = 0;
  if (x != 0) {
    int nn = decpt - 1;
    if (nn < 0)
      nn = -nn;
    for (; nn > 9; nn /= 10)
      *--suffix = (char) todigit(nn % 10);
    *--suffix = (char) todigit(nn);
  }
        
  // prepend leading zeros to exponent
  while (suffix > &expbuf[MAXESIZ - 2])
    *--suffix = '0';
  
  // put in the exponent sign
  *--suffix = (char) ((decpt > 0 || x == 0) ? '+' : '-');
  
  // put in the e
  *--suffix = flags & ios_base::uppercase ? 'E' : 'e';

  // copy the suffix
  strcpy(buf, suffix);
}
  
void __format_float_fixed(char * buf, const char * bp, 
                          int decpt, int sign, double x,
                          ios_base::fmtflags flags,
                          int precision, bool islong)
{
  if (sign && decpt > -precision && *bp != 0)
    *buf++ = '-';
  else if (flags & ios_base::showpos)
    *buf++ = '+';
  
  int rzero   = 0;
  int nn      = decpt;
  int k       = 0;
  int maxfsig = islong ? 2*MAXFSIG : MAXFSIG;

  do {
    *buf++ = (char) ((nn <= 0 || *bp == 0 || k >= maxfsig) ?
      '0' : (k++, *bp++));
  } while (--nn > 0);

  // decimal point if needed
  if (flags & ios_base::showpoint || precision > 0)
    *buf++ = '.';

  // digits after decimal point if any
  nn = min(precision, MAXFCVT);
  if (precision > nn)
    rzero = precision - nn;
  while (--nn >= 0)
    *buf++ = (++decpt <= 0 || *bp == '\0' || k >= maxfsig)
                ? '0' : (k++, *bp++);

  // trailing zeros if needed
  while (rzero-- > 0)
    *buf++ = '0';
  *buf++ = '\0';
}

// Tests for infinity and NaN differ on different OSs.  We encapsulate
// these differences here.
namespace {
#if defined(__sgi) || defined(__sun__) /* IRIX or Solaris */
  inline bool is_nan_or_inf(double x) { return IsNANorINF(x); }
  inline bool is_inf(double x)        { return IsINF(x); }
  inline bool is_neg_inf(double x)    { return IsNegNAN(x); }
  inline bool is_neg_nan(double x)    { return IsNegNAN(x); }
#elif defined(__GNUC__) && defined(__linux__) /* linux, using gnu c++ */
  inline bool is_nan_or_inf(double x) { return !finite(x); }
  inline bool is_inf(double x)        { return isinf(x); }
  inline bool is_neg_inf(double x)    { return isinf(x) < 0; }
  inline bool is_neg_nan(double x)    { return copysign(1., x) < 0; } 
#elif defined(_MSC_VER) && defined(_WINDOWS)
  inline bool is_nan_or_inf(double x) { return !_finite(x); }
  inline bool is_inf(double x) {
    int fclass = _fpclass(x); 
    return fclass == _FPCLASS_NINF || fclass == _FPCLASS_PINF; 
  }
  inline bool is_neg_inf(double x)    { 
    return _fpclass(x) == _FPCLASS_NINF; 
  }
  inline bool is_neg_nan(double x)    { 
    return _isnan(x) && _copysign(1., x) < 0; 
  }
#endif
} // Close unnamed namespace

void __format_float(char * buf, const char * bp, 
                    int decpt, int sign, double x,
                    ios_base::fmtflags flags,
                    int precision, bool islong)
{
  const char* inf[2] = { "inf", "INF" };
  const char* nan[2] = { "nan", "NAN" };

  // Output of infinities and NANs does not depend on the format flags
  if (is_nan_or_inf(x)) {       // Infinity or NaN
    const char** inf_or_nan = 0;
    if (is_inf(x)) {            // Infinity
      inf_or_nan = inf;
      if (is_neg_inf(x))
        *buf++ = '-';
      else if (flags & ios_base::showpos)
        *buf++ = '+';
    }
    else {                      // NaN
      inf_or_nan = nan;
      if (is_neg_nan(x))
        *buf++ = '-';
      else if (flags & ios_base::showpos)
        *buf++ = '+';
    }
    strcpy(buf, flags & ios_base::uppercase ? inf_or_nan[1] : inf_or_nan[0]);
  }
  else {                        // representable number
    switch (flags & ios_base::floatfield) {
      case ios_base::scientific:
        __format_float_scientific(buf, bp, decpt, sign, x, flags,
                                  precision, islong);
        break;
  
      case ios_base::fixed:
        __format_float_fixed(buf, bp, decpt, sign, x, flags,
                             precision, islong);
        break;
  
      default: // g format
        // establish default precision
        if (flags & ios_base::showpoint || precision > 0) {
          if (precision == 0) precision = 1;
        }
        else
          precision = 6;

        // reset exponent if value is zero
        if (x == 0)
          decpt = 1;

        int kk = precision;
        if (!(flags & ios_base::showpoint)) {
          int n = strlen(bp);
          if (n < kk)
          kk = n;
          while (kk >= 1 && bp[kk-1] == '0')
            --kk;
        }

        if (decpt < -3 || decpt > precision) {
          precision = kk - 1;
          __format_float_scientific(buf, bp, decpt, sign, x,
                                    flags, precision, islong);
        }
        else {
          precision = kk - decpt;
          __format_float_fixed(buf, bp, decpt, sign, x,
                               flags, precision, islong);
        }
      break;
    } /* switch */
  } /* else */
}

// Reentrant versions of floating-point conversion functions.  The argument
// lists look slightly different on different operating systems, so we're
// encapsulating the differences here.
namespace {
#if defined(__sgi) /* IRIX */
  inline char* ecvtR(double x, int n, int* pt, int* sign, char* buf)
    { return ecvt_r(x, n, pt, sign, buf); }
  inline char* fcvtR(double x, int n, int* pt, int* sign, char* buf)
    { return fcvt_r(x, n, pt, sign, buf); }
  inline char* qecvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return qecvt_r(x, n, pt, sign, buf); }
  inline char* qfcvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return qfcvt_r(x, n, pt, sign, buf); }
#elif defined(__GNUC__) && defined(__linux__) /* linux, using gnu c++ */
  inline char* ecvtR(double x, int n, int* pt, int* sign, char* buf)
    { return buf + ecvt_r(x, n, pt, sign, buf, NDIG+2); }
  inline char* fcvtR(double x, int n, int* pt, int* sign, char* buf)
    { return buf + fcvt_r(x, n, pt, sign, buf, NDIG+2); }
  inline char* qecvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return buf + qecvt_r(x, n, pt, sign, buf, NDIG+2); }
  inline char* qfcvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return buf + qfcvt_r(x, n, pt, sign, buf, NDIG+2); }
#elif defined(_MSC_VER) && defined(_WINDOWS)
  inline char* ecvtR(double x, int n, int* pt, int* sign, char* buf)
    {
    strcpy(buf, _ecvt(x, n, pt, sign));
    return buf; 
    }
  inline char* fcvtR(double x, int n, int* pt, int* sign, char* buf)
    {
    strcpy(buf, _fcvt(x, n, pt, sign));
    return buf; 
    }
  inline char* qecvtR(long double x, int n, int* pt, int* sign, char* buf)
    {
    strcpy(buf, _ecvt((double)x, n, pt, sign));
    return buf; 
    }
  inline char* qfcvtR(long double x, int n, int* pt, int* sign, char* buf)
    {
    strcpy(buf, _fcvt((double)x, n, pt, sign));
    return buf; 
    }
#elif defined(__sun__)
  inline char* ecvtR(double x, int n, int* pt, int* sign, char* buf)
    { return econvert(x, n, pt, sign, buf); }
  inline char* fcvtR(double x, int n, int* pt, int* sign, char* buf)
    { return fconvert(x, n, pt, sign, buf); }
  inline char* qecvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return qeconvert(&x, n, pt, sign, buf); }
  inline char* qfcvtR(long double x, int n, int* pt, int* sign, char* buf)
    { return qfconvert(&x, n, pt, sign, buf); }
#endif
} // Close unnamed namespace

char* __write_float(char* buf, ios_base::fmtflags flags, int precision,
                    double x)
{
  char cvtbuf[NDIG+2];
  char * bp;
  int decpt, sign;

  if (flags & ios_base::fixed)
    bp = fcvtR(x, min(precision, MAXFCVT), &decpt, &sign, cvtbuf);
  else
    bp = ecvtR(x, min(precision + 1, MAXECVT),     &decpt, &sign, cvtbuf);

  __format_float(buf, bp, decpt, sign, x, flags, precision, false);

  return buf + strlen(buf);
}

char* __write_float(char* buf, ios_base::fmtflags flags, int precision,
                    long double x)
{
  char cvtbuf[NDIG+2];
  char * bp;
  int decpt, sign;

  if (flags & ios_base::scientific)
    bp = qecvtR(x, min(precision + 1, MAXECVT), &decpt, &sign, cvtbuf);
  else
    bp = qfcvtR(x, min(precision, MAXFCVT),     &decpt, &sign, cvtbuf);

  __format_float(buf, bp, decpt, sign, x, flags, precision, true);

  return buf + strlen(buf);
}

wchar_t*
__convert_float_buffer(const char* first, const char* last, wchar_t* out,
                       const ctype<wchar_t>& ct, wchar_t dot)
{
  ct.widen(first, last, out);
  replace(out, out + (last - first), ct.widen('.'), dot);
  return out + (last - first);
}

void __adjust_float_buffer(char* first, char* last, char dot)
{
  replace(first, last, '.', dot);
}

// Helper functions for _M_do_put_integer

void __make_integer_conversion_spec(char *             cvtspec, 
                                    ios_base::fmtflags flags,
                                    bool               is_signed,
                                    bool               is_long_long) 
{
  *cvtspec++ = '%';
  if (flags & ios_base::showpos)  *cvtspec++ = '+';
  if (flags & ios_base::showbase) *cvtspec++ = '#';

  *cvtspec++ = 'l';             // %l... for long, %ll... for unsigned long.
  if (is_long_long)             // We never construct a conversion specifier
    *cvtspec++ = 'l';           // for any type shorter than long.

  switch (flags & ios_base::basefield) {
    case ios_base::oct:
      *cvtspec++ = 'o'; break;
    case ios_base::hex:
      *cvtspec++ = flags & ios_base::uppercase ? 'X' : 'x'; break;
    default:
      *cvtspec++ = is_signed ? 'd' : 'u';
  }

  *cvtspec++ = 0;
}

char*
__write_integer(char* buf, ios_base::fmtflags flags, long x)
{
  char cvtspec[64];
  __make_integer_conversion_spec(cvtspec, flags, true, false);
  sprintf(buf, cvtspec, x);
  return buf + strlen(buf);
}

char*
__write_integer(char* buf, ios_base::fmtflags flags, unsigned long x)
{
  char cvtspec[64];
  __make_integer_conversion_spec(cvtspec, flags, false, false);
  sprintf(buf, cvtspec, x);
  return buf + strlen(buf);
}

#ifdef __STL_LONG_LONG

char*
__write_integer(char* buf, ios_base::fmtflags flags, long long x)
{
  char cvtspec[64];
  __make_integer_conversion_spec(cvtspec, flags, true, true);
  sprintf(buf, cvtspec, x);
  return buf + strlen(buf);
}

char*
__write_integer(char* buf, ios_base::fmtflags flags, unsigned long long x)
{
  char cvtspec[64];
  __make_integer_conversion_spec(cvtspec, flags, false, true);
  sprintf(buf, cvtspec, x);
  return buf + strlen(buf);
}

#endif /* __STL_LONG_LONG */

// Note that grouping[0] is the number of digits in the *rightmost* group.
// We assume, without checking, that *last is null and that there is enough
// space in the buffer to extend the number past [first, last).
template <class Char>
ptrdiff_t 
__insert_grouping_aux(Char* first, Char* last, const string& grouping,
                      Char separator, Char plus, Char minus,
                      int basechars)
{
  typedef string::size_type str_size;

  if (first == last)
    return 0;

  int sign = 0;

  if (*first == plus || *first == minus) {
    sign = 1;
    ++first;
  }
 
  first += basechars;
  str_size n = 0;               // Index of the current group.
  Char* cur_group = last;       // Points immediately beyond the rightmost
                                // digit of the current group.
  int groupsize = 0;            // Size of the current group.
  
  while (true) {
    groupsize = n < grouping.size() ? grouping[n] : groupsize;
    ++n;

    if (groupsize <= 0 || groupsize >= cur_group - first)
      break;

    // Insert a separator character just before position cur_group - groupsize
    cur_group -= groupsize;
    ++last;
    copy_backward(cur_group, last, last + 1);
    *cur_group = separator;
  }

  return (last - first) + sign + basechars;
}

ptrdiff_t
__insert_grouping(char* first, char* last, const string& grouping,
                  char separator, int basechars)
{
  return __insert_grouping_aux(first, last, grouping, separator,
                               '+', '-', basechars);
}

ptrdiff_t
__insert_grouping(char * first, char * last, const string& grouping,
                  char separator, char plus, char minus, int basechars)
{
  return __insert_grouping_aux(first, last, grouping, 
                               separator, plus, minus, basechars);
}

ptrdiff_t
__insert_grouping(wchar_t* first, wchar_t* last, const string& grouping,
                  wchar_t separator, wchar_t plus, wchar_t minus,
                  int basechars)
{
  return __insert_grouping_aux(first, last, grouping, separator, 
                               plus, minus, basechars);
}

//----------------------------------------------------------------------
// Force instantiation of of num_get<> and num_put<>

template class num_get<char, istreambuf_iterator<char> >;
template class num_get<char, const char*>;
template class num_put<char, ostreambuf_iterator<char> >;
template class num_put<char, char*>;

#ifdef INSTANTIATE_WIDE_STREAMS
template class num_put<wchar_t, ostreambuf_iterator<wchar_t> >;
template class num_put<wchar_t, wchar_t*>;
template class num_get<wchar_t, istreambuf_iterator<wchar_t> >;
template class num_get<wchar_t, const wchar_t*>;
#endif /* INSTANTIATE_WIDE_STREAMS */

__STL_END_NAMESPACE

// Local Variables:
// mode:C++
// End:
