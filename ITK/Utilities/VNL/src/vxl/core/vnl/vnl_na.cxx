// This is core/vnl/vnl_na.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_na.h"
#include <vxl_config.h>
#include <vcl_istream.h>
#include <vcl_ios.h>

//: A particular qNaN to indicate not available.
// This returns the bit pattern 0x7ff00000000007a2, as used by Octave and R
// Don't assume that any VXL functions will treat the value as NA rather than NaN, unless
// explicitly documented.
double vnl_na(double)
{
  double a;

#if VXL_HAS_INT_64
  *reinterpret_cast<vxl_uint_64*>(&a) = 0x7ff00000000007a2LL;
#else
# if VXL_BIG_ENDIAN
#  define hw 0
#  define lw 1
# else  // VXL_LITTLE_ENDIAN
#  define hw 1
#  define lw 0
# endif
  reinterpret_cast<vxl_uint_32*>(&a)[hw]=0x7ff00000;
  reinterpret_cast<vxl_uint_32*>(&a)[lw]=0x000007a2;
#endif

  return a;
}



//: A particular qNaN to indicate not available.
// This returns the bit pattern 0x7f8007a2
// Don't assume that any VXL functions will treat the value as NA rather than NaN, unless
// explicitly documented.
float vnl_na(float)
{
  float a;

  *reinterpret_cast<vxl_uint_32*>(&a) = 0x7f8007a2L;

  return a;
}


//: True if parameter is specific NA qNaN.
// Tests for bit pattern 0x7ff00000000007a2, as used by Octave and R
bool vnl_na_isna(double x)
{
#if VXL_HAS_INT_64
  return ((*reinterpret_cast<vxl_uint_64*>(&x))&0xfff7ffffffffffffLL) // ignore signalling bit
    == 0x7ff00000000007a2LL;
#else
  return ((reinterpret_cast<vxl_int_32*>(&x)[hw]) & 0xfff7ffff) == 0x7ff00000 &&
         reinterpret_cast<vxl_int_32*>(&x)[lw] == 0x000007a2;
#endif
}

//: True if parameter is specific NA qNaN.
// Tests for bit pattern 0x7F8007a2
bool vnl_na_isna(float x)
{
  return ((*reinterpret_cast<vxl_uint_32*>(&x))&0xffbfffffL) // ignore signalling bit
    == 0x7f8007a2L;
}

//: Read a floating point number or "NA" from a stream.
template <class T> inline void vnl_na_extract_type(vcl_istream &is, T& x)
{
  if (!is) return;
  is >> x;

  if (!!is || is.eof()) return;
  is.clear();

  char c=' ';
  is >> c;
  if (c != 'N' && c!='n')
  {
    is.putback(c);
    is.clear(vcl_ios::badbit);
    return;
  }
  is >> c;
  if (c != 'A' && c!='a')
  {
    is.putback(c);
    is.clear(vcl_ios::badbit);
    return;
  }
  x = vnl_na(T());
}

void vnl_na_extract(vcl_istream &is, double& x) { vnl_na_extract_type(is, x); }
void vnl_na_extract(vcl_istream &is, float& x) { vnl_na_extract_type(is, x); }

//: Write a floating point number or "NA" to a stream.
void vnl_na_insert(vcl_ostream &os, double x)
{
  if (vnl_na_isna(x))
    os << "NA";
  else
    os << x;
}

//: Write a floating point number or "NA" to a stream.
void vnl_na_insert(vcl_ostream &os, float x)
{
  if (vnl_na_isna(x))
    os << "NA";
  else
    os << x;
}
//----------------------------------------------------------------------
