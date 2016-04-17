// This is core/vnl/vnl_na.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include <istream>
#include <sstream>
#include <cctype>
#include <string>
#include "vnl_na.h"

#include <vxl_config.h>
#include <vcl_compiler.h>
#include <vnl/vnl_math.h>

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


//: Replace NaNs with NA, leave other values alone.
double vnl_na_nan_to_na(double v)
{
  return vnl_math::isnan(v) ? vnl_na(double()) : v;
}

//: Replace NaNs with NA, leave other values alone.
float vnl_na_nan_to_na(float v)
{
  return vnl_math::isnan(v) ? vnl_na(float()) : v;
}

//: Read a floating point number or "NA" from a stream.
template <class T> inline void vnl_na_extract_type(std::istream &is, T& value)
{
  if (!is) return;
  std::stringstream oneToken("");
  unsigned int char_processed_count = 0;
  bool period_found = false;
  bool current_location_is_delimiter = false;

  while (!is.eof()) {
    std::stringstream::char_type c;
    std::istream::int_type p = is.peek();
    if ( char_processed_count == 0 ) { //The first character is the start of the token of interest.
      if (std::isspace(p)) {
        is.get(c); // Gobble up the peeked at character
        continue; //Gobble up preceeding white space
      }
      if ( p == 'N' || p == 'n' ) {
        is.get(c);// Gobble up the N
        p = is.peek();
        if (p == 'A' || p == 'a') {
          is.get(c); // Gobble up the A
          value = vnl_na(T());
          return;
        }
        else
        {
          std::string checkForNAString;
          is >> checkForNAString; //Gobble up the rest of the token, whatever that is
          value = 999.999; // Invalid parsing occured
          return;
        }
      }
    }
    // Find if character is candidate for float values
    if (std::isdigit(p) || p == '-' || p == '+' || p == '.') {
      // After the first character, sign character is delimiter
      if ((char_processed_count != 0) && ((p == '-') || (p == '+'))) {
        current_location_is_delimiter = true;
      }
      if (p == '.') {
        if (period_found) //Second period encountered is a delimiter
        {
          current_location_is_delimiter = true;
        }
        period_found = true;
      }
    }
    else // All other characters are delimiters
    {
      current_location_is_delimiter = true;
    }
    if ( current_location_is_delimiter) {
      break;
    }
    else {
      std::stringstream::char_type pp=' ';
      is.get(pp); //Gobble up peeked character
      oneToken << pp;
    }
    ++char_processed_count;
  }
  oneToken >> value;
}

void vnl_na_extract(std::istream &is, double& x) { vnl_na_extract_type(is, x); }
void vnl_na_extract(std::istream &is, float& x) { vnl_na_extract_type(is, x); }

//: Write a floating point number or "NA" to a stream.
void vnl_na_insert(std::ostream &os, double x)
{
  if (vnl_na_isna(x))
    os << "NA";
  else
    os << x;
}

//: Write a floating point number or "NA" to a stream.
void vnl_na_insert(std::ostream &os, float x)
{
  if (vnl_na_isna(x))
    os << "NA";
  else
    os << x;
}
//----------------------------------------------------------------------
