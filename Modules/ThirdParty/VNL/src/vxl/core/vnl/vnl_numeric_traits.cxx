// This is core/vnl/vnl_numeric_traits.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 12 Feb 98
//
//-----------------------------------------------------------------------------

#include "vnl_numeric_traits.h"
#include <vcl_complex.h>
#include <vxl_config.h>

static const long s16 = 0x7fffL;
static const unsigned long u16 = 0xffffL;
static const long s32 = 0x7fffffffL;
static const unsigned long u32 = 0xffffffffL;
#if VXL_HAS_INT_64 // need this arithmetic magic to avoid compiler errors
static const vxl_uint_64 u64 = (vxl_uint_64)(-1);
static const vxl_sint_64 s64 = u64/2;
#else // dummy
static const long s64 = 0L;
static const unsigned long u64 = 0L;
#endif

#if !VCL_STATIC_CONST_INIT_INT_NO_DEFN
const bool vnl_numeric_traits<bool>::zero VCL_STATIC_CONST_INIT_INT_DEFN(false);
const char vnl_numeric_traits<char>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const unsigned char vnl_numeric_traits<unsigned char>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const signed char vnl_numeric_traits<signed char>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const short vnl_numeric_traits<short>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const unsigned short vnl_numeric_traits<unsigned short>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const int vnl_numeric_traits<int>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const unsigned int vnl_numeric_traits<unsigned int>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const long vnl_numeric_traits<long>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
const unsigned long vnl_numeric_traits<unsigned long>::zero VCL_STATIC_CONST_INIT_INT_DEFN(0);
#endif

#if !VCL_STATIC_CONST_INIT_INT_NO_DEFN
const bool vnl_numeric_traits<bool>::one VCL_STATIC_CONST_INIT_INT_DEFN(true);
const char vnl_numeric_traits<char>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const unsigned char vnl_numeric_traits<unsigned char>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const signed char vnl_numeric_traits<signed char>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const short vnl_numeric_traits<short>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const unsigned short vnl_numeric_traits<unsigned short>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const int vnl_numeric_traits<int>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const unsigned int vnl_numeric_traits<unsigned int>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const long vnl_numeric_traits<long>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
const unsigned long vnl_numeric_traits<unsigned long>::one VCL_STATIC_CONST_INIT_INT_DEFN(1);
#endif

#if !VCL_STATIC_CONST_INIT_INT_NO_DEFN
const bool vnl_numeric_traits<bool>::maxval VCL_STATIC_CONST_INIT_INT_DEFN(true);
const char vnl_numeric_traits<char>::maxval VCL_STATIC_CONST_INIT_INT_DEFN(char(255)<0?127:255);
//  It is 127 when "char" is signed and 255 when "char" is unsigned.
const unsigned char vnl_numeric_traits<unsigned char>::maxval VCL_STATIC_CONST_INIT_INT_DEFN(255);
const signed char vnl_numeric_traits<signed char>::maxval VCL_STATIC_CONST_INIT_INT_DEFN(127);
#endif

const short vnl_numeric_traits<short>::maxval = s16;
const unsigned short vnl_numeric_traits<unsigned short>::maxval = u16;
const int vnl_numeric_traits<int>::maxval = sizeof(int)==4?s32:s16;
const unsigned int vnl_numeric_traits<unsigned int>::maxval = sizeof(unsigned int)==4?u32:u16;
const long vnl_numeric_traits<long>::maxval = sizeof(long)==8?s64:s32;
const unsigned long vnl_numeric_traits<unsigned long>::maxval = sizeof(unsigned long)==8?u64:u32;

#if !VCL_STATIC_CONST_INIT_FLOAT_NO_DEFN
const float vnl_numeric_traits<float>::zero VCL_STATIC_CONST_INIT_FLOAT_DEFN(0.0F);
const double vnl_numeric_traits<double>::zero VCL_STATIC_CONST_INIT_FLOAT_DEFN(0.0);
const long double vnl_numeric_traits<long double>::zero VCL_STATIC_CONST_INIT_FLOAT_DEFN(0.0);

const float vnl_numeric_traits<float>::one VCL_STATIC_CONST_INIT_FLOAT_DEFN(1.0F);
const double vnl_numeric_traits<double>::one VCL_STATIC_CONST_INIT_FLOAT_DEFN(1.0);
const long double vnl_numeric_traits<long double>::one VCL_STATIC_CONST_INIT_FLOAT_DEFN(1.0);

const float vnl_numeric_traits<float>::maxval VCL_STATIC_CONST_INIT_FLOAT_DEFN(3.40282346638528860e+38F);
const double vnl_numeric_traits<double>::maxval VCL_STATIC_CONST_INIT_FLOAT_DEFN(1.7976931348623157E+308);
const long double vnl_numeric_traits<long double>::maxval VCL_STATIC_CONST_INIT_FLOAT_DEFN(1.7976931348623157E+308);
#endif

// Must use constructor-call syntax for initialization of complex
// specializations for Borland compiler.
const vcl_complex<float> vnl_numeric_traits<vcl_complex<float> >::zero(0.0f);
const vcl_complex<double> vnl_numeric_traits<vcl_complex<double> >::zero(0.0);
const vcl_complex<long double> vnl_numeric_traits<vcl_complex<long double> >::zero(0.0);

const vcl_complex<float> vnl_numeric_traits<vcl_complex<float> >::one(1.0f);
const vcl_complex<double> vnl_numeric_traits<vcl_complex<double> >::one(1.0);
const vcl_complex<long double> vnl_numeric_traits<vcl_complex<long double> >::one(1.0);

// Unknown, so undefined. Will cause link errors if someone refers to it.
//const vcl_complex<float> vnl_numeric_traits<vcl_complex<float> >::maxval;
//const vcl_complex<double> vnl_numeric_traits<vcl_complex<double> >::maxval;
//const vcl_complex<long double> vnl_numeric_traits<vcl_complex<long double> >::maxval;

//--------------------------------------------------------------------------------
