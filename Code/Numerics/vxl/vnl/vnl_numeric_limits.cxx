#ifdef __GNUC__
#pragma implementation
#endif
// This is vxl/vnl/vnl_numeric_limits.cxx

//
// numeric_limits
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 28 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_numeric_limits.h"

union vnl_numeric_limits_double_nan {
  double nan;
  unsigned char x[8];
  
  vnl_numeric_limits_double_nan() {
    x[0] = 0x7f;
    x[1] = 0xff;
    x[2] = 0xff;
    x[3] = 0xff;
    x[4] = 0xff;
    x[5] = 0xff;
    x[6] = 0xff;
    x[7] = 0xff;
  }
};
static vnl_numeric_limits_double_nan dnan;

union vnl_numeric_limits_double_inf {
  double inf;
  unsigned char x[8];
  
  vnl_numeric_limits_double_inf() {
    x[0] = 0x7f;
    x[1] = 0xf0;
    x[2] = 0x00;
    x[3] = 0x00;
    x[4] = 0x00;
    x[5] = 0x00;
    x[6] = 0x00;
    x[7] = 0x00;
  }
};
static vnl_numeric_limits_double_inf dinf;

//VCL_DEFINE_SPECIALIZATION
double vnl_numeric_limits<double>::infinity()
{
  return dinf.inf;
}

//VCL_DEFINE_SPECIALIZATION
double vnl_numeric_limits<double>::quiet_NaN()
{
  return dnan.nan;
}

//VCL_DEFINE_SPECIALIZATION
double vnl_numeric_limits<double>::signaling_NaN()
{
  return quiet_NaN();
}

// *****************************************************************************
union vnl_numeric_limits_float_nan {
  float nan;
  unsigned char x[4];
  
  vnl_numeric_limits_float_nan() {
    x[0] = 0x7f;
    x[1] = 0xff;
    x[2] = 0xff;
    x[3] = 0xff;
  }
};
static vnl_numeric_limits_float_nan fnan;

union vnl_numeric_limits_float_inf {
  float inf;
  unsigned char x[4];
  
  vnl_numeric_limits_float_inf() {
    x[0] = 0x7f;
    x[1] = 0x80;
    x[2] = 0x00;
    x[3] = 0x00;
  }
};
static vnl_numeric_limits_float_inf finf;

//VCL_DEFINE_SPECIALIZATION
float vnl_numeric_limits<float>::infinity()
{
  return finf.inf;
}

//VCL_DEFINE_SPECIALIZATION
float vnl_numeric_limits<float>::quiet_NaN()
{
  return fnan.nan;
}

//VCL_DEFINE_SPECIALIZATION
float vnl_numeric_limits<float>::signaling_NaN()
{
  return quiet_NaN();
}

#if !VCL_CAN_STATIC_CONST_INIT_INT
// float
const bool vnl_numeric_limits<float>::is_specialized = true;
const int  vnl_numeric_limits<float>::digits   = 24;
const int  vnl_numeric_limits<float>::digits10 =  6;
const bool vnl_numeric_limits<float>::is_signed  = true;
const bool vnl_numeric_limits<float>::is_integer = false;
const bool vnl_numeric_limits<float>::is_exact   = false;
const int  vnl_numeric_limits<float>::radix = 2;
const int  vnl_numeric_limits<float>::min_exponent   = -125;
const int  vnl_numeric_limits<float>::min_exponent10 = -37;
const int  vnl_numeric_limits<float>::max_exponent   = 128;
const int  vnl_numeric_limits<float>::max_exponent10 = 38;
const bool vnl_numeric_limits<float>::has_infinity      = true;
const bool vnl_numeric_limits<float>::has_quiet_NaN     = true;
const bool vnl_numeric_limits<float>::has_signaling_NaN = true;
const bool vnl_numeric_limits<float>::has_denorm        = false;
const bool vnl_numeric_limits<float>::is_iec559  = true;
const bool vnl_numeric_limits<float>::is_bounded = true;
const bool vnl_numeric_limits<float>::is_modulo  = false;
const bool vnl_numeric_limits<float>::traps      = true;
const bool vnl_numeric_limits<float>::tinyness_before = true;
const vnl_float_round_style vnl_numeric_limits<float>::round_style = vnl_round_to_nearest;
// double
const bool vnl_numeric_limits<double>::is_specialized = true;
const int  vnl_numeric_limits<double>::digits   = 53;
const int  vnl_numeric_limits<double>::digits10 =  15;
const bool vnl_numeric_limits<double>::is_signed  = true;
const bool vnl_numeric_limits<double>::is_integer = false;
const bool vnl_numeric_limits<double>::is_exact   = false;
const int  vnl_numeric_limits<double>::radix = 2;
const int  vnl_numeric_limits<double>::min_exponent   = -1021;
const int  vnl_numeric_limits<double>::min_exponent10 = -307;
const int  vnl_numeric_limits<double>::max_exponent   = 1024;
const int  vnl_numeric_limits<double>::max_exponent10 = 308;
const bool vnl_numeric_limits<double>::has_infinity      = true;
const bool vnl_numeric_limits<double>::has_quiet_NaN     = true;
const bool vnl_numeric_limits<double>::has_signaling_NaN = true;
const bool vnl_numeric_limits<double>::has_denorm        = true;
const bool vnl_numeric_limits<double>::is_iec559  = true;
const bool vnl_numeric_limits<double>::is_bounded = true;
const bool vnl_numeric_limits<double>::is_modulo  = false;
const bool vnl_numeric_limits<double>::traps      = true;
const bool vnl_numeric_limits<double>::tinyness_before = true;
const vnl_float_round_style vnl_numeric_limits<double>::round_style = vnl_round_to_nearest;
#endif
