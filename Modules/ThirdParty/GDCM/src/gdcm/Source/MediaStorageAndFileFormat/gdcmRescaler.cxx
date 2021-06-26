/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmRescaler.h"
#include <limits>
#include <algorithm> // std::max
#include <stdlib.h> // abort
#include <string.h> // memcpy
#include <cmath> // std::lround

namespace gdcm
{

// parameter 'size' is in bytes
template <typename TOut, typename TIn>
void RescaleFunction(TOut *out, const TIn *in, double intercept, double slope, size_t size)
{
  size /= sizeof(TIn);
  for(size_t i = 0; i != size; ++i)
    {
    // Implementation detail:
    // The rescale function does not add the usual +0.5 to do the proper integer type
    // cast, since TOut is expected to be floating point type whenever it would occur
    out[i] = (TOut)(slope * in[i] + intercept);
    //assert( out[i] == (TOut)(slope * in[i] + intercept) ); // will really slow down stuff...
    //assert( in[i] == (TIn)(((double)out[i] - intercept) / slope + 0.5) );

    // For image such as: gdcmData/MR16BitsAllocated_8BitsStored.dcm, the following line will not work:
    // Indeed the pixel declares itself as 16/8/7 with pixel representation of 1. In this case
    // anything outside the range [-127,128] is required to be discarded !
    //assert( (TIn)out[i] == in[i] );
    }
}

// no such thing as partial specialization of function in c++
// so instead use this trick:
template<typename TOut, typename TIn>
struct FImpl;

template<typename TOut, typename TIn>
void InverseRescaleFunction(TOut *out, const TIn *in, double intercept, double slope, size_t size)
{ FImpl<TOut,TIn>::InverseRescaleFunction(out,in,intercept,slope,size); } // users, don't touch this!

template<typename TOut, typename TIn>
struct FImpl
{
  // parameter 'size' is in bytes
  // TODO: add template parameter for intercept/slope so that we can have specialized instantiation
  // when 1. both are int, 2. slope is 1, 3. intercept is 0
  // Detail: casting from float to int is soooo slow
  static void InverseRescaleFunction( TOut *out, const TIn *in,
    double intercept, double slope, size_t size) // users, go ahead and specialize this
    {
    // If you read the code down below you'll see a specialized function for float, thus
    // if we reach here it pretty much means slope/intercept were integer type
    assert( intercept == (int)intercept );
    assert( slope == (int)slope );
    size /= sizeof(TIn);
    for(size_t i = 0; i != size; ++i)
      {
      // '+ 0.5' trick is NOT needed for image such as: gdcmData/D_CLUNIE_CT1_J2KI.dcm
      out[i] = (TOut)(((double)in[i] - intercept) / slope );
      }
    }
};

// http://stackoverflow.com/questions/485525/round-for-float-in-c
// http://en.cppreference.com/w/c/numeric/math/round
template < typename T >
static inline T round_impl(const double d)
{
  // round() is C99, std::round() is C++11
  return (T)std::lround(d);
}

template<typename TOut>
struct FImpl<TOut, float>
{
  static void InverseRescaleFunction(TOut *out, const float *in,
    double intercept, double slope, size_t size)
    {
    size /= sizeof(float);
    for(size_t i = 0; i != size; ++i)
      {
      // '+ 0.5' trick is needed for instance for : gdcmData/MR-MONO2-12-shoulder.dcm
      // well known trick of adding 0.5 after a floating point type operation to properly find the
      // closest integer that will represent the transformation
      // TOut in this case is integer type, while input is floating point type
      out[i] = round_impl<TOut>(((double)in[i] - intercept) / slope);
      //assert( out[i] == (TOut)(((double)in[i] - intercept) / slope ) );
      }
    }
};
template<typename TOut>
struct FImpl<TOut, double>
{
  static void InverseRescaleFunction(TOut *out, const double *in,
    double intercept, double slope, size_t size)
    {
    size /= sizeof(double);
    for(size_t i = 0; i != size; ++i)
      {
      // '+ 0.5' trick is needed for instance for : gdcmData/MR-MONO2-12-shoulder.dcm
      // well known trick of adding 0.5 after a floating point type operation to properly find the
      // closest integer that will represent the transformation
      // TOut in this case is integer type, while input is floating point type
      out[i] = round_impl<TOut>(((double)in[i] - intercept) / slope);
      //assert( out[i] == (TOut)(((double)in[i] - intercept) / slope ) );
      }
    }
};

static inline PixelFormat::ScalarType ComputeBestFit(const PixelFormat &pf, double intercept, double slope)
{
  PixelFormat::ScalarType st = PixelFormat::UNKNOWN;
  assert( slope == (int)slope && intercept == (int)intercept);

  assert( pf.GetMin() <= pf.GetMax() );
  const double pfmin = slope >= 0. ? (double)pf.GetMin() : (double)pf.GetMax();
  const double pfmax = slope >= 0. ? (double)pf.GetMax() : (double)pf.GetMin();
  const double min = slope * pfmin + intercept;
  const double max = slope * pfmax + intercept;
  assert( min <= max );
  assert( min == (int64_t)min && max == (int64_t)max );
  if( min >= 0 ) // unsigned
    {
    if( max <= std::numeric_limits<uint8_t>::max() )
      {
      st = PixelFormat::UINT8;
      }
    else if( max <= std::numeric_limits<uint16_t>::max() )
      {
      st = PixelFormat::UINT16;
      }
    else if( max <= std::numeric_limits<uint32_t>::max() )
      {
      st = PixelFormat::UINT32;
      }
    else if( max <= static_cast<double>(std::numeric_limits<uint64_t>::max()) )
      {
      // very large value in Rescale Slope ?
      return PixelFormat::FLOAT64;
      }
    else
      {
      gdcmErrorMacro( "Unhandled Pixel Format" );
      return st;
      }
    }
  else
    {
    if( max <= std::numeric_limits<int8_t>::max()
     && min >= std::numeric_limits<int8_t>::min() )
      {
      st = PixelFormat::INT8;
      }
    else if( max <= std::numeric_limits<int16_t>::max()
      && min >= std::numeric_limits<int16_t>::min() )
      {
      st = PixelFormat::INT16;
      }
    else if( max <= std::numeric_limits<int32_t>::max()
      && min >= std::numeric_limits<int32_t>::min() )
      {
      st = PixelFormat::INT32;
      }
    else if( max <= static_cast<double>(std::numeric_limits<int64_t>::max())
      && min >= static_cast<double>(std::numeric_limits<int64_t>::min() ) )
      {
      // very large value in Rescale Slope ?
      return PixelFormat::FLOAT64;
      }
    else
      {
      gdcmErrorMacro( "Unhandled Pixel Format" );
      return st;
      }
    }
  // postcondition:
  assert( min >= PixelFormat(st).GetMin() );
  assert( max <= PixelFormat(st).GetMax() );
  assert( st != PixelFormat::UNKNOWN );
  return st;
}

PixelFormat::ScalarType Rescaler::ComputeInterceptSlopePixelType()
{
  assert( PF != PixelFormat::UNKNOWN );
  if( PF.GetSamplesPerPixel() != 1 )
    {
    gdcmErrorMacro( "Sample Per Pixel is required to be 1" );
    return PF;
    }
  PixelFormat::ScalarType output = PixelFormat::UNKNOWN;
  if( PF == PixelFormat::SINGLEBIT ) return PixelFormat::SINGLEBIT;
  if( Slope != (int)Slope || Intercept != (int)Intercept)
    {
    //assert( PF != PixelFormat::INT8 && PF != PixelFormat::UINT8 ); // Is there any Object that have Rescale on char ?
    assert( PF != PixelFormat::SINGLEBIT );
    return PixelFormat::FLOAT64;
    }
  //if( PF.IsValid() )
    {
    const double intercept = Intercept;
    const double slope = Slope;
    output = ComputeBestFit (PF,intercept,slope);
    }
  return output;
}

template <typename TIn>
void Rescaler::RescaleFunctionIntoBestFit(char *out8, const TIn *in, size_t n)
{
  double intercept = Intercept;
  double slope = Slope;
  PixelFormat::ScalarType output = ComputeInterceptSlopePixelType();
  void *out = out8;
  if( UseTargetPixelType )
    {
    output = TargetScalarType;
    }
  switch(output)
    {
  case PixelFormat::SINGLEBIT:
    assert(0);
    break;
  case PixelFormat::UINT8:
    RescaleFunction<uint8_t,TIn>((uint8_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT8:
    RescaleFunction<int8_t,TIn>((int8_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::UINT16:
    RescaleFunction<uint16_t,TIn>((uint16_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT16:
    RescaleFunction<int16_t,TIn>((int16_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::UINT32:
    RescaleFunction<uint32_t,TIn>((uint32_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT32:
    RescaleFunction<int32_t,TIn>((int32_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::FLOAT32:
    RescaleFunction<float,TIn>((float*)out,in,intercept,slope,n);
    break;
  case PixelFormat::FLOAT64:
    RescaleFunction<double,TIn>((double*)out,in,intercept,slope,n);
    break;
  default:
    assert(0);
    break;
    }
 }

template <typename TIn>
void Rescaler::InverseRescaleFunctionIntoBestFit(char *out8, const TIn *in, size_t n)
{
  const double intercept = Intercept;
  const double slope = Slope;
  PixelFormat output = ComputePixelTypeFromMinMax();
  void *out = out8;
  switch(output)
    {
  case PixelFormat::SINGLEBIT:
    assert(0);
    break;
  case PixelFormat::UINT8:
    InverseRescaleFunction<uint8_t,TIn>((uint8_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT8:
    InverseRescaleFunction<int8_t,TIn>((int8_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::UINT16:
    InverseRescaleFunction<uint16_t,TIn>((uint16_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT16:
    InverseRescaleFunction<int16_t,TIn>((int16_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::UINT32:
    InverseRescaleFunction<uint32_t,TIn>((uint32_t*)out,in,intercept,slope,n);
    break;
  case PixelFormat::INT32:
    InverseRescaleFunction<int32_t,TIn>((int32_t*)out,in,intercept,slope,n);
    break;
  default:
    assert(0);
    break;
    }
 }


bool Rescaler::InverseRescale(char *out, const char *in8, size_t n)
{
  bool fastpath = true;
  const void* in = in8;
  switch(PF)
    {
  case PixelFormat::FLOAT32:
  case PixelFormat::FLOAT64:
    fastpath = false;
    break;
  default:
    ;
    }

  // fast path:
  if( fastpath && (Slope == 1 && Intercept == 0) )
    {
    memcpy(out,in,n);
    return true;
    }
  // check if we are dealing with floating point type
  if( Slope != (int)Slope || Intercept != (int)Intercept)
    {
    // need to rescale as double (64bits) as slope/intercept are 64bits
    //assert(0);
    }
  // else integral type
  switch(PF)
    {
  case PixelFormat::UINT8:
    InverseRescaleFunctionIntoBestFit<uint8_t>(out,(const uint8_t*)in,n);
    break;
  case PixelFormat::INT8:
    InverseRescaleFunctionIntoBestFit<int8_t>(out,(const int8_t*)in,n);
    break;
  case PixelFormat::UINT16:
    InverseRescaleFunctionIntoBestFit<uint16_t>(out,(const uint16_t*)in,n);
    break;
  case PixelFormat::INT16:
    InverseRescaleFunctionIntoBestFit<int16_t>(out,(const int16_t*)in,n);
    break;
  case PixelFormat::UINT32:
    InverseRescaleFunctionIntoBestFit<uint32_t>(out,(const uint32_t*)in,n);
    break;
  case PixelFormat::INT32:
    InverseRescaleFunctionIntoBestFit<int32_t>(out,(const int32_t*)in,n);
    break;
  case PixelFormat::FLOAT32:
    assert( sizeof(float) == 32 / 8 );
    InverseRescaleFunctionIntoBestFit<float>(out,(const float*)in,n);
    break;
  case PixelFormat::FLOAT64:
    assert( sizeof(double) == 64 / 8 );
    InverseRescaleFunctionIntoBestFit<double>(out,(const double*)in,n);
    break;
  default:
    assert(0);
    break;
    }

  return true;
}

bool Rescaler::Rescale(char *out, const char *in8, size_t n)
{
  const void *in = in8;
  if( UseTargetPixelType == false )
    {
    // fast path:
    if( Slope == 1 && Intercept == 0 )
      {
      memcpy(out,in,n);
      return true;
      }
    // check if we are dealing with floating point type
    if( Slope != (int)Slope || Intercept != (int)Intercept)
      {
      // need to rescale as float (32bits) as slope/intercept are 32bits
      }
    }
  // else integral type
  switch(PF)
    {
  case PixelFormat::SINGLEBIT:
    memcpy(out,in,n);
    break;
  case PixelFormat::UINT8:
    RescaleFunctionIntoBestFit<uint8_t>(out,(const uint8_t*)in,n);
    break;
  case PixelFormat::INT8:
    RescaleFunctionIntoBestFit<int8_t>(out,(const int8_t*)in,n);
    break;
  case PixelFormat::UINT12:
  case PixelFormat::UINT16:
    RescaleFunctionIntoBestFit<uint16_t>(out,(const uint16_t*)in,n);
    break;
  case PixelFormat::INT12:
  case PixelFormat::INT16:
    RescaleFunctionIntoBestFit<int16_t>(out,(const int16_t*)in,n);
    break;
  case PixelFormat::UINT32:
    RescaleFunctionIntoBestFit<uint32_t>(out,(const uint32_t*)in,n);
    break;
  case PixelFormat::INT32:
    RescaleFunctionIntoBestFit<int32_t>(out,(const  int32_t*)in,n);
    break;
  default:
    gdcmErrorMacro( "Unhandled: " << PF );
    assert(0);
    break;
    }

  return true;
}

static PixelFormat ComputeInverseBestFitFromMinMax(/*const PixelFormat &pf,*/ double intercept, double slope, double _min, double _max)
{
  PixelFormat st = PixelFormat::UNKNOWN;
  //assert( slope == (int)slope && intercept == (int)intercept);

  assert( _min <= _max );
  double dmin = (_min - intercept ) / slope;
  double dmax = (_max - intercept ) / slope;
  if( slope < 0 )
    {
    dmin = (_max - intercept ) / slope;
    dmax = (_min - intercept ) / slope;
    }
  assert( dmin <= dmax );
  assert( dmax <= static_cast<double>(std::numeric_limits<int64_t>::max() ) );
  assert( dmin >= static_cast<double>(std::numeric_limits<int64_t>::min() ) );
  /*
   * Tricky: what happen in the case where floating point approximate dmax as: 65535.000244081035
   * Take for instance: _max = 64527, intercept = -1024, slope = 1.000244140625
   * => dmax = 65535.000244081035
   * thus we must always make sure to cast to an integer first.
   */
  int64_t min = (int64_t)dmin;
  int64_t max = (int64_t)dmax;

  int log2max = 0;

  if( min >= 0 ) // unsigned
    {
    if( max <= std::numeric_limits<uint8_t>::max() )
      {
      st = PixelFormat::UINT8;
      }
    else if( max <= std::numeric_limits<uint16_t>::max() )
      {
      st = PixelFormat::UINT16;
      }
    else if( max <= std::numeric_limits<uint32_t>::max() )
      {
      st = PixelFormat::UINT32;
      }
    else
      {
      gdcmAssertAlwaysMacro(0);
      }
    int64_t max2 = max; // make a copy
    while (max2 >>= 1) ++log2max;
    // need + 1 in case max == 4095 => 12bits stored required
    st.SetBitsStored( (unsigned short)(log2max + 1) );
    }
  else
    {
    if( max <= std::numeric_limits<int8_t>::max()
      && min >= std::numeric_limits<int8_t>::min() )
      {
      st = PixelFormat::INT8;
      }
    else if( max <= std::numeric_limits<int16_t>::max()
      && min >= std::numeric_limits<int16_t>::min() )
      {
      st = PixelFormat::INT16;
      }
    else if( max <= std::numeric_limits<int32_t>::max()
      && min >= std::numeric_limits<int32_t>::min() )
      {
      st = PixelFormat::INT32;
      }
    else
      {
      gdcmAssertAlwaysMacro(0);
      }
    assert( min < 0 );
#if 0
    int log2min = 0;
    int64_t min2 = -min; // make a copy
    int64_t max2 = max; // make a copy
    while (min2 >>= 1) ++log2min;
    while (max2 >>= 1) ++log2max;
    const int64_t bs = std::max( log2min, log2max ) + 1 + 1 /* 2 complement */;
#else
    int64_t max2 = max - min; // make a copy
    while (max2 >>= 1) ++log2max;
    const int64_t bs = log2max + 1;
#endif
    assert( bs <= st.GetBitsAllocated() );
    st.SetBitsStored( (unsigned short)bs );
    }
  // postcondition:
  assert( min >= PixelFormat(st).GetMin() );
  assert( max <= PixelFormat(st).GetMax() );
  assert( st != PixelFormat::UNKNOWN );
  assert( st != PixelFormat::FLOAT32 && st != PixelFormat::FLOAT16 && st != PixelFormat::FLOAT64 );
  return st;
}

void Rescaler::SetMinMaxForPixelType(double min, double max)
{
  if( min < max )
  {
    ScalarRangeMin = min;
    ScalarRangeMax = max;
  }
  else
  {
    gdcmWarningMacro( "Min > Max. Correcting" );
    ScalarRangeMin = max;
    ScalarRangeMax = min;
  }
}

PixelFormat Rescaler::ComputePixelTypeFromMinMax()
{
  assert( PF != PixelFormat::UNKNOWN );
  const double intercept = Intercept;
  const double slope = Slope;
  const PixelFormat output =
    ComputeInverseBestFitFromMinMax (/*PF,*/intercept,slope,ScalarRangeMin,ScalarRangeMax);
  assert( output != PixelFormat::UNKNOWN && output >= PixelFormat::UINT8 && output <= PixelFormat::INT32 );
  return output;
}

void Rescaler::SetTargetPixelType( PixelFormat const & targetpf )
{
  TargetScalarType = targetpf.GetScalarType();
}

void Rescaler::SetUseTargetPixelType(bool b)
{
  UseTargetPixelType = b;
}

} // end namespace gdcm
