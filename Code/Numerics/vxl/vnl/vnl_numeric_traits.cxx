//-*- c++ -*-------------------------------------------------------------------
#ifdef __GNUC__
#pragma implementation
#endif
//
// Class: vnl_numeric_traits
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 12 Feb 98
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_numeric_traits.h>
#include <vcl_complex.h>

#if !VCL_CAN_STATIC_CONST_INIT_INT

#ifndef NO_STD_BOOL
const bool vnl_numeric_traits<bool>::zero = 0;
const bool vnl_numeric_traits<bool>::one = 1;
#endif

const unsigned char vnl_numeric_traits<unsigned char>::zero = 0;
const unsigned char vnl_numeric_traits<unsigned char>::one = 1;

const signed char vnl_numeric_traits<signed char>::zero = 0;
const signed char vnl_numeric_traits<signed char>::one = 1;

const unsigned short vnl_numeric_traits<unsigned short>::zero = 0;
const unsigned short vnl_numeric_traits<unsigned short>::one = 1;

const signed short vnl_numeric_traits<signed short>::zero = 0;
const signed short vnl_numeric_traits<signed short>::one = 1;

const unsigned int vnl_numeric_traits<unsigned int>::zero = 0;
const unsigned int vnl_numeric_traits<unsigned int>::one = 1;

const signed int vnl_numeric_traits<signed int>::zero = 0;
const signed int vnl_numeric_traits<signed int>::one = 1;

const signed long vnl_numeric_traits<signed long>::zero = 0;
const signed long vnl_numeric_traits<signed long>::one = 1;

const unsigned long vnl_numeric_traits<unsigned long>::zero = 0;
const unsigned long vnl_numeric_traits<unsigned long>::one = 1;

#endif

#if !VCL_CAN_STATIC_CONST_INIT_FLOAT

const float vnl_numeric_traits<float>::zero = 0.0F;
const float vnl_numeric_traits<float>::one = 1.0F;

const double vnl_numeric_traits<double>::zero = 0.0;
const double vnl_numeric_traits<double>::one = 1.0;

#endif

//--------------------------------------------------------------------------------

// declaring these const crashes 2.7.2
#ifdef VCL_GCC_272
#define const
#endif
const vcl_complex<double> vnl_numeric_traits<vcl_complex<double> >::zero = 0.0;
const vcl_complex<double> vnl_numeric_traits<vcl_complex<double> >::one = 1.0;

const vcl_complex<float> vnl_numeric_traits<vcl_complex<float> >::zero = 0.0;
const vcl_complex<float> vnl_numeric_traits<vcl_complex<float> >::one = 1.0;
