/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkNumericTraits is used to extend the traits associated with native types
 * such as float, char, int, and so on. These traits are extensions of the
 * standard <numeric_limits> defined by the C++ compilers. Some of the added
 * traits include minimum and maximum value; accumulation type; etc.  
 */

#ifndef __itkNumericTraits_h
#define __itkNumericTraits_h

#if defined(_WIN32) || defined(WIN32)
#include <limits>
#define ITK_NUMERIC_LIMITS std::numeric_limits
#else
#include "vnl/vnl_numeric_limits.h"
#define ITK_NUMERIC_LIMITS vnl_numeric_limits
#endif

template <class T>
class itkNumericTraits : public ITK_NUMERIC_LIMITS<T> {
public:
  /* Return the type of this native type */
  typedef T ValueType; 

  /* Return value of abs() */
  typedef T AbsType; 

  /* Accumulation of addition and multiplication */
  typedef double AccumulateType; 

  /* Additive identity */
  static const T Zero;

  /* Multiplicative identity */
  static const T One;
};

template <>
class itkNumericTraits<bool> : public ITK_NUMERIC_LIMITS<bool> {
public:
  typedef bool ValueType;
  typedef unsigned char AbsType;
  typedef unsigned char AccumulateType;
  static const bool Zero;
  static const bool One;
};

template <>
class itkNumericTraits<char> : public ITK_NUMERIC_LIMITS<char> {
public:
  typedef char ValueType;
  typedef unsigned char AbsType;
  typedef short AccumulateType;
  static const char Zero;
  static const char One;
};

template <>
class itkNumericTraits<unsigned char> : public ITK_NUMERIC_LIMITS<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char AbsType;
  typedef unsigned short AccumulateType;
  static const unsigned char Zero;
  static const unsigned char One;
};

template <>
class itkNumericTraits<short> : public ITK_NUMERIC_LIMITS<short> {
public:
  typedef short ValueType;
  typedef unsigned short AbsType;
  typedef int AccumulateType;
  static const short Zero;
  static const short One;
};

template <>
class itkNumericTraits<unsigned short> : public ITK_NUMERIC_LIMITS<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned short Zero;
  static const unsigned short One;
};

template <>
class itkNumericTraits<int> : public ITK_NUMERIC_LIMITS<int> {
public:
  typedef int ValueType;
  typedef unsigned int AbsType;
  typedef long AccumulateType;
  static const int Zero;
  static const int One;
};

template <>
class itkNumericTraits<unsigned int> : public ITK_NUMERIC_LIMITS<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned int Zero;
  static const unsigned int One;
};

template <>
class itkNumericTraits<long> : public ITK_NUMERIC_LIMITS<long> {
public:
  typedef long ValueType;
  typedef unsigned long AbsType;
  typedef long AccumulateType;
  static const long Zero;
  static const long One;
};

template <>
class itkNumericTraits<unsigned long> : public ITK_NUMERIC_LIMITS<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long AbsType;
  typedef unsigned long AccumulateType;
  static const unsigned long Zero;
  static const unsigned long One;
};

template <>
class itkNumericTraits<float> : public ITK_NUMERIC_LIMITS<float> {
public:
  typedef float ValueType;
  typedef float AbsType;
  typedef double AccumulateType;
  static const float Zero;
  static const float One;
};

template <>
class itkNumericTraits<double> : public ITK_NUMERIC_LIMITS<double> {
public:
  typedef double ValueType;
  typedef double AbsType;
  typedef double AccumulateType;
  static const double Zero;
  static const double One;
};

template <>
class itkNumericTraits<long double> : public ITK_NUMERIC_LIMITS<long double> {
public:
  typedef long double ValueType;
  typedef long double AbsType;
  typedef long double AccumulateType;
  static const long double Zero;
  static const long double One;
};

#endif // __itkNumericTraits_h
