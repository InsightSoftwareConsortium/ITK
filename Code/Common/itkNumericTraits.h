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
#ifndef __itkNumericTraits_h
#define __itkNumericTraits_h

#if defined(_MSC_VER)
#include <limits>
#define ITK_NUMERIC_LIMITS std::numeric_limits
#else
#include "vnl/vnl_numeric_limits.h"
#define ITK_NUMERIC_LIMITS vnl_numeric_limits
#endif

ITK_NAMESPACE_BEGIN

/** \class NumericTraits
 * \brief Define additional traits for native types such as int or float.
 *
 * NumericTraits is used to extend the traits associated with native types
 * such as float, char, int, and so on. These traits are extensions of the
 * standard <numeric_limits> defined by the C++ compilers. Some of the added
 * traits include minimum and maximum value; accumulation type; etc.  
 */
template <class T>
class NumericTraits : public ITK_NUMERIC_LIMITS<T> {
public:
  /** Return the type of this native type. */
  typedef T ValueType; 

  /** Return value of abs(). */
  typedef T AbsType; 

  /** Accumulation of addition and multiplication. */
  typedef double AccumulateType; 

  /** Additive identity. */
  static const T Zero;

  /** Multiplicative identity. */
  static const T One;
};

/** \class NumericTraits<bool>
 * \brief Define traits for type bool.
 */
template <>
class NumericTraits<bool> : public ITK_NUMERIC_LIMITS<bool> {
public:
  typedef bool ValueType;
  typedef unsigned char AbsType;
  typedef unsigned char AccumulateType;
  static const bool Zero;
  static const bool One;
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 */
template <>
class NumericTraits<char> : public ITK_NUMERIC_LIMITS<char> {
public:
  typedef char ValueType;
  typedef unsigned char AbsType;
  typedef short AccumulateType;
  static const char Zero;
  static const char One;
};

/** \class NumericTraits<unsigned char>
 * \brief Define traits for type unsigned char.
 */
template <>
class NumericTraits<unsigned char> : public ITK_NUMERIC_LIMITS<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef unsigned char AbsType;
  typedef unsigned short AccumulateType;
  static const unsigned char Zero;
  static const unsigned char One;
};

/** \class NumericTraits<short>
 * \brief Define traits for type short.
 */
template <>
class NumericTraits<short> : public ITK_NUMERIC_LIMITS<short> {
public:
  typedef short ValueType;
  typedef unsigned short AbsType;
  typedef int AccumulateType;
  static const short Zero;
  static const short One;
};

/** \class NumericTraits<unsigned short>
 * \brief Define traits for type unsigned short.
 */
template <>
class NumericTraits<unsigned short> : public ITK_NUMERIC_LIMITS<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned short Zero;
  static const unsigned short One;
};

/** \class NumericTraits<int>
 * \brief Define traits for type int.
 */
template <>
class NumericTraits<int> : public ITK_NUMERIC_LIMITS<int> {
public:
  typedef int ValueType;
  typedef unsigned int AbsType;
  typedef long AccumulateType;
  static const int Zero;
  static const int One;
};

/** \class NumericTraits<unsigned int>
 * \brief Define traits for type unsigned int.
 */
template <>
class NumericTraits<unsigned int> : public ITK_NUMERIC_LIMITS<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int AbsType;
  typedef unsigned int AccumulateType;
  static const unsigned int Zero;
  static const unsigned int One;
};

/** \class NumericTraits<long>
 * \brief Define traits for type long.
 */
template <>
class NumericTraits<long> : public ITK_NUMERIC_LIMITS<long> {
public:
  typedef long ValueType;
  typedef unsigned long AbsType;
  typedef long AccumulateType;
  static const long Zero;
  static const long One;
};

/** \class NumericTraits<unsigned long>
 * \brief Define traits for type unsigned long.
 */
template <>
class NumericTraits<unsigned long> : public ITK_NUMERIC_LIMITS<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long AbsType;
  typedef unsigned long AccumulateType;
  static const unsigned long Zero;
  static const unsigned long One;
};

/** \class NumericTraits<float>
 * \brief Define traits for type float.
 */
template <>
class NumericTraits<float> : public ITK_NUMERIC_LIMITS<float> {
public:
  typedef float ValueType;
  typedef float AbsType;
  typedef double AccumulateType;
  static const float Zero;
  static const float One;
};

/** \class NumericTraits<double>
 * \brief Define traits for type double.
 */
template <>
class NumericTraits<double> : public ITK_NUMERIC_LIMITS<double> {
public:
  typedef double ValueType;
  typedef double AbsType;
  typedef double AccumulateType;
  static const double Zero;
  static const double One;
};

/** \class NumericTraits<long double>
 * \brief Define traits for type long double.
 */
template <>
class NumericTraits<long double> : public ITK_NUMERIC_LIMITS<long double> {
public:
  typedef long double ValueType;
  typedef long double AbsType;
  typedef long double AccumulateType;
  static const long double Zero;
  static const long double One;
};

ITK_NAMESPACE_END

#endif // __itkNumericTraits_h
