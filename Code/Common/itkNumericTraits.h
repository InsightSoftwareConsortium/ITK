/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraits_h
#define __itkNumericTraits_h

#include "itkMacro.h"
#undef min
#undef max

#if (defined(_MSC_VER)) || (defined(__BORLANDC__)) || (defined(__sgi) && !defined(__GNUC__))
#include <limits>
#define ITK_NUMERIC_LIMITS std::numeric_limits
#else
#include "vnl/vnl_numeric_limits.h"
#define ITK_NUMERIC_LIMITS vnl_numeric_limits
#endif

namespace itk
{

/** \class NumericTraits
 * \brief Define additional traits for native types such as int or float.
 *
 * NumericTraits is used to extend the traits associated with native types
 * such as float, char, int, and so on. These traits are extensions of the
 * standard <numeric_limits> defined by the C++ compilers. Some of the added
 * traits include minimum and maximum value; accumulation type; etc.
 *
 * \ingroup DataRepresentation
 */
template <class T>
class NumericTraits : public ITK_NUMERIC_LIMITS<T> {
public:
  /** Return the type of this native type. */
  typedef T ValueType; 

  /** Return the type that can be printed. */
  typedef T PrintType; 

  /** Return value of abs(). */
  typedef T AbsType; 

  /** Accumulation of addition and multiplication. */
  typedef double AccumulateType; 

  /** Additive identity. */
  static const T Zero;

  /** Multiplicative identity. */
  static const T One;

  /** Smallest (most nonpositive) value **/
  static T NonpositiveMin() { return min(); }

  /** Is a given value positive? **/
  static bool IsPositive(T val) { return val > Zero; }

  /** Is a given value nonpositive? **/
  static bool IsNonpositive(T val) { return val <= Zero; }

  /** Is a given value negative? **/
  static bool IsNegative(T val) { return val < Zero; }

  /** Is a given value nonnegative? **/
  static bool IsNonnegative(T val) { return val >= Zero; }
};

/** \class NumericTraits<bool>
 * \brief Define traits for type bool.
 * 
 * \ingroup DataRepresentation
 */

template <>
class NumericTraits<bool> : public ITK_NUMERIC_LIMITS<bool> {
public:
  typedef bool ValueType;
  typedef bool PrintType;
  typedef unsigned char AbsType;
  typedef unsigned char AccumulateType;
  static const bool ITKCommon_EXPORT Zero;
  static const bool ITKCommon_EXPORT One;

  static bool NonpositiveMin() { return false; }
  static bool IsPositive(bool val) { return val; }
  static bool IsNonpositive(bool val) { return !val; }
  static bool IsNegative(bool /* val */) { return false; }
  static bool IsNonnegative(bool /*val*/) {return true; }
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guarenteed to be signed. On SGI's, thge default is unsigned
 */
template <>
class NumericTraits<char> : public ITK_NUMERIC_LIMITS<char> {
public:
  typedef char ValueType;
  typedef int PrintType;
  typedef unsigned char AbsType;
  typedef short AccumulateType;
  typedef double RealType;
  static const char ITKCommon_EXPORT Zero;
  static const char ITKCommon_EXPORT One;

  static char min() { return char(255) < 0 ? -128 : 0; }
  static char max() { return char(255) < 0 ? 127 : 255; }
  static char NonpositiveMin() { return min(); }
  static bool IsPositive(char val) { return val > Zero; }
  static bool IsNonpositive(char val) { return val <= Zero; }
  static bool IsNegative(char val) { return val < Zero; }
  static bool IsNonnegative(char val) {return val >= Zero; }
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guarenteed to be signed. On SGI's, thge default is unsigned
 */
template <>
class NumericTraits<signed char> : public ITK_NUMERIC_LIMITS<signed char> {
public:
  typedef signed char ValueType;
  typedef int PrintType;
  typedef unsigned char AbsType;
  typedef short AccumulateType;
  typedef double RealType;
  static const signed char ITKCommon_EXPORT Zero;
  static const signed char ITKCommon_EXPORT One;

  static signed char min() { return -128; }
  static signed char max() { return  127; }
  static signed char NonpositiveMin() { return min(); }
  static bool IsPositive(signed char val) { return val > Zero; }
  static bool IsNonpositive(signed char val) { return val <= Zero; }
  static bool IsNegative(signed char val) { return val < Zero; }
  static bool IsNonnegative(signed char val) {return val >= Zero; }
};

/** \class NumericTraits<unsigned char>
 * \brief Define traits for type unsigned char.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<unsigned char> : public ITK_NUMERIC_LIMITS<unsigned char> {
public:
  typedef unsigned char ValueType;
  typedef int PrintType;
  typedef unsigned char AbsType;
  typedef unsigned short AccumulateType;
  typedef double RealType;
  static const unsigned char ITKCommon_EXPORT Zero;
  static const unsigned char ITKCommon_EXPORT One;

  static unsigned char NonpositiveMin() { return min(); }
  static bool IsPositive(unsigned char val) { return val != Zero; }
  static bool IsNonpositive(unsigned char val) { return val == Zero; }
  static bool IsNegative(unsigned char /* val */) { return false; }
  static bool IsNonnegative(unsigned char /*val */) {return true; }
};

/** \class NumericTraits<short>
 * \brief Define traits for type short.
 */
template <>
class NumericTraits<short> : public ITK_NUMERIC_LIMITS<short> {
public:
  typedef short ValueType;
  typedef short PrintType;
  typedef unsigned short AbsType;
  typedef int AccumulateType;
  typedef double RealType;
  static const short ITKCommon_EXPORT Zero;
  static const short ITKCommon_EXPORT One;

  static short NonpositiveMin() { return min(); }
  static bool IsPositive(short val) { return val > Zero; }
  static bool IsNonpositive(short val) { return val <= Zero; }
  static bool IsNegative(short val) { return val < Zero; }
  static bool IsNonnegative(short val) {return val >= Zero; }
};

/** \class NumericTraits<unsigned short>
 * \brief Define traits for type unsigned short.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<unsigned short> : public ITK_NUMERIC_LIMITS<unsigned short> {
public:
  typedef unsigned short ValueType;
  typedef unsigned short PrintType;
  typedef unsigned short AbsType;
  typedef unsigned int AccumulateType;
  typedef double RealType;
  static const unsigned short ITKCommon_EXPORT Zero;
  static const unsigned short ITKCommon_EXPORT One;

  static unsigned short NonpositiveMin() { return min(); }
  static unsigned short IsPositive(unsigned short val) { return val != Zero; }
  static bool IsNonpositive(unsigned short val) { return val == Zero; }
  static bool IsNegative(unsigned short/* val*/) { return false; }
  static bool IsNonnegative(unsigned short /*val*/) {return true; }
};

/** \class NumericTraits<int>
 * \brief Define traits for type int.
 */
template <>
class NumericTraits<int> : public ITK_NUMERIC_LIMITS<int> {
public:
  typedef int ValueType;
  typedef int PrintType;
  typedef unsigned int AbsType;
  typedef long AccumulateType;
  typedef double RealType;
  static const int ITKCommon_EXPORT Zero;
  static const int ITKCommon_EXPORT One;

  static int NonpositiveMin() { return min(); }
  static bool IsPositive(int val) { return val > Zero; }
  static bool IsNonpositive(int val) { return val <= Zero; }
  static bool IsNegative(int val) { return val < Zero; }
  static bool IsNonnegative(int val) {return val >= Zero; }
};

/** \class NumericTraits<unsigned int>
 * \brief Define traits for type unsigned int.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<unsigned int> : public ITK_NUMERIC_LIMITS<unsigned int> {
public:
  typedef unsigned int ValueType;
  typedef unsigned int PrintType;
  typedef unsigned int AbsType;
  typedef unsigned int AccumulateType;
  typedef double RealType;
  static const unsigned int ITKCommon_EXPORT Zero;
  static const unsigned int ITKCommon_EXPORT One;

  static unsigned int min(void) { return 0; }
  static unsigned int max(void) { return static_cast<unsigned int>( -1 ); }
  static unsigned int NonpositiveMin() { return 0; }
  static bool IsPositive(unsigned int val) { return val != Zero; }
  static bool IsNonpositive(unsigned int val) { return  val == Zero; }
  static bool IsNegative(unsigned int /*val*/) { return false; }
  static bool IsNonnegative(unsigned int /*val*/) {return true; }
};

/** \class NumericTraits<long>
 * \brief Define traits for type long.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<long> : public ITK_NUMERIC_LIMITS<long> {
public:
  typedef long ValueType;
  typedef long PrintType;
  typedef unsigned long AbsType;
  typedef long AccumulateType;
  typedef double RealType;
  static const long ITKCommon_EXPORT Zero;
  static const long ITKCommon_EXPORT One;

  static long NonpositiveMin() { return min(); }
  static bool IsPositive(long val) { return val > Zero; }
  static bool IsNonpositive(long val) { return val <= Zero; }
  static bool IsNegative(long val) { return val < Zero; }
  static bool IsNonnegative(long val) {return val >= Zero; }
};

/** \class NumericTraits<unsigned long>
 * \brief Define traits for type unsigned long.
 * \ingroup DataRepresentation 
 */
template <>
class NumericTraits<unsigned long> : public ITK_NUMERIC_LIMITS<unsigned long> {
public:
  typedef unsigned long ValueType;
  typedef unsigned long PrintType;
  typedef unsigned long AbsType;
  typedef unsigned long AccumulateType;
  typedef double RealType;
  static const unsigned long ITKCommon_EXPORT Zero;
  static const unsigned long ITKCommon_EXPORT One;

  static unsigned long NonpositiveMin() { return min(); }
  static bool IsPositive(unsigned long val) { return val != Zero; }
  static bool IsNonpositive(unsigned long val) { return val == Zero; }
  static bool IsNegative(unsigned long) { return false; }
  static bool IsNonnegative(unsigned long) {return true; }
};

/** \class NumericTraits<float>
 * \brief Define traits for type float.
 * \ingroup DataRepresentation
 */
template <>
class NumericTraits<float> : public ITK_NUMERIC_LIMITS<float> {
public:
  typedef float ValueType;
  typedef float PrintType;
  typedef float AbsType;
  typedef double AccumulateType;
  typedef double RealType;
  static const float ITKCommon_EXPORT Zero;
  static const float ITKCommon_EXPORT One;

  static float NonpositiveMin() { return -max(); }
  static bool IsPositive(float val) { return val > Zero; }
  static bool IsNonpositive(float val) { return val <= Zero; }
  static bool IsNegative(float val) { return val < Zero; }
  static bool IsNonnegative(float val) {return val >= Zero; }
};

/** \class NumericTraits<double>
 * \brief Define traits for type double.
 * \ingroup DataRepresentation 
 */
template <>
class NumericTraits<double> : public ITK_NUMERIC_LIMITS<double> {
public:
  typedef double ValueType;
  typedef double PrintType;
  typedef double AbsType;
  typedef double AccumulateType;
  typedef double RealType;
  static const double ITKCommon_EXPORT Zero;
  static const double ITKCommon_EXPORT One;

  static double NonpositiveMin() { return -max(); }
  static bool IsPositive(double val) { return val > Zero; }
  static bool IsNonpositive(double val) { return val <= Zero; }
  static bool IsNegative(double val) { return val < Zero; }
  static bool IsNonnegative(double val) {return val >= Zero; }
};

/** \class NumericTraits<long double>
 * \brief Define traits for type long double.
 * \ingroup DataRepresentation 
 */
template <>
class NumericTraits<long double> : public ITK_NUMERIC_LIMITS<long double> {
public:
  typedef long double ValueType;
  typedef long double PrintType;
  typedef long double AbsType;
  typedef long double AccumulateType;
  typedef long double RealType;
  static const long double ITKCommon_EXPORT Zero;
  static const long double ITKCommon_EXPORT One;

  static long double NonpositiveMin() { return -max(); }
  static bool IsPositive(long double val) { return val > Zero; }
  static bool IsNonpositive(long double val) { return val <= Zero; }
  static bool IsNegative(long double val) { return val < Zero; }
  static bool IsNonnegative(long double val) {return val >= Zero; }
};

} // end namespace itk

#endif // __itkNumericTraits_h
