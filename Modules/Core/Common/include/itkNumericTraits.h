/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNumericTraits_h
#define itkNumericTraits_h

#include "itkMacro.h"

#undef min
#undef max

#define itkNUMERIC_TRAITS_MIN_MAX_MACRO()          \
  static ITK_CONSTEXPR_FUNC ValueType min()                           \
    {                                              \
    return std::numeric_limits< ValueType >::min(); \
    }                                              \
  static ITK_CONSTEXPR_FUNC ValueType max()                           \
    {                                              \
    return std::numeric_limits< ValueType >::max(); \
    }                                              \
  static ITK_CONSTEXPR_FUNC ValueType min(ValueType)                  \
    {                                              \
    return std::numeric_limits< ValueType >::min(); \
    }                                              \
  static ITK_CONSTEXPR_FUNC ValueType max(ValueType)                  \
    {                                              \
    return std::numeric_limits< ValueType >::max(); \
    }                                              \

#if ITK_COMPILER_CXX_CONSTEXPR
#define itkNUMERIC_TRAITS_C11_ASSINMENT(x) = x
#else
#define itkNUMERIC_TRAITS_C11_ASSINMENT(x)
#endif

#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_limits.h"
#endif
#include <limits> // for std::numeric_limits
#include <complex>

namespace itk
{

// forward decare to avoid circular dependencies
template< typename TValue, unsigned int VLength>  class FixedArray;

/** \class NumericTraits
 * \brief Define additional traits for native types such as int or float.
 *
 * NumericTraits is used to extend the traits associated with native types
 * such as float, char, int, and so on. These traits are extensions of the
 * standard numeric_limits defined by the C++ compilers. Some of the added
 * traits include minimum and maximum value; accumulation type; etc.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/NumericTraits,Get some basic information about a type}
 * \endwiki
 */
template< typename T >
class NumericTraits:public std::numeric_limits< T >
{
public:
  /** The type of this limits trait object. */
  typedef std::numeric_limits< T > TraitsType;

  /** Return the type of this native type. */
  typedef T ValueType;

  /** Return the type that can be printed. */
  typedef T PrintType;

  /** Return value of std::abs(). */
  typedef T AbsType;

  /** Accumulation of addition and multiplication. */
  typedef double AccumulateType;

  /** Measurement vector type */
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  /** Typedef for operations that use floating point instead of real precision
   *  to save memory */
  typedef float FloatType;

  /** Type for real-valued scalar operations. */
  typedef double RealType;

  /** Type for real-valued scalar operations. */
  typedef RealType ScalarRealType;

  /** Additive identity. */
  static const T Zero;

  /** Multiplicative identity. */
  static const T One;

  /** Smallest (most nonpositive) value */
  static ITK_CONSTEXPR_FUNC T NonpositiveMin() { return TraitsType::min(); }

  /** Is a given value positive? */
  static bool IsPositive(T val) { return val > Zero; }

  /** Is a given value nonpositive? */
  static bool IsNonpositive(T val) { return val <= Zero; }

  /** Is a given value negative? */
  static bool IsNegative(T val) { return val < Zero; }

  /** Is a given value nonnegative? */
  static bool IsNonnegative(T val) { return val >= Zero; }

  /** Is a given type signed? -- default is no.
      For uniform array data types in ITK, the value of IsSigned
      is determined by the component elements of the array.*/
  static ITK_CONSTEXPR_VAR bool IsSigned = false;

  /** Is a given type an integer? -- default is no.
      For uniform array data types in ITK, the value of IsInteger
      is determined by the component elements of the array.*/
  static ITK_CONSTEXPR_VAR bool IsInteger = false;

  /** Is a given type complex? -- default is no.
      For uniform array data types in ITK, the value of IsComplex
      is determined by the component elements of the array.*/
  static ITK_CONSTEXPR_VAR bool IsComplex = false;

  /** Return zero value. This function should be used to support
   *  RGBPixel type and standard types (not vectors) */
  static T ZeroValue() { return Zero; }

  /** Return one value. This function should be used to support
   *  RGBPixel type and standard types (not vectors) */
  static T OneValue() { return One; }

  /* Provide a default implementation of the max() method with
   * argument. This API is needed for VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built. */
  static ITK_CONSTEXPR_FUNC T max(const T &) { return TraitsType::max(); }
  static ITK_CONSTEXPR_FUNC T min(const T &) { return TraitsType::min(); }

  /** Scalars cannot be resized, so an exception will
   * be thrown if the input size is not 1.  If the size is valid
   * the will be zeros. This API is needed for VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built.
   */
  static void SetLength(T & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
  /** Return the length of the scalar. This API is needed for
   * VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built.
   */
  static unsigned int GetLength(const T &)
  {
    return GetLength();
  }

  /** Return the length of the scalar: 1. Array types can return a different value */
  static unsigned int GetLength()
  {
    return 1;
  }

  /** Smallest (most nonpositive) value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T NonpositiveMin(const T &)
  {
    return NonpositiveMin();
  }

  /** Zero value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T ZeroValue(const T &)
  {
    return ZeroValue();
  }

  /** One value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T OneValue(const T &)
  {
    return OneValue();
  }

  /** assign the value to an array */
  template<typename TArray>
  static void AssignToArray( const T & v, TArray & mv )
  {
    mv[0] = v;
  }

};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

/** \class NumericTraits<bool>
 * \brief Define traits for type bool.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */

template< >
class NumericTraits< bool > :public std::numeric_limits< bool >
{
public:
  typedef bool                     ValueType;
  typedef bool                     PrintType;
  typedef unsigned char            AbsType;
  typedef unsigned char            AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR bool Zero = false;
  static ITK_CONSTEXPR_VAR bool One = true;

  static ITK_CONSTEXPR_FUNC bool min() { return false; }
  static ITK_CONSTEXPR_FUNC bool max() { return true; }
  static ITK_CONSTEXPR_FUNC bool min(bool) { return min(); }
  static ITK_CONSTEXPR_FUNC bool max(bool) { return max(); }
  static ITK_CONSTEXPR_FUNC bool NonpositiveMin() { return false; }
  static ITK_CONSTEXPR_FUNC bool IsPositive(bool val) { return val; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(bool val) { return !val; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(bool val) { return val ? false : false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(bool val) { return val ? true : true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC bool ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC bool OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }

};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guaranteed to be signed. On SGI computers, the default is unsigned
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< char > :public std::numeric_limits< char >
{
public:
  typedef char                     ValueType;
  typedef int                      PrintType;
  typedef unsigned char            AbsType;
  typedef short                    AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR char ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR char ITKCommon_EXPORT One = 1;

  static ITK_CONSTEXPR_FUNC char min() { return char(255) < char(0) ? char(-128) : char(0); }
  static ITK_CONSTEXPR_FUNC char max() { return char(255) < char(0) ? char(127) : char(255); }

  static ITK_CONSTEXPR_FUNC char min(char) { return min(); }
  static ITK_CONSTEXPR_FUNC char max(char) { return max(); }
  static ITK_CONSTEXPR_FUNC char NonpositiveMin() { return min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(char val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(char val) { return val <= Zero; }
// char on PowerPC, for example, is not signed
#if VCL_CHAR_IS_SIGNED
  static ITK_CONSTEXPR_FUNC bool IsNegative(char val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(char val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
#else
  static ITK_CONSTEXPR_FUNC bool IsNegative(char) { return false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(char) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
#endif
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC char ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC char OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guaranteed to be signed. On SGI computers, the default is unsigned
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< signed char > :public std::numeric_limits< signed char >
{
public:
  typedef signed char              ValueType;
  typedef int                      PrintType;
  typedef unsigned char            AbsType;
  typedef short                    AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR signed char ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR signed char ITKCommon_EXPORT One = 1;

  static ITK_CONSTEXPR_FUNC signed char min() { return -128; }
  static ITK_CONSTEXPR_FUNC signed char max() { return 127; }
  static ITK_CONSTEXPR_FUNC signed char min(signed char) { return min(); }
  static ITK_CONSTEXPR_FUNC signed char max(signed char) { return max(); }
  static ITK_CONSTEXPR_FUNC signed char NonpositiveMin() { return min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(signed char val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(signed char val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(signed char val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(signed char val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC signed char  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC signed char OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<unsigned char>
 * \brief Define traits for type unsigned char.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< unsigned char > :public std::numeric_limits< unsigned char >
{
public:
  typedef unsigned char            ValueType;
  typedef int                      PrintType;
  typedef unsigned char            AbsType;
  typedef unsigned short           AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR unsigned char ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR unsigned char ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();

  static ITK_CONSTEXPR_FUNC unsigned char NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(unsigned char val) { return val != Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(unsigned char val) { return val == Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(unsigned char val) { return val ? false : false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(unsigned char val) { return val ? true : true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC unsigned char  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC unsigned char OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<short>
 * \brief Define traits for type short.
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< short > :public std::numeric_limits< short >
{
public:
  typedef short                    ValueType;
  typedef short                    PrintType;
  typedef unsigned short           AbsType;
  typedef int                      AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR short ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR short ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC short NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(short val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(short val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(short val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(short val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC short  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC short OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<unsigned short>
 * \brief Define traits for type unsigned short.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< unsigned short > :public std::numeric_limits< unsigned short >
{
public:
  typedef unsigned short           ValueType;
  typedef unsigned short           PrintType;
  typedef unsigned short           AbsType;
  typedef unsigned int             AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR unsigned short ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR unsigned short ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC unsigned short NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(unsigned short val) { return val != Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(unsigned short val) { return val == Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(unsigned short val) { return val ? false : false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(unsigned short val) { return val ? true : true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC unsigned short ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC unsigned short OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<int>
 * \brief Define traits for type int.
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< int > :public std::numeric_limits< int >
{
public:
  typedef int                      ValueType;
  typedef int                      PrintType;
  typedef unsigned int             AbsType;
  typedef long                     AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR int ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR int ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC int NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(int val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(int val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(int val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(int val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC int  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC int OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<unsigned int>
 * \brief Define traits for type unsigned int.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< unsigned int > :public std::numeric_limits< unsigned int >
{
public:
  typedef unsigned int             ValueType;
  typedef unsigned int             PrintType;
  typedef unsigned int             AbsType;
  typedef unsigned int             AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR unsigned int ITKCommon_EXPORT Zero = 0;
  static ITK_CONSTEXPR_VAR unsigned int ITKCommon_EXPORT One = 1;

  static ITK_CONSTEXPR_FUNC unsigned int min(void) { return 0; }
  static ITK_CONSTEXPR_FUNC unsigned int max(void) { return static_cast< unsigned int >( -1 ); }
  static ITK_CONSTEXPR_FUNC unsigned int min(unsigned int) { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC unsigned int max(unsigned int) { return std::numeric_limits< ValueType >::max(); }
  static ITK_CONSTEXPR_FUNC unsigned int NonpositiveMin() { return 0; }
  static ITK_CONSTEXPR_FUNC bool IsPositive(unsigned int val) { return val != Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(unsigned int val) { return val == Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(unsigned int val) { return val ? false : false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(unsigned int val) { return val ? true : true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC unsigned int  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC unsigned int OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<long>
 * \brief Define traits for type long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< long > :public std::numeric_limits< long >
{
public:
  typedef long                     ValueType;
  typedef long                     PrintType;
  typedef unsigned long            AbsType;
  typedef long                     AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR long ITKCommon_EXPORT Zero = 0L;
  static ITK_CONSTEXPR_VAR long ITKCommon_EXPORT One = 1L;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC long NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(long val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(long val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(long val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(long val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC long  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC long OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<unsigned long>
 * \brief Define traits for type unsigned long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< unsigned long > :public std::numeric_limits< unsigned long >
{
public:
  typedef unsigned long            ValueType;
  typedef unsigned long            PrintType;
  typedef unsigned long            AbsType;
  typedef unsigned long            AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR unsigned long ITKCommon_EXPORT Zero = 0UL;
  static ITK_CONSTEXPR_VAR unsigned long ITKCommon_EXPORT One = 1UL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC unsigned long NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(unsigned long val) { return val != Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(unsigned long val) { return val == Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(unsigned long) { return false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(unsigned long) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC unsigned long  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC unsigned long  OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<float>
 * \brief Define traits for type float.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< float > :public std::numeric_limits< float >
{
public:
  typedef float                    ValueType;
  typedef float                    PrintType;
  typedef float                    AbsType;
  typedef double                   AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;


  static ITK_CONSTEXPR_VAR float ITKCommon_EXPORT Zero itkNUMERIC_TRAITS_C11_ASSINMENT(0.0f);
  static ITK_CONSTEXPR_VAR float ITKCommon_EXPORT One itkNUMERIC_TRAITS_C11_ASSINMENT(1.0f);

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC float NonpositiveMin() { return -std::numeric_limits< ValueType >::max(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(float val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(float val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(float val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(float val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC float  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC float  OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<double>
 * \brief Define traits for type double.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< double > :public std::numeric_limits< double >
{
public:
  typedef double                   ValueType;
  typedef double                   PrintType;
  typedef double                   AbsType;
  typedef double                   AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR double ITKCommon_EXPORT Zero itkNUMERIC_TRAITS_C11_ASSINMENT(0.0);
  static ITK_CONSTEXPR_VAR double ITKCommon_EXPORT One  itkNUMERIC_TRAITS_C11_ASSINMENT(1.0);

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC double NonpositiveMin() { return -std::numeric_limits< ValueType >::max(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(double val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(double val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(double val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(double val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC double  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC double  OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<long double>
 * \brief Define traits for type long double.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< long double > :public std::numeric_limits< long double >
{
public:
  typedef long double ValueType;
#if defined( __SUNPRO_CC ) && defined( _ILP32 )
  // sun studio in 32 bit mode is unable to print long double values: it
  // segfaults.
  // conversion to double will give usable results if the value is in the double
  // range - better than nothing.
  typedef double                   PrintType;
#else
  typedef long double              PrintType;
#endif
  typedef long double              AbsType;
  typedef long double              AccumulateType;
  typedef long double              RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR long double ITKCommon_EXPORT Zero itkNUMERIC_TRAITS_C11_ASSINMENT(0.0);
  static ITK_CONSTEXPR_VAR long double ITKCommon_EXPORT One itkNUMERIC_TRAITS_C11_ASSINMENT(1.0);

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC long double NonpositiveMin() { return -std::numeric_limits< ValueType >::max(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(long double val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(long double val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(long double val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(long double val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC long double ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC long double OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};


/** \class NumericTraits<long long>
 * \brief Define traits for type long long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< long long > :
  public std::numeric_limits< long long >
{
public:
  typedef long long                ValueType;
  typedef long long                PrintType;
  typedef long long                AbsType;
  typedef long long                AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR ValueType ITKCommon_EXPORT Zero = 0LL;
  static ITK_CONSTEXPR_VAR ValueType ITKCommon_EXPORT One = 1LL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(ValueType val) { return val > Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(ValueType val) { return val <= Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(ValueType val) { return val < Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(ValueType val) { return val >= Zero; }
  static ITK_CONSTEXPR_VAR bool IsSigned = true;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC ValueType  ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC ValueType  OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits<unsigned long long>
 * \brief Define traits for type unsigned long long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< unsigned long long > :
  public std::numeric_limits< unsigned long long >
{
public:
  typedef unsigned long long       ValueType;
  typedef unsigned long long       PrintType;
  typedef unsigned long long       AbsType;
  typedef unsigned long long       AccumulateType;
  typedef double                   RealType;
  typedef RealType                 ScalarRealType;
  typedef float                    FloatType;
  typedef FixedArray<ValueType, 1> MeasurementVectorType;

  static ITK_CONSTEXPR_VAR ValueType ITKCommon_EXPORT Zero = 0ULL;
  static ITK_CONSTEXPR_VAR ValueType ITKCommon_EXPORT One = 1ULL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin() { return std::numeric_limits< ValueType >::min(); }
  static ITK_CONSTEXPR_FUNC bool IsPositive(ValueType val) { return val != Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNonpositive(ValueType val) { return val == Zero; }
  static ITK_CONSTEXPR_FUNC bool IsNegative(ValueType) { return false; }
  static ITK_CONSTEXPR_FUNC bool IsNonnegative(ValueType) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = false;
  static ITK_CONSTEXPR_VAR bool IsInteger = true;
  static ITK_CONSTEXPR_VAR bool IsComplex = false;
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue() { return Zero; }
  static ITK_CONSTEXPR_FUNC ValueType OneValue() { return One; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength(const ValueType &) { return 1; }
  static ITK_CONSTEXPR_FUNC unsigned int GetLength() { return 1; }
  static ITK_CONSTEXPR_FUNC ValueType NonpositiveMin(const ValueType &) { return NonpositiveMin(); }
  static ITK_CONSTEXPR_FUNC ValueType ZeroValue(const ValueType &) { return ZeroValue(); }
  static ITK_CONSTEXPR_FUNC ValueType OneValue(const ValueType &) { return OneValue(); }

  template<typename TArray>
  static void AssignToArray( const ValueType & v, TArray & mv )
  {
    mv[0] = v;
  }
  static void SetLength(ValueType & m, const unsigned int s)
  {
    if ( s != 1 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};


/** \class NumericTraits< std::complex<char> >
 * \brief Define traits for type std::complex<char>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< char > >
{
public:
  typedef std::complex< char >   Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef char                   ValueType;
  typedef std::complex< int >    PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<char, 2>    MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0; }
// char on PowerPC, for example, is not signed
#if VCL_CHAR_IS_SIGNED
  static bool IsNegative(Self val) { return val.real() < 0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0; }
#else
  static bool IsNegative(Self) { return false; }
  static bool IsNonnegative(Self) { return true; }
#endif
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<unsigned char> >
 * \brief Define traits for type std::complex<unsigned char>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< unsigned char > >
{
public:
  typedef std::complex< unsigned char >   Self;
  // for backward compatibility
  typedef Self                         TheType;
  typedef unsigned char                ValueType;
  typedef std::complex< unsigned int > PrintType;
  typedef double                       AbsType;
  typedef Self                         AccumulateType;
  typedef std::complex< double >       RealType;
  typedef double                       ScalarRealType;
  typedef std::complex< float >        FloatType;
  typedef FixedArray<unsigned char, 2> MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() == 0; }
  static bool IsNegative(Self) { return false; }
  static bool IsNonnegative(Self) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<short> >
 * \brief Define traits for type std::complex<short>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< short > >
{
public:
  typedef std::complex< short >  Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef short                  ValueType;
  typedef Self                   PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<short, 2>   MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0; }
  static bool IsNegative(Self val) { return val.real() < 0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<unsigned short> >
 * \brief Define traits for type std::complex<unsigned short>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< unsigned short > >
{
public:
  typedef std::complex< unsigned short >  Self;
  // for backward compatibility
  typedef Self                            TheType;
  typedef unsigned short                  ValueType;
  typedef Self                            PrintType;
  typedef double                          AbsType;
  typedef Self                            AccumulateType;
  typedef std::complex< double >          RealType;
  typedef double                          ScalarRealType;
  typedef std::complex< float >           FloatType;
  typedef FixedArray<unsigned short, 2>   MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() == 0; }
  static bool IsNegative(Self) { return false; }
  static bool IsNonnegative(Self) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<int> >
 * \brief Define traits for type std::complex<int>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< int > >
{
public:
  typedef std::complex< int >    Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef int                    ValueType;
  typedef Self                   PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<int, 2>     MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0; }
  static bool IsNegative(Self val) { return val.real() < 0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<unsigned int> >
 * \brief Define traits for type std::complex<unsigned int>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< unsigned int > >
{
public:
  typedef std::complex< unsigned int >  Self;
  // for backward compatibility
  typedef Self                          TheType;
  typedef unsigned int                  ValueType;
  typedef Self                          PrintType;
  typedef double                        AbsType;
  typedef Self                          AccumulateType;
  typedef std::complex< double >        RealType;
  typedef double                        ScalarRealType;
  typedef std::complex< float >         FloatType;
  typedef FixedArray<unsigned int, 2>   MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() == 0; }
  static bool IsNegative(Self) { return false; }
  static bool IsNonnegative(Self) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<long> >
 * \brief Define traits for type std::complex<long>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< long > >
{
public:
  typedef std::complex< long >    Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef long                   ValueType;
  typedef Self                   PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<long, 2>    MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0; }
  static bool IsNegative(Self val) { return val.real() < 0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<unsigned long> >
 * \brief Define traits for type std::complex<unsigned long>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< unsigned long > >
{
public:
  typedef std::complex< unsigned long >  Self;
  // for backward compatibility
  typedef Self                           TheType;
  typedef unsigned long                  ValueType;
  typedef Self                           PrintType;
  typedef double                         AbsType;
  typedef Self                           AccumulateType;
  typedef std::complex< double >         RealType;
  typedef double                         ScalarRealType;
  typedef std::complex< float >          FloatType;
  typedef FixedArray<unsigned long, 2>   MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0);
  }

  static bool IsPositive(Self val) { return val.real() > 0; }
  static bool IsNonpositive(Self val) { return val.real() == 0; }
  static bool IsNegative(Self) { return false; }
  static bool IsNonnegative(Self) { return true; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<float> >
 * \brief Define traits for type std::complex<float>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< float > >
{
public:
  typedef std::complex< float >  Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef float                  ValueType;
  typedef Self                   PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<float, 2>   MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< Self >::min(); }
  static Self max() { return std::numeric_limits< Self >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< float >::NonpositiveMin(), 0.0f);
  }

  static bool IsPositive(Self val) { return val.real() > 0.0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0.0; }
  static bool IsNegative(Self val) { return val.real() < 0.0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0.0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<double> >
 * \brief Define traits for type std::complex<double>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< double > >
{
public:
  typedef std::complex< double > Self;
  // for backward compatibility
  typedef Self                   TheType;
  typedef double                 ValueType;
  typedef Self                   PrintType;
  typedef double                 AbsType;
  typedef Self                   AccumulateType;
  typedef std::complex< double > RealType;
  typedef double                 ScalarRealType;
  typedef std::complex< float >  FloatType;
  typedef FixedArray<double, 2>  MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< ValueType >::min(); }
  static Self max() { return std::numeric_limits< ValueType >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< double >::NonpositiveMin(), 0.0);
  }

  static bool IsPositive(Self val) { return val.real() > 0.0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0.0; }
  static bool IsNegative(Self val) { return val.real() < 0.0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0.0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/** \class NumericTraits< std::complex<long double> >
 * \brief Define traits for type std::complex<long double>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< >
class NumericTraits< std::complex< long double > >
{
public:
  typedef std::complex< long double > Self;
  // for backward compatibility
  typedef Self                        TheType;
  typedef long double                 ValueType;
  typedef Self                        PrintType;
  typedef long double                 AbsType;
  typedef Self                        AccumulateType;
  typedef std::complex< long double > RealType;
  typedef long double                 ScalarRealType;
  typedef std::complex< float >       FloatType;
  typedef FixedArray<long double, 2>  MeasurementVectorType;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static Self min() { return std::numeric_limits< ValueType >::min(); }
  static Self max() { return std::numeric_limits< ValueType >::max(); }
  static Self min(Self) { return min(); }
  static Self max(Self) { return max(); }
  static Self NonpositiveMin()
  {
    return Self(NumericTraits< ValueType >::NonpositiveMin(), 0.0);
  }

  static bool IsPositive(Self val) { return val.real() > 0.0; }
  static bool IsNonpositive(Self val) { return val.real() <= 0.0; }
  static bool IsNegative(Self val) { return val.real() < 0.0; }
  static bool IsNonnegative(Self val) { return val.real() >= 0.0; }
  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = false;
  static ITK_CONSTEXPR_VAR bool IsComplex = true;
  static Self ZeroValue() { return Zero; }
  static Self OneValue() { return One; }
  static unsigned int GetLength(const Self &) { return 2; }
  static unsigned int GetLength() { return 2; }
  static Self NonpositiveMin(const Self &) { return NonpositiveMin(); }
  static Self ZeroValue(const Self &) { return ZeroValue(); }
  static Self OneValue(const Self &) { return OneValue(); }
  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void SetLength(Self & m, const unsigned int s)
  {
    if ( s != 2 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
      }
    m = NumericTraits< ValueType >::ZeroValue();
  }
};

/// \endcond

} // end namespace itk

#undef itkNUMERIC_TRAITS_C11_ASSINMENT
#include "itkFixedArray.h"

#endif // itkNumericTraits_h
