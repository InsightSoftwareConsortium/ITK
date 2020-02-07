/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#define itkNUMERIC_TRAITS_MIN_MAX_MACRO()                                                                              \
  static constexpr ValueType min(ValueType) { return std::numeric_limits<ValueType>::min(); }                          \
  static constexpr ValueType max(ValueType) { return std::numeric_limits<ValueType>::max(); }                          \
  static constexpr ValueType min() { return std::numeric_limits<ValueType>::min(); }                                   \
  static constexpr ValueType max() { return std::numeric_limits<ValueType>::max(); }

#include <limits> // for std::numeric_limits
#include <complex>

namespace itk
{

// forward decare to avoid circular dependencies
template <typename TValue, unsigned int VLength>
class FixedArray;

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
 * \sphinx
 * \sphinxexample{Core/Common/GetTypeBasicInformation,Get Type Basic Information}
 * \endsphinx
 */
template <typename T>
class NumericTraits : public std::numeric_limits<T>
{
public:
  /** The type of this limits trait object. */
  using TraitsType = std::numeric_limits<T>;

  /** Return the type of this native type. */
  using ValueType = T;

  /** Return the type that can be printed. */
  using PrintType = T;

  /** Return value of std::abs(). */
  using AbsType = T;

  /** Accumulation of addition and multiplication. */
  using AccumulateType = double;

  /** Measurement vector type */
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  /** Typedef for operations that use floating point instead of real precision
   *  to save memory */
  using FloatType = float;

  /** Type for real-valued scalar operations. */
  using RealType = double;

  /** Type for real-valued scalar operations. */
  using ScalarRealType = RealType;

  /** Additive identity. */
  static const T ITKCommon_EXPORT Zero;

  /** Multiplicative identity. */
  static const T ITKCommon_EXPORT One;

  /** Smallest (most nonpositive) value */
  static constexpr T
  NonpositiveMin()
  {
    return TraitsType::lowest();
  }

  /** Is a given value positive? */
  static bool
  IsPositive(T val)
  {
    return val > Zero;
  }

  /** Is a given value nonpositive? */
  static bool
  IsNonpositive(T val)
  {
    return val <= Zero;
  }

  /** Is a given value negative? */
  static bool
  IsNegative(T val)
  {
    return val < Zero;
  }

  /** Is a given value nonnegative? */
  static bool
  IsNonnegative(T val)
  {
    return val >= Zero;
  }

  /** Is a given type signed? -- default is no.
      For uniform array data types in ITK, the value of IsSigned
      is determined by the component elements of the array.*/
  static constexpr bool IsSigned = false;

  /** Is a given type an integer? -- default is no.
      For uniform array data types in ITK, the value of IsInteger
      is determined by the component elements of the array.*/
  static constexpr bool IsInteger = false;

  /** Is a given type complex? -- default is no.
      For uniform array data types in ITK, the value of IsComplex
      is determined by the component elements of the array.*/
  static constexpr bool IsComplex = false;

  /** Return zero value. This function should be used to support
   *  RGBPixel type and standard types (not vectors) */
  static T
  ZeroValue()
  {
    return Zero;
  }

  /** Return one value. This function should be used to support
   *  RGBPixel type and standard types (not vectors) */
  static T
  OneValue()
  {
    return One;
  }

  /* Provide a default implementation of the max() method with
   * argument. This API is needed for VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built. */
  static constexpr T
  max(const T &)
  {
    return TraitsType::max();
  }
  static constexpr T
  min(const T &)
  {
    return TraitsType::min();
  }

  /** Scalars cannot be resized, so an exception will
   * be thrown if the input size is not 1.  If the size is valid
   * the will be zeros. This API is needed for VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built.
   */
  static void
  SetLength(T & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
  /** Return the length of the scalar. This API is needed for
   * VariableLengthVector because
   * its length is only known at run-time. Specializations of the
   * VariableLengthVector will provide a different implementation
   * where a vector of the correct size is built.
   */
  static unsigned int
  GetLength(const T &)
  {
    return GetLength();
  }

  /** Return the length of the scalar: 1. Array types can return a different value */
  static unsigned int
  GetLength()
  {
    return 1;
  }

  /** Smallest (most nonpositive) value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T
  NonpositiveMin(const T &)
  {
    return NonpositiveMin();
  }

  /** Zero value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T
  ZeroValue(const T &)
  {
    return ZeroValue();
  }

  /** One value. This API is needed for
   * VariableLengthVector because its length is only known at run-time.
   */
  static T
  OneValue(const T &)
  {
    return OneValue();
  }

  /** assign the value to an array */
  template <typename TArray>
  static void
  AssignToArray(const T & v, TArray & mv)
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

template <>
class NumericTraits<bool> : public std::numeric_limits<bool>
{
public:
  using ValueType = bool;
  using PrintType = bool;
  using AbsType = unsigned char;
  using AccumulateType = unsigned char;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr bool ITKCommon_EXPORT Zero = false;
  static constexpr bool ITKCommon_EXPORT One = true;

  static constexpr bool
  min()
  {
    return false;
  }
  static constexpr bool
  max()
  {
    return true;
  }
  static constexpr bool
  min(bool)
  {
    return min();
  }
  static constexpr bool
  max(bool)
  {
    return max();
  }
  static constexpr bool
  NonpositiveMin()
  {
    return false;
  }
  static constexpr bool
  IsPositive(bool val)
  {
    return val;
  }
  static constexpr bool
  IsNonpositive(bool val)
  {
    return !val;
  }
  static constexpr bool
  IsNegative(bool val)
  {
    return val ? false : false;
  }
  static constexpr bool
  IsNonnegative(bool val)
  {
    return val ? true : true;
  }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr bool
  ZeroValue()
  {
    return Zero;
  }
  static constexpr bool
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guaranteed to be signed. On SGI computers, the default is unsigned
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<char> : public std::numeric_limits<char>
{
public:
  using ValueType = char;
  using PrintType = int;
  using AbsType = unsigned char;
  using AccumulateType = short;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr char ITKCommon_EXPORT Zero = 0;
  static constexpr char ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();

  static constexpr char
  NonpositiveMin()
  {
    return lowest();
  }
  static constexpr bool
  IsPositive(char val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(char val)
  {
    return val <= Zero;
  }

  static constexpr bool
  IsNegative(char)
  {
    return false;
  }
  static constexpr bool
  IsNonnegative(char)
  {
    return true;
  }
  static constexpr bool IsSigned = std::numeric_limits<char>::is_signed;

  static constexpr bool IsInteger = std::numeric_limits<char>::is_integer;
  static constexpr bool IsComplex = false;
  static constexpr char
  ZeroValue()
  {
    return Zero;
  }
  static constexpr char
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<char>
 * \brief Define traits for type char.
 * NOTE: char is not guaranteed to be signed. On SGI computers, the default is unsigned
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<signed char> : public std::numeric_limits<signed char>
{
public:
  using ValueType = signed char;
  using PrintType = int;
  using AbsType = unsigned char;
  using AccumulateType = short;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr signed char ITKCommon_EXPORT Zero = 0;
  static constexpr signed char ITKCommon_EXPORT One = 1;

  static constexpr signed char
  min()
  {
    return -128;
  }
  static constexpr signed char
  max()
  {
    return 127;
  }
  static constexpr signed char
  min(signed char)
  {
    return min();
  }
  static constexpr signed char
  max(signed char)
  {
    return max();
  }
  static constexpr signed char
  NonpositiveMin()
  {
    return lowest();
  }
  static constexpr bool
  IsPositive(signed char val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(signed char val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(signed char val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(signed char val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr signed char
  ZeroValue()
  {
    return Zero;
  }
  static constexpr signed char
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<unsigned char>
 * \brief Define traits for type unsigned char.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<unsigned char> : public std::numeric_limits<unsigned char>
{
public:
  using ValueType = unsigned char;
  using PrintType = int;
  using AbsType = unsigned char;
  using AccumulateType = unsigned short;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr unsigned char ITKCommon_EXPORT Zero = 0;
  static constexpr unsigned char ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();

  static constexpr unsigned char
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(unsigned char val)
  {
    return val != Zero;
  }
  static constexpr bool
  IsNonpositive(unsigned char val)
  {
    return val == Zero;
  }
  static constexpr bool
  IsNegative(unsigned char val)
  {
    return val ? false : false;
  }
  static constexpr bool
  IsNonnegative(unsigned char val)
  {
    return val ? true : true;
  }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr unsigned char
  ZeroValue()
  {
    return Zero;
  }
  static constexpr unsigned char
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<short>
 * \brief Define traits for type short.
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<short> : public std::numeric_limits<short>
{
public:
  using ValueType = short;
  using PrintType = short;
  using AbsType = unsigned short;
  using AccumulateType = int;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr short ITKCommon_EXPORT Zero = 0;
  static constexpr short ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr short
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(short val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(short val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(short val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(short val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr short
  ZeroValue()
  {
    return Zero;
  }
  static constexpr short
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<unsigned short>
 * \brief Define traits for type unsigned short.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<unsigned short> : public std::numeric_limits<unsigned short>
{
public:
  using ValueType = unsigned short;
  using PrintType = unsigned short;
  using AbsType = unsigned short;
  using AccumulateType = unsigned int;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr unsigned short ITKCommon_EXPORT Zero = 0;
  static constexpr unsigned short ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr unsigned short
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(unsigned short val)
  {
    return val != Zero;
  }
  static constexpr bool
  IsNonpositive(unsigned short val)
  {
    return val == Zero;
  }
  static constexpr bool
  IsNegative(unsigned short val)
  {
    return val ? false : false;
  }
  static constexpr bool
  IsNonnegative(unsigned short val)
  {
    return val ? true : true;
  }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr unsigned short
  ZeroValue()
  {
    return Zero;
  }
  static constexpr unsigned short
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<int>
 * \brief Define traits for type int.
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<int> : public std::numeric_limits<int>
{
public:
  using ValueType = int;
  using PrintType = int;
  using AbsType = unsigned int;
  using AccumulateType = long;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr int ITKCommon_EXPORT Zero = 0;
  static constexpr int ITKCommon_EXPORT One = 1;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr int
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(int val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(int val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(int val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(int val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr int
  ZeroValue()
  {
    return Zero;
  }
  static constexpr int
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<unsigned int>
 * \brief Define traits for type unsigned int.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<unsigned int> : public std::numeric_limits<unsigned int>
{
public:
  using ValueType = unsigned int;
  using PrintType = unsigned int;
  using AbsType = unsigned int;
  using AccumulateType = unsigned int;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr unsigned int ITKCommon_EXPORT Zero = 0;
  static constexpr unsigned int ITKCommon_EXPORT One = 1;

  static constexpr unsigned int
  min()
  {
    return 0;
  }
  static constexpr unsigned int
  max()
  {
    return static_cast<unsigned int>(-1);
  }
  static constexpr unsigned int
  min(unsigned int)
  {
    return std::numeric_limits<ValueType>::min();
  }
  static constexpr unsigned int
  max(unsigned int)
  {
    return std::numeric_limits<ValueType>::max();
  }
  static constexpr unsigned int
  NonpositiveMin()
  {
    return 0;
  }
  static constexpr bool
  IsPositive(unsigned int val)
  {
    return val != Zero;
  }
  static constexpr bool
  IsNonpositive(unsigned int val)
  {
    return val == Zero;
  }
  static constexpr bool
  IsNegative(unsigned int val)
  {
    return val ? false : false;
  }
  static constexpr bool
  IsNonnegative(unsigned int val)
  {
    return val ? true : true;
  }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr unsigned int
  ZeroValue()
  {
    return Zero;
  }
  static constexpr unsigned int
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<long>
 * \brief Define traits for type long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<long> : public std::numeric_limits<long>
{
public:
  using ValueType = long;
  using PrintType = long;
  using AbsType = unsigned long;
  using AccumulateType = long;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr long ITKCommon_EXPORT Zero = 0L;
  static constexpr long ITKCommon_EXPORT One = 1L;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr long
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(long val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(long val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(long val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(long val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr long
  ZeroValue()
  {
    return Zero;
  }
  static constexpr long
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<unsigned long>
 * \brief Define traits for type unsigned long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<unsigned long> : public std::numeric_limits<unsigned long>
{
public:
  using ValueType = unsigned long;
  using PrintType = unsigned long;
  using AbsType = unsigned long;
  using AccumulateType = unsigned long;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr unsigned long ITKCommon_EXPORT Zero = 0UL;
  static constexpr unsigned long ITKCommon_EXPORT One = 1UL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr unsigned long
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(unsigned long val)
  {
    return val != Zero;
  }
  static constexpr bool
  IsNonpositive(unsigned long val)
  {
    return val == Zero;
  }
  static constexpr bool
  IsNegative(unsigned long)
  {
    return false;
  }
  static constexpr bool
  IsNonnegative(unsigned long)
  {
    return true;
  }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr unsigned long
  ZeroValue()
  {
    return Zero;
  }
  static constexpr unsigned long
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<float>
 * \brief Define traits for type float.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<float> : public std::numeric_limits<float>
{
public:
  using ValueType = float;
  using PrintType = float;
  using AbsType = float;
  using AccumulateType = double;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;


  static constexpr float ITKCommon_EXPORT Zero = 0.0f;
  static constexpr float ITKCommon_EXPORT One = 1.0f;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr float
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(float val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(float val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(float val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(float val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = false;
  static constexpr bool IsComplex = false;
  static constexpr float
  ZeroValue()
  {
    return Zero;
  }
  static constexpr float
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<double>
 * \brief Define traits for type double.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<double> : public std::numeric_limits<double>
{
public:
  using ValueType = double;
  using PrintType = double;
  using AbsType = double;
  using AccumulateType = double;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr double ITKCommon_EXPORT Zero = 0.0;
  static constexpr double ITKCommon_EXPORT One = 1.0;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr double
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(double val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(double val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(double val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(double val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = false;
  static constexpr bool IsComplex = false;
  static constexpr double
  ZeroValue()
  {
    return Zero;
  }
  static constexpr double
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<long double>
 * \brief Define traits for type long double.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<long double> : public std::numeric_limits<long double>
{
public:
  using ValueType = long double;
#if defined(__SUNPRO_CC) && defined(_ILP32)
  // sun studio in 32 bit mode is unable to print long double values: it
  // segfaults.
  // conversion to double will give usable results if the value is in the double
  // range - better than nothing.
  using PrintType = double;
#else
  using PrintType = long double;
#endif
  using AbsType = long double;
  using AccumulateType = long double;
  using RealType = long double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr long double ITKCommon_EXPORT Zero = 0.0;
  static constexpr long double ITKCommon_EXPORT One = 1.0;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr long double
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(long double val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(long double val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(long double val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(long double val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = false;
  static constexpr bool IsComplex = false;
  static constexpr long double
  ZeroValue()
  {
    return Zero;
  }
  static constexpr long double
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};


/** \class NumericTraits<long long>
 * \brief Define traits for type long long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<long long> : public std::numeric_limits<long long>
{
public:
  using ValueType = long long;
  using PrintType = long long;
  using AbsType = long long;
  using AccumulateType = long long;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr ValueType ITKCommon_EXPORT Zero = 0LL;
  static constexpr ValueType ITKCommon_EXPORT One = 1LL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr ValueType
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(ValueType val)
  {
    return val > Zero;
  }
  static constexpr bool
  IsNonpositive(ValueType val)
  {
    return val <= Zero;
  }
  static constexpr bool
  IsNegative(ValueType val)
  {
    return val < Zero;
  }
  static constexpr bool
  IsNonnegative(ValueType val)
  {
    return val >= Zero;
  }
  static constexpr bool IsSigned = true;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr ValueType
  ZeroValue()
  {
    return Zero;
  }
  static constexpr ValueType
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};

/** \class NumericTraits<unsigned long long>
 * \brief Define traits for type unsigned long long.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <>
class NumericTraits<unsigned long long> : public std::numeric_limits<unsigned long long>
{
public:
  using ValueType = unsigned long long;
  using PrintType = unsigned long long;
  using AbsType = unsigned long long;
  using AccumulateType = unsigned long long;
  using RealType = double;
  using ScalarRealType = RealType;
  using FloatType = float;
  using MeasurementVectorType = FixedArray<ValueType, 1>;

  static constexpr ValueType ITKCommon_EXPORT Zero = 0ULL;
  static constexpr ValueType ITKCommon_EXPORT One = 1ULL;

  itkNUMERIC_TRAITS_MIN_MAX_MACRO();
  static constexpr ValueType
  NonpositiveMin()
  {
    return std::numeric_limits<ValueType>::lowest();
  }
  static constexpr bool
  IsPositive(ValueType val)
  {
    return val != Zero;
  }
  static constexpr bool
  IsNonpositive(ValueType val)
  {
    return val == Zero;
  }
  static constexpr bool IsNegative(ValueType) { return false; }
  static constexpr bool IsNonnegative(ValueType) { return true; }
  static constexpr bool IsSigned = false;
  static constexpr bool IsInteger = true;
  static constexpr bool IsComplex = false;
  static constexpr ValueType
  ZeroValue()
  {
    return Zero;
  }
  static constexpr ValueType
  OneValue()
  {
    return One;
  }
  static constexpr unsigned int
  GetLength(const ValueType &)
  {
    return 1;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 1;
  }
  static constexpr ValueType
  NonpositiveMin(const ValueType &)
  {
    return NonpositiveMin();
  }
  static constexpr ValueType
  ZeroValue(const ValueType &)
  {
    return ZeroValue();
  }
  static constexpr ValueType
  OneValue(const ValueType &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const ValueType & v, TArray & mv)
  {
    mv[0] = v;
  }
  static void
  SetLength(ValueType & m, const unsigned int s)
  {
    if (s != 1)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a scalar to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};


/** \class NumericTraits< std::complex<TComponent> >
 * \brief Define traits for type std::complex<TComponent>.
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template <typename TComponent>
class NumericTraits<std::complex<TComponent>>
{
public:
  using Self = std::complex<TComponent>;
  // for backward compatibility
  using TheType = Self;
  using ValueType = TComponent;
  using PrintType = Self;
  using AbsType = double;
  using AccumulateType = Self;
  using RealType = std::complex<double>;
  using ScalarRealType = double;
  using FloatType = std::complex<float>;
  using MeasurementVectorType = FixedArray<ValueType, 2>;

  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;

  static constexpr Self
  min()
  {
    return std::numeric_limits<ValueType>::min();
  }
  static constexpr Self
  max()
  {
    return std::numeric_limits<ValueType>::max();
  }
  static constexpr Self min(Self) { return min(); }
  static constexpr Self max(Self) { return max(); }
  static constexpr ValueType
  epsilon()
  {
    return std::numeric_limits<ValueType>::epsilon();
  }
  static constexpr Self
  NonpositiveMin()
  {
    return Self(NumericTraits<ValueType>::NonpositiveMin(), 0);
  }

  static constexpr bool
  IsPositive(Self val)
  {
    return val.real() > 0;
  }
  static constexpr bool
  IsNonpositive(Self val)
  {
    return val.real() <= 0;
  }
  static constexpr bool
  IsNegative(Self val)
  {
    return val.real() < 0;
  }
  static constexpr bool
  IsNonnegative(Self val)
  {
    return val.real() >= 0;
  }

  static constexpr bool IsSigned = NumericTraits<ValueType>::IsSigned;
  static constexpr bool IsInteger = false;
  static constexpr bool IsComplex = true;
  static Self
  ZeroValue()
  {
    return Self(0, 0);
  }
  static Self
  OneValue()
  {
    return Self(1, 0);
  }
  static constexpr unsigned int
  GetLength(const Self &)
  {
    return 2;
  }
  static constexpr unsigned int
  GetLength()
  {
    return 2;
  }
  static constexpr Self
  NonpositiveMin(const Self &)
  {
    return NonpositiveMin();
  }
  static Self
  ZeroValue(const Self &)
  {
    return ZeroValue();
  }
  static Self
  OneValue(const Self &)
  {
    return OneValue();
  }

  template <typename TArray>
  static void
  AssignToArray(const Self & v, TArray & mv)
  {
    mv[0] = v.real();
    mv[1] = v.imag();
  }
  static void
  SetLength(Self & m, const unsigned int s)
  {
    if (s != 2)
    {
      itkGenericExceptionMacro(<< "Cannot set the size of a complex to " << s);
    }
    m = NumericTraits<ValueType>::ZeroValue();
  }
};
/// \endcond
} // end namespace itk

#include "itkFixedArray.h"

#endif // itkNumericTraits_h
