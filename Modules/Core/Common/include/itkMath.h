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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMath_h
#define itkMath_h

#include "itkMathDetail.h"
#include "itkConceptChecking.h"
#include <vnl/vnl_math.h>

/* Only maintain backwards compatibility with old versions
 * of VXL back to the point where vnl_math:: was introduced
 * versions of VXL where only vnl_math_ was available are not
 * supported.
 */
#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL <= 20121114
# error "VXL version must support vnl_math:: namespace versions of functions"
#endif

namespace itk
{
namespace Math
{
// These constants originate from VXL's vnl_math.h. They have been
// moved here to improve visibility, and to ensure that the constants
// are available during compile time ( as opposed to static ITK_CONSTEXPR
// member vaiables ).


/** \brief \f[e\f] The base of the natural logarithm or Euler's number */
static ITK_CONSTEXPR_VAR double e                = vnl_math::e;
/** \brief  \f[ \log_2 e \f] */
static ITK_CONSTEXPR_VAR double log2e            = vnl_math::log2e;
/** \brief \f[ \log_{10} e \f] */
static ITK_CONSTEXPR_VAR double log10e           = vnl_math::log10e;
/** \brief \f[ \log_e 2 \f] */
static ITK_CONSTEXPR_VAR double ln2              = vnl_math::ln2;
/** \brief \f[ \log_e 10 \f] */
static ITK_CONSTEXPR_VAR double ln10             = vnl_math::ln10;
/** \brief \f[ \pi \f]  */
static ITK_CONSTEXPR_VAR double pi               = vnl_math::pi;
/** \brief \f[ 2\pi \f]  */
static ITK_CONSTEXPR_VAR double twopi            = vnl_math::twopi;
/** \brief \f[ \frac{\pi}{2} \f]  */
static ITK_CONSTEXPR_VAR double pi_over_2        = vnl_math::pi_over_2;
/** \brief \f[ \frac{\pi}{4} \f]  */
static ITK_CONSTEXPR_VAR double pi_over_4        = vnl_math::pi_over_4;
/** \brief \f[ \frac{\pi}{180} \f]  */
static ITK_CONSTEXPR_VAR double pi_over_180      = vnl_math::pi_over_180;
/** \brief \f[ \frac{1}{\pi} \f]  */
static ITK_CONSTEXPR_VAR double one_over_pi      = vnl_math::one_over_pi;
/** \brief \f[ \frac{2}{\pi} \f]  */
static ITK_CONSTEXPR_VAR double two_over_pi      = vnl_math::two_over_pi;
/** \brief \f[ \frac{180}{\pi} \f]  */
static ITK_CONSTEXPR_VAR double deg_per_rad      = vnl_math::deg_per_rad;
/** \brief \f[ \sqrt{2\pi} \f]  */
static ITK_CONSTEXPR_VAR double sqrt2pi          = vnl_math::sqrt2pi;
/** \brief \f[ \frac{2}{\sqrt{\pi}} \f]  */
static ITK_CONSTEXPR_VAR double two_over_sqrtpi  = vnl_math::two_over_sqrtpi;
/** \brief \f[ \frac{2}{\sqrt{2\pi}} \f]  */
static ITK_CONSTEXPR_VAR double one_over_sqrt2pi = vnl_math::one_over_sqrt2pi;
/** \brief \f[ \sqrt{2} \f]  */
static ITK_CONSTEXPR_VAR double sqrt2            = vnl_math::sqrt2;
/** \brief \f[ \sqrt{ \frac{1}{2}} \f] */
static ITK_CONSTEXPR_VAR double sqrt1_2          = vnl_math::sqrt1_2;
/** \brief \f[ \sqrt{ \frac{1}{3}} \f] */
static ITK_CONSTEXPR_VAR double sqrt1_3          = vnl_math::sqrt1_3;
/** \brief euler constant */
static ITK_CONSTEXPR_VAR double euler            = vnl_math::euler;

//: IEEE double machine precision
static ITK_CONSTEXPR_VAR double eps              = vnl_math::eps;
static ITK_CONSTEXPR_VAR double sqrteps          = vnl_math::sqrteps;
//: IEEE single machine precision
static ITK_CONSTEXPR_VAR float  float_eps        = vnl_math::float_eps;
static ITK_CONSTEXPR_VAR float  float_sqrteps    = vnl_math::float_sqrteps;

/** A useful macro to generate a template floating point to integer
 *  conversion templated on the return type and using either the 32
 *  bit, the 64 bit or the vanilla version */
#define itkTemplateFloatingToIntegerMacro(name)                                     \
  template< typename TReturn, typename TInput >                                     \
  inline TReturn name(TInput x)                                                     \
    {                                                                               \
                                                                                    \
    if ( sizeof( TReturn ) <= 4 )                                                   \
      {                                                                             \
      return static_cast< TReturn >( Detail::name##_32(x) );                        \
      }                                                                             \
    else if ( sizeof( TReturn ) <= 8 )                                              \
      {                                                                             \
      return static_cast< TReturn >( Detail::name##_64(x) );                        \
      }                                                                             \
    else                                                                            \
      {                                                                             \
      return static_cast< TReturn >( Detail::name##_base< TReturn, TInput >(x) );   \
      }                                                                             \
    }

/** \brief Round towards nearest integer
 *
 *  \tparam TReturn must be an integer type
 *  \tparam TInput must be float or double
 *
 *          halfway cases are rounded towards the nearest even
 *          integer, e.g.
 *  \code
 *          RoundHalfIntegerToEven( 1.5) ==  2
 *          RoundHalfIntegerToEven(-1.5) == -2
 *          RoundHalfIntegerToEven( 2.5) ==  2
 *          RoundHalfIntegerToEven( 3.5) ==  4
 *  \endcode
 *
 *  The behavior of overflow is undefined due to numerous implementations.
 *
 *  \warning We assume that the rounding mode is not changed from the default
 *  one (or at least that it is always restored to the default one).
 */
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerToEven);

/** \brief Round towards nearest integer
 *
 *  \tparam TReturn must be an integer type
 *  \tparam TInput must be float or double
 *
 *          halfway cases are rounded upward, e.g.
 *  \code
 *          RoundHalfIntegerUp( 1.5) ==  2
 *          RoundHalfIntegerUp(-1.5) == -1
 *          RoundHalfIntegerUp( 2.5) ==  3
 *  \endcode
 *
 *  The behavior of overflow is undefined due to numerous implementations.
 *
 *  \warning The argument absolute value must be less than
 *  NumbericTraits<TReturn>::max()/2 for RoundHalfIntegerUp to be
 *  guaranteed to work.
 *
 *  \warning We also assume that the rounding mode is not changed from
 *  the default one (or at least that it is always restored to the
 *  default one).
 */
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerUp);

/** \brief Round towards nearest integer (This is a synonym for RoundHalfIntegerUp)
 *
 *  \tparam TReturn must be an integer type
 *  \tparam TInput must be float or double
 *
 *  \sa RoundHalfIntegerUp<TReturn, TInput>()
 */
template< typename TReturn, typename TInput >
inline TReturn Round(TInput x) { return RoundHalfIntegerUp< TReturn, TInput >(x); }

/** \brief Round towards minus infinity
 *
 *  The behavior of overflow is undefined due to numerous implementations.
 *
 *  \warning argument absolute value must be less than
 *  NumbericTraits<TReturn>::max()/2 for vnl_math_floor to be
 *  guaranteed to work.
 *
 *  \warning We also assume that the rounding mode is not changed from
 *  the default one (or at least that it is always restored to the
 *  default one).
 */
itkTemplateFloatingToIntegerMacro(Floor);

/** \brief Round towards plus infinity
 *
 *  The behavior of overflow is undefined due to numerous implementations.
 *
 *  \warning argument absolute value must be less than INT_MAX/2
 *  for vnl_math_ceil to be guaranteed to work.
 *  \warning We also assume that the rounding mode is not changed from
 *  the default one (or at least that it is always restored to the
 *  default one).
 */
itkTemplateFloatingToIntegerMacro(Ceil);

#undef  itkTemplateFloatingToIntegerMacro

template< typename TReturn, typename TInput >
inline TReturn CastWithRangeCheck(TInput x)
{
#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( OnlyDefinedForIntegerTypes1, ( itk::Concept::IsInteger< TReturn > ) );
  itkConceptMacro( OnlyDefinedForIntegerTypes2, ( itk::Concept::IsInteger< TInput > ) );
#endif // ITK_USE_CONCEPT_CHECKING

  TReturn ret = static_cast< TReturn >( x );
  if ( sizeof( TReturn ) > sizeof( TInput )
       && !( !itk::NumericTraits< TReturn >::is_signed &&  itk::NumericTraits< TInput >::is_signed ) )
    {
    // if the output type is bigger and we are not converting a signed
    // integer to an unsigned integer then we have no problems
    return ret;
    }
  else if ( sizeof( TReturn ) >= sizeof( TInput ) )
    {
    if ( itk::NumericTraits< TInput >::IsPositive(x) != itk::NumericTraits< TReturn >::IsPositive(ret) )
      {
      itk::RangeError _e(__FILE__, __LINE__);
      throw _e;
      }
    }
  else if ( static_cast< TInput >( ret ) != x
            || ( itk::NumericTraits< TInput >::IsPositive(x) != itk::NumericTraits< TReturn >::IsPositive(ret) ) )
    {
    itk::RangeError _e(__FILE__, __LINE__);
    throw _e;
    }
  return ret;
}

/** \brief Return the signed distance in ULPs (units in the last place) between two floats.
 *
 * This is the signed distance, i.e., if x1 > x2, then the result is positive.
 *
 * \sa FloatAlmostEqual
 * \sa FloatAddULP
 */
template <typename T>
inline typename Detail::FloatIEEE<T>::IntType
FloatDifferenceULP( T x1, T x2 )
{
  Detail::FloatIEEE<T> x1f(x1);
  Detail::FloatIEEE<T> x2f(x2);
  return x1f.AsULP() - x2f.AsULP();
}

/** \brief Add the given ULPs (units in the last place) to a float.
 *
 * If the given ULPs can are negative, they are subtracted.
 *
 * \sa FloatAlmostEqual
 * \sa FloatDifferenceULP
 */
template <typename T>
inline T
FloatAddULP( T x, typename Detail::FloatIEEE<T>::IntType ulps )
{
  Detail::FloatIEEE<T> representInput( x );
  Detail::FloatIEEE<T> representOutput( representInput.asInt + ulps );
  return representOutput.asFloat;
}

/** \brief Compare two floats and return if they are effectively equal.
 *
 * Determining when floats are almost equal is difficult because of their
 * IEEE bit representation.  This function uses the integer representation of
 * the float to determine if they are almost equal.
 *
 * The implementation is based off the explanation in the white papers:
 *
 * - http://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
 * - http://randomascii.wordpress.com/category/floating-point/
 *
 * This function is not a cure-all, and reading those articles is important
 * to understand its appropriate use in the context of ULPs, zeros, subnormals,
 * infinities, and NANs.  For example, it is preferable to use this function on
 * two floats directly instead of subtracting them and comparing them to zero.
 *
 * The tolerance is specified in ULPs (units in the last place), i.e. how many
 * floats there are in between the numbers.  Therefore, the tolerance depends on
 * the magnitude of the values that are being compared.  A second tolerance is
 * a maximum difference allowed, which is important when comparing numbers close to
 * zero.
 *
 * A NAN compares as not equal to a number, but two NAN's may compare as equal
 * to each other.
 *
 * \param x1                    first floating value to compare
 * \param x2                    second floating values to compare
 * \param maxUlps               maximum units in the last place to be considered equal
 * \param maxAbsoluteDifference maximum absolute difference to be considered equal
 */
template <typename T>
inline bool
FloatAlmostEqual( T x1, T x2,
  typename Detail::FloatIEEE<T>::IntType maxUlps = 4,
  typename Detail::FloatIEEE<T>::FloatType maxAbsoluteDifference = 0.1*itk::NumericTraits<T>::epsilon() )
{
  // Check if the numbers are really close -- needed
  // when comparing numbers near zero.
  const T absDifference = std::abs(x1 - x2);
  if ( absDifference <= maxAbsoluteDifference )
    {
    return true;
    }

#if defined(__APPLE__) && (__clang_major__ == 3) && (__clang_minor__ == 0) && defined(NDEBUG) && defined(__x86_64__)
  Detail::FloatIEEE<T> x1f(x1);
  Detail::FloatIEEE<T> x2f(x2);
  double x1fAsULP = static_cast<double>(x1f.AsULP());
  double x2fAsULP = static_cast<double>(x2f.AsULP());
  double ulps = x1fAsULP - x2fAsULP;
  if(ulps < 0)
    {
    ulps = -ulps;
    }
  return ulps <= static_cast<double>(maxUlps);
#else
   typename Detail::FloatIEEE<T>::IntType
    ulps = FloatDifferenceULP(x1, x2);
  if(ulps < 0)
    {
    ulps = -ulps;
    }
  return ulps <= maxUlps;
#endif
}

// The following code cannot be moved to the itkMathDetail.h file without introducing circular dependencies
namespace Detail  // The Detail namespace holds the templates used by AlmostEquals
{
// The following structs and templates are used to choose
// which version of the AlmostEquals function
// should be implemented base on input parameter types

// Structs for choosing AlmostEquals function

struct AlmostEqualsFloatVsFloat
{
  template <typename TFloatType1, typename TFloatType2>
  static bool AlmostEqualsFunction(TFloatType1 x1, TFloatType2 x2)
  {
    return FloatAlmostEqual<double>(x1, x2);
  }

  template <typename TFloatType1, typename TFloatType2>
  static bool
  AlmostEqualsFunction(double x1, double x2)
  {
    return FloatAlmostEqual<double>(x1, x2);
  }

  template <typename TFloatType1, typename TFloatType2>
  static bool
  AlmostEqualsFunction(double x1, float x2)
  {
    return FloatAlmostEqual<float>(x1, x2);
  }

  template <typename TFloatType1, typename TFloatType2>
  static bool
  AlmostEqualsFunction(float x1, double x2)
  {
    return FloatAlmostEqual<float>(x1, x2);
  }

  template <typename TFloatType1, typename TFloatType2>
  static bool
  AlmostEqualsFunction(float x1, float x2)
  {
    return FloatAlmostEqual<float>(x1, x2);
  }
};

struct AlmostEqualsFloatVsInteger
{
  template <typename TFloatType, typename TIntType>
  static bool AlmostEqualsFunction(TFloatType floatingVariable, TIntType integerVariable)
  {
    return FloatAlmostEqual<TFloatType> (floatingVariable, integerVariable);
  }
};

struct AlmostEqualsIntegerVsFloat
{
  template <typename TIntType, typename TFloatType>
  static bool AlmostEqualsFunction(TIntType integerVariable, TFloatType floatingVariable)
  {
    return AlmostEqualsFloatVsInteger::AlmostEqualsFunction(floatingVariable, integerVariable);
  }
};

struct AlmostEqualsSignedVsUnsigned
{
  template <typename TSignedInt, typename TUnsignedInt>
  static bool AlmostEqualsFunction(TSignedInt signedVariable, TUnsignedInt unsignedVariable)
  {
    if(signedVariable < 0) return false;
    if( unsignedVariable > static_cast< size_t >(itk::NumericTraits<TSignedInt>::max()) ) return false;
    return signedVariable == static_cast< TSignedInt >(unsignedVariable);
  }
};

struct AlmostEqualsUnsignedVsSigned
{
  template <typename TUnsignedInt, typename TSignedInt>
  static bool AlmostEqualsFunction(TUnsignedInt unsignedVariable, TSignedInt signedVariable)
  {
    return AlmostEqualsSignedVsUnsigned::AlmostEqualsFunction(signedVariable, unsignedVariable);
  }
};

struct AlmostEqualsPlainOldEquals
{
  template <typename TIntegerType1, typename TIntegerType2>
  static bool AlmostEqualsFunction(TIntegerType1 x1, TIntegerType2 x2)
  {
    return x1 == x2;
  }
};
// end of structs that choose the specific AlmostEquals function

// Selector structs, these select the correct case based on its types
//        input1 is int?  input 1 is signed? input2 is int?  input 2 is signed?
template<bool TInput1IsIntger, bool TInput1IsSigned, bool TInput2IsInteger, bool TInput2IsSigned>
struct AlmostEqualsFunctionSelector
{ // default case
  typedef AlmostEqualsPlainOldEquals SelectedVersion;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION
template<>
struct AlmostEqualsFunctionSelector < false, true, false, true>
// floating type v floating type
{
  typedef AlmostEqualsFloatVsFloat SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector <false, true, true, true>
// float vs signed int
{
  typedef AlmostEqualsFloatVsInteger SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector <false, true, true,false>
// float vs unsigned int
{
  typedef AlmostEqualsFloatVsInteger SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector <true, false, false, true>
// unsigned int vs float
{
  typedef AlmostEqualsIntegerVsFloat SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector <true, true, false, true>
// signed int vs float
{
  typedef AlmostEqualsIntegerVsFloat SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector<true, true, true, false>
// signed vs unsigned
{
  typedef AlmostEqualsSignedVsUnsigned SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector<true, false, true, true>
// unsigned vs signed
{
  typedef AlmostEqualsUnsignedVsSigned SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector<true, true, true, true>
//   signed vs signed
{
  typedef AlmostEqualsPlainOldEquals SelectedVersion;
};

template<>
struct AlmostEqualsFunctionSelector<true, false, true, false>
// unsigned vs unsigned
{
  typedef AlmostEqualsPlainOldEquals SelectedVersion;
};
// end of AlmostEqualsFunctionSelector structs

 // The implementor tells the selector what to do
template<typename TInputType1, typename TInputType2>
struct AlmostEqualsScalarImplementer
{
  static ITK_CONSTEXPR_VAR bool TInputType1IsInteger = itk::NumericTraits<TInputType1>::IsInteger;
  static ITK_CONSTEXPR_VAR bool TInputType1IsSigned  = itk::NumericTraits<TInputType1>::IsSigned;
  static ITK_CONSTEXPR_VAR bool TInputType2IsInteger = itk::NumericTraits<TInputType2>::IsInteger;
  static ITK_CONSTEXPR_VAR bool TInputType2IsSigned  = itk::NumericTraits<TInputType2>::IsSigned;

  typedef typename AlmostEqualsFunctionSelector< TInputType1IsInteger, TInputType1IsSigned,
                                                 TInputType2IsInteger, TInputType2IsSigned >::SelectedVersion SelectedVersion;
};

// The AlmostEqualsScalarComparer returns the result of an
// approximate comparison between two scalar values of
// potentially different data types.
template <typename TScalarType1, typename TScalarType2>
inline bool
AlmostEqualsScalarComparer( TScalarType1 x1, TScalarType2 x2 )
{
  return AlmostEqualsScalarImplementer<TScalarType1, TScalarType2>::SelectedVersion:: template AlmostEqualsFunction<TScalarType1, TScalarType2>(x1, x2);
}

// The following structs are used to evaluate approximate comparisons between
// complex and scalar values of potentially different types.

// Comparisons between scalar types use the AlmostEqualsScalarComparer function.
struct AlmostEqualsScalarVsScalar
{
  template <typename TScalarType1, typename TScalarType2>
  static bool
  AlmostEqualsFunction(TScalarType1 x1, TScalarType2 x2)
  {
    return AlmostEqualsScalarComparer(x1, x2);
  }
};

// Comparisons between two complex values compare the real and imaginary components
// separately with the AlmostEqualsScalarComparer function.
struct AlmostEqualsComplexVsComplex
{
  template <typename TComplexType1, typename TComplexType2>
  static bool
  AlmostEqualsFunction(TComplexType1 x1, TComplexType2 x2)
  {
    return AlmostEqualsScalarComparer(x1.real(), x2.real()) && AlmostEqualsScalarComparer( x1.imag(), x2.imag() );
  }
};

// Comparisons between complex and scalar values first check to see if the imaginary component
// of the complex value is approximately 0. Then a ScalarComparison is done between the real
// part of the complex number and the scalar value.
struct AlmostEqualsScalarVsComplex
{
  template <typename TScalarType, typename TComplexType>
  static bool
  AlmostEqualsFunction(TScalarType scalarVariable, TComplexType complexVariable)
  {
    if( !AlmostEqualsScalarComparer( complexVariable.imag(), itk::NumericTraits< typename itk::NumericTraits< TComplexType >::ValueType >::ZeroValue() ) )
      {
      return false;
      }
    return AlmostEqualsScalarComparer(scalarVariable, complexVariable.real());
  }
};

struct AlmostEqualsComplexVsScalar
{
  template <typename TComplexType, typename TScalarType>
  static bool
  AlmostEqualsFunction(TComplexType complexVariable, TScalarType scalarVariable)
  {
    return AlmostEqualsScalarVsComplex::AlmostEqualsFunction(scalarVariable, complexVariable);
  }
};

// The AlmostEqualsComplexChooser structs choose the correct case
// from the input parameter types' IsComplex property
// The default case is scalar vs scalar
template < bool T1IsComplex, bool T2IsComplex > //Default is false, false
struct AlmostEqualsComplexChooser
{
  typedef AlmostEqualsScalarVsScalar ChosenVersion;
};

template <>
struct AlmostEqualsComplexChooser< true, true >
{
  typedef AlmostEqualsComplexVsComplex ChosenVersion;
};

template <>
struct AlmostEqualsComplexChooser< false, true >
{
  typedef AlmostEqualsScalarVsComplex ChosenVersion;
};

template <>
struct AlmostEqualsComplexChooser< true, false>
{
  typedef AlmostEqualsComplexVsScalar ChosenVersion;
};
// End of AlmostEqualsComplexChooser structs.

// The AlmostEqualsComplexImplementer determines which of the input
// parameters are complex and which are real, and sends that information
// to the AlmostEqualsComplexChooser structs to determine the proper
// type of evaluation.
template <typename T1, typename T2>
struct AlmostEqualsComplexImplementer
{
  static ITK_CONSTEXPR_VAR bool T1IsComplex = NumericTraits< T1 >::IsComplex;
  static ITK_CONSTEXPR_VAR bool T2IsComplex = NumericTraits< T2 >::IsComplex;

  typedef typename AlmostEqualsComplexChooser< T1IsComplex, T2IsComplex >::ChosenVersion ChosenVersion;
};
/// \endcond

} // end namespace Detail

/** \brief Provide consistent equality checks between values of potentially different scalar or complex types
 *
 * template< typename T1, typename T2 >
 * AlmostEquals( T1 x1, T2 x2 )
 *
 * template< typename T1, typename T2 >
 * NotAlmostEquals( T1 x1, T2 x2 )
 *
 * This function compares two scalar or complex values of potentially different types.
 * For maximum extensibility the function is implemented through a series of templated
 * structs which direct the AlmostEquals() call to the correct function by evaluating
 * the parameter's types.
 *
 * Overall algorithm:
 *   If both values are complex...
 *     separate values into real and imaginary components and compare them separately
 *
 *   If one of the values is complex..
 *     see if the imaginary part of the complex value is approximately 0 ...
 *       compare real part of complex value with scalar value
 *
 *   If both values are scalars...
 *
 *   To compare two floating point types...
 *     use FloatAlmostEqual.
 *
 *   To compare a floating point and an integer type...
 *        Use static_cast<FloatingPointType>(integerValue) and call FloatAlmostEqual
 *
 *   To compare signed and unsigned integers...
 *     Check for negative value or overflow, then cast and use ==
 *
 *   To compare two signed or two unsigned integers ...
 *     Use ==
 *
 *   To compare anything else ...
 *     Use ==
 *
 * \param x1                    first scalar value to compare
 * \param x2                    second scalar value to compare
 */

// The AlmostEquals function
template <typename T1, typename T2>
inline bool
AlmostEquals( T1 x1, T2 x2 )
{
  return Detail::AlmostEqualsComplexImplementer<T1,T2>::ChosenVersion::AlmostEqualsFunction(x1, x2);
}

// The NotAlmostEquals function
template <typename T1, typename T2>
inline bool
NotAlmostEquals( T1 x1, T2 x2 )
{
  return ! AlmostEquals( x1, x2 );
}


/** \brief  Return the result of an exact comparison between two scalar values of potetially different types.
 *
 * template <typename TInput1, typename TInput2>
 * inline bool
 * ExactlyEquals( const TInput & x1, const TInput & x2 )
 *
 * template <typename TInput1, typename TInput2>
 * inline bool
 * NotExactlyEquals( const TInput & x1, const TInput & x2 )
 *
 * These functions complement the EqualsComparison functions and determine if two scalar
 * values are exactly equal using the compilers casting rules when comparing two different types.
 * While this is also easily accomplished by using the equality operators,
 * use of this function demonstrates the intent of an exact equality check and thus improves
 * readability and clarity of code. In addition, this function suppresses float-equal warnings
 * produced when using Clang.
 *
 * \param x1                    first floating point value to compare
 * \param x2                    second floating point value to compare
 */

// The ExactlyEquals function
template <typename TInput1, typename TInput2>
inline bool
ExactlyEquals( const TInput1 & x1, const TInput2 & x2 )
{
CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
  return x1 == x2;
CLANG_PRAGMA_POP
}

//The NotExactlyEquals function
template <typename TInput1, typename TInput2>
inline bool
NotExactlyEquals( const TInput1 & x1, const TInput2 & x2 )
{
  return !ExactlyEquals(x1, x2);
}


/** Return whether the number is a prime number or not.
 *
 * \note Negative numbers cannot be prime.
 */
ITKCommon_EXPORT bool IsPrime( unsigned short n );
ITKCommon_EXPORT bool IsPrime( unsigned int n );
ITKCommon_EXPORT bool IsPrime( unsigned long n );
ITKCommon_EXPORT bool IsPrime( unsigned long long n );


/** Return the greatest factor of the decomposition in prime numbers. */
ITKCommon_EXPORT unsigned short     GreatestPrimeFactor( unsigned short n );
ITKCommon_EXPORT unsigned int       GreatestPrimeFactor( unsigned int n );
ITKCommon_EXPORT unsigned long      GreatestPrimeFactor( unsigned long n );
ITKCommon_EXPORT unsigned long long GreatestPrimeFactor( unsigned long long n );


/*==========================================
 * Alias the vnl_math functions in the itk::Math
 * namespace. If possible, use the std:: equivalents
 */
#if  ITK_COMPILED_CXX_STANDARD_VERSION >= 201103L

/** A macro to allow perfect forwarding of functions using
  * C++11 or greater features
  * http://stackoverflow.com/questions/9864125/c11-how-to-alias-a-function
  */
#define ITK_PERFECT_FORWARD_MACRO(new_name, old_name) \
  template <typename... TArgs> \
    auto new_name(TArgs&&... args) -> decltype(old_name(std::forward<TArgs>(args)...)) { \
      return old_name(std::forward<TArgs>(args)...); \
    }

  // Prefer to use perfect forwarding to the std library if C++11 features are available and consistent with vnl
ITK_PERFECT_FORWARD_MACRO(isnan,std::isnan);
ITK_PERFECT_FORWARD_MACRO(isinf,std::isinf);
ITK_PERFECT_FORWARD_MACRO(isfinite,std::isfinite);
ITK_PERFECT_FORWARD_MACRO(isnormal,std::isnormal);
ITK_PERFECT_FORWARD_MACRO(cbrt,std::cbrt);
ITK_PERFECT_FORWARD_MACRO(hypot,std::hypot);
// Pefect forwarding to vnl specializations
ITK_PERFECT_FORWARD_MACRO(angle_0_to_2pi,vnl_math::angle_0_to_2pi);
ITK_PERFECT_FORWARD_MACRO(angle_minuspi_to_pi,vnl_math::angle_minuspi_to_pi);
ITK_PERFECT_FORWARD_MACRO(rnd_halfinttoeven,vnl_math::rnd_halfinttoeven);
ITK_PERFECT_FORWARD_MACRO(rnd_halfintup,vnl_math::rnd_halfintup);
ITK_PERFECT_FORWARD_MACRO(rnd,vnl_math::rnd);
ITK_PERFECT_FORWARD_MACRO(floor,vnl_math::floor);
ITK_PERFECT_FORWARD_MACRO(ceil,vnl_math::ceil);
ITK_PERFECT_FORWARD_MACRO(sgn,vnl_math::sgn);
ITK_PERFECT_FORWARD_MACRO(sgn0,vnl_math::sgn0);
ITK_PERFECT_FORWARD_MACRO(remainder_truncated,vnl_math::remainder_truncated);
ITK_PERFECT_FORWARD_MACRO(remainder_floored,vnl_math::remainder_floored);
ITK_PERFECT_FORWARD_MACRO(abs,vnl_math::abs);
ITK_PERFECT_FORWARD_MACRO(sqr,vnl_math::sqr);
ITK_PERFECT_FORWARD_MACRO(cube,vnl_math::cube);
ITK_PERFECT_FORWARD_MACRO(squared_magnitude,vnl_math::squared_magnitude);

#undef ITK_PERFECT_FORWARD_MACRO

#else
template<typename T> bool isnan(const T value) { return vnl_math::isnan(value); }
template<typename T> bool isinf(const T value) { return vnl_math::isinf(value); }
template<typename T> bool isfinite(const T value) { return vnl_math::isfinite(value); }
template<typename T> T cbrt(const T value) { return vnl_math::cuberoot(value); }
template<typename T> T hypot(const T value1, const T value2) { return vnl_math::hypot(value1,value2); }
template<typename T> T angle_0_to_2pi(const T angle) { return vnl_math::angle_0_to_2pi(angle); }
template<typename T> T angle_minuspi_to_pi(const T angle) { return vnl_math::angle_minuspi_to_pi(angle); }
template<typename T> inline int rnd_halfinttoeven(const T x) {return vnl_math::rnd_halfinttoeven(x); }
template<typename T> inline int rnd_halfintup(const T x) { return vnl_math::rnd_halfintup(x); }
template<typename T> inline int rnd(const T x) { return vnl_math::rnd(x); }
template<typename T> inline int floor(const T x) { return vnl_math::floor(x); }
template<typename T> inline int ceil(const T x) { return vnl_math::ceil(x); }
template<typename T> int sgn(const T x)       { return vnl_math::sgn(x); }
template<typename T> int sgn0(const T x)       { return vnl_math::sgn0(x); }
template<typename T> T remainder_truncated(const T x, const T y) { return vnl_math::remainder_truncated(x,y); }
template<typename T> T remainder_floored(const T x, const T y) { return vnl_math::remainder_floored(x,y); }

inline bool               abs(const bool x)               { return x; }
inline unsigned char      abs(const unsigned char x)      { return x; }
inline unsigned char      abs(const signed char x)        { return vnl_math::abs(x); }
inline unsigned char      abs(const char x)               { return vnl_math::abs(x); }
inline unsigned short     abs(const short x)              { return vnl_math::abs(x); }
inline unsigned short     abs(const unsigned short x)     { return x; }
inline unsigned int       abs(const int x)                { return vnl_math::abs(x); }
inline unsigned int       abs(const unsigned int x)       { return x; }
inline unsigned long      abs(const long x)               { return vnl_math::abs(x); }
inline unsigned long      abs(const unsigned long x)      { return x; }
#if VCL_HAS_LONG_LONG
inline unsigned long long abs(const long long x)          { return vnl_math::abs(x); }
inline unsigned long long abs(const unsigned long long x) { return x; }
#endif
inline float              abs(const float x)              { return std::abs(x); }
inline double             abs(const double x)             { return std::abs(x); }
inline long double        abs(const long double x)        { return std::abs(x); }

inline bool               sqr(const bool x)               { return vnl_math::sqr(x); }
inline int                sqr(const int x)                { return vnl_math::sqr(x); }
inline unsigned int       sqr(const unsigned int x)       { return vnl_math::sqr(x); }
inline long               sqr(const long x)               { return vnl_math::sqr(x); }
inline unsigned long      sqr(const unsigned long x)      { return vnl_math::sqr(x); }
#if VCL_HAS_LONG_LONG
inline long long          sqr(const long long x)          { return vnl_math::sqr(x); }
inline unsigned long long sqr(const unsigned long long x) { return vnl_math::sqr(x); }
#endif
inline float              sqr(const float x)              { return vnl_math::sqr(x); }
inline double             sqr(const double x)             { return vnl_math::sqr(x); }

inline bool               cube(const bool x)               { return vnl_math::cube(x); }
inline int                cube(const int x)                { return vnl_math::cube(x); }
inline unsigned int       cube(const unsigned int x)       { return vnl_math::cube(x); }
inline long               cube(const long x)               { return vnl_math::cube(x); }
inline unsigned long      cube(const unsigned long x)      { return vnl_math::cube(x); }
#if VCL_HAS_LONG_LONG
inline long long          cube(const long long x)          { return vnl_math::cube(x); }
inline unsigned long long cube(const unsigned long long x) { return vnl_math::cube(x); }
#endif
inline float              cube(const float x)              { return vnl_math::cube(x); }
inline double             cube(const double x)             { return vnl_math::cube(x); }

inline unsigned int       squared_magnitude(const char               x) { return vnl_math::squared_magnitude(x); }
inline unsigned int       squared_magnitude(const unsigned char      x) { return vnl_math::squared_magnitude(x); }
inline unsigned int       squared_magnitude(const int                x) { return vnl_math::squared_magnitude(x); }
inline unsigned int       squared_magnitude(const unsigned int       x) { return vnl_math::squared_magnitude(x); }
inline unsigned long      squared_magnitude(const long               x) { return vnl_math::squared_magnitude(x); }
inline unsigned long      squared_magnitude(const unsigned long      x) { return vnl_math::squared_magnitude(x); }
#if VCL_HAS_LONG_LONG
inline unsigned long long squared_magnitude(const long long          x) { return vnl_math::squared_magnitude(x); }
inline unsigned long long squared_magnitude(const unsigned long long x) { return vnl_math::squared_magnitude(x); }
#endif
inline float              squared_magnitude(const float              x) { return vnl_math::squared_magnitude(x); }
inline double             squared_magnitude(const double             x) { return vnl_math::squared_magnitude(x); }
inline long double        squared_magnitude(const long double        x) { return vnl_math::squared_magnitude(x); }

#endif //If not C++11 features

} // end namespace Math
} // end namespace itk

#endif // end of itkMath.h
