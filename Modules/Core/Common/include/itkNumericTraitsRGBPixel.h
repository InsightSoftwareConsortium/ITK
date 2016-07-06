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
#ifndef itkNumericTraitsRGBPixel_h
#define itkNumericTraitsRGBPixel_h

#include "itkNumericTraits.h"
#include "itkRGBPixel.h"

namespace itk
{
/**
 * \brief Define numeric traits for RGBPixel.
 * \tparam T Component type of RGBPixel
 *
 * We provide here a generic implementation based on creating types of
 * RGBPixel whose components are the types of the NumericTraits from
 * the original RGBPixel components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<RGBPixel< T > >  is defined piecewise by
 *   RGBPixel< NumericTraits< T > >
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename T >
class NumericTraits< RGBPixel< T > >
{
private:

  typedef typename NumericTraits< T >::AbsType        ElementAbsType;
  typedef typename NumericTraits< T >::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits< T >::FloatType      ElementFloatType;
  typedef typename NumericTraits< T >::PrintType      ElementPrintType;
  typedef typename NumericTraits< T >::RealType       ElementRealType;

public:

  /** Return the type of the native component type. */
  typedef T ValueType;

  typedef RGBPixel< T > Self;

  /** Unsigned component type */
  typedef RGBPixel< ElementAbsType > AbsType;

  /** Accumulation of addition and multiplication. */
  typedef RGBPixel< ElementAccumulateType > AccumulateType;

  /** Typedef for operations that use floating point instead of real precision
    */
  typedef RGBPixel< ElementFloatType > FloatType;

  /** Return the type that can be printed. */
  typedef RGBPixel< ElementPrintType > PrintType;

  /** Type for real-valued scalar operations. */
  typedef RGBPixel< ElementRealType > RealType;

  /** Type for real-valued scalar operations. */
  typedef ElementRealType ScalarRealType;

  /** Measurement vector type */
  typedef Self MeasurementVectorType;

  /** Component wise defined element
   *
   * \note minimum value for floating pointer types is defined as
   * minimum positive normalize value.
   */
  static const Self max(const Self &)
  {
    return Self( NumericTraits< T >::max() );
  }

  static const Self min(const Self &)
  {
    return Self( NumericTraits< T >::min() );
  }

  static const Self max()
  {
    return Self( NumericTraits< T >::max() );
  }

  static const Self min()
  {
    return Self( NumericTraits< T >::min() );
  }

  static const Self NonpositiveMin()
  {
    return Self( NumericTraits< ValueType >::NonpositiveMin() );
  }

  static const Self ZeroValue()
  {
    return Self(NumericTraits< T >::ZeroValue());
  }

  static const Self OneValue()
  {
    return Self(NumericTraits< T >::OneValue());
  }

  static const Self NonpositiveMin(const Self &)
  {
    return NonpositiveMin();
  }

  static const Self ZeroValue(const Self &)
  {
    return ZeroValue();
  }

  static const Self OneValue(const Self &)
  {
    return OneValue();
  }

  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = NumericTraits< ValueType >::IsInteger;
  static ITK_CONSTEXPR_VAR bool IsComplex = NumericTraits< ValueType >::IsComplex;

  /** RGB pixels must have 3 components, so the size cannot be
   *  set to anything besides 3.  If called with size of 3, this
   *  function will fill the pixel with zeros. */
  static void SetLength(RGBPixel< T > & m, const unsigned int s)
  {
    if ( s != 3 )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a RGBPixel to anything other "
                               "than 3.");
      }
    m.Fill(NumericTraits< T >::ZeroValue());
  }

  /** Return the dimensionality of the pixel. Always returns 3. */
  static unsigned int GetLength(const RGBPixel< T > &)
  {
    return 3;
  }

  /** Return the dimensionality of the pixel. Always returns 3. */
  static unsigned int GetLength()
  {
    return 3;
  }

  static void AssignToArray( const Self & v, MeasurementVectorType & mv )
  {
    mv = v;
  }

  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    for( unsigned int i=0; i<3; i++ )
      {
      mv[i] = v[i];
      }
  }

  /** \note: the functions are preferred over the member variables as
   * they are defined for all partial specialization
   */
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};
} // end namespace itk

#endif // itkNumericTraitsRGBPixel_h
