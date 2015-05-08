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
#ifndef itkNumericTraitsFixedArrayPixel_h
#define itkNumericTraitsFixedArrayPixel_h

#include "itkNumericTraits.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 * \brief Define numeric traits for FixedArray.
 * \tparam T Component type of the FixedArray
 * \tparam D Dimension of the FixedArray
 *
 * We provide here a generic implementation based on creating types of
 * FixedArray whose components are the types of the NumericTraits from
 * the original FixedArray components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<FixedArray< T, D > >  is defined piecewise by
 *   FixedArray< NumericTraits< T > >
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename T, unsigned int D >
class NumericTraits< FixedArray< T, D > >
{
private:

  typedef  typename NumericTraits< T >::AbsType        ElementAbsType;
  typedef  typename NumericTraits< T >::AccumulateType ElementAccumulateType;
  typedef  typename NumericTraits< T >::FloatType      ElementFloatType;
  typedef  typename NumericTraits< T >::PrintType      ElementPrintType;
  typedef  typename NumericTraits< T >::RealType       ElementRealType;

public:

  /** Return the type of the native component type. */
  typedef T ValueType;

  typedef FixedArray< T, D > Self;

  /** Unsigned component type */
  typedef FixedArray< ElementAbsType, D > AbsType;

  /** Accumulation of addition and multiplication. */
  typedef FixedArray< ElementAccumulateType, D > AccumulateType;

  /** Typedef for operations that use floating point instead of real precision
    */
  typedef FixedArray< ElementFloatType, D > FloatType;

  /** Return the type that can be printed. */
  typedef FixedArray< ElementPrintType, D > PrintType;

  /** Type for real-valued scalar operations. */
  typedef FixedArray< ElementRealType, D > RealType;

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
    return Self( NumericTraits< T >::NonpositiveMin() );
  }

  static const Self ZeroValue()
  {
    return Self( NumericTraits< T >::ZeroValue() );
  }

  static const Self OneValue()
  {
    return Self( NumericTraits< T >::OneValue() );
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

  /** Fixed length vectors cannot be resized, so an exception will
   *  be thrown if the input size is not valid.  If the size is valid
   *  the vector will be filled with zeros. */
  static void SetLength(FixedArray< T, D > & m, const unsigned int s)
  {
    if ( s != D )
      {
      itkGenericExceptionMacro(<< "Cannot set the size of a FixedArray of length "
                               << D << " to " << s);
      }
    m.Fill(NumericTraits< T >::ZeroValue());
  }

  /** Return the length of the array. */
  static unsigned int GetLength(const FixedArray< T, D > &)
  {
    return D;
  }

  /** Return the length of the array. */
  static unsigned int GetLength()
  {
    return D;
  }

  static void AssignToArray( const Self & v, MeasurementVectorType & mv )
  {
    mv = v;
  }

  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    for( unsigned int i=0; i<D; i++ )
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

// a macro to define and initialize static member variables
#define itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, D)                                 \
  template< >                                                                                        \
  ITKCommon_EXPORT const GENERIC_ARRAY< T, D >  NumericTraits< GENERIC_ARRAY< T, D > >::Zero = GENERIC_ARRAY< T, D >( \
    NumericTraits< T >::Zero);                                                                       \
  template< >                                                                                        \
  ITKCommon_EXPORT const GENERIC_ARRAY< T, D >  NumericTraits< GENERIC_ARRAY< T, D > >::One = GENERIC_ARRAY< T, D >( \
    NumericTraits< T >::One);

//
// List here the array dimension specializations of these static
// Traits:
//
#define itkStaticNumericTraitsGenericArrayDimensionsMacro(GENERIC_ARRAY, T) \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 1);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 2);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 3);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 4);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 5);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 6);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 7);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 8);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 9);             \
  itkStaticNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, 10);
} // end namespace itk

#endif // itkNumericTraitsFixedArrayPixel_h
