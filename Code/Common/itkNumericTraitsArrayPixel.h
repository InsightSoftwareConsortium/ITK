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
#ifndef __itkNumericTraitsArrayPixel_h
#define __itkNumericTraitsArrayPixel_h

#include "itkNumericTraits.h"
#include "itkArray.h"

namespace itk
{
template< typename T >
class NumericTraits< Array< T > >
{
private:

  typedef  typename NumericTraits< T >::AbsType        ElementAbsType;
  typedef  typename NumericTraits< T >::AccumulateType ElementAccumulateType;
  typedef  typename NumericTraits< T >::FloatType      ElementFloatType;
  typedef  typename NumericTraits< T >::PrintType      ElementPrintType;
  typedef  typename NumericTraits< T >::RealType       ElementRealType;
public:

  /** Return the type of the native component type. */
  typedef T              ValueType;
  typedef Array< T >     Self;

  /** Unsigned component type */
  typedef Array< ElementAbsType > AbsType;

  /** Accumulation of addition and multiplication. */
  typedef Array< ElementAccumulateType > AccumulateType;

  /** Typedef for operations that use floating point instead of real precision
    */
  typedef Array< ElementFloatType > FloatType;

  /** Return the type that can be printed. */
  typedef Array< ElementPrintType > PrintType;

  /** Type for real-valued scalar operations. */
  typedef Array< ElementRealType > RealType;

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

  /** Set the length of the input array and fill it with zeros. */
  static void SetLength(Array< T > & m, const unsigned int s)
  {
    m.SetSize(s);
    m.Fill(NumericTraits< T >::Zero);
  }

  /** Get the length of the input array. */
  static unsigned int GetLength(const Array< T > & m)
  {
    return m.GetSize();
  }

  /** \note: the functions are prefered over the member variables as
   * they are defined for all partial specialization
   */
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};

// a macro to define and initialize static member variables with no dimension
#define itkStaticNumericTraitsGenericArrayNoDimensionMacro(GENERIC_ARRAY,T) \
  template< > \
  const GENERIC_ARRAY< T > NumericTraits< GENERIC_ARRAY< T > >::Zero = GENERIC_ARRAY< T >( \
  NumericTraits< T >::Zero); \
  template< > \
  const GENERIC_ARRAY< T > NumericTraits< GENERIC_ARRAY< T > >::One = GENERIC_ARRAY< T >( \
  NumericTraits< T >::One); \

} // end namespace itk

#endif // __itkNumericTraitsArrayPixel_h
