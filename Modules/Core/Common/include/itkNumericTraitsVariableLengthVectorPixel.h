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
#ifndef itkNumericTraitsVariableLengthVectorPixel_h
#define itkNumericTraitsVariableLengthVectorPixel_h

#include "itkVariableLengthVector.h"

// This work is part of the National Alliance for Medical Image Computing
// (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
// for Medical Research, Grant U54 EB005149.

namespace itk
{
/**
 * \brief Define numeric traits for VariableLengthVector.
 * \tparam T Component type of VariableLenghtVector
 *
 * We provide here a generic implementation based on creating types of
 * VariableLengthVector whose components are the types of the NumericTraits from
 * the original VariableLengthVector components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<VariableLengthVector< T > >  is defined piecewise by
 *   VariableLengthVector< NumericTraits< T > >
 *
 * \note The Zero(), One(), min() and max() methods here take
 * references to a pixel as input.  This is due to the fact that the
 * length of the VariableLengthVector is not known until
 * run-time. Since the most common use of Zero and One is for
 * comparison purposes or initialization of sums etc, this might just
 * as easily be re-written with a pixel passed in as a reference and
 * the length is inferred from this pixel.
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename T >
class NumericTraits< VariableLengthVector< T > >
{
public:

  typedef typename NumericTraits< T >::AbsType        ElementAbsType;
  typedef typename NumericTraits< T >::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits< T >::FloatType      ElementFloatType;
  typedef typename NumericTraits< T >::PrintType      ElementPrintType;
  typedef typename NumericTraits< T >::RealType       ElementRealType;

  /** Return the type of the native component type. */
  typedef T ValueType;

  typedef VariableLengthVector< T > Self;

  /** Unsigned component type */
  typedef VariableLengthVector< ElementAbsType > AbsType;

  /** Accumulation of addition and multiplication. */
  typedef VariableLengthVector< ElementAccumulateType > AccumulateType;

  /** Typedef for operations that use floating point instead of real precision
    */
  typedef VariableLengthVector< ElementFloatType > FloatType;

  /** Return the type that can be printed. */
  typedef VariableLengthVector< ElementPrintType > PrintType;

  /** Type for real-valued scalar operations. */
  typedef VariableLengthVector< ElementRealType > RealType;

  /** Type for real-valued scalar operations. */
  typedef ElementRealType ScalarRealType;

  /** Measurement vector type */
  typedef Self MeasurementVectorType;

  /** Component wise defined element
   *
   * \note minimum value for floating pointer types is defined as
   * minimum positive normalize value.
   */
  static const Self max(const Self & a)
  {
    Self b( a.Size() );

    b.Fill( NumericTraits< T >::max() );
    return b;
  }

  static const Self min(const Self & a)
  {
    Self b( a.Size() );

    b.Fill( NumericTraits< T >::min() );
    return b;
  }

  static const Self ZeroValue(const Self  & a)
  {
    Self b( a.Size() );

    b.Fill(NumericTraits< T >::ZeroValue());
    return b;
  }

  static const Self OneValue(const Self & a)
  {
    Self b( a.Size() );

    b.Fill(NumericTraits< T >::OneValue());
    return b;
  }

  static const Self NonpositiveMin(const Self & a)
  {
    Self b( a.Size() );
    b.Fill( NumericTraits< T >::NonpositiveMin() );
    return b;
  }

  static bool IsPositive( const Self & a)
  {
    bool flag = false;
    for (unsigned int i=0; i < GetLength( a ); i++)
      {
      if ( a[i] > NumericTraits< ValueType >::ZeroValue() )
        {
        flag = true;
        }
      }
    return flag;
  }

  static bool IsNonpositive( const Self & a)
  {
    bool flag = false;
    for (unsigned int i=0; i < GetLength( a ); i++)
      {
      if ( ! (a[i] > 0.0 ) )
        {
        flag = true;
        }
      }
    return flag;
  }

  static bool IsNegative( const Self & a)
  {
    bool flag = false;
    for (unsigned int i=0; i < GetLength( a ); i++)
      {
      if ( a[i] < NumericTraits< ValueType >::ZeroValue() )
        {
        flag = true;
        }
      }
    return flag;
  }

  static bool IsNonnegative( const Self & a)
  {
    bool flag = false;
    for (unsigned int i=0; i < GetLength( a ); i++)
      {
      if ( ! (a[i] < 0.0 ))
        {
        flag = true;
        }
      }
    return flag;
  }

  static ITK_CONSTEXPR_VAR bool IsSigned = NumericTraits< ValueType >::IsSigned;
  static ITK_CONSTEXPR_VAR bool IsInteger = NumericTraits< ValueType >::IsInteger;
  static ITK_CONSTEXPR_VAR bool IsComplex = NumericTraits< ValueType >::IsComplex;


  /** Resize the input vector to the specified size. */
  static void SetLength(VariableLengthVector< T > & m, const unsigned int s)
  {
    m.SetSize(s);
    m.Fill(NumericTraits< T >::ZeroValue());
  }

  /** Return the size of the vector. */
  static unsigned int GetLength(const VariableLengthVector< T > & m)
  {
    return m.GetSize();
  }

  static void AssignToArray( const Self & v, MeasurementVectorType & mv )
  {
    mv = v;
  }

  template<typename TArray>
  static void AssignToArray( const Self & v, TArray & mv )
  {
    for( unsigned int i=0; i<GetLength(v); i++ )
      {
      mv[i] = v[i];
      }
  }

#if !defined(ITK_LEGACY_REMOVE)
  //The extra #ifdef is need because the itkLegacyMacro
  //can not format a static const member function properly
  //in the case of removing the code.

  /** \deprecated use ZeroValue() instead */
  itkLegacyMacro(static const Self Zero(const Self  & a))
  {
    Self b( a.Size() );

    b.Fill(NumericTraits< T >::ZeroValue());
    return b;
  }
  /** \deprecated use OneValue() instead */
  itkLegacyMacro(static const Self One(const Self & a))
  {
    Self b( a.Size() );

    b.Fill(NumericTraits< T >::OneValue());
    return b;
  }
#endif

};
} // end namespace itk

#endif // itkNumericTraitsVariableLengthVector_h
