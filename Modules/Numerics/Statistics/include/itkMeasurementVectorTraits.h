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
#ifndef __itkMeasurementVectorTraits_h
#define __itkMeasurementVectorTraits_h

#include "itkVariableLengthVector.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkRGBPixel.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"
#include "itkNumericTraits.h"
#include "itkNumericTraitsStdVector.h"
#include "itkSize.h"
#include <vector>

namespace itk
{
namespace Statistics
{
/** \class MeasurementVectorTraits
 * \brief
 * \ingroup Statistics
 * \ingroup ITKStatistics
 */

class MeasurementVectorTraits
{
public:

  /** In the old framework, the FrequencyType is set to float. The problem is for
      large histograms the total frequency can be more than 1e+7, than increasing
      the frequency by one does not change the total frequency (because of lack of
      precision). Using double type will also ultimately fall into the same problem.
      Hence in the new statistics framework, InstanceIdentifier/FrequencyTypes are
      set to the the largest possible integer on the machine */
  typedef IdentifierType  InstanceIdentifier;

  /** Type defined for representing the frequency of measurement vectors */
  typedef InstanceIdentifier                                     AbsoluteFrequencyType;
  typedef NumericTraits< AbsoluteFrequencyType >::RealType       RelativeFrequencyType;
  typedef NumericTraits< AbsoluteFrequencyType >::AccumulateType TotalAbsoluteFrequencyType;
  typedef NumericTraits< RelativeFrequencyType >::AccumulateType TotalRelativeFrequencyType;

  typedef unsigned int MeasurementVectorLength;

  template< class TVectorType >
  static bool IsResizable(const TVectorType &)
  {
    // Test whether the vector type is resizable or not
    //
    // If the default constructor creates a vector of
    // length zero, we assume that it is resizable,
    // otherwise that is a pretty useless measurement vector.
    TVectorType             m;
    MeasurementVectorLength len = NumericTraits<TVectorType>::GetLength(m);

    return ( len == 0 );
  }

  template< class TValueType1, unsigned int VLength, class TValueType2, unsigned int VLength2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > &,
                                        const FixedArray< TValueType2, VLength2 > &,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( VLength != VLength2 )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2, unsigned int VLength2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > *,
                                        const FixedArray< TValueType2, VLength2 > *,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( VLength != VLength2 )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const Array< TValueType1 > & a,
                                        const Array< TValueType2 > & b, const char *errMsg = "Length Mismatch")
  {
    if ( b.Size() != a.Size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const Array< TValueType1 > *a,
                                        const Array< TValueType2 > *b, const char *errMsg = "Length Mismatch")
  {
    if ( b->Size() != a->Size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const VariableLengthVector< TValueType1 > & a,
                                        const VariableLengthVector< TValueType2 > & b,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( b.Size() != a.Size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const VariableLengthVector< TValueType1 > *a,
                                        const VariableLengthVector< TValueType2 > *b,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( b->Size() != a->Size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const std::vector< TValueType1 > & a,
                                        const std::vector< TValueType2 > & b, const char *errMsg = "Length Mismatch")
  {
    if ( b.size() != a.size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, class TValueType2 >
  static MeasurementVectorLength Assert(const std::vector< TValueType1 > *a,
                                        const std::vector< TValueType2 > *b, const char *errMsg = "Length Mismatch")
  {
    if ( b->size() != a->size() )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > &,
                                        const Array< TValueType2 > & b, const char *errMsg = "Length Mismatch")
  {
    if ( b.Size() == 0 )
      {
      return VLength;
      }
    if ( b.Size() != 0 )
      {
      if ( b.Size() != VLength )
        {
        itkGenericExceptionMacro(<< errMsg);
        }
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > *,
                                        const Array< TValueType2 > *b, const char *errMsg = "Length Mismatch")
  {
    if ( b->Size() == 0 )
      {
      return VLength;
      }
    else if ( b->Size() != VLength )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > &,
                                        const VariableLengthVector< TValueType2 > & b,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( b.Size() == 0 )
      {
      return VLength;
      }
    if ( b.Size() != 0 )
      {
      if ( b.Size() != VLength )
        {
        itkGenericExceptionMacro(<< errMsg);
        }
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > *,
                                        const VariableLengthVector< TValueType2 > *b,
                                        const char *errMsg = "Length Mismatch")
  {
    if ( b->Size() == 0 )
      {
      return VLength;
      }
    else if ( b->Size() != VLength )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > &,
                                        const std::vector< TValueType2 > & b, const char *errMsg = "Length Mismatch")
  {
    if ( b.size() == 0 )
      {
      return VLength;
      }
    if ( b.size() != 0 )
      {
      if ( b.size() != VLength )
        {
        itkGenericExceptionMacro(<< errMsg);
        }
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength, class TValueType2 >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > *,
                                        const std::vector< TValueType2 > *b, const char *errMsg = "Length Mismatch")
  {
    if ( b->size() == 0 )
      {
      return VLength;
      }
    else if ( b->size() != VLength )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > &,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( l == 0 )
      {
      return VLength;
      }
    else if ( l != VLength )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType1, unsigned int VLength >
  static MeasurementVectorLength Assert(const FixedArray< TValueType1, VLength > *,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( l == 0 )
      {
      return VLength;
      }
    else if ( l != VLength )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const Array< TValueType > & a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a.Size() != l ) ) || ( a.Size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a.Size();
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const Array< TValueType > *a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a->Size() != l ) ) || ( a->Size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a->Size();
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const VariableLengthVector< TValueType > & a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a.Size() != l ) ) || ( a.Size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a.Size();
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const VariableLengthVector< TValueType > *a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a->Size() != l ) ) || ( a->Size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a->Size();
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const std::vector< TValueType > & a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a.size() != l ) ) || ( a.size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a.size();
      }
    return 0;
  }

  template< class TValueType >
  static MeasurementVectorLength Assert(const std::vector< TValueType > *a,
                                        const MeasurementVectorLength l, const char *errMsg = "Length Mismatch")
  {
    if ( ( ( l != 0 ) && ( a->size() != l ) ) || ( a->size() == 0 ) )
      {
      itkGenericExceptionMacro(<< errMsg);
      }
    else if ( l == 0 )
      {
      return a->size();
      }
    return 0;
  }

  template< class TArrayType >
  static void  Assign(TArrayType & m, const TArrayType & v)
  {
    m = v;
  }

  template< class TValueType, unsigned int VLength >
  static void  Assign(FixedArray< TValueType, VLength > & m, const TValueType & v)
  {
    m[0] = v;
  }
};

/** \class MeasurementVectorTraitsTypes
 * \brief
 * \ingroup Statistics
 * \ingroup ITKStatistics
 */

template< class TMeasurementVector >
class MeasurementVectorTraitsTypes
{
public:
  typedef typename TMeasurementVector::ValueType ValueType;
};

template< class T >
class MeasurementVectorTraitsTypes< std::vector< T > >
{
public:
  typedef T ValueType;
};

/** Traits for generating the MeasurementVectorType that best matches a
 * particular pixel type. */

template< class TPixelType >
class MeasurementVectorPixelTraits
{
public:
  /* type of the vector that matches this pixel type */
  typedef TPixelType MeasurementVectorType;
};

/**
 *  \cond HIDE_SPECIALIZATION_DOCUMENTATION
 *  \class MeasurementVectorPixelTraits
 * \ingroup ITKStatistics
 */
template< >
class MeasurementVectorPixelTraits< char >
{
public:
  typedef FixedArray< char, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< uint8_t >
{
public:
  typedef FixedArray< uint8_t, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< int8_t >
{
public:
  typedef FixedArray< int8_t, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< uint16_t >
{
public:
  typedef FixedArray< uint16_t, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< int16_t >
{
public:
  typedef FixedArray< int16_t, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< unsigned int >
{
public:
  typedef FixedArray< unsigned int, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< signed int >
{
public:
  typedef FixedArray< signed int, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< unsigned long >
{
public:
  typedef FixedArray< unsigned long, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< signed long >
{
public:
  typedef FixedArray< signed long, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< float >
{
public:
  typedef FixedArray< float, 1 > MeasurementVectorType;
};

template< >
class MeasurementVectorPixelTraits< double >
{
public:
  typedef FixedArray< double, 1 > MeasurementVectorType;
};

/** \endcond*/

} // namespace Statistics
} // namespace itk

#endif  // __itkMeasurementVectorTraits_h
