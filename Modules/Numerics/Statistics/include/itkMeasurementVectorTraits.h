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
#ifndef itkMeasurementVectorTraits_h
#define itkMeasurementVectorTraits_h

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
  using InstanceIdentifier = IdentifierType;

  /** Type defined for representing the frequency of measurement vectors */
  using AbsoluteFrequencyType = InstanceIdentifier;
  using RelativeFrequencyType = NumericTraits<AbsoluteFrequencyType>::RealType;
  using TotalAbsoluteFrequencyType = NumericTraits<AbsoluteFrequencyType>::AccumulateType;
  using TotalRelativeFrequencyType = NumericTraits<RelativeFrequencyType>::AccumulateType;

  using MeasurementVectorLength = std::size_t;

  template <typename TVectorType>
  static bool
  IsResizable(const TVectorType &)
  {
    // Test whether the vector type is resizable or not
    //
    // If the default constructor creates a vector of
    // length zero, we assume that it is resizable,
    // otherwise that is a pretty useless measurement vector.
    TVectorType             m;
    MeasurementVectorLength len = NumericTraits<TVectorType>::GetLength(m);

    return (len == 0);
  }

  template <typename TValue1, unsigned int VLength, typename TValue2, unsigned int VLength2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> &,
         const FixedArray<TValue2, VLength2> &,
         const char * errMsg = "Length Mismatch")
  {
    if (VLength != VLength2)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2, unsigned int VLength2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> *,
         const FixedArray<TValue2, VLength2> *,
         const char * errMsg = "Length Mismatch")
  {
    if (VLength != VLength2)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const Array<TValue1> & a, const Array<TValue2> & b, const char * errMsg = "Length Mismatch")
  {
    if (b.Size() != a.Size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const Array<TValue1> * a, const Array<TValue2> * b, const char * errMsg = "Length Mismatch")
  {
    if (b->Size() != a->Size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const VariableLengthVector<TValue1> & a,
         const VariableLengthVector<TValue2> & b,
         const char *                          errMsg = "Length Mismatch")
  {
    if (b.Size() != a.Size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const VariableLengthVector<TValue1> * a,
         const VariableLengthVector<TValue2> * b,
         const char *                          errMsg = "Length Mismatch")
  {
    if (b->Size() != a->Size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const std::vector<TValue1> & a, const std::vector<TValue2> & b, const char * errMsg = "Length Mismatch")
  {
    if (b.size() != a.size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, typename TValue2>
  static MeasurementVectorLength
  Assert(const std::vector<TValue1> * a, const std::vector<TValue2> * b, const char * errMsg = "Length Mismatch")
  {
    if (b->size() != a->size())
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> &, const Array<TValue2> & b, const char * errMsg = "Length Mismatch")
  {
    if (b.Size() == 0)
    {
      return VLength;
    }
    if (b.Size() != 0)
    {
      if (b.Size() != VLength)
      {
        itkGenericExceptionMacro(<< errMsg);
      }
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> *, const Array<TValue2> * b, const char * errMsg = "Length Mismatch")
  {
    if (b->Size() == 0)
    {
      return VLength;
    }
    else if (b->Size() != VLength)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> &,
         const VariableLengthVector<TValue2> & b,
         const char *                          errMsg = "Length Mismatch")
  {
    if (b.Size() == 0)
    {
      return VLength;
    }
    if (b.Size() != 0)
    {
      if (b.Size() != VLength)
      {
        itkGenericExceptionMacro(<< errMsg);
      }
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> *,
         const VariableLengthVector<TValue2> * b,
         const char *                          errMsg = "Length Mismatch")
  {
    if (b->Size() == 0)
    {
      return VLength;
    }
    else if (b->Size() != VLength)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> &, const std::vector<TValue2> & b, const char * errMsg = "Length Mismatch")
  {
    if (b.empty())
    {
      return VLength;
    }
    if (!b.empty())
    {
      if (b.size() != VLength)
      {
        itkGenericExceptionMacro(<< errMsg);
      }
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength, typename TValue2>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> *, const std::vector<TValue2> * b, const char * errMsg = "Length Mismatch")
  {
    if (b->size() == 0)
    {
      return VLength;
    }
    else if (b->size() != VLength)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> &, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (l == 0)
    {
      return VLength;
    }
    else if (l != VLength)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue1, unsigned int VLength>
  static MeasurementVectorLength
  Assert(const FixedArray<TValue1, VLength> *, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (l == 0)
    {
      return VLength;
    }
    else if (l != VLength)
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const Array<TValue> & a, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a.Size() != l)) || (a.Size() == 0))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return a.Size();
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const Array<TValue> * a, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a->Size() != l)) || (a->Size() == 0))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return a->Size();
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const VariableLengthVector<TValue> & a,
         const MeasurementVectorLength        l,
         const char *                         errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a.Size() != l)) || (a.Size() == 0))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return a.Size();
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const VariableLengthVector<TValue> * a,
         const MeasurementVectorLength        l,
         const char *                         errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a->Size() != l)) || (a->Size() == 0))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return a->Size();
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const std::vector<TValue> & a, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a.size() != l)) || (a.empty()))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return static_cast<MeasurementVectorLength>(a.size());
    }
    return 0;
  }

  template <typename TValue>
  static MeasurementVectorLength
  Assert(const std::vector<TValue> * a, const MeasurementVectorLength l, const char * errMsg = "Length Mismatch")
  {
    if (((l != 0) && (a->size() != l)) || (a->size() == 0))
    {
      itkGenericExceptionMacro(<< errMsg);
    }
    else if (l == 0)
    {
      return a->size();
    }
    return 0;
  }

  template <typename TArrayType>
  static void
  Assign(TArrayType & m, const TArrayType & v)
  {
    m = v;
  }

  template <typename TValue, unsigned int VLength>
  static void
  Assign(FixedArray<TValue, VLength> & m, const TValue & v)
  {
    m[0] = v;
  }
};

/** \class MeasurementVectorTraitsTypes
 * \brief
 * \ingroup Statistics
 * \ingroup ITKStatistics
 */

template <typename TMeasurementVector>
class MeasurementVectorTraitsTypes
{
public:
  using ValueType = typename TMeasurementVector::ValueType;
};

template <typename T>
class MeasurementVectorTraitsTypes<std::vector<T>>
{
public:
  using ValueType = T;
};

/** Traits for generating the MeasurementVectorType that best matches a
 * particular pixel type. */

template <typename TPixelType>
class MeasurementVectorPixelTraits
{
public:
  /* type of the vector that matches this pixel type */
  using MeasurementVectorType = TPixelType;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION
/**
 * \class MeasurementVectorPixelTraits
 * \ingroup ITKStatistics
 */
template <>
class MeasurementVectorPixelTraits<char>
{
public:
  using MeasurementVectorType = FixedArray<char, 1>;
};

template <>
class MeasurementVectorPixelTraits<unsigned char>
{
public:
  using MeasurementVectorType = FixedArray<unsigned char, 1>;
};

template <>
class MeasurementVectorPixelTraits<signed char>
{
public:
  using MeasurementVectorType = FixedArray<signed char, 1>;
};

template <>
class MeasurementVectorPixelTraits<unsigned short>
{
public:
  using MeasurementVectorType = FixedArray<unsigned short, 1>;
};

template <>
class MeasurementVectorPixelTraits<signed short>
{
public:
  using MeasurementVectorType = FixedArray<signed short, 1>;
};

template <>
class MeasurementVectorPixelTraits<unsigned int>
{
public:
  using MeasurementVectorType = FixedArray<unsigned int, 1>;
};

template <>
class MeasurementVectorPixelTraits<signed int>
{
public:
  using MeasurementVectorType = FixedArray<signed int, 1>;
};

template <>
class MeasurementVectorPixelTraits<unsigned long>
{
public:
  using MeasurementVectorType = FixedArray<unsigned long, 1>;
};

template <>
class MeasurementVectorPixelTraits<signed long>
{
public:
  using MeasurementVectorType = FixedArray<signed long, 1>;
};

template <>
class MeasurementVectorPixelTraits<unsigned long long>
{
public:
  using MeasurementVectorType = FixedArray<unsigned long long, 1>;
};

template <>
class MeasurementVectorPixelTraits<signed long long>
{
public:
  using MeasurementVectorType = FixedArray<signed long long, 1>;
};

template <>
class MeasurementVectorPixelTraits<float>
{
public:
  using MeasurementVectorType = FixedArray<float, 1>;
};

template <>
class MeasurementVectorPixelTraits<double>
{
public:
  using MeasurementVectorType = FixedArray<double, 1>;
};

/// \endcond

} // namespace Statistics
} // namespace itk

#endif // itkMeasurementVectorTraits_h
