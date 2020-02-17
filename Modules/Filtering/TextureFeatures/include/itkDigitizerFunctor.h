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
#ifndef itkDigitizerFunctor_h
#define itkDigitizerFunctor_h

#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{

template <typename TInput1, typename TInput2 = TInput1, typename TOutput = TInput1>
class Digitizer
{
public:
  using PixelType = TInput2;
  using MaskPixelType = TInput1;

  Digitizer()
    : m_MaskValue(1)
    , m_Min(NumericTraits<PixelType>::min())
    , m_Max(NumericTraits<PixelType>::max())
  {}

  Digitizer(unsigned int numberOfBinsPerAxis, MaskPixelType maskValue, PixelType min, PixelType max)
    : m_NumberOfBinsPerAxis(numberOfBinsPerAxis)
    , m_MaskValue(maskValue)
    , m_Min(min)
    , m_Max(max)
  {}

  ~Digitizer() = default;

  bool
  operator!=(const Digitizer & other) const
  {
    return (m_NumberOfBinsPerAxis != other.m_NumberOfBinsPerAxis) || (m_MaskValue != other.m_MaskValue) ||
           (m_Min != other.m_Min) || (m_Max != other.m_Max);
  }

  bool
  operator==(const Digitizer & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const MaskPixelType & maskPixel, const PixelType & inputPixel) const
  {

    if (maskPixel != m_MaskValue)
    {
      return -10;
    }
    else if (inputPixel < this->m_Min || inputPixel >= m_Max)
    {
      return -1;
    }
    else
    {
      return Math::Floor<TOutput>((inputPixel - m_Min) / ((m_Max - m_Min) / (float)m_NumberOfBinsPerAxis));
    }
  }

  unsigned int m_NumberOfBinsPerAxis{ 256 };

  MaskPixelType m_MaskValue;

  typename NumericTraits<PixelType>::RealType m_Min;
  typename NumericTraits<PixelType>::RealType m_Max;
};

} // namespace Statistics
} // namespace itk

#endif
