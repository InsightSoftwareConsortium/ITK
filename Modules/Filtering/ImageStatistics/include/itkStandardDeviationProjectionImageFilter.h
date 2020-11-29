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
#ifndef itkStandardDeviationProjectionImageFilter_h
#define itkStandardDeviationProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class StandardDeviationProjectionImageFilter
 * \brief Mean projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 * https://www.insight-journal.org/browse/publication/71
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel, typename TAccumulate>
class StandardDeviationAccumulator
{
public:
  using RealType = typename NumericTraits<TInputPixel>::RealType;

  StandardDeviationAccumulator(SizeValueType size)
  {
    m_Size = size;
    m_Values.reserve(size);
  }

  ~StandardDeviationAccumulator() = default;

  inline void
  Initialize()
  {
    m_Sum = NumericTraits<TAccumulate>::ZeroValue();
    m_Values.clear();
  }

  inline void
  operator()(const TInputPixel & input)
  {
    m_Sum = m_Sum + input;
    m_Values.push_back(input);
  }

  inline RealType
  GetValue()
  {
    // to avoid division by zero
    if (m_Size <= 1)
    {
      return NumericTraits<RealType>::ZeroValue();
    }

    typename NumericTraits<TInputPixel>::RealType mean = ((RealType)m_Sum) / m_Size;
    typename std::vector<TInputPixel>::iterator   it;
    RealType                                      squaredSum = NumericTraits<RealType>::ZeroValue();
    for (it = m_Values.begin(); it != m_Values.end(); it++)
    {
      squaredSum += itk::Math::sqr(*it - mean);
    }
    return std::sqrt(squaredSum / (m_Size - 1));
  }

  TAccumulate              m_Sum;
  SizeValueType            m_Size;
  std::vector<TInputPixel> m_Values;
};
} // namespace Functor

template <typename TInputImage,
          typename TOutputImage,
          typename TAccumulate = typename NumericTraits<typename TOutputImage::PixelType>::AccumulateType>
class StandardDeviationProjectionImageFilter
  : public ProjectionImageFilter<TInputImage,
                                 TOutputImage,
                                 Functor::StandardDeviationAccumulator<typename TInputImage::PixelType, TAccumulate>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StandardDeviationProjectionImageFilter);

  using Self = StandardDeviationProjectionImageFilter;

  using Superclass =
    ProjectionImageFilter<TInputImage,
                          TOutputImage,
                          Functor::StandardDeviationAccumulator<typename TInputImage::PixelType, TAccumulate>>;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(StandardDeviationProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelToOutputPixelTypeGreaterAdditiveOperatorCheck,
                  (Concept::AdditiveOperators<TAccumulate, InputPixelType, TAccumulate>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

  itkConceptMacro(AccumulateHasNumericTraitsCheck, (Concept::HasNumericTraits<TAccumulate>));

  // End concept checking
#endif

protected:
  StandardDeviationProjectionImageFilter() = default;
  ~StandardDeviationProjectionImageFilter() override = default;
}; // end StandardDeviationProjectionImageFilter
} // end namespace itk

#endif
