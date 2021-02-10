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
#ifndef itkMeanProjectionImageFilter_h
#define itkMeanProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class MeanProjectionImageFilter
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
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel, typename TAccumulate>
class MeanAccumulator
{
public:
  using RealType = typename NumericTraits<TInputPixel>::RealType;

  MeanAccumulator(SizeValueType size) { m_Size = size; }

  ~MeanAccumulator() { m_Size = NumericTraits<SizeValueType>::ZeroValue(); }

  inline void
  Initialize()
  {
    m_Sum = NumericTraits<TAccumulate>::ZeroValue();
  }

  inline void
  operator()(const TInputPixel & input)
  {
    m_Sum = m_Sum + input;
  }

  inline RealType
  GetValue()
  {
    return ((RealType)m_Sum) / m_Size;
  }

  TAccumulate   m_Sum;
  SizeValueType m_Size;
};
} // namespace Functor

template <typename TInputImage,
          typename TOutputImage,
          typename TAccumulate = typename NumericTraits<typename TOutputImage::PixelType>::AccumulateType>
class MeanProjectionImageFilter
  : public ProjectionImageFilter<TInputImage,
                                 TOutputImage,
                                 Functor::MeanAccumulator<typename TInputImage::PixelType, TAccumulate>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeanProjectionImageFilter);

  using Self = MeanProjectionImageFilter;
  using Superclass = ProjectionImageFilter<TInputImage,
                                           TOutputImage,
                                           Functor::MeanAccumulator<typename TInputImage::PixelType, TAccumulate>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Runtime information support. */
  itkTypeMacro(MeanProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelToOutputPixelTypeGreaterAdditiveOperatorCheck,
                  (Concept::AdditiveOperators<OutputPixelType, InputPixelType, OutputPixelType>));

  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  MeanProjectionImageFilter() = default;
  ~MeanProjectionImageFilter() override = default;
}; // end MeanProjectionImageFilter
} // end namespace itk

#endif
