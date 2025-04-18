/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSumProjectionImageFilter_h
#define itkSumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class SumProjectionImageFilter
 * \brief Sum projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann
 * \cite Lehmann_2006_a.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel, typename TOutputPixel>
class SumAccumulator
{
public:
  SumAccumulator(SizeValueType) {}
  ~SumAccumulator() = default;

  inline void
  Initialize()
  {
    m_Sum = TOutputPixel{};
  }

  inline void
  operator()(const TInputPixel & input)
  {
    m_Sum = m_Sum + input;
  }

  inline TOutputPixel
  GetValue()
  {
    return m_Sum;
  }

  TOutputPixel m_Sum;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class SumProjectionImageFilter
  : public ProjectionImageFilter<
      TInputImage,
      TOutputImage,
      Functor::SumAccumulator<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SumProjectionImageFilter);

  using Self = SumProjectionImageFilter;
  using Superclass =
    ProjectionImageFilter<TInputImage,
                          TOutputImage,
                          Functor::SumAccumulator<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(SumProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkConceptMacro(InputPixelToOutputPixelTypeGreaterAdditiveOperatorCheck,
                  (Concept::AdditiveOperators<OutputPixelType, InputPixelType, OutputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

protected:
  SumProjectionImageFilter() = default;
  ~SumProjectionImageFilter() override = default;
}; // end SumProjectionImageFilter
} // end namespace itk

#endif
