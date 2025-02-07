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
#ifndef itkMinimumProjectionImageFilter_h
#define itkMinimumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class MinimumProjectionImageFilter
 * \brief Minimum projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 * https://doi.org/10.54294/0pjyho
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel>
class MinimumAccumulator
{
public:
  MinimumAccumulator(SizeValueType) {}
  ~MinimumAccumulator() = default;

  inline void
  Initialize()
  {
    if constexpr (std::is_same<TInputPixel, typename NumericTraits<TInputPixel>::ValueType>::value)
    {
      m_Minimum = NumericTraits<TInputPixel>::max();
    }
    else
    {
      m_Minimum = TInputPixel();
      m_Minimum.Fill(NumericTraits<typename TInputPixel::ValueType>::max());
    }
  }

  inline void
  operator()(const TInputPixel & input)
  {

    if constexpr (std::is_same<TInputPixel, typename NumericTraits<TInputPixel>::ValueType>::value)
    {
      m_Minimum = std::min(m_Minimum, input);
    }
    else
    {
      if (itk::NumericTraits<TInputPixel>::GetLength(m_Minimum) == 0)
      {
        m_Minimum = input;
      }
      else
      {
        for (unsigned int i = 0; i < itk::NumericTraits<TInputPixel>::GetLength(m_Minimum); ++i)
        {
          m_Minimum[i] = std::min(m_Minimum[i], input[i]);
        }
      }
    }
  }

  inline TInputPixel
  GetValue()
  {
    return m_Minimum;
  }

  TInputPixel m_Minimum;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class MinimumProjectionImageFilter
  : public ProjectionImageFilter<TInputImage,
                                 TOutputImage,
                                 Functor::MinimumAccumulator<typename TInputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinimumProjectionImageFilter);

  using Self = MinimumProjectionImageFilter;
  using Superclass =
    ProjectionImageFilter<TInputImage, TOutputImage, Functor::MinimumAccumulator<typename TInputImage::PixelType>>;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MinimumProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkConceptMacro(InputPixelTypeGreaterThanComparable,
                  (Concept::LessThanComparable<typename itk::NumericTraits<InputPixelType>::ValueType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

protected:
  MinimumProjectionImageFilter() = default;
  ~MinimumProjectionImageFilter() override = default;
}; // end
   // MinimumProjectionImageFilter
} // end namespace itk

#endif
