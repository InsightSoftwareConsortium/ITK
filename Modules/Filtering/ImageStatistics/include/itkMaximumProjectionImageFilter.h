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
#ifndef itkMaximumProjectionImageFilter_h
#define itkMaximumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class MaximumProjectionImageFilter
 * \brief Maximum projection
 *
 * This class was contributed to the insight journal by Gaetan Lehmann.
 * The original paper can be found at
 * https://doi.org/10.54294/0pjyho
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la reproduction,
 *  inra de jouy-en-josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template <typename TInputPixel>
class MaximumAccumulator
{
public:
  MaximumAccumulator(SizeValueType) {}
  ~MaximumAccumulator() = default;

  inline void
  Initialize()
  {

    // check if scalar or fixed length array type
    if constexpr (std::is_same<TInputPixel, typename NumericTraits<TInputPixel>::ValueType>::value)
    {
      m_Maximum = NumericTraits<TInputPixel>::NonpositiveMin();
    }
    else
    {
      m_Maximum = TInputPixel();
      m_Maximum.Fill(NumericTraits<typename TInputPixel::ValueType>::NonpositiveMin());
    }
  }

  inline void
  operator()(const TInputPixel & input)
  {
    if constexpr (std::is_same<TInputPixel, typename NumericTraits<TInputPixel>::ValueType>::value)
    {
      m_Maximum = std::max(m_Maximum, input);
    }
    else
    {
      if (itk::NumericTraits<TInputPixel>::GetLength(m_Maximum) == 0)
      {
        m_Maximum = input;
      }
      else
      {
        for (unsigned int i = 0; i < itk::NumericTraits<TInputPixel>::GetLength(m_Maximum); ++i)
        {
          m_Maximum[i] = std::max(m_Maximum[i], input[i]);
        }
      }
    }
  }

  inline TInputPixel
  GetValue()
  {
    return m_Maximum;
  }

  TInputPixel m_Maximum;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class MaximumProjectionImageFilter
  : public ProjectionImageFilter<TInputImage,
                                 TOutputImage,
                                 Functor::MaximumAccumulator<typename TInputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaximumProjectionImageFilter);

  using Self = MaximumProjectionImageFilter;
  using Superclass =
    ProjectionImageFilter<TInputImage, TOutputImage, Functor::MaximumAccumulator<typename TInputImage::PixelType>>;

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MaximumProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkConceptMacro(InputPixelTypeGreaterThanComparable,
                  (Concept::GreaterThanComparable<typename itk::NumericTraits<InputPixelType>::ValueType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));

protected:
  MaximumProjectionImageFilter() = default;
  ~MaximumProjectionImageFilter() override = default;
}; // end
   // MaximumProjectionImageFilter
} // end namespace itk

#endif
