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
#ifndef itkSquareImageFilter_h
#define itkSquareImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"

namespace itk
{
/**
 *\class SquareImageFilter
 * \brief Computes the square of the intensity values pixel-wise
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/SquareEveryPixel,Square Every Pixel}
 * \endsphinx
 */

namespace Functor
{
template <typename TInput, typename TOutput>
class Square
{
public:
  using RealType = typename NumericTraits<TInput>::RealType;
  Square() = default;
  ~Square() = default;
  bool
  operator!=(const Square &) const
  {
    return false;
  }

  bool
  operator==(const Square & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    const auto ra = static_cast<RealType>(A);

    return static_cast<TOutput>(ra * ra);
  }
};
} // namespace Functor
template <typename TInputImage, typename TOutputImage>
class SquareImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SquareImageFilter);

  /** Standard class type aliases. */
  using Self = SquareImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Square<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SquareImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType>));
  itkConceptMacro(RealTypeMultiplyOperatorCheck,
                  (Concept::MultiplyOperator<typename NumericTraits<typename TInputImage::PixelType>::RealType>));
  // End concept checking
#endif

protected:
  SquareImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~SquareImageFilter() override = default;
};
} // end namespace itk

#endif
