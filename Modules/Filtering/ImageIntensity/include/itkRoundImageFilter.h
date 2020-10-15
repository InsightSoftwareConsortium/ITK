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
#ifndef itkRoundImageFilter_h
#define itkRoundImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Round
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class Round
{
public:
  Round() = default;
  ~Round() = default;
  bool
  operator!=(const Round &) const
  {
    return false;
  }

  bool
  operator==(const Round & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return itk::Math::Round<TOutput, TInput>(A);
  }
};
} // namespace Functor

/** \class RoundImageFilter
 * \brief Rounds the value of each pixel.
 *
 * The computations are performed using itk::Math::Round(x).
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage>
class RoundImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RoundImageFilter);

  /** Standard class type aliases. */
  using Self = RoundImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Round<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RoundImageFilter, UnaryGeneratorImageFilter);

protected:
  RoundImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~RoundImageFilter() override = default;
};
} // end namespace itk

#endif
