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
#ifndef itkRoundImageFilter_h
#define itkRoundImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"
#include <type_traits> // For conditional_t and is_integral_v.

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
  bool
  operator==(const Round &) const
  {
    return true;
  }

  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Round);

  inline TOutput
  operator()(const TInput & A) const
  {
    if constexpr (sizeof(TOutput) <= sizeof(int64_t) && !std::is_integral_v<TOutput>)
    {
      using IntegerType = std::conditional_t<sizeof(TOutput) <= sizeof(int32_t), int32_t, int64_t>;

      // The TReturn argument of Math::Round must be an integer type.
      return static_cast<TOutput>(Math::Round<IntegerType>(A));
    }
    else
    {
      // Rare (exceptional) case: sizeof(TOutput) > sizeof(int64_t).
      return Math::Round<TOutput>(A);
    }
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
class ITK_TEMPLATE_EXPORT RoundImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
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
  itkOverrideGetNameOfClassMacro(RoundImageFilter);

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
