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
#ifndef itkRGBToLuminanceImageFilter_h
#define itkRGBToLuminanceImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"

namespace itk
{
/**
 *\class RGBToLuminanceImageFilter
 * \brief Converts an RGB image into a grayscale image.
 *
 * This filters converts an RGB image into a Luminance on by computing
 * pixel-wise a linear combination on the Red, Green and Blue channels. The
 * pixel type of the input image must have a GetLuminance() method. This is the
 * case of the itk::RGBPixel class.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class RGBToLuminance
{
public:
  using ComponentType = typename TInput::ComponentType;
  using RealType = typename itk::NumericTraits<ComponentType>::RealType;

  RGBToLuminance() = default;
  ~RGBToLuminance() = default;
  bool
  operator!=(const RGBToLuminance &) const
  {
    return false;
  }

  bool
  operator==(const RGBToLuminance & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(A.GetLuminance());
  }
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class RGBToLuminanceImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RGBToLuminanceImageFilter);

  /** Standard class type aliases. */
  using Self = RGBToLuminanceImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::RGBToLuminance<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RGBToLuminanceImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TInputImage::PixelType::ComponentType>));
  // End concept checking
#endif

protected:
  RGBToLuminanceImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~RGBToLuminanceImageFilter() override = default;
};
} // end namespace itk

#endif
