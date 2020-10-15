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
#ifndef itkMinimumImageFilter_h
#define itkMinimumImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class Minimum
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2 = TInput1, typename TOutput = TInput1>
class Minimum
{
public:
  Minimum() = default;
  ~Minimum() = default;
  bool
  operator!=(const Minimum &) const
  {
    return false;
  }

  bool
  operator==(const Minimum & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    return static_cast<TOutput>((A < B) ? A : B);
  }
};
} // namespace Functor

/** \class MinimumImageFilter
 * \brief Implements a pixel-wise operator Min(a,b) between two images.
 *
 * The pixel values of the output image are the minimum between the
 * corresponding pixels of the two input images.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/SetOutputPixelToMin,Compare Two Images And Set Output Pixel To Min}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class MinimumImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinimumImageFilter);

  /** Standard class type aliases. */
  using Self = MinimumImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::
    Minimum<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MinimumImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1ConvertibleToInput2Check,
                  (Concept::Convertible<typename TInputImage1::PixelType, typename TInputImage2::PixelType>));
  itkConceptMacro(Input2ConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage2::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(Input1LessThanInput2Check,
                  (Concept::LessThanComparable<typename TInputImage1::PixelType, typename TInputImage2::PixelType>));
  // End concept checking
#endif

protected:
  MinimumImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~MinimumImageFilter() override = default;
};
} // end namespace itk

#endif
