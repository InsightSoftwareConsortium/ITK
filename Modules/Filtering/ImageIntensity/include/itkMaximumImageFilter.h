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
#ifndef itkMaximumImageFilter_h
#define itkMaximumImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class Maximum
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2 = TInput1, typename TOutput = TInput1>
class Maximum
{
public:
  Maximum() = default;
  ~Maximum() = default;
  bool
  operator!=(const Maximum &) const
  {
    return false;
  }

  bool
  operator==(const Maximum & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    if (A > B)
    {
      return static_cast<TOutput>(A);
    }
    else
    {
      return static_cast<TOutput>(B);
    }
  }
};
} // namespace Functor

/**
 *\class MaximumImageFilter
 * \brief Implements a pixel-wise operator Max(a,b) between two images.
 *
 * The pixel values of the output image are the maximum between the
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
 * \sphinxexample{Filtering/ImageIntensity/SetOutputPixelToMax,Compare Two Images And Set Output Pixel To Max}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class MaximumImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaximumImageFilter);

  /** Standard class type aliases. */
  using Self = MaximumImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::
    Maximum<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaximumImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1ConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage1::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(Input2ConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage2::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(Input1GreaterThanInput2Check,
                  (Concept::GreaterThanComparable<typename TInputImage1::PixelType, typename TInputImage2::PixelType>));
  // End concept checking
#endif

protected:
  MaximumImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~MaximumImageFilter() override = default;
};
} // end namespace itk

#endif
