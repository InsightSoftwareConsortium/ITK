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
#ifndef itkBinaryMagnitudeImageFilter_h
#define itkBinaryMagnitudeImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class Modulus2
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2, typename TOutput>
class Modulus2
{
public:
  Modulus2() = default;
  ~Modulus2() = default;
  bool
  operator!=(const Modulus2 &) const
  {
    return false;
  }

  bool
  operator==(const Modulus2 & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    const auto dA = static_cast<double>(A);
    const auto dB = static_cast<double>(B);

    return static_cast<TOutput>(std::sqrt(dA * dA + dB * dB));
  }
};
} // namespace Functor

/**
 *\class BinaryMagnitudeImageFilter
 * \brief Computes the square root of the sum of squares of corresponding input pixels.
 *
 * This filter is templated over the types of the two
 * input images and the type of the output image.
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter walks over all of the pixels in the two input images, and for
 * each pixel does the following:
 *
 * \li cast the input 1 pixel value to \c double
 * \li cast the input 2 pixel value to \c double
 * \li compute the sum of squares of the two pixel values
 * \li compute the square root of the sum
 * \li cast the \c double value resulting from \c std::sqrt() to the pixel type of the output image
 * \li store the cast value into the output image.
 *
 * The filter expects all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND)
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class BinaryMagnitudeImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryMagnitudeImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryMagnitudeImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::
    Modulus2<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryMagnitudeImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage1::PixelType, double>));
  itkConceptMacro(Input2ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage2::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  BinaryMagnitudeImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~BinaryMagnitudeImageFilter() override = default;
};
} // end namespace itk

#endif
