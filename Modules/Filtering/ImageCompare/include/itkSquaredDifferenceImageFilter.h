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
#ifndef itkSquaredDifferenceImageFilter_h
#define itkSquaredDifferenceImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"

namespace itk
{
/**
 *\class SquaredDifferenceImageFilter
 * \brief Implements pixel-wise the computation of squared difference.
 *
 * This filter is parameterized over the types of the two
 * input images and the type of the output image.
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter will walk over all the pixels in the two input images, and for
 * each one of them it will do the following:
 *
 * - cast the input 1 pixel value to \c double
 * - cast the input 2 pixel value to \c double
 * - compute the difference of the two pixel values
 * - compute the square of the difference
 * - cast the \c double value resulting from \c sqr() to the pixel type of the output image
 * - store the casted value into the output image.
 *
 * The filter expect all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND)
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageCompare
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageCompare/SquaredDifferenceOfTwoImages,Squared Difference Of Two Images}
 * \endsphinx
 */
namespace Functor
{
template <typename TInput1, typename TInput2, typename TOutput>
class SquaredDifference2
{
public:
  SquaredDifference2() = default;
  ~SquaredDifference2() = default;
  bool
  operator!=(const SquaredDifference2 &) const
  {
    return false;
  }

  bool
  operator==(const SquaredDifference2 & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    const auto   dA = static_cast<double>(A);
    const auto   dB = static_cast<double>(B);
    const double diff = dA - dB;

    return static_cast<TOutput>(diff * diff);
  }
};
} // namespace Functor

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class SquaredDifferenceImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SquaredDifferenceImageFilter);

  /** Standard class type aliases. */
  using Self = SquaredDifferenceImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using FunctorType = Functor::SquaredDifference2<typename TInputImage1::PixelType,
                                                  typename TInputImage2::PixelType,
                                                  typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SquaredDifferenceImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage1::PixelType, double>));
  itkConceptMacro(Input2ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage2::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  SquaredDifferenceImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~SquaredDifferenceImageFilter() override = default;
};
} // end namespace itk

#endif
