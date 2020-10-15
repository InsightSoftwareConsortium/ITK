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
#ifndef itkAbsoluteValueDifferenceImageFilter_h
#define itkAbsoluteValueDifferenceImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class AbsoluteValueDifferenceImageFilter
 * \brief Implements pixel-wise the computation of absolute value difference.
 *
 * This filter is parameterized over the types of the two
 * input images and the type of the output image.
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter will walk over all the pixels in the two input images, and for
 * each one of them it will do the following:
 *
 * \li Cast the input 1 pixel value to \c double.
 * \li Cast the input 2 pixel value to \c double.
 * \li Compute the difference of the two pixel values.
 * \li Compute the absolute value of the difference.
 * \li Cast the \c double value resulting from the absolute value to the pixel type of the output image.
 * \li Store the casted value into the output image.
 *
 * The filter expects all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND).
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageCompare
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageCompare/AbsValueOfTwoImages,Absolute Value Of Difference Between Two Images}
 * \endsphinx
 */
namespace Functor
{
template <typename TInput1, typename TInput2, typename TOutput>
class AbsoluteValueDifference2
{
public:
  AbsoluteValueDifference2() = default;
  ~AbsoluteValueDifference2() = default;
  bool
  operator!=(const AbsoluteValueDifference2 &) const
  {
    return false;
  }

  bool
  operator==(const AbsoluteValueDifference2 & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    const auto   dA = static_cast<double>(A);
    const auto   dB = static_cast<double>(B);
    const double diff = dA - dB;
    const double absdiff = (diff > 0.0) ? diff : -diff;

    return static_cast<TOutput>(absdiff);
  }
};
} // namespace Functor

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class AbsoluteValueDifferenceImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AbsoluteValueDifferenceImageFilter);

  /** Standard class type aliases. */
  using Self = AbsoluteValueDifferenceImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  using FunctorType = Functor::AbsoluteValueDifference2<typename TInputImage1::PixelType,
                                                        typename TInputImage2::PixelType,
                                                        typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AbsoluteValueDifferenceImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1CovertibleToDoubleCheck, (Concept::Convertible<typename TInputImage1::PixelType, double>));
  itkConceptMacro(Input2ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage2::PixelType, double>));
  itkConceptMacro(DoubleCovertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  AbsoluteValueDifferenceImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~AbsoluteValueDifferenceImageFilter() override = default;
};
} // end namespace itk

#endif
