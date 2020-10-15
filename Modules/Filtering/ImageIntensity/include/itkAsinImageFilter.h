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
#ifndef itkAsinImageFilter_h
#define itkAsinImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Asin
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class Asin
{
public:
  Asin() = default;
  ~Asin() = default;
  bool
  operator!=(const Asin &) const
  {
    return false;
  }

  bool
  operator==(const Asin & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(std::asin(static_cast<double>(A)));
  }
};
} // namespace Functor

/**
 *\class AsinImageFilter
 * \brief Computes the sine of each pixel.
 *
 * This filter is templated over the pixel type of the input image
 * and the pixel type of the output image.
 *
 * The filter walks over all the pixels in the input image, and for
 * each pixel does the following:
 *
 * \li cast the pixel value to \c double,
 * \li apply the \c std::asin() function to the \c double value,
 * \li cast the \c double value resulting from \c std::asin() to the
 *     pixel type of the output image,
 * \li store the casted value into the output image.
 *
 * The filter expects both images to have the same dimension (e.g. both
 * 2D, or both 3D, or both ND)
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage>
class AsinImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AsinImageFilter);

  /** Standard class type aliases. */
  using Self = AsinImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Asin<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AsinImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  AsinImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~AsinImageFilter() override = default;
};
} // end namespace itk

#endif
