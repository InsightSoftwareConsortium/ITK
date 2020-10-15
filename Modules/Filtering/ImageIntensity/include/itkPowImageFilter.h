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
#ifndef itkPowImageFilter_h
#define itkPowImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Functor
{
/**
 * \class Pow
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2 = TInput1, typename TOutput = TInput1>
class Pow
{
public:
  Pow() = default;
  bool
  operator!=(const Pow &) const
  {
    // we contain no data, so we are always the same
    return false;
  }

  bool
  operator==(const Pow & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {

    using RealType1 = typename NumericTraits<TInput1>::RealType;
    using RealType2 = typename NumericTraits<TInput2>::RealType;
    return static_cast<TOutput>(std::pow(static_cast<RealType1>(A), static_cast<RealType2>(B)));
  }
};
} // namespace Functor

/**
 *\class PowImageFilter
 * \brief Computes the powers of 2 images
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The output of the pow function will be cast to the pixel type of
 * the output image.
 *
 * The total operation over one pixel will be
   \code
   output_pixel = static_cast< TOutput >( std::pow(static_cast<RealType>(A),static_cast<RealType>(B)) );
   \endcode
 *
 * The pow function can be applied to two images with the following:
   \code
   SetInput1( image1 );
   SetInput2( image2 );
   \endcode
 *
 * Additionally, this filter can be used to raise every pixel of an
 * image to a power of a constant by using
   \code
   SetInput1( image1 );
   SetConstant2( constant );
   \endcode
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class PowImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PowImageFilter);

  /** Standard class type aliases. */
  using Self = PowImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType =
    Functor::Pow<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(PowImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // End concept checking
#endif

protected:
  PowImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~PowImageFilter() override = default;
};
} // end namespace itk

#endif
