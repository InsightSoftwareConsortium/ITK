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
#ifndef itkAtan2ImageFilter_h
#define itkAtan2ImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Atan2
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput1, typename TInput2, typename TOutput>
class Atan2
{
public:
  Atan2() = default;
  ~Atan2() = default;
  bool
  operator!=(const Atan2 &) const
  {
    return false;
  }

  bool
  operator==(const Atan2 & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B) const
  {
    return static_cast<TOutput>(std::atan2(static_cast<double>(A), static_cast<double>(B)));
  }
};
} // namespace Functor

/** \class Atan2ImageFilter
 * \brief Computes two argument inverse tangent.
 *
 * The first argument to the atan function is provided by a pixel
 * in the first input image (SetInput1()) and the corresponding
 * pixel in the second input image (SetInput2()) is used as the second
 * argument.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Both pixel input types are cast to \c double in order to be
 * used as parameters of \c std::atan2(). The resulting \c double value
 * is cast to the output pixel type.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/ApplyAtanImageFilter,Apply Atan Image Filter}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class Atan2ImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Atan2ImageFilter);

  /** Standard class type aliases. */
  using Self = Atan2ImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::
    Atan2<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(Atan2ImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage1::PixelType, double>));
  itkConceptMacro(Input2ConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage2::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  Atan2ImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif // !defined( ITK_WRAPPING_PARSER )
  }
  ~Atan2ImageFilter() override = default;
};
} // end namespace itk

#endif
