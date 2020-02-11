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
#ifndef itkAbsImageFilter_h
#define itkAbsImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
namespace Functor
{
/** \class Abs
 * \brief Computes the absolute value of a pixel.
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class Abs
{
public:
  Abs() = default;
  ~Abs() = default;
  bool
  operator!=(const Abs &) const
  {
    return false;
  }

  bool
  operator==(const Abs & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(itk::Math::abs(A));
  }
};
} // namespace Functor

/** \class AbsImageFilter
 * \brief Computes the absolute value of each pixel.
 *
 * itk::Math::abs() is used to perform the computation.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/AbsValueOfImage,Absolute Value Of Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class AbsImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AbsImageFilter);

  /** Standard class type aliases. */
  using Self = AbsImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Abs<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AbsImageFilter, UnaryGeneratorImageFilter);

  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ConvertibleCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  itkConceptMacro(InputGreaterThanIntCheck, (Concept::GreaterThanComparable<InputPixelType, InputPixelType>));
  // End concept checking
#endif

protected:
  AbsImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~AbsImageFilter() override = default;
};
} // end namespace itk

#endif
