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
#ifndef itkComplexToImaginaryImageFilter_h
#define itkComplexToImaginaryImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
/**
 *\class ComplexToImaginaryImageFilter
 * \brief Computes pixel-wise the imaginary part of a complex image.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class ComplexToImaginary
{
public:
  ComplexToImaginary() = default;
  ~ComplexToImaginary() = default;
  bool
  operator!=(const ComplexToImaginary &) const
  {
    return false;
  }

  bool
  operator==(const ComplexToImaginary & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(A.imag());
  }
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class ComplexToImaginaryImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComplexToImaginaryImageFilter);

  /** Standard class type aliases. */
  using Self = ComplexToImaginaryImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::ComplexToImaginary<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ComplexToImaginaryImageFilter, UnaryGeneratorImageFilter);

  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelValueType = typename NumericTraits<InputPixelType>::ValueType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelValueType, OutputPixelType>));
  // End concept checking
#endif

protected:
  ComplexToImaginaryImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~ComplexToImaginaryImageFilter() override = default;
};
} // end namespace itk

#endif
