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
#ifndef itkExpImageFilter_h
#define itkExpImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Exp
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class Exp
{
public:
  Exp() = default;
  ~Exp() = default;
  bool
  operator!=(const Exp &) const
  {
    return false;
  }

  bool
  operator==(const Exp & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(std::exp(static_cast<double>(A)));
  }
};
} // namespace Functor

/**
 *\class ExpImageFilter
 * \brief Computes the exponential function of each pixel.
 *
 * The computation is performed using std::exp(x).
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 *
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage>
class ExpImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ExpImageFilter);

  /** Standard class type aliases. */
  using Self = ExpImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Exp<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ExpImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  ExpImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~ExpImageFilter() override = default;
};
} // end namespace itk

#endif
