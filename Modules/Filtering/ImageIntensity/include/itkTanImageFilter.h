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
#ifndef itkTanImageFilter_h
#define itkTanImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Tan
 * \brief
 * \ingroup ITKImageIntensity
 */
template <typename TInput, typename TOutput>
class Tan
{
public:
  Tan() = default;
  ~Tan() = default;
  bool
  operator!=(const Tan &) const
  {
    return false;
  }

  bool
  operator==(const Tan & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(std::tan(static_cast<double>(A)));
  }
};
} // namespace Functor

/**
 *\class TanImageFilter
 * \brief Computes the tangent of each input pixel.
 *
 * The computations are performed using std::tan(x).
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage>
class TanImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TanImageFilter);

  /** Standard class type aliases. */
  using Self = TanImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::Tan<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(TanImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToDoubleCheck, (Concept::Convertible<typename TInputImage::PixelType, double>));
  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  TanImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }


  ~TanImageFilter() override = default;
};
} // end namespace itk

#endif
