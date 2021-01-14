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
#ifndef itkEdgePotentialImageFilter_h
#define itkEdgePotentialImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"

namespace itk
{
/** \class EdgePotentialImageFilter
 *
 * \brief Computes the edge potential of an image from the image gradient.
 *
 * Input to this filter should be a CovariantVector image representing
 * the image gradient.
 *
 * The filter expect both the input and output images to have the same
 * number of dimensions, and the output to be of a scalar image type.
 *
 * \ingroup ITKImageIntensity
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/ComputeEdgePotential,Compute Edge Potential}
 * \endsphinx
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class EdgePotential
{
public:
  EdgePotential() = default;
  ~EdgePotential() = default;
  bool
  operator!=(const EdgePotential &) const
  {
    return false;
  }

  bool
  operator==(const EdgePotential & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(std::exp(-1.0 * A.GetNorm()));
  }
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class EdgePotentialImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(EdgePotentialImageFilter);

  /** Standard class type aliases. */
  using Self = EdgePotentialImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::EdgePotential<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(EdgePotentialImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  EdgePotentialImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~EdgePotentialImageFilter() override = default;
};
} // end namespace itk

#endif
