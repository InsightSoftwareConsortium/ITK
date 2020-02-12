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
#ifndef itkVectorMagnitudeImageFilter_h
#define itkVectorMagnitudeImageFilter_h

#include "itkUnaryGeneratorImageFilter.h"

namespace itk
{
/**
 *\class VectorMagnitudeImageFilter
 *
 * \brief Take an image of vectors as input and produce an image with the
 *  magnitude of those vectors.
 *
 * The filter expects the input image pixel type to be a vector and
 * the output image pixel type to be a scalar.
 *
 * This filter assumes that the PixelType of the input image is a VectorType
 * that provides a GetNorm() method.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/ComputerMagInVectorImageToMakeMagImage,Computer Magnitude In Vector Image To
 * Make Magnitude Image} \endsphinx
 */

namespace Functor
{
template <typename TInput, typename TOutput>
class VectorMagnitude
{
public:
  VectorMagnitude() = default;
  ~VectorMagnitude() = default;

  bool
  operator!=(const VectorMagnitude &) const
  {
    return false;
  }

  bool
  operator==(const VectorMagnitude & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    return static_cast<TOutput>(A.GetNorm());
  }
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class VectorMagnitudeImageFilter : public UnaryGeneratorImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorMagnitudeImageFilter);

  /** Standard class type aliases. */
  using Self = VectorMagnitudeImageFilter;
  using Superclass = UnaryGeneratorImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType = Functor::VectorMagnitude<typename TInputImage::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(VectorMagnitudeImageFilter, UnaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  VectorMagnitudeImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }


  ~VectorMagnitudeImageFilter() override = default;
};
} // end namespace itk

#endif
