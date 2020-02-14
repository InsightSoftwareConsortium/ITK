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
#ifndef itkVectorCastImageFilter_h
#define itkVectorCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{
/**
 *\class VectorCastImageFilter
 *
 * \brief Casts input vector pixels to output vector pixel type.
 *
 * This filter is templated over the input image type and
 * output image type.
 *
 * The filter expect both images to have the same number of dimensions,
 * and that both the input and output have itk::Vector pixel types
 * of the same VectorDimension.
 *
 * \sa Vector
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKDeprecated
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class VectorCast
{
public:
  VectorCast() = default;
  ~VectorCast() = default;
  bool
  operator!=(const VectorCast &) const
  {
    return false;
  }

  bool
  operator==(const VectorCast & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    using OutputValueType = typename TOutput::ValueType;

    TOutput value;
    for (unsigned int k = 0; k < TOutput::Dimension; k++)
    {
      value[k] = static_cast<OutputValueType>(A[k]);
    }
    return value;
  }
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class VectorCastImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::VectorCast<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorCastImageFilter);

  /** Standard class type aliases. */
  using Self = VectorCastImageFilter;
  using Superclass =
    UnaryFunctorImageFilter<TInputImage,
                            TOutputImage,
                            Functor::VectorCast<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(VectorCastImageFilter, UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TOutputImage::PixelType::ValueType>));
  itkConceptMacro(
    InputConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage::PixelType::ValueType, typename TOutputImage::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  VectorCastImageFilter() = default;
  ~VectorCastImageFilter() override = default;
};
} // end namespace itk

#endif
