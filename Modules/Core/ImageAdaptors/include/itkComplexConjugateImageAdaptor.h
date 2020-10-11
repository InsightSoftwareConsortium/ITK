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
#ifndef itkComplexConjugateImageAdaptor_h
#define itkComplexConjugateImageAdaptor_h

#include "itkImageAdaptor.h"
#include <complex>

namespace itk
{
namespace Accessor
{
/**
 *\class ComplexConjugatePixelAccessor
 * \brief Provide access to the conjugate of a std::complex<> value.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TComplexType>
class ComplexConjugatePixelAccessor
{
public:
  using ExternalType = TComplexType;
  using InternalType = TComplexType;

  static inline void
  Set(TComplexType & output, const TComplexType & input)
  {
    output = std::conj(input);
  }

  static inline TComplexType
  Get(const TComplexType & input)
  {
    return std::conj(input);
  }
};
} // end namespace Accessor

/**
 *\class ComplexConjugateImageAdaptor
 * \brief Presents each pixel of a complex image as its complex conjugate.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TImage>
class ComplexConjugateImageAdaptor
  : public ImageAdaptor<TImage, Accessor::ComplexConjugatePixelAccessor<typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ComplexConjugateImageAdaptor);

  /** Standard class type aliases. */
  using Self = ComplexConjugateImageAdaptor;
  using Superclass = ImageAdaptor<TImage, Accessor::ComplexConjugatePixelAccessor<typename TImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComplexConjugateImageAdaptor, ImageAdaptor);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking. */
  itkConceptMacro(InputConvertibleToComplex,
                  (Concept::Convertible<std::complex<typename NumericTraits<typename TImage::PixelType>::ValueType>,
                                        typename TImage::PixelType>));
  // End concept checking. */
#endif

protected:
  ComplexConjugateImageAdaptor() = default;
  ~ComplexConjugateImageAdaptor() override = default;
};
} // end namespace itk

#endif
