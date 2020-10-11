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
#ifndef itkComplexToRealImageAdaptor_h
#define itkComplexToRealImageAdaptor_h

#include "itkImageAdaptor.h"
#include <complex>

namespace itk
{
namespace Accessor
{
/**
 *\class ComplexToRealPixelAccessor
 * \brief Give access to the Real part of a std::complex<> value.
 *
 * ComplexToRealPixelAccessor is templated over an internal type and an
 * external type representation. The internal type is an std::complex<T> and
 * the external part is a type T. This class casts the input, applies the function
 * to it, and casts the result according to the types defined as template
 * parameters.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TInternalType, typename TExternalType>
class ComplexToRealPixelAccessor
{
public:
  /** External type alias. It defines the external aspect
   * that this class will exhibit. */
  using ExternalType = TExternalType;

  /** Internal type alias. It defines the internal real
   * representation of data. */
  using InternalType = TInternalType;

  static inline void
  Set(TInternalType & output, const TExternalType & input)
  {
    output = (TInternalType)(input);
  }

  static inline TExternalType
  Get(const TInternalType & input)
  {
    return (TExternalType)(input.real());
  }
};
} // end namespace Accessor

/**
 *\class ComplexToRealImageAdaptor
 * \brief Presents a complex image as being composed of real() part of
 *        its pixels.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TImage, typename TOutputPixelType>
class ComplexToRealImageAdaptor
  : public ImageAdaptor<TImage, Accessor::ComplexToRealPixelAccessor<typename TImage::PixelType, TOutputPixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ComplexToRealImageAdaptor);

  /** Standard class type aliases. */
  using Self = ComplexToRealImageAdaptor;
  using Superclass =
    ImageAdaptor<TImage, Accessor::ComplexToRealPixelAccessor<typename TImage::PixelType, TOutputPixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ComplexToRealImageAdaptor, ImageAdaptor);

protected:
  ComplexToRealImageAdaptor() = default;
  ~ComplexToRealImageAdaptor() override = default;
};
} // end namespace itk

#endif
