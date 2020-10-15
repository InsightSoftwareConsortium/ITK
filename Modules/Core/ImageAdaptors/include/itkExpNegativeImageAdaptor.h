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
#ifndef itkExpNegativeImageAdaptor_h
#define itkExpNegativeImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkMath.h"

namespace itk
{
namespace Accessor
{
/**
 *\class ExpNegativePixelAccessor
 * \brief Give access to the std::exp() function of a value
 *
 * ExpNegativePixelAccessor is templated over an internal type and an
 * external type representation. This class cast the input
 * applies the function to it and cast the result according
 * to the types defined as template parameters
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TInternalType, typename TExternalType>
class ExpNegativePixelAccessor
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
    output = static_cast<TInternalType>(std::exp(-static_cast<double>(input)));
  }

  static inline TExternalType
  Get(const TInternalType & input)
  {
    return static_cast<TExternalType>(std::exp(-static_cast<double>(input)));
  }
};
} // end namespace Accessor

/**
 *\class ExpNegativeImageAdaptor
 * \brief Presents an image as being composed of the std::exp() of its pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TImage, typename TOutputPixelType>
class ExpNegativeImageAdaptor
  : public ImageAdaptor<TImage, Accessor::ExpNegativePixelAccessor<typename TImage::PixelType, TOutputPixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ExpNegativeImageAdaptor);

  /** Standard class type aliases. */
  using Self = ExpNegativeImageAdaptor;
  using Superclass =
    ImageAdaptor<TImage, Accessor::ExpNegativePixelAccessor<typename TImage::PixelType, TOutputPixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExpNegativeImageAdaptor, ImageAdaptor);

protected:
  ExpNegativeImageAdaptor() = default;
  ~ExpNegativeImageAdaptor() override = default;
};
} // end namespace itk

#endif
