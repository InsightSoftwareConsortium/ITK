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
#ifndef itkVectorToRGBImageAdaptor_h
#define itkVectorToRGBImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkVectorToRGBPixelAccessor.h"

namespace itk
{
/**
 *\class VectorToRGBImageAdaptor
 * \brief Presents an image of pixel type Vector as being and image of
 * RGBPixel type.
 *
 * \ingroup ImageAdaptors
 *
 * \ingroup ITKImageAdaptors
 */
template <typename TImage>
class VectorToRGBImageAdaptor
  : public ImageAdaptor<TImage, Accessor::VectorToRGBPixelAccessor<typename TImage::PixelType::ValueType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorToRGBImageAdaptor);

  /** Standard class type aliases. */
  using Self = VectorToRGBImageAdaptor;
  using Superclass = ImageAdaptor<TImage, Accessor::VectorToRGBPixelAccessor<typename TImage::PixelType::ValueType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorToRGBImageAdaptor, ImageAdaptor);

  /** PixelContainer type alias support Used to construct a container for
   * the pixel data. */
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using PixelContainerConstPointer = typename Superclass::PixelContainerConstPointer;

protected:
  VectorToRGBImageAdaptor() = default;
  ~VectorToRGBImageAdaptor() override = default;
};
} // end namespace itk

#endif
