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
#ifndef itkAddImageAdaptor_h
#define itkAddImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkAddPixelAccessor.h"

namespace itk
{
/**
 *\class AddImageAdaptor
 * \brief Presents an image as being the addition of a constant value to all pixels
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 */
template <typename TImage>
class AddImageAdaptor : public ImageAdaptor<TImage, Accessor::AddPixelAccessor<typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AddImageAdaptor);

  /** Standard class type aliases. */
  using Self = AddImageAdaptor;
  using Superclass = ImageAdaptor<TImage, Accessor::AddPixelAccessor<typename TImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PixelType = typename TImage::PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(AddImageAdaptor, ImageAdaptor);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the value to be added to image pixels */
  void
  SetValue(const PixelType newvalue)
  {
    this->GetPixelAccessor().SetValue(newvalue);
  }

  /** Get the value to be added to image pixels */
  PixelType
  GetValue() const
  {
    return this->GetPixelAccessor().GetValue();
  }

protected:
  AddImageAdaptor() = default;
  ~AddImageAdaptor() override = default;
};
} // end namespace itk

#endif
