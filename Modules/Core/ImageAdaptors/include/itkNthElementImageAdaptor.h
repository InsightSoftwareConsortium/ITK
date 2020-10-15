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
#ifndef itkNthElementImageAdaptor_h
#define itkNthElementImageAdaptor_h

#include "itkImageAdaptor.h"
#include "itkNthElementPixelAccessor.h"

namespace itk
{
/**
 *\class NthElementImageAdaptor
 * \brief Presents an image as being composed of the N-th element of its pixels
 *
 * It assumes that the pixels are of container type and have in their API
 * an operator[]( unsigned int ) defined.
 *
 * Additional casting is performed according to the input and output image
 * types following C++ default casting rules.
 *
 * \ingroup ImageAdaptors
 * \ingroup ITKImageAdaptors
 *
 * \sphinx
 * \sphinxexample{Core/ImageAdaptors/ExtractChannelOfImageWithMultipleComponents,Extract Channel Of Image With Multiple
 * Components} \sphinxexample{Core/ImageAdaptors/ProcessNthComponentOfVectorImage,Process Nth Component Of Vector Image}
 * \endsphinx
 */

// Create a helper class to help the SunPro CC compiler
// parse the templates for the NthElementImageAdaptor.
// This is used to define the Super class.  for NthElementImageAdaptor
template <typename TImage, typename TOutputPixelType>
class NthElementImageAdaptorHelper
{
public:
  using PixelAccessor = NthElementPixelAccessor<TOutputPixelType, typename TImage::PixelType>;

  using Super = ImageAdaptor<TImage, PixelAccessor>;
};

template <typename TImage, typename TOutputPixelType>
class NthElementImageAdaptor : public NthElementImageAdaptorHelper<TImage, TOutputPixelType>::Super
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NthElementImageAdaptor);

  /** Standard class type aliases. */
  using Self = NthElementImageAdaptor;
  using Superclass = typename NthElementImageAdaptorHelper<TImage, TOutputPixelType>::Super;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NthElementImageAdaptor, ImageAdaptor);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Select the element number to be accessed */
  void
  SelectNthElement(unsigned int nth)
  {
    this->GetPixelAccessor().SetElementNumber(nth);
    this->Modified();
  }

protected:
  NthElementImageAdaptor() = default;
  ~NthElementImageAdaptor() override = default;
};
} // end namespace itk

#endif
