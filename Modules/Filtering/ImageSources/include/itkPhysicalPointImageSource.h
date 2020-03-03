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

#ifndef itkPhysicalPointImageSource_h
#define itkPhysicalPointImageSource_h

#include "itkGenerateImageSource.h"

namespace itk
{

/**
 *\class PhysicalPointImageSource
 * \brief Generate an image of the physical locations of each pixel.
 *
 * This image source supports image which have a multi-component pixel
 * equal to the image dimension, and variable length VectorImages. It
 * is recommended that the component type be a real valued type.
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT PhysicalPointImageSource : public GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhysicalPointImageSource);

  using Self = PhysicalPointImageSource;
  using Superclass = GenerateImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Output image type alias */
  using OutputImageType = TOutputImage;
  using PixelType = typename OutputImageType::PixelType;
  using RegionType = typename OutputImageType::RegionType;
  using SpacingType = typename OutputImageType::SpacingType;
  using PointType = typename OutputImageType::PointType;
  using DirectionType = typename OutputImageType::DirectionType;


  using SizeType = typename RegionType::SizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhysicalPointImageSource, GenerateImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  PhysicalPointImageSource()
  {
    this->DynamicMultiThreadingOn();
    this->ThreaderUpdateProgressOff();
  };
  ~PhysicalPointImageSource() override = default;

  void
  GenerateOutputInformation() override;

  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhysicalPointImageSource.hxx"
#endif

#endif
