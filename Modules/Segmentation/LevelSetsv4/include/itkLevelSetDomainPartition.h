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
#ifndef itkLevelSetDomainPartition_h
#define itkLevelSetDomainPartition_h

#include "itkLevelSetDomainPartitionBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 *\class LevelSetDomainPartition
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 * \ingroup ITKLevelSetsv4
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LevelSetDomainPartition : public LevelSetDomainPartitionBase<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartition);

  using Self = LevelSetDomainPartition;
  using Superclass = LevelSetDomainPartitionBase<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetDomainPartition, LevelSetDomainPartitionBase);

  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;

  using ListPixelType = typename Superclass::ListPixelType;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void
  PopulateListImage();

protected:
  LevelSetDomainPartition() = default;
  ~LevelSetDomainPartition() = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainPartition.hxx"
#endif

#endif
