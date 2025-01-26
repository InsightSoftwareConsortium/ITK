/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkReplaceFeatureMapNanInfImageFilter_h
#define itkReplaceFeatureMapNanInfImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"

// useful filters
#include <itkVectorIndexSelectionCastImageFilter.h>
#include <itkMinimumMaximumImageFilter.h>
#include <itkMaskImageFilter.h>
#include <itkComposeImageFilter.h>
#include <itkMath.h>

#include <vector>

namespace itk
{
/** \class ReplaceFeatureMapNanInfImageFilter
 * \brief This new filter can be used after the usage of itkBoneMorphometryFeaturesImageFilter
 * in order to remove the Nan and Inf values of the feature maps. (Those values are due to
 * neighborhood containing only bone voxel, containing 0 bone voxel)
 *
 * This filter is working with two passes for each feature map:
 *   -The first pass allow the detection of the minumum and maximum values of the feature maps.
 *   -During the second pass, every NaN or Inf value will be replace by eather the maximum of minimum value detected
 *   in the first path  depending of the feature.
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup BoneMorphometry
 *
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ReplaceFeatureMapNanInfImageFilter : public ImageToImageFilter<TImage, TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ReplaceFeatureMapNanInfImageFilter);

  /** Standard Self type alias. */
  using Self = ReplaceFeatureMapNanInfImageFilter;
  using Superclass = ImageToImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkOverrideGetNameOfClassMacro(ReplaceFeatureMapNanInfImageFilter);

protected:
  /** Input Image related type alias. */
  using ImagePointer = typename TImage::Pointer;
  using RegionType = typename TImage::RegionType;
  using SizeType = typename TImage::SizeType;
  using IndexType = typename TImage::IndexType;
  using PixelType = typename TImage::PixelType;

  /** Type to use for computations. */
  using RealType = typename NumericTraits<PixelType>::ScalarRealType;

  /** Intermediate Image related type alias. */
  using InterImageType = itk::Image<RealType, TImage::ImageDimension>;
  using InterIteratorType = itk::ImageRegionConstIterator<InterImageType>;

  ReplaceFeatureMapNanInfImageFilter();
  ~ReplaceFeatureMapNanInfImageFilter() override = default;

  using IndexSelectionFiterType = VectorIndexSelectionCastImageFilter<TImage, InterImageType>;
  using MinMaxImageFilterType = MinimumMaximumImageFilter<InterImageType>;
  using MaskImageFilterType = MaskImageFilter<InterImageType, InterImageType>;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  typename IndexSelectionFiterType::Pointer m_IndexSelectionFiter;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkReplaceFeatureMapNanInfImageFilter.hxx"
#endif

#endif // itkReplaceFeatureMapNanInfImageFilter_h
