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
#ifndef itkRegionBasedLevelSetFunctionData_h
#define itkRegionBasedLevelSetFunctionData_h

#include "itkLightObject.h"

#include "itkVector.h"
#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class RegionBasedLevelSetFunctionData
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 *
 * This class holds cache data used during the computation of the LevelSet updates.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ITKReview
 */
template <typename TInputImage, typename TFeatureImage>
class ITK_TEMPLATE_EXPORT RegionBasedLevelSetFunctionData : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionBasedLevelSetFunctionData);

  using Self = RegionBasedLevelSetFunctionData;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(RegionBasedLevelSetFunctionData, LightObject);

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputPixelType = typename InputImageType::PixelType;
  using InputRegionType = typename InputImageType::RegionType;
  using InputSizeType = typename InputImageType::SizeType;
  using InputSizeValueType = typename InputSizeType::SizeValueType;
  using InputSpacingType = typename InputImageType::SpacingType;
  using InputIndexType = typename InputImageType::IndexType;
  using InputIndexValueType = typename InputIndexType::IndexValueType;
  using InputPointType = typename InputImageType::PointType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeatureImageConstPointer = typename FeatureImageType::ConstPointer;
  using FeaturePixelType = typename FeatureImageType::PixelType;
  using FeatureRegionType = typename FeatureImageType::RegionType;
  using FeatureSizeType = typename FeatureImageType::SizeType;
  using FeatureSizeValueType = typename FeatureSizeType::SizeValueType;
  using FeatureSpacingType = typename FeatureImageType::SpacingType;
  using FeatureIndexType = typename FeatureImageType::IndexType;
  using FeaturePointType = typename FeatureImageType::PointType;

  // Allocates m_HeavisideFunctionOfLevelSetImage to have same origin,
  // spacing and size as image. Also sets the m_Start and m_End indices.
  void
  CreateHeavisideFunctionOfLevelSetImage(const InputImageType * image);

  // Checks if the given index lies in the domain of the current
  // level-set function. The domain is defined by the start and end indices.
  template <typename TIndex>
  bool
  VerifyInsideRegion(const TIndex & featureIndex)
  {
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      if ((featureIndex[j] < static_cast<InputIndexValueType>(this->m_Start[j])) ||
          (featureIndex[j] > static_cast<InputIndexValueType>(this->m_End[j])))
      {
        return false;
      }
    }
    return true;
  }

  // Get the index into the domain of the current level-set function
  InputIndexType
  GetIndex(const FeatureIndexType & featureIndex);

  // Get the index in the domain of the feature image
  FeatureIndexType
  GetFeatureIndex(const InputIndexType & inputIndex);

  double m_WeightedNumberOfPixelsInsideLevelSet;
  double m_WeightedNumberOfPixelsOutsideLevelSet;

  InputImagePointer m_HeavisideFunctionOfLevelSetImage;
  InputIndexType    m_Start;
  InputIndexType    m_End;

protected:
  RegionBasedLevelSetFunctionData();
  ~RegionBasedLevelSetFunctionData() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionBasedLevelSetFunctionData.hxx"
#endif
#endif
