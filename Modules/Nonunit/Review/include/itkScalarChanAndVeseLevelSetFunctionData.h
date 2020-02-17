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
#ifndef itkScalarChanAndVeseLevelSetFunctionData_h
#define itkScalarChanAndVeseLevelSetFunctionData_h

#include "itkRegionBasedLevelSetFunctionData.h"

namespace itk
{
/** \class ScalarChanAndVeseLevelSetFunctionData
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
class ScalarChanAndVeseLevelSetFunctionData : public RegionBasedLevelSetFunctionData<TInputImage, TFeatureImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseLevelSetFunctionData);

  using Self = ScalarChanAndVeseLevelSetFunctionData;
  using Superclass = RegionBasedLevelSetFunctionData<TInputImage, TFeatureImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(ScalarChanAndVeseLevelSetFunctionData, RegionBasedLevelSetFunctionData);

  using InputImageType = TInputImage;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputRegionType = typename Superclass::InputRegionType;
  using InputSizeType = typename Superclass::InputSizeType;
  using InputSizeValueType = typename Superclass::InputSizeValueType;
  using InputSpacingType = typename Superclass::InputSpacingType;
  using InputIndexType = typename Superclass::InputIndexType;
  using InputIndexValueType = typename Superclass::InputIndexValueType;
  using InputPointType = typename Superclass::InputPointType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename Superclass::FeatureImagePointer;
  using FeatureImageConstPointer = typename Superclass::FeatureImageConstPointer;
  using FeaturePixelType = typename Superclass::FeaturePixelType;
  using FeatureRegionType = typename Superclass::FeatureRegionType;
  using FeatureSizeType = typename Superclass::FeatureSizeType;
  using FeatureSizeValueType = typename Superclass::FeatureSizeValueType;
  using FeatureSpacingType = typename Superclass::FeatureSpacingType;
  using FeatureIndexType = typename Superclass::FeatureIndexType;
  using FeaturePointType = typename Superclass::FeaturePointType;

  double m_BackgroundConstantValues;
  double m_ForegroundConstantValues;
  double m_WeightedSumOfPixelValuesInsideLevelSet;
  double m_WeightedSumOfPixelValuesOutsideLevelSet;

protected:
  ScalarChanAndVeseLevelSetFunctionData()
    : Superclass()
  {
    m_BackgroundConstantValues = 0.;
    m_ForegroundConstantValues = 0.;
    m_WeightedSumOfPixelValuesInsideLevelSet = 0.;
    m_WeightedSumOfPixelValuesOutsideLevelSet = 0.;
  }

  ~ScalarChanAndVeseLevelSetFunctionData() override = default;
};
} // end namespace itk

#endif
