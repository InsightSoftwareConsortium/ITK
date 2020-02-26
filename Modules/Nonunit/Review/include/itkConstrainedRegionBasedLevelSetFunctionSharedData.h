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
#ifndef itkConstrainedRegionBasedLevelSetFunctionSharedData_h
#define itkConstrainedRegionBasedLevelSetFunctionSharedData_h

#include "itkRegionBasedLevelSetFunctionSharedData.h"

namespace itk
{
/** \class ConstrainedRegionBasedLevelSetFunctionSharedData
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
template <typename TInputImage, typename TFeatureImage, typename TSingleData>
class ConstrainedRegionBasedLevelSetFunctionSharedData
  : public RegionBasedLevelSetFunctionSharedData<TInputImage, TFeatureImage, TSingleData>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConstrainedRegionBasedLevelSetFunctionSharedData);

  using Self = ConstrainedRegionBasedLevelSetFunctionSharedData;
  using Superclass = RegionBasedLevelSetFunctionSharedData<TInputImage, TFeatureImage, TSingleData>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(ConstrainedRegionBasedLevelSetFunctionSharedData, RegionBasedLevelSetFunctionSharedData);

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

  using ListPixelType = typename Superclass::ListPixelType;
  using ListImageType = typename Superclass::ListImageType;
  using ListImagePointer = typename Superclass::ListImagePointer;
  using ListImageConstPointer = typename Superclass::ListImageConstPointer;
  using ListRegionType = typename Superclass::ListRegionType;
  using ListSizeType = typename Superclass::ListSizeType;
  using ListSizeValueType = typename Superclass::ListSizeValueType;
  using ListSpacingType = typename Superclass::ListSpacingType;
  using ListIndexType = typename Superclass::ListIndexType;
  using ListIndexValueType = typename Superclass::ListIndexValueType;
  using ListPointType = typename Superclass::ListPointType;
  using ListIteratorType = typename Superclass::ListIteratorType;

  using CentroidVectorType = typename Superclass::CentroidVectorType;
  using SampleType = typename Superclass::SampleType;
  using TreeGeneratorType = typename Superclass::TreeGeneratorType;
  using TreePointer = typename Superclass::TreePointer;
  using TreeType = typename Superclass::TreeType;
  using KdTreePointer = typename Superclass::KdTreePointer;

  using LevelSetDataType = TSingleData;
  using LevelSetDataPointer = typename Superclass::LevelSetDataPointer;
  using LevelSetDataPointerVector = typename Superclass::LevelSetDataPointerVector;
  using LevelSetDataPointerVectorIterator = typename Superclass::LevelSetDataPointerVectorIterator;

  void
  PopulateListImage() override
  {
    ListSpacingType spacing = this->m_NearestNeighborListImage->GetSpacing();

    ListRegionType region = this->m_NearestNeighborListImage->GetLargestPossibleRegion();

    ListIteratorType lIt(this->m_NearestNeighborListImage, region);

    if (this->m_KdTree.IsNotNull())
    {
      for (lIt.GoToBegin(); !lIt.IsAtEnd(); ++lIt)
      {
        ListIndexType ind = lIt.GetIndex();

        float queryPoint[ImageDimension];
        for (unsigned int i = 0; i < ImageDimension; i++)
        {
          queryPoint[i] = ind[i] * spacing[i];
        }

        typename TreeType::InstanceIdentifierVectorType neighbors;
        this->m_KdTree->Search(queryPoint, this->m_NumberOfNeighbors, neighbors);

        ListPixelType L;
        for (unsigned int i = 0; i < this->m_NumberOfNeighbors; i++)
        {
          if (this->m_LevelSetDataPointerVector[i]->VerifyInsideRegion(ind))
          {
            L.push_back(neighbors[i]);
          }
        }
        lIt.Set(L);
      }
    }
    else
    {
      for (lIt.GoToBegin(); !lIt.IsAtEnd(); ++lIt)
      {
        ListIndexType ind = lIt.GetIndex();
        ListPixelType L;
        for (unsigned int i = 0; i < this->m_FunctionCount; i++)
        {
          if (this->m_LevelSetDataPointerVector[i]->VerifyInsideRegion(ind))
          {
            L.push_back(i);
          }
        }
        lIt.Set(L);
      }
    }
  }

protected:
  ConstrainedRegionBasedLevelSetFunctionSharedData()
    : Superclass()
  {}
  ~ConstrainedRegionBasedLevelSetFunctionSharedData() override = default;
};
} // end namespace itk

#endif
