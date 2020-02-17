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
#ifndef itkRegionBasedLevelSetFunctionSharedData_h
#define itkRegionBasedLevelSetFunctionSharedData_h

#include "itkLightObject.h"

#include "itkVector.h"
#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class RegionBasedLevelSetFunctionSharedData
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
class RegionBasedLevelSetFunctionSharedData : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionBasedLevelSetFunctionSharedData);

  using Self = RegionBasedLevelSetFunctionSharedData;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TFeatureImage::ImageDimension;

  itkTypeMacro(RegionBasedLevelSetFunctionSharedData, LightObject);

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

  using ListPixelType = std::list<unsigned int>;
  using ListImageType = Image<ListPixelType, Self::ImageDimension>;
  using ListImagePointer = typename ListImageType::Pointer;
  using ListImageConstPointer = typename ListImageType::ConstPointer;
  using ListRegionType = typename ListImageType::RegionType;
  using ListSizeType = typename ListImageType::SizeType;
  using ListSizeValueType = typename ListSizeType::SizeValueType;
  using ListSpacingType = typename ListImageType::SpacingType;
  using ListIndexType = typename ListImageType::IndexType;
  using ListIndexValueType = typename ListIndexType::IndexValueType;
  using ListPointType = typename ListImageType::PointType;
  using ListIteratorType = ImageRegionIteratorWithIndex<ListImageType>;

  using CentroidVectorType = Vector<float, Self::ImageDimension>;
  using SampleType = itk::Statistics::ListSample<CentroidVectorType>;
  using TreeGeneratorType = itk::Statistics::KdTreeGenerator<SampleType>;
  using TreePointer = typename TreeGeneratorType::Pointer;
  using TreeType = typename TreeGeneratorType::KdTreeType;
  using KdTreePointer = typename TreeType::Pointer;

  using LevelSetDataType = TSingleData;
  using LevelSetDataPointer = typename LevelSetDataType::Pointer;
  using LevelSetDataPointerVector = std::vector<LevelSetDataPointer>;
  using LevelSetDataPointerVectorIterator = typename LevelSetDataPointerVector::iterator;

  void
  SetFunctionCount(const unsigned int & n)
  {
    this->m_FunctionCount = n;
    this->m_LevelSetDataPointerVector.resize(n, nullptr);

    auto it = m_LevelSetDataPointerVector.begin();
    auto end = m_LevelSetDataPointerVector.end();
    while (it != end)
    {
      (*it) = LevelSetDataType::New();
      it++;
    }
  }

  void
  SetNumberOfNeighbors(const unsigned int & n)
  {
    this->m_NumberOfNeighbors = n;
  }

  void
  CreateHeavisideFunctionOfLevelSetImage(const unsigned int & j, const InputImageType * image)
  {
    m_LevelSetDataPointerVector[j]->CreateHeavisideFunctionOfLevelSetImage(image);
  }

  void
  SetKdTree(KdTreePointer kdtree)
  {
    this->m_KdTree = kdtree;
  }

  void
  AllocateListImage(const FeatureImageType * featureImage)
  {
    this->m_NearestNeighborListImage = ListImageType::New();
    this->m_NearestNeighborListImage->CopyInformation(featureImage);
    this->m_NearestNeighborListImage->SetRegions(featureImage->GetLargestPossibleRegion());
    this->m_NearestNeighborListImage->Allocate();
  }

  virtual void
  PopulateListImage() = 0;

  LevelSetDataPointerVector m_LevelSetDataPointerVector;

  unsigned int     m_FunctionCount;
  unsigned int     m_NumberOfNeighbors{ 6 };
  ListImagePointer m_NearestNeighborListImage;
  KdTreePointer    m_KdTree;

protected:
  RegionBasedLevelSetFunctionSharedData()
    : m_KdTree(nullptr)
  {}
  ~RegionBasedLevelSetFunctionSharedData() override = default;
};
} // end namespace itk

#endif
