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
#ifndef itkLevelSetDomainPartitionImageWithKdTree_h
#define itkLevelSetDomainPartitionImageWithKdTree_h

#include "itkLevelSetDomainPartitionImage.h"

#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

namespace itk
{
/**
 *\class LevelSetDomainPartitionImageWithKdTree
 *
 * \brief Helper class used to share data in the ScalarChanAndVeseLevelSetFunction.
 * \ingroup ITKLevelSetsv4
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionImageWithKdTree : public LevelSetDomainPartitionImage<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetDomainPartitionImageWithKdTree);

  using Self = LevelSetDomainPartitionImageWithKdTree;
  using Superclass = LevelSetDomainPartitionImage<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetDomainPartitionImageWithKdTree, LevelSetDomainPartitionImage);

  using ImageType = TImage;
  using ListIndexType = typename Superclass::ListIndexType;
  using ListRegionType = typename Superclass::ListRegionType;
  using ListPointType = typename Superclass::ListPointType;
  using ListIteratorType = typename Superclass::ListIteratorType;
  using IdentifierListType = typename Superclass::IdentifierListType;

  using CentroidVectorType = typename ListPointType::VectorType;
  using SampleType = typename Statistics::ListSample<CentroidVectorType>;
  using TreeGeneratorType = typename Statistics::KdTreeGenerator<SampleType>;
  using TreePointer = typename TreeGeneratorType::Pointer;
  using TreeType = typename TreeGeneratorType::KdTreeType;
  using KdTreePointer = typename TreeType::Pointer;

  /** Initialize with a precomputed kd-tree */
  itkSetObjectMacro(KdTree, TreeType);

  /** Number of neighbors level sets connected to this level set. */
  using NeighborsIdType = unsigned int;

  /** Get/Set number of neighbors in the kd-tree leaf node */
  itkSetMacro(NumberOfNeighbors, NeighborsIdType);
  itkGetMacro(NumberOfNeighbors, NeighborsIdType);

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void
  PopulateListDomain() override;

protected:
  LevelSetDomainPartitionImageWithKdTree();
  ~LevelSetDomainPartitionImageWithKdTree() override = default;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void
  PopulateDomainWithKdTree();

private:
  KdTreePointer   m_KdTree;
  NeighborsIdType m_NumberOfNeighbors{ 10 };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainPartitionImageWithKdTree.hxx"
#endif

#endif
