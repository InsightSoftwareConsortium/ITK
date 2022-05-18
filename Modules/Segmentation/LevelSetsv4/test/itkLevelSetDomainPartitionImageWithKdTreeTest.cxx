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

#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkLevelSetDomainPartitionImageWithKdTree.h"
#include "itkTestingMacros.h"

int
itkLevelSetDomainPartitionImageWithKdTreeTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using IdentifierType = itk::IdentifierType;

  using DomainPartitionSourceType = itk::LevelSetDomainPartitionImageWithKdTree<InputImageType>;
  using ListImageType = DomainPartitionSourceType::ListImageType;
  using LevelSetDomainRegionVectorType = DomainPartitionSourceType::LevelSetDomainRegionVectorType;
  using CentroidVectorType = DomainPartitionSourceType::CentroidVectorType;
  using SampleType = DomainPartitionSourceType::SampleType;
  using TreeGeneratorType = DomainPartitionSourceType::TreeGeneratorType;
  using TreeType = DomainPartitionSourceType::TreeType;

  using ListType = ListImageType::PixelType;
  using ListImageIteratorType = itk::ImageRegionConstIteratorWithIndex<ListImageType>;

  // load binary mask
  InputImageType::SizeType size;
  size[0] = 100;
  size[1] = 10;

  InputImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;

  InputImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  InputImageType::IndexType index;
  index.Fill(0);

  InputImageType::RegionType region;
  region.SetIndex(index);
  region.SetSize(size);

  // Binary initialization
  auto binary = InputImageType::New();
  binary->SetRegions(region);
  binary->SetSpacing(spacing);
  binary->SetOrigin(origin);
  binary->Allocate();
  binary->FillBuffer(itk::NumericTraits<InputPixelType>::ZeroValue());

  IdentifierType numberOfLevelSetFunctions = 10;

  LevelSetDomainRegionVectorType regionVector;
  regionVector.resize(numberOfLevelSetFunctions);

  CentroidVectorType mv;
  auto               sample = SampleType::New();
  sample->SetMeasurementVectorSize(Dimension);

  for (unsigned int i = 0; i < numberOfLevelSetFunctions; ++i)
  {
    index[0] = 10 * i;
    index[1] = 0;
    size.Fill(10);

    InputImageType::RegionType region1;
    region1.SetIndex(index);
    region1.SetSize(size);

    regionVector[i] = region1;

    mv[0] = 10 * i + 5.0;
    mv[1] = 5.0;
    sample->PushBack(mv);
  }

  auto treeGenerator = TreeGeneratorType::New();
  treeGenerator->SetSample(sample);
  treeGenerator->SetBucketSize(2);
  treeGenerator->Update();
  TreeType::Pointer kdtree = treeGenerator->GetOutput();

  auto partitionSource = DomainPartitionSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    partitionSource, LevelSetDomainPartitionImageWithKdTree, LevelSetDomainPartitionImage);


  partitionSource->SetNumberOfLevelSetFunctions(numberOfLevelSetFunctions);
  partitionSource->SetImage(binary);
  partitionSource->SetLevelSetDomainRegionVector(regionVector);

  typename DomainPartitionSourceType::NeighborsIdType numberOfNeighbors = 3;
  partitionSource->SetNumberOfNeighbors(numberOfNeighbors);
  ITK_TEST_SET_GET_VALUE(numberOfNeighbors, partitionSource->GetNumberOfNeighbors());

  partitionSource->SetKdTree(kdtree);
  partitionSource->PopulateListDomain();


  bool flag = true;

  ListType                    ll;
  ListImageType::ConstPointer listImage = partitionSource->GetListDomain();
  ListImageIteratorType       It(listImage, listImage->GetLargestPossibleRegion());
  It.GoToBegin();
  while (!It.IsAtEnd())
  {
    index = It.GetIndex();
    ll = It.Get();

    if (ll.size() != 1)
    {
      flag = false;
      break;
    }

    auto it = ll.begin();

    while (it != ll.end())
    {
      IdentifierType id = index[0] / 10;
      if (*it != id)
      {
        flag = false;
        break;
      }
      ++it;
    }
    ++It;
  }

  if (!flag)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
