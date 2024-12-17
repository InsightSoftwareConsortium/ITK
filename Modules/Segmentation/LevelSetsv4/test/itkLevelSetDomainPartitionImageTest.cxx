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
#include "itkLevelSetDomainPartitionImage.h"
#include "itkTestingMacros.h"

int
itkLevelSetDomainPartitionImageTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using IdentifierType = itk::IdentifierType;

  using DomainPartitionSourceType = itk::LevelSetDomainPartitionImage<InputImageType>;
  using ListImageType = DomainPartitionSourceType::ListImageType;
  using LevelSetDomainRegionVectorType = DomainPartitionSourceType::LevelSetDomainRegionVectorType;

  using ListType = ListImageType::PixelType;
  using ListImageIteratorType = itk::ImageRegionConstIteratorWithIndex<ListImageType>;

  // load binary mask
  auto size = InputImageType::SizeType::Filled(50);

  InputImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;

  InputImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  constexpr InputImageType::IndexType index{};

  const InputImageType::RegionType region{ index, size };

  // Binary initialization
  auto binary = InputImageType::New();
  binary->SetRegions(region);
  binary->SetSpacing(spacing);
  binary->SetOrigin(origin);
  binary->Allocate();
  binary->FillBuffer(InputPixelType{});

  constexpr IdentifierType numberOfLevelSetFunctions = 2;

  LevelSetDomainRegionVectorType regionVector;
  regionVector.resize(numberOfLevelSetFunctions);
  regionVector[0] = region;
  regionVector[1] = region;

  auto partitionSource = DomainPartitionSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(partitionSource, LevelSetDomainPartitionImage, LevelSetDomainPartitionBase);


  // Exercise exceptions
  ITK_TRY_EXPECT_EXCEPTION(partitionSource->PopulateListDomain());

  partitionSource->SetNumberOfLevelSetFunctions(numberOfLevelSetFunctions);

  partitionSource->SetLevelSetDomainRegionVector(regionVector);

  LevelSetDomainRegionVectorType regionVectorObtained = partitionSource->GetLevelSetDomainRegionVector();

  ITK_TEST_EXPECT_EQUAL(regionVector.size(), regionVectorObtained.size());
  for (size_t i = 0; i < regionVector.size(); ++i)
  {
    ITK_TEST_EXPECT_EQUAL(regionVector[i], regionVectorObtained[i]);
  }

  partitionSource->SetImage(binary);
  ITK_TEST_SET_GET_VALUE(binary, partitionSource->GetImage());

  partitionSource->PopulateListDomain();


  bool flag = true;

  const ListImageType::ConstPointer listImage = partitionSource->GetListDomain();
  ListImageIteratorType             It(listImage, listImage->GetLargestPossibleRegion());
  It.GoToBegin();
  while (!It.IsAtEnd())
  {
    ListType ll = It.Get();
    if (ll.size() != 2)
    {
      flag = false;
      break;
    }

    auto it = ll.begin();

    while (it != ll.end())
    {
      if ((*it != 0) && (*it != 1))
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
