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

#include "itkImageFileWriter.h"
#include "itkPointSetToImageFilter.h"
#include "itkPointSet.h"
#include "itkTestingMacros.h"


int
itkPointSetToImageFilterTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " pointSetFile outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int PointSetDimension = 3;
  constexpr unsigned int ImageDimension = 3;

  using PointSetPointType = float;
  using PixelType = unsigned char;

  using PointSetType = itk::PointSet<PointSetPointType, PointSetDimension>;
  using BinaryImageType = itk::Image<PixelType, ImageDimension>;

  using PointSetType = itk::PointSet<PointSetPointType, PointSetDimension>;

  auto pointSet = PointSetType::New();

  using PointType = PointSetType::PointType;

  using PointsContainer = PointSetType::PointsContainer;
  auto pointContainer = PointsContainer::New();

  PointType point;

  // Read the point set
  std::ifstream file;
  file.open(argv[1]);
  if (file.fail())
  {
    std::cerr << "Error opening point set file with name : " << std::endl;
    std::cerr << argv[1] << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int pointId = 0;
  file >> point;
  while (!file.eof())
  {
    pointContainer->InsertElement(pointId, point);
    file >> point;
    pointId++;
  }
  pointSet->SetPoints(pointContainer);

  using FilterType = itk::PointSetToImageFilter<PointSetType, BinaryImageType>;
  auto filter = FilterType::New();

  filter->SetInput(pointSet);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  BinaryImageType::Pointer binaryImage = filter->GetOutput();

  itk::WriteImage(binaryImage, argv[2]);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
