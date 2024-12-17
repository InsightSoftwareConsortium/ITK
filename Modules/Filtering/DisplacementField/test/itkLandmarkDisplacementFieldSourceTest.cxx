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

#include "itkLandmarkDisplacementFieldSource.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkThinPlateSplineKernelTransform.h"

#include <fstream>
#include "itkTestingMacros.h"


int
itkLandmarkDisplacementFieldSourceTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " landmarksFile outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using VectorComponentType = float;

  using VectorType = itk::Vector<VectorComponentType, Dimension>;

  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  using FilterType = itk::LandmarkDisplacementFieldSource<DisplacementFieldType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, LandmarkDisplacementFieldSource, ImageSource);


  const itk::SimpleFilterWatcher watcher(filter);


  DisplacementFieldType::SizeType size;
  size[0] = 128;
  size[1] = 128;
  DisplacementFieldType::IndexType start;
  start[0] = 0;
  start[1] = 0;
  const DisplacementFieldType::RegionType region{ start, size };
  DisplacementFieldType::DirectionType    direction;
  direction.SetIdentity();

  auto kernelTransform = itk::ThinPlateSplineKernelTransform<double, FilterType::ImageDimension>::New();
  filter->SetKernelTransform(kernelTransform);
  ITK_TEST_SET_GET_VALUE(kernelTransform, filter->GetKernelTransform());

  // Test default values
  auto spacingDefault = itk::MakeFilled<DisplacementFieldType::SpacingType>(1.0);
  ITK_TEST_SET_GET_VALUE(spacingDefault, filter->GetOutputSpacing());

  constexpr DisplacementFieldType::PointType originDefault{};
  ITK_TEST_SET_GET_VALUE(originDefault, filter->GetOutputOrigin());

  // Test non-default values
  auto spacingNonDefault = itk::MakeFilled<DisplacementFieldType::SpacingType>(9876.0);
  filter->SetOutputSpacing(spacingNonDefault);
  ITK_TEST_SET_GET_VALUE(spacingNonDefault, filter->GetOutputSpacing());

  constexpr auto originNonDefault = itk::MakeFilled<DisplacementFieldType::PointType>(1235.0);
  filter->SetOutputOrigin(originNonDefault);
  ITK_TEST_SET_GET_VALUE(originNonDefault, filter->GetOutputOrigin());

  filter->SetOutputRegion(region);
  ITK_TEST_SET_GET_VALUE(region, filter->GetOutputRegion());

  filter->SetOutputDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, filter->GetOutputDirection());

  //  Create source and target landmarks.
  //
  using LandmarkContainerType = FilterType::LandmarkContainer;
  using LandmarkPointType = FilterType::LandmarkPointType;

  auto sourceLandmarks = LandmarkContainerType::New();
  auto targetLandmarks = LandmarkContainerType::New();

  std::ifstream pointsFile;
  pointsFile.open(argv[1]);

  unsigned int pointId = 0;

  LandmarkPointType sourcePoint;
  pointsFile >> sourcePoint;
  LandmarkPointType targetPoint;
  pointsFile >> targetPoint;

  while (!pointsFile.fail())
  {
    sourceLandmarks->InsertElement(pointId, sourcePoint);
    targetLandmarks->InsertElement(pointId, targetPoint);
    pointId++;

    std::cout << sourcePoint << "  -->> " << targetPoint << std::endl;

    pointsFile >> sourcePoint;
    pointsFile >> targetPoint;
  }

  pointsFile.close();


  filter->SetSourceLandmarks(sourceLandmarks);
  filter->SetTargetLandmarks(targetLandmarks);

  try
  {
    filter->UpdateLargestPossibleRegion();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  // Write an image for regression testing
  using WriterType = itk::ImageFileWriter<DisplacementFieldType>;

  auto writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown by writer" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
