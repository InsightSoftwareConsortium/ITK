/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkLandmarkDisplacementFieldSource.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

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

  FilterType::Pointer filter = FilterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  DisplacementFieldType::SpacingType spacing;
  spacing.Fill(1.0);

  DisplacementFieldType::PointType origin;
  origin.Fill(0.0);

  DisplacementFieldType::RegionType region;
  DisplacementFieldType::SizeType   size;
  DisplacementFieldType::IndexType  start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize(size);
  region.SetIndex(start);

  DisplacementFieldType::DirectionType direction;
  direction.SetIdentity();


  filter->SetOutputSpacing(spacing);
  filter->SetOutputOrigin(origin);
  filter->SetOutputRegion(region);
  filter->SetOutputDirection(direction);

  //  Create source and target landmarks.
  //
  using LandmarkContainerType = FilterType::LandmarkContainer;
  using LandmarkPointType = FilterType::LandmarkPointType;

  LandmarkContainerType::Pointer sourceLandmarks = LandmarkContainerType::New();
  LandmarkContainerType::Pointer targetLandmarks = LandmarkContainerType::New();

  LandmarkPointType sourcePoint;
  LandmarkPointType targetPoint;

  std::ifstream pointsFile;
  pointsFile.open(argv[1]);

  unsigned int pointId = 0;

  pointsFile >> sourcePoint;
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

  WriterType::Pointer writer = WriterType::New();

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
