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

#include "itkPolygonSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include <iostream>
#include <fstream>
#include <sstream>

// -------------------------------------------------------------------------------------
// The purpose of this test is to make sure that the function
// ObjectIsInsideInObjectSpace is working as expected.
// This is related to a regression that was fixed under case 1082
// which showed that certain contours that were created from the polygonSpatialObject
// had trailing tails that were not supposed to be there.
// In order to test this we take a known regression list of contour points
// and create a contour image from these points.
// We compare the resulting image with the known baseline contour image.
// ---------------------------------------------------------------------------------------

int
itkPolygonSpatialObjectIsInsideInObjectSpaceTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " polygonPointsCSVFile baselineImageFileName outImageFileName ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PointType = itk::ImageBase<Dimension>::PointType;
  using PolygonPointType = itk::PolygonSpatialObject<Dimension>::PolygonPointType;
  using PolygonPointListType = itk::PolygonSpatialObject<Dimension>::PolygonPointListType;
  using PixelType = uint16_t;
  using Group = itk::GroupSpatialObject<Dimension>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using SpatialObjectToImageFilterType = itk::SpatialObjectToImageFilter<Group, ImageType>;

  const char * csvFileName = argv[1];
  const char * baselineFileName = argv[2];
  const char * outFileName = argv[3];

  // Get polygon points from CSV file
  PolygonPointListType polygonPointList;
  std::ifstream        fs;
  char                 data[100];

  fs.open(csvFileName);
  if (fs.good())
  {
    // read over first line
    fs.getline(data, sizeof(data), '\n');
    char commaDelim;
    while (fs.getline(data, sizeof(data), '\n'))
    {
      std::stringstream ss(data);

      PolygonPointType                       polygonPt;
      itk::SpatialObject<Dimension>::Pointer so = itk::SpatialObject<Dimension>::New();
      polygonPt.SetSpatialObject(so);
      PointType pt;

      ss >> pt[0];
      ss >> commaDelim;
      ss >> pt[1];

      polygonPt.SetPositionInWorldSpace(pt);
      polygonPointList.push_back(polygonPt);
    }
  }
  else
  {
    fs.close();
    std::cerr << "Error reading CSV file" << csvFileName << std::endl;
    return EXIT_FAILURE;
  }

  // Create polygon spatial group
  auto groupPtr = Group::New();
  auto polygonPtr = itk::PolygonSpatialObject<Dimension>::New();
  groupPtr->AddChild(polygonPtr);
  polygonPtr->SetPoints(polygonPointList);
  polygonPtr->SetDefaultInsideValue(1.0);
  polygonPtr->SetDefaultOutsideValue(0.0);
  ITK_TRY_EXPECT_NO_EXCEPTION(polygonPtr->Update());

  // Read baseline image
  ImageType::Pointer baselineImagePtr = nullptr;
  ITK_TRY_EXPECT_NO_EXCEPTION(baselineImagePtr = itk::ReadImage<ImageType>(baselineFileName));


  // create image
  auto toImageFilter = SpatialObjectToImageFilterType::New();
  toImageFilter->SetInput(groupPtr);
  toImageFilter->SetSize(baselineImagePtr->GetLargestPossibleRegion().GetSize());
  toImageFilter->SetSpacing(baselineImagePtr->GetSpacing());
  ITK_TRY_EXPECT_NO_EXCEPTION(toImageFilter->Update());

  // write image
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(toImageFilter->GetOutput(), outFileName));

  return EXIT_SUCCESS;
}
