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

#include "itkImageFileReader.h"
#include "itkDCMTKImageIO.h"
#include "itkImageRegionConstIterator.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

using PixelType = short;
using ImageType = itk::Image<PixelType, 3>;
using DirectionType = ImageType::DirectionType;
using SpacingType = ImageType::SpacingType;
using PointType = ImageType::PointType;
using ReaderType = itk::ImageFileReader<ImageType>;
using ImageIOType = itk::DCMTKImageIO;

namespace
{
//
// if the difference is < average/10000, close enough
bool
CloseEnough(double a, double b)
{
  double diff = itk::Math::abs(a - b);
  double avg = (itk::Math::abs(a) + itk::Math::abs(b)) / 2.0;
  if (diff == 0.0 || diff < avg / 10000.00)
  {
    return true;
  }
  return false;
}

bool
Equal(DirectionType dir1, DirectionType dir2)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      if (!CloseEnough(dir1(i, j), dir2(i, j)))
      {
        return false;
      }
    }
  }
  return true;
}

bool
Equal(SpacingType spacing1, SpacingType spacing2)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    if (!CloseEnough(spacing1[i], spacing2[i]))
    {
      return false;
    }
  }
  return true;
}

bool
Equal(PointType p1, PointType p2)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    if (!CloseEnough(p1[i], p2[i]))
    {
      return false;
    }
  }
  return true;
}
} // namespace

// This tests if slice spacing is correctly read from a Philips MRI where
// SpacingBetweenSlices is stored as a global tag.
//
int
itkDCMTKImageIOSpacingTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " multiframeImage" << std::endl;
    return EXIT_FAILURE;
  }

  auto dcmImageIO = ImageIOType::New();

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(dcmImageIO);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  const ImageType::Pointer im = reader->GetOutput();
  const DirectionType      dir = im->GetDirection();
  const SpacingType        spacing = im->GetSpacing();
  const PointType          origin = im->GetOrigin();

  std::cerr << "Direction " << dir << std::endl
            << "Spacing " << spacing << std::endl
            << "Origin " << origin << std::endl;
  DirectionType expectedDirection;
  expectedDirection(0, 0) = 0.999894;
  expectedDirection(0, 1) = 0.0145622;
  expectedDirection(0, 2) = -0.000350456;
  expectedDirection(1, 0) = 0.000350419;
  expectedDirection(1, 1) = 5.1034e-06;
  expectedDirection(1, 2) = 1.0;
  expectedDirection(2, 0) = 0.0145622;
  expectedDirection(2, 1) = -0.999894;
  expectedDirection(2, 2) = -1.33323e-11;
  if (!Equal(dir, expectedDirection))
  {
    std::cerr << "Expected directions" << std::endl
              << expectedDirection << std::endl
              << "Actual directions" << std::endl
              << dir << std::endl;
    return EXIT_FAILURE;
  }
  SpacingType expectedSpacing;
  expectedSpacing[0] = 0.55;
  expectedSpacing[1] = 0.55;
  expectedSpacing[2] = 1.3;
  if (!Equal(spacing, expectedSpacing))
  {
    std::cerr << "Expected spacing" << std::endl
              << expectedSpacing << std::endl
              << "Actual spacing" << std::endl
              << spacing << std::endl;
    return EXIT_FAILURE;
  }
  PointType expectedOrigin;
  expectedOrigin[0] = -112.767;
  expectedOrigin[1] = -41.4828;
  expectedOrigin[2] = 117.567;
  if (!Equal(origin, expectedOrigin))
  {
    std::cerr << "Expected origin" << std::endl
              << expectedOrigin << std::endl
              << "Actual origin" << std::endl
              << origin << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
