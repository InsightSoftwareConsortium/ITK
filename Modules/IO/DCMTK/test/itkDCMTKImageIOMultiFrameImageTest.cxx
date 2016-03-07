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

#include "itkImageFileReader.h"
#include "itkDCMTKImageIO.h"
#include "itkImageRegionConstIterator.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkMath.h"

typedef short                             PixelType;
typedef itk::Image< PixelType, 3 >        ImageType;
typedef ImageType::DirectionType          DirectionType;
typedef ImageType::SpacingType            SpacingType;
typedef ImageType::PointType              PointType;
typedef itk::ImageFileReader< ImageType > ReaderType;
typedef itk::DCMTKImageIO                 ImageIOType;

namespace
{
//
// if the difference is < average/10000, close enough
bool CloseEnough(double a, double b)
{
  double diff = itk::Math::abs(a - b);
  double avg = (itk::Math::abs(a) + itk::Math::abs(b)) / 2.0;
  if(diff == 0.0 || diff < avg/10000.00)
    {
    return true;
    }
  return false;
}

bool
Equal(DirectionType dir1,DirectionType dir2)
{
  for(unsigned i = 0; i < 3; ++i)
    {
    for(unsigned j = 0; j < 3; ++j)
      {
      if(!CloseEnough(dir1(i,j),dir2(i,j)))
        {
        return false;
        }
      }
    }
  return true;
}

bool
Equal(SpacingType spacing1,SpacingType spacing2)
{
  for(unsigned i = 0; i < 3; ++i)
    {
    if(!CloseEnough(spacing1[i],spacing2[i]))
      {
      return false;
      }
    }
  return true;
}
}
//
// This tests a Philips Multiframe image orieintation/spacing
// known to exist in the test file in order to ensure that
// they're properly read out of the functional group.
int itkDCMTKImageIOMultiFrameImageTest(int ac, char * av[])
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0]
              << " <multiframe image>"
              << std::endl;
    return EXIT_FAILURE;
    }

  ImageIOType::Pointer dcmImageIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );
  reader->SetImageIO( dcmImageIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  ImageType::Pointer im = reader->GetOutput();
  DirectionType dir = im->GetDirection();
  SpacingType spacing = im->GetSpacing();
  PointType origin = im->GetOrigin();

  std::cerr << "Direction " << dir << std::endl
            << "Spacing " << spacing << std::endl
            << "Origin " << origin << std::endl;
  DirectionType expectedDirection;
  expectedDirection(0,0) = 0.0;
  expectedDirection(0,1) = 0.0;
  expectedDirection(0,2) = -1.0;
  expectedDirection(1,0) = 1.0;
  expectedDirection(1,1) = 0.0;
  expectedDirection(1,2) = 0.0;
  expectedDirection(2,0) = 0.0;
  expectedDirection(2,1) = -1.0;
  expectedDirection(2,2) = 0.0;
  if(!Equal(dir,expectedDirection))
    {
    std::cerr << "Expected directions" << std::endl
              << expectedDirection << std::endl
              << "Actual directions" << std::endl
              << dir << std::endl;
    return EXIT_FAILURE;
    }
  SpacingType expectedSpacing;
  expectedSpacing[0] = expectedSpacing[1] = expectedSpacing[2] = 0.75;
  if(!Equal(spacing,expectedSpacing))
    {
    std::cerr << "Expected spacing" << std::endl
              << expectedSpacing << std::endl
              << "Actual spacing" << std::endl
              << spacing << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
