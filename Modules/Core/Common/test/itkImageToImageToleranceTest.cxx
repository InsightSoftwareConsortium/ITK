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
#include "itkImage.h"
#include "itkAddImageFilter.h"
#include "itkTestingMacros.h"

int itkImageToImageToleranceTest( int, char * [] )
{
  typedef itk::Image<unsigned char,3> ImageType;

  ImageType::Pointer image1 = ImageType::New();
  ImageType::Pointer image2 = ImageType::New();

  ImageType::SizeType size = { {3,3,3} };
  image1->SetRegions(size);
  image1->Allocate();
  image1->FillBuffer(1);
  image2->SetRegions(size);
  image2->Allocate();
  image2->FillBuffer(2);

  ImageType::PointType origin;
  origin[0] = 1.0;
  origin[1] = 1.0;
  origin[2] = 1.0;
  image1->SetOrigin(origin);
  origin[0] += 1.0E-5;
  image2->SetOrigin(origin);

  // test coordinate tolerance

  typedef itk::AddImageFilter<ImageType,ImageType,ImageType> AddImageFilterType;

  AddImageFilterType::Pointer addImageFilter =
    AddImageFilterType::New();
  addImageFilter->SetInput1(image1);
  addImageFilter->SetInput2(image2);
  bool exceptCaught = false;
  try
    {
    addImageFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    exceptCaught = true;
    std::cout << "Expected exception:" << std::endl
              << e << std::endl;
    }
  if(!exceptCaught)
    {
    std::cerr << "Expected exception not caught" << std::endl;
    return EXIT_FAILURE;
    }
  addImageFilter =
    AddImageFilterType::New();
  addImageFilter->SetCoordinateTolerance(1.0E-4);
  std::cerr << addImageFilter << std::endl;
  addImageFilter->SetInput1(image1);
  addImageFilter->SetInput2(image2);
  exceptCaught = false;
  try
    {
    addImageFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    exceptCaught = true;
    std::cout << "Unexpected exception:" << std::endl
              << e << std::endl;
    return EXIT_FAILURE;
    }

  origin[0] = 1.0;
  image2->SetOrigin(origin);

  ImageType::DirectionType dir;
  dir.SetIdentity();
  image1->SetDirection(dir);
  dir[0][0] += 10.0E-5;
  image2->SetDirection(dir);

  addImageFilter =
    AddImageFilterType::New();
  addImageFilter->SetInput1(image1);
  addImageFilter->SetInput2(image2);
  exceptCaught = false;
  try
    {
    addImageFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    exceptCaught = true;
    std::cout << "Expected exception:" << std::endl
              << e << std::endl;
    }
  if(!exceptCaught)
    {
    std::cerr << "Expected exception not caught" << std::endl;
    return EXIT_FAILURE;
    }

  addImageFilter =
    AddImageFilterType::New();
  addImageFilter->SetDirectionTolerance(1.0E-4);
  addImageFilter->SetInput1(image1);
  addImageFilter->SetInput2(image2);
  exceptCaught = false;
  try
    {
    addImageFilter->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    exceptCaught = true;
    std::cout << "Unexpected exception:" << std::endl
              << e << std::endl;
    return EXIT_FAILURE;
    }

  // test global defaults
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultCoordinateTolerance(), 1.0e-6);
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultDirectionTolerance(), 1.0e-6);

  addImageFilter = AddImageFilterType::New();
  TEST_EXPECT_EQUAL( addImageFilter->GetCoordinateTolerance(), 1.0e-6);
  TEST_EXPECT_EQUAL( addImageFilter->GetDirectionTolerance(), 1.0e-6);

  AddImageFilterType::SetGlobalDefaultCoordinateTolerance( 1.0e-4 );
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultCoordinateTolerance(), 1.0e-4);
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultDirectionTolerance(), 1.0e-6);

  itk::ImageToImageFilterCommon::SetGlobalDefaultDirectionTolerance( 1.0e-5 );
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultCoordinateTolerance(), 1.0e-4);
  TEST_EXPECT_EQUAL( AddImageFilterType::GetGlobalDefaultDirectionTolerance(), 1.0e-5);

  addImageFilter = AddImageFilterType::New();
  TEST_EXPECT_EQUAL( addImageFilter->GetCoordinateTolerance(), 1.0e-4);
  TEST_EXPECT_EQUAL( addImageFilter->GetDirectionTolerance(), 1.0e-5);

  return EXIT_SUCCESS;
}
