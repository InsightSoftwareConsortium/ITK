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

#include <iostream>
#include "itkImage.h"
#include "itkTestingExtractSliceImageFilter.h"
#include "itkTestingMacros.h"

int itkTestingExtractSliceImageFilterTest(int, char* [] )
{

  typedef unsigned char PixelType;
  const unsigned int    InputDimension = 3;
  const unsigned int    OutputDimension = 2;

  typedef itk::Image< PixelType, InputDimension >    InputImageType;
  typedef itk::Image< PixelType, OutputDimension >   OutputImageType;

  typedef itk::Testing::ExtractSliceImageFilter< InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer inputImage = InputImageType::New();

  InputImageType::SizeType size;
  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  InputImageType::RegionType region;
  region.SetSize(size);

  inputImage->SetRegions(region);
  inputImage->Allocate(true); // initialize buffer to zero

  InputImageType::DirectionType direction;
  direction[0][0] = 1.0;
  direction[0][1] = 0.5;
  direction[0][2] = 0.0;
  direction[1][0] = 0.5;
  direction[1][1] = 1.0;
  direction[1][2] = 0.5;
  direction[2][0] = 0.5;
  direction[2][1] = 0.5;
  direction[2][2] = 1.0;

  inputImage->SetDirection( direction );

  InputImageType::SpacingType spacing;
  spacing[0] = 2.5;
  spacing[1] = 1.5;
  spacing[2] = 3.5;

  inputImage->SetSpacing( spacing );

  filter->SetInput( inputImage );

  FilterType::InputImageRegionType extractRegion = inputImage->GetBufferedRegion();

  InputImageType::SizeType regionSize = extractRegion.GetSize();

  // expect exception, because for output dimension = 2, one of the size
  // components must be zero.
  TRY_EXPECT_EXCEPTION( filter->SetExtractionRegion( extractRegion ) );

  // Set properly, one of the size components to zero.
  regionSize[2] = 0;
  extractRegion.SetSize(regionSize);

  // Now it should be good, with the zero inserted.
  TRY_EXPECT_NO_EXCEPTION( filter->SetExtractionRegion( extractRegion ) );
  TEST_SET_GET_VALUE( extractRegion, filter->GetExtractionRegion() );

#ifdef ITKV3_COMPATIBILITY
  FilterType::DIRECTIONCOLLAPSESTRATEGY strategy = FilterType::DIRECTIONCOLLAPSETOGUESS;
#else
  FilterType::DIRECTIONCOLLAPSESTRATEGY strategy = FilterType::DIRECTIONCOLLAPSETOUNKOWN;

  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_EXCEPTION( filter->Update() );
#endif

  TRY_EXPECT_EXCEPTION( filter->SetDirectionCollapseToStrategy( FilterType::DIRECTIONCOLLAPSETOUNKOWN ) );

  filter->SetDirectionCollapseToIdentity();
  strategy = FilterType::DIRECTIONCOLLAPSETOIDENTITY;
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  filter->SetDirectionCollapseToGuess();
  strategy = FilterType::DIRECTIONCOLLAPSETOGUESS;
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  filter->SetDirectionCollapseToSubmatrix();
  strategy = FilterType::DIRECTIONCOLLAPSETOSUBMATRIX;
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  strategy = FilterType::DIRECTIONCOLLAPSETOIDENTITY;
  filter->SetDirectionCollapseToStrategy(strategy);
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  strategy = FilterType::DIRECTIONCOLLAPSETOGUESS;
  filter->SetDirectionCollapseToStrategy(strategy);
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  strategy = FilterType::DIRECTIONCOLLAPSETOSUBMATRIX;
  filter->SetDirectionCollapseToStrategy(strategy);
  TEST_SET_GET_VALUE( strategy, filter->GetDirectionCollapseToStrategy() );
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise PrintSelf()
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
