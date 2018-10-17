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

#include "itkMaximumAbsoluteValueImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageRegionIterator.h"

int itkMaximumAbsoluteValueImageFilterTest( int, char * [] )
{
  constexpr unsigned int Dimension = 2;
  using PixelType = int;
  using ImageType = itk::Image< PixelType, Dimension >;
  using MaximumAbsoluteValueImageFilterType = itk::MaximumAbsoluteValueImageFilter<ImageType>;
  MaximumAbsoluteValueImageFilterType::Pointer maxAbsFilter = MaximumAbsoluteValueImageFilterType::New();
  
  EXERCISE_BASIC_OBJECT_METHODS( maxAbsFilter, MaximumAbsoluteValueImageFilter, BinaryFunctorImageFilter );

  /** Create an image and run a basic test */
  ImageType::RegionType region;
  ImageType::IndexType start;

  start[0] = 0;
  start[1] = 0;
 
  ImageType::SizeType size;
  size[0] = 200;
  size[1] = 300;
 
  region.SetSize(size);
  region.SetIndex(start);
 
  ImageType::Pointer image1 = ImageType::New();
  image1->SetRegions(region);
  image1->Allocate();

  ImageType::Pointer image2 = ImageType::New();
  image2->SetRegions(region);
  image2->Allocate();

  using IteratorType = itk::ImageRegionIterator< ImageType >;
  IteratorType  it1( image1, region);
  IteratorType  it2( image2, region);
  it1.GoToBegin();
  it2.GoToBegin();
  int i = 0;
  while( !it1.IsAtEnd() )
  {
    if ((i % 2) == 0) {
      it1.Set( 1.0 );
      it2.Set( -2.0 );
    } else {
      it1.Set( 2.0 );
      it2.Set( -1.0 );
    }
    ++it1;
    ++it2;
  }

  maxAbsFilter->SetInput1(image1);
  maxAbsFilter->SetInput2(image2);
  maxAbsFilter->Update();
  ImageType::Pointer outputImage = maxAbsFilter->GetOutput();

  IteratorType  ot( outputImage, region);
  ot.GoToBegin();
  i = 0;
  while( !ot.IsAtEnd() )
  {
    if ((i % 2) == 0) {
      TEST_EXPECT_EQUAL(ot.Get(), -2);
    } else {
      TEST_EXPECT_EQUAL(ot.Get(), 2);
    }
    ++ot;
  }

  /** TODO: Write an integration test */

  return EXIT_SUCCESS;
}
